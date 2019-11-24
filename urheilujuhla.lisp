(defpackage #:urheilujuhla
  (:use #:common-lisp #:iterate)
  (:export :main))

(in-package :urheilujuhla)

;;;
;;; Logging
;;;
(declaim (notinline log-message))
(defun log-message (&rest args)
  (if (= 1 (length args))
      (format *error-output* "~A" (first args))
      (apply #'format (cons *error-output* args)))
  (format *error-output* "~%")
  (force-output *error-output*))

(declaim (notinline log-describing))
(defun log-describing (describable)
  (describe describable *error-output*)
  (force-output *error-output*))

(declaim (notinline log-describing))
(defun log-backtrace ()
  (sb-debug:print-backtrace :count 5 :stream *error-output*)
  (terpri *error-output*)
  (force-output *error-output*))

;;;
;;; State references
;;;
(defvar *irc-thread*)
(defvar *irc-sender-thread*)
(defvar *processing-thread*)

(defvar *irc-connection*)
(defvar *irc-channels*)
(defvar *irc-sender-should-stop* nil)

(defvar *queue-lock* (bt:make-lock "queue-lock"))
(defvar *queues-updated* (bt:make-condition-variable :name "queues-updated"))
(defvar *from-irc* '())
(defvar *to-irc* '())

;; Will contain an alist from (lat lon) => fmisid as string
(defvar *location-to-fmisid* '())

;;;
;;; IRC
;;;
(define-condition irc-connection-error (error)
  ((description :initarg :description :reader description)))

(defun handle-irc-privmsg (message)
  (bt:with-lock-held (*queue-lock*)
    (push message *from-irc*)
    (bt:condition-notify *queues-updated*)))

(defun handle-irc-error (message)
  (log-message "In HANDLE-IRC-ERROR~%")
  (log-describing message)

  (with-slots (irc:arguments) message
    (let ((message-proper (car irc:arguments)))
      (when 
	  (or
	   (search "ping timeout" (string-downcase message-proper))
	   (search "closing link" (string-downcase message-proper)))
	(log-message "Error with message ~A detected." message-proper)

	(log-message "Quitting the IRC connection ~A." *irc-connection*)
	(irc:quit *irc-connection*)

	(log-message "Signaling an error to re-establish connection.")
	(error 'irc-connection-error :description message-proper)))))

(defun connect-to-irc-server (host port nick username realname)
  (setf *irc-connection* (irc:connect :server host :port port :nickname nick
				      :username username :realname realname))
  (when *irc-connection*
    (irc:remove-hooks *irc-connection* 'irc:irc-privmsg-message))
  
  (irc:add-hook *irc-connection* 'irc:irc-privmsg-message #'handle-irc-privmsg)
  (irc:add-hook *irc-connection* 'irc:irc-error-message #'handle-irc-error)

  (dolist (channel *irc-channels*)
    (log-message "Joining channel ~A~%" channel)
    (force-output *error-output*)
    (irc:join *irc-connection* channel))

  (setf *irc-sender-thread* (make-irc-sender-thread))

  (irc:read-message-loop *irc-connection*))

(defun run-irc-thread (host port nick username realname)
  (loop do
       (handler-case
	   (connect-to-irc-server host port nick username realname)
	 (irc-connection-error (err)
	   (log-message "Connection failed due to: ~A." err))
	 (usocket:socket-error (err)
	   (log-message "Connection failed due to network issue: ~A." err)))

       (setf *irc-sender-should-stop* t)

       (log-message "Thread ~A sleeping for 30 seconds..."
		    (bt:thread-name (bt:current-thread)))
       (sleep 30)))

(defun make-irc-thread (host port nick username realname)
  (bt:make-thread #'(lambda () (run-irc-thread host port nick username realname))
		  :name "irc-thread"))

(defun run-irc-sender-thread ()
  (loop do
       (when *irc-sender-should-stop*
	 (log-message "IRC sender thread stopping~%")
	 (setf *irc-sender-should-stop* nil)
	 (return-from run-irc-sender-thread))

       (bt:with-lock-held (*queue-lock*)
	 (when (> (length *to-irc*) 0)
	   (let ((popped (pop *to-irc*)))
	     (when (and (first popped) (second popped))
	       (format t "IRC << ~A~%" popped)
	       (handler-case
		   (apply #'irc:privmsg (list *irc-connection* (first popped) (second popped)))
		 (error (e)
		   (push popped *to-irc*)
		   (log-message "Encountered error while trying to send message to IRC: '~A', stack:" e)
		   (log-backtrace)
		   (sleep 5)))))))
       (sleep 1)))

(defun make-irc-sender-thread ()
  (bt:make-thread #'urheilujuhla::run-irc-sender-thread :name "irc-sender-thread"))

;;;
;;; Actual "brain" of the bot
;;;
(defun run-processing-thread ()
  (loop do
       (bt:acquire-lock *queue-lock*)
       (bt:condition-wait *queues-updated* *queue-lock*)
       ;; (format t "Begin processing queues...~%")

       (loop while (> (length *from-irc*) 0) do
	    (let ((msg (pop *from-irc*)))
	      (bt:make-thread #'(lambda ()
				  (handle-irc-message msg))
			      :name "irc-message-handler")))

       ;; (format t "Done processing queues.~%")
       (force-output)
       (bt:release-lock *queue-lock*)))

(defun make-processing-thread ()
  (bt:make-thread #'run-processing-thread :name "processing-thread"))

(defvar +date-format+
  '((:day 2) #\. (:month 2) #\. #\Space (:HOUR 2) #\: (:MIN 2) #\Space #\( :gmt-offset #\)))

;; Simple textual Unicode sparklines in Common Lisp;
;;  see <http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0001OR"> for reference.
(defvar *spark-chars* #(#\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█))
(defun sparkline (seq)
  (flet ((apply-to-numbers (seq comparator)
	   (iter (for elem in-sequence seq)
		 (when (numberp elem)
		   (collect elem into numbers))
		 (finally (return (apply comparator numbers)))))

	 (format-values (seq min max)
	   (with-output-to-string (out)
	     (iter (for elem in-sequence seq)
		   (if (not (numberp elem))
		       (format out "~a" "X")
		       (format out "~a"
			       (elt *spark-chars*
				    (floor (/ (- elem min)
					      (/
					       (- max min)
					       (- (length *spark-chars*) 1)))))))))))
    (if (= (length seq)
	   (count nil seq :test #'eq))
	(values "N/A" "N/A" "N/A")
	(let ((min (apply-to-numbers seq #'min))
	      (max (apply-to-numbers seq #'max)))
	  (values
	   (format-values seq min max) min max)))))

(defun directions-line (seq)
  (flet ((direction-number (number)
	   (if (eq nil number)
	       nil
	       (let ((direction (round (/ number 45.0))))
		 (if (= direction 8)
		     0
		     direction))))
	 (direction-symbol (number)
	   (if (eq nil number)
	       #\X
	       (let
		   ((direction-chars #(#\↓ #\↙ #\← #\↖ #\↑ #\↗ #\→ #\↘)))
		 (elt direction-chars number)))))
    (format nil "~{~A~}"
     (map 'list (alexandria:compose #'direction-symbol #'direction-number) seq))))

(defun format-station-place-name (observation)
  (let* ((station (fmi-observations:station observation))
	 (name (fmi-observations:station-name station))
	 (region (fmi-observations:station-region station)))
    (let ((comp (search region name :test #'char=)))
      (if (null comp)
	  (format nil "~A, ~A" name region)
	  (if (= 0 (search region name :test #'char=))
	      (format nil "~A, ~A" (subseq name (+ (length region) 1)) region)
	      (format nil "~A, ~A" name region))))))

(defun format-last-observation (observations)
  (format nil "~A: ~A @ ~A"
	  (format-station-place-name (first observations))
	  (cadar (last observations))
	  (local-time:format-timestring nil (caar (last observations)) :format +date-format+)))

(defun minutes-ago (timestamp)
  (round (/ (float (- (local-time:timestamp-to-universal (local-time:now))
		      (local-time:timestamp-to-universal timestamp))) 60)))

(defun format-short-text (observations location-source)
  (let* ((item-count (length observations))
	 (last-observation (last observations))
	 (last-twentyfour (if (> (length observations) 23)
			      (subseq observations (- item-count 24))
			      observations))
	 (twentyfour-observations (mapcar #'fmi-observations:temperature last-twentyfour)))
    (multiple-value-bind (sparkline min max)
	(sparkline twentyfour-observations)
      (format nil "~A: [~A, ~A] ~A; ~A (~A)."
	      (format-station-place-name (first observations)) min max sparkline
	      (format nil "~{~{~A°C (-~A min.)~}~^, ~}"
		      (mapcar #'(lambda (item)
				  (list (fmi-observations:temperature item)
					(minutes-ago (fmi-observations:observation-time item))))
			      last-observation)) location-source))))

(defun format-wind-short-text (observations location-source)
  (let* ((item-count (length observations))
	 (last-observation (car (last observations)))
	 (last-twentyfour (if (> (length observations) 23)
			      (subseq observations (- item-count 24))
			      observations))
	 (twentyfour-observations (mapcar #'fmi-observations:windspeed last-twentyfour))
	 (twentyfour-directions (mapcar #'fmi-observations:wind-direction last-twentyfour))
	 (directions-line (directions-line twentyfour-directions)))
    (multiple-value-bind (sparkline min max)
	(sparkline twentyfour-observations)
      (format nil "~A: [~A, ~A] ~A; ~A; ~A (~A)."
	      (format-station-place-name (first observations))
	      min max sparkline directions-line
	      (format nil "~A m/s, ~A° (-~A min.)"
		      (fmi-observations:windspeed last-observation)
		      (fmi-observations:wind-direction last-observation)
		      (minutes-ago (fmi-observations:observation-time last-observation)))
	      location-source))))

(defun format-gusts-short-text (observations location-source)
  (let* ((item-count (length observations))
	 (last-observation (car (last observations)))
	 (last-twentyfour (if (> (length observations) 23)
			      (subseq observations (- item-count 24))
			      observations))
	 (twentyfour-observations (mapcar #'fmi-observations:wind-gusts last-twentyfour))
	 (twentyfour-directions (mapcar #'fmi-observations:wind-direction last-twentyfour))
	 (directions-line (directions-line twentyfour-directions)))
    (multiple-value-bind (sparkline min max)
	(sparkline twentyfour-observations)
      (format nil "~A: [~A, ~A] ~A; ~A; ~A (~A)."
	      (format-station-place-name (first observations))
	      min max sparkline directions-line
	      (format nil "~A m/s, ~A° (-~A min.)"
		      (fmi-observations:wind-gusts last-observation)
		      (fmi-observations:wind-direction last-observation)
		      (minutes-ago (fmi-observations:observation-time last-observation)))
	      location-source))))

;; (defun paikkis-geocode (place-name)
;;   (let ((place place-name)
;; 	(municipality nil))

;;     (when (some #'(lambda (x) (char-equal #\, x)) place-name)
;;       (destructuring-bind (p m) (cl-ppcre:split "," place-name)
;; 	(setf place p)
;; 	(setf municipality m)))

;;     (let* ((parameters
;; 	   (if municipality
;; 	       (list (cons "filter" (string-trim " " place))
;; 		     (cons "municipality" (string-trim " " municipality)))
;; 	       (list (cons "filter" (string-trim " " place)))))
;; 	   (stream
;; 	    (flexi-streams:make-flexi-stream
;; 	     (drakma:http-request "http://api.paikkis.fi/v1/pois.json" :want-stream t
;; 				  :parameters (cons '("resultcount" . 1) parameters))
;; 	     :external-format (flexi-streams:make-external-format :utf-8)))
;; 	   (alist (cl-json:decode-json stream)))
;;       (values alist parameters))))

(defun nominatim-geocode (place-name)
  (let* ((url
	 (format nil "http://nominatim.openstreetmap.org/search/~A?format=json&limit=1"
		 (drakma:url-encode place-name :utf-8)))
	 (stream
	  (flexi-streams:make-flexi-stream
	   (drakma:http-request url :want-stream t
				:user-agent "urheilujuhla IRC bot (+http://github.com/guaq/urheilujuhla/)")
	   :external-format (flexi-streams:make-external-format :utf-8)))
	 (alist (cl-json:decode-json stream)))
    alist))

(defun nearest-weather-station-id (lat lon &key (second nil))
  (let ((nearest)
	(nearest-distance)
	(second-nearest))

    (flet ((distance (lat1 lon1 lat2 lon2)
	     (sqrt (+
		    (* (abs (- lat1 lat2)) (abs (- lat1 lat2)))
		    (* (abs (- lon1 lon2)) (abs (- lon1 lon2)))))))

      (dolist (item *location-to-fmisid*)
	(let ((dist (distance (caar item) (cadar item) lat lon)))
	  (when (or
		 (eq nearest nil)
		 (< dist nearest-distance))
	    (progn
	      (setf second-nearest nearest)
	      (setf nearest item)
	      (setf nearest-distance dist)))))

      (if second
	  (car (last second-nearest))
	  (car (last nearest))))))

(defun formatted-top-temperatures (ignored-arg &key (bottom nil) (count 5))
  (check-type count integer)
  (let
      ((comparator
	(if bottom #'< #'>))
       (observations (fmi-observations:observations
		      (fmi-observations:make-bbox-criterion 20 60 30 70)
		      :time-step 5
		      :start-time (local-time:timestamp- (local-time:now) 30 :minute)))
       (stations (make-hash-table :test 'equal))
       (latest-non-nil))
		    
    (iter (for item in observations)
	  (push item
		(gethash (format nil "~A-~A"
				 (fmi-observations:station-region (fmi-observations:station item))
				 (fmi-observations:station-name (fmi-observations:station item)))
			 stations)))

    (iter (for (key value) in-hashtable stations)
	  (push (car (sort (remove-if #'null value :key 'fmi-observations:temperature)
			   #'local-time:timestamp> :key 'fmi-observations:observation-time))
		latest-non-nil))
    
    (let ((out
	   (subseq
	    (sort (remove-if #'null latest-non-nil) comparator :key 'fmi-observations:temperature)
	    0 count)))

      (format nil "~{~{~A: ~A°C (-~A min.)~}~^; ~}"
	      (mapcar #'(lambda (item)
			  (list
			   (format-station-place-name item)
			   (fmi-observations:temperature item)
			   (minutes-ago (fmi-observations:observation-time item))))
	      out)))))

(defun formatted-median (ignored-arg &key (count 5))
  (check-type count integer)
  (let
      ((observations (fmi-observations:observations
		      (fmi-observations:make-bbox-criterion 20 60 30 70)
		      :time-step 5
		      :start-time (local-time:timestamp- (local-time:now) 30 :minute)))
       (stations (make-hash-table :test 'equal))
       (latest-non-nil))

    (iter (for item in observations)
	  (push item
		(gethash (format nil "~A-~A"
				 (fmi-observations:station-region (fmi-observations:station item))
				 (fmi-observations:station-name (fmi-observations:station item))) stations)))

    (iter (for (key value) in-hashtable stations)
	  (push (car (sort (remove-if #'null value :key 'fmi-observations:temperature)
			   #'local-time:timestamp> :key 'fmi-observations:observation-time))
		latest-non-nil))
    
    (let* ((stations (sort (remove-if #'null latest-non-nil) #'< :key 'fmi-observations:temperature))
	   (station-count (length stations))
	   (median (floor (/ station-count 2)))
	   (start (max 0 (- median (ceiling (/ count 2)))))
	   (end (min (- station-count 1) (+ median (floor (/ count 2)))))
	   (out 
	    (subseq stations start end)))

      (format nil "~{~{~A: ~A°C (-~A min.)~}~^; ~}"
	      (mapcar #'(lambda (item)
			  (list
			   (format-station-place-name item)
			   (fmi-observations:temperature item)
			   (minutes-ago (fmi-observations:observation-time item))))
	      out)))))

(define-condition station-not-found-error () ())

(defun observations-fmi (place-name)
  (handler-case
      (values
       (fmi-observations:observations (fmi-observations:make-place-name-criterion place-name)
				      :time-step 30
				      :start-time (local-time:timestamp- (local-time:now) 12 :hour))
       "FMI")
    (fmi-observations:no-stations-error ())))

(defun observations-nominatim (place-name &key (second nil))
  (let
      ((place (first (nominatim-geocode place-name))))
    (when (= (length place) 0)
      (error 'station-not-found-error))

    (let ((lat (parse-number:parse-number (cdr (assoc :lat place))))
	  (lon (parse-number:parse-number (cdr (assoc :lon place)))))
      (when (eq lat nil)
	(error 'station-not-found-error))

      (handler-case
	  (let ((observations (fmi-observations:observations
			       (fmi-observations:make-fmisid-criterion
				(parse-integer
				 (nearest-weather-station-id lat lon :second second)))
			       :time-step 30
			       :start-time (local-time:timestamp- (local-time:now) 12 :hour))))
	    (values observations "OSM Nominatim"))
	(fmi-observations:no-stations-error () (error 'station-not-found-error))))))

;; (defun observations-paikkis (place-name &key (second nil))
;;   (let
;;       ((place (first (paikkis-geocode place-name))))
;;     (when (= (length place) 0)
;;       (error 'station-not-found-error))

;;     (let ((lat (cdr (assoc :lat place)))
;; 	  (lon (cdr (assoc :lon place))))
;;       (when (eq lat nil)
;; 	(error 'station-not-found-error))

;;       (handler-case
;; 	  (let ((observations (fmi-observations:observations
;; 			       (fmi-observations:make-fmisid-criterion
;; 				(parse-integer
;; 				 (nearest-weather-station-id lat lon :second second)))
;; 			       :time-step 30
;; 			       :start-time (local-time:timestamp- (local-time:now) 12 :hour))))
;; 	    (values observations "Paikkis"))
;; 	(fmi-observations:no-stations-error () (error 'station-not-found-error))))))

(defun observations-biomine (place-name)
  (let
      ((resolved
	(cond
	  ((string= (string-downcase place-name) "bc") "Otaniemi")
	  ((string= (string-downcase place-name) "torstinpyhtää") "Espoonlahti"))))
    (when (not (null resolved))
      (return-from observations-biomine (values (observations-nominatim resolved) "Biomine")))
    (error 'station-not-found-error)))

(defun observations-good-p (observations slot-accessor)
  (cond
    ((null observations) nil)
    ((> (length observations) 0)
     (some #'(lambda (x) (not (null x)))
      (mapcar #'(lambda (item) (apply slot-accessor (list item))) observations)))))

(defun observations-cascading (place-name slot-accessor)
  "Returns a pair of (observations geocoding-service)."
  (check-type place-name string)

  (dolist (fun
	    (list #'observations-biomine
		  #'observations-fmi
		  #'observations-nominatim
		  #'(lambda (place-name) (observations-nominatim place-name :second t))))
		  ;; #'observations-paikkis))
    (handler-case
	(multiple-value-bind
	      (observations source)
	    (apply fun (list place-name))
	  (when (observations-good-p observations slot-accessor)
	    (return-from observations-cascading (values observations source))))
      (urheilujuhla::station-not-found-error ())))
  "?¿")

(defmacro check-place-name (place-name function)
  `(progn
     (when (eq nil ,place-name)
       (return-from ,function "Paikan nimi vaaditaan."))
     (check-type ,place-name string)))

(defun formatted-temperature (place-name)
  (check-place-name place-name formatted-temperature)
  (handler-case
      (multiple-value-bind (observations geocoding-method)
	  (observations-cascading place-name #'fmi-observations:temperature)
	(format-short-text observations geocoding-method))
    (station-not-found-error () (format nil "Ei pystytty paikantamaan nimeä '~a'." place-name))))

(defun formatted-wind (place-name)
  (check-place-name place-name formatted-wind)
  (handler-case
      (multiple-value-bind (observations geocoding-method)
	  (observations-cascading place-name #'fmi-observations:windspeed)
	(format-wind-short-text observations geocoding-method))
    (station-not-found-error () (format nil "Ei pystytty paikantamaan nimeä '~a'." place-name))))

(defun formatted-gusts (place-name)
  (check-place-name place-name formatted-gusts)
  (handler-case
      (multiple-value-bind (observations geocoding-method)
	  (observations-cascading place-name #'fmi-observations:wind-gusts)
	(format-gusts-short-text observations geocoding-method))
    (station-not-found-error () (format nil "Ei pystytty paikantamaan nimeä '~a'." place-name))))

(defun handle-irc-message (message)
  (with-slots (irc:source irc:arguments) message
    (let* ((from-channel (if (cl-ppcre:all-matches "^[#\!].*$" (first irc:arguments)) 
			    (first irc:arguments) nil))
	   (from-person (if (cl-ppcre:all-matches "^[^#^\!].*$" (first irc:arguments))
			    (first irc:arguments) nil))
	   (message-proper (second irc:arguments))
	   (first-word (string-trim ",:;"
				    (first
				     (cl-ppcre:split " "
						     (string-trim " " message-proper)))))
	   (rest-words (first (rest (cl-ppcre:split " "
						    (string-trim " "
								 message-proper)
						    :limit 2)))))

      (flet
	  ((handle-call (stimulus-function &optional args)
	     (log-message "Stimulus function ~a called with arguments: ~a" stimulus-function args)
	     (handler-case (if args
			       (funcall stimulus-function args)
			       (funcall stimulus-function nil))
	       (error (e)
		 (log-message "Failed to respond to stimulus for ~a (~a)" args e)
		 (format nil "Virhe: ~a (~a)" e (class-of e))))))

	(let ((message
	       (cond
		 ((string= first-word "LÄMPÖ")
		  (handle-call #'formatted-temperature rest-words))
		 ((string= first-word "TOP")
		  (handle-call #'formatted-top-temperatures))
		 ((string= first-word "HUIPUT")
		  (handle-call #'formatted-top-temperatures))
		 ((string= first-word "MEDIAN")
		  (handle-call #'formatted-median))
		 ((string= first-word "MEDIAANI")
		  (handle-call #'formatted-median))
		 ((string= first-word "BOTTOM")
		  (handle-call #'(lambda (ignored-arg) (formatted-top-temperatures ignored-arg :bottom t))))
		 ((string= first-word "TUULI")
		  (handle-call #'formatted-wind rest-words))
		 ((string= first-word "PUUSKAT")
		  (handle-call #'formatted-gusts rest-words)))))

	  (unless (null message)
	    (bt:with-lock-held (*queue-lock*)
	      (if from-person
		  (push (list irc:source message) *to-irc*)
		  (push (list from-channel (format nil "~A, ~A" irc:source message)) *to-irc*)))))))))

;;;
;;; Command line options
;;;
(net.didierverna.clon:defsynopsis ()
  (text :contents "This is Urheilujuhla.")
  (group (:header "Immediate exit options:")
	 (flag :short-name "h" :long-name "help"
	       :description "Print this help and exit."))
  (group (:header "IRC settings:")
	 (stropt :long-name "irc-host")
	 (stropt :long-name "irc-port" :default-value "6667")
	 (stropt :long-name "irc-nick")
	 (stropt :long-name "irc-username" :default-value "urheilujuhla")
	 (stropt :long-name "irc-realname" :default-value "UJ")
	 (stropt :long-name "irc-channels"))
  (group (:header "SWANK settings:")
	 (switch :long-name "enable-swank"
		 :description "Enable SWANK (for development)")
	 (stropt :long-name "swank-port")))

(defun extract-station-location-to-fmisid-mapping ()
  (let ((all-station-observations
	 (remove-if-not
	  #'(lambda (observation)
	      (or (not (null (fmi-observations:temperature observation)))
		  (not (null (fmi-observations:windspeed observation)))))
	  (fmi-observations:observations (fmi-observations:make-bbox-criterion 20 58 30 70)
					 :time-step 60
					 :start-time (local-time:timestamp- (local-time:now) 1 :hour)))))
    (mapcar #'(lambda (observation)
		(let ((station (fmi-observations:station observation)))
		  (list
		   (list (fmi-observations:y (fmi-observations:station-location station))
			 (fmi-observations:x (fmi-observations:station-location station)))
		   (fmi-observations:station-fmi-id station))))
	    all-station-observations)))

(defun make-location-to-fmisid-mapping-thread ()
  "This runs periodically."
  (bt:make-thread #'(lambda ()
		      (loop do
			   (handler-case
			       (progn
				 (setf *location-to-fmisid*
				       (extract-station-location-to-fmisid-mapping))
				 (log-message
				  "location-to-fmisid mapping loaded, ~A locations."
				  (length *location-to-fmisid*))
				 (sleep 3600))
			     (error (e)
			       (log-message
				"Encountered error while loading focation-to-fmisid mapping: '~A', stack:~%." e)
			       (log-backtrace)
			       (sleep 120)))))
		  :name "location-to-fmisid-thread"))

(defun main ()
  (net.didierverna.clon:make-context)
  (when (net.didierverna.clon:getopt :short-name "h")
    (net.didierverna.clon:help)
    (net.didierverna.clon:exit))

  (let ((irc-host (net.didierverna.clon:getopt :long-name "irc-host"))
	(irc-port (net.didierverna.clon:getopt :long-name "irc-port"))
	(irc-nick (net.didierverna.clon:getopt :long-name "irc-nick"))
	(irc-username (net.didierverna.clon:getopt :long-name "irc-username"))
	(irc-channels (net.didierverna.clon:getopt :long-name "irc-channels"))
	(irc-realname (net.didierverna.clon:getopt :long-name "irc-realname")))

    (unless (and irc-host irc-port irc-nick irc-username irc-realname irc-channels)
      (log-message "All IRC options are required!")

      (dolist (arg (list (list "irc-host" irc-host)
			 (list "irc-port" irc-port)
			 (list "irc-nick" irc-nick)
			 (list "irc-username" irc-username)
			 (list "irc-realname" irc-realname)
			 (list "irc-channels" irc-channels))
	       (log-message " ~A: ~A" (first arg) (second arg))))
      (net.didierverna.clon:help)
      (net.didierverna.clon:exit))

    (if (net.didierverna.clon:getopt :long-name "enable-swank")
	(progn
	  (log-message "Enabling SWANK...")
	  (let ((port 14005)
		(port-string (net.didierverna.clon:getopt :long-name "swank-port")))
	    (when port-string
	      (setf port (parse-integer port-string)))
	    (swank:create-server :port port :dont-close t)))
	(log-message "Not starting SWANK."))

    (setf *irc-channels* (cl-ppcre:split "," irc-channels))
    (setf *irc-thread*
	  (make-irc-thread
	   irc-host (parse-integer irc-port) irc-nick irc-username irc-realname))
    (setf *processing-thread* (make-processing-thread))

    (make-location-to-fmisid-mapping-thread)

    (log-message "Running...")
    (let ((uptime 0))
      (loop do
	   (log-message "Uptime: ~A seconds." uptime)
	   (sleep 300)
	   (incf uptime 300)))))

(defun test-main ()
  (let ((fmisid-mapping-thread (make-location-to-fmisid-mapping-thread)))
    (log-message "Running...")
    (let ((uptime 0))
      (loop do
	   (log-message "Uptime: ~A seconds." uptime)
	   (sleep 1)
	   (incf uptime 1)))))
