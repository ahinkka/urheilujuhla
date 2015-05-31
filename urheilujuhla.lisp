(defpackage #:urheilujuhla
  (:use
   #:common-lisp
   #:iterate

   #:cl-irc
   #:object-system)
  (:export :main))

(in-package :urheilujuhla)

(defvar *irc-thread*)
(defvar *irc-sender-thread*)
(defvar *object-system-thread*)
(defvar *processing-thread*)

(defvar *irc-connection*)
(defvar *irc-channels*)
(defvar *irc-sender-should-stop* nil)

(defvar *queue-lock* (bt:make-lock "queue-lock"))
(defvar *queues-updated* (bt:make-condition-variable :name "queues-updated"))
(defvar *from-irc* '())
(defvar *to-irc* '())
(defvar *from-object-system* '())
(defvar *to-object-system* '())

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
  (format *error-output* ";; In HANDLE-IRC-ERROR~%")
  (describe message *error-output*)
  (force-output *error-output*)

  (with-slots (arguments) message
    (let ((message-proper (car arguments)))
      (when 
	  (or
	   (search "ping timeout" (string-downcase message-proper))
	   (search "closing link" (string-downcase message-proper)))
	(format *error-output* ";; Error with message ~A detected.~%" message-proper)
	(force-output *error-output*)

	(format *error-output* ";; Quitting the IRC connection ~A.~%" *irc-connection*)
	(force-output *error-output*)
	(irc:quit *irc-connection*)

	(format *error-output* ";; Signaling an error to re-establish connection.~%")
	(force-output *error-output*)
	(error 'irc-connection-error :description message-proper)))))

(defun connect-to-irc-server (host port nick username realname)
  (setf *irc-connection* (irc:connect :server host :port port :nickname nick
				      :username username :realname realname))
  (when *irc-connection*
    (remove-hooks *irc-connection* 'irc-privmsg-message))
  
  (add-hook *irc-connection* 'irc-privmsg-message #'handle-irc-privmsg)
  (add-hook *irc-connection* 'irc-error-message #'handle-irc-error)

  (dolist (channel *irc-channels*)
    (format *error-output* ";; Joining channel ~A~%" channel)
    (force-output *error-output*)
    (irc:join *irc-connection* channel))

  (setf *irc-sender-thread* (start-irc-sender-thread))

  (irc:read-message-loop *irc-connection*))

(defun run-irc-thread (host port nick username realname)
  (loop do
       (handler-case
	   (connect-to-irc-server host port nick username realname)
	 (irc-connection-error (err)
	   (format *error-output* ";; Connection failed due to: ~A~%" err))
	 (usocket:socket-error (err)
	   (format *error-output* ";; Connection failed due to network issue: ~A~%" err)))

       (setf *irc-sender-should-stop* t)

       (format *error-output* ";; Thread ~A sleeping for 30 seconds...~%"
	       (bt:thread-name (bt:current-thread)))
       (force-output *error-output*)
       (sleep 30)))

(defun start-irc-thread (host port nick username realname)
  (bt:make-thread #'(lambda () (run-irc-thread host port nick username realname))
		  :name "irc-thread"))

(defun run-irc-sender-thread ()
  (loop do
       (when *irc-sender-should-stop*
	 (format *error-output* ";; IRC sender thread stopping~%")
	 (setf *irc-sender-should-stop* nil)
	 (return-from run-irc-sender-thread))

       (bt:with-lock-held (*queue-lock*)
	 (when (> (length *to-irc*) 0)
	   (let ((popped (pop *to-irc*)))
	     (when (and (first popped) (second popped))
	       (format t "IRC << ~A~%" popped)
	       (apply #'irc:privmsg (list *irc-connection* (first popped) (second popped)))))))
       (sleep 1)))


(defun start-irc-sender-thread ()
  (bt:make-thread #'run-irc-sender-thread :name "irc-sender-thread"))


;;;
;;; Testing (without IRC connection...)
;;;
(defun run-testing-thread ()
  (loop do
       (bt:with-lock-held (*queue-lock*)
	 (let ((val (random 100)))
	   (format t "Inserting ~A...~%" val)
	   (push val *from-irc*)
	   (bt:condition-notify *queues-updated*)))
       (sleep 3)))

(defun start-testing-thread ()
  (bt:make-thread #'run-testing-thread :name "testing-thread"))


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



       (loop while (> (length *from-object-system*) 0) do
	    (format t "OBJECT-SYSTEM> ~A~%" (pop *from-object-system*)))

       ;; (format t "Done processing queues.~%")
       (force-output)
       (bt:release-lock *queue-lock*)))

(defun start-processing-thread ()
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
    (if (= 0 (search region name :test #'char=))
	(format nil "~A, ~A" (subseq name (+ (length region) 1)) region)
	(format nil "~A, ~A" name region))))

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

(defun format-wind-gusts-short-text (observations location-source)
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

(defun paikkis-geocode (place-name)
  (let ((place place-name)
	(municipality nil))

    (when (some #'(lambda (x) (char-equal #\, x)) place-name)
      (destructuring-bind (p m) (cl-ppcre:split "," place-name)
	(setf place p)
	(setf municipality m)))

    (let* ((url
	   (if municipality
	       (format nil "http://api.paikkis.fi/v1/pois.json?filter=~A&municipality=~A&resultcount=1"
		       (drakma:url-encode (string-trim " " place) :utf-8)
		       (drakma:url-encode (string-trim " " municipality) :utf-8))
	       (format nil "http://api.paikkis.fi/v1/pois.json?filter=~A&resultcount=1"
		       (drakma:url-encode (string-trim " " place) :utf-8))))
	   (stream
	    (flexi-streams:make-flexi-stream
	     (drakma:http-request url :want-stream t)
	     :external-format (flexi-streams:make-external-format :utf-8)))
	   (alist (cl-json:decode-json stream)))
      (values alist url))))

(defun nearest-weather-station-id (lat lon)
  (let ((nearest)
	(nearest-distance))

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
	      (setf nearest item)
	      (setf nearest-distance dist)))))

      (car (last nearest)))))

(defun formatted-top-temperatures (&key bottom (count 5))
  (let
      ((comparator
	(if bottom #'< #'>))
       (observations (fmi-observations:observations
		      (fmi-observations:make-bbox-criterion 20 60 30 70)
		      :time-step 1 :time-step-count 1))
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

(defun formatted-median (&key (count 5))
  (let
      ((observations (fmi-observations:observations
		      (fmi-observations:make-bbox-criterion 20 60 30 70)
		      :time-step 1 :time-step-count 1))
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

(defun observations-cascading (place-name)
  "Returns a pair of (observations geocoding-service)."
  (handler-case
      (let ((observations (fmi-observations:observations (fmi-observations:make-place-name-criterion place-name))))
	(return-from observations-cascading (values observations "FMI")))
    (fmi-observations:no-stations-error ()))

  (let*
      ((place (first (paikkis-geocode place-name)))
       (lat (cdr (assoc :lat place)))
       (lon (cdr (assoc :lon place))))

    (when (eq lat nil)
      (error 'station-not-found-error))

    (handler-case
	(let ((observations (fmi-observations:observations
			     (fmi-observations:make-fmisid-criterion (parse-integer (nearest-weather-station-id lat lon))))))
	  (values observations "Paikkis"))
      (fmi-observations:no-stations-error ()))))

(defun formatted-temperature (place-name)
  (when (eq nil place-name)
    (return-from formatted-temperature "Paikan nimi vaaditaan."))

  (handler-case
      (multiple-value-bind (observations geocoding-method)
	  (observations-cascading place-name)
	(format-short-text observations geocoding-method))
    (station-not-found-error () (format nil "Ei pystytty paikantamaan nimeä '~a'." place-name))))

(defun formatted-wind (place-name)
  (when (eq nil place-name)
    (return-from formatted-wind "Paikan nimi vaaditaan."))

  (handler-case
      (multiple-value-bind (observations geocoding-method)
	  (observations-cascading place-name)
	(format-wind-short-text observations geocoding-method))
    (station-not-found-error () (format nil "Ei pystytty paikantamaan nimeä '~a'." place-name))))

(defun formatted-gusts (place-name)
  (when (eq nil place-name)
    (return-from formatted-gusts "Paikan nimi vaaditaan."))

  (handler-case
      (multiple-value-bind (observations geocoding-method)
	  (observations-cascading place-name)
	(format-gusts-short-text observations geocoding-method))
    (station-not-found-error () (format nil "Ei pystytty paikantamaan nimeä '~a'." place-name))))

(defun handle-irc-message (message)
  (with-slots (source arguments) message
    (let* ((from-channel (if (cl-ppcre:all-matches "^[#\!].*$" (first arguments)) 
			    (first arguments) nil))
	   (from-person (if (cl-ppcre:all-matches "^[^#^\!].*$" (first arguments))
			    (first arguments) nil))
	   (message-proper (second arguments))
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
	     (handler-case (if args
			       (funcall stimulus-function args)
			       (funcall stimulus-function))
	       (error (e)
		(format *error-output* "Failed to respond to stimulus for ~a (~a)" args e)
		(format nil "Virhe: ~a" (class-of e))))))
	       

	(let ((message 
	       (cond
		 ((string= first-word "LÄMPÖ")
		  (handle-call #'formatted-temperature rest-words))
		 ((string= first-word "TOP")
		  (handle-call #'formatted-top-temperatures))
		 ((string= first-word "MEDIAN")
		  (handle-call #'formatted-median))
		 ((string= first-word "BOTTOM")
		  (handle-call #'(lambda () (formatted-top-temperatures :bottom t))))
		 ((string= first-word "TUULI")
		  (handle-call #'formatted-wind rest-words))
		 ((string= first-word "PUUSKAT")
		  (handle-call #'formatted-gusts rest-words)))))

	  (unless (null message)
	    (bt:with-lock-held (*queue-lock*)
	      (if from-person
		  (push (list source message) *to-irc*)
		  (push (list from-channel (format nil "~A, ~A" source message)) *to-irc*)))))))))


;;;
;;; Command line options
;;;
(com.dvlsoft.clon:defsynopsis (:postfix "FILES...")
  (text :contents "This is Urheilujuhla.")
  (group (:header "Immediate exit options:")
	 (flag :short-name "h" :long-name "help"
	       :description "Print this help and exit."))
  (group (:header "IRC settings:")
	 (stropt :long-name "irc-host")
	 (stropt :long-name "irc-port")
	 (stropt :long-name "irc-nick")
	 (stropt :long-name "irc-username")
	 (stropt :long-name "irc-realname")
	 (stropt :long-name "irc-channels"))
  (group (:header "SWANK settings:")
	 (switch :long-name "enable-swank"
		 :description "Enable SWANK (for development)")
	 (stropt :long-name "swank-port"))
  (group (:header "FMI API settings:")
	 (stropt :long-name "fmi-api-key"
		 :description "API-key for FMI API (required)")))

(defun extract-station-location-to-fmisid-mapping ()
  (let ((all-station-observations
	 (fmi-observations:observations (fmi-observations:make-bbox-criterion 20 58 30 70)
					:time-step-count 1)))
    (mapcar #'(lambda (observation)
		(let ((station (fmi-observations:station observation)))
		  (list
		   (list (fmi-observations:y (fmi-observations:station-location station))
			 (fmi-observations:x (fmi-observations:station-location station)))
		   (fmi-observations:station-fmi-id station))))
	    all-station-observations)))

(defun start-location-to-fmisid-mapping-thread ()
  "This only runs once at startup. It's just so slow. :("
  (bt:make-thread #'(lambda ()
		      (progn
			(setf *location-to-fmisid*
			      (extract-station-location-to-fmisid-mapping))
			(format *error-output*
				";; location-to-fmisid mapping loaded, ~A locations~%" (length *location-to-fmisid*))
			(terpri *error-output*)))
		  :name "location-to-fmisid-thread"))

(defun main ()
  (com.dvlsoft.clon:make-context)
  (when (com.dvlsoft.clon:getopt :short-name "h")
    (com.dvlsoft.clon:help)
    (com.dvlsoft.clon:exit))

  (let ((irc-host (com.dvlsoft.clon:getopt :long-name "irc-host"))
	(irc-port (com.dvlsoft.clon:getopt :long-name "irc-port"))
	(irc-nick (com.dvlsoft.clon:getopt :long-name "irc-nick"))
	(irc-username (com.dvlsoft.clon:getopt :long-name "irc-username"))
	(irc-channels (com.dvlsoft.clon:getopt :long-name "irc-channels"))
	(irc-realname (com.dvlsoft.clon:getopt :long-name "irc-realname")))

    (unless (and irc-host irc-port irc-nick irc-username irc-realname irc-channels)
      (format *error-output* "All IRC options are required!~%")

      (dolist (arg (list (list "irc-host" irc-host)
			 (list "irc-port" irc-port)
			 (list "irc-nick" irc-nick)
			 (list "irc-username" irc-username)
			 (list "irc-realname" irc-realname)
			 (list "irc-channels" irc-channels))
	(format t " ~A: ~A~%" (first arg) (second arg))))
      (terpri *error-output*)

      (force-output *error-output*)
      (com.dvlsoft.clon:help)
      (com.dvlsoft.clon:exit))

    (let ((api-key (com.dvlsoft.clon:getopt :long-name "fmi-api-key")))
      (when (eq nil api-key)
	(format *error-output* "FMI API-key required!~%")

	(force-output *error-output*)
	(com.dvlsoft.clon:help)
	(com.dvlsoft.clon:exit))

      (format *error-output* ";; Using ~A as FMI API-key.~%" api-key)
      (setf fmi-observations:*api-key* api-key))

    (if (com.dvlsoft.clon:getopt :long-name "enable-swank")
	(progn
	  (format *error-output* ";; Enabling SWANK...~%")
	  (let ((port 14005)
		(port-string (com.dvlsoft.clon:getopt :long-name "swank-port")))
	    (when port-string
	      (setf port (parse-integer port-string)))
	    (swank:create-server :port port :dont-close t)))
	(format *error-output* ";; Not starting SWANK.~%"))
    (force-output *error-output*)

    (setf *irc-channels* (cl-ppcre:split "," irc-channels))
    (setf *irc-thread*
	  (start-irc-thread irc-host
			    (parse-integer irc-port)
			    irc-nick irc-username irc-realname))
    ;; (setf *testing-thread* (start-testing-thread))
    (setf *processing-thread* (start-processing-thread))

    (start-location-to-fmisid-mapping-thread)

    (format t ";; Running...~%")
    (let ((uptime 0))
      (loop do
	   (format t "Uptime: ~A seconds~%" uptime)
	   (force-output)
	   (sleep 300)
	   (incf uptime 300)))))
