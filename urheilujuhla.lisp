(defpackage #:urheilujuhla
  (:use
   #:common-lisp

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
(defvar *irc-react-to-handles* nil)

(defvar *queue-lock* (bt:make-lock "queue-lock"))
(defvar *queues-updated* (bt:make-condition-variable :name "queues-updated"))
(defvar *from-irc* '())
(defvar *to-irc* '())
(defvar *from-object-system* '())
(defvar *to-object-system* '())

;;;
;;; IRC
;;;
(defun handle-irc-privmsg (message)
  (bt:with-lock-held (*queue-lock*)
    (push message *from-irc*)
    (bt:condition-notify *queues-updated*)))

(defun connect-to-irc-server (host port nick username realname)
  (setf *irc-connection* (irc:connect :server host :port port :nickname nick
				      :username username :realname realname))
  (when *irc-connection*
    (remove-hooks *irc-connection* 'irc-privmsg-message))
  
  (add-hook *irc-connection* 'irc-privmsg-message #'handle-irc-privmsg)

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
				  (handle-irc-message msg)))))



       (loop while (> (length *from-object-system*) 0) do
	    (format t "OBJECT-SYSTEM> ~A~%" (pop *from-object-system*)))

       ;; (format t "Done processing queues.~%")
       (force-output)
       (bt:release-lock *queue-lock*)))

(defun start-processing-thread ()
  (bt:make-thread #'run-processing-thread :name "processing-thread"))

(defvar +date-format+
  '((:day 2) #\. (:month 2) #\. #\Space (:HOUR 2) #\: (:MIN 2) #\Space #\( :gmt-offset #\)))

;; Sparklines in Common Lisp;
;;  see <http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0001OR"> for reference.
(defun sparkline (seq)
  (let* ((sorted (sort (copy-seq seq) #'<))
	 (min (elt sorted 0))
	 (max (elt sorted (- (length sorted) 1))))
	 ;; (a / ((aMax - aMin) / (max - min))) + min

    (let ((spark-chars #(#\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█)))
      (with-output-to-string (out)
	(loop for elem being the elements of seq
	   do
	     (format out "~a"
		     (elt spark-chars
			  (floor (/ (- elem min)
				    (/
				     (- max min)
				     (- (length spark-chars) 1)))))))
	out))))

(defun format-last-observation (region location observations)
  (format nil "~A, ~A: ~A @ ~A" region location
	  (cadar (last observations))
	  (local-time:format-timestring nil (caar (last observations)) :format +date-format+)))


(defun minutes-ago (timestamp)
  (round (/ (float (- (local-time:timestamp-to-universal (local-time:now))
		      (local-time:timestamp-to-universal timestamp))) 60)))

(defun format-last-three-observations (region location observations)
  (let* ((non-nil-observations (remove-if-not #'cadr observations))
	 (item-count (length non-nil-observations))
	 (last-three (subseq non-nil-observations (- item-count 3)))
	 (last-twentyfour (subseq non-nil-observations (- item-count 24)))
	 (sparkline (sparkline (mapcar #' car (mapcar #'last last-twentyfour)))))
    (format nil "~A, ~A: ~A; ~A." region location sparkline
	    (format nil "~{~{~A°C (~A min. sitten)~}~^, ~}"
		    (mapcar #'(lambda (item)
				(list (car (last item))
				      (minutes-ago (first item))))
			    last-three)))))

(defun resolve-place-name-coordinates (place-name)
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

      (dolist (item fmi-observations:*location-to-fmisid*)
	(let ((dist (distance (caar item) (cadar item) lat lon)))
	  (when (or
		 (eq nearest nil)
		 (< dist nearest-distance))
	    (progn
	      (setf nearest item)
	      (setf nearest-distance dist)))))

      (car (last nearest)))))


(defun formatted-weather-using-paikkis (place-name)
  (let*
      ((place (first (resolve-place-name-coordinates place-name)))
       (lat (cdr (assoc :lat place)))
       (lon (cdr (assoc :lon place))))

    (when (eq lat nil)
      (return-from formatted-weather-using-paikkis nil))

    (destructuring-bind (region location observations)
	(fmi-observations:get-station-observations (nearest-weather-station-id lat lon))
      (if (eq nil region)
	  nil
	  (format-last-three-observations region location observations)))))

(defun formatted-weather-using-fmi-weather (place-name)
  (destructuring-bind (region location observations)
      (fmi-observations:get-weather place-name)
    (if (eq nil region)
	nil
	(format-last-three-observations region location observations))))

(defun formatted-weather (place-name)
  (when place-name
    (let ((weather (formatted-weather-using-fmi-weather place-name)))
      (when (eq nil weather)
	(setf weather (formatted-weather-using-paikkis place-name)))
      (if weather
	  (return-from formatted-weather weather)
	  (format nil "Paikkaa ~A ei löytynyt." place-name))))
  "?¡")


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

      (when (member first-word *irc-react-to-handles* :test #'string-equal)
	(bt:with-lock-held (*queue-lock*)
	  (if from-person
	      (push (list source
			  (formatted-weather rest-words)) *to-irc*)
	      (push (list from-channel
			  (format nil "~A, ~A"
				  source (formatted-weather rest-words)))
			  *to-irc*)))))))


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
	 (stropt :long-name "swank-port")))

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

    (setf *irc-react-to-handles* (list irc-nick "sää" "uj"))

    (setf *irc-channels* (cl-ppcre:split "," irc-channels))
    (setf *irc-thread*
	  (start-irc-thread irc-host
			    (parse-integer irc-port)
			    irc-nick irc-username irc-realname))
    ;; (setf *testing-thread* (start-testing-thread))
    (setf *processing-thread* (start-processing-thread))


    (format t ";; Running...~%")
    (let ((uptime 0))
      (loop do
	   (format t "Uptime: ~A seconds~%" uptime)
	   (force-output)
	   (sleep 300)
	   (incf uptime 300)))))
