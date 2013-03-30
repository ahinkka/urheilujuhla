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
;;; Weather extraction code
;;;
(defun string-to-dom (string)
  (cxml:parse-octets
   (babel:string-to-octets string)
   (cxml-dom:make-dom-builder)))

(defun extract-node-values (dom xpath-expression)
  (xpath:with-namespaces (("gml" "http://www.opengis.net/gml/3.2")
			  ("target" "http://xml.fmi.fi/namespace/om/atmosphericfeatures/0.95")
			  ("gmlcov" "http://www.opengis.net/gmlcov/1.0"))
    (let ((node-set (xpath:evaluate xpath-expression dom)))
      (xpath:map-node-set->list
       #'(lambda (node) (xpath:evaluate "string()" node))
       node-set))))

(defun get-weather-data (place-name)
  (let* ((url
	  (format nil
		  "http://data.fmi.fi/fmi-apikey/~A/wfs?request=getFeature&storedquery_id=fmi::observations::weather::realtime::place::multipointcoverage&place=~A&timestep=30"
		  "b37f3e99-cdb8-4858-b850-bfffea6542f9"
		  (drakma:url-encode place-name :utf-8)))
	 (xml (drakma:http-request url :external-format-out :utf-8 :external-format-in :utf-8))
	 (dom (string-to-dom xml)))
    (values
     (list
      (extract-node-values dom
			   "//target:region[@codeSpace='http://xml.fmi.fi/namespace/location/region']")
      (extract-node-values dom
			   "//gml:name[@codeSpace='http://xml.fmi.fi/namespace/locationcode/name']")
      (extract-node-values dom
			   "//gmlcov:positions")
      (extract-node-values dom
			   "//gml:doubleOrNilReasonTupleList"))
     url)))

(defun weather-string-to-list (string)
  (remove-if
   #'(lambda (line) (= 0 (length line)))
   (mapcar #'(lambda (line)
	       (string-trim " " line))
	   (cl-ppcre:split "\\n" string))))

(defun item-from-weather-string (string position-function)
  (mapcar #'(lambda (line)
	      (funcall position-function (remove-if #'(lambda (item) (= 0 (length item)))
							 (cl-ppcre:split " " line))))
	  (weather-string-to-list string)))

(defun temperature-tuples (weather-positions weather-observations)
  (mapcar #'list
	  (item-from-weather-string weather-positions
				    #'(lambda (item) (local-time:unix-to-timestamp
						      (parse-integer (car (last item))))))
	  (item-from-weather-string weather-observations
				    #'(lambda (item) 
					(handler-case 
					    (parse-number:parse-number (first item))
					  (sb-int:simple-parse-error (s-p-e)
					    (declare (ignore s-p-e))
					    nil))))))

(defun sort-temperature-tuples (tuples)
  (sort tuples
	#'(lambda (x y) (local-time:timestamp< (car x) (car y)))))

(defun get-weather (place-name)
  (let* ((result (get-weather-data place-name))
	 (locations (car (third result)))
	 (observations (car (fourth result))))
    (values
     (car (first result))
     (car (second result))
     (sort-temperature-tuples
      (temperature-tuples
       locations observations)))))

;; (mapcar #'list
;; 	(item-from-weather-string (first *liakka-times*)
;; 				  #'(lambda (item) (local-time:unix-to-timestamp
;; 						    (parse-integer (car (last item))))))
;; 	(item-from-weather-string (first *liakka-observations*)
;; 				  #'(lambda (item) (parse-number:parse-number (first item)))))



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

(defun formatted-weather (place-name)
  (if (not place-name)
      "?¡"
      (multiple-value-bind (region location observations)
	  (fmi-observations:get-weather place-name)
	(if (not region)
	    (format nil "Paikkaa ~A ei löydy!" place-name)
	    (format nil "~A, ~A: ~A @ ~A" region location
		    (cadar (last observations))
		    (caar (last observations)))))))
      

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
	   (rest-words (first (rest (cl-ppcre:split " " message-proper :limit 2)))))
      (declare (ignore from-channel from-person))

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

    (setf *irc-react-to-handles* (list irc-nick))

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
