(defpackage #:urheilujuhla
  (:use
   #:common-lisp

   #:cl-irc
   #:object-system)
  (:export :main))

(in-package :urheilujuhla)

(defvar *irc-thread*)
(defvar *object-system-thread*)
(defvar *processing-thread*)

(defvar *irc-connection*)

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

(defun run-irc-thread (host port nick username realname)
  (setf *irc-connection* (irc:connect :server host :port port :nickname nick
				      :username username :realname realname))
  (add-hook *irc-connection* 'irc-privmsg-message #'handle-irc-privmsg)
  (irc:read-message-loop *irc-connection*))

(defun start-irc-thread (host port nick username realname)
  (bt:make-thread #'(lambda () (run-irc-thread host port nick username realname))
		  :name "irc-thread"))


(defun run-irc-sender-thread ()
  (irc:join *irc-connection* "!biomine")

  (loop do
       (bt:with-lock-held (*queue-lock*)
	 (when (> (length *to-irc*) 0)
	   (let ((popped (pop *to-irc*)))
	     (format t "IRC << ~A~%" (pop *to-irc*)))))
	     ;;(apply #'irc:privmsg (list *irc-connection* (first popped) (second popped)) ; (channel message)
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
	    (format t "IRC >> ~A~%" (pop *from-irc*)))

       (loop while (> (length *from-object-system*) 0) do
	    (format t "OBJECT-SYSTEM> ~A~%" (pop *from-object-system*)))

       ;; (format t "Done processing queues.~%")
       (force-output)
       (bt:release-lock *queue-lock*)))

(defun start-processing-thread ()
  (bt:make-thread #'run-processing-thread :name "processing-thread"))


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
	 (stropt :long-name "irc-realname")))

(defun main ()
  (com.dvlsoft.clon:make-context)
  (when (com.dvlsoft.clon:getopt :short-name "h")
    (com.dvlsoft.clon:help)
    (com.dvlsoft.clon:exit))

  (let ((irc-host (com.dvlsoft.clon:getopt :long-name "irc-host"))
	(irc-port (com.dvlsoft.clon:getopt :long-name "irc-port"))
	(irc-nick (com.dvlsoft.clon:getopt :long-name "irc-nick"))
	(irc-username (com.dvlsoft.clon:getopt :long-name "irc-username"))
	(irc-realname (com.dvlsoft.clon:getopt :long-name "irc-realname")))

    (unless (and irc-host irc-port irc-nick irc-username irc-realname)
      (format *error-output* "All IRC options are required!~%")

      (dolist (arg (list (list "irc-host" irc-host)
			 (list "irc-port" irc-port)
			 (list "irc-nick" irc-nick)
			 (list "irc-username" irc-username)
			 (list "irc-realname" irc-realname)))
	(format t " ~A: ~A~%" (first arg) (second arg)))
      (terpri *error-output*)

      (force-output *error-output*)
      (com.dvlsoft.clon:help)
      (com.dvlsoft.clon:exit))


    (setf *irc-thread* (start-irc-thread irc-host (parse-integer irc-port) irc-nick irc-username irc-realname))
    ;; (setf *testing-thread* (start-testing-thread))
    (setf *processing-thread* (start-processing-thread))

    (swank:create-server :port 14005
			 ;; if non-nil the connection won't be closed
			 ;; after connecting
			 :dont-close nil)

    (format t "Running...~%")
    (let ((uptime 0))
      (loop do
	   (format t "Uptime: ~A seconds~%" uptime)
	   (force-output)
	   (sleep 300)
	   (incf uptime 300)))))
