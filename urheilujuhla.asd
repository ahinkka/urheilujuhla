(asdf:defsystem #:urheilujuhla
  :depends-on (#:bordeaux-threads
	       #:flexi-streams
	       #:babel
	       #:alexandria
	       #:iterate
	       #:drakma
	       #:parse-number

	       #:lift
	       #:cl-irc
	       #:cl-json

	       #:net.didierverna.clon
	       #:swank

	       #:fmi-observations
	       #:local-time)
  :components ((:file "urheilujuhla")
	       (:file "tests")))
