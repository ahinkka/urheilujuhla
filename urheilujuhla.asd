(asdf:defsystem #:urheilujuhla
  :depends-on (#:bordeaux-threads
	       #:flexi-streams
	       #:babel
	       #:alexandria
	       #:iterate
	       #:drakma
	       #:parse-number

	       #:object-system
	       #:lift
	       #:cl-irc

	       #:net.didierverna.clon
	       #:swank

	       #:fmi-observations
	       #:local-time)
  :components ((:file "urheilujuhla")
	       (:file "tests")))
