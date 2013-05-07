(asdf:defsystem #:urheilujuhla
  :depends-on (#:bordeaux-threads
	       #:flexi-streams
	       #:babel
	       #:alexandria
	       #:iterate

	       #:object-system
	       #:lift
	       #:cl-irc

	       #:com.dvlsoft.clon
	       #:swank

	       #:fmi-observations
	       #:local-time)
  :components ((:file "urheilujuhla")
	       (:file "tests")))
