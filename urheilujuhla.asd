(asdf:defsystem #:urheilujuhla
  :depends-on (#:bordeaux-threads
	       #:flexi-streams
	       #:babel
	       #:lift

	       #:object-system
	       #:cl-irc

	       #:com.dvlsoft.clon
	       #:swank

	       #:fmi-observations
	       #:local-time)
  :components ((:file "urheilujuhla")
	       (:file "tests")))
