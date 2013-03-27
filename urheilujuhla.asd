(asdf:defsystem #:urheilujuhla
  :depends-on (#:bordeaux-threads
	       #:flexi-streams
	       #:babel

	       #:object-system
	       #:cl-irc

	       #:com.dvlsoft.clon

	       #:fmi-observations
	       #:local-time)
  :components ((:file "urheilujuhla")))