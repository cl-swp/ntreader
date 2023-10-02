(asdf:defsystem :ntreader
  :name    "ntreadser"
  :version "1.0.0"
  :author  "Seiji Koide"
  :description "N-triple Reader and Parser"
  :depends-on (:namespace)
  :depends-on (:line-reader)
  :depends-on (:newdtree)
  :serial t
  :components ((:file "../new-knowledge-machine/basic-packages.lisp")
	       (:file "../utilities/utilities.lisp")
               (:file "../utilities/ncutils.lisp")
               (:file "../new-knowledge-machine/gxutils1.lisp")
               (:file "ntparser")
               (:file "ntreader.lisp")
	       (:file "ntadder.lisp")
               ))