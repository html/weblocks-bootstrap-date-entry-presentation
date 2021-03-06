;;;; weblocks-bootstrap-date-entry-presentation.asd

(asdf:defsystem #:weblocks-bootstrap-date-entry-presentation
  :serial t
  :version (:read-from-file "version.lisp-expr")
  :description "Bootstrap date/time picker form presentation for weblocks"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :depends-on (#:weblocks
               #:yaclml 
               #:parenscript 
               #:metatilities
               #:weblocks-utils)
  :components ((:file "package")
               (:file "weblocks-bootstrap-date-entry-presentation")))

