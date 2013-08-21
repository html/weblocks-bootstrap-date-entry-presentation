;;;; weblocks-bootstrap-date-entry-presentation.asd

(asdf:defsystem #:weblocks-bootstrap-date-entry-presentation
  :serial t
  :version "0.0.7"
  :description "Bootstrap date/time picker form presentation for weblocks"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :depends-on (#:weblocks
               #:yaclml 
               #:parenscript 
               #:metatilities)
  :components ((:file "package")
               (:file "weblocks-bootstrap-date-entry-presentation")))

