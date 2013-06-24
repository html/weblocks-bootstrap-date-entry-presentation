;;;; weblocks-bootstrap-date-entry-presentation-tests.asd

(asdf:defsystem #:weblocks-bootstrap-date-entry-presentation-tests
  :serial t
  :version "0.0.1"
  :description "Tests for weblocks-bootstrap-date-entry-presentation"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :depends-on (#:weblocks-bootstrap-date-entry-presentation #:weblocks-selenium-tests #:weblocks-utils #:weblocks-twitter-bootstrap-application-tests)
  :components ((:file "tests")))

