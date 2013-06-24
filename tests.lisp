(defpackage #:weblocks-bootstrap-date-entry-presentation-tests
  (:use #:cl #:weblocks 
        #:weblocks-selenium-tests 
        #:selenium 
        #:weblocks-utils 
        #:weblocks-bootstrap-date-entry-presentation))

(in-package :weblocks-bootstrap-date-entry-presentation-tests)

(defun bootstrap-date-entry-demonstration-action (&rest args)
  (let ((widget (make-instance 'composite)))
    (setf 
      (composite-widgets widget) 
      (list 
        (make-quickform 
          (defview 
            nil 
            (:caption "Input with bootstrap date entry" :type form :persistp nil :enctype "multipart/form-data" :use-ajax-p nil)
            (date 
              :present-as bootstrap-date-entry 
              :parse-as bootstrap-date)))
        (lambda (&rest args)
          (render-link (lambda (&rest args)
                         (answer widget t)) "back"))))
    (do-page widget)))

(weblocks-twitter-bootstrap-application-tests::define-bootstrap-demo-action "Date entry demos" #'bootstrap-date-entry-demonstration-action)
