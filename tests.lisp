(defpackage #:weblocks-bootstrap-date-entry-presentation-tests
  (:use #:cl #:weblocks 
        #:weblocks-selenium-tests 
        #:selenium 
        #:weblocks-utils 
        #:weblocks-bootstrap-date-entry-presentation 
        #:weblocks-twitter-bootstrap-application-tests)
  (:import-from :weblocks-bootstrap-date-entry-presentation #:with-yaclml))

(in-package :weblocks-bootstrap-date-entry-presentation-tests)

(defmacro date-entry-action (caption presentation)
  `(lambda (&rest args)
     (let ((widget (make-instance 'composite)))
       (setf 
         (composite-widgets widget) 
         (list 
           (make-quickform 
             (defview 
               nil 
               (:caption ,caption :type form :persistp nil :enctype "multipart/form-data" :use-ajax-p t)
               (date 
                 :present-as ,presentation
                 :parse-as bootstrap-date))
             :answerp nil
             :on-success (lambda/cc (form result)
                           (let ((date (slot-value result 'date)))
                             (do-information (format nil "Parsed date result is ~A" 
                                                     (if date 
                                                       (format nil "~A, which is date ~A" 
                                                               date (metatilities:format-date "%d.%m.%Y %I:%M:%S %p" date))
                                                       date))))))
           (lambda (&rest args)
             (render-link (lambda (&rest args)
                            (answer widget t)) "back"))))
       (do-page widget))))

(defun bootstrap-date-entry-demonstration-action (&rest args)
  (let ((render-links))
    (setf render-links (make-widget 
                         (lambda (&rest args)
                           (with-yaclml 
                             (<:h1 "Date entry demos")
                             (<:hr)
                             (render-link 
                               (date-entry-action "Input with bootstrap date entry" bootstrap-date-entry)
                               "Default configuration (date, time without seconds showing)")
                             (<:br)                    
                             (render-link 
                               (date-entry-action "Input with bootstrap date entry with seconds showing" (bootstrap-date-entry :show-seconds-p t))
                               "Date and time with seconds showing")
                             (<:br)
                             (render-link 
                               (date-entry-action "Input with bootstrap date entry without time showing" (bootstrap-date-entry :show-time-p nil))
                               "Date only (without time showing)")
                             (<:hr)
                             (render-link (lambda (&rest args)
                                            (answer render-links))
                                          "back")))))
    (do-page render-links)))

(define-bootstrap-demo-action "Date entry demos" #'bootstrap-date-entry-demonstration-action)

(def-test-suite weblocks-bootstrap-date-entry-presentation-tests)

(deftest shows-date-entry ()
  (with-new-or-existing-selenium-session-on-bootstrap-site 
    (do-click-and-wait "link=Date entry demos")
    (do-click-and-wait "link=Default configuration (date, time without seconds showing)")
    (do-screen-state-test "bootstrap/date-entry")
    (do-click-and-wait "name=submit")
    (is (string= (do-get-text "css=.modal-body") "Parsed date result is NIL"))))

(deftest parses-date-entry-with-default-params ()
  (with-new-or-existing-selenium-session-on-bootstrap-site 
    (do-click-and-wait "link=Date entry demos")
    (do-click-and-wait "link=Default configuration (date, time without seconds showing)")
    (do-click-and-wait "name=date[date]")
    (do-click-and-wait "css=.day.active")
    (do-click-and-wait "css=.bootstrap-timepicker-component .add-on")
    (do-click-and-wait "css=.show-meridian a:first")
    (do-click-and-wait "name=submit")
    (is 
      (integerp (ppcre:scan 
                  (format nil "which is date ~A 02:00:00 AM" (metatilities:format-date "%d.%m.%Y" (get-universal-time)))
                  (do-get-text "css=.modal-body"))))))

(deftest parses-date-entry-with-seconds-showing ()
  (with-new-or-existing-selenium-session-on-bootstrap-site 
    (do-click-and-wait "link=Date entry demos")
    (do-click-and-wait "link=Date and time with seconds showing")
    (do-click-and-wait "name=date[date]")
    (do-click-and-wait "css=.day.active")
    (do-click-and-wait "css=.bootstrap-timepicker-component .add-on")
    (do-click-and-wait "css=.show-meridian a:nth(2)")
    (do-click-and-wait "name=submit")
    (is 
      (integerp (ppcre:scan 
                  (format nil "which is date ~A 01:00:15 AM" 
                          (metatilities:format-date "%d.%m.%Y" (get-universal-time)))
                  (do-get-text "css=.modal-body"))))))

(deftest parses-date-entry-without-time-showing ()
  (with-new-or-existing-selenium-session-on-bootstrap-site 
    (do-click-and-wait "link=Date entry demos")
    (do-click-and-wait "link=Date only (without time showing)")
    (do-click-and-wait "name=date[date]")
    (do-click-and-wait "css=.day.active")
    (do-click-and-wait "name=submit")
    (is 
      (integerp (ppcre:scan 
                  (format nil "which is date ~A 12:00:00 AM" 
                          (metatilities:format-date "%d.%m.%Y" (get-universal-time)))
                  (do-get-text "css=.modal-body"))))))
