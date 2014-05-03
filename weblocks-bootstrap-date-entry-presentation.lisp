;;;; weblocks-bootstrap-date-entry-presentation.lisp

(in-package #:weblocks-bootstrap-date-entry-presentation)

(defmacro with-yaclml (&body body)
  "A wrapper around cl-yaclml with-yaclml-stream macro."
  `(yaclml:with-yaclml-stream *weblocks-output-stream*
     ,@body))

(defclass bootstrap-date-parser (date-parser)
  ())

(defmethod parse-view-field-value ((parser date-parser) value obj
                                                        (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let* ((name (attributize-name (view-field-slot-name field)))
         (date (request-parameter (format nil "~A[date]" name)))
         (time (request-parameter (format nil "~A[time]" name)))
         (hour))
    (multiple-value-bind (date-matched date-elements) (cl-ppcre:scan-to-strings "(\\d+)\\.(\\d+)\\.(\\d+)" date)
      (when (not date-matched)
        (return-from parse-view-field-value (values t nil nil)))
      (setf date-elements (loop for i across date-elements collect (parse-integer i)))

      (multiple-value-bind (time-matched time-elements) (cl-ppcre:scan-to-strings "^(\\d+):(\\d+)(:(\\d+))?\\s(AM|PM)$" time)

        (unless time-matched 
          (setf time-elements #("0" "0" "0" "0" "AM")))

        (setf hour (+ (parse-integer (aref time-elements 0))
                      (if (string= (aref time-elements 4) "PM")
                        12 
                        0)))

        (cond 
          ((= hour 24)
           (setf hour 12))
          ((= hour 12)
           (setf hour 0)))

        (values t t 
                (eval `(encode-universal-time 
                         ,(if (aref time-elements 3)
                            (parse-integer (aref time-elements 3))
                            0) 
                         ,(parse-integer (aref time-elements 1))
                         ,hour
                         ,@date-elements)))))))

(defclass bootstrap-date-entry-presentation (date-entry-presentation)
  ((show-time-p :initform t 
                :initarg :show-time-p 
                :accessor bootstrap-date-entry-presentation-show-time-p)
   (show-seconds-p :initform nil :initarg :show-seconds-p :accessor bootstrap-date-entry-presentation-show-seconds-p)
   (datepicker-options :initform (list :autoclose t
                                       :format "dd.mm.yyyy")
                       :initarg :datepicker-options)
   (default-date :initform (metatilities:format-date "%Y-%m-%d" (get-universal-time))
                 :initarg :default-date)))

(defparameter *datepicker-locale-file* "/pub/scripts/bootstrap-datepicker-locales/bootstrap-datepicker.ru.js")

(defmethod render-view-field-value (value 
                                     (presentation bootstrap-date-entry-presentation)
                                     (field form-view-field)
                                     (view form-view) 
                                     widget obj
                                     &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))

  (with-slots (datepicker-options default-date) presentation
    (let* ((name (attributize-name (view-field-slot-name field)))
           (date (request-parameter (format nil "~A[date]" name)))
           (time (request-parameter (format nil "~A[time]" name)))
           (date-input-id (format nil "~A-date" *presentation-dom-id*))
           (time-input-id (format nil "~A-time" *presentation-dom-id*))
           (field-name (if field-info
                         (attributize-view-field-name field-info)
                         (attributize-name (view-field-slot-name field)))))
      (with-yaclml 
        (<:div :class "input-append date" :id *presentation-dom-id* 
               (<:input :class "input-small" :size "10" :type "text" :id date-input-id 
                        :value (or date 
                                   (when value 
                                     (metatilities:format-date "%d.%m.%Y" value)))
                        :name (format nil "~A[date]" field-name))
               (<:span :class "add-on"
                       (<:i :class "icon-th")))
        (when (bootstrap-date-entry-presentation-show-time-p presentation)
          (<:as-is "&nbsp;")
          (<:div :class "input-append bootstrap-timepicker-component"
                 (<:input :id time-input-id :type "text" :class "input-small" 
                          :value (or time 
                                     (when value 
                                       (if (bootstrap-date-entry-presentation-show-seconds-p presentation)
                                         (metatilities:format-date "%I:%M:%S %p" value)
                                         (metatilities:format-date "%I:%M %p" value))))
                          :name (format nil "~A[time]" field-name))
                 (<:span :class "add-on"
                         (<:i :class "icon-time")))))

      (weblocks-utils:require-assets 
        "https://raw.github.com/html/weblocks-assets/master/bootstrap-datepicker/1.2.0/")

      (send-script 
        (ps:ps 
          (weblocks-utils:ps-with-scripts-and-styles 
            ("/pub/scripts/bootstrap-datepicker.js" (ps:LISP *datepicker-locale-file*))
            ("/pub/stylesheets/bootstrap-datepicker.css")
            (let* ((date-elem (ps:chain 
                                (j-query (ps:LISP (format nil "#~A" date-input-id)))
                                (parent)))
                   (set-default-date (lambda ()
                                       (unless (ps:chain date-elem (data "date"))
                                         (ps:chain date-elem (data "date" (ps:new ((ps:LISP (intern "Date")) (ps:LISP default-date)))))))))

              (ps:chain 
                date-elem
                (datepicker 
                  (ps:LISP `(ps:create 
                              ,@datepicker-options))))

              (ps:chain date-elem (on "click" set-default-date))
              (ps:chain date-elem (on "keydown" set-default-date))
              (ps:chain date-elem (on "keyup" set-default-date))

              (ps:chain 
                (j-query (ps:LISP (format nil "#~A" *presentation-dom-id*)))
                (find ".icon-th")
                (click (lambda ()
                         (set-default-date)
                         (ps:chain date-elem (datepicker "show")))))
              (ps:LISP 
                (when default-date 
                  `(let ((interval))
                     (setf interval 
                           (set-interval 
                             (lambda ()
                               (when (and (not (ps:chain date-elem (find "input:first") (val)))
                                          (ps:chain date-elem (data "datepicker")))
                                 (clear-interval interval)
                                 (ps:chain date-elem 
                                           (data "datepicker") 
                                           (update (ps:new (,(intern "Date") ,default-date))))
                                 (ps:chain date-elem (find "input:first") (val ""))))
                             100)))))))))

      (when (bootstrap-date-entry-presentation-show-time-p presentation)

        (weblocks-utils:require-assets 
          "https://raw.github.com/html/weblocks-assets/master/bootstrap-timepicker/0.2.3/")

        (send-script 
          (ps:ps 
            (weblocks-utils:ps-with-scripts-and-styles 
              ("/pub/scripts/bootstrap-timepicker.js")
              ("/pub/stylesheets/bootstrap-timepicker.css")
              (ps:chain 
                (j-query (ps:LISP (format nil "#~A" time-input-id)))
                (timepicker 
                  (eval 
                    (ps:LISP 
                      (format 
                        nil 
                        "(~A)"
                        (cl-json:encode-json-plist-to-string 
                          (append 
                            (list "defaultTime" "value")
                            (when (bootstrap-date-entry-presentation-show-seconds-p presentation)
                              (list "showSeconds" t))))))))))))))))
