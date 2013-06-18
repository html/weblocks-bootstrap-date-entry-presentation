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
      (multiple-value-bind (time-matched time-elements) (cl-ppcre:scan-to-strings "^(\\d+):(\\d+)(:(\\d+))?\\s(AM|PM)$" time)
        (when (or (not date-matched)
                  (not time-matched))
          (return-from parse-view-field-value (values t nil nil)))
        (setf date-elements (loop for i being the elements of date-elements collect (parse-integer i)))

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
  ((show-seconds :initform nil :initarg :show-seconds :accessor bootstrap-date-entry-presentation-show-seconds)))

(defmethod render-view-field-value (value 
                                     (presentation bootstrap-date-entry-presentation)
                                     (field form-view-field)
                                     (view form-view) 
                                     widget obj
                                     &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))

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
      (<:as-is "&nbsp;")
      (<:div :class "input-append bootstrap-timepicker-component"
             (<:input :id time-input-id :type "text" :class "input-small" 
                      :value (or time 
                                 (when value 
                                   (if (bootstrap-date-entry-presentation-show-seconds presentation)
                                     (metatilities:format-date "%I:%M:%S %p" value)
                                     (metatilities:format-date "%I:%M %p" value))))
                      :name (format nil "~A[time]" field-name))
             (<:span :class "add-on"
                     (<:i :class "icon-time"))))
    (send-script 
      (ps:ps 
        (with-styles 
          "/pub/stylesheets/datepicker.css"
          (lambda ()
            (with-scripts 
              "/pub/scripts/bootstrap-datepicker/bootstrap-datepicker.js"
              "/pub/scripts/bootstrap-datepicker/locales/bootstrap-datepicker.ru.js"
              (lambda ()
                (ps:chain 
                  (j-query (ps:LISP (format nil "#~A" date-input-id)))
                  (datepicker 
                    (ps:create 
                      :autoclose t
                      :format "dd.mm.yyyy")))
                (ps:chain 
                  (j-query (ps:LISP (format nil "#~A" *presentation-dom-id*)))
                  (find ".icon-th")
                  (click (lambda ()
                           (ps:chain (j-query (ps:LISP (format nil "#~A" date-input-id)))
                                     (datepicker "show")))))))))

        (with-styles 
          "/pub/stylesheets/timepicker.css"
          (lambda ()
            (with-scripts 
              "/pub/scripts/bootstrap-timepicker.js"
              (lambda ()
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
                              (when (bootstrap-date-entry-presentation-show-seconds presentation)
                                (list "showSeconds" t)))))))))))))))))
