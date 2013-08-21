This is bootstrap date entry presentation for weblocks. 

It gives you ability to use bootstrap date entry component https://github.com/eternicode/bootstrap-datepicker 
and time entry component from here https://github.com/jdewit/bootstrap-timepicker

To use it you should load package :weblocks-bootstrap-date-entry-presentation from weblocks app and use it in your defview  body like this 

(pub-date 
 :label "Publication date"
 :present-as bootstrap-date-entry 
 :reader (lambda(item)
   (firephp:fb "Editing item ~A" item)
   (or 
    (slot-value item 'pub-date)
    (get-universal-time)))
 :parse-as bootstrap-date)

Package uses weblocks assets so all dependencies should be installed automatically.
