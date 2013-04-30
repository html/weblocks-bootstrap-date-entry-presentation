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

Additional files dependencies are required - 
- bootstrap-timepicker.js from https://github.com/jdewit/bootstrap-timepicker should be in pub/scripts/
- timepicker.css from https://github.com/jdewit/bootstrap-timepicker should be in pub/stylesheets/
- datepicker.css from https://github.com/eternicode/bootstrap-datepicker should be in pub/stylesheets/
- contents of js/ folder from https://github.com/eternicode/bootstrap-datepicker should be in pub/scripts/bootstrap-datepicker/

!!! Warning 
At the time it works with bootstrap-timepicker revision 796688ba405916186aeae4326165b219f4c6659d and does not work with latest revision.
