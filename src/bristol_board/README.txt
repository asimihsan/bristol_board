------------------------------------------------------------------------
TODO
------------------------------------------------------------------------


20th May 2011 08:44

-  PostgreSQL returns microsecond precision datetimes.  epgsql
   faithfully returns this precision by making the seconds
   fields floating point numbers.  dh_date can't handle this.
   
   My initial fix was to fork dh_date and make it truncate the
   seconds field.  This works but a better fix is to get
   epgsql to drop the precision by truncating seconds there instead.
   