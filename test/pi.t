   # this will cause an error
   # perltidy does not handle prototypes on separate lines correctly yet
   sub pi 
   ()
   {
     4 * atan2(1,1);
   }
   print pi / 2;
