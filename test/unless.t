# snippet from Wolfgang Weisselberg
# desired formatting is shown
error( "\$config{compare} can only be",
       "'all' or 'check if changed' or 'remove changed'.",
       $config{compare} . "is illegal" )
  unless ( $compare eq 'all' 
        or $compare eq 'check if changed'
        or $compare eq 'remove changed' );

