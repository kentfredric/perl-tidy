# The $$ at end of line is a punctuation variable and should become type 'i'.
# Therefore, the trailing '&' should become type '&'.
$pid16
=$$
&0xffff
;

# These are valid punctuation variables
@� = ( a .. z );
print "@�\n"; 
$� = 101;
print "$�\n" 

@: = ("a", "b", "c", "d");
foreach $i (0 .. $#: ) {
  print "$i\n";
}

 # These are valid (but deprecated):
 $imax=$#:;
 $imax=$#-;
 $imax=$#+;
 $imax=$#;;
 $imax=$#,;
