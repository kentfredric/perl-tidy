# The $$ at end of line is a punctuation variable and should become type 'i'.
# Therefore, the trailing '&' should become type '&'.
$pid16
=$$
&0xffff
;

# These are valid punctuation variables
@© = ( a .. z );
print "@©\n"; 
$¢ = 101;
print "$¢\n" 
