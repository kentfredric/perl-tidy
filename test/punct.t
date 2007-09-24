# This caused an error at one time; '&~' is NOT a punctuation variable
$self->{text}->{colorMap}
  ->[ Prima::PodView::COLOR_CODE_FOREGROUND & ~tb::COLOR_INDEX ] =
  $sec->{ColorCode}

%# = ( foo => 'bar', baz => 'buz' );
print keys(%#), "\n";

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
print "$¢\n" ;

@: = ("a", "b", "c", "d");
foreach $i (0 .. $#: ) {
  print "$i\n";
}

#sum of punctuation array maximum indexes
$#=5;
@: = ("a", "b", "c");
my $num1=$#:;

# $#+ and $#- are valid in perl 5.6 but will fail in earlier versions:
my $num2=$#-;
my $num3=$#+;
$sum=+$#:+ +$#-+$#++$#;#:)
print "num1 = $num1, num2=$num2, num3=$num3, sum is: $sum\n";
