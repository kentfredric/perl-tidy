# test for block types.  This does not come out well yet;
# need to form small one-line anonymous hash blocks.
$b = -6;
$a = ( $b > 0 ) ? {
    a => 1,
    b => 2
} : { a => 6, b => 8 };
$c = $$a{a};
print "c=$c\n";

# this should be broken with one conditional per line
*{"${callpkg}::$sym"} =
  $type eq '&' ? \&{"${pkg}::$sym"} :
  $type eq '$' ? \${"${pkg}::$sym"} :
  $type eq '@' ? \@{"${pkg}::$sym"} :
  $type eq '%' ? \%{"${pkg}::$sym"} :
  $type eq '*' ? *{"${pkg}::$sym"} :
  do { require Carp; Carp::croak("Can't export symbol: $type$sym") };

{

    # another colon break test; the original is nicely formatted with
    # ':' and '?' at beginning of lines, but for automated formatting,
    # placing them at the end seems to work better
    $estatus =
      ( $^O eq 'VMS' ? eval 'use vmsish "status"; $estatus = $?' :
      $wstatus >> 8 );
}

# This example from 'camel 3', p 106, fits on one line so will not
# be broken
$leapyear = $year % 4 ? 0 : $year % 100 ? 1 : $year % 400 ? 0 : 1;
