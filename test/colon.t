# test for block types.  This does not come out well yet;
# need to form small one-line anonymous hash blocks.
$b = -6;
$a = ( $b > 0 ) ? {
    a => 1,
    b => 2
} : { a => 6, b => 8 };
$c = $$a{a};
print "c=$c\n";

{{{
            ( $feat->strand == -1 )
              ? ( @range = ( $feat->end, $feat->start, $feat->strand ) )
              : ( @range = ( $feat->start, $feat->end, $feat->strand ) );
}}}

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
$leapyear = $year % 4 ? 0 
  : $year % 100 ? 1 : $year % 400 ? 0 : 1;

{{{
            # Would be nice to alingn ? and : here
            # or break before '?' instead of after =
            ( $SO, $SE ) =
              $opt{H} ? ( $terminal->Tputs('so'), $terminal->Tputs('se') )
                      : ( $terminal->Tputs('us'), $terminal->Tputs('ue') )
}}}

( $_ eq '*' ? '.*'
  : ( $_ eq '?' ? '.'
      : ( $_ eq '.' ? '\.' : ( $_ =~ /^\[/ ? $_ : quotemeta($_) ) ) ) );

    return wantarray ? (
        $pubpre ? (
            $this->{DELEGATE}{$pubpre}{sysid},
            $this->{DELEGATE}{$pubpre}{base}
        ) : ()
    ) : $pubpre ? 1 : 0;
