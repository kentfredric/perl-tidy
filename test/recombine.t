# Old Problem with -pbp : do not need break at && here; recombine
$clone->{$_}
    = ref $value && UNIVERSAL::isa( $value => __PACKAGE__ )
    ? $value->clone
    : $value;

# all the $ti terms will be recombined:
print $t0 && $t1 && $t2 && $t3 && $t4 && $t5 && $t6 && $t7
  ? "ok $i \# - $code - $name - $mapping - $status\n"
  : "not ok $i \# - $code - $name - $mapping - $status - $t0 $t1 $t2 $t3 $t4 $t5 $t6 $t7\n";

# recombine '= [' here:
$retarray = [
    &{ $sth->{'xbase_parsed_sql'}{'selectfn'} }( $xbase, $values,
        $sth->{'xbase_bind_values'} ) ]
  if defined $values;
# recombine '= lrotate(' here
{ { {
            $W[$r] = lrotate(
                $W[ $r - 3 ] ^ $W[ $r - 8 ] ^ $W[ $r - 14 ] ^ $W[ $r - 16 ],
                1 );
 } } } 
{
    # do not recombine at = here with pbp
    my $moon_corrected_longitude
        = $moon_mean_longitude 
        + $moon_evection 
        + $moon_correction_for_center
        - $moon_annual_equation
        - $moon_correction_2;
# Ok to recombine if end type is 'h'
        if $res
        = eval <<'EOE';
# break at ( would give better alignment
# Note: vertical tightness could combine these two lines ok:
EOE
# recombine:
    1
      while
      s/^([^;]*)([1-9])(0*)([^1]*\2(.)[^;]*\3(9*).*\|\?.)[^~]*~/$1$5$6$4/s;
    ok( 103,
        $db = tie( %h, 'DB_File', $Dfile, O_RDWR | O_CREAT, 0640, $DB_BTREE ) );
}
{{{{
    # recombine the = if third line starts with ;
                my $max = 1
                    + 2 * ( int( 2**( $len - 1 ) ) - 1 )
                    ;    # The max possible checksum
}}}}
croak( "can't create " . $this->name )
  if $pdl->isnull and !$this->{FlagCreat};

return 'equal'
  if @seen_twice == keys %$set1
  and @seen_twice == keys %$set2;

# recombination logic is used to keep this as a sequence
    (/^$/) ? $filecol
      : (s/^\+//) ? $filecol + $_
      : (s/^\-//) ? $filecol - $_
      : (s/^>//)  ? ( $filecol + $_ ) % $pages
      : (s/^<//)  ? ( $filecol - $_ ) % $pages
      : (s/^<//)  ? ( $a ? ( $filecol - $_ ) % $pages : $b )
      : $d;

# perltidy will break after the '=' here
my @host_seq = $level eq "easy" ?
	    @reordered : 0..$last;  # reordered has CDROM up front

{{
        # No recombine final . here:
        push (
            @{$hash},
            " " x ( 11 - ( length( $month_name . " " . $year ) ) / 2 )
              . "$month_name $year" . ' ' x ( ( 29 - length($month_name) ) / 2 )
        );

        # here breaking after the '=' creates two additional lines
        my @host_seq = $level eq "easy" ?
                    @reordered : 0..$last;  # reordered has CDROM up front
}}   

# sub recombine keeps keywords return, last, next, redo on separate lines
# if the line is broken, so that they standout
sub Restore {
    my $w = shift;
    return if !$w->toplevel->IsMapped || !$w->slave->IsMapped
      || !$w->cget('-restore');
    $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}

			# Another example
            next if ( ( $types_to_go[$imid] eq 'k' )
              && ( $tokens_to_go[$imid] =~ /^(last|next|redo|return)$/ ) );

{{{
            # do not recombine if good allignment possible!
            $psr =
              $F->get_attribute( 'capacity', $u, $v ) -
              $F->get_attribute( 'flow',     $u, $v );

            $delta =
              ( $points[$i][0] - $points[ $i - 1 ][0] ) /
              ( $points[ $i + 1 ][0] - $points[ $i - 1 ][0] );

            # ok to join short " :
                $parens[ $j - $i ][$j] = "("
                  . $parens[ $j - $i ][$k] . "x"
                  . $parens[ $k + 1 ][$j] . ")";
            $Out .= "  |"
              . Center( "Compiled on $CompileDate at $CompileTime.", $iWidth )
              . "|\n";
}}}

# don't leave the '3' on single line
$leading_block_text_line_length =
  length($accumulating_text_for_block) +
  length( $rOpts->{'closing-side-comment-prefix'} ) +
  $leading_block_text_level * $rOpts_indent_columns + 3;

# try -scl=12 to see '$returns' joined with the previous line
    $format =
        "format STDOUT =\n"
      . &format_line('Function:       @') . '$name' . "\n"
      . &format_line('Arguments:      @') . '$args' . "\n"
      . &format_line('Returns:        @')
      . '$returns' . "\n"
      . &format_line('             ~~ ^') . '$desc' . "\n.\n";

{

    # do not recombine into two lines after a comma if
    # the term is complex (has parens) or changes level
    $delta_time = sprintf "%.4f",
      ( ( $done[0] + ( $done[1] / 1e6 ) ) -
          ( $start[0] + ( $start[1] / 1e6 ) ) );

    push @addr,
      ( ord( substr( $uname, $_, 1 ) ) - 97 << 4 ) |
      ( ord( substr( $uname, $_ + 4, 1 ) ) - 97 );
}

# need to join '|| (' with line having leading ? or :
$package =
  ref $_[0] && UNIVERSAL::isa( $_[0] => 'SOAP::Lite' )
  ? $currenturi
  : $package eq 'SOAP' ? ref $_[0] || (
      $_[0] eq 'SOAP'
    ? $currenturi || Carp::croak "URI is not specified for SOAP call"
    : $_[0]
  )
  : $package eq 'main' ? $currenturi || $package
  :                      $package;
{{

        # maybe do not recombine something like this 
        # comma break, math, 
        $delta_time = sprintf "%.4f", (($done[0] + ($done[1] / 1e6)) -
                ($start[0] + ($start[1] / 1e6)));

        # another example of same:
        push @addr, ((ord (substr ($uname, $_, 1)) - 97 << 4) |
                (ord (substr ($uname, $_ + 4, 1)) - 97));
}}
