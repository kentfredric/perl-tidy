{
    # with side comment
    $me = $Is_MSWin32 ? $ENV{'USERNAME'}
      : $^O eq 'os2' ? $ENV{'USER'} || $ENV{'LOGNAME'}
      : eval { getpwuid($<) };    # May be missing
}
{{{
            # a simple ?/: pair to format
            ( $feat->strand == -1 )
              ? ( @range = ( $feat->end, $feat->start, $feat->strand ) )
              : ( @range = ( $feat->start, $feat->end, $feat->strand ) );

            # caused trouble at one time
            ( $SO, $SE ) =
              $opt{H} ? ( $terminal->Tputs('so'), $terminal->Tputs('se') )
              : ( $terminal->Tputs('us'), $terminal->Tputs('ue') );

        # complex
        $text .= tabs($level) . "$k"
          . ( ref $v
          ? (
              ref $v eq "ARRAY" 
              ? $this->array_out( $v, $level + 1 )
              : ( " " . $v->to_text( $level + 1 ) ) )
          : "\t$v\n" );
}}}


{
# multiple ?/: pairs
return wantarray
      ? (
        $pubpre
        ? (
            $this->{DELEGATE}{$pubpre}{sysid},
            $this->{DELEGATE}{$pubpre}{base}
        )
        : ()
      )
      : $pubpre ? 1
      : 0;

    # same but has one-line block:
    return wantarray ? (
        $pubpre ? (
            $this->{DELEGATE}{$pubpre}{sysid},
            $this->{DELEGATE}{$pubpre}{base}
        ) : ()
    ) : $pubpre ? 1 : 0;
}

# an intervening '.' operator to handle
{{
        printf( STDOUT "%-24s\t ",
            ( $data[ $labels{"Last name"} ]
            ? $data[ $labels{"Last name"} ] . ", "
            . $data[ $labels{"First name"} ]
            : $data[ $labels{"Company"} ] ) );
}}

# this was trouble at one time
$b = -6;
$a = ( $b > 0 ) ? {
    a => 1,
    b => 2
} : { a => 6, b => 8 };
$c = $$a{a};
print "c=$c\n";


# this should be broken with one conditional per line
# with leading ':' by default
*{"${callpkg}::$sym"} =
  $type eq '&' ? \&{"${pkg}::$sym"}
  : $type eq '$' ? \${"${pkg}::$sym"}
  : $type eq '@' ? \@{"${pkg}::$sym"}
  : $type eq '%' ? \%{"${pkg}::$sym"}
  : $type eq '*' ? *{"${pkg}::$sym"}
  : do { require Carp; Carp::croak("Can't export symbol: $type$sym") };

# same as above but broken with a side comment.  That causes trouble
# at present
*{"${callpkg}::$sym"} =
  $type eq '&' ? \&{"${pkg}::$sym"}  #
  : $type eq '$' ? \${"${pkg}::$sym"} #
  : $type eq '@' ? \@{"${pkg}::$sym"}
  : $type eq '%' ? \%{"${pkg}::$sym"}    # side comment 
  : $type eq '*' ? *{"${pkg}::$sym"}  #
  : do { require Carp; Carp::croak("Can't export symbol: $type$sym") };

{

    # another colon break test; the original is nicely formatted with
    # ':' and '?' at beginning of lines.
    $estatus =
      ( $^O eq 'VMS' ? eval 'use vmsish "status"; $estatus = $?' :
      $wstatus >> 8 );
}

# This example from 'camel 3', p 106, fits on one line so will not
# be broken.  It would be nice to detect and format with line breaks.
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


{{{{{
                    $_ =
                      ( ($_)
                      ? &process_math_in_latex( "indisplay", '', '',
                          $doimage . $_ )
                      : '' );
}}}}}

{{
          # complex:
          map $_ =~ s/^_//g ? $cgi->param( '_' . $_ )
          ? "$_ = '" . ( $cgi->param( '_' . $_ ) || '' ) . "'"
          : ''
          : "$_ = " . ( $cgi->param($_) || 0 ), param("_fields") ? split /,/,
          param("_fields") : grep $_ =~ /^_?\@/, $cgi->param();
}}

print$$<300?"$$<300\n":$$<700?"$$<700\n":$$<2_000?"$$<2,000\n":$$<10_000?"$$ <10,000\n":"$$>9,999\n";

{{
        # Original is beautifully done; but this becomes a mess
        # without indentation.  Adding parens would solve the problem.
        $a
        ? @colnums = @colnums
                     ? @colnums
                     : grep {
                             $colnum=$_,
                             &$rcolsel( [ map ${$_}[$colnum], @{$array} ] )
                            } 0..$#{$array->[0]}
        : @$rcolsel;

            my @s =
              defined $G->{Succ}->{$v}
              ? map { @{ $G->{Succ}->{$v}->{$_} } }
              sort keys %{ $G->{Succ}->{$v} }
              : ();
}}

{
    $targetprimary == 8
      ? return send_command($serial_port,
                            "\x00\x00"
                            . pack("C", $targetbit)
                            . "\x00\x00\x00\x00\x00\x04")
      : return send_command($serial_port,
                            "\x00\x00\x00"
                            . pack("C", $targetbit)
                            . "\x00\x00\x00\x00\x08");
}
{{{
            my ($subst) = join ( ' ',
                $2 eq 'f' ? map( $_ && $_->{entry} || (), @src )
                  : $2 eq 'd' ? map( $_ && $_->rfile->{dir}->path || (), @src )
                  : map( $_ && $_->rpath || (), @src ) );

            # Test case for rule of not exploding containers preceded by ?:
            push (
                @opt_exclude_regex,
                join (
                    '',
                    '(\A|/)',
                    (
                      map {
                          ( $_ eq '*' ? '.*'
                            : ( $_ eq '?' ? '.'
                                  : ( $_ eq '.' ? '\.'
                                      : ( $_ =~ /^\[/ ? $_ : quotemeta($_) ) ) )
                          );
                      } @a ),
                    '\Z'
                  )
            );
}}}
