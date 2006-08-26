# at one time did not break on all :'s here!! test8/vshnu.tdy with brand model
sub bykeyorder
{
          ($b eq '') ? -1 : ($a eq '') ? 1
        : (length ($a) == 1 && length ($b) != 1) ? -1
        : (length ($a) != 1 && length ($b) == 1) ? 1
        :                                          $a cmp $b;
    }

# Old Problem: A break at the first : triggers all later : breaks
# But a break at the second colon does not trigger earlier breaks!
    $year % 4 ? 0 
    : $year % 100 ? 1 : $year % 400 ? 0 : 1;

    $year % 4 ? 0 : $year % 100 ? 1 
    : $year % 400 ? 0 : 1;

# Old problem: a single old colon  break will not trigger all breaks at that
# level And therefore this gets really messed up...
        $bits = $top > 0xffff ? 32 : $top > 0xff ? 16 : $top > 1 ? 8 
        : 1;

push(
    @opt_exclude_regex,
    join(
        '', '(\A|/)',
        (
            map {
                (
                    $_ eq '*' ? '.*'
                    : (
                        $_ eq '?' ? '.'
                        : (
                            $_ eq '.' ? '\.'
                            : ( $_ =~ /^\[/ ? $_ : quotemeta($_) )
                        )
                    )
                  )
              } @a
        ),
        '\Z'
    )
);

( $options_r->{'bg_color'} )
  ? $options_r->{'bg_color'}
  : $MIDI::Opus::BG_color;


$self->{'can_javascript'} = (
    $q->user_agent =~ m#(Mozilla/[4-9])#i ? 1.2
    : (
        $q->user_agent =~ m#(Mozilla/3)#i ? 1.1
        : (
            $q->user_agent =~ m#(Mozilla/2|Konqueror)#i ? 1.0
            : 0
        )
    )
);
{
    $this->{'tracks'} =
      (defined($options_r->{'tracks'})
        and ref($options_r->{'tracks'}) eq 'ARRAY') 
      ? $options_r->{'tracks'}
      : [];
    return;

    # ok
    defined $uri
      or $uri =
      !$prefix || $prefix eq $self->namespace
      ? (
         $method_is_data
           && $^W
           && warn("URI is not provided as an attribute for ($method)\n"),
         ''
      )
      : die "Can't find namespace for method ($prefix:$method)\n";

    # This is tough - here's the original
    push @results, map {
      $colnr=$_;
      &$colf([ map { ${$_}[$colnr] } @{ $self->{'seqs'} } ])
        # Basically, &$colf([ map { ... } ]) is calculated; [ ] creates
        # the reference so that call-by-reference can be done.
        # In the outer map, $colnr=$_ loops thru all indices in @$rcolsel
        # In the inner map, $_ loops thru all rows _and_ from each row
        # the element row[$colnr] is taken. The inner map assembles these
        # elements into a list which is passed to &$colf.
    } ref($rcolsel) eq 'CODE'
        ? @colnums = @colnums
                     ? @colnums
                     : grep {
                             $colnum=$_,
                             &$rcolsel( [ map ${$_}[$colnum], @{$array} ] )
                            } 0..$#{$array->[0]}
        : @$rcolsel;

}
{{
# quite difficult:
	$filecol =
	    (/^$/)     ? $filecol					 :
	    (s/^\+//)  ? $filecol  + $_					 :
	    (s/^\-//)  ? $filecol  - $_					 :
	    (s/^>//)   ? ($filecol + $_) % $pages			 :
	    (s/^]//)   ? (($filecol + $_ >= $pages) ? 0 : $filecol + $_) :
	    (s/^<//)   ? ($filecol - $_) % $pages			 :
	    (s/^\[//)  ? (($filecol == 0) ? $pages - ($pages % $_ || $_) :
			  ($filecol - $_ < 0) ? 0 : $filecol - $_)	 :
	    (/^\d/)    ? $_ - 1						 :
	    (s/^\\?//) ? (($col{$_}, $row{$_}) = &pageto($_))[0]	 : 0;
}}

# indent here: between line ending in '=' and one with leading ':'
my $message =
    "There " . ( $words == 1 ) ? "is"
  : "are" . " $words word" . ( $words == 1 ) ? ""
  : "s" . " in the text \n";

# Break at '='
*{"${callpkg}::$sym"} = $type eq '&' ? \&{"${pkg}::$sym"}
  : $type eq '$' ? \${"${pkg}::$sym"}
  : $type eq '@' ? \@{"${pkg}::$sym"}
  : $type eq '%' ? \%{"${pkg}::$sym"}
  : $type eq '*' ? *{"${pkg}::$sym"}
  : do { require Carp; Carp::croak("Can't export symbol: $type$sym") };
{
    # mixed logical and conditional; can go many ways:
    $ok && $msg[0] =~ /\A(\S+)/ ? $1 : undef;
    
    $ok && 
    $msg[0] =~ /\A(\S+)/ ? $1 : undef;

    $ok && $msg[0] =~ /\A(\S+)/ ? $1 
    : undef;

    $ok
      && $msg[0] =~ /\A(\S+)/
      ? $1
      : undef;

    if ($minbits < 32) {
        while ($list) { 
	    $top = $val if $val > $top;
	}
        $bits =
	    $top > 0xffff ? 32 :
	    $top > 0xff ? 16 :
	    $top > 1 ? 8 : 1
    }

    @a = $horz
      ? ( $XY, $slv->y )
      : ( $slv->x, $XY );
    $cmd =
      (!ref $cmd) 
      ? $cmd
      : (!ref $$cmd[0])
      ? (
        ($$cmd[1] ne '' && !&opt('p')) ? "\\" . eval("qq^$$cmd[1]^") : $$cmd[0])
      : return map(&helpstr($key, substr($$_[2], 0, 1), $tab, $_), &cmds($cmd));

    # with side comment
    $me = $Is_MSWin32 ? $ENV{'USERNAME'}
      : $^O eq 'os2' ? $ENV{'USER'} || $ENV{'LOGNAME'}
      : eval { getpwuid($<) };    # May be missing

    # This will exercise sub recombine breakpoints;
    # the second ? in the last line makes it not a colon chain in
    # sub set_continuation_breaks
    (/^$/) ? $filecol
      : (s/^\+//) ? $filecol + $_
      : (s/^\-//) ? $filecol - $_
      : (s/^>//)  ? ( $filecol + $_ ) % $pages
      : (s/^<//)  ? ( $filecol - $_ ) % $pages
      : (s/^<//)  ? ( $a ? ( $filecol - $_ ) % $pages : $b )
      : $d;

{
        # This is too complex for sub recombine_breakpoints
        # From (test4/vshnu)
	$filecol =
	    (/^$/)     ? $filecol					 :
	    (s/^\+//)  ? $filecol  + $_					 :
	    (s/^\-//)  ? $filecol  - $_					 :
	    (s/^>//)   ? ($filecol + $_) % $pages			 :
	    (s/^]//)   ? (($filecol + $_ >= $pages) ? 0 : $filecol + $_) :
	    (s/^<//)   ? ($filecol - $_) % $pages			 :
	    (s/^\[//)  ? (($filecol == 0) ? $pages - ($pages % $_ || $_) :
			  ($filecol - $_ < 0) ? 0 : $filecol - $_)	 :
	    (/^\d/)    ? $_ - 1						 :
	    (s/^\\?//) ? (($col{$_}, $row{$_}) = &pageto($_))[0]	 : 0;
        ( ref($usage_fref) =~ /CODE/ ) 
          ? &$usage_fref
          : ( &blast_usage, &blast_params, &blast_general_params );
        exit 1;

{
            ( $feat->strand == -1 )
              ? ( @range = ( $feat->end, $feat->start, $feat->strand ) )
              : ( @range = ( $feat->start, $feat->end, $feat->strand ) );
            sprintf(
                "%d%s",
                $mday,
                (
                  ( $mday < 20 && $mday > 3 ) ? 'th'
                : (
                      $mday % 10 == 1 ? "st"
                      : ( $mday % 10 == 2 ? "nd"
                          : ( $mday % 10 == 3 ? "rd" : "th" ) )
                  )
                )
            );
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
                $s->{seen}{$id} = [
                    (
                      ( $name =~ /^[@%]/ ) ? ( '\\' . $name )
                    : ( $realtype eq 'CODE' and $name =~ /^[*](.*)$/ )
                    ? ( '\\&' . $1 )
                    : $name
                    ),
                    $val
                ];

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

print(
    (
        ~0 > 0 && do { use integer; ~0 }
          == -1
    ) ? "ok 7\n" : "not ok 7\n"
);

# problems with terminal indentation which work now:

        ( @_ == 0 ) ? ''
      : ( @_ == 1 ) ? $_[0]
      : ( @_ == 2 ) ? join( " and ",     @_ )
      :               join( "$sepchar ", @_[ 0 .. ( $#_ - 1 ) ], "and $_[-1]" );

    # extra fields in last line caused alignment trouble
    my $tempd =
        defined $ENV{TEMP} ? $ENV{TEMP}
      : defined $ENV{TMP}  ? $ENV{TMP}
      :                      cdir( $fs->rootdir, $td );

    # why no alignment
    my $self =
        @$options > 1  ? shift @$options
      : @$options == 1 ? { %{ $options->[0] } }
      :                  {};

    $norun =
        ( $_[0] eq 'toggle' ) ? !$norun
      : ( $_[0] eq 'once' )   ? 'once'
      : ( $_[0] eq 'on' )     ? 1
      :                         0;

    my $max_allowed_sparsity =
        ( $item_count < 3 )    ? 0.1
      : ( $packed_lines == 1 ) ? 0.15
      : ( $packed_lines == 2 ) ? 0.4
      :                          0.7;

    # align ? here
    $extra_space .=
        ( $input_line_number < 10 )  ? "  "
      : ( $input_line_number < 100 ) ? " "
      :                                "";

# Old problem no alignment of return because not enough fields
    ( $k eq 'file0' )   ? do { $file0   = $v }
  : ( $k eq 'file1' )   ? do { $file1   = $v }
  : ( $k eq 'fileptr' ) ? do { $fileptr = $v }
  :   return;
