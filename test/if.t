# that's an if, not a flag!
warn qq/warning: -refresh ignored for ftp URLs\n/if $refresh;

# Good example: 
if (
    $USE_UTF
    || (   !$NO_UTF
        && ( defined %unicode_table )
        && length(%unicode_table) > 2 )
  )
  {}
# complex -- contains equals, brace, 'last'
if    ( $i eq "-a" ) { $host = "whois.arin.net";  last; }
elsif ( $i eq "-d" ) { $host = "whois.nic.mil";   last; }
elsif ( $i eq "-p" ) { $host = "whois.apnic.net"; last; }
elsif ( $i eq "-r" ) { $host = "whois.ripe.net";  last; }
elsif ( $i eq "-g" ) { $host = "whois.nic.gov";   last; }
elsif ( $i eq "-6" ) { $host = "whois.6bone.net"; last; }
elsif ( $i eq "-h" ) { $host = shift;             last; }
else { unshift ( @ARGV, $i ); last; }

# the break at this if should be retained even though the line is short
push @allowed, $_
  if PDL->rpiccan($_) && defined $formats{$_};

# the terminal '{' will be at character 80:
{{{
    if ($a) {
    } elsif (/^FontBBox *(-?[0-9]*)  *(-?[0-9]*)  *(-?[0-9]*)  *(-?[0-9]*)/) {
      @fontBBox=($1,$2,$3,$4); 
    }
}}}

# an if block a bit too long for one line
{
    # Two lines to get logical padding
    if (   $q->user_agent('Nokia6210')
           || $q->user_agent('Nokia6250'))
    {
        $bla;
    }

    # this currently breaks open
    if ( @yes_colors == 1 && $included{"colorless"} && grep { $excluded{$_} }
        @COLORS, 'gold' ) {}

    foreach (@extn_hints) { print "$_ "; }
    if ($text) { write_text_block( $text, $is_head ); $text = (); $is_head = 0; }

    

    TRY: {
        if ( $startname ne '' && $zielname ne '' ) {
            last TRY
              if (
                (
                    ( defined $via2 && $via2 ne '' )
                    || ( defined $via && $via ne '' )
                )
                && ( !defined $vianame || $vianame eq '' )
              );
            warn "Wähle Kreuzung für $startname und $zielname\n"
              if $debug;
            get_kreuzung( $startname, $vianame, $zielname );
            return;
        }
    }

    # want 'if' on a new line:
    $w = $w->Subwidget('frame')
      if (
        @_
        && $_[0] =~ /^(?: bbox
				|columnconfigure
				|location
				|propagate
				|rowconfigure
				|size
				|slaves)$/x
      );

{
        if (
            -f (
                $libperl = $self->catfile(
                    $Config{'installarchlib'}, 'CORE',
                    "libperl$self->{LIB_EXT}"
                )
            )
          )
        {
        }

        #  a complex if with comments
        if (
            (

                # If the y is between the (y-) borders ...
                ( ( $y[$i] <= $y ) && ( $y < $y[$j] ) )
                || ( ( $y[$j] <= $y ) && ( $y < $y[$i] ) )
            )
            and

            # ...the (x,y) to infinity line crosses the edge
            # from the ith point to the jth point...
            (
                $x <
                ( $x[$j] - $x[$i] ) * ( $y - $y[$i] ) / ( $y[$j] - $y[$i] ) +
                $x[$i]
            )
          )
        {
        }
}
}

# some nested if statements
{
    if (seek(DATA, 0, 0)) { ## 
	$_ = <DATA>; if (m/^#!/) { print;
	    $_ = <DATA>; if (m/^\s*eval/) { print;
		$_ = <DATA>; if (m/^\s*if/) { print; }
	    }
	}
	print "\n#-\n";
    }
}

# should break at opening '(' of the 'if' but block length
# after '&&' is not accurately calculated
{
    if ( ref($to)
        && ( UNIVERSAL::isa( $to, 'GLOB' )
            || UNIVERSAL::isa( $to, 'IO::Handle' ) ) )
    {
        *TO = *$to;
    }
}


    # cannot retain this one-line block
    if (defined &replace_external_references_hook) {&replace_external_references_hook;}

        # test of formatting nested parens
        if ( $styles !~ /^($do_include_rx)$/o
            && $filename !~ /\.($do_include_ext_rx)$/o
            && ( $styles =~ /^($dont_include_rx)$/o
                || ( $opt{auto_exclude}
                    && $filename =~ /\.($dont_include_ext_rx)$/o ) ) )
        {
            print STDERR "$prompt %--- ignoring $filename" if ($debug);
            print STYLES "$styles\n" if ( $opt{save_styles} );
        }


    # Flush before a long if statement to avoid unwanted alignment:
    if ($EndYear =~ /\D/) { $EndYear = 0; }
    elsif ($EndYear < 50)  { $EndYear += 2000; }
    elsif ($EndYear < 100) { $EndYear += 1900; }
    if    (($Year < 1601)
        || ($Year > 2899)
        || ($EndYear < 1601)
        || ($EndYear > 2899))
    {
        &Error_OutOfRange;
    }

{

    # retain '&&' breaks even with comment
    if (
        defined($i_opening)
        && !$is_unbreakable_container->($dd)
        && !(

            # Avoid a break which would place an isolated or on a line
            $type eq 'Q'
            && $i_opening >= $max_index_to_go - 2
            && $token =~ /^['"]$/
        )
      )
    {
    }
}
