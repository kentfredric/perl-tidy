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
    foreach (@extn_hints) { print "$_ "; }
    if ($text) { write_text_block( $text, $is_head ); $text = (); $is_head = 0; }
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
