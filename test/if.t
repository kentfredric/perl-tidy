# the terminal '{' will be at character 80:
{{{
    } elsif (/^FontBBox *(-?[0-9]*)  *(-?[0-9]*)  *(-?[0-9]*)  *(-?[0-9]*)/) {
      @fontBBox=($1,$2,$3,$4); 
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
