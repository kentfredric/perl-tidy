{
    # break at all ';'s
    for ( $i = 0 ;
        $i < $self->{"retry"} ;
        ++$i, $retrans *= 2, $timeout = int( $retrans / ( $#ns + 1 ) ) )
    # commas and ';'
    for ( my ( $xa, $ya ) = @xy[ -2, -1 ] ; my ( $xb, $yb ) = splice @xy, 0, 2 ;
        ( $xa, $ya ) = ( $xb, $yb ) ) {}
}

# These are the same:
for ( $i = 0 ; $i <= 10 ; $i += 2 ) {
    print "$i ";
}
foreach ( $i = 0 ; $i <= 10 ; $i += 2 ) {
    print "$i ";
}

{{
        # broken 'for'
        for (
            $j = 0, $match_j = -1 ;
            $j < $sub_len
            &&

            # changed from naive_string_matcher
            $sub->[$j] eq $big->[ $i + $j ] ; $j++
          )
          {}

	# a foreach loop with large list, from dh_debstd
	# We'd like the opening '{' to be outdented inline with 'foreach':
	# and it would look best to leave the closing ')' at the end of
	# the list.  The list will be lined up vertically, although this
	# only allows room for 2 columns
	foreach $f ('services','inittab','crontab','protocols','profile',
		'shells','rpc','syslog.conf','conf.modules','modules',
		'aliases','diversions','inetd.conf','X11/Xresources',
		'X11/config','X11/window-managers','X11/xinit','purge') {
		if ( -f "$prefix$f") {
			warning("file $prefix$f was ignored.");
		}
	}


	# A side comment forces the list to use old line breaks (but the '{'
	# is still outdented).
	foreach $f ('services','inittab','crontab','protocols','profile', # XX
		'shells','rpc','syslog.conf','conf.modules','modules',
		'aliases','diversions','inetd.conf','X11/Xresources',
		'X11/config','X11/window-managers','X11/xinit','purge') {
		if ( -f "$prefix$f") {
			warning("file $prefix$f was ignored.");
		}
	}

	# we want to make the '{' lines up below the 'if' here:
        if ( $USE_UTF || ( !$NO_UTF && ( defined %unicode_table )
          && length(%unicode_table) > 2 ) ) {
            &convert_to_unicode($_);
        }
}}
