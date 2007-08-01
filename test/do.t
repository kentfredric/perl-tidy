# with side comment and trailing if
# At one time during development, side comment caused trouble forming one-line
# blocks.
do {    ### local($_) = $body;
    &make_unique($body);
} if ( $body =~ /$O/ );

# closing brace is not outdented here because line does not end in ;
do
{
    ++$self->{next_move};
  } while $self->{next_move} <= 8
  && $self->{board}[$self->{next_move}] ne $empty;

{
# a difficult block to break
        print( ( ( 1 << ( $bits - 1 ) ) == $cusp && do { use integer; 1 << ( $bits - 1 ) } == -$cusp ) ? "ok 11\n" : "not ok 11\n" );
}

# Note that 'unless' should get continuation indentation here
{{	    
            do { $new_command{$cmd} = join ( ':!:', 0, $body, '}' ) }
            unless ( defined &$tmp );
}}

# this can become one line
{
    do { $paragraph = <$fileHandle>; }
    while ( !eof($fileHandle) );
}

	# a one-line do block containing a one line if block
	s/^(.)// && do { if ($1 ne '#') { $new .= $1; } next;};

# '->' will be on separate line from '}' after do
do { __PACKAGE__->can('bootstrap') || \&DynaLoader::bootstrap; }
      ->( __PACKAGE__, $VERSION );

