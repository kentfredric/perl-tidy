# one line blocks with user-defined block functions
my $foo = first { $_->[1] le "e" and "e" le $_->[2] }[qw(a b c)], [qw(d e f)],
  [qw(g h i)];
sub_array { lc shift } @array;
sub_array { lc shift }( 'O', 'K', ' ', $i++ );
foreach my ${DISTRO}( @{ ${ RELEASE_FILES { $OS } } } ) {
}
undef ${ nucleic { $seq_name } };
( $a, $b ) = rgrep { /foo"(.*)".*-(.*)-/ } *OUT;
print "not " unless 9 == reduce { $a / $b } 756, 3, 7, 4;
print "not " unless min(@a) == reduce { $a < $b ? $a : $b } @a;
print "not " unless 9 == first { 8 == ( $_ - 1 ) } 9, 4, 5, 6;

# would like one-line block, but there is a 
# block within a block - so eval block will be broken
print "not " if defined eval { first {die if $_} 0, 0, 1 };

sub _directives {
    {
	'ENDIF'         => \&_endif,
	'IF'            => \&_if,
	'SYMLOOK'       => \&_symlook
    };
}

# inner braces are anonymous hash reference
$a = [ sub { {} } ];

{
    $tr = async
    {
        ok(3, 1, "lock factory: child: locking all locks");
        lock $locks2[0];
        lock $locks2[1];
        lock $locks2[2];
        lock $locks2[3];
        ok(4, 1, "lock factory: child: locked all locks");
    };
}

# had strange indentation (block type problem)
PDL::thread_define 'coords(vertices(n=3); colors(n)) NOtherPars => 3',
  PDL::over {
    ${$_[2]}   .= $_[4] . sprintf("%.3f %.3f %.3f,\n", $_[0]->list);
      ${$_[3]} .= $_[4] . sprintf("%.3f %.3f %.3f,\n", $_[1]->list);
      };

{

 # Non-ISO8601 dates
 s,\s*$sep\s*, ,g;    # change all non-ISO8601 seps to spaces
   s,^\s*,,;          # remove leading/trailing space
   s,\s*$,,;
}
