# perltidy incorrectly identifies the second '{' here as being a block
# type token, not an anonymous hash reference.  It is difficult
# to tell what type of brace it is without a significant look-ahead.
# Two consequences:
# 1. a patch has been added to prevent it from adding a semi-colon
# before the closing brace.
# 2. currently, continuation indentation is added to the second line,
# so it doesn't look very good
sub _directives {
    {
	'ENDIF'         => \&_endif,
	'IF'            => \&_if,
	'SYMLOOK'       => \&_symlook
    };
}

# inner braces are anonymous hash reference
$a = [ sub { {} } ];
