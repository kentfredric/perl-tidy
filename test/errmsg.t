
	#!/usr/bin/perl

=pod

#!/bin/perl -w

=cut

# mutiple subs
sub a{ print "hi"}
sub a{ print "hi"}

package DbTdstr.pm;   # '.pm' is an error
package junk          
use IO::File;         # missing ';' before use

$a = $b ? : $c;       # '? :' needs a term

# complain of multiple commas (require -w)
register_language(
    'name'        => => 'objc',
    'linker'      => 'OBJCLINK',
    ,
    'link' =>
    );

  # nesting error
$b = $a ? ( $a : $a + 1 );

# operators where term expected:
.;
&& $a;
|| $b;

# Some code to generate various error messages
# No use strict, no -w

# Here the space after 'redirect' will confuse perltidy
use CGI qw( :standard );
   print redirect ( "$prefix$file" );

        # missing paren between braces
	while ($self->match($string)
	{
		$string = $self->postmatch;
	}

# extra ';' in for statement
for ($i=1; $i<10; $i++;) {
   print "i now be $i\n";
}

# only 1 ';' in for statement
for ($i=1; $i<10, $i++) {
   print "i now be $i\n";
}

# extra opening paren
while ($n++ < 20) { insert($root, int(rand(1000)) }

    # extra closing paren
    while ( $position = $positions->extract_minimum ) ) {
        return $position if $position->is_answer;
    }

