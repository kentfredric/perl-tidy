# perltidy should parse sub attributes here ok since they are on the
# same line as the sub name:
sub be_careful () : locked method {
    my $self = shift;

    # ...
}

# In perl v5.6.0, prototype may be on separate line from sub name,
# (but prototype needs to be on one entire line still).
# sub may have attribute, possibly on separate line
sub 
witch 
()   # prototype may be on new line, but cannot put line break within prototype
: 
locked 
{
	print "and your little dog ";
}
my $a=witch+1;
print "$a\n";

package Canine;
package Dog;
my Canine $spot : Watchful ;  

package Felis;
my $cat : Nervous;

package X;
sub foo : locked ;  

package X;
sub Y::x : locked { 1 }  

package X;
sub foo { 1 }

package Y;
BEGIN { *bar = \&X::foo; }

package Z;
sub Y::bar : locked ;  

package MyTest;

use base 'Test::Class';

use Test::More;

sub one : Test {
	ok 1, "one";
}

sub two : Test(2) {
	ok 1, "one";
	ok 1, "two";
}
