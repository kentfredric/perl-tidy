# testing brace types for user-defined block operator
package test;
sub mygrep (&@) {
	my $coderef = shift;
	my @result;
	foreach $_ (@_) {
		push (@result, $_) if &$coderef;
	}
	@result;
}
my @list = qw(a ; b ; c);
my @b = mygrep { $_ ne ';' } @list;
print "@b\n";
package test2;
my @list = qw(a : b : c);
my @b = test::mygrep { $_ ne ':' } @list;
print "@b\n";

