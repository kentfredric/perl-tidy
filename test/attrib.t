# Perltidy will not parse this correctly yet..
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
