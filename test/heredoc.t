# some here_doc tests

# Escaped here doc target with numerical target
my $a = <<\1;
#1 OK
1
print "a=$a";

# Escaped here doc with empty target
my $a = <<\;
#2 OK

print "a=$a";

# Escaped here doc with alphabetic target
my $a = <<\n;
#3 OK
n
print "a=$a";

=pod
# Same with space does not work
my $a = << \n;
#4 OK
print "a=$a";
=cut

# This gives a single quote as target
# The target string is reported as \' in the log file
# but matches a single quote correctly
my $a = << '\'';
#5 OK
'
print "a=$a";

# a single quote as target
my $a = << "'";
#6 OK
'
print "a=$a";

# a single quote plus word as target
my $a = << "'BOOGA";
#7 OK
'BOOGA
print "a=$a";

# a double quote as target
my $a = << '"';
#8 OK
"
print "a=$a";

# a long string as target - note that the target string
# has an escape character
my $a = << "gobble'de'gook\"weird";
#9 OK
gobble'de'gook"weird
print "a=$a";
