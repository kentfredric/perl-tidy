# some here_doc tests for perltidy
# syntax is ok on all of these

# unusual target characters
print <<".*E\t  $X\n^;";
Hello,
World 0
.*E\t  $X\n^;

print << "EOM\n";
Hello, 
World 1
EOM\n

print << "EOM'";
Hello, 
World 2
EOM'

print << "EOM\"";
Hello, 
World 3
EOM"

print << 'EOM\"';
Hello, 
World 4
EOM\"

# quoted backtick works
print << `E\`OM`;
echo "Hello, World 5";
E`OM

# This works if it is in quotes, otherwise
# it works but we get stopped at the __END__
# go figure.  but see __DATA__
print << "__END__";
Hello, 
World 6
__END__

# no interpolation in target string
$a = "STILL_GOING";
print <<"$a";
Hello, 
World 7
STILL_GOING
$a
=b
;
=cut

#this works
print <<format;
Hello, 
World 8
format

#this works
print <<"=head1 HERE-TARGETS";
Hello, 
World 9

=head1 HERE-TARGETS

#this works without quotes (but __END__ needs quotes)
print <<__DATA__;
Hello, 
World 10
__DATA__

print <<"LABEL:";
Hello, 
World 11
LABEL:

# this does nothing useful:
<<';';

(secret hiding place)

;

print <<; # target is emptyline
# Hello World 12!

print <<# empty target string, still need a ;
# Hello World 13!

;<<'}'# leading ; terminates previous statement

if (another secret place) {
  print "Syntax ok!\n;
}
;#

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

# Do not break this long line after here target
pp_addhdr(<<"EOD") if defined $Config{'o_nonblock'} && $Config{'o_nonblock'} ne 'O_NONBLOCK';
#define O_NONBLOCK $Config{'o_nonblock'}
EOD
