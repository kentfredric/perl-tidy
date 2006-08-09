# test ability to parse 'carat variables';
use warnings;
my $a=${^WARNING_BITS};
print "$a\n";
@{^HOWDY_PARDNER}=(101,102);
print "@{^HOWDY_PARDNER}\n";

${^W} = 1;
print "$^W\n";

# This is an error but perltidy will not catch it yet
# Need minor update in tokenizer
my %hash;
$hash{^HOWDY} = 56;
my $result = $hash{^HOWDY};
print "result is $result\n";

# perl accepts this:
$^] = 1;
my @bb;
$bb[$^]]="bubba";
print "bb=@bb\n";
@^] = (1,2);
my @b=@^];
print "@b\n";
