#!/usr/bin/perl
# this is a test file developed by Michael Cartmell for testing
# the vstrings implementation in perltidy.
# usage: run with perl; run after processing by perltidy; 
# output should be identical

use strict;
use warnings;

my@a=(
v101,                   # 0
v101.102,               # 1
v101. 102,              # 2
v101 .102,              # 3
v101 . 102,             # 4
.100.102,               # 5
.100 .102,              # 6
.100. 102,              # 7
.100 . 100,             # 8
101.102.103,            # 9
101.102 .103,           #10
101.102. 103,           #11
101.102 . 103,          #12
101 .102.103,           #13
101 . 102.103,          #14
v101.102.103,           #15
v101.102 .103,          #16
v101.102. 103,          #17
v101.102 . 103,         #18
v101 .102.103,          #19
v101. 102.103,          #20
v101 . 102.103,         #21
v101. 102 .103,         #22
101.102.103.104,        #23
101.102.103. 104,       #24
101.102.103 .104,       #25
101.102.103 . 104,      #26
101.102. 103.104,       #27
101.102 .103.104,       #28
101.102 . 103.104,      #29
101.102. 103 .104,      #30
101 .102.103.104,       #31
v101.102.103.104,       #32
v101 .102.103.104,      #33
v101. 102.103.104,      #34
v101 . 102.103.104,     #35
v101.102 . 103.104,     #36
v101.102.103 .104,      #37
v101. 102.103 .104,     #38
v101 .102 .103.104,     #39
);

my $i = 0;
foreach my $a (@a) {
    printf "%2d: [", $i++;
    print $a, "]\n";
}

print "\n--------------------\n";
print v101 .102.103, "\n";
print "a" .102.103, "\n";
print v101 .102.103.104, "\n";
print "a" .102.103.104, "\n";

print "\n--------------------\n";
print v101.102.v100, "\n";
print .102.v100, "\n";
print 100.101.102.v100, "\n";

print "\n--------------------\n";
print v101.v100, "\n";
print v100.101.102, "\n";
print v100.101.102.103, "\n";
sub v100 {
    return "FUNCTION";
}
print v100.101.102, "\n";
print v100.101.102.103, "\n";

print "\n--------------------\n";
print v101.102.v100, "\n";
print .102.v100, "\n";
print 100.101.102.v100, "\n";

print "\n--------------------\n";
print v101.101.102.103, "\n";
print v101.v100, "\n";
use constant "v101" => "CONSTANT";  # without the quotes v101 is interpreted as a v-string
print v101.101.102.103, "\n";
print v101, "\n";
print v101.v100, "\n";

# This next set produces a LOT of output
print "\n--------------------\n";
print "a"..102.103.104, "\n";
print "a" ..102.103.104, "\n";
print "a". .102.103.104, "\n";
print "a" . .102.103.104, "\n";

print "\n--------------------\n";
print "a". .102.103, "\n";
print "a" . .102.103, "\n";

print "\n--------------------\n";
print "An ordinary string v101.102.103.104 with an embedded v-string\n";
print "An ordinary string 101.102.103.104 with an embedded v-string\n";
print "v101.102.103.104", "\n";
print "101.102.103.104", "\n";
print <<TEXT;
An ordinary here doc v101.102.103.104 with an embedded v-string
An ordinary here doc 101.102.103.104 with an embedded v-string
TEXT
print <<v102;
An ordinary here doc with v-string delimiter
v102

print "\n--------------------\n";
print "before goto\n";
goto v102;
print "goto considered bad\n";
v102:
print "but good for testing\n";

print "\n--------------------\n";
v102: for my $i (1..10.0) {
    print "Testing ... ";
    next v102 if $i < 3;
    print "OK\n";
    last v102 if $i > 4;
}


__END__
