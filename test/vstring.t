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
foreach my $bb (@a) {
    printf "%2d: [", $i++;
    print $bb, "]\n";
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
# goto v102;  does not work now
goto L101;
print "goto considered bad\n";
# v102:  does not work now
L101:
print "but good for testing\n";

print "\n--------------------\n";
L102: for my $i (1..10.0) {
    print "Testing ... ";
    next L102 if $i < 3;
    print "OK\n";
    last L102 if $i > 4;
}

my $utf=(
12298.26131.32463.12299.31532.19968.21350.
24406.26352.65306.
22823.21705.20094.20803.65292.19975.29289.36164.22987.65292.
20035.32479.22825.12290.
20113.34892.38632.26045.65292.21697.29289.27969.24418.12290.
22823.26126.22987.32456.65292.20845.20301.26102.25104.65292.
26102.20056.20845.40857.20197.24481.22825.12290.
20094.36947.21464.21270.65292.21508.27491.24615.21629.65292.
20445.21512.22823.21644.65292.20035.21033.36126.12290.
39318.20986.24246.29289.65292.19975.22269.21688.23425.12290
    );


__END__
