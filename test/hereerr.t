# this is a here-doc:
$a = <<16;
Hi!
16
print "$a\n";

# this is a here doc with empty target
# which doesn't exist because there is no blank line at the end
$a = << 12  ;
Bye!
12
print "$a\n";
