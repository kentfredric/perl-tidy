# demonstrate perl's weird parsing rules
$a=4;
print $a/ 2, "/hi";    # division
print $a / 2, "/hi";    # division
print $a/ 2, "/hi";     # division
print $a /2,"/ hi ";  # pattern (and error)!

