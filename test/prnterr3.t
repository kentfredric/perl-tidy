# demonstrate perl's weird parsing rules
# perltidy's color html output shows the problem
print $a/ 2, "/hi";    # division
print $a / 2, "/hi";    # division
print $a/ 2, "/hi";     # division
print $a /2,"/ hi ";  # pattern (and error)!

