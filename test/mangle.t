# run with --mangle
# Troublesome punctuation variables: $$ and $#

# don't delete ws between '$$' and 'if'
kill 'ABRT', $$ if $panic++;

# Do not remove the space between '$#' and 'eq'
$, = "Hello, World!\n";
$#=$,; 
print "$# ";
$# eq $,? print "yes\n" : print "no\n";

# The space after the '?' is essential and must not be deleted
print $::opt_m ? "  Files:  ".my_wrap("","          ",$v) : $v;
