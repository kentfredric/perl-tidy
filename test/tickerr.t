# A few tests for parsing embedded single quote marks
# This is valid (note lack of space after word join)
@array =qw(1 2 3);
$a = join'abc ',@array;
print $a;

# This sub definition is ok
sub join'abc{
"hello, world!\n";
}

# This will work
$a=&join'abc; 
print $a;

# But this should generate an error (missing ending ')
$a=join'abc(); 
print $a;
