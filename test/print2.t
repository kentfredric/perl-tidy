# if you let perltidy put a space between 'print' and '(' in this script,
# you change the interpretation from list to function and
# you get an error stating that print is being interpreted as a
# function call!
BEGIN {
	$^W = 1;
	$SIG{__WARN__} = sub { die "Dying on warning: ", @_ };
}
$a=2, print("hello!\n"), $a=1;
