# A simple test hash
# conversions of month number into letters (0-11)
my %MonthChars = ('0', 'Jan',
		  '1', 'Feb',
		  '2', 'Mar',
		  '3', 'Apr',
		  '4', 'May',
		  '5', 'Jun',
		  '6', 'Jul',
		  '7', 'Aug',
		  '8', 'Sep',
		  '9', 'Oct',
		  '10','Nov',
		  '11','Dec');

# an example created from epsmerge
sub check_paper
{
my
$sizeref
=
{
'a2'
=>
[
1190,
1684
]
,
'a3'
=>
[
842,
1190
]
,
'a4'
=>
[
595,
842
]
,
'a5'
=>
[
420,
595
]
,
'b4'
=>
[
709,
1001
]
,
'b5'
=>
[
499,
709
]
,
'letter'
=>
[
612,
792
]
,
'tabloid'
=>
[
792,
1224
]
,
'ledger'
=>
[
1224,
792
]
,
'legal'
=>
[
612,
1008
]
,
'executive'
=>
[
540,
720
]
,
}
->{
lc
(
shift
)
};
return
$sizeref;
}


