# This coding from BigFloat.pm is a challenge.
# The '},' needs special treatment in sub output_indented_line.
# Most lines are too long to get the => line up as it is in the original.

use overload
'+'	=>	sub {new Math::BigFloat &fadd},
'-'	=>	sub {new Math::BigFloat
		       $_[2]? fsub($_[1],${$_[0]}) : fsub(${$_[0]},$_[1])},
'<=>'	=>	sub {new Math::BigFloat
		       $_[2]? fcmp($_[1],${$_[0]}) : fcmp(${$_[0]},$_[1])},
'cmp'	=>	sub {new Math::BigFloat
		       $_[2]? ($_[1] cmp ${$_[0]}) : (${$_[0]} cmp $_[1])},
'*'	=>	sub {new Math::BigFloat &fmul},
'/'	=>	sub {new Math::BigFloat 
		       $_[2]? scalar fdiv($_[1],${$_[0]}) :
			 scalar fdiv(${$_[0]},$_[1])},
'neg'	=>	sub {new Math::BigFloat &fneg},
'abs'	=>	sub {new Math::BigFloat &fabs},

qw(
""	stringify
0+	numify)			# Order of arguments unsignificant
;

# The '},' needs special treatment here
use overload '+' => sub {
	print length $_[2], "\n";
	my ( $x, $y ) = _order(@_);
	Number::Roman->new( int $x + $y )
}, '-' => sub {
	my ( $x, $y ) = _order(@_);
	Number::Roman->new( int $x - $y )
}, '*' => sub {
	my ( $x, $y ) = _order(@_);
	Number::Roman->new( int $x * $y )
}, '/' => sub {
	my ( $x, $y ) = _order(@_);
	Number::Roman->new( int $x / $y )
}, '%' => sub {
	my ( $x, $y ) = _order(@_);
	Number::Roman->new( int $x % $y )
}, '**' => sub {
	my ( $x, $y ) = _order(@_);
	Number::Roman->new( int $x**$y )
}, '<=>' => sub { my ( $x, $y ) = _order(@_); $x <=> $y },
  '++' => "_incr",
  '--' => "_decr",
  '""' => sub {
	_toRoman( ${ $_[0] } )
}, '0+' => sub { ${ $_[0] } };

