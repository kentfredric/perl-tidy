# This is equivalent to elsif.pl
# Created from elsif with perl -MO=Deparse elsif.pl >elsif2.pl
/a4/ ? do { $width = 595; $length = 842 }
: ( /letter/ ? do { $width = 612;  $length = 792 }
: ( /legal/ ? do  { $width = 612;  $length = 1008 }
: ( /note/ ? do   { $width = 540;  $length = 720 }
: ( /b5/ ? do     { $width = 501;  $length = 709 }
: ( /a5/ ? do     { $width = 421;  $length = 595 }
: ( /a6/ ? do     { $width = 297;  $length = 421 }
: ( /a7/ ? do     { $width = 210;  $length = 297 }
: ( /a8/ ? do     { $width = 148;  $length = 210 }
: ( /a9/ ? do     { $width = 105;  $length = 148 }
: ( /a10/ ? do    { $width = 74;   $length = 105 }
: ( /b4/ ? do     { $width = 709;  $length = 1002 }
: ( /a3/ ? do     { $width = 842;  $length = 1190 }
: ( /b3/ ? do     { $width = 1002; $length = 1418 }
: ( /a2/ ? do     { $width = 1190; $length = 1684 }
: ( /b2/ ? do     { $width = 1418; $length = 2004 }
: ( /a1/ ? do     { $width = 1684; $length = 2380 }
: ( /b1/ ? do     { $width = 2004; $length = 2836 }
: ( /a0/ ? do     { $width = 2380; $length = 3368 }
: /b0/ && do { $width = 2836; $length = 4013 }
) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) );

