	# change elsif to if
	if (/a4/) { $width = 595; $length = 842; }
	elsif (/letter/) { $width = 612;  $length = 792; }
	elsif (/legal/) { $width  = 612;  $length = 1008; }
	elsif (/note/) { $width   = 540;  $length = 720; }
	elsif (/b5/) { $width     = 501;  $length = 709; }
	elsif (/a5/) { $width     = 421;  $length = 595; }
	elsif (/a6/) { $width     = 297;  $length = 421; }
	elsif (/a7/) { $width     = 210;  $length = 297; }
	elsif (/a8/) { $width     = 148;  $length = 210; }
	elsif (/a9/) { $width     = 105;  $length = 148; }
	elsif (/a10/) { $width    = 74;   $length = 105; }
	elsif (/b4/) { $width     = 709;  $length = 1002; }
	elsif (/a3/) { $width     = 842;  $length = 1190; }
	elsif (/b3/) { $width     = 1002; $length = 1418; }
	elsif (/a2/) { $width     = 1190; $length = 1684; }
	elsif (/b2/) { $width     = 1418; $length = 2004; }
	elsif (/a1/) { $width     = 1684; $length = 2380; }
	elsif (/b1/) { $width     = 2004; $length = 2836; }
	elsif (/a0/) { $width     = 2380; $length = 3368; }
	elsif (/b0/) { $width     = 2836; $length = 4013; }

