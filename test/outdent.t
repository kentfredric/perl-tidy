{ { {

			# outdenting long lines
			# This entire line fits
			print "Copyright (C) 1999 Free Software Foundation, Inc.\n";

			# This needs to be broken and outdented
			print "This is free software; see the source for copying conditions.  There is NO\n";

			# This will be broken after 'print' and outdented, but it still
			# exceeds 80 columns
			print "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n";
} } }
