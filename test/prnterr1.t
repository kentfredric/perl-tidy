	# the space before the '?' makes a difference near a possible filehandle
	$inside = 1;

	# Without a space, it is a conditional
	print $inside?"</UL>":"<HR></UL>";

	# With a space, perl takes this as the start of a pattern:
	print $inside ?"</UL>":"<HR></UL>";

	# and you get an error message
