#!/usr/bin/perl -w
while (<>) {
	s/\&/&amp;/g;
	s/\</&lt;/g;
	s/\>/&gt;/g;
	s/\"/&quot;/g;
	print;
}
