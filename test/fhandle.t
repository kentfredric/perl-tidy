	# perl lets you write stderr in lower case, but perltidy should
	# complain
        print stderr "stderr works for me\n";
	
	# 'hello' is a sub call, not a file handle
	# since there is no space between 'hello' and '('
        print hello();

        sub hello {
	return "Hello World!\n";
	}

	# 'goodbye' is a file handle, not a sub call
	# since we haven't seen sub goodbye, and there is a space after it
	# This will generate an error message
        print main::main::main::goodbye ();

        sub goodbye {
	return "GoodBye World!\n";
	}

	print usage() and exit;
	sub usage {
		print "Don't even think of using this\n";
	}

	# header is a sub call here; perltidy will mark it type 'Z'
        use CGI':all';print header,start_html('x');
