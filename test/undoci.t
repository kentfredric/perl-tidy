	# undoci.t: testing undo-continuation indentation in sub flush
	# the continuation indentation of the '-stipple' field will be undone
	# to make the fields line up
        $rinfo{deleteStyle} = [
            -fill      => 'red',
              -stipple => '@' . Tk->findINC('demos/images/grey.25'),
        ];

	   # another example; this tests logic in store_tokens_to_go
       $root{$vol} = {
       	path       => $vol . $SEPARATOR,
       	  prefix   => $vol . $SEPARATOR,
       	  srcpath  => $vol . $SEPARATOR,
       	  'exists' => 1
       };
