    # A patch to 'terminal_type' was required to make the @files get 
    # continuation indentation.
    if ( $sortby eq 'date' or $sortby eq 'size' ) {
        @files =
          sort { $file_data{$a}{$soby} <=> $fe_data{$b}{$sortby} or $a cmp $b }
          @files;
    }

    my @loads = reverse map { $_ > LOAD_MAX ? LOAD_MAX : $_ }
      split /,\s*/, $up_string;

    my @xpi = sort { $unsorted_x->[$a] <=> $unsorted_x->[$b] }
      0 .. $#$unsorted_x;

	# the map block is broken here:
        sub xxx {
            map { $_->[0] }
              sort { $a->[1] cmp $b->[1] }
              map {
                [ $_, &$xform( $_ . "" ) ]
              }
              @_;
        }

	# same as above but with unbroken map block.
	# It should stay together as a one-line block
        sub xxx {
            map { $_->[0] }
              sort { $a->[1] cmp $b->[1] }
              map { [ $_, &$xform( $_ . "" ) ] } @_;
        }
