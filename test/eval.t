	    # This is complex.  Two points to note: 
            # 1. no break between '}' and ')' because ')' is a 'brace_follower'.
            # 2. the opening '{' of the if should get outdented to line up
	    # with the 'if'
            if ( defined( $len = eval {
                  $ber->[ _POS() ] = 0;
                  unpack_tag($ber);
                  unpack_length($ber) + $ber->[ _POS() ];
              } ) && $len >= $ber->[ _POS() ] )
            {
                $n = $len;
                last;
            }

    # Another example of complex braces
    if ( eval { Convert::BER::_decode( $ber, $ref ) }
    ) {
        $$yes = 1 if ref($yes);
    }
    else {
        $$yes = undef if ref($yes);
        $ber->[ Convert::BER::_POS() ] = $pos;
    }

	# The comma here once caused trouble:
        if ( !GetOptions( \%Opts, @option_string ) ) {
            die "Programming Bug: error in setting default options";
        }


            # combined if and eval
            if ( defined( $len = eval {
                  $ber->[ _POS() ] = 0;
                  unpack_tag($ber);
                  unpack_length($ber) + $ber->[ _POS() ];
              } )

              # unpack_length will return -1 for unknown length
              && $len >= $ber->[ _POS() ] )
            {

                $n = $len;
                last;
            }

{
    # side comment interrupts one-line block formation attempt
    eval {
        # Prepare/execute query
          $stmt = $DB->prepare($cmd);
        $stmt->execute();
    };
}
    
# one-line eval block will die at the 'if' block
{
    eval {
        local (*DIR);
        if ( !opendir( DIR, $mhonarc::OUTDIR ) )
        {
            die qq/ERROR: Unable to open "$mhonarc::OUTDIR": $!\n"/;
        }
        }
}

$res = eval {0};
$res = eval {1} unless ($res);
