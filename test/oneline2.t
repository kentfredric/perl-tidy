    # Here is one long line to break. We have to recognize that the ','
    # before join will probably allow the block to stay intact
    warn "Warning: Some files contain no patches:", join ( "\n\t", '', map { $_->{in}; } @no_outs ), "\n";

    # Here the map block is already broken, and must be reformed
    warn "Warning: Some files contain no patches:", join ( "\n\t", '', map {
          $_->{in};
      } @no_outs ),
      "\n";
