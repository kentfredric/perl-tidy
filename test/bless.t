    my $self = bless [ {}, {} ], $class;

    bless { B => $B, Root => $Root } => $package;

    # do not break on ',' following '}' for bless, do, and eval

    bless {
        _name => $name, _price => $price, _rebate => $rebate
    }, $pkg;

    grep( do {
          if ( $i == $depth ) { $_++; }
          elsif ( $i > $depth ) { $_ = 0; }
          $i++;
          0;
      }, @curr_sec_id );

    # note lack of outdent for the '}' because of the continuation
    do {
        $paragraph = <$fileHandle>;
      } while ( !eof($fileHandle)
      && ( $paragraph !~ /^\n?From .*\d:\d+:\d.* \d{4}/i ) );


    my $self =
      bless [ @_ == 1 ? shift: "", 0,
        ref($package) ? $package->[ Convert::BER::_INDEX() ] : [], ], $class;
