    my $self = bless [ {}, {} ], $class;

    bless { B => $B, Root => $Root } => $package;

    # do not break on ',' following '}' for bless, do, and eval

    bless {
        _name => $name, _price => $price, _rebate => $rebate
    }, $pkg;

    # note lack of outdent for the '}' because of the continuation
    do {
        $paragraph = <$fileHandle>;
      } while ( !eof($fileHandle)
      && ( $paragraph !~ /^\n?From .*\d:\d+:\d.* \d{4}/i ) );


    my $self =
      bless [ @_ == 1 ? shift: "", 0,
        ref($package) ? $package->[ Convert::BER::_INDEX() ] : [], ], $class;
