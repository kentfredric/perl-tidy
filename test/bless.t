    return bless { handle   => $handle,
                   Source   => $source,
                   Computer => $server } => $class;

   # Nice example of why break after comma (conway) helps (vtc=1 vs vtc=2):
    bless {_name      => $arg{name}      || croak("missing name"),
           _artist    => $arg{artist}    || "???",
           _publisher => $arg{publisher} || "???",
           _ISBN      => $arg{ISBN}      || "???",
           _tracks    => $arg{tracks}    || "???",
           _room      => $arg{room}      || "uncataloged",
           _shelf     => $arg{shelf}     || "",
           _rating    => $arg{rating}    || ask_rating($arg{name}),}, $class;
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
