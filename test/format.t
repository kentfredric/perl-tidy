    if (/^--list$/o) {
        format =
@<<<<<<<<<<<<<<<<<<<<<<<< 	@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$_, $val
.
          print "Available strips:\n";
        for ( split ( /\|/, $known_strips ) ) {
            $val = $defs{$_}{'name'};
            write;
        }
    }

