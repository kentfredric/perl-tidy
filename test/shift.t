      # will break before this shift operator
      ( length( $self->{"password"} ) + 1 ) << 8;                          # PASSWD LEN (clobbers long passwds?)
