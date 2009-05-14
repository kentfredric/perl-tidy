#####################################################################
#
# the Perl::Tidy::LineSink class supplies a write_line method for
# actual file writing
#
#####################################################################

package Perl::Tidy::LineSink;

sub new {

    my ( $class, $output_file, $tee_file, $line_separator, $rOpts,
        $rpending_logfile_message, $binmode )
      = @_;
    my $fh               = undef;
    my $fh_copy          = undef;
    my $fh_tee           = undef;
    my $output_file_copy = "";
    my $output_file_open = 0;

    if ( $rOpts->{'format'} eq 'tidy' ) {
        ( $fh, $output_file ) = Perl::Tidy::streamhandle( $output_file, 'w' );
        unless ($fh) { die "Cannot write to output stream\n"; }
        $output_file_open = 1;
        if ($binmode) {
            if ( ref($fh) eq 'IO::File' ) {
                binmode $fh;
            }
            if ( $output_file eq '-' ) { binmode STDOUT }
        }
    }

    # in order to check output syntax when standard output is used,
    # or when it is an object, we have to make a copy of the file
    if ( $output_file eq '-' || ref $output_file ) {
        if ( $rOpts->{'check-syntax'} ) {

            # Turning off syntax check when standard output is used.
            # The reason is that temporary files cause problems on
            # on many systems.
            $rOpts->{'check-syntax'} = 0;
            $output_file_copy = '-';
            $$rpending_logfile_message .= <<EOM;
Note: --syntax check will be skipped because standard output is used
EOM

        }
    }

    bless {
        _fh               => $fh,
        _fh_copy          => $fh_copy,
        _fh_tee           => $fh_tee,
        _output_file      => $output_file,
        _output_file_open => $output_file_open,
        _output_file_copy => $output_file_copy,
        _tee_flag         => 0,
        _tee_file         => $tee_file,
        _tee_file_opened  => 0,
        _line_separator   => $line_separator,
        _binmode          => $binmode,
    }, $class;
}

sub write_line {

    my $self    = shift;
    my $fh      = $self->{_fh};
    my $fh_copy = $self->{_fh_copy};

    my $output_file_open = $self->{_output_file_open};
    chomp $_[0];
    $_[0] .= $self->{_line_separator};

    $fh->print( $_[0] ) if ( $self->{_output_file_open} );
    print $fh_copy $_[0] if ( $fh_copy && $self->{_output_file_copy} );

    if ( $self->{_tee_flag} ) {
        unless ( $self->{_tee_file_opened} ) { $self->really_open_tee_file() }
        my $fh_tee = $self->{_fh_tee};
        print $fh_tee $_[0];
    }
}

sub get_output_file_copy {
    my $self   = shift;
    my $ofname = $self->{_output_file_copy};
    unless ($ofname) {
        $ofname = $self->{_output_file};
    }
    return $ofname;
}

sub tee_on {
    my $self = shift;
    $self->{_tee_flag} = 1;
}

sub tee_off {
    my $self = shift;
    $self->{_tee_flag} = 0;
}

sub really_open_tee_file {
    my $self     = shift;
    my $tee_file = $self->{_tee_file};
    my $fh_tee;
    $fh_tee = IO::File->new(">$tee_file")
      or die("couldn't open TEE file $tee_file: $!\n");
    binmode $fh_tee if $self->{_binmode};
    $self->{_tee_file_opened} = 1;
    $self->{_fh_tee}          = $fh_tee;
}

sub close_output_file {
    my $self = shift;
    eval { $self->{_fh}->close() }      if $self->{_output_file_open};
    eval { $self->{_fh_copy}->close() } if ( $self->{_output_file_copy} );
    $self->close_tee_file();
}

sub close_tee_file {
    my $self = shift;

    if ( $self->{_tee_file_opened} ) {
        eval { $self->{_fh_tee}->close() };
        $self->{_tee_file_opened} = 0;
    }
}

1;
