#####################################################################
#
# the Perl::Tidy::LineSource class supplies an object with a 'get_line()' method
# which returns the next line to be parsed
#
#####################################################################

package Perl::Tidy::LineSource;

sub new {

    my ( $class, $input_file, $rOpts, $rpending_logfile_message ) = @_;
    my $input_file_copy = undef;
    my $fh_copy;

    my $input_line_ending;
    if ( $rOpts->{'preserve-line-endings'} ) {
        $input_line_ending = Perl::Tidy::find_input_line_ending($input_file);
    }

    ( my $fh, $input_file ) = Perl::Tidy::streamhandle( $input_file, 'r' );
    return undef unless $fh;

    # in order to check output syntax when standard output is used,
    # or when it is an object, we have to make a copy of the file
    if ( ( $input_file eq '-' || ref $input_file ) && $rOpts->{'check-syntax'} )
    {

        # Turning off syntax check when input output is used.
        # The reason is that temporary files cause problems on
        # on many systems.
        $rOpts->{'check-syntax'} = 0;
        $input_file_copy = '-';

        $$rpending_logfile_message .= <<EOM;
Note: --syntax check will be skipped because standard input is used
EOM

    }

    return bless {
        _fh                => $fh,
        _fh_copy           => $fh_copy,
        _filename          => $input_file,
        _input_file_copy   => $input_file_copy,
        _input_line_ending => $input_line_ending,
        _rinput_buffer     => [],
        _started           => 0,
    }, $class;
}

sub get_input_file_copy_name {
    my $self   = shift;
    my $ifname = $self->{_input_file_copy};
    unless ($ifname) {
        $ifname = $self->{_filename};
    }
    return $ifname;
}

sub close_input_file {
    my $self = shift;
    eval { $self->{_fh}->close() };
    eval { $self->{_fh_copy}->close() } if $self->{_fh_copy};
}

sub get_line {
    my $self          = shift;
    my $line          = undef;
    my $fh            = $self->{_fh};
    my $fh_copy       = $self->{_fh_copy};
    my $rinput_buffer = $self->{_rinput_buffer};

    if ( scalar(@$rinput_buffer) ) {
        $line = shift @$rinput_buffer;
    }
    else {
        $line = $fh->getline();

        # patch to read raw mac files under unix, dos
        # see if the first line has embedded \r's
        if ( $line && !$self->{_started} ) {
            if ( $line =~ /[\015][^\015\012]/ ) {

                # found one -- break the line up and store in a buffer
                @$rinput_buffer = map { $_ . "\n" } split /\015/, $line;
                my $count = @$rinput_buffer;
                $line = shift @$rinput_buffer;
            }
            $self->{_started}++;
        }
    }
    if ( $line && $fh_copy ) { $fh_copy->print($line); }
    return $line;
}

1;
