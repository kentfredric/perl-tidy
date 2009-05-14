#####################################################################
#
# The Perl::Tidy::Diagnostics class writes the DIAGNOSTICS file, which is
# useful for program development.
#
# Only one such file is created regardless of the number of input
# files processed.  This allows the results of processing many files
# to be summarized in a single file.
#
#####################################################################

package Perl::Tidy::Diagnostics;

sub new {

    my $class = shift;
    bless {
        _write_diagnostics_count => 0,
        _last_diagnostic_file    => "",
        _input_file              => "",
        _fh                      => undef,
    }, $class;
}

sub set_input_file {
    my $self = shift;
    $self->{_input_file} = $_[0];
}

# This is a diagnostic routine which is useful for program development.
# Output from debug messages go to a file named DIAGNOSTICS, where
# they are labeled by file and line.  This allows many files to be
# scanned at once for some particular condition of interest.
sub write_diagnostics {
    my $self = shift;

    unless ( $self->{_write_diagnostics_count} ) {
        open DIAGNOSTICS, ">DIAGNOSTICS"
          or death("couldn't open DIAGNOSTICS: $!\n");
    }

    my $last_diagnostic_file = $self->{_last_diagnostic_file};
    my $input_file           = $self->{_input_file};
    if ( $last_diagnostic_file ne $input_file ) {
        print DIAGNOSTICS "\nFILE:$input_file\n";
    }
    $self->{_last_diagnostic_file} = $input_file;
    my $input_line_number = Perl::Tidy::Tokenizer::get_input_line_number();
    print DIAGNOSTICS "$input_line_number:\t@_";
    $self->{_write_diagnostics_count}++;
}
1;
