
#####################################################################
#
# The Perl::Tidy::Debugger class shows line tokenization
#
#####################################################################

package Perl::Tidy::Debugger;

sub new {

    my ( $class, $filename ) = @_;

    bless {
        _debug_file        => $filename,
        _debug_file_opened => 0,
        _fh                => undef,
    }, $class;
}

sub really_open_debug_file {

    my $self       = shift;
    my $debug_file = $self->{_debug_file};
    my $fh;
    unless ( $fh = IO::File->new("> $debug_file") ) {
        warn("can't open $debug_file: $!\n");
    }
    $self->{_debug_file_opened} = 1;
    $self->{_fh}                = $fh;
    print $fh
      "Use -dump-token-types (-dtt) to get a list of token type codes\n";
}

sub close_debug_file {

    my $self = shift;
    my $fh   = $self->{_fh};
    if ( $self->{_debug_file_opened} ) {

        eval { $self->{_fh}->close() };
    }
}

sub write_debug_entry {

    # This is a debug dump routine which may be modified as necessary
    # to dump tokens on a line-by-line basis.  The output will be written
    # to the .DEBUG file when the -D flag is entered.
    my $self           = shift;
    my $line_of_tokens = shift;

    my $input_line        = $line_of_tokens->{_line_text};
    my $rtoken_type       = $line_of_tokens->{_rtoken_type};
    my $rtokens           = $line_of_tokens->{_rtokens};
    my $rlevels           = $line_of_tokens->{_rlevels};
    my $rslevels          = $line_of_tokens->{_rslevels};
    my $rblock_type       = $line_of_tokens->{_rblock_type};
    my $input_line_number = $line_of_tokens->{_line_number};
    my $line_type         = $line_of_tokens->{_line_type};

    my ( $j, $num );

    my $token_str              = "$input_line_number: ";
    my $reconstructed_original = "$input_line_number: ";
    my $block_str              = "$input_line_number: ";

    #$token_str .= "$line_type: ";
    #$reconstructed_original .= "$line_type: ";

    my $pattern   = "";
    my @next_char = ( '"', '"' );
    my $i_next    = 0;
    unless ( $self->{_debug_file_opened} ) { $self->really_open_debug_file() }
    my $fh = $self->{_fh};

    for ( $j = 0 ; $j < @$rtoken_type ; $j++ ) {

        # testing patterns
        if ( $$rtoken_type[$j] eq 'k' ) {
            $pattern .= $$rtokens[$j];
        }
        else {
            $pattern .= $$rtoken_type[$j];
        }
        $reconstructed_original .= $$rtokens[$j];
        $block_str .= "($$rblock_type[$j])";
        $num = length( $$rtokens[$j] );
        my $type_str = $$rtoken_type[$j];

        # be sure there are no blank tokens (shouldn't happen)
        # This can only happen if a programming error has been made
        # because all valid tokens are non-blank
        if ( $type_str eq ' ' ) {
            print $fh "BLANK TOKEN on the next line\n";
            $type_str = $next_char[$i_next];
            $i_next   = 1 - $i_next;
        }

        if ( length($type_str) == 1 ) {
            $type_str = $type_str x $num;
        }
        $token_str .= $type_str;
    }

    # Write what you want here ...
    # print $fh "$input_line\n";
    # print $fh "$pattern\n";
    print $fh "$reconstructed_original\n";
    print $fh "$token_str\n";

    #print $fh "$block_str\n";
}

1;
