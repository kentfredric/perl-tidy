#####################################################################
#
# The Perl::Tidy::LineBuffer class supplies a 'get_line()'
# method for returning the next line to be parsed, as well as a
# 'peek_ahead()' method
#
# The input parameter is an object with a 'get_line()' method
# which returns the next line to be parsed
#
#####################################################################

package Perl::Tidy::LineBuffer;

sub new {

    my $class              = shift;
    my $line_source_object = shift;

    return bless {
        _line_source_object => $line_source_object,
        _rlookahead_buffer  => [],
    }, $class;
}

sub peek_ahead {
    my $self               = shift;
    my $buffer_index       = shift;
    my $line               = undef;
    my $line_source_object = $self->{_line_source_object};
    my $rlookahead_buffer  = $self->{_rlookahead_buffer};
    if ( $buffer_index < scalar(@$rlookahead_buffer) ) {
        $line = $$rlookahead_buffer[$buffer_index];
    }
    else {
        $line = $line_source_object->get_line();
        push( @$rlookahead_buffer, $line );
    }
    return $line;
}

sub get_line {
    my $self               = shift;
    my $line               = undef;
    my $line_source_object = $self->{_line_source_object};
    my $rlookahead_buffer  = $self->{_rlookahead_buffer};

    if ( scalar(@$rlookahead_buffer) ) {
        $line = shift @$rlookahead_buffer;
    }
    else {
        $line = $line_source_object->get_line();
    }
    return $line;
}

1;
