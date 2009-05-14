#####################################################################
#
# the Perl::Tidy::VerticalAligner::Alignment class holds information
# on a single column being aligned
#
#####################################################################
package Perl::Tidy::VerticalAligner::Alignment;

{

    use strict;

    #use Carp;

    # Symbolic array indexes
    use constant COLUMN          => 0;    # the current column number
    use constant STARTING_COLUMN => 1;    # column number when created
    use constant MATCHING_TOKEN  => 2;    # what token we are matching
    use constant STARTING_LINE   => 3;    # the line index of creation
    use constant ENDING_LINE     => 4;    # the most recent line to use it
    use constant SAVED_COLUMN    => 5;    # the most recent line to use it
    use constant SERIAL_NUMBER   => 6;    # unique number for this alignment
                                          # (just its index in an array)

    # Correspondence between variables and array indexes
    my %_index_map;
    $_index_map{column}          = COLUMN;
    $_index_map{starting_column} = STARTING_COLUMN;
    $_index_map{matching_token}  = MATCHING_TOKEN;
    $_index_map{starting_line}   = STARTING_LINE;
    $_index_map{ending_line}     = ENDING_LINE;
    $_index_map{saved_column}    = SAVED_COLUMN;
    $_index_map{serial_number}   = SERIAL_NUMBER;

    my @_default_data = ();
    $_default_data[COLUMN]          = undef;
    $_default_data[STARTING_COLUMN] = undef;
    $_default_data[MATCHING_TOKEN]  = undef;
    $_default_data[STARTING_LINE]   = undef;
    $_default_data[ENDING_LINE]     = undef;
    $_default_data[SAVED_COLUMN]    = undef;
    $_default_data[SERIAL_NUMBER]   = undef;

    # class population count
    {
        my $_count = 0;
        sub get_count        { $_count; }
        sub _increment_count { ++$_count }
        sub _decrement_count { --$_count }
    }

    # constructor
    sub new {
        my ( $caller, %arg ) = @_;
        my $caller_is_obj = ref($caller);
        my $class = $caller_is_obj || $caller;
        no strict "refs";
        my $self = bless [], $class;

        foreach ( keys %_index_map ) {
            my $index = $_index_map{$_};
            if    ( exists $arg{$_} ) { $self->[$index] = $arg{$_} }
            elsif ($caller_is_obj)    { $self->[$index] = $caller->[$index] }
            else { $self->[$index] = $_default_data[$index] }
        }
        $self->_increment_count();
        return $self;
    }

    sub DESTROY {
        $_[0]->_decrement_count();
    }

    sub get_column          { return $_[0]->[COLUMN] }
    sub get_starting_column { return $_[0]->[STARTING_COLUMN] }
    sub get_matching_token  { return $_[0]->[MATCHING_TOKEN] }
    sub get_starting_line   { return $_[0]->[STARTING_LINE] }
    sub get_ending_line     { return $_[0]->[ENDING_LINE] }
    sub get_serial_number   { return $_[0]->[SERIAL_NUMBER] }

    sub set_column          { $_[0]->[COLUMN]          = $_[1] }
    sub set_starting_column { $_[0]->[STARTING_COLUMN] = $_[1] }
    sub set_matching_token  { $_[0]->[MATCHING_TOKEN]  = $_[1] }
    sub set_starting_line   { $_[0]->[STARTING_LINE]   = $_[1] }
    sub set_ending_line     { $_[0]->[ENDING_LINE]     = $_[1] }
    sub increment_column { $_[0]->[COLUMN] += $_[1] }

    sub save_column    { $_[0]->[SAVED_COLUMN] = $_[0]->[COLUMN] }
    sub restore_column { $_[0]->[COLUMN]       = $_[0]->[SAVED_COLUMN] }

}
1;
