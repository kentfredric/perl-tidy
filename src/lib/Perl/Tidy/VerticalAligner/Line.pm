#####################################################################
#
# the Perl::Tidy::VerticalAligner::Line class supplies an object to
# contain a single output line
#
#####################################################################

package Perl::Tidy::VerticalAligner::Line;

{

    use strict;
    use Carp;

    use constant JMAX                      => 0;
    use constant JMAX_ORIGINAL_LINE        => 1;
    use constant RTOKENS                   => 2;
    use constant RFIELDS                   => 3;
    use constant RPATTERNS                 => 4;
    use constant INDENTATION               => 5;
    use constant LEADING_SPACE_COUNT       => 6;
    use constant OUTDENT_LONG_LINES        => 7;
    use constant LIST_TYPE                 => 8;
    use constant IS_HANGING_SIDE_COMMENT   => 9;
    use constant RALIGNMENTS               => 10;
    use constant MAXIMUM_LINE_LENGTH       => 11;
    use constant RVERTICAL_TIGHTNESS_FLAGS => 12;

    my %_index_map;
    $_index_map{jmax}                      = JMAX;
    $_index_map{jmax_original_line}        = JMAX_ORIGINAL_LINE;
    $_index_map{rtokens}                   = RTOKENS;
    $_index_map{rfields}                   = RFIELDS;
    $_index_map{rpatterns}                 = RPATTERNS;
    $_index_map{indentation}               = INDENTATION;
    $_index_map{leading_space_count}       = LEADING_SPACE_COUNT;
    $_index_map{outdent_long_lines}        = OUTDENT_LONG_LINES;
    $_index_map{list_type}                 = LIST_TYPE;
    $_index_map{is_hanging_side_comment}   = IS_HANGING_SIDE_COMMENT;
    $_index_map{ralignments}               = RALIGNMENTS;
    $_index_map{maximum_line_length}       = MAXIMUM_LINE_LENGTH;
    $_index_map{rvertical_tightness_flags} = RVERTICAL_TIGHTNESS_FLAGS;

    my @_default_data = ();
    $_default_data[JMAX]                      = undef;
    $_default_data[JMAX_ORIGINAL_LINE]        = undef;
    $_default_data[RTOKENS]                   = undef;
    $_default_data[RFIELDS]                   = undef;
    $_default_data[RPATTERNS]                 = undef;
    $_default_data[INDENTATION]               = undef;
    $_default_data[LEADING_SPACE_COUNT]       = undef;
    $_default_data[OUTDENT_LONG_LINES]        = undef;
    $_default_data[LIST_TYPE]                 = undef;
    $_default_data[IS_HANGING_SIDE_COMMENT]   = undef;
    $_default_data[RALIGNMENTS]               = [];
    $_default_data[MAXIMUM_LINE_LENGTH]       = undef;
    $_default_data[RVERTICAL_TIGHTNESS_FLAGS] = undef;

    {

        # methods to count object population
        my $_count = 0;
        sub get_count        { $_count; }
        sub _increment_count { ++$_count }
        sub _decrement_count { --$_count }
    }

    # Constructor may be called as a class method
    sub new {
        my ( $caller, %arg ) = @_;
        my $caller_is_obj = ref($caller);
        my $class = $caller_is_obj || $caller;
        no strict "refs";
        my $self = bless [], $class;

        $self->[RALIGNMENTS] = [];

        my $index;
        foreach ( keys %_index_map ) {
            $index = $_index_map{$_};
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

    sub get_jmax                      { $_[0]->[JMAX] }
    sub get_jmax_original_line        { $_[0]->[JMAX_ORIGINAL_LINE] }
    sub get_rtokens                   { $_[0]->[RTOKENS] }
    sub get_rfields                   { $_[0]->[RFIELDS] }
    sub get_rpatterns                 { $_[0]->[RPATTERNS] }
    sub get_indentation               { $_[0]->[INDENTATION] }
    sub get_leading_space_count       { $_[0]->[LEADING_SPACE_COUNT] }
    sub get_outdent_long_lines        { $_[0]->[OUTDENT_LONG_LINES] }
    sub get_list_type                 { $_[0]->[LIST_TYPE] }
    sub get_is_hanging_side_comment   { $_[0]->[IS_HANGING_SIDE_COMMENT] }
    sub get_rvertical_tightness_flags { $_[0]->[RVERTICAL_TIGHTNESS_FLAGS] }

    sub set_column     { $_[0]->[RALIGNMENTS]->[ $_[1] ]->set_column( $_[2] ) }
    sub get_alignment  { $_[0]->[RALIGNMENTS]->[ $_[1] ] }
    sub get_alignments { @{ $_[0]->[RALIGNMENTS] } }
    sub get_column     { $_[0]->[RALIGNMENTS]->[ $_[1] ]->get_column() }

    sub get_starting_column {
        $_[0]->[RALIGNMENTS]->[ $_[1] ]->get_starting_column();
    }

    sub increment_column {
        $_[0]->[RALIGNMENTS]->[ $_[1] ]->increment_column( $_[2] );
    }
    sub set_alignments { my $self = shift; @{ $self->[RALIGNMENTS] } = @_; }

    sub current_field_width {
        my $self = shift;
        my ($j) = @_;
        if ( $j == 0 ) {
            return $self->get_column($j);
        }
        else {
            return $self->get_column($j) - $self->get_column( $j - 1 );
        }
    }

    sub field_width_growth {
        my $self = shift;
        my $j    = shift;
        return $self->get_column($j) - $self->get_starting_column($j);
    }

    sub starting_field_width {
        my $self = shift;
        my $j    = shift;
        if ( $j == 0 ) {
            return $self->get_starting_column($j);
        }
        else {
            return $self->get_starting_column($j) -
              $self->get_starting_column( $j - 1 );
        }
    }

    sub increase_field_width {

        my $self = shift;
        my ( $j, $pad ) = @_;
        my $jmax = $self->get_jmax();
        for my $k ( $j .. $jmax ) {
            $self->increment_column( $k, $pad );
        }
    }

    sub get_available_space_on_right {
        my $self = shift;
        my $jmax = $self->get_jmax();
        return $self->[MAXIMUM_LINE_LENGTH] - $self->get_column($jmax);
    }

    sub set_jmax                    { $_[0]->[JMAX]                    = $_[1] }
    sub set_jmax_original_line      { $_[0]->[JMAX_ORIGINAL_LINE]      = $_[1] }
    sub set_rtokens                 { $_[0]->[RTOKENS]                 = $_[1] }
    sub set_rfields                 { $_[0]->[RFIELDS]                 = $_[1] }
    sub set_rpatterns               { $_[0]->[RPATTERNS]               = $_[1] }
    sub set_indentation             { $_[0]->[INDENTATION]             = $_[1] }
    sub set_leading_space_count     { $_[0]->[LEADING_SPACE_COUNT]     = $_[1] }
    sub set_outdent_long_lines      { $_[0]->[OUTDENT_LONG_LINES]      = $_[1] }
    sub set_list_type               { $_[0]->[LIST_TYPE]               = $_[1] }
    sub set_is_hanging_side_comment { $_[0]->[IS_HANGING_SIDE_COMMENT] = $_[1] }
    sub set_alignment               { $_[0]->[RALIGNMENTS]->[ $_[1] ]  = $_[2] }

}

1
