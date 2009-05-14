#####################################################################
#
# the Perl::Tidy::IndentationItem class supplies items which contain
# how much whitespace should be used at the start of a line
#
#####################################################################

package Perl::Tidy::IndentationItem;

# Indexes for indentation items
use constant SPACES             => 0;     # total leading white spaces
use constant LEVEL              => 1;     # the indentation 'level'
use constant CI_LEVEL           => 2;     # the 'continuation level'
use constant AVAILABLE_SPACES   => 3;     # how many left spaces available
                                          # for this level
use constant CLOSED             => 4;     # index where we saw closing '}'
use constant COMMA_COUNT        => 5;     # how many commas at this level?
use constant SEQUENCE_NUMBER    => 6;     # output batch number
use constant INDEX              => 7;     # index in output batch list
use constant HAVE_CHILD         => 8;     # any dependents?
use constant RECOVERABLE_SPACES => 9;     # how many spaces to the right
                                          # we would like to move to get
                                          # alignment (negative if left)
use constant ALIGN_PAREN        => 10;    # do we want to try to align
                                          # with an opening structure?
use constant MARKED             => 11;    # if visited by corrector logic
use constant STACK_DEPTH        => 12;    # indentation nesting depth
use constant STARTING_INDEX     => 13;    # first token index of this level
use constant ARROW_COUNT        => 14;    # how many =>'s

sub new {

    # Create an 'indentation_item' which describes one level of leading
    # whitespace when the '-lp' indentation is used.  We return
    # a reference to an anonymous array of associated variables.
    # See above constants for storage scheme.
    my (
        $class,               $spaces,           $level,
        $ci_level,            $available_spaces, $index,
        $gnu_sequence_number, $align_paren,      $stack_depth,
        $starting_index,
    ) = @_;
    my $closed            = -1;
    my $arrow_count       = 0;
    my $comma_count       = 0;
    my $have_child        = 0;
    my $want_right_spaces = 0;
    my $marked            = 0;
    bless [
        $spaces,              $level,          $ci_level,
        $available_spaces,    $closed,         $comma_count,
        $gnu_sequence_number, $index,          $have_child,
        $want_right_spaces,   $align_paren,    $marked,
        $stack_depth,         $starting_index, $arrow_count,
    ], $class;
}

sub permanently_decrease_AVAILABLE_SPACES {

    # make a permanent reduction in the available indentation spaces
    # at one indentation item.  NOTE: if there are child nodes, their
    # total SPACES must be reduced by the caller.

    my ( $item, $spaces_needed ) = @_;
    my $available_spaces = $item->get_AVAILABLE_SPACES();
    my $deleted_spaces =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $item->decrease_AVAILABLE_SPACES($deleted_spaces);
    $item->decrease_SPACES($deleted_spaces);
    $item->set_RECOVERABLE_SPACES(0);

    return $deleted_spaces;
}

sub tentatively_decrease_AVAILABLE_SPACES {

    # We are asked to tentatively delete $spaces_needed of indentation
    # for a indentation item.  We may want to undo this later.  NOTE: if
    # there are child nodes, their total SPACES must be reduced by the
    # caller.
    my ( $item, $spaces_needed ) = @_;
    my $available_spaces = $item->get_AVAILABLE_SPACES();
    my $deleted_spaces =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $item->decrease_AVAILABLE_SPACES($deleted_spaces);
    $item->decrease_SPACES($deleted_spaces);
    $item->increase_RECOVERABLE_SPACES($deleted_spaces);
    return $deleted_spaces;
}

sub get_STACK_DEPTH {
    my $self = shift;
    return $self->[STACK_DEPTH];
}

sub get_SPACES {
    my $self = shift;
    return $self->[SPACES];
}

sub get_MARKED {
    my $self = shift;
    return $self->[MARKED];
}

sub set_MARKED {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[MARKED] = $value;
    }
    return $self->[MARKED];
}

sub get_AVAILABLE_SPACES {
    my $self = shift;
    return $self->[AVAILABLE_SPACES];
}

sub decrease_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[SPACES] -= $value;
    }
    return $self->[SPACES];
}

sub decrease_AVAILABLE_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[AVAILABLE_SPACES] -= $value;
    }
    return $self->[AVAILABLE_SPACES];
}

sub get_ALIGN_PAREN {
    my $self = shift;
    return $self->[ALIGN_PAREN];
}

sub get_RECOVERABLE_SPACES {
    my $self = shift;
    return $self->[RECOVERABLE_SPACES];
}

sub set_RECOVERABLE_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[RECOVERABLE_SPACES] = $value;
    }
    return $self->[RECOVERABLE_SPACES];
}

sub increase_RECOVERABLE_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[RECOVERABLE_SPACES] += $value;
    }
    return $self->[RECOVERABLE_SPACES];
}

sub get_CI_LEVEL {
    my $self = shift;
    return $self->[CI_LEVEL];
}

sub get_LEVEL {
    my $self = shift;
    return $self->[LEVEL];
}

sub get_SEQUENCE_NUMBER {
    my $self = shift;
    return $self->[SEQUENCE_NUMBER];
}

sub get_INDEX {
    my $self = shift;
    return $self->[INDEX];
}

sub get_STARTING_INDEX {
    my $self = shift;
    return $self->[STARTING_INDEX];
}

sub set_HAVE_CHILD {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[HAVE_CHILD] = $value;
    }
    return $self->[HAVE_CHILD];
}

sub get_HAVE_CHILD {
    my $self = shift;
    return $self->[HAVE_CHILD];
}

sub set_ARROW_COUNT {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[ARROW_COUNT] = $value;
    }
    return $self->[ARROW_COUNT];
}

sub get_ARROW_COUNT {
    my $self = shift;
    return $self->[ARROW_COUNT];
}

sub set_COMMA_COUNT {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[COMMA_COUNT] = $value;
    }
    return $self->[COMMA_COUNT];
}

sub get_COMMA_COUNT {
    my $self = shift;
    return $self->[COMMA_COUNT];
}

sub set_CLOSED {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[CLOSED] = $value;
    }
    return $self->[CLOSED];
}

sub get_CLOSED {
    my $self = shift;
    return $self->[CLOSED];
}

1;
