#####################################################################
#
# The Perl::Tidy::Formatter package adds indentation, whitespace, and
# line breaks to the token stream
#
# WARNING: This is not a real class for speed reasons.  Only one
# Formatter may be used.
#
#####################################################################

package Perl::Tidy::Formatter;

BEGIN {

    # Caution: these debug flags produce a lot of output
    # They should all be 0 except when debugging small scripts
    use constant FORMATTER_DEBUG_FLAG_BOND    => 0;
    use constant FORMATTER_DEBUG_FLAG_BREAK   => 0;
    use constant FORMATTER_DEBUG_FLAG_CI      => 0;
    use constant FORMATTER_DEBUG_FLAG_FLUSH   => 0;
    use constant FORMATTER_DEBUG_FLAG_FORCE   => 0;
    use constant FORMATTER_DEBUG_FLAG_LIST    => 0;
    use constant FORMATTER_DEBUG_FLAG_NOBREAK => 0;
    use constant FORMATTER_DEBUG_FLAG_OUTPUT  => 0;
    use constant FORMATTER_DEBUG_FLAG_SPARSE  => 0;
    use constant FORMATTER_DEBUG_FLAG_STORE   => 0;
    use constant FORMATTER_DEBUG_FLAG_UNDOBP  => 0;
    use constant FORMATTER_DEBUG_FLAG_WHITE   => 0;

    my $debug_warning = sub {
        print "FORMATTER_DEBUGGING with key $_[0]\n";
    };

    FORMATTER_DEBUG_FLAG_BOND    && $debug_warning->('BOND');
    FORMATTER_DEBUG_FLAG_BREAK   && $debug_warning->('BREAK');
    FORMATTER_DEBUG_FLAG_CI      && $debug_warning->('CI');
    FORMATTER_DEBUG_FLAG_FLUSH   && $debug_warning->('FLUSH');
    FORMATTER_DEBUG_FLAG_FORCE   && $debug_warning->('FORCE');
    FORMATTER_DEBUG_FLAG_LIST    && $debug_warning->('LIST');
    FORMATTER_DEBUG_FLAG_NOBREAK && $debug_warning->('NOBREAK');
    FORMATTER_DEBUG_FLAG_OUTPUT  && $debug_warning->('OUTPUT');
    FORMATTER_DEBUG_FLAG_SPARSE  && $debug_warning->('SPARSE');
    FORMATTER_DEBUG_FLAG_STORE   && $debug_warning->('STORE');
    FORMATTER_DEBUG_FLAG_UNDOBP  && $debug_warning->('UNDOBP');
    FORMATTER_DEBUG_FLAG_WHITE   && $debug_warning->('WHITE');
}

use Carp;
use vars qw{

  @gnu_stack
  $max_gnu_stack_index
  $gnu_position_predictor
  $line_start_index_to_go
  $last_indentation_written
  $last_unadjusted_indentation
  $last_leading_token

  $saw_VERSION_in_this_file
  $saw_END_or_DATA_

  @gnu_item_list
  $max_gnu_item_index
  $gnu_sequence_number
  $last_output_indentation
  %last_gnu_equals
  %gnu_comma_count
  %gnu_arrow_count

  @block_type_to_go
  @type_sequence_to_go
  @container_environment_to_go
  @bond_strength_to_go
  @forced_breakpoint_to_go
  @lengths_to_go
  @levels_to_go
  @leading_spaces_to_go
  @reduced_spaces_to_go
  @matching_token_to_go
  @mate_index_to_go
  @nesting_blocks_to_go
  @ci_levels_to_go
  @nesting_depth_to_go
  @nobreak_to_go
  @old_breakpoint_to_go
  @tokens_to_go
  @types_to_go

  %saved_opening_indentation

  $max_index_to_go
  $comma_count_in_batch
  $old_line_count_in_batch
  $last_nonblank_index_to_go
  $last_nonblank_type_to_go
  $last_nonblank_token_to_go
  $last_last_nonblank_index_to_go
  $last_last_nonblank_type_to_go
  $last_last_nonblank_token_to_go
  @nonblank_lines_at_depth
  $starting_in_quote
  $ending_in_quote

  $in_format_skipping_section
  $format_skipping_pattern_begin
  $format_skipping_pattern_end

  $forced_breakpoint_count
  $forced_breakpoint_undo_count
  @forced_breakpoint_undo_stack
  %postponed_breakpoint

  $tabbing
  $embedded_tab_count
  $first_embedded_tab_at
  $last_embedded_tab_at
  $deleted_semicolon_count
  $first_deleted_semicolon_at
  $last_deleted_semicolon_at
  $added_semicolon_count
  $first_added_semicolon_at
  $last_added_semicolon_at
  $first_tabbing_disagreement
  $last_tabbing_disagreement
  $in_tabbing_disagreement
  $tabbing_disagreement_count
  $input_line_tabbing

  $last_line_type
  $last_line_leading_type
  $last_line_leading_level
  $last_last_line_leading_level

  %block_leading_text
  %block_opening_line_number
  $csc_new_statement_ok
  $accumulating_text_for_block
  $leading_block_text
  $rleading_block_if_elsif_text
  $leading_block_text_level
  $leading_block_text_length_exceeded
  $leading_block_text_line_length
  $leading_block_text_line_number
  $closing_side_comment_prefix_pattern
  $closing_side_comment_list_pattern

  $last_nonblank_token
  $last_nonblank_type
  $last_last_nonblank_token
  $last_last_nonblank_type
  $last_nonblank_block_type
  $last_output_level
  %is_do_follower
  %is_if_brace_follower
  %space_after_keyword
  $rbrace_follower
  $looking_for_else
  %is_last_next_redo_return
  %is_other_brace_follower
  %is_else_brace_follower
  %is_anon_sub_brace_follower
  %is_anon_sub_1_brace_follower
  %is_sort_map_grep
  %is_sort_map_grep_eval
  %is_sort_map_grep_eval_do
  %is_block_without_semicolon
  %is_if_unless
  %is_and_or
  %is_assignment
  %is_chain_operator
  %is_if_unless_and_or_last_next_redo_return
  %is_until_while_for_if_elsif_else

  @has_broken_sublist
  @dont_align
  @want_comma_break

  $is_static_block_comment
  $index_start_one_line_block
  $semicolons_before_block_self_destruct
  $index_max_forced_break
  $input_line_number
  $diagnostics_object
  $vertical_aligner_object
  $logger_object
  $file_writer_object
  $formatter_self
  @ci_stack
  $last_line_had_side_comment
  %want_break_before
  %outdent_keyword
  $static_block_comment_pattern
  $static_side_comment_pattern
  %opening_vertical_tightness
  %closing_vertical_tightness
  %closing_token_indentation

  %opening_token_right
  %stack_opening_token
  %stack_closing_token

  $block_brace_vertical_tightness_pattern

  $rOpts_add_newlines
  $rOpts_add_whitespace
  $rOpts_block_brace_tightness
  $rOpts_block_brace_vertical_tightness
  $rOpts_brace_left_and_indent
  $rOpts_comma_arrow_breakpoints
  $rOpts_break_at_old_keyword_breakpoints
  $rOpts_break_at_old_comma_breakpoints
  $rOpts_break_at_old_logical_breakpoints
  $rOpts_break_at_old_ternary_breakpoints
  $rOpts_closing_side_comment_else_flag
  $rOpts_closing_side_comment_maximum_text
  $rOpts_continuation_indentation
  $rOpts_cuddled_else
  $rOpts_delete_old_whitespace
  $rOpts_fuzzy_line_length
  $rOpts_indent_columns
  $rOpts_line_up_parentheses
  $rOpts_maximum_fields_per_table
  $rOpts_maximum_line_length
  $rOpts_short_concatenation_item_length
  $rOpts_swallow_optional_blank_lines
  $rOpts_ignore_old_breakpoints
  $rOpts_format_skipping
  $rOpts_space_function_paren
  $rOpts_space_keyword_paren
  $rOpts_keep_interior_semicolons

  $half_maximum_line_length

  %is_opening_type
  %is_closing_type
  %is_keyword_returning_list
  %tightness
  %matching_token
  $rOpts
  %right_bond_strength
  %left_bond_strength
  %binary_ws_rules
  %want_left_space
  %want_right_space
  %is_digraph
  %is_trigraph
  $bli_pattern
  $bli_list_string
  %is_closing_type
  %is_opening_type
  %is_closing_token
  %is_opening_token
};

BEGIN {

    # default list of block types for which -bli would apply
    $bli_list_string = 'if else elsif unless while for foreach do : sub';

    @_ = qw(
      .. :: << >> ** && .. || // -> => += -= .= %= &= |= ^= *= <>
      <= >= == =~ !~ != ++ -- /= x=
    );
    @is_digraph{@_} = (1) x scalar(@_);

    @_ = qw( ... **= <<= >>= &&= ||= //= <=> );
    @is_trigraph{@_} = (1) x scalar(@_);

    @_ = qw(
      = **= += *= &= <<= &&=
      -= /= |= >>= ||= //=
      .= %= ^=
      x=
    );
    @is_assignment{@_} = (1) x scalar(@_);

    @_ = qw(
      grep
      keys
      map
      reverse
      sort
      split
    );
    @is_keyword_returning_list{@_} = (1) x scalar(@_);

    @_ = qw(is if unless and or err last next redo return);
    @is_if_unless_and_or_last_next_redo_return{@_} = (1) x scalar(@_);

    # always break after a closing curly of these block types:
    @_ = qw(until while for if elsif else);
    @is_until_while_for_if_elsif_else{@_} = (1) x scalar(@_);

    @_ = qw(last next redo return);
    @is_last_next_redo_return{@_} = (1) x scalar(@_);

    @_ = qw(sort map grep);
    @is_sort_map_grep{@_} = (1) x scalar(@_);

    @_ = qw(sort map grep eval);
    @is_sort_map_grep_eval{@_} = (1) x scalar(@_);

    @_ = qw(sort map grep eval do);
    @is_sort_map_grep_eval_do{@_} = (1) x scalar(@_);

    @_ = qw(if unless);
    @is_if_unless{@_} = (1) x scalar(@_);

    @_ = qw(and or err);
    @is_and_or{@_} = (1) x scalar(@_);

    # Identify certain operators which often occur in chains.
    # Note: the minus (-) causes a side effect of padding of the first line in
    # something like this (by sub set_logical_padding):
    #    Checkbutton => 'Transmission checked',
    #   -variable    => \$TRANS
    # This usually improves appearance so it seems ok.
    @_ = qw(&& || and or : ? . + - * /);
    @is_chain_operator{@_} = (1) x scalar(@_);

    # We can remove semicolons after blocks preceded by these keywords
    @_ =
      qw(BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue if elsif else
      unless while until for foreach);
    @is_block_without_semicolon{@_} = (1) x scalar(@_);

    # 'L' is token for opening { at hash key
    @_ = qw" L { ( [ ";
    @is_opening_type{@_} = (1) x scalar(@_);

    # 'R' is token for closing } at hash key
    @_ = qw" R } ) ] ";
    @is_closing_type{@_} = (1) x scalar(@_);

    @_ = qw" { ( [ ";
    @is_opening_token{@_} = (1) x scalar(@_);

    @_ = qw" } ) ] ";
    @is_closing_token{@_} = (1) x scalar(@_);
}

# whitespace codes
use constant WS_YES      => 1;
use constant WS_OPTIONAL => 0;
use constant WS_NO       => -1;

# Token bond strengths.
use constant NO_BREAK    => 10000;
use constant VERY_STRONG => 100;
use constant STRONG      => 2.1;
use constant NOMINAL     => 1.1;
use constant WEAK        => 0.8;
use constant VERY_WEAK   => 0.55;

# values for testing indexes in output array
use constant UNDEFINED_INDEX => -1;

# Maximum number of little messages; probably need not be changed.
use constant MAX_NAG_MESSAGES => 6;

# increment between sequence numbers for each type
# For example, ?: pairs might have numbers 7,11,15,...
use constant TYPE_SEQUENCE_INCREMENT => 4;

{

    # methods to count instances
    my $_count = 0;
    sub get_count        { $_count; }
    sub _increment_count { ++$_count }
    sub _decrement_count { --$_count }
}

sub trim {

    # trim leading and trailing whitespace from a string
    $_[0] =~ s/\s+$//;
    $_[0] =~ s/^\s+//;
    return $_[0];
}

sub split_words {

    # given a string containing words separated by whitespace,
    # return the list of words
    my ($str) = @_;
    return unless $str;
    $str =~ s/\s+$//;
    $str =~ s/^\s+//;
    return split( /\s+/, $str );
}

# interface to Perl::Tidy::Logger routines
sub warning {
    if ($logger_object) {
        $logger_object->warning(@_);
    }
}

sub complain {
    if ($logger_object) {
        $logger_object->complain(@_);
    }
}

sub write_logfile_entry {
    if ($logger_object) {
        $logger_object->write_logfile_entry(@_);
    }
}

sub black_box {
    if ($logger_object) {
        $logger_object->black_box(@_);
    }
}

sub report_definite_bug {
    if ($logger_object) {
        $logger_object->report_definite_bug();
    }
}

sub get_saw_brace_error {
    if ($logger_object) {
        $logger_object->get_saw_brace_error();
    }
}

sub we_are_at_the_last_line {
    if ($logger_object) {
        $logger_object->we_are_at_the_last_line();
    }
}

# interface to Perl::Tidy::Diagnostics routine
sub write_diagnostics {

    if ($diagnostics_object) {
        $diagnostics_object->write_diagnostics(@_);
    }
}

sub get_added_semicolon_count {
    my $self = shift;
    return $added_semicolon_count;
}

sub DESTROY {
    $_[0]->_decrement_count();
}

sub new {

    my $class = shift;

    # we are given an object with a write_line() method to take lines
    my %defaults = (
        sink_object        => undef,
        diagnostics_object => undef,
        logger_object      => undef,
    );
    my %args = ( %defaults, @_ );

    $logger_object      = $args{logger_object};
    $diagnostics_object = $args{diagnostics_object};

    # we create another object with a get_line() and peek_ahead() method
    my $sink_object = $args{sink_object};
    $file_writer_object =
      Perl::Tidy::FileWriter->new( $sink_object, $rOpts, $logger_object );

    # initialize the leading whitespace stack to negative levels
    # so that we can never run off the end of the stack
    $gnu_position_predictor = 0;    # where the current token is predicted to be
    $max_gnu_stack_index    = 0;
    $max_gnu_item_index     = -1;
    $gnu_stack[0] = new_lp_indentation_item( 0, -1, -1, 0, 0 );
    @gnu_item_list               = ();
    $last_output_indentation     = 0;
    $last_indentation_written    = 0;
    $last_unadjusted_indentation = 0;
    $last_leading_token          = "";

    $saw_VERSION_in_this_file = !$rOpts->{'pass-version-line'};
    $saw_END_or_DATA_         = 0;

    @block_type_to_go            = ();
    @type_sequence_to_go         = ();
    @container_environment_to_go = ();
    @bond_strength_to_go         = ();
    @forced_breakpoint_to_go     = ();
    @lengths_to_go               = ();    # line length to start of ith token
    @levels_to_go                = ();
    @matching_token_to_go        = ();
    @mate_index_to_go            = ();
    @nesting_blocks_to_go        = ();
    @ci_levels_to_go             = ();
    @nesting_depth_to_go         = (0);
    @nobreak_to_go               = ();
    @old_breakpoint_to_go        = ();
    @tokens_to_go                = ();
    @types_to_go                 = ();
    @leading_spaces_to_go        = ();
    @reduced_spaces_to_go        = ();

    @dont_align         = ();
    @has_broken_sublist = ();
    @want_comma_break   = ();

    @ci_stack                   = ("");
    $first_tabbing_disagreement = 0;
    $last_tabbing_disagreement  = 0;
    $tabbing_disagreement_count = 0;
    $in_tabbing_disagreement    = 0;
    $input_line_tabbing         = undef;

    $last_line_type               = "";
    $last_last_line_leading_level = 0;
    $last_line_leading_level      = 0;
    $last_line_leading_type       = '#';

    $last_nonblank_token        = ';';
    $last_nonblank_type         = ';';
    $last_last_nonblank_token   = ';';
    $last_last_nonblank_type    = ';';
    $last_nonblank_block_type   = "";
    $last_output_level          = 0;
    $looking_for_else           = 0;
    $embedded_tab_count         = 0;
    $first_embedded_tab_at      = 0;
    $last_embedded_tab_at       = 0;
    $deleted_semicolon_count    = 0;
    $first_deleted_semicolon_at = 0;
    $last_deleted_semicolon_at  = 0;
    $added_semicolon_count      = 0;
    $first_added_semicolon_at   = 0;
    $last_added_semicolon_at    = 0;
    $last_line_had_side_comment = 0;
    $is_static_block_comment    = 0;
    %postponed_breakpoint       = ();

    # variables for adding side comments
    %block_leading_text        = ();
    %block_opening_line_number = ();
    $csc_new_statement_ok      = 1;

    %saved_opening_indentation  = ();
    $in_format_skipping_section = 0;

    reset_block_text_accumulator();

    prepare_for_new_input_lines();

    $vertical_aligner_object =
      Perl::Tidy::VerticalAligner->initialize( $rOpts, $file_writer_object,
        $logger_object, $diagnostics_object );

    if ( $rOpts->{'entab-leading-whitespace'} ) {
        write_logfile_entry(
"Leading whitespace will be entabbed with $rOpts->{'entab-leading-whitespace'} spaces per tab\n"
        );
    }
    elsif ( $rOpts->{'tabs'} ) {
        write_logfile_entry("Indentation will be with a tab character\n");
    }
    else {
        write_logfile_entry(
            "Indentation will be with $rOpts->{'indent-columns'} spaces\n");
    }

    # This was the start of a formatter referent, but object-oriented
    # coding has turned out to be too slow here.
    $formatter_self = {};

    bless $formatter_self, $class;

    # Safety check..this is not a class yet
    if ( _increment_count() > 1 ) {
        confess
"Attempt to create more than 1 object in $class, which is not a true class yet\n";
    }
    return $formatter_self;
}

sub prepare_for_new_input_lines {

    $gnu_sequence_number++;    # increment output batch counter
    %last_gnu_equals                = ();
    %gnu_comma_count                = ();
    %gnu_arrow_count                = ();
    $line_start_index_to_go         = 0;
    $max_gnu_item_index             = UNDEFINED_INDEX;
    $index_max_forced_break         = UNDEFINED_INDEX;
    $max_index_to_go                = UNDEFINED_INDEX;
    $last_nonblank_index_to_go      = UNDEFINED_INDEX;
    $last_nonblank_type_to_go       = '';
    $last_nonblank_token_to_go      = '';
    $last_last_nonblank_index_to_go = UNDEFINED_INDEX;
    $last_last_nonblank_type_to_go  = '';
    $last_last_nonblank_token_to_go = '';
    $forced_breakpoint_count        = 0;
    $forced_breakpoint_undo_count   = 0;
    $rbrace_follower                = undef;
    $lengths_to_go[0]               = 0;
    $old_line_count_in_batch        = 1;
    $comma_count_in_batch           = 0;
    $starting_in_quote              = 0;

    destroy_one_line_block();
}

sub write_line {

    my $self = shift;
    my ($line_of_tokens) = @_;

    my $line_type  = $line_of_tokens->{_line_type};
    my $input_line = $line_of_tokens->{_line_text};

    # _line_type codes are:
    #   SYSTEM         - system-specific code before hash-bang line
    #   CODE           - line of perl code (including comments)
    #   POD_START      - line starting pod, such as '=head'
    #   POD            - pod documentation text
    #   POD_END        - last line of pod section, '=cut'
    #   HERE           - text of here-document
    #   HERE_END       - last line of here-doc (target word)
    #   FORMAT         - format section
    #   FORMAT_END     - last line of format section, '.'
    #   DATA_START     - __DATA__ line
    #   DATA           - unidentified text following __DATA__
    #   END_START      - __END__ line
    #   END            - unidentified text following __END__
    #   ERROR          - we are in big trouble, probably not a perl script

    # put a blank line after an =cut which comes before __END__ and __DATA__
    # (required by podchecker)
    if ( $last_line_type eq 'POD_END' && !$saw_END_or_DATA_ ) {
        $file_writer_object->reset_consecutive_blank_lines();
        if ( $input_line !~ /^\s*$/ ) { want_blank_line() }
    }

    # handle line of code..
    if ( $line_type eq 'CODE' ) {

        # let logger see all non-blank lines of code
        if ( $input_line !~ /^\s*$/ ) {
            my $output_line_number =
              $vertical_aligner_object->get_output_line_number();
            black_box( $line_of_tokens, $output_line_number );
        }
        print_line_of_tokens($line_of_tokens);
    }

    # handle line of non-code..
    else {

        # set special flags
        my $skip_line = 0;
        my $tee_line  = 0;
        if ( $line_type =~ /^POD/ ) {

            # Pod docs should have a preceding blank line.  But be
            # very careful in __END__ and __DATA__ sections, because:
            #   1. the user may be using this section for any purpose whatsoever
            #   2. the blank counters are not active there
            # It should be safe to request a blank line between an
            # __END__ or __DATA__ and an immediately following '=head'
            # type line, (types END_START and DATA_START), but not for
            # any other lines of type END or DATA.
            if ( $rOpts->{'delete-pod'} ) { $skip_line = 1; }
            if ( $rOpts->{'tee-pod'} )    { $tee_line  = 1; }
            if (  !$skip_line
                && $line_type eq 'POD_START'
                && $last_line_type !~ /^(END|DATA)$/ )
            {
                want_blank_line();
            }
        }

        # leave the blank counters in a predictable state
        # after __END__ or __DATA__
        elsif ( $line_type =~ /^(END_START|DATA_START)$/ ) {
            $file_writer_object->reset_consecutive_blank_lines();
            $saw_END_or_DATA_ = 1;
        }

        # write unindented non-code line
        if ( !$skip_line ) {
            if ($tee_line) { $file_writer_object->tee_on() }
            write_unindented_line($input_line);
            if ($tee_line) { $file_writer_object->tee_off() }
        }
    }
    $last_line_type = $line_type;
}

sub create_one_line_block {
    $index_start_one_line_block            = $_[0];
    $semicolons_before_block_self_destruct = $_[1];
}

sub destroy_one_line_block {
    $index_start_one_line_block            = UNDEFINED_INDEX;
    $semicolons_before_block_self_destruct = 0;
}

sub leading_spaces_to_go {

    # return the number of indentation spaces for a token in the output stream;
    # these were previously stored by 'set_leading_whitespace'.

    return get_SPACES( $leading_spaces_to_go[ $_[0] ] );

}

sub get_SPACES {

    # return the number of leading spaces associated with an indentation
    # variable $indentation is either a constant number of spaces or an object
    # with a get_SPACES method.
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_SPACES() : $indentation;
}

sub get_RECOVERABLE_SPACES {

    # return the number of spaces (+ means shift right, - means shift left)
    # that we would like to shift a group of lines with the same indentation
    # to get them to line up with their opening parens
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_RECOVERABLE_SPACES() : 0;
}

sub get_AVAILABLE_SPACES_to_go {

    my $item = $leading_spaces_to_go[ $_[0] ];

    # return the number of available leading spaces associated with an
    # indentation variable.  $indentation is either a constant number of
    # spaces or an object with a get_AVAILABLE_SPACES method.
    return ref($item) ? $item->get_AVAILABLE_SPACES() : 0;
}

sub new_lp_indentation_item {

    # this is an interface to the IndentationItem class
    my ( $spaces, $level, $ci_level, $available_spaces, $align_paren ) = @_;

    # A negative level implies not to store the item in the item_list
    my $index = 0;
    if ( $level >= 0 ) { $index = ++$max_gnu_item_index; }

    my $item = Perl::Tidy::IndentationItem->new(
        $spaces,      $level,
        $ci_level,    $available_spaces,
        $index,       $gnu_sequence_number,
        $align_paren, $max_gnu_stack_index,
        $line_start_index_to_go,
    );

    if ( $level >= 0 ) {
        $gnu_item_list[$max_gnu_item_index] = $item;
    }

    return $item;
}

sub set_leading_whitespace {

    # This routine defines leading whitespace
    # given: the level and continuation_level of a token,
    # define: space count of leading string which would apply if it
    # were the first token of a new line.

    my ( $level, $ci_level, $in_continued_quote ) = @_;

    # modify for -bli, which adds one continuation indentation for
    # opening braces
    if (   $rOpts_brace_left_and_indent
        && $max_index_to_go == 0
        && $block_type_to_go[$max_index_to_go] =~ /$bli_pattern/o )
    {
        $ci_level++;
    }

    # patch to avoid trouble when input file has negative indentation.
    # other logic should catch this error.
    if ( $level < 0 ) { $level = 0 }

    #-------------------------------------------
    # handle the standard indentation scheme
    #-------------------------------------------
    unless ($rOpts_line_up_parentheses) {
        my $space_count =
          $ci_level * $rOpts_continuation_indentation +
          $level * $rOpts_indent_columns;
        my $ci_spaces =
          ( $ci_level == 0 ) ? 0 : $rOpts_continuation_indentation;

        if ($in_continued_quote) {
            $space_count = 0;
            $ci_spaces   = 0;
        }
        $leading_spaces_to_go[$max_index_to_go] = $space_count;
        $reduced_spaces_to_go[$max_index_to_go] = $space_count - $ci_spaces;
        return;
    }

    #-------------------------------------------------------------
    # handle case of -lp indentation..
    #-------------------------------------------------------------

    # The continued_quote flag means that this is the first token of a
    # line, and it is the continuation of some kind of multi-line quote
    # or pattern.  It requires special treatment because it must have no
    # added leading whitespace. So we create a special indentation item
    # which is not in the stack.
    if ($in_continued_quote) {
        my $space_count     = 0;
        my $available_space = 0;
        $level = -1;    # flag to prevent storing in item_list
        $leading_spaces_to_go[$max_index_to_go] =
          $reduced_spaces_to_go[$max_index_to_go] =
          new_lp_indentation_item( $space_count, $level, $ci_level,
            $available_space, 0 );
        return;
    }

    # get the top state from the stack
    my $space_count      = $gnu_stack[$max_gnu_stack_index]->get_SPACES();
    my $current_level    = $gnu_stack[$max_gnu_stack_index]->get_LEVEL();
    my $current_ci_level = $gnu_stack[$max_gnu_stack_index]->get_CI_LEVEL();

    my $type        = $types_to_go[$max_index_to_go];
    my $token       = $tokens_to_go[$max_index_to_go];
    my $total_depth = $nesting_depth_to_go[$max_index_to_go];

    if ( $type eq '{' || $type eq '(' ) {

        $gnu_comma_count{ $total_depth + 1 } = 0;
        $gnu_arrow_count{ $total_depth + 1 } = 0;

        # If we come to an opening token after an '=' token of some type,
        # see if it would be helpful to 'break' after the '=' to save space
        my $last_equals = $last_gnu_equals{$total_depth};
        if ( $last_equals && $last_equals > $line_start_index_to_go ) {

            # find the position if we break at the '='
            my $i_test = $last_equals;
            if ( $types_to_go[ $i_test + 1 ] eq 'b' ) { $i_test++ }

            # TESTING
            ##my $too_close = ($i_test==$max_index_to_go-1);

            my $test_position = total_line_length( $i_test, $max_index_to_go );

            if (

                # the equals is not just before an open paren (testing)
                ##!$too_close &&

                # if we are beyond the midpoint
                $gnu_position_predictor > $half_maximum_line_length

                # or we are beyont the 1/4 point and there was an old
                # break at the equals
                || (
                    $gnu_position_predictor > $half_maximum_line_length / 2
                    && (
                        $old_breakpoint_to_go[$last_equals]
                        || (   $last_equals > 0
                            && $old_breakpoint_to_go[ $last_equals - 1 ] )
                        || (   $last_equals > 1
                            && $types_to_go[ $last_equals - 1 ] eq 'b'
                            && $old_breakpoint_to_go[ $last_equals - 2 ] )
                    )
                )
              )
            {

                # then make the switch -- note that we do not set a real
                # breakpoint here because we may not really need one; sub
                # scan_list will do that if necessary
                $line_start_index_to_go = $i_test + 1;
                $gnu_position_predictor = $test_position;
            }
        }
    }

    # Check for decreasing depth ..
    # Note that one token may have both decreasing and then increasing
    # depth. For example, (level, ci) can go from (1,1) to (2,0).  So,
    # in this example we would first go back to (1,0) then up to (2,0)
    # in a single call.
    if ( $level < $current_level || $ci_level < $current_ci_level ) {

        # loop to find the first entry at or completely below this level
        my ( $lev, $ci_lev );
        while (1) {
            if ($max_gnu_stack_index) {

                # save index of token which closes this level
                $gnu_stack[$max_gnu_stack_index]->set_CLOSED($max_index_to_go);

                # Undo any extra indentation if we saw no commas
                my $available_spaces =
                  $gnu_stack[$max_gnu_stack_index]->get_AVAILABLE_SPACES();

                my $comma_count = 0;
                my $arrow_count = 0;
                if ( $type eq '}' || $type eq ')' ) {
                    $comma_count = $gnu_comma_count{$total_depth};
                    $arrow_count = $gnu_arrow_count{$total_depth};
                    $comma_count = 0 unless $comma_count;
                    $arrow_count = 0 unless $arrow_count;
                }
                $gnu_stack[$max_gnu_stack_index]->set_COMMA_COUNT($comma_count);
                $gnu_stack[$max_gnu_stack_index]->set_ARROW_COUNT($arrow_count);

                if ( $available_spaces > 0 ) {

                    if ( $comma_count <= 0 || $arrow_count > 0 ) {

                        my $i = $gnu_stack[$max_gnu_stack_index]->get_INDEX();
                        my $seqno =
                          $gnu_stack[$max_gnu_stack_index]
                          ->get_SEQUENCE_NUMBER();

                        # Be sure this item was created in this batch.  This
                        # should be true because we delete any available
                        # space from open items at the end of each batch.
                        if (   $gnu_sequence_number != $seqno
                            || $i > $max_gnu_item_index )
                        {
                            warning(
"Program bug with -lp.  seqno=$seqno should be $gnu_sequence_number and i=$i should be less than max=$max_gnu_item_index\n"
                            );
                            report_definite_bug();
                        }

                        else {
                            if ( $arrow_count == 0 ) {
                                $gnu_item_list[$i]
                                  ->permanently_decrease_AVAILABLE_SPACES(
                                    $available_spaces);
                            }
                            else {
                                $gnu_item_list[$i]
                                  ->tentatively_decrease_AVAILABLE_SPACES(
                                    $available_spaces);
                            }

                            my $j;
                            for (
                                $j = $i + 1 ;
                                $j <= $max_gnu_item_index ;
                                $j++
                              )
                            {
                                $gnu_item_list[$j]
                                  ->decrease_SPACES($available_spaces);
                            }
                        }
                    }
                }

                # go down one level
                --$max_gnu_stack_index;
                $lev    = $gnu_stack[$max_gnu_stack_index]->get_LEVEL();
                $ci_lev = $gnu_stack[$max_gnu_stack_index]->get_CI_LEVEL();

                # stop when we reach a level at or below the current level
                if ( $lev <= $level && $ci_lev <= $ci_level ) {
                    $space_count =
                      $gnu_stack[$max_gnu_stack_index]->get_SPACES();
                    $current_level    = $lev;
                    $current_ci_level = $ci_lev;
                    last;
                }
            }

            # reached bottom of stack .. should never happen because
            # only negative levels can get here, and $level was forced
            # to be positive above.
            else {
                warning(
"program bug with -lp: stack_error. level=$level; lev=$lev; ci_level=$ci_level; ci_lev=$ci_lev; rerun with -nlp\n"
                );
                report_definite_bug();
                last;
            }
        }
    }

    # handle increasing depth
    if ( $level > $current_level || $ci_level > $current_ci_level ) {

        # Compute the standard incremental whitespace.  This will be
        # the minimum incremental whitespace that will be used.  This
        # choice results in a smooth transition between the gnu-style
        # and the standard style.
        my $standard_increment =
          ( $level - $current_level ) * $rOpts_indent_columns +
          ( $ci_level - $current_ci_level ) * $rOpts_continuation_indentation;

        # Now we have to define how much extra incremental space
        # ("$available_space") we want.  This extra space will be
        # reduced as necessary when long lines are encountered or when
        # it becomes clear that we do not have a good list.
        my $available_space = 0;
        my $align_paren     = 0;
        my $excess          = 0;

        # initialization on empty stack..
        if ( $max_gnu_stack_index == 0 ) {
            $space_count = $level * $rOpts_indent_columns;
        }

        # if this is a BLOCK, add the standard increment
        elsif ($last_nonblank_block_type) {
            $space_count += $standard_increment;
        }

        # if last nonblank token was not structural indentation,
        # just use standard increment
        elsif ( $last_nonblank_type ne '{' ) {
            $space_count += $standard_increment;
        }

        # otherwise use the space to the first non-blank level change token
        else {

            $space_count = $gnu_position_predictor;

            my $min_gnu_indentation =
              $gnu_stack[$max_gnu_stack_index]->get_SPACES();

            $available_space = $space_count - $min_gnu_indentation;
            if ( $available_space >= $standard_increment ) {
                $min_gnu_indentation += $standard_increment;
            }
            elsif ( $available_space > 1 ) {
                $min_gnu_indentation += $available_space + 1;
            }
            elsif ( $last_nonblank_token =~ /^[\{\[\(]$/ ) {
                if ( ( $tightness{$last_nonblank_token} < 2 ) ) {
                    $min_gnu_indentation += 2;
                }
                else {
                    $min_gnu_indentation += 1;
                }
            }
            else {
                $min_gnu_indentation += $standard_increment;
            }
            $available_space = $space_count - $min_gnu_indentation;

            if ( $available_space < 0 ) {
                $space_count     = $min_gnu_indentation;
                $available_space = 0;
            }
            $align_paren = 1;
        }

        # update state, but not on a blank token
        if ( $types_to_go[$max_index_to_go] ne 'b' ) {

            $gnu_stack[$max_gnu_stack_index]->set_HAVE_CHILD(1);

            ++$max_gnu_stack_index;
            $gnu_stack[$max_gnu_stack_index] =
              new_lp_indentation_item( $space_count, $level, $ci_level,
                $available_space, $align_paren );

            # If the opening paren is beyond the half-line length, then
            # we will use the minimum (standard) indentation.  This will
            # help avoid problems associated with running out of space
            # near the end of a line.  As a result, in deeply nested
            # lists, there will be some indentations which are limited
            # to this minimum standard indentation. But the most deeply
            # nested container will still probably be able to shift its
            # parameters to the right for proper alignment, so in most
            # cases this will not be noticable.
            if (   $available_space > 0
                && $space_count > $half_maximum_line_length )
            {
                $gnu_stack[$max_gnu_stack_index]
                  ->tentatively_decrease_AVAILABLE_SPACES($available_space);
            }
        }
    }

    # Count commas and look for non-list characters.  Once we see a
    # non-list character, we give up and don't look for any more commas.
    if ( $type eq '=>' ) {
        $gnu_arrow_count{$total_depth}++;

        # tentatively treating '=>' like '=' for estimating breaks
        # TODO: this could use some experimentation
        $last_gnu_equals{$total_depth} = $max_index_to_go;
    }

    elsif ( $type eq ',' ) {
        $gnu_comma_count{$total_depth}++;
    }

    elsif ( $is_assignment{$type} ) {
        $last_gnu_equals{$total_depth} = $max_index_to_go;
    }

    # this token might start a new line
    # if this is a non-blank..
    if ( $type ne 'b' ) {

        # and if ..
        if (

            # this is the first nonblank token of the line
            $max_index_to_go == 1 && $types_to_go[0] eq 'b'

            # or previous character was one of these:
            || $last_nonblank_type_to_go =~ /^([\:\?\,f])$/

            # or previous character was opening and this does not close it
            || ( $last_nonblank_type_to_go eq '{' && $type ne '}' )
            || ( $last_nonblank_type_to_go eq '(' and $type ne ')' )

            # or this token is one of these:
            || $type =~ /^([\.]|\|\||\&\&)$/

            # or this is a closing structure
            || (   $last_nonblank_type_to_go eq '}'
                && $last_nonblank_token_to_go eq $last_nonblank_type_to_go )

            # or previous token was keyword 'return'
            || ( $last_nonblank_type_to_go eq 'k'
                && ( $last_nonblank_token_to_go eq 'return' && $type ne '{' ) )

            # or starting a new line at certain keywords is fine
            || (   $type eq 'k'
                && $is_if_unless_and_or_last_next_redo_return{$token} )

            # or this is after an assignment after a closing structure
            || (
                $is_assignment{$last_nonblank_type_to_go}
                && (
                    $last_last_nonblank_type_to_go =~ /^[\}\)\]]$/

                    # and it is significantly to the right
                    || $gnu_position_predictor > $half_maximum_line_length
                )
            )
          )
        {
            check_for_long_gnu_style_lines();
            $line_start_index_to_go = $max_index_to_go;

            # back up 1 token if we want to break before that type
            # otherwise, we may strand tokens like '?' or ':' on a line
            if ( $line_start_index_to_go > 0 ) {
                if ( $last_nonblank_type_to_go eq 'k' ) {

                    if ( $want_break_before{$last_nonblank_token_to_go} ) {
                        $line_start_index_to_go--;
                    }
                }
                elsif ( $want_break_before{$last_nonblank_type_to_go} ) {
                    $line_start_index_to_go--;
                }
            }
        }
    }

    # remember the predicted position of this token on the output line
    if ( $max_index_to_go > $line_start_index_to_go ) {
        $gnu_position_predictor =
          total_line_length( $line_start_index_to_go, $max_index_to_go );
    }
    else {
        $gnu_position_predictor = $space_count +
          token_sequence_length( $max_index_to_go, $max_index_to_go );
    }

    # store the indentation object for this token
    # this allows us to manipulate the leading whitespace
    # (in case we have to reduce indentation to fit a line) without
    # having to change any token values
    $leading_spaces_to_go[$max_index_to_go] = $gnu_stack[$max_gnu_stack_index];
    $reduced_spaces_to_go[$max_index_to_go] =
      ( $max_gnu_stack_index > 0 && $ci_level )
      ? $gnu_stack[ $max_gnu_stack_index - 1 ]
      : $gnu_stack[$max_gnu_stack_index];
    return;
}

sub check_for_long_gnu_style_lines {

    # look at the current estimated maximum line length, and
    # remove some whitespace if it exceeds the desired maximum

    # this is only for the '-lp' style
    return unless ($rOpts_line_up_parentheses);

    # nothing can be done if no stack items defined for this line
    return if ( $max_gnu_item_index == UNDEFINED_INDEX );

    # see if we have exceeded the maximum desired line length
    # keep 2 extra free because they are needed in some cases
    # (result of trial-and-error testing)
    my $spaces_needed =
      $gnu_position_predictor - $rOpts_maximum_line_length + 2;

    return if ( $spaces_needed < 0 );

    # We are over the limit, so try to remove a requested number of
    # spaces from leading whitespace.  We are only allowed to remove
    # from whitespace items created on this batch, since others have
    # already been used and cannot be undone.
    my @candidates = ();
    my $i;

    # loop over all whitespace items created for the current batch
    for ( $i = 0 ; $i <= $max_gnu_item_index ; $i++ ) {
        my $item = $gnu_item_list[$i];

        # item must still be open to be a candidate (otherwise it
        # cannot influence the current token)
        next if ( $item->get_CLOSED() >= 0 );

        my $available_spaces = $item->get_AVAILABLE_SPACES();

        if ( $available_spaces > 0 ) {
            push( @candidates, [ $i, $available_spaces ] );
        }
    }

    return unless (@candidates);

    # sort by available whitespace so that we can remove whitespace
    # from the maximum available first
    @candidates = sort { $b->[1] <=> $a->[1] } @candidates;

    # keep removing whitespace until we are done or have no more
    my $candidate;
    foreach $candidate (@candidates) {
        my ( $i, $available_spaces ) = @{$candidate};
        my $deleted_spaces =
          ( $available_spaces > $spaces_needed )
          ? $spaces_needed
          : $available_spaces;

        # remove the incremental space from this item
        $gnu_item_list[$i]->decrease_AVAILABLE_SPACES($deleted_spaces);

        my $i_debug = $i;

        # update the leading whitespace of this item and all items
        # that came after it
        for ( ; $i <= $max_gnu_item_index ; $i++ ) {

            my $old_spaces = $gnu_item_list[$i]->get_SPACES();
            if ( $old_spaces > $deleted_spaces ) {
                $gnu_item_list[$i]->decrease_SPACES($deleted_spaces);
            }

            # shouldn't happen except for code bug:
            else {
                my $level        = $gnu_item_list[$i_debug]->get_LEVEL();
                my $ci_level     = $gnu_item_list[$i_debug]->get_CI_LEVEL();
                my $old_level    = $gnu_item_list[$i]->get_LEVEL();
                my $old_ci_level = $gnu_item_list[$i]->get_CI_LEVEL();
                warning(
"program bug with -lp: want to delete $deleted_spaces from item $i, but old=$old_spaces deleted: lev=$level ci=$ci_level  deleted: level=$old_level ci=$ci_level\n"
                );
                report_definite_bug();
            }
        }
        $gnu_position_predictor -= $deleted_spaces;
        $spaces_needed          -= $deleted_spaces;
        last unless ( $spaces_needed > 0 );
    }
}

sub finish_lp_batch {

    # This routine is called once after each each output stream batch is
    # finished to undo indentation for all incomplete -lp
    # indentation levels.  It is too risky to leave a level open,
    # because then we can't backtrack in case of a long line to follow.
    # This means that comments and blank lines will disrupt this
    # indentation style.  But the vertical aligner may be able to
    # get the space back if there are side comments.

    # this is only for the 'lp' style
    return unless ($rOpts_line_up_parentheses);

    # nothing can be done if no stack items defined for this line
    return if ( $max_gnu_item_index == UNDEFINED_INDEX );

    # loop over all whitespace items created for the current batch
    my $i;
    for ( $i = 0 ; $i <= $max_gnu_item_index ; $i++ ) {
        my $item = $gnu_item_list[$i];

        # only look for open items
        next if ( $item->get_CLOSED() >= 0 );

        # Tentatively remove all of the available space
        # (The vertical aligner will try to get it back later)
        my $available_spaces = $item->get_AVAILABLE_SPACES();
        if ( $available_spaces > 0 ) {

            # delete incremental space for this item
            $gnu_item_list[$i]
              ->tentatively_decrease_AVAILABLE_SPACES($available_spaces);

            # Reduce the total indentation space of any nodes that follow
            # Note that any such nodes must necessarily be dependents
            # of this node.
            foreach ( $i + 1 .. $max_gnu_item_index ) {
                $gnu_item_list[$_]->decrease_SPACES($available_spaces);
            }
        }
    }
    return;
}

sub reduce_lp_indentation {

    # reduce the leading whitespace at token $i if possible by $spaces_needed
    # (a large value of $spaces_needed will remove all excess space)
    # NOTE: to be called from scan_list only for a sequence of tokens
    # contained between opening and closing parens/braces/brackets

    my ( $i, $spaces_wanted ) = @_;
    my $deleted_spaces = 0;

    my $item             = $leading_spaces_to_go[$i];
    my $available_spaces = $item->get_AVAILABLE_SPACES();

    if (
        $available_spaces > 0
        && ( ( $spaces_wanted <= $available_spaces )
            || !$item->get_HAVE_CHILD() )
      )
    {

        # we'll remove these spaces, but mark them as recoverable
        $deleted_spaces =
          $item->tentatively_decrease_AVAILABLE_SPACES($spaces_wanted);
    }

    return $deleted_spaces;
}

sub token_sequence_length {

    # return length of tokens ($ifirst .. $ilast) including first & last
    # returns 0 if $ifirst > $ilast
    my $ifirst = shift;
    my $ilast  = shift;
    return 0 if ( $ilast < 0 || $ifirst > $ilast );
    return $lengths_to_go[ $ilast + 1 ] if ( $ifirst < 0 );
    return $lengths_to_go[ $ilast + 1 ] - $lengths_to_go[$ifirst];
}

sub total_line_length {

    # return length of a line of tokens ($ifirst .. $ilast)
    my $ifirst = shift;
    my $ilast  = shift;
    if ( $ifirst < 0 ) { $ifirst = 0 }

    return leading_spaces_to_go($ifirst) +
      token_sequence_length( $ifirst, $ilast );
}

sub excess_line_length {

    # return number of characters by which a line of tokens ($ifirst..$ilast)
    # exceeds the allowable line length.
    my $ifirst = shift;
    my $ilast  = shift;
    if ( $ifirst < 0 ) { $ifirst = 0 }
    return leading_spaces_to_go($ifirst) +
      token_sequence_length( $ifirst, $ilast ) - $rOpts_maximum_line_length;
}

sub finish_formatting {

    # flush buffer and write any informative messages
    my $self = shift;

    flush();
    $file_writer_object->decrement_output_line_number()
      ;    # fix up line number since it was incremented
    we_are_at_the_last_line();
    if ( $added_semicolon_count > 0 ) {
        my $first = ( $added_semicolon_count > 1 ) ? "First" : "";
        my $what =
          ( $added_semicolon_count > 1 ) ? "semicolons were" : "semicolon was";
        write_logfile_entry("$added_semicolon_count $what added:\n");
        write_logfile_entry(
            "  $first at input line $first_added_semicolon_at\n");

        if ( $added_semicolon_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_added_semicolon_at\n");
        }
        write_logfile_entry("  (Use -nasc to prevent semicolon addition)\n");
        write_logfile_entry("\n");
    }

    if ( $deleted_semicolon_count > 0 ) {
        my $first = ( $deleted_semicolon_count > 1 ) ? "First" : "";
        my $what =
          ( $deleted_semicolon_count > 1 )
          ? "semicolons were"
          : "semicolon was";
        write_logfile_entry(
            "$deleted_semicolon_count unnecessary $what deleted:\n");
        write_logfile_entry(
            "  $first at input line $first_deleted_semicolon_at\n");

        if ( $deleted_semicolon_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_deleted_semicolon_at\n");
        }
        write_logfile_entry("  (Use -ndsc to prevent semicolon deletion)\n");
        write_logfile_entry("\n");
    }

    if ( $embedded_tab_count > 0 ) {
        my $first = ( $embedded_tab_count > 1 ) ? "First" : "";
        my $what =
          ( $embedded_tab_count > 1 )
          ? "quotes or patterns"
          : "quote or pattern";
        write_logfile_entry("$embedded_tab_count $what had embedded tabs:\n");
        write_logfile_entry(
"This means the display of this script could vary with device or software\n"
        );
        write_logfile_entry("  $first at input line $first_embedded_tab_at\n");

        if ( $embedded_tab_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_embedded_tab_at\n");
        }
        write_logfile_entry("\n");
    }

    if ($first_tabbing_disagreement) {
        write_logfile_entry(
"First indentation disagreement seen at input line $first_tabbing_disagreement\n"
        );
    }

    if ($in_tabbing_disagreement) {
        write_logfile_entry(
"Ending with indentation disagreement which started at input line $in_tabbing_disagreement\n"
        );
    }
    else {

        if ($last_tabbing_disagreement) {

            write_logfile_entry(
"Last indentation disagreement seen at input line $last_tabbing_disagreement\n"
            );
        }
        else {
            write_logfile_entry("No indentation disagreement seen\n");
        }
    }
    write_logfile_entry("\n");

    $vertical_aligner_object->report_anything_unusual();

    $file_writer_object->report_line_length_errors();
}

sub check_options {

    # This routine is called to check the Opts hash after it is defined

    ($rOpts) = @_;
    my ( $tabbing_string, $tab_msg );

    make_static_block_comment_pattern();
    make_static_side_comment_pattern();
    make_closing_side_comment_prefix();
    make_closing_side_comment_list_pattern();
    $format_skipping_pattern_begin =
      make_format_skipping_pattern( 'format-skipping-begin', '#<<<' );
    $format_skipping_pattern_end =
      make_format_skipping_pattern( 'format-skipping-end', '#>>>' );

    # If closing side comments ARE selected, then we can safely
    # delete old closing side comments unless closing side comment
    # warnings are requested.  This is a good idea because it will
    # eliminate any old csc's which fall below the line count threshold.
    # We cannot do this if warnings are turned on, though, because we
    # might delete some text which has been added.  So that must
    # be handled when comments are created.
    if ( $rOpts->{'closing-side-comments'} ) {
        if ( !$rOpts->{'closing-side-comment-warnings'} ) {
            $rOpts->{'delete-closing-side-comments'} = 1;
        }
    }

    # If closing side comments ARE NOT selected, but warnings ARE
    # selected and we ARE DELETING csc's, then we will pretend to be
    # adding with a huge interval.  This will force the comments to be
    # generated for comparison with the old comments, but not added.
    elsif ( $rOpts->{'closing-side-comment-warnings'} ) {
        if ( $rOpts->{'delete-closing-side-comments'} ) {
            $rOpts->{'delete-closing-side-comments'}  = 0;
            $rOpts->{'closing-side-comments'}         = 1;
            $rOpts->{'closing-side-comment-interval'} = 100000000;
        }
    }

    make_bli_pattern();
    make_block_brace_vertical_tightness_pattern();

    if ( $rOpts->{'line-up-parentheses'} ) {

        if (   $rOpts->{'indent-only'}
            || !$rOpts->{'add-newlines'}
            || !$rOpts->{'delete-old-newlines'} )
        {
            warn <<EOM;
-----------------------------------------------------------------------
Conflict: -lp  conflicts with -io, -fnl, -nanl, or -ndnl; ignoring -lp
    
The -lp indentation logic requires that perltidy be able to coordinate
arbitrarily large numbers of line breakpoints.  This isn't possible
with these flags. Sometimes an acceptable workaround is to use -wocb=3
-----------------------------------------------------------------------
EOM
            $rOpts->{'line-up-parentheses'} = 0;
        }
    }

    # At present, tabs are not compatable with the line-up-parentheses style
    # (it would be possible to entab the total leading whitespace
    # just prior to writing the line, if desired).
    if ( $rOpts->{'line-up-parentheses'} && $rOpts->{'tabs'} ) {
        warn <<EOM;
Conflict: -t (tabs) cannot be used with the -lp  option; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    # Likewise, tabs are not compatable with outdenting..
    if ( $rOpts->{'outdent-keywords'} && $rOpts->{'tabs'} ) {
        warn <<EOM;
Conflict: -t (tabs) cannot be used with the -okw options; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    if ( $rOpts->{'outdent-labels'} && $rOpts->{'tabs'} ) {
        warn <<EOM;
Conflict: -t (tabs) cannot be used with the -ola  option; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    if ( !$rOpts->{'space-for-semicolon'} ) {
        $want_left_space{'f'} = -1;
    }

    if ( $rOpts->{'space-terminal-semicolon'} ) {
        $want_left_space{';'} = 1;
    }

    # implement outdenting preferences for keywords
    %outdent_keyword = ();
    unless ( @_ = split_words( $rOpts->{'outdent-keyword-okl'} ) ) {
        @_ = qw(next last redo goto return);    # defaults
    }

    # FUTURE: if not a keyword, assume that it is an identifier
    foreach (@_) {
        if ( $Perl::Tidy::Tokenizer::is_keyword{$_} ) {
            $outdent_keyword{$_} = 1;
        }
        else {
            warn "ignoring '$_' in -okwl list; not a perl keyword";
        }
    }

    # implement user whitespace preferences
    if ( @_ = split_words( $rOpts->{'want-left-space'} ) ) {
        @want_left_space{@_} = (1) x scalar(@_);
    }

    if ( @_ = split_words( $rOpts->{'want-right-space'} ) ) {
        @want_right_space{@_} = (1) x scalar(@_);
    }

    if ( @_ = split_words( $rOpts->{'nowant-left-space'} ) ) {
        @want_left_space{@_} = (-1) x scalar(@_);
    }

    if ( @_ = split_words( $rOpts->{'nowant-right-space'} ) ) {
        @want_right_space{@_} = (-1) x scalar(@_);
    }
    if ( $rOpts->{'dump-want-left-space'} ) {
        dump_want_left_space(*STDOUT);
        exit 1;
    }

    if ( $rOpts->{'dump-want-right-space'} ) {
        dump_want_right_space(*STDOUT);
        exit 1;
    }

    # default keywords for which space is introduced before an opening paren
    # (at present, including them messes up vertical alignment)
    @_ = qw(my local our and or err eq ne if else elsif until
      unless while for foreach return switch case given when);
    @space_after_keyword{@_} = (1) x scalar(@_);

    # allow user to modify these defaults
    if ( @_ = split_words( $rOpts->{'space-after-keyword'} ) ) {
        @space_after_keyword{@_} = (1) x scalar(@_);
    }

    if ( @_ = split_words( $rOpts->{'nospace-after-keyword'} ) ) {
        @space_after_keyword{@_} = (0) x scalar(@_);
    }

    # implement user break preferences
    my @all_operators = qw(% + - * / x != == >= <= =~ !~ < > | &
      = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=
      . : ? && || and or err xor
    );

    my $break_after = sub {
        foreach my $tok (@_) {
            if ( $tok eq '?' ) { $tok = ':' }    # patch to coordinate ?/:
            my $lbs = $left_bond_strength{$tok};
            my $rbs = $right_bond_strength{$tok};
            if ( defined($lbs) && defined($rbs) && $lbs < $rbs ) {
                ( $right_bond_strength{$tok}, $left_bond_strength{$tok} ) =
                  ( $lbs, $rbs );
            }
        }
    };

    my $break_before = sub {
        foreach my $tok (@_) {
            my $lbs = $left_bond_strength{$tok};
            my $rbs = $right_bond_strength{$tok};
            if ( defined($lbs) && defined($rbs) && $rbs < $lbs ) {
                ( $right_bond_strength{$tok}, $left_bond_strength{$tok} ) =
                  ( $lbs, $rbs );
            }
        }
    };

    $break_after->(@all_operators) if ( $rOpts->{'break-after-all-operators'} );
    $break_before->(@all_operators)
      if ( $rOpts->{'break-before-all-operators'} );

    $break_after->( split_words( $rOpts->{'want-break-after'} ) );
    $break_before->( split_words( $rOpts->{'want-break-before'} ) );

    # make note if breaks are before certain key types
    %want_break_before = ();
    foreach my $tok ( @all_operators, ',' ) {
        $want_break_before{$tok} =
          $left_bond_strength{$tok} < $right_bond_strength{$tok};
    }

    # Coordinate ?/: breaks, which must be similar
    if ( !$want_break_before{':'} ) {
        $want_break_before{'?'}   = $want_break_before{':'};
        $right_bond_strength{'?'} = $right_bond_strength{':'} + 0.01;
        $left_bond_strength{'?'}  = NO_BREAK;
    }

    # Define here tokens which may follow the closing brace of a do statement
    # on the same line, as in:
    #   } while ( $something);
    @_ = qw(until while unless if ; : );
    push @_, ',';
    @is_do_follower{@_} = (1) x scalar(@_);

    # These tokens may follow the closing brace of an if or elsif block.
    # In other words, for cuddled else we want code to look like:
    #   } elsif ( $something) {
    #   } else {
    if ( $rOpts->{'cuddled-else'} ) {
        @_ = qw(else elsif);
        @is_if_brace_follower{@_} = (1) x scalar(@_);
    }
    else {
        %is_if_brace_follower = ();
    }

    # nothing can follow the closing curly of an else { } block:
    %is_else_brace_follower = ();

    # what can follow a multi-line anonymous sub definition closing curly:
    @_ = qw# ; : => or and  && || ~~ !~~ ) #;
    push @_, ',';
    @is_anon_sub_brace_follower{@_} = (1) x scalar(@_);

    # what can follow a one-line anonynomous sub closing curly:
    # one-line anonumous subs also have ']' here...
    # see tk3.t and PP.pm
    @_ = qw#  ; : => or and  && || ) ] ~~ !~~ #;
    push @_, ',';
    @is_anon_sub_1_brace_follower{@_} = (1) x scalar(@_);

    # What can follow a closing curly of a block
    # which is not an if/elsif/else/do/sort/map/grep/eval/sub
    # Testfiles: 'Toolbar.pm', 'Menubar.pm', bless.t, '3rules.pl'
    @_ = qw#  ; : => or and  && || ) #;
    push @_, ',';

    # allow cuddled continue if cuddled else is specified
    if ( $rOpts->{'cuddled-else'} ) { push @_, 'continue'; }

    @is_other_brace_follower{@_} = (1) x scalar(@_);

    $right_bond_strength{'{'} = WEAK;
    $left_bond_strength{'{'}  = VERY_STRONG;

    # make -l=0  equal to -l=infinite
    if ( !$rOpts->{'maximum-line-length'} ) {
        $rOpts->{'maximum-line-length'} = 1000000;
    }

    # make -lbl=0  equal to -lbl=infinite
    if ( !$rOpts->{'long-block-line-count'} ) {
        $rOpts->{'long-block-line-count'} = 1000000;
    }

    my $ole = $rOpts->{'output-line-ending'};
    if ($ole) {
        my %endings = (
            dos  => "\015\012",
            win  => "\015\012",
            mac  => "\015",
            unix => "\012",
        );
        $ole = lc $ole;
        unless ( $rOpts->{'output-line-ending'} = $endings{$ole} ) {
            my $str = join " ", keys %endings;
            die <<EOM;
Unrecognized line ending '$ole'; expecting one of: $str
EOM
        }
        if ( $rOpts->{'preserve-line-endings'} ) {
            warn "Ignoring -ple; conflicts with -ole\n";
            $rOpts->{'preserve-line-endings'} = undef;
        }
    }

    # hashes used to simplify setting whitespace
    %tightness = (
        '{' => $rOpts->{'brace-tightness'},
        '}' => $rOpts->{'brace-tightness'},
        '(' => $rOpts->{'paren-tightness'},
        ')' => $rOpts->{'paren-tightness'},
        '[' => $rOpts->{'square-bracket-tightness'},
        ']' => $rOpts->{'square-bracket-tightness'},
    );
    %matching_token = (
        '{' => '}',
        '(' => ')',
        '[' => ']',
        '?' => ':',
    );

    # frequently used parameters
    $rOpts_add_newlines          = $rOpts->{'add-newlines'};
    $rOpts_add_whitespace        = $rOpts->{'add-whitespace'};
    $rOpts_block_brace_tightness = $rOpts->{'block-brace-tightness'};
    $rOpts_block_brace_vertical_tightness =
      $rOpts->{'block-brace-vertical-tightness'};
    $rOpts_brace_left_and_indent   = $rOpts->{'brace-left-and-indent'};
    $rOpts_comma_arrow_breakpoints = $rOpts->{'comma-arrow-breakpoints'};
    $rOpts_break_at_old_ternary_breakpoints =
      $rOpts->{'break-at-old-ternary-breakpoints'};
    $rOpts_break_at_old_comma_breakpoints =
      $rOpts->{'break-at-old-comma-breakpoints'};
    $rOpts_break_at_old_keyword_breakpoints =
      $rOpts->{'break-at-old-keyword-breakpoints'};
    $rOpts_break_at_old_logical_breakpoints =
      $rOpts->{'break-at-old-logical-breakpoints'};
    $rOpts_closing_side_comment_else_flag =
      $rOpts->{'closing-side-comment-else-flag'};
    $rOpts_closing_side_comment_maximum_text =
      $rOpts->{'closing-side-comment-maximum-text'};
    $rOpts_continuation_indentation = $rOpts->{'continuation-indentation'};
    $rOpts_cuddled_else             = $rOpts->{'cuddled-else'};
    $rOpts_delete_old_whitespace    = $rOpts->{'delete-old-whitespace'};
    $rOpts_fuzzy_line_length        = $rOpts->{'fuzzy-line-length'};
    $rOpts_indent_columns           = $rOpts->{'indent-columns'};
    $rOpts_line_up_parentheses      = $rOpts->{'line-up-parentheses'};
    $rOpts_maximum_fields_per_table = $rOpts->{'maximum-fields-per-table'};
    $rOpts_maximum_line_length      = $rOpts->{'maximum-line-length'};
    $rOpts_short_concatenation_item_length =
      $rOpts->{'short-concatenation-item-length'};
    $rOpts_swallow_optional_blank_lines =
      $rOpts->{'swallow-optional-blank-lines'};
    $rOpts_ignore_old_breakpoints   = $rOpts->{'ignore-old-breakpoints'};
    $rOpts_format_skipping          = $rOpts->{'format-skipping'};
    $rOpts_space_function_paren     = $rOpts->{'space-function-paren'};
    $rOpts_space_keyword_paren      = $rOpts->{'space-keyword-paren'};
    $rOpts_keep_interior_semicolons = $rOpts->{'keep-interior-semicolons'};
    $half_maximum_line_length       = $rOpts_maximum_line_length / 2;

    # Note that both opening and closing tokens can access the opening
    # and closing flags of their container types.
    %opening_vertical_tightness = (
        '(' => $rOpts->{'paren-vertical-tightness'},
        '{' => $rOpts->{'brace-vertical-tightness'},
        '[' => $rOpts->{'square-bracket-vertical-tightness'},
        ')' => $rOpts->{'paren-vertical-tightness'},
        '}' => $rOpts->{'brace-vertical-tightness'},
        ']' => $rOpts->{'square-bracket-vertical-tightness'},
    );

    %closing_vertical_tightness = (
        '(' => $rOpts->{'paren-vertical-tightness-closing'},
        '{' => $rOpts->{'brace-vertical-tightness-closing'},
        '[' => $rOpts->{'square-bracket-vertical-tightness-closing'},
        ')' => $rOpts->{'paren-vertical-tightness-closing'},
        '}' => $rOpts->{'brace-vertical-tightness-closing'},
        ']' => $rOpts->{'square-bracket-vertical-tightness-closing'},
    );

    # assume flag for '>' same as ')' for closing qw quotes
    %closing_token_indentation = (
        ')' => $rOpts->{'closing-paren-indentation'},
        '}' => $rOpts->{'closing-brace-indentation'},
        ']' => $rOpts->{'closing-square-bracket-indentation'},
        '>' => $rOpts->{'closing-paren-indentation'},
    );

    %opening_token_right = (
        '(' => $rOpts->{'opening-paren-right'},
        '{' => $rOpts->{'opening-hash-brace-right'},
        '[' => $rOpts->{'opening-square-bracket-right'},
    );

    %stack_opening_token = (
        '(' => $rOpts->{'stack-opening-paren'},
        '{' => $rOpts->{'stack-opening-hash-brace'},
        '[' => $rOpts->{'stack-opening-square-bracket'},
    );

    %stack_closing_token = (
        ')' => $rOpts->{'stack-closing-paren'},
        '}' => $rOpts->{'stack-closing-hash-brace'},
        ']' => $rOpts->{'stack-closing-square-bracket'},
    );
}

sub make_static_block_comment_pattern {

    # create the pattern used to identify static block comments
    $static_block_comment_pattern = '^\s*##';

    # allow the user to change it
    if ( $rOpts->{'static-block-comment-prefix'} ) {
        my $prefix = $rOpts->{'static-block-comment-prefix'};
        $prefix =~ s/^\s*//;
        my $pattern = $prefix;

        # user may give leading caret to force matching left comments only
        if ( $prefix !~ /^\^#/ ) {
            if ( $prefix !~ /^#/ ) {
                die
"ERROR: the -sbcp prefix is '$prefix' but must begin with '#' or '^#'\n";
            }
            $pattern = '^\s*' . $prefix;
        }
        eval "'##'=~/$pattern/";
        if ($@) {
            die
"ERROR: the -sbc prefix '$prefix' causes the invalid regex '$pattern'\n";
        }
        $static_block_comment_pattern = $pattern;
    }
}

sub make_format_skipping_pattern {
    my ( $opt_name, $default ) = @_;
    my $param = $rOpts->{$opt_name};
    unless ($param) { $param = $default }
    $param =~ s/^\s*//;
    if ( $param !~ /^#/ ) {
        die "ERROR: the $opt_name parameter '$param' must begin with '#'\n";
    }
    my $pattern = '^' . $param . '\s';
    eval "'#'=~/$pattern/";
    if ($@) {
        die
"ERROR: the $opt_name parameter '$param' causes the invalid regex '$pattern'\n";
    }
    return $pattern;
}

sub make_closing_side_comment_list_pattern {

    # turn any input list into a regex for recognizing selected block types
    $closing_side_comment_list_pattern = '^\w+';
    if ( defined( $rOpts->{'closing-side-comment-list'} )
        && $rOpts->{'closing-side-comment-list'} )
    {
        $closing_side_comment_list_pattern =
          make_block_pattern( '-cscl', $rOpts->{'closing-side-comment-list'} );
    }
}

sub make_bli_pattern {

    if ( defined( $rOpts->{'brace-left-and-indent-list'} )
        && $rOpts->{'brace-left-and-indent-list'} )
    {
        $bli_list_string = $rOpts->{'brace-left-and-indent-list'};
    }

    $bli_pattern = make_block_pattern( '-blil', $bli_list_string );
}

sub make_block_brace_vertical_tightness_pattern {

    # turn any input list into a regex for recognizing selected block types
    $block_brace_vertical_tightness_pattern =
      '^((if|else|elsif|unless|while|for|foreach|do|\w+:)$|sub)';

    if ( defined( $rOpts->{'block-brace-vertical-tightness-list'} )
        && $rOpts->{'block-brace-vertical-tightness-list'} )
    {
        $block_brace_vertical_tightness_pattern =
          make_block_pattern( '-bbvtl',
            $rOpts->{'block-brace-vertical-tightness-list'} );
    }
}

sub make_block_pattern {

    #  given a string of block-type keywords, return a regex to match them
    #  The only tricky part is that labels are indicated with a single ':'
    #  and the 'sub' token text may have additional text after it (name of
    #  sub).
    #
    #  Example:
    #
    #   input string: "if else elsif unless while for foreach do : sub";
    #   pattern:  '^((if|else|elsif|unless|while|for|foreach|do|\w+:)$|sub)';

    my ( $abbrev, $string ) = @_;
    my @list  = split_words($string);
    my @words = ();
    my %seen;
    for my $i (@list) {
        next if $seen{$i};
        $seen{$i} = 1;
        if ( $i eq 'sub' ) {
        }
        elsif ( $i eq ':' ) {
            push @words, '\w+:';
        }
        elsif ( $i =~ /^\w/ ) {
            push @words, $i;
        }
        else {
            warn "unrecognized block type $i after $abbrev, ignoring\n";
        }
    }
    my $pattern = '(' . join( '|', @words ) . ')$';
    if ( $seen{'sub'} ) {
        $pattern = '(' . $pattern . '|sub)';
    }
    $pattern = '^' . $pattern;
    return $pattern;
}

sub make_static_side_comment_pattern {

    # create the pattern used to identify static side comments
    $static_side_comment_pattern = '^##';

    # allow the user to change it
    if ( $rOpts->{'static-side-comment-prefix'} ) {
        my $prefix = $rOpts->{'static-side-comment-prefix'};
        $prefix =~ s/^\s*//;
        my $pattern = '^' . $prefix;
        eval "'##'=~/$pattern/";
        if ($@) {
            die
"ERROR: the -sscp prefix '$prefix' causes the invalid regex '$pattern'\n";
        }
        $static_side_comment_pattern = $pattern;
    }
}

sub make_closing_side_comment_prefix {

    # Be sure we have a valid closing side comment prefix
    my $csc_prefix = $rOpts->{'closing-side-comment-prefix'};
    my $csc_prefix_pattern;
    if ( !defined($csc_prefix) ) {
        $csc_prefix         = '## end';
        $csc_prefix_pattern = '^##\s+end';
    }
    else {
        my $test_csc_prefix = $csc_prefix;
        if ( $test_csc_prefix !~ /^#/ ) {
            $test_csc_prefix = '#' . $test_csc_prefix;
        }

        # make a regex to recognize the prefix
        my $test_csc_prefix_pattern = $test_csc_prefix;

        # escape any special characters
        $test_csc_prefix_pattern =~ s/([^#\s\w])/\\$1/g;

        $test_csc_prefix_pattern = '^' . $test_csc_prefix_pattern;

        # allow exact number of intermediate spaces to vary
        $test_csc_prefix_pattern =~ s/\s+/\\s\+/g;

        # make sure we have a good pattern
        # if we fail this we probably have an error in escaping
        # characters.
        eval "'##'=~/$test_csc_prefix_pattern/";
        if ($@) {

            # shouldn't happen..must have screwed up escaping, above
            report_definite_bug();
            warn
"Program Error: the -cscp prefix '$csc_prefix' caused the invalid regex '$csc_prefix_pattern'\n";

            # just warn and keep going with defaults
            warn "Please consider using a simpler -cscp prefix\n";
            warn "Using default -cscp instead; please check output\n";
        }
        else {
            $csc_prefix         = $test_csc_prefix;
            $csc_prefix_pattern = $test_csc_prefix_pattern;
        }
    }
    $rOpts->{'closing-side-comment-prefix'} = $csc_prefix;
    $closing_side_comment_prefix_pattern = $csc_prefix_pattern;
}

sub dump_want_left_space {
    my $fh = shift;
    local $" = "\n";
    print $fh <<EOM;
These values are the main control of whitespace to the left of a token type;
They may be altered with the -wls parameter.
For a list of token types, use perltidy --dump-token-types (-dtt)
 1 means the token wants a space to its left
-1 means the token does not want a space to its left
------------------------------------------------------------------------
EOM
    foreach ( sort keys %want_left_space ) {
        print $fh "$_\t$want_left_space{$_}\n";
    }
}

sub dump_want_right_space {
    my $fh = shift;
    local $" = "\n";
    print $fh <<EOM;
These values are the main control of whitespace to the right of a token type;
They may be altered with the -wrs parameter.
For a list of token types, use perltidy --dump-token-types (-dtt)
 1 means the token wants a space to its right
-1 means the token does not want a space to its right
------------------------------------------------------------------------
EOM
    foreach ( sort keys %want_right_space ) {
        print $fh "$_\t$want_right_space{$_}\n";
    }
}

{    # begin is_essential_whitespace

    my %is_sort_grep_map;
    my %is_for_foreach;

    BEGIN {

        @_ = qw(sort grep map);
        @is_sort_grep_map{@_} = (1) x scalar(@_);

        @_ = qw(for foreach);
        @is_for_foreach{@_} = (1) x scalar(@_);

    }

    sub is_essential_whitespace {

        # Essential whitespace means whitespace which cannot be safely deleted
        # without risking the introduction of a syntax error.
        # We are given three tokens and their types:
        # ($tokenl, $typel) is the token to the left of the space in question
        # ($tokenr, $typer) is the token to the right of the space in question
        # ($tokenll, $typell) is previous nonblank token to the left of $tokenl
        #
        # This is a slow routine but is not needed too often except when -mangle
        # is used.
        #
        # Note: This routine should almost never need to be changed.  It is
        # for avoiding syntax problems rather than for formatting.
        my ( $tokenll, $typell, $tokenl, $typel, $tokenr, $typer ) = @_;

        my $result =

          # never combine two bare words or numbers
          # examples:  and ::ok(1)
          #            return ::spw(...)
          #            for bla::bla:: abc
          # example is "%overload:: and" in files Dumpvalue.pm or colonbug.pl
          #            $input eq"quit" to make $inputeq"quit"
          #            my $size=-s::SINK if $file;  <==OK but we won't do it
          # don't join something like: for bla::bla:: abc
          # example is "%overload:: and" in files Dumpvalue.pm or colonbug.pl
          ( ( $tokenl =~ /([\'\w]|\:\:)$/ ) && ( $tokenr =~ /^([\'\w]|\:\:)/ ) )

          # do not combine a number with a concatination dot
          # example: pom.caputo:
          # $vt100_compatible ? "\e[0;0H" : ('-' x 78 . "\n");
          || ( ( $typel eq 'n' ) && ( $tokenr eq '.' ) )
          || ( ( $typer eq 'n' ) && ( $tokenl eq '.' ) )

          # do not join a minus with a bare word, because you might form
          # a file test operator.  Example from Complex.pm:
          # if (CORE::abs($z - i) < $eps); "z-i" would be taken as a file test.
          || ( ( $tokenl eq '-' ) && ( $tokenr =~ /^[_A-Za-z]$/ ) )

          # and something like this could become ambiguous without space
          # after the '-':
          #   use constant III=>1;
          #   $a = $b - III;
          # and even this:
          #   $a = - III;
          || ( ( $tokenl eq '-' )
            && ( $typer =~ /^[wC]$/ && $tokenr =~ /^[_A-Za-z]/ ) )

          # '= -' should not become =- or you will get a warning
          # about reversed -=
          # || ($tokenr eq '-')

          # keep a space between a quote and a bareword to prevent the
          # bareword from becomming a quote modifier.
          || ( ( $typel eq 'Q' ) && ( $tokenr =~ /^[a-zA-Z_]/ ) )

          # keep a space between a token ending in '$' and any word;
          # this caused trouble:  "die @$ if $@"
          || ( ( $typel eq 'i' && $tokenl =~ /\$$/ )
            && ( $tokenr =~ /^[a-zA-Z_]/ ) )

          # perl is very fussy about spaces before <<
          || ( $tokenr =~ /^\<\</ )

          # avoid combining tokens to create new meanings. Example:
          #     $a+ +$b must not become $a++$b
          || ( $is_digraph{ $tokenl . $tokenr } )
          || ( $is_trigraph{ $tokenl . $tokenr } )

          # another example: do not combine these two &'s:
          #     allow_options & &OPT_EXECCGI
          || ( $is_digraph{ $tokenl . substr( $tokenr, 0, 1 ) } )

          # don't combine $$ or $# with any alphanumeric
          # (testfile mangle.t with --mangle)
          || ( ( $tokenl =~ /^\$[\$\#]$/ ) && ( $tokenr =~ /^\w/ ) )

          # retain any space after possible filehandle
          # (testfiles prnterr1.t with --extrude and mangle.t with --mangle)
          || ( $typel eq 'Z' )

          # Perl is sensitive to whitespace after the + here:
          #  $b = xvals $a + 0.1 * yvals $a;
          || ( $typell eq 'Z' && $typel =~ /^[\/\?\+\-\*]$/ )

          # keep paren separate in 'use Foo::Bar ()'
          || ( $tokenr eq '('
            && $typel   eq 'w'
            && $typell  eq 'k'
            && $tokenll eq 'use' )

          # keep any space between filehandle and paren:
          # file mangle.t with --mangle:
          || ( $typel eq 'Y' && $tokenr eq '(' )

          # retain any space after here doc operator ( hereerr.t)
          || ( $typel eq 'h' )

          # be careful with a space around ++ and --, to avoid ambiguity as to
          # which token it applies
          || ( ( $typer =~ /^(pp|mm)$/ )     && ( $tokenl !~ /^[\;\{\(\[]/ ) )
          || ( ( $typel =~ /^(\+\+|\-\-)$/ ) && ( $tokenr !~ /^[\;\}\)\]]/ ) )

          # need space after foreach my; for example, this will fail in
          # older versions of Perl:
          # foreach my$ft(@filetypes)...
          || (
            $tokenl eq 'my'

            #  /^(for|foreach)$/
            && $is_for_foreach{$tokenll} 
            && $tokenr =~ /^\$/
          )

          # must have space between grep and left paren; "grep(" will fail
          || ( $tokenr eq '(' && $is_sort_grep_map{$tokenl} )

          # don't stick numbers next to left parens, as in:
          #use Mail::Internet 1.28 (); (see Entity.pm, Head.pm, Test.pm)
          || ( ( $typel eq 'n' ) && ( $tokenr eq '(' ) )

          # We must be sure that a space between a ? and a quoted string
          # remains if the space before the ? remains.  [Loca.pm, lockarea]
          # ie,
          #    $b=join $comma ? ',' : ':', @_;  # ok
          #    $b=join $comma?',' : ':', @_;    # ok!
          #    $b=join $comma ?',' : ':', @_;   # error!
          # Not really required:
          ## || ( ( $typel eq '?' ) && ( $typer eq 'Q' ) )

          # do not remove space between an '&' and a bare word because
          # it may turn into a function evaluation, like here
          # between '&' and 'O_ACCMODE', producing a syntax error [File.pm]
          #    $opts{rdonly} = (($opts{mode} & O_ACCMODE) == O_RDONLY);
          || ( ( $typel eq '&' ) && ( $tokenr =~ /^[a-zA-Z_]/ ) )

          ;    # the value of this long logic sequence is the result we want
        return $result;
    }
}

sub set_white_space_flag {

    #    This routine examines each pair of nonblank tokens and
    #    sets values for array @white_space_flag.
    #
    #    $white_space_flag[$j] is a flag indicating whether a white space
    #    BEFORE token $j is needed, with the following values:
    #
    #            -1 do not want a space before token $j
    #             0 optional space or $j is a whitespace
    #             1 want a space before token $j
    #
    #
    #   The values for the first token will be defined based
    #   upon the contents of the "to_go" output array.
    #
    #   Note: retain debug print statements because they are usually
    #   required after adding new token types.

    BEGIN {

        # initialize these global hashes, which control the use of
        # whitespace around tokens:
        #
        # %binary_ws_rules
        # %want_left_space
        # %want_right_space
        # %space_after_keyword
        #
        # Many token types are identical to the tokens themselves.
        # See the tokenizer for a complete list. Here are some special types:
        #   k = perl keyword
        #   f = semicolon in for statement
        #   m = unary minus
        #   p = unary plus
        # Note that :: is excluded since it should be contained in an identifier
        # Note that '->' is excluded because it never gets space
        # parentheses and brackets are excluded since they are handled specially
        # curly braces are included but may be overridden by logic, such as
        # newline logic.

        # NEW_TOKENS: create a whitespace rule here.  This can be as
        # simple as adding your new letter to @spaces_both_sides, for
        # example.

        @_ = qw" L { ( [ ";
        @is_opening_type{@_} = (1) x scalar(@_);

        @_ = qw" R } ) ] ";
        @is_closing_type{@_} = (1) x scalar(@_);

        my @spaces_both_sides = qw"
          + - * / % ? = . : x < > | & ^ .. << >> ** && .. || // => += -=
          .= %= x= &= |= ^= *= <> <= >= == =~ !~ /= != ... <<= >>= ~~ !~~
          &&= ||= //= <=> A k f w F n C Y U G v
          ";

        my @spaces_left_side = qw"
          t ! ~ m p { \ h pp mm Z j
          ";
        push( @spaces_left_side, '#' );    # avoids warning message

        my @spaces_right_side = qw"
          ; } ) ] R J ++ -- **=
          ";
        push( @spaces_right_side, ',' );    # avoids warning message
        @want_left_space{@spaces_both_sides} = (1) x scalar(@spaces_both_sides);
        @want_right_space{@spaces_both_sides} =
          (1) x scalar(@spaces_both_sides);
        @want_left_space{@spaces_left_side}  = (1) x scalar(@spaces_left_side);
        @want_right_space{@spaces_left_side} = (-1) x scalar(@spaces_left_side);
        @want_left_space{@spaces_right_side} =
          (-1) x scalar(@spaces_right_side);
        @want_right_space{@spaces_right_side} =
          (1) x scalar(@spaces_right_side);
        $want_left_space{'L'}   = WS_NO;
        $want_left_space{'->'}  = WS_NO;
        $want_right_space{'->'} = WS_NO;
        $want_left_space{'**'}  = WS_NO;
        $want_right_space{'**'} = WS_NO;

        # hash type information must stay tightly bound
        # as in :  ${xxxx}
        $binary_ws_rules{'i'}{'L'} = WS_NO;
        $binary_ws_rules{'i'}{'{'} = WS_YES;
        $binary_ws_rules{'k'}{'{'} = WS_YES;
        $binary_ws_rules{'U'}{'{'} = WS_YES;
        $binary_ws_rules{'i'}{'['} = WS_NO;
        $binary_ws_rules{'R'}{'L'} = WS_NO;
        $binary_ws_rules{'R'}{'{'} = WS_NO;
        $binary_ws_rules{'t'}{'L'} = WS_NO;
        $binary_ws_rules{'t'}{'{'} = WS_NO;
        $binary_ws_rules{'}'}{'L'} = WS_NO;
        $binary_ws_rules{'}'}{'{'} = WS_NO;
        $binary_ws_rules{'$'}{'L'} = WS_NO;
        $binary_ws_rules{'$'}{'{'} = WS_NO;
        $binary_ws_rules{'@'}{'L'} = WS_NO;
        $binary_ws_rules{'@'}{'{'} = WS_NO;
        $binary_ws_rules{'='}{'L'} = WS_YES;

        # the following includes ') {'
        # as in :    if ( xxx ) { yyy }
        $binary_ws_rules{']'}{'L'} = WS_NO;
        $binary_ws_rules{']'}{'{'} = WS_NO;
        $binary_ws_rules{')'}{'{'} = WS_YES;
        $binary_ws_rules{')'}{'['} = WS_NO;
        $binary_ws_rules{']'}{'['} = WS_NO;
        $binary_ws_rules{']'}{'{'} = WS_NO;
        $binary_ws_rules{'}'}{'['} = WS_NO;
        $binary_ws_rules{'R'}{'['} = WS_NO;

        $binary_ws_rules{']'}{'++'} = WS_NO;
        $binary_ws_rules{']'}{'--'} = WS_NO;
        $binary_ws_rules{')'}{'++'} = WS_NO;
        $binary_ws_rules{')'}{'--'} = WS_NO;

        $binary_ws_rules{'R'}{'++'} = WS_NO;
        $binary_ws_rules{'R'}{'--'} = WS_NO;

        ########################################################
        # should no longer be necessary (see niek.pl)
        ##$binary_ws_rules{'k'}{':'} = WS_NO;     # keep colon with label
        ##$binary_ws_rules{'w'}{':'} = WS_NO;
        ########################################################
        $binary_ws_rules{'i'}{'Q'} = WS_YES;
        $binary_ws_rules{'n'}{'('} = WS_YES;    # occurs in 'use package n ()'

        # FIXME: we need to split 'i' into variables and functions
        # and have no space for functions but space for variables.  For now,
        # I have a special patch in the special rules below
        $binary_ws_rules{'i'}{'('} = WS_NO;

        $binary_ws_rules{'w'}{'('} = WS_NO;
        $binary_ws_rules{'w'}{'{'} = WS_YES;
    }
    my ( $jmax, $rtokens, $rtoken_type, $rblock_type ) = @_;
    my ( $last_token, $last_type, $last_block_type, $token, $type,
        $block_type );
    my (@white_space_flag);
    my $j_tight_closing_paren = -1;

    if ( $max_index_to_go >= 0 ) {
        $token      = $tokens_to_go[$max_index_to_go];
        $type       = $types_to_go[$max_index_to_go];
        $block_type = $block_type_to_go[$max_index_to_go];
    }
    else {
        $token      = ' ';
        $type       = 'b';
        $block_type = '';
    }

    # loop over all tokens
    my ( $j, $ws );

    for ( $j = 0 ; $j <= $jmax ; $j++ ) {

        if ( $$rtoken_type[$j] eq 'b' ) {
            $white_space_flag[$j] = WS_OPTIONAL;
            next;
        }

        # set a default value, to be changed as needed
        $ws              = undef;
        $last_token      = $token;
        $last_type       = $type;
        $last_block_type = $block_type;
        $token           = $$rtokens[$j];
        $type            = $$rtoken_type[$j];
        $block_type      = $$rblock_type[$j];

        #---------------------------------------------------------------
        # section 1:
        # handle space on the inside of opening braces
        #---------------------------------------------------------------

        #    /^[L\{\(\[]$/
        if ( $is_opening_type{$last_type} ) {

            $j_tight_closing_paren = -1;

            # let's keep empty matched braces together: () {} []
            # except for BLOCKS
            if ( $token eq $matching_token{$last_token} ) {
                if ($block_type) {
                    $ws = WS_YES;
                }
                else {
                    $ws = WS_NO;
                }
            }
            else {

                # we're considering the right of an opening brace
                # tightness = 0 means always pad inside with space
                # tightness = 1 means pad inside if "complex"
                # tightness = 2 means never pad inside with space

                my $tightness;
                if (   $last_type eq '{'
                    && $last_token eq '{'
                    && $last_block_type )
                {
                    $tightness = $rOpts_block_brace_tightness;
                }
                else { $tightness = $tightness{$last_token} }

                if ( $tightness <= 0 ) {
                    $ws = WS_YES;
                }
                elsif ( $tightness > 1 ) {
                    $ws = WS_NO;
                }
                else {

                    # Patch to count '-foo' as single token so that
                    # each of  $a{-foo} and $a{foo} and $a{'foo'} do
                    # not get spaces with default formatting.
                    my $j_here = $j;
                    ++$j_here
                      if ( $token eq '-'
                        && $last_token eq '{'
                        && $$rtoken_type[ $j + 1 ] eq 'w' );

                    # $j_next is where a closing token should be if
                    # the container has a single token
                    my $j_next =
                      ( $$rtoken_type[ $j_here + 1 ] eq 'b' )
                      ? $j_here + 2
                      : $j_here + 1;
                    my $tok_next  = $$rtokens[$j_next];
                    my $type_next = $$rtoken_type[$j_next];

                    # for tightness = 1, if there is just one token
                    # within the matching pair, we will keep it tight
                    if (
                        $tok_next eq $matching_token{$last_token}

                        # but watch out for this: [ [ ]    (misc.t)
                        && $last_token ne $token
                      )
                    {

                        # remember where to put the space for the closing paren
                        $j_tight_closing_paren = $j_next;
                        $ws                    = WS_NO;
                    }
                    else {
                        $ws = WS_YES;
                    }
                }
            }
        }    # done with opening braces and brackets
        my $ws_1 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 2:
        # handle space on inside of closing brace pairs
        #---------------------------------------------------------------

        #   /[\}\)\]R]/
        if ( $is_closing_type{$type} ) {

            if ( $j == $j_tight_closing_paren ) {

                $j_tight_closing_paren = -1;
                $ws                    = WS_NO;
            }
            else {

                if ( !defined($ws) ) {

                    my $tightness;
                    if ( $type eq '}' && $token eq '}' && $block_type ) {
                        $tightness = $rOpts_block_brace_tightness;
                    }
                    else { $tightness = $tightness{$token} }

                    $ws = ( $tightness > 1 ) ? WS_NO : WS_YES;
                }
            }
        }

        my $ws_2 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 3:
        # use the binary table
        #---------------------------------------------------------------
        if ( !defined($ws) ) {
            $ws = $binary_ws_rules{$last_type}{$type};
        }
        my $ws_3 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 4:
        # some special cases
        #---------------------------------------------------------------
        if ( $token eq '(' ) {

            # This will have to be tweaked as tokenization changes.
            # We usually want a space at '} (', for example:
            #     map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );
            #
            # But not others:
            #     &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );
            # At present, the above & block is marked as type L/R so this case
            # won't go through here.
            if ( $last_type eq '}' ) { $ws = WS_YES }

            # NOTE: some older versions of Perl had occasional problems if
            # spaces are introduced between keywords or functions and opening
            # parens.  So the default is not to do this except is certain
            # cases.  The current Perl seems to tolerate spaces.

            # Space between keyword and '('
            elsif ( $last_type eq 'k' ) {
                $ws = WS_NO
                  unless ( $rOpts_space_keyword_paren
                    || $space_after_keyword{$last_token} );
            }

            # Space between function and '('
            # -----------------------------------------------------
            # 'w' and 'i' checks for something like:
            #   myfun(    &myfun(   ->myfun(
            # -----------------------------------------------------
            elsif (( $last_type =~ /^[wU]$/ )
                || ( $last_type =~ /^[wi]$/ && $last_token =~ /^(\&|->)/ ) )
            {
                $ws = WS_NO unless ($rOpts_space_function_paren);
            }

            # space between something like $i and ( in
            # for $i ( 0 .. 20 ) {
            # FIXME: eventually, type 'i' needs to be split into multiple
            # token types so this can be a hardwired rule.
            elsif ( $last_type eq 'i' && $last_token =~ /^[\$\%\@]/ ) {
                $ws = WS_YES;
            }

            # allow constant function followed by '()' to retain no space
            elsif ( $last_type eq 'C' && $$rtokens[ $j + 1 ] eq ')' ) {
                $ws = WS_NO;
            }
        }

        # patch for SWITCH/CASE: make space at ']{' optional
        # since the '{' might begin a case or when block
        elsif ( ( $token eq '{' && $type ne 'L' ) && $last_token eq ']' ) {
            $ws = WS_OPTIONAL;
        }

        # keep space between 'sub' and '{' for anonymous sub definition
        if ( $type eq '{' ) {
            if ( $last_token eq 'sub' ) {
                $ws = WS_YES;
            }

            # this is needed to avoid no space in '){'
            if ( $last_token eq ')' && $token eq '{' ) { $ws = WS_YES }

            # avoid any space before the brace or bracket in something like
            #  @opts{'a','b',...}
            if ( $last_type eq 'i' && $last_token =~ /^\@/ ) {
                $ws = WS_NO;
            }
        }

        elsif ( $type eq 'i' ) {

            # never a space before ->
            if ( $token =~ /^\-\>/ ) {
                $ws = WS_NO;
            }
        }

        # retain any space between '-' and bare word
        elsif ( $type eq 'w' || $type eq 'C' ) {
            $ws = WS_OPTIONAL if $last_type eq '-';

            # never a space before ->
            if ( $token =~ /^\-\>/ ) {
                $ws = WS_NO;
            }
        }

        # retain any space between '-' and bare word
        # example: avoid space between 'USER' and '-' here:
        #   $myhash{USER-NAME}='steve';
        elsif ( $type eq 'm' || $type eq '-' ) {
            $ws = WS_OPTIONAL if ( $last_type eq 'w' );
        }

        # always space before side comment
        elsif ( $type eq '#' ) { $ws = WS_YES if $j > 0 }

        # always preserver whatever space was used after a possible
        # filehandle (except _) or here doc operator
        if (
            $type ne '#'
            && ( ( $last_type eq 'Z' && $last_token ne '_' )
                || $last_type eq 'h' )
          )
        {
            $ws = WS_OPTIONAL;
        }

        my $ws_4 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 5:
        # default rules not covered above
        #---------------------------------------------------------------
        # if we fall through to here,
        # look at the pre-defined hash tables for the two tokens, and
        # if (they are equal) use the common value
        # if (either is zero or undef) use the other
        # if (either is -1) use it
        # That is,
        # left  vs right
        #  1    vs    1     -->  1
        #  0    vs    0     -->  0
        # -1    vs   -1     --> -1
        #
        #  0    vs   -1     --> -1
        #  0    vs    1     -->  1
        #  1    vs    0     -->  1
        # -1    vs    0     --> -1
        #
        # -1    vs    1     --> -1
        #  1    vs   -1     --> -1
        if ( !defined($ws) ) {
            my $wl = $want_left_space{$type};
            my $wr = $want_right_space{$last_type};
            if ( !defined($wl) ) { $wl = 0 }
            if ( !defined($wr) ) { $wr = 0 }
            $ws = ( ( $wl == $wr ) || ( $wl == -1 ) || !$wr ) ? $wl : $wr;
        }

        if ( !defined($ws) ) {
            $ws = 0;
            write_diagnostics(
                "WS flag is undefined for tokens $last_token $token\n");
        }

        # Treat newline as a whitespace. Otherwise, we might combine
        # 'Send' and '-recipients' here according to the above rules:
        #    my $msg = new Fax::Send
        #      -recipients => $to,
        #      -data => $data;
        if ( $ws == 0 && $j == 0 ) { $ws = 1 }

        if (   ( $ws == 0 )
            && $j > 0
            && $j < $jmax
            && ( $last_type !~ /^[Zh]$/ ) )
        {

            # If this happens, we have a non-fatal but undesirable
            # hole in the above rules which should be patched.
            write_diagnostics(
                "WS flag is zero for tokens $last_token $token\n");
        }
        $white_space_flag[$j] = $ws;

        FORMATTER_DEBUG_FLAG_WHITE && do {
            my $str = substr( $last_token, 0, 15 );
            $str .= ' ' x ( 16 - length($str) );
            if ( !defined($ws_1) ) { $ws_1 = "*" }
            if ( !defined($ws_2) ) { $ws_2 = "*" }
            if ( !defined($ws_3) ) { $ws_3 = "*" }
            if ( !defined($ws_4) ) { $ws_4 = "*" }
            print
"WHITE:  i=$j $str $last_type $type $ws_1 : $ws_2 : $ws_3 : $ws_4 : $ws \n";
        };
    }
    return \@white_space_flag;
}

{    # begin print_line_of_tokens

    my $rtoken_type;
    my $rtokens;
    my $rlevels;
    my $rslevels;
    my $rblock_type;
    my $rcontainer_type;
    my $rcontainer_environment;
    my $rtype_sequence;
    my $input_line;
    my $rnesting_tokens;
    my $rci_levels;
    my $rnesting_blocks;

    my $in_quote;
    my $python_indentation_level;

    # These local token variables are stored by store_token_to_go:
    my $block_type;
    my $ci_level;
    my $container_environment;
    my $container_type;
    my $in_continued_quote;
    my $level;
    my $nesting_blocks;
    my $no_internal_newlines;
    my $slevel;
    my $token;
    my $type;
    my $type_sequence;

    # routine to pull the jth token from the line of tokens
    sub extract_token {
        my $j = shift;
        $token                 = $$rtokens[$j];
        $type                  = $$rtoken_type[$j];
        $block_type            = $$rblock_type[$j];
        $container_type        = $$rcontainer_type[$j];
        $container_environment = $$rcontainer_environment[$j];
        $type_sequence         = $$rtype_sequence[$j];
        $level                 = $$rlevels[$j];
        $slevel                = $$rslevels[$j];
        $nesting_blocks        = $$rnesting_blocks[$j];
        $ci_level              = $$rci_levels[$j];
    }

    {
        my @saved_token;

        sub save_current_token {

            @saved_token = (
                $block_type,            $ci_level,
                $container_environment, $container_type,
                $in_continued_quote,    $level,
                $nesting_blocks,        $no_internal_newlines,
                $slevel,                $token,
                $type,                  $type_sequence,
            );
        }

        sub restore_current_token {
            (
                $block_type,            $ci_level,
                $container_environment, $container_type,
                $in_continued_quote,    $level,
                $nesting_blocks,        $no_internal_newlines,
                $slevel,                $token,
                $type,                  $type_sequence,
            ) = @saved_token;
        }
    }

    # Routine to place the current token into the output stream.
    # Called once per output token.
    sub store_token_to_go {

        my $flag = $no_internal_newlines;
        if ( $_[0] ) { $flag = 1 }

        $tokens_to_go[ ++$max_index_to_go ]            = $token;
        $types_to_go[$max_index_to_go]                 = $type;
        $nobreak_to_go[$max_index_to_go]               = $flag;
        $old_breakpoint_to_go[$max_index_to_go]        = 0;
        $forced_breakpoint_to_go[$max_index_to_go]     = 0;
        $block_type_to_go[$max_index_to_go]            = $block_type;
        $type_sequence_to_go[$max_index_to_go]         = $type_sequence;
        $container_environment_to_go[$max_index_to_go] = $container_environment;
        $nesting_blocks_to_go[$max_index_to_go]        = $nesting_blocks;
        $ci_levels_to_go[$max_index_to_go]             = $ci_level;
        $mate_index_to_go[$max_index_to_go]            = -1;
        $matching_token_to_go[$max_index_to_go]        = '';
        $bond_strength_to_go[$max_index_to_go]         = 0;

        # Note: negative levels are currently retained as a diagnostic so that
        # the 'final indentation level' is correctly reported for bad scripts.
        # But this means that every use of $level as an index must be checked.
        # If this becomes too much of a problem, we might give up and just clip
        # them at zero.
        ## $levels_to_go[$max_index_to_go] = ( $level > 0 ) ? $level : 0;
        $levels_to_go[$max_index_to_go] = $level;
        $nesting_depth_to_go[$max_index_to_go] = ( $slevel >= 0 ) ? $slevel : 0;
        $lengths_to_go[ $max_index_to_go + 1 ] =
          $lengths_to_go[$max_index_to_go] + length($token);

        # Define the indentation that this token would have if it started
        # a new line.  We have to do this now because we need to know this
        # when considering one-line blocks.
        set_leading_whitespace( $level, $ci_level, $in_continued_quote );

        if ( $type ne 'b' ) {
            $last_last_nonblank_index_to_go = $last_nonblank_index_to_go;
            $last_last_nonblank_type_to_go  = $last_nonblank_type_to_go;
            $last_last_nonblank_token_to_go = $last_nonblank_token_to_go;
            $last_nonblank_index_to_go      = $max_index_to_go;
            $last_nonblank_type_to_go       = $type;
            $last_nonblank_token_to_go      = $token;
            if ( $type eq ',' ) {
                $comma_count_in_batch++;
            }
        }

        FORMATTER_DEBUG_FLAG_STORE && do {
            my ( $a, $b, $c ) = caller();
            print
"STORE: from $a $c: storing token $token type $type lev=$level slev=$slevel at $max_index_to_go\n";
        };
    }

    sub insert_new_token_to_go {

        # insert a new token into the output stream.  use same level as
        # previous token; assumes a character at max_index_to_go.
        save_current_token();
        ( $token, $type, $slevel, $no_internal_newlines ) = @_;

        if ( $max_index_to_go == UNDEFINED_INDEX ) {
            warning("code bug: bad call to insert_new_token_to_go\n");
        }
        $level = $levels_to_go[$max_index_to_go];

        # FIXME: it seems to be necessary to use the next, rather than
        # previous, value of this variable when creating a new blank (align.t)
        #my $slevel         = $nesting_depth_to_go[$max_index_to_go];
        $nesting_blocks        = $nesting_blocks_to_go[$max_index_to_go];
        $ci_level              = $ci_levels_to_go[$max_index_to_go];
        $container_environment = $container_environment_to_go[$max_index_to_go];
        $in_continued_quote    = 0;
        $block_type            = "";
        $type_sequence         = "";
        store_token_to_go();
        restore_current_token();
        return;
    }

    sub print_line_of_tokens {

        my $line_of_tokens = shift;

        # This routine is called once per input line to process all of
        # the tokens on that line.  This is the first stage of
        # beautification.
        #
        # Full-line comments and blank lines may be processed immediately.
        #
        # For normal lines of code, the tokens are stored one-by-one,
        # via calls to 'sub store_token_to_go', until a known line break
        # point is reached.  Then, the batch of collected tokens is
        # passed along to 'sub output_line_to_go' for further
        # processing.  This routine decides if there should be
        # whitespace between each pair of non-white tokens, so later
        # routines only need to decide on any additional line breaks.
        # Any whitespace is initally a single space character.  Later,
        # the vertical aligner may expand that to be multiple space
        # characters if necessary for alignment.

        # extract input line number for error messages
        $input_line_number = $line_of_tokens->{_line_number};

        $rtoken_type            = $line_of_tokens->{_rtoken_type};
        $rtokens                = $line_of_tokens->{_rtokens};
        $rlevels                = $line_of_tokens->{_rlevels};
        $rslevels               = $line_of_tokens->{_rslevels};
        $rblock_type            = $line_of_tokens->{_rblock_type};
        $rcontainer_type        = $line_of_tokens->{_rcontainer_type};
        $rcontainer_environment = $line_of_tokens->{_rcontainer_environment};
        $rtype_sequence         = $line_of_tokens->{_rtype_sequence};
        $input_line             = $line_of_tokens->{_line_text};
        $rnesting_tokens        = $line_of_tokens->{_rnesting_tokens};
        $rci_levels             = $line_of_tokens->{_rci_levels};
        $rnesting_blocks        = $line_of_tokens->{_rnesting_blocks};

        $in_continued_quote = $starting_in_quote =
          $line_of_tokens->{_starting_in_quote};
        $in_quote        = $line_of_tokens->{_ending_in_quote};
        $ending_in_quote = $in_quote;
        $python_indentation_level =
          $line_of_tokens->{_python_indentation_level};

        my $j;
        my $j_next;
        my $jmax;
        my $next_nonblank_token;
        my $next_nonblank_token_type;
        my $rwhite_space_flag;

        $jmax                    = @$rtokens - 1;
        $block_type              = "";
        $container_type          = "";
        $container_environment   = "";
        $type_sequence           = "";
        $no_internal_newlines    = 1 - $rOpts_add_newlines;
        $is_static_block_comment = 0;

        # Handle a continued quote..
        if ($in_continued_quote) {

            # A line which is entirely a quote or pattern must go out
            # verbatim.  Note: the \n is contained in $input_line.
            if ( $jmax <= 0 ) {
                if ( ( $input_line =~ "\t" ) ) {
                    note_embedded_tab();
                }
                write_unindented_line("$input_line");
                $last_line_had_side_comment = 0;
                return;
            }

            # prior to version 20010406, perltidy had a bug which placed
            # continuation indentation before the last line of some multiline
            # quotes and patterns -- exactly the lines passing this way.
            # To help find affected lines in scripts run with these
            # versions, run with '-chk', and it will warn of any quotes or
            # patterns which might have been modified by these early
            # versions.
            if ( $rOpts->{'check-multiline-quotes'} && $input_line =~ /^ / ) {
                warning(
"-chk: please check this line for extra leading whitespace\n"
                );
            }
        }

        # Write line verbatim if we are in a formatting skip section
        if ($in_format_skipping_section) {
            write_unindented_line("$input_line");
            $last_line_had_side_comment = 0;

            # Note: extra space appended to comment simplifies pattern matching
            if (   $jmax == 0
                && $$rtoken_type[0] eq '#'
                && ( $$rtokens[0] . " " ) =~ /$format_skipping_pattern_end/o )
            {
                $in_format_skipping_section = 0;
                write_logfile_entry("Exiting formatting skip section\n");
            }
            return;
        }

        # See if we are entering a formatting skip section
        if (   $rOpts_format_skipping
            && $jmax == 0
            && $$rtoken_type[0] eq '#'
            && ( $$rtokens[0] . " " ) =~ /$format_skipping_pattern_begin/o )
        {
            flush();
            $in_format_skipping_section = 1;
            write_logfile_entry("Entering formatting skip section\n");
            write_unindented_line("$input_line");
            $last_line_had_side_comment = 0;
            return;
        }

        # delete trailing blank tokens
        if ( $jmax > 0 && $$rtoken_type[$jmax] eq 'b' ) { $jmax-- }

        # Handle a blank line..
        if ( $jmax < 0 ) {

            # For the 'swallow-optional-blank-lines' option, we delete all
            # old blank lines and let the blank line rules generate any
            # needed blanks.
            if ( !$rOpts_swallow_optional_blank_lines ) {
                flush();
                $file_writer_object->write_blank_code_line();
                $last_line_leading_type = 'b';
            }
            $last_line_had_side_comment = 0;
            return;
        }

        # see if this is a static block comment (starts with ## by default)
        my $is_static_block_comment_without_leading_space = 0;
        if (   $jmax == 0
            && $$rtoken_type[0] eq '#'
            && $rOpts->{'static-block-comments'}
            && $input_line =~ /$static_block_comment_pattern/o )
        {
            $is_static_block_comment = 1;
            $is_static_block_comment_without_leading_space =
              substr( $input_line, 0, 1 ) eq '#';
        }

        # Check for comments which are line directives
        # Treat exactly as static block comments without leading space
        # reference: perlsyn, near end, section Plain Old Comments (Not!)
        # example: '# line 42 "new_filename.plx"'
        if (
               $jmax == 0
            && $$rtoken_type[0] eq '#'
            && $input_line =~ /^\#   \s*
                               line \s+ (\d+)   \s*
                               (?:\s("?)([^"]+)\2)? \s*
                               $/x
          )
        {
            $is_static_block_comment                       = 1;
            $is_static_block_comment_without_leading_space = 1;
        }

        # create a hanging side comment if appropriate
        if (
               $jmax == 0
            && $$rtoken_type[0] eq '#'    # only token is a comment
            && $last_line_had_side_comment    # last line had side comment
            && $input_line =~ /^\s/           # there is some leading space
            && !$is_static_block_comment    # do not make static comment hanging
            && $rOpts->{'hanging-side-comments'}    # user is allowing this
          )
        {

            # We will insert an empty qw string at the start of the token list
            # to force this comment to be a side comment. The vertical aligner
            # should then line it up with the previous side comment.
            unshift @$rtoken_type,            'q';
            unshift @$rtokens,                '';
            unshift @$rlevels,                $$rlevels[0];
            unshift @$rslevels,               $$rslevels[0];
            unshift @$rblock_type,            '';
            unshift @$rcontainer_type,        '';
            unshift @$rcontainer_environment, '';
            unshift @$rtype_sequence,         '';
            unshift @$rnesting_tokens,        $$rnesting_tokens[0];
            unshift @$rci_levels,             $$rci_levels[0];
            unshift @$rnesting_blocks,        $$rnesting_blocks[0];
            $jmax = 1;
        }

        # remember if this line has a side comment
        $last_line_had_side_comment =
          ( $jmax > 0 && $$rtoken_type[$jmax] eq '#' );

        # Handle a block (full-line) comment..
        if ( ( $jmax == 0 ) && ( $$rtoken_type[0] eq '#' ) ) {

            if ( $rOpts->{'delete-block-comments'} ) { return }

            if ( $rOpts->{'tee-block-comments'} ) {
                $file_writer_object->tee_on();
            }

            destroy_one_line_block();
            output_line_to_go();

            # output a blank line before block comments
            if (
                   $last_line_leading_type !~ /^[#b]$/
                && $rOpts->{'blanks-before-comments'}    # only if allowed
                && !
                $is_static_block_comment    # never before static block comments
              )
            {
                flush();                    # switching to new output stream
                $file_writer_object->write_blank_code_line();
                $last_line_leading_type = 'b';
            }

            # TRIM COMMENTS -- This could be turned off as a option
            $$rtokens[0] =~ s/\s*$//;       # trim right end

            if (
                $rOpts->{'indent-block-comments'}
                && (  !$rOpts->{'indent-spaced-block-comments'}
                    || $input_line =~ /^\s+/ )
                && !$is_static_block_comment_without_leading_space
              )
            {
                extract_token(0);
                store_token_to_go();
                output_line_to_go();
            }
            else {
                flush();    # switching to new output stream
                $file_writer_object->write_code_line( $$rtokens[0] . "\n" );
                $last_line_leading_type = '#';
            }
            if ( $rOpts->{'tee-block-comments'} ) {
                $file_writer_object->tee_off();
            }
            return;
        }

        # compare input/output indentation except for continuation lines
        # (because they have an unknown amount of initial blank space)
        # and lines which are quotes (because they may have been outdented)
        # Note: this test is placed here because we know the continuation flag
        # at this point, which allows us to avoid non-meaningful checks.
        my $structural_indentation_level = $$rlevels[0];
        compare_indentation_levels( $python_indentation_level,
            $structural_indentation_level )
          unless ( $python_indentation_level < 0
            || ( $$rci_levels[0] > 0 )
            || ( ( $python_indentation_level == 0 ) && $$rtoken_type[0] eq 'Q' )
          );

        #   Patch needed for MakeMaker.  Do not break a statement
        #   in which $VERSION may be calculated.  See MakeMaker.pm;
        #   this is based on the coding in it.
        #   The first line of a file that matches this will be eval'd:
        #       /([\$*])(([\w\:\']*)\bVERSION)\b.*\=/
        #   Examples:
        #     *VERSION = \'1.01';
        #     ( $VERSION ) = '$Revision: 1.73 $ ' =~ /\$Revision:\s+([^\s]+)/;
        #   We will pass such a line straight through without breaking
        #   it unless -npvl is used

        my $is_VERSION_statement = 0;

        if (
              !$saw_VERSION_in_this_file
            && $input_line =~ /VERSION/    # quick check to reject most lines
            && $input_line =~ /([\$*])(([\w\:\']*)\bVERSION)\b.*\=/
          )
        {
            $saw_VERSION_in_this_file = 1;
            $is_VERSION_statement     = 1;
            write_logfile_entry("passing VERSION line; -npvl deactivates\n");
            $no_internal_newlines = 1;
        }

        # take care of indentation-only
        # NOTE: In previous versions we sent all qw lines out immediately here.
        # No longer doing this: also write a line which is entirely a 'qw' list
        # to allow stacking of opening and closing tokens.  Note that interior
        # qw lines will still go out at the end of this routine.
        if ( $rOpts->{'indent-only'} ) {
            flush();
            trim($input_line);

            extract_token(0);
            $token                 = $input_line;
            $type                  = 'q';
            $block_type            = "";
            $container_type        = "";
            $container_environment = "";
            $type_sequence         = "";
            store_token_to_go();
            output_line_to_go();
            return;
        }

        push( @$rtokens,     ' ', ' ' );   # making $j+2 valid simplifies coding
        push( @$rtoken_type, 'b', 'b' );
        ($rwhite_space_flag) =
          set_white_space_flag( $jmax, $rtokens, $rtoken_type, $rblock_type );

        # find input tabbing to allow checks for tabbing disagreement
        ## not used for now
        ##$input_line_tabbing = "";
        ##if ( $input_line =~ /^(\s*)/ ) { $input_line_tabbing = $1; }

        # if the buffer hasn't been flushed, add a leading space if
        # necessary to keep essential whitespace. This is really only
        # necessary if we are squeezing out all ws.
        if ( $max_index_to_go >= 0 ) {

            $old_line_count_in_batch++;

            if (
                is_essential_whitespace(
                    $last_last_nonblank_token,
                    $last_last_nonblank_type,
                    $tokens_to_go[$max_index_to_go],
                    $types_to_go[$max_index_to_go],
                    $$rtokens[0],
                    $$rtoken_type[0]
                )
              )
            {
                my $slevel = $$rslevels[0];
                insert_new_token_to_go( ' ', 'b', $slevel,
                    $no_internal_newlines );
            }
        }

        # If we just saw the end of an elsif block, write nag message
        # if we do not see another elseif or an else.
        if ($looking_for_else) {

            unless ( $$rtokens[0] =~ /^(elsif|else)$/ ) {
                write_logfile_entry("(No else block)\n");
            }
            $looking_for_else = 0;
        }

        # This is a good place to kill incomplete one-line blocks
        if (   ( $semicolons_before_block_self_destruct == 0 )
            && ( $max_index_to_go >= 0 )
            && ( $types_to_go[$max_index_to_go] eq ';' )
            && ( $$rtokens[0] ne '}' ) )
        {
            destroy_one_line_block();
            output_line_to_go();
        }

        # loop to process the tokens one-by-one
        $type  = 'b';
        $token = "";

        foreach $j ( 0 .. $jmax ) {

            # pull out the local values for this token
            extract_token($j);

            if ( $type eq '#' ) {

                # trim trailing whitespace
                # (there is no option at present to prevent this)
                $token =~ s/\s*$//;

                if (
                    $rOpts->{'delete-side-comments'}

                    # delete closing side comments if necessary
                    || (   $rOpts->{'delete-closing-side-comments'}
                        && $token =~ /$closing_side_comment_prefix_pattern/o
                        && $last_nonblank_block_type =~
                        /$closing_side_comment_list_pattern/o )
                  )
                {
                    if ( $types_to_go[$max_index_to_go] eq 'b' ) {
                        unstore_token_to_go();
                    }
                    last;
                }
            }

            # If we are continuing after seeing a right curly brace, flush
            # buffer unless we see what we are looking for, as in
            #   } else ...
            if ( $rbrace_follower && $type ne 'b' ) {

                unless ( $rbrace_follower->{$token} ) {
                    output_line_to_go();
                }
                $rbrace_follower = undef;
            }

            $j_next = ( $$rtoken_type[ $j + 1 ] eq 'b' ) ? $j + 2 : $j + 1;
            $next_nonblank_token      = $$rtokens[$j_next];
            $next_nonblank_token_type = $$rtoken_type[$j_next];

            #--------------------------------------------------------
            # Start of section to patch token text
            #--------------------------------------------------------

            # Modify certain tokens here for whitespace
            # The following is not yet done, but could be:
            #   sub (x x x)
            if ( $type =~ /^[wit]$/ ) {

                # Examples:
                # change '$  var'  to '$var' etc
                #        '-> new'  to '->new'
                if ( $token =~ /^([\$\&\%\*\@]|\-\>)\s/ ) {
                    $token =~ s/\s*//g;
                }

                if ( $token =~ /^sub/ ) { $token =~ s/\s+/ /g }
            }

            # change 'LABEL   :'   to 'LABEL:'
            elsif ( $type eq 'J' ) { $token =~ s/\s+//g }

            # patch to add space to something like "x10"
            # This avoids having to split this token in the pre-tokenizer
            elsif ( $type eq 'n' ) {
                if ( $token =~ /^x\d+/ ) { $token =~ s/x/x / }
            }

            elsif ( $type eq 'Q' ) {
                note_embedded_tab() if ( $token =~ "\t" );

                # make note of something like '$var = s/xxx/yyy/;'
                # in case it should have been '$var =~ s/xxx/yyy/;'
                if (
                       $token =~ /^(s|tr|y|m|\/)/
                    && $last_nonblank_token =~ /^(=|==|!=)$/

                    # precededed by simple scalar
                    && $last_last_nonblank_type eq 'i'
                    && $last_last_nonblank_token =~ /^\$/

                    # followed by some kind of termination
                    # (but give complaint if we can's see far enough ahead)
                    && $next_nonblank_token =~ /^[; \)\}]$/

                    # scalar is not decleared
                    && !(
                           $types_to_go[0] eq 'k'
                        && $tokens_to_go[0] =~ /^(my|our|local)$/
                    )
                  )
                {
                    my $guess = substr( $last_nonblank_token, 0, 1 ) . '~';
                    complain(
"Note: be sure you want '$last_nonblank_token' instead of '$guess' here\n"
                    );
                }
            }

           # trim blanks from right of qw quotes
           # (To avoid trimming qw quotes use -ntqw; the tokenizer handles this)
            elsif ( $type eq 'q' ) {
                $token =~ s/\s*$//;
                note_embedded_tab() if ( $token =~ "\t" );
            }

            #--------------------------------------------------------
            # End of section to patch token text
            #--------------------------------------------------------

            # insert any needed whitespace
            if (   ( $type ne 'b' )
                && ( $max_index_to_go >= 0 )
                && ( $types_to_go[$max_index_to_go] ne 'b' )
                && $rOpts_add_whitespace )
            {
                my $ws = $$rwhite_space_flag[$j];

                if ( $ws == 1 ) {
                    insert_new_token_to_go( ' ', 'b', $slevel,
                        $no_internal_newlines );
                }
            }

            # Do not allow breaks which would promote a side comment to a
            # block comment.  In order to allow a break before an opening
            # or closing BLOCK, followed by a side comment, those sections
            # of code will handle this flag separately.
            my $side_comment_follows = ( $next_nonblank_token_type eq '#' );
            my $is_opening_BLOCK =
              (      $type eq '{'
                  && $token eq '{'
                  && $block_type
                  && $block_type ne 't' );
            my $is_closing_BLOCK =
              (      $type eq '}'
                  && $token eq '}'
                  && $block_type
                  && $block_type ne 't' );

            if (   $side_comment_follows
                && !$is_opening_BLOCK
                && !$is_closing_BLOCK )
            {
                $no_internal_newlines = 1;
            }

            # We're only going to handle breaking for code BLOCKS at this
            # (top) level.  Other indentation breaks will be handled by
            # sub scan_list, which is better suited to dealing with them.
            if ($is_opening_BLOCK) {

                # Tentatively output this token.  This is required before
                # calling starting_one_line_block.  We may have to unstore
                # it, though, if we have to break before it.
                store_token_to_go($side_comment_follows);

                # Look ahead to see if we might form a one-line block
                my $too_long =
                  starting_one_line_block( $j, $jmax, $level, $slevel,
                    $ci_level, $rtokens, $rtoken_type, $rblock_type );
                clear_breakpoint_undo_stack();

                # to simplify the logic below, set a flag to indicate if
                # this opening brace is far from the keyword which introduces it
                my $keyword_on_same_line = 1;
                if (   ( $max_index_to_go >= 0 )
                    && ( $last_nonblank_type eq ')' ) )
                {
                    if (   $block_type =~ /^(if|else|elsif)$/
                        && ( $tokens_to_go[0] eq '}' )
                        && $rOpts_cuddled_else )
                    {
                        $keyword_on_same_line = 1;
                    }
                    elsif ( ( $slevel < $nesting_depth_to_go[0] ) || $too_long )
                    {
                        $keyword_on_same_line = 0;
                    }
                }

                # decide if user requested break before '{'
                my $want_break =

                  # use -bl flag if not a sub block of any type
                  $block_type !~ /^sub/
                  ? $rOpts->{'opening-brace-on-new-line'}

                  # use -sbl flag unless this is an anonymous sub block
                  : $block_type !~ /^sub\W*$/
                  ? $rOpts->{'opening-sub-brace-on-new-line'}

                  # do not break for anonymous subs
                  : 0;

                # Break before an opening '{' ...
                if (

                    # if requested
                    $want_break

                    # and we were unable to start looking for a block,
                    && $index_start_one_line_block == UNDEFINED_INDEX

                    # or if it will not be on same line as its keyword, so that
                    # it will be outdented (eval.t, overload.t), and the user
                    # has not insisted on keeping it on the right
                    || (   !$keyword_on_same_line
                        && !$rOpts->{'opening-brace-always-on-right'} )

                  )
                {

                    # but only if allowed
                    unless ($no_internal_newlines) {

                        # since we already stored this token, we must unstore it
                        unstore_token_to_go();

                        # then output the line
                        output_line_to_go();

                        # and now store this token at the start of a new line
                        store_token_to_go($side_comment_follows);
                    }
                }

                # Now update for side comment
                if ($side_comment_follows) { $no_internal_newlines = 1 }

                # now output this line
                unless ($no_internal_newlines) {
                    output_line_to_go();
                }
            }

            elsif ($is_closing_BLOCK) {

                # If there is a pending one-line block ..
                if ( $index_start_one_line_block != UNDEFINED_INDEX ) {

                    # we have to terminate it if..
                    if (

                    # it is too long (final length may be different from
                    # initial estimate). note: must allow 1 space for this token
                        excess_line_length( $index_start_one_line_block,
                            $max_index_to_go ) >= 0

                        # or if it has too many semicolons
                        || (   $semicolons_before_block_self_destruct == 0
                            && $last_nonblank_type ne ';' )
                      )
                    {
                        destroy_one_line_block();
                    }
                }

                # put a break before this closing curly brace if appropriate
                unless ( $no_internal_newlines
                    || $index_start_one_line_block != UNDEFINED_INDEX )
                {

                    # add missing semicolon if ...
                    # there are some tokens
                    if (
                        ( $max_index_to_go > 0 )

                        # and we don't have one
                        && ( $last_nonblank_type ne ';' )

                        # patch until some block type issues are fixed:
                        # Do not add semi-colon for block types '{',
                        # '}', and ';' because we cannot be sure yet
                        # that this is a block and not an anonomyous
                        # hash (blktype.t, blktype1.t)
                        && ( $block_type !~ /^[\{\};]$/ )

                        # it seems best not to add semicolons in these
                        # special block types: sort|map|grep
                        && ( !$is_sort_map_grep{$block_type} )

                        # and we are allowed to do so.
                        && $rOpts->{'add-semicolons'}
                      )
                    {

                        save_current_token();
                        $token  = ';';
                        $type   = ';';
                        $level  = $levels_to_go[$max_index_to_go];
                        $slevel = $nesting_depth_to_go[$max_index_to_go];
                        $nesting_blocks =
                          $nesting_blocks_to_go[$max_index_to_go];
                        $ci_level       = $ci_levels_to_go[$max_index_to_go];
                        $block_type     = "";
                        $container_type = "";
                        $container_environment = "";
                        $type_sequence         = "";

                        # Note - we remove any blank AFTER extracting its
                        # parameters such as level, etc, above
                        if ( $types_to_go[$max_index_to_go] eq 'b' ) {
                            unstore_token_to_go();
                        }
                        store_token_to_go();

                        note_added_semicolon();
                        restore_current_token();
                    }

                    # then write out everything before this closing curly brace
                    output_line_to_go();

                }

                # Now update for side comment
                if ($side_comment_follows) { $no_internal_newlines = 1 }

                # store the closing curly brace
                store_token_to_go();

                # ok, we just stored a closing curly brace.  Often, but
                # not always, we want to end the line immediately.
                # So now we have to check for special cases.

                # if this '}' successfully ends a one-line block..
                my $is_one_line_block = 0;
                my $keep_going        = 0;
                if ( $index_start_one_line_block != UNDEFINED_INDEX ) {

                    # Remember the type of token just before the
                    # opening brace.  It would be more general to use
                    # a stack, but this will work for one-line blocks.
                    $is_one_line_block =
                      $types_to_go[$index_start_one_line_block];

                    # we have to actually make it by removing tentative
                    # breaks that were set within it
                    undo_forced_breakpoint_stack(0);
                    set_nobreaks( $index_start_one_line_block,
                        $max_index_to_go - 1 );

                    # then re-initialize for the next one-line block
                    destroy_one_line_block();

                    # then decide if we want to break after the '}' ..
                    # We will keep going to allow certain brace followers as in:
                    #   do { $ifclosed = 1; last } unless $losing;
                    #
                    # But make a line break if the curly ends a
                    # significant block:
                    if (
                        $is_block_without_semicolon{$block_type}

                        # if needless semicolon follows we handle it later
                        && $next_nonblank_token ne ';'
                      )
                    {
                        output_line_to_go() unless ($no_internal_newlines);
                    }
                }

                # set string indicating what we need to look for brace follower
                # tokens
                if ( $block_type eq 'do' ) {
                    $rbrace_follower = \%is_do_follower;
                }
                elsif ( $block_type =~ /^(if|elsif|unless)$/ ) {
                    $rbrace_follower = \%is_if_brace_follower;
                }
                elsif ( $block_type eq 'else' ) {
                    $rbrace_follower = \%is_else_brace_follower;
                }

                # added eval for borris.t
                elsif ($is_sort_map_grep_eval{$block_type}
                    || $is_one_line_block eq 'G' )
                {
                    $rbrace_follower = undef;
                    $keep_going      = 1;
                }

                # anonymous sub
                elsif ( $block_type =~ /^sub\W*$/ ) {

                    if ($is_one_line_block) {
                        $rbrace_follower = \%is_anon_sub_1_brace_follower;
                    }
                    else {
                        $rbrace_follower = \%is_anon_sub_brace_follower;
                    }
                }

                # None of the above: specify what can follow a closing
                # brace of a block which is not an
                # if/elsif/else/do/sort/map/grep/eval
                # Testfiles:
                # 'Toolbar.pm', 'Menubar.pm', bless.t, '3rules.pl', 'break1.t
                else {
                    $rbrace_follower = \%is_other_brace_follower;
                }

                # See if an elsif block is followed by another elsif or else;
                # complain if not.
                if ( $block_type eq 'elsif' ) {

                    if ( $next_nonblank_token_type eq 'b' ) {    # end of line?
                        $looking_for_else = 1;    # ok, check on next line
                    }
                    else {

                        unless ( $next_nonblank_token =~ /^(elsif|else)$/ ) {
                            write_logfile_entry("No else block :(\n");
                        }
                    }
                }

                # keep going after certain block types (map,sort,grep,eval)
                # added eval for borris.t
                if ($keep_going) {

                    # keep going
                }

                # if no more tokens, postpone decision until re-entring
                elsif ( ( $next_nonblank_token_type eq 'b' )
                    && $rOpts_add_newlines )
                {
                    unless ($rbrace_follower) {
                        output_line_to_go() unless ($no_internal_newlines);
                    }
                }

                elsif ($rbrace_follower) {

                    unless ( $rbrace_follower->{$next_nonblank_token} ) {
                        output_line_to_go() unless ($no_internal_newlines);
                    }
                    $rbrace_follower = undef;
                }

                else {
                    output_line_to_go() unless ($no_internal_newlines);
                }

            }    # end treatment of closing block token

            # handle semicolon
            elsif ( $type eq ';' ) {

                # kill one-line blocks with too many semicolons
                $semicolons_before_block_self_destruct--;
                if (
                    ( $semicolons_before_block_self_destruct < 0 )
                    || (   $semicolons_before_block_self_destruct == 0
                        && $next_nonblank_token_type !~ /^[b\}]$/ )
                  )
                {
                    destroy_one_line_block();
                }

                # Remove unnecessary semicolons, but not after bare
                # blocks, where it could be unsafe if the brace is
                # mistokenized.
                if (
                    (
                        $last_nonblank_token eq '}'
                        && (
                            $is_block_without_semicolon{
                                $last_nonblank_block_type}
                            || $last_nonblank_block_type =~ /^sub\s+\w/
                            || $last_nonblank_block_type =~ /^\w+:$/ )
                    )
                    || $last_nonblank_type eq ';'
                  )
                {

                    if (
                        $rOpts->{'delete-semicolons'}

                        # don't delete ; before a # because it would promote it
                        # to a block comment
                        && ( $next_nonblank_token_type ne '#' )
                      )
                    {
                        note_deleted_semicolon();
                        output_line_to_go()
                          unless ( $no_internal_newlines
                            || $index_start_one_line_block != UNDEFINED_INDEX );
                        next;
                    }
                    else {
                        write_logfile_entry("Extra ';'\n");
                    }
                }
                store_token_to_go();

                output_line_to_go()
                  unless ( $no_internal_newlines
                    || ( $rOpts_keep_interior_semicolons && $j < $jmax )
                    || ( $next_nonblank_token eq '}' ) );

            }

            # handle here_doc target string
            elsif ( $type eq 'h' ) {
                $no_internal_newlines =
                  1;    # no newlines after seeing here-target
                destroy_one_line_block();
                store_token_to_go();
            }

            # handle all other token types
            else {

                # if this is a blank...
                if ( $type eq 'b' ) {

                    # make it just one character
                    $token = ' ' if $rOpts_add_whitespace;

                    # delete it if unwanted by whitespace rules
                    # or we are deleting all whitespace
                    my $ws = $$rwhite_space_flag[ $j + 1 ];
                    if ( ( defined($ws) && $ws == -1 )
                        || $rOpts_delete_old_whitespace )
                    {

                        # unless it might make a syntax error
                        next
                          unless is_essential_whitespace(
                            $last_last_nonblank_token,
                            $last_last_nonblank_type,
                            $tokens_to_go[$max_index_to_go],
                            $types_to_go[$max_index_to_go],
                            $$rtokens[ $j + 1 ],
                            $$rtoken_type[ $j + 1 ]
                          );
                    }
                }
                store_token_to_go();
            }

            # remember two previous nonblank OUTPUT tokens
            if ( $type ne '#' && $type ne 'b' ) {
                $last_last_nonblank_token = $last_nonblank_token;
                $last_last_nonblank_type  = $last_nonblank_type;
                $last_nonblank_token      = $token;
                $last_nonblank_type       = $type;
                $last_nonblank_block_type = $block_type;
            }

            # unset the continued-quote flag since it only applies to the
            # first token, and we want to resume normal formatting if
            # there are additional tokens on the line
            $in_continued_quote = 0;

        }    # end of loop over all tokens in this 'line_of_tokens'

        # we have to flush ..
        if (

            # if there is a side comment
            ( ( $type eq '#' ) && !$rOpts->{'delete-side-comments'} )

            # if this line ends in a quote
            # NOTE: This is critically important for insuring that quoted lines
            # do not get processed by things like -sot and -sct
            || $in_quote

            # if this is a VERSION statement
            || $is_VERSION_statement

            # to keep a label on one line if that is how it is now
            || ( ( $type eq 'J' ) && ( $max_index_to_go == 0 ) )

            # if we are instructed to keep all old line breaks
            || !$rOpts->{'delete-old-newlines'}
          )
        {
            destroy_one_line_block();
            output_line_to_go();
        }

        # mark old line breakpoints in current output stream
        if ( $max_index_to_go >= 0 && !$rOpts_ignore_old_breakpoints ) {
            $old_breakpoint_to_go[$max_index_to_go] = 1;
        }
    }    # end sub print_line_of_tokens
}    # end print_line_of_tokens

# sub output_line_to_go sends one logical line of tokens on down the
# pipeline to the VerticalAligner package, breaking the line into continuation
# lines as necessary.  The line of tokens is ready to go in the "to_go"
# arrays.
sub output_line_to_go {

    # debug stuff; this routine can be called from many points
    FORMATTER_DEBUG_FLAG_OUTPUT && do {
        my ( $a, $b, $c ) = caller;
        write_diagnostics(
"OUTPUT: output_line_to_go called: $a $c $last_nonblank_type $last_nonblank_token, one_line=$index_start_one_line_block, tokens to write=$max_index_to_go\n"
        );
        my $output_str = join "", @tokens_to_go[ 0 .. $max_index_to_go ];
        write_diagnostics("$output_str\n");
    };

    # just set a tentative breakpoint if we might be in a one-line block
    if ( $index_start_one_line_block != UNDEFINED_INDEX ) {
        set_forced_breakpoint($max_index_to_go);
        return;
    }

    my $cscw_block_comment;
    $cscw_block_comment = add_closing_side_comment()
      if ( $rOpts->{'closing-side-comments'} && $max_index_to_go >= 0 );

    match_opening_and_closing_tokens();

    # tell the -lp option we are outputting a batch so it can close
    # any unfinished items in its stack
    finish_lp_batch();

    # If this line ends in a code block brace, set breaks at any
    # previous closing code block braces to breakup a chain of code
    # blocks on one line.  This is very rare but can happen for
    # user-defined subs.  For example we might be looking at this:
    #  BOOL { $server_data{uptime} > 0; } NUM { $server_data{load}; } STR {
    my $saw_good_break = 0;    # flag to force breaks even if short line
    if (

        # looking for opening or closing block brace
        $block_type_to_go[$max_index_to_go]

        # but not one of these which are never duplicated on a line:
        # until|while|for|if|elsif|else
        && !$is_block_without_semicolon{ $block_type_to_go[$max_index_to_go] }
      )
    {
        my $lev = $nesting_depth_to_go[$max_index_to_go];

        # Walk backwards from the end and
        # set break at any closing block braces at the same level.
        # But quit if we are not in a chain of blocks.
        for ( my $i = $max_index_to_go - 1 ; $i >= 0 ; $i-- ) {
            last if ( $levels_to_go[$i] < $lev );    # stop at a lower level
            next if ( $levels_to_go[$i] > $lev );    # skip past higher level

            if ( $block_type_to_go[$i] ) {
                if ( $tokens_to_go[$i] eq '}' ) {
                    set_forced_breakpoint($i);
                    $saw_good_break = 1;
                }
            }

            # quit if we see anything besides words, function, blanks
            # at this level
            elsif ( $types_to_go[$i] !~ /^[\(\)Gwib]$/ ) { last }
        }
    }

    my $imin = 0;
    my $imax = $max_index_to_go;

    # trim any blank tokens
    if ( $max_index_to_go >= 0 ) {
        if ( $types_to_go[$imin] eq 'b' ) { $imin++ }
        if ( $types_to_go[$imax] eq 'b' ) { $imax-- }
    }

    # anything left to write?
    if ( $imin <= $imax ) {

        # add a blank line before certain key types
        if ( $last_line_leading_type !~ /^[#b]/ ) {
            my $want_blank    = 0;
            my $leading_token = $tokens_to_go[$imin];
            my $leading_type  = $types_to_go[$imin];

            # blank lines before subs except declarations and one-liners
            # MCONVERSION LOCATION - for sub tokenization change
            if ( $leading_token =~ /^(sub\s)/ && $leading_type eq 'i' ) {
                $want_blank = ( $rOpts->{'blanks-before-subs'} )
                  && (
                    terminal_type( \@types_to_go, \@block_type_to_go, $imin,
                        $imax ) !~ /^[\;\}]$/
                  );
            }

            # break before all package declarations
            # MCONVERSION LOCATION - for tokenizaton change
            elsif ($leading_token =~ /^(package\s)/
                && $leading_type eq 'i' )
            {
                $want_blank = ( $rOpts->{'blanks-before-subs'} );
            }

            # break before certain key blocks except one-liners
            if ( $leading_token =~ /^(BEGIN|END)$/ && $leading_type eq 'k' ) {
                $want_blank = ( $rOpts->{'blanks-before-subs'} )
                  && (
                    terminal_type( \@types_to_go, \@block_type_to_go, $imin,
                        $imax ) ne '}'
                  );
            }

            # Break before certain block types if we haven't had a
            # break at this level for a while.  This is the
            # difficult decision..
            elsif ($leading_token =~ /^(unless|if|while|until|for|foreach)$/
                && $leading_type eq 'k' )
            {
                my $lc = $nonblank_lines_at_depth[$last_line_leading_level];
                if ( !defined($lc) ) { $lc = 0 }

                $want_blank =
                     $rOpts->{'blanks-before-blocks'}
                  && $lc >= $rOpts->{'long-block-line-count'}
                  && $file_writer_object->get_consecutive_nonblank_lines() >=
                  $rOpts->{'long-block-line-count'}
                  && (
                    terminal_type( \@types_to_go, \@block_type_to_go, $imin,
                        $imax ) ne '}'
                  );
            }

            if ($want_blank) {

                # future: send blank line down normal path to VerticalAligner
                Perl::Tidy::VerticalAligner::flush();
                $file_writer_object->write_blank_code_line();
            }
        }

        # update blank line variables and count number of consecutive
        # non-blank, non-comment lines at this level
        $last_last_line_leading_level = $last_line_leading_level;
        $last_line_leading_level      = $levels_to_go[$imin];
        if ( $last_line_leading_level < 0 ) { $last_line_leading_level = 0 }
        $last_line_leading_type = $types_to_go[$imin];
        if (   $last_line_leading_level == $last_last_line_leading_level
            && $last_line_leading_type ne 'b'
            && $last_line_leading_type ne '#'
            && defined( $nonblank_lines_at_depth[$last_line_leading_level] ) )
        {
            $nonblank_lines_at_depth[$last_line_leading_level]++;
        }
        else {
            $nonblank_lines_at_depth[$last_line_leading_level] = 1;
        }

        FORMATTER_DEBUG_FLAG_FLUSH && do {
            my ( $package, $file, $line ) = caller;
            print
"FLUSH: flushing from $package $file $line, types= $types_to_go[$imin] to $types_to_go[$imax]\n";
        };

        # add a couple of extra terminal blank tokens
        pad_array_to_go();

        # set all forced breakpoints for good list formatting
        my $is_long_line = excess_line_length( $imin, $max_index_to_go ) > 0;

        if (
            $max_index_to_go > 0
            && (
                   $is_long_line
                || $old_line_count_in_batch > 1
                || is_unbalanced_batch()
                || (
                    $comma_count_in_batch
                    && (   $rOpts_maximum_fields_per_table > 0
                        || $rOpts_comma_arrow_breakpoints == 0 )
                )
            )
          )
        {
            $saw_good_break ||= scan_list();
        }

        # let $ri_first and $ri_last be references to lists of
        # first and last tokens of line fragments to output..
        my ( $ri_first, $ri_last );

        # write a single line if..
        if (

            # we aren't allowed to add any newlines
            !$rOpts_add_newlines

            # or, we don't already have an interior breakpoint
            # and we didn't see a good breakpoint
            || (
                   !$forced_breakpoint_count
                && !$saw_good_break

                # and this line is 'short'
                && !$is_long_line
            )
          )
        {
            @$ri_first = ($imin);
            @$ri_last  = ($imax);
        }

        # otherwise use multiple lines
        else {

            ( $ri_first, $ri_last, my $colon_count ) =
              set_continuation_breaks($saw_good_break);

            break_all_chain_tokens( $ri_first, $ri_last );

            break_equals( $ri_first, $ri_last );

            # now we do a correction step to clean this up a bit
            # (The only time we would not do this is for debugging)
            if ( $rOpts->{'recombine'} ) {
                ( $ri_first, $ri_last ) =
                  recombine_breakpoints( $ri_first, $ri_last );
            }

            insert_final_breaks( $ri_first, $ri_last ) if $colon_count;
        }

        # do corrector step if -lp option is used
        my $do_not_pad = 0;
        if ($rOpts_line_up_parentheses) {
            $do_not_pad = correct_lp_indentation( $ri_first, $ri_last );
        }
        send_lines_to_vertical_aligner( $ri_first, $ri_last, $do_not_pad );
    }
    prepare_for_new_input_lines();

    # output any new -cscw block comment
    if ($cscw_block_comment) {
        flush();
        $file_writer_object->write_code_line( $cscw_block_comment . "\n" );
    }
}

sub note_added_semicolon {
    $last_added_semicolon_at = $input_line_number;
    if ( $added_semicolon_count == 0 ) {
        $first_added_semicolon_at = $last_added_semicolon_at;
    }
    $added_semicolon_count++;
    write_logfile_entry("Added ';' here\n");
}

sub note_deleted_semicolon {
    $last_deleted_semicolon_at = $input_line_number;
    if ( $deleted_semicolon_count == 0 ) {
        $first_deleted_semicolon_at = $last_deleted_semicolon_at;
    }
    $deleted_semicolon_count++;
    write_logfile_entry("Deleted unnecessary ';'\n");    # i hope ;)
}

sub note_embedded_tab {
    $embedded_tab_count++;
    $last_embedded_tab_at = $input_line_number;
    if ( !$first_embedded_tab_at ) {
        $first_embedded_tab_at = $last_embedded_tab_at;
    }

    if ( $embedded_tab_count <= MAX_NAG_MESSAGES ) {
        write_logfile_entry("Embedded tabs in quote or pattern\n");
    }
}

sub starting_one_line_block {

    # after seeing an opening curly brace, look for the closing brace
    # and see if the entire block will fit on a line.  This routine is
    # not always right because it uses the old whitespace, so a check
    # is made later (at the closing brace) to make sure we really
    # have a one-line block.  We have to do this preliminary check,
    # though, because otherwise we would always break at a semicolon
    # within a one-line block if the block contains multiple statements.

    my ( $j, $jmax, $level, $slevel, $ci_level, $rtokens, $rtoken_type,
        $rblock_type )
      = @_;

    # kill any current block - we can only go 1 deep
    destroy_one_line_block();

    # return value:
    #  1=distance from start of block to opening brace exceeds line length
    #  0=otherwise

    my $i_start = 0;

    # shouldn't happen: there must have been a prior call to
    # store_token_to_go to put the opening brace in the output stream
    if ( $max_index_to_go < 0 ) {
        warning("program bug: store_token_to_go called incorrectly\n");
        report_definite_bug();
    }
    else {

        # cannot use one-line blocks with cuddled else else/elsif lines
        if ( ( $tokens_to_go[0] eq '}' ) && $rOpts_cuddled_else ) {
            return 0;
        }
    }

    my $block_type = $$rblock_type[$j];

    # find the starting keyword for this block (such as 'if', 'else', ...)

    if ( $block_type =~ /^[\{\}\;\:]$/ ) {
        $i_start = $max_index_to_go;
    }

    elsif ( $last_last_nonblank_token_to_go eq ')' ) {

        # For something like "if (xxx) {", the keyword "if" will be
        # just after the most recent break. This will be 0 unless
        # we have just killed a one-line block and are starting another.
        # (doif.t)
        $i_start = $index_max_forced_break + 1;
        if ( $types_to_go[$i_start] eq 'b' ) {
            $i_start++;
        }

        unless ( $tokens_to_go[$i_start] eq $block_type ) {
            return 0;
        }
    }

    # the previous nonblank token should start these block types
    elsif (
        ( $last_last_nonblank_token_to_go eq $block_type )
        || (   $block_type =~ /^sub/
            && $last_last_nonblank_token_to_go =~ /^sub/ )
      )
    {
        $i_start = $last_last_nonblank_index_to_go;
    }

    # patch for SWITCH/CASE to retain one-line case/when blocks
    elsif ( $block_type eq 'case' || $block_type eq 'when' ) {
        $i_start = $index_max_forced_break + 1;
        if ( $types_to_go[$i_start] eq 'b' ) {
            $i_start++;
        }
        unless ( $tokens_to_go[$i_start] eq $block_type ) {
            return 0;
        }
    }

    else {
        return 1;
    }

    my $pos = total_line_length( $i_start, $max_index_to_go ) - 1;

    my $i;

    # see if length is too long to even start
    if ( $pos > $rOpts_maximum_line_length ) {
        return 1;
    }

    for ( $i = $j + 1 ; $i <= $jmax ; $i++ ) {

        # old whitespace could be arbitrarily large, so don't use it
        if   ( $$rtoken_type[$i] eq 'b' ) { $pos += 1 }
        else                              { $pos += length( $$rtokens[$i] ) }

        # Return false result if we exceed the maximum line length,
        if ( $pos > $rOpts_maximum_line_length ) {
            return 0;
        }

        # or encounter another opening brace before finding the closing brace.
        elsif ($$rtokens[$i] eq '{'
            && $$rtoken_type[$i] eq '{'
            && $$rblock_type[$i] )
        {
            return 0;
        }

        # if we find our closing brace..
        elsif ($$rtokens[$i] eq '}'
            && $$rtoken_type[$i] eq '}'
            && $$rblock_type[$i] )
        {

            # be sure any trailing comment also fits on the line
            my $i_nonblank =
              ( $$rtoken_type[ $i + 1 ] eq 'b' ) ? $i + 2 : $i + 1;

            if ( $$rtoken_type[$i_nonblank] eq '#' ) {
                $pos += length( $$rtokens[$i_nonblank] );

                if ( $i_nonblank > $i + 1 ) {
                    $pos += length( $$rtokens[ $i + 1 ] );
                }

                if ( $pos > $rOpts_maximum_line_length ) {
                    return 0;
                }
            }

            # ok, it's a one-line block
            create_one_line_block( $i_start, 20 );
            return 0;
        }

        # just keep going for other characters
        else {
        }
    }

    # Allow certain types of new one-line blocks to form by joining
    # input lines.  These can be safely done, but for other block types,
    # we keep old one-line blocks but do not form new ones. It is not
    # always a good idea to make as many one-line blocks as possible,
    # so other types are not done.  The user can always use -mangle.
    if ( $is_sort_map_grep_eval{$block_type} ) {
        create_one_line_block( $i_start, 1 );
    }

    return 0;
}

sub unstore_token_to_go {

    # remove most recent token from output stream
    if ( $max_index_to_go > 0 ) {
        $max_index_to_go--;
    }
    else {
        $max_index_to_go = UNDEFINED_INDEX;
    }

}

sub want_blank_line {
    flush();
    $file_writer_object->want_blank_line();
}

sub write_unindented_line {
    flush();
    $file_writer_object->write_line( $_[0] );
}

sub undo_lp_ci {

    # If there is a single, long parameter within parens, like this:
    #
    #  $self->command( "/msg "
    #        . $infoline->chan
    #        . " You said $1, but did you know that it's square was "
    #        . $1 * $1 . " ?" );
    #
    # we can remove the continuation indentation of the 2nd and higher lines
    # to achieve this effect, which is more pleasing:
    #
    #  $self->command("/msg "
    #                 . $infoline->chan
    #                 . " You said $1, but did you know that it's square was "
    #                 . $1 * $1 . " ?");

    my ( $line_open, $i_start, $closing_index, $ri_first, $ri_last ) = @_;
    my $max_line = @$ri_first - 1;

    # must be multiple lines
    return unless $max_line > $line_open;

    my $lev_start     = $levels_to_go[$i_start];
    my $ci_start_plus = 1 + $ci_levels_to_go[$i_start];

    # see if all additional lines in this container have continuation
    # indentation
    my $n;
    my $line_1 = 1 + $line_open;
    for ( $n = $line_1 ; $n <= $max_line ; ++$n ) {
        my $ibeg = $$ri_first[$n];
        my $iend = $$ri_last[$n];
        if ( $ibeg eq $closing_index ) { $n--; last }
        return if ( $lev_start != $levels_to_go[$ibeg] );
        return if ( $ci_start_plus != $ci_levels_to_go[$ibeg] );
        last   if ( $closing_index <= $iend );
    }

    # we can reduce the indentation of all continuation lines
    my $continuation_line_count = $n - $line_open;
    @ci_levels_to_go[ @$ri_first[ $line_1 .. $n ] ] =
      (0) x ($continuation_line_count);
    @leading_spaces_to_go[ @$ri_first[ $line_1 .. $n ] ] =
      @reduced_spaces_to_go[ @$ri_first[ $line_1 .. $n ] ];
}

sub set_logical_padding {

    # Look at a batch of lines and see if extra padding can improve the
    # alignment when there are certain leading operators. Here is an
    # example, in which some extra space is introduced before
    # '( $year' to make it line up with the subsequent lines:
    #
    #       if (   ( $Year < 1601 )
    #           || ( $Year > 2899 )
    #           || ( $EndYear < 1601 )
    #           || ( $EndYear > 2899 ) )
    #       {
    #           &Error_OutOfRange;
    #       }
    #
    my ( $ri_first, $ri_last ) = @_;
    my $max_line = @$ri_first - 1;

    my ( $ibeg, $ibeg_next, $ibegm, $iend, $iendm, $ipad, $line, $pad_spaces,
        $tok_next, $type_next, $has_leading_op_next, $has_leading_op );

    # looking at each line of this batch..
    foreach $line ( 0 .. $max_line - 1 ) {

        # see if the next line begins with a logical operator
        $ibeg      = $$ri_first[$line];
        $iend      = $$ri_last[$line];
        $ibeg_next = $$ri_first[ $line + 1 ];
        $tok_next  = $tokens_to_go[$ibeg_next];
        $type_next = $types_to_go[$ibeg_next];

        $has_leading_op_next = ( $tok_next =~ /^\w/ )
          ? $is_chain_operator{$tok_next}      # + - * / : ? && ||
          : $is_chain_operator{$type_next};    # and, or

        next unless ($has_leading_op_next);

        # next line must not be at lesser depth
        next
          if ( $nesting_depth_to_go[$ibeg] > $nesting_depth_to_go[$ibeg_next] );

        # identify the token in this line to be padded on the left
        $ipad = undef;

        # handle lines at same depth...
        if ( $nesting_depth_to_go[$ibeg] == $nesting_depth_to_go[$ibeg_next] ) {

            # if this is not first line of the batch ...
            if ( $line > 0 ) {

                # and we have leading operator..
                next if $has_leading_op;

                # Introduce padding if..
                # 1. the previous line is at lesser depth, or
                # 2. the previous line ends in an assignment
                # 3. the previous line ends in a 'return'
                # 4. the previous line ends in a comma
                # Example 1: previous line at lesser depth
                #       if (   ( $Year < 1601 )      # <- we are here but
                #           || ( $Year > 2899 )      #  list has not yet
                #           || ( $EndYear < 1601 )   # collapsed vertically
                #           || ( $EndYear > 2899 ) )
                #       {
                #
                # Example 2: previous line ending in assignment:
                #    $leapyear =
                #        $year % 4   ? 0     # <- We are here
                #      : $year % 100 ? 1
                #      : $year % 400 ? 0
                #      : 1;
                #
                # Example 3: previous line ending in comma:
                #    push @expr,
                #        /test/   ? undef
                #      : eval($_) ? 1
                #      : eval($_) ? 1
                #      :            0;

                # be sure levels agree (do not indent after an indented 'if')
                next if ( $levels_to_go[$ibeg] ne $levels_to_go[$ibeg_next] );

                # allow padding on first line after a comma but only if:
                # (1) this is line 2 and
                # (2) there are at more than three lines and
                # (3) lines 3 and 4 have the same leading operator
                # These rules try to prevent padding within a long
                # comma-separated list.
                my $ok_comma;
                if (   $types_to_go[$iendm] eq ','
                    && $line == 1
                    && $max_line > 2 )
                {
                    my $ibeg_next_next = $$ri_first[ $line + 2 ];
                    my $tok_next_next  = $tokens_to_go[$ibeg_next_next];
                    $ok_comma = $tok_next_next eq $tok_next;
                }

                next
                  unless (
                       $is_assignment{ $types_to_go[$iendm] }
                    || $ok_comma
                    || ( $nesting_depth_to_go[$ibegm] <
                        $nesting_depth_to_go[$ibeg] )
                    || (   $types_to_go[$iendm] eq 'k'
                        && $tokens_to_go[$iendm] eq 'return' )
                  );

                # we will add padding before the first token
                $ipad = $ibeg;
            }

            # for first line of the batch..
            else {

                # WARNING: Never indent if first line is starting in a
                # continued quote, which would change the quote.
                next if $starting_in_quote;

                # if this is text after closing '}'
                # then look for an interior token to pad
                if ( $types_to_go[$ibeg] eq '}' ) {

                }

                # otherwise, we might pad if it looks really good
                else {

                    # we might pad token $ibeg, so be sure that it
                    # is at the same depth as the next line.
                    next
                      if ( $nesting_depth_to_go[$ibeg] !=
                        $nesting_depth_to_go[$ibeg_next] );

                    # We can pad on line 1 of a statement if at least 3
                    # lines will be aligned. Otherwise, it
                    # can look very confusing.

                 # We have to be careful not to pad if there are too few
                 # lines.  The current rule is:
                 # (1) in general we require at least 3 consecutive lines
                 # with the same leading chain operator token,
                 # (2) but an exception is that we only require two lines
                 # with leading colons if there are no more lines.  For example,
                 # the first $i in the following snippet would get padding
                 # by the second rule:
                 #
                 #   $i == 1 ? ( "First", "Color" )
                 # : $i == 2 ? ( "Then",  "Rarity" )
                 # :           ( "Then",  "Name" );

                    if ( $max_line > 1 ) {
                        my $leading_token = $tokens_to_go[$ibeg_next];
                        my $tokens_differ;

                        # never indent line 1 of a '.' series because
                        # previous line is most likely at same level.
                        # TODO: we should also look at the leasing_spaces
                        # of the last output line and skip if it is same
                        # as this line.
                        next if ( $leading_token eq '.' );

                        my $count = 1;
                        foreach my $l ( 2 .. 3 ) {
                            last if ( $line + $l > $max_line );
                            my $ibeg_next_next = $$ri_first[ $line + $l ];
                            if ( $tokens_to_go[$ibeg_next_next] ne
                                $leading_token )
                            {
                                $tokens_differ = 1;
                                last;
                            }
                            $count++;
                        }
                        next if ($tokens_differ);
                        next if ( $count < 3 && $leading_token ne ':' );
                        $ipad = $ibeg;
                    }
                    else {
                        next;
                    }
                }
            }
        }

        # find interior token to pad if necessary
        if ( !defined($ipad) ) {

            for ( my $i = $ibeg ; ( $i < $iend ) && !$ipad ; $i++ ) {

                # find any unclosed container
                next
                  unless ( $type_sequence_to_go[$i]
                    && $mate_index_to_go[$i] > $iend );

                # find next nonblank token to pad
                $ipad = $i + 1;
                if ( $types_to_go[$ipad] eq 'b' ) {
                    $ipad++;
                    last if ( $ipad > $iend );
                }
            }
            last unless $ipad;
        }

        # next line must not be at greater depth
        my $iend_next = $$ri_last[ $line + 1 ];
        next
          if ( $nesting_depth_to_go[ $iend_next + 1 ] >
            $nesting_depth_to_go[$ipad] );

        # lines must be somewhat similar to be padded..
        my $inext_next = $ibeg_next + 1;
        if ( $types_to_go[$inext_next] eq 'b' ) {
            $inext_next++;
        }
        my $type      = $types_to_go[$ipad];
        my $type_next = $types_to_go[ $ipad + 1 ];

        # see if there are multiple continuation lines
        my $logical_continuation_lines = 1;
        if ( $line + 2 <= $max_line ) {
            my $leading_token  = $tokens_to_go[$ibeg_next];
            my $ibeg_next_next = $$ri_first[ $line + 2 ];
            if (   $tokens_to_go[$ibeg_next_next] eq $leading_token
                && $nesting_depth_to_go[$ibeg_next] eq
                $nesting_depth_to_go[$ibeg_next_next] )
            {
                $logical_continuation_lines++;
            }
        }

        # see if leading types match
        my $types_match = $types_to_go[$inext_next] eq $type;
        my $matches_without_bang;

        # if first line has leading ! then compare the following token
        if ( !$types_match && $type eq '!' ) {
            $types_match = $matches_without_bang =
              $types_to_go[$inext_next] eq $types_to_go[ $ipad + 1 ];
        }

        if (

            # either we have multiple continuation lines to follow
            # and we are not padding the first token
            ( $logical_continuation_lines > 1 && $ipad > 0 )

            # or..
            || (

                # types must match
                $types_match

                # and keywords must match if keyword
                && !(
                       $type eq 'k'
                    && $tokens_to_go[$ipad] ne $tokens_to_go[$inext_next]
                )
            )
          )
        {

            #----------------------begin special checks--------------
            #
            # SPECIAL CHECK 1:
            # A check is needed before we can make the pad.
            # If we are in a list with some long items, we want each
            # item to stand out.  So in the following example, the
            # first line begining with '$casefold->' would look good
            # padded to align with the next line, but then it
            # would be indented more than the last line, so we
            # won't do it.
            #
            #  ok(
            #      $casefold->{code}         eq '0041'
            #        && $casefold->{status}  eq 'C'
            #        && $casefold->{mapping} eq '0061',
            #      'casefold 0x41'
            #  );
            #
            # Note:
            # It would be faster, and almost as good, to use a comma
            # count, and not pad if comma_count > 1 and the previous
            # line did not end with a comma.
            #
            my $ok_to_pad = 1;

            my $ibg   = $$ri_first[ $line + 1 ];
            my $depth = $nesting_depth_to_go[ $ibg + 1 ];

            # just use simplified formula for leading spaces to avoid
            # needless sub calls
            my $lsp = $levels_to_go[$ibg] + $ci_levels_to_go[$ibg];

            # look at each line beyond the next ..
            my $l = $line + 1;
            foreach $l ( $line + 2 .. $max_line ) {
                my $ibg = $$ri_first[$l];

                # quit looking at the end of this container
                last
                  if ( $nesting_depth_to_go[ $ibg + 1 ] < $depth )
                  || ( $nesting_depth_to_go[$ibg] < $depth );

                # cannot do the pad if a later line would be
                # outdented more
                if ( $levels_to_go[$ibg] + $ci_levels_to_go[$ibg] < $lsp ) {
                    $ok_to_pad = 0;
                    last;
                }
            }

            # don't pad if we end in a broken list
            if ( $l == $max_line ) {
                my $i2 = $$ri_last[$l];
                if ( $types_to_go[$i2] eq '#' ) {
                    my $i1 = $$ri_first[$l];
                    next
                      if (
                        terminal_type( \@types_to_go, \@block_type_to_go, $i1,
                            $i2 ) eq ','
                      );
                }
            }

            # SPECIAL CHECK 2:
            # a minus may introduce a quoted variable, and we will
            # add the pad only if this line begins with a bare word,
            # such as for the word 'Button' here:
            #    [
            #         Button      => "Print letter \"~$_\"",
            #        -command     => [ sub { print "$_[0]\n" }, $_ ],
            #        -accelerator => "Meta+$_"
            #    ];
            #
            #  On the other hand, if 'Button' is quoted, it looks best
            #  not to pad:
            #    [
            #        'Button'     => "Print letter \"~$_\"",
            #        -command     => [ sub { print "$_[0]\n" }, $_ ],
            #        -accelerator => "Meta+$_"
            #    ];
            if ( $types_to_go[$ibeg_next] eq 'm' ) {
                $ok_to_pad = 0 if $types_to_go[$ibeg] eq 'Q';
            }

            next unless $ok_to_pad;

            #----------------------end special check---------------

            my $length_1 = total_line_length( $ibeg,      $ipad - 1 );
            my $length_2 = total_line_length( $ibeg_next, $inext_next - 1 );
            $pad_spaces = $length_2 - $length_1;

            # If the first line has a leading ! and the second does
            # not, then remove one space to try to align the next
            # leading characters, which are often the same.  For example:
            #  if (  !$ts
            #      || $ts == $self->Holder
            #      || $self->Holder->Type eq "Arena" )
            #
            # This usually helps readability, but if there are subsequent
            # ! operators things will still get messed up.  For example:
            #
            #  if (  !exists $Net::DNS::typesbyname{$qtype}
            #      && exists $Net::DNS::classesbyname{$qtype}
            #      && !exists $Net::DNS::classesbyname{$qclass}
            #      && exists $Net::DNS::typesbyname{$qclass} )
            # We can't fix that.
            if ($matches_without_bang) { $pad_spaces-- }

            # make sure this won't change if -lp is used
            my $indentation_1 = $leading_spaces_to_go[$ibeg];
            if ( ref($indentation_1) ) {
                if ( $indentation_1->get_RECOVERABLE_SPACES() == 0 ) {
                    my $indentation_2 = $leading_spaces_to_go[$ibeg_next];
                    unless ( $indentation_2->get_RECOVERABLE_SPACES() == 0 ) {
                        $pad_spaces = 0;
                    }
                }
            }

            # we might be able to handle a pad of -1 by removing a blank
            # token
            if ( $pad_spaces < 0 ) {

                if ( $pad_spaces == -1 ) {
                    if ( $ipad > $ibeg && $types_to_go[ $ipad - 1 ] eq 'b' ) {
                        $tokens_to_go[ $ipad - 1 ] = '';
                    }
                }
                $pad_spaces = 0;
            }

            # now apply any padding for alignment
            if ( $ipad >= 0 && $pad_spaces ) {

                my $length_t = total_line_length( $ibeg, $iend );
                if ( $pad_spaces + $length_t <= $rOpts_maximum_line_length ) {
                    $tokens_to_go[$ipad] =
                      ' ' x $pad_spaces . $tokens_to_go[$ipad];
                }
            }
        }
    }
    continue {
        $iendm          = $iend;
        $ibegm          = $ibeg;
        $has_leading_op = $has_leading_op_next;
    }    # end of loop over lines
    return;
}

sub correct_lp_indentation {

    # When the -lp option is used, we need to make a last pass through
    # each line to correct the indentation positions in case they differ
    # from the predictions.  This is necessary because perltidy uses a
    # predictor/corrector method for aligning with opening parens.  The
    # predictor is usually good, but sometimes stumbles.  The corrector
    # tries to patch things up once the actual opening paren locations
    # are known.
    my ( $ri_first, $ri_last ) = @_;
    my $do_not_pad = 0;

    #  Note on flag '$do_not_pad':
    #  We want to avoid a situation like this, where the aligner inserts
    #  whitespace before the '=' to align it with a previous '=', because
    #  otherwise the parens might become mis-aligned in a situation like
    #  this, where the '=' has become aligned with the previous line,
    #  pushing the opening '(' forward beyond where we want it.
    #
    #  $mkFloor::currentRoom = '';
    #  $mkFloor::c_entry     = $c->Entry(
    #                                 -width        => '10',
    #                                 -relief       => 'sunken',
    #                                 ...
    #                                 );
    #
    #  We leave it to the aligner to decide how to do this.

    # first remove continuation indentation if appropriate
    my $max_line = @$ri_first - 1;

    # looking at each line of this batch..
    my ( $ibeg, $iend );
    my $line;
    foreach $line ( 0 .. $max_line ) {
        $ibeg = $$ri_first[$line];
        $iend = $$ri_last[$line];

        # looking at each token in this output line..
        my $i;
        foreach $i ( $ibeg .. $iend ) {

            # How many space characters to place before this token
            # for special alignment.  Actual padding is done in the
            # continue block.

            # looking for next unvisited indentation item
            my $indentation = $leading_spaces_to_go[$i];
            if ( !$indentation->get_MARKED() ) {
                $indentation->set_MARKED(1);

                # looking for indentation item for which we are aligning
                # with parens, braces, and brackets
                next unless ( $indentation->get_ALIGN_PAREN() );

                # skip closed container on this line
                if ( $i > $ibeg ) {
                    my $im = $i - 1;
                    if ( $types_to_go[$im] eq 'b' && $im > $ibeg ) { $im-- }
                    if (   $type_sequence_to_go[$im]
                        && $mate_index_to_go[$im] <= $iend )
                    {
                        next;
                    }
                }

                if ( $line == 1 && $i == $ibeg ) {
                    $do_not_pad = 1;
                }

                # Ok, let's see what the error is and try to fix it
                my $actual_pos;
                my $predicted_pos = $indentation->get_SPACES();
                if ( $i > $ibeg ) {

                    # token is mid-line - use length to previous token
                    $actual_pos = total_line_length( $ibeg, $i - 1 );

                    # for mid-line token, we must check to see if all
                    # additional lines have continuation indentation,
                    # and remove it if so.  Otherwise, we do not get
                    # good alignment.
                    my $closing_index = $indentation->get_CLOSED();
                    if ( $closing_index > $iend ) {
                        my $ibeg_next = $$ri_first[ $line + 1 ];
                        if ( $ci_levels_to_go[$ibeg_next] > 0 ) {
                            undo_lp_ci( $line, $i, $closing_index, $ri_first,
                                $ri_last );
                        }
                    }
                }
                elsif ( $line > 0 ) {

                    # handle case where token starts a new line;
                    # use length of previous line
                    my $ibegm = $$ri_first[ $line - 1 ];
                    my $iendm = $$ri_last[ $line - 1 ];
                    $actual_pos = total_line_length( $ibegm, $iendm );

                    # follow -pt style
                    ++$actual_pos
                      if ( $types_to_go[ $iendm + 1 ] eq 'b' );
                }
                else {

                    # token is first character of first line of batch
                    $actual_pos = $predicted_pos;
                }

                my $move_right = $actual_pos - $predicted_pos;

                # done if no error to correct (gnu2.t)
                if ( $move_right == 0 ) {
                    $indentation->set_RECOVERABLE_SPACES($move_right);
                    next;
                }

                # if we have not seen closure for this indentation in
                # this batch, we can only pass on a request to the
                # vertical aligner
                my $closing_index = $indentation->get_CLOSED();

                if ( $closing_index < 0 ) {
                    $indentation->set_RECOVERABLE_SPACES($move_right);
                    next;
                }

                # If necessary, look ahead to see if there is really any
                # leading whitespace dependent on this whitespace, and
                # also find the longest line using this whitespace.
                # Since it is always safe to move left if there are no
                # dependents, we only need to do this if we may have
                # dependent nodes or need to move right.

                my $right_margin = 0;
                my $have_child   = $indentation->get_HAVE_CHILD();

                my %saw_indentation;
                my $line_count = 1;
                $saw_indentation{$indentation} = $indentation;

                if ( $have_child || $move_right > 0 ) {
                    $have_child = 0;
                    my $max_length = 0;
                    if ( $i == $ibeg ) {
                        $max_length = total_line_length( $ibeg, $iend );
                    }

                    # look ahead at the rest of the lines of this batch..
                    my $line_t;
                    foreach $line_t ( $line + 1 .. $max_line ) {
                        my $ibeg_t = $$ri_first[$line_t];
                        my $iend_t = $$ri_last[$line_t];
                        last if ( $closing_index <= $ibeg_t );

                        # remember all different indentation objects
                        my $indentation_t = $leading_spaces_to_go[$ibeg_t];
                        $saw_indentation{$indentation_t} = $indentation_t;
                        $line_count++;

                        # remember longest line in the group
                        my $length_t = total_line_length( $ibeg_t, $iend_t );
                        if ( $length_t > $max_length ) {
                            $max_length = $length_t;
                        }
                    }
                    $right_margin = $rOpts_maximum_line_length - $max_length;
                    if ( $right_margin < 0 ) { $right_margin = 0 }
                }

                my $first_line_comma_count =
                  grep { $_ eq ',' } @types_to_go[ $ibeg .. $iend ];
                my $comma_count = $indentation->get_COMMA_COUNT();
                my $arrow_count = $indentation->get_ARROW_COUNT();

                # This is a simple approximate test for vertical alignment:
                # if we broke just after an opening paren, brace, bracket,
                # and there are 2 or more commas in the first line,
                # and there are no '=>'s,
                # then we are probably vertically aligned.  We could set
                # an exact flag in sub scan_list, but this is good
                # enough.
                my $indentation_count = keys %saw_indentation;
                my $is_vertically_aligned =
                  (      $i == $ibeg
                      && $first_line_comma_count > 1
                      && $indentation_count == 1
                      && ( $arrow_count == 0 || $arrow_count == $line_count ) );

                # Make the move if possible ..
                if (

                    # we can always move left
                    $move_right < 0

                    # but we should only move right if we are sure it will
                    # not spoil vertical alignment
                    || ( $comma_count == 0 )
                    || ( $comma_count > 0 && !$is_vertically_aligned )
                  )
                {
                    my $move =
                      ( $move_right <= $right_margin )
                      ? $move_right
                      : $right_margin;

                    foreach ( keys %saw_indentation ) {
                        $saw_indentation{$_}
                          ->permanently_decrease_AVAILABLE_SPACES( -$move );
                    }
                }

                # Otherwise, record what we want and the vertical aligner
                # will try to recover it.
                else {
                    $indentation->set_RECOVERABLE_SPACES($move_right);
                }
            }
        }
    }
    return $do_not_pad;
}

# flush is called to output any tokens in the pipeline, so that
# an alternate source of lines can be written in the correct order

sub flush {
    destroy_one_line_block();
    output_line_to_go();
    Perl::Tidy::VerticalAligner::flush();
}

sub reset_block_text_accumulator {

    # save text after 'if' and 'elsif' to append after 'else'
    if ($accumulating_text_for_block) {

        if ( $accumulating_text_for_block =~ /^(if|elsif)$/ ) {
            push @{$rleading_block_if_elsif_text}, $leading_block_text;
        }
    }
    $accumulating_text_for_block        = "";
    $leading_block_text                 = "";
    $leading_block_text_level           = 0;
    $leading_block_text_length_exceeded = 0;
    $leading_block_text_line_number     = 0;
    $leading_block_text_line_length     = 0;
}

sub set_block_text_accumulator {
    my $i = shift;
    $accumulating_text_for_block = $tokens_to_go[$i];
    if ( $accumulating_text_for_block !~ /^els/ ) {
        $rleading_block_if_elsif_text = [];
    }
    $leading_block_text       = "";
    $leading_block_text_level = $levels_to_go[$i];
    $leading_block_text_line_number =
      $vertical_aligner_object->get_output_line_number();
    $leading_block_text_length_exceeded = 0;

    # this will contain the column number of the last character
    # of the closing side comment
    $leading_block_text_line_length =
      length($accumulating_text_for_block) +
      length( $rOpts->{'closing-side-comment-prefix'} ) +
      $leading_block_text_level * $rOpts_indent_columns + 3;
}

sub accumulate_block_text {
    my $i = shift;

    # accumulate leading text for -csc, ignoring any side comments
    if (   $accumulating_text_for_block
        && !$leading_block_text_length_exceeded
        && $types_to_go[$i] ne '#' )
    {

        my $added_length = length( $tokens_to_go[$i] );
        $added_length += 1 if $i == 0;
        my $new_line_length = $leading_block_text_line_length + $added_length;

        # we can add this text if we don't exceed some limits..
        if (

            # we must not have already exceeded the text length limit
            length($leading_block_text) <
            $rOpts_closing_side_comment_maximum_text

            # and either:
            # the new total line length must be below the line length limit
            # or the new length must be below the text length limit
            # (ie, we may allow one token to exceed the text length limit)
            && ( $new_line_length < $rOpts_maximum_line_length
                || length($leading_block_text) + $added_length <
                $rOpts_closing_side_comment_maximum_text )

            # UNLESS: we are adding a closing paren before the brace we seek.
            # This is an attempt to avoid situations where the ... to be
            # added are longer than the omitted right paren, as in:

            #   foreach my $item (@a_rather_long_variable_name_here) {
            #      &whatever;
            #   } ## end foreach my $item (@a_rather_long_variable_name_here...

            || (
                $tokens_to_go[$i] eq ')'
                && (
                    (
                           $i + 1 <= $max_index_to_go
                        && $block_type_to_go[ $i + 1 ] eq
                        $accumulating_text_for_block
                    )
                    || (   $i + 2 <= $max_index_to_go
                        && $block_type_to_go[ $i + 2 ] eq
                        $accumulating_text_for_block )
                )
            )
          )
        {

            # add an extra space at each newline
            if ( $i == 0 ) { $leading_block_text .= ' ' }

            # add the token text
            $leading_block_text .= $tokens_to_go[$i];
            $leading_block_text_line_length = $new_line_length;
        }

        # show that text was truncated if necessary
        elsif ( $types_to_go[$i] ne 'b' ) {
            $leading_block_text_length_exceeded = 1;
            $leading_block_text .= '...';
        }
    }
}

{
    my %is_if_elsif_else_unless_while_until_for_foreach;

    BEGIN {

        # These block types may have text between the keyword and opening
        # curly.  Note: 'else' does not, but must be included to allow trailing
        # if/elsif text to be appended.
        # patch for SWITCH/CASE: added 'case' and 'when'
        @_ = qw(if elsif else unless while until for foreach case when);
        @is_if_elsif_else_unless_while_until_for_foreach{@_} = (1) x scalar(@_);
    }

    sub accumulate_csc_text {

        # called once per output buffer when -csc is used. Accumulates
        # the text placed after certain closing block braces.
        # Defines and returns the following for this buffer:

        my $block_leading_text = "";    # the leading text of the last '}'
        my $rblock_leading_if_elsif_text;
        my $i_block_leading_text =
          -1;    # index of token owning block_leading_text
        my $block_line_count    = 100;    # how many lines the block spans
        my $terminal_type       = 'b';    # type of last nonblank token
        my $i_terminal          = 0;      # index of last nonblank token
        my $terminal_block_type = "";

        for my $i ( 0 .. $max_index_to_go ) {
            my $type       = $types_to_go[$i];
            my $block_type = $block_type_to_go[$i];
            my $token      = $tokens_to_go[$i];

            # remember last nonblank token type
            if ( $type ne '#' && $type ne 'b' ) {
                $terminal_type       = $type;
                $terminal_block_type = $block_type;
                $i_terminal          = $i;
            }

            my $type_sequence = $type_sequence_to_go[$i];
            if ( $block_type && $type_sequence ) {

                if ( $token eq '}' ) {

                    # restore any leading text saved when we entered this block
                    if ( defined( $block_leading_text{$type_sequence} ) ) {
                        ( $block_leading_text, $rblock_leading_if_elsif_text ) =
                          @{ $block_leading_text{$type_sequence} };
                        $i_block_leading_text = $i;
                        delete $block_leading_text{$type_sequence};
                        $rleading_block_if_elsif_text =
                          $rblock_leading_if_elsif_text;
                    }

                    # if we run into a '}' then we probably started accumulating
                    # at something like a trailing 'if' clause..no harm done.
                    if (   $accumulating_text_for_block
                        && $levels_to_go[$i] <= $leading_block_text_level )
                    {
                        my $lev = $levels_to_go[$i];
                        reset_block_text_accumulator();
                    }

                    if ( defined( $block_opening_line_number{$type_sequence} ) )
                    {
                        my $output_line_number =
                          $vertical_aligner_object->get_output_line_number();
                        $block_line_count =
                          $output_line_number -
                          $block_opening_line_number{$type_sequence} + 1;
                        delete $block_opening_line_number{$type_sequence};
                    }
                    else {

                        # Error: block opening line undefined for this line..
                        # This shouldn't be possible, but it is not a
                        # significant problem.
                    }
                }

                elsif ( $token eq '{' ) {

                    my $line_number =
                      $vertical_aligner_object->get_output_line_number();
                    $block_opening_line_number{$type_sequence} = $line_number;

                    if (   $accumulating_text_for_block
                        && $levels_to_go[$i] == $leading_block_text_level )
                    {

                        if ( $accumulating_text_for_block eq $block_type ) {

                            # save any leading text before we enter this block
                            $block_leading_text{$type_sequence} = [
                                $leading_block_text,
                                $rleading_block_if_elsif_text
                            ];
                            $block_opening_line_number{$type_sequence} =
                              $leading_block_text_line_number;
                            reset_block_text_accumulator();
                        }
                        else {

                            # shouldn't happen, but not a serious error.
                            # We were accumulating -csc text for block type
                            # $accumulating_text_for_block and unexpectedly
                            # encountered a '{' for block type $block_type.
                        }
                    }
                }
            }

            if (   $type eq 'k'
                && $csc_new_statement_ok
                && $is_if_elsif_else_unless_while_until_for_foreach{$token}
                && $token =~ /$closing_side_comment_list_pattern/o )
            {
                set_block_text_accumulator($i);
            }
            else {

                # note: ignoring type 'q' because of tricks being played
                # with 'q' for hanging side comments
                if ( $type ne 'b' && $type ne '#' && $type ne 'q' ) {
                    $csc_new_statement_ok =
                      ( $block_type || $type eq 'J' || $type eq ';' );
                }
                if (   $type eq ';'
                    && $accumulating_text_for_block
                    && $levels_to_go[$i] == $leading_block_text_level )
                {
                    reset_block_text_accumulator();
                }
                else {
                    accumulate_block_text($i);
                }
            }
        }

        # Treat an 'else' block specially by adding preceding 'if' and
        # 'elsif' text.  Otherwise, the 'end else' is not helpful,
        # especially for cuddled-else formatting.
        if ( $terminal_block_type =~ /^els/ && $rblock_leading_if_elsif_text ) {
            $block_leading_text =
              make_else_csc_text( $i_terminal, $terminal_block_type,
                $block_leading_text, $rblock_leading_if_elsif_text );
        }

        return ( $terminal_type, $i_terminal, $i_block_leading_text,
            $block_leading_text, $block_line_count );
    }
}

sub make_else_csc_text {

    # create additional -csc text for an 'else' and optionally 'elsif',
    # depending on the value of switch
    # $rOpts_closing_side_comment_else_flag:
    #
    #  = 0 add 'if' text to trailing else
    #  = 1 same as 0 plus:
    #      add 'if' to 'elsif's if can fit in line length
    #      add last 'elsif' to trailing else if can fit in one line
    #  = 2 same as 1 but do not check if exceed line length
    #
    # $rif_elsif_text = a reference to a list of all previous closing
    # side comments created for this if block
    #
    my ( $i_terminal, $block_type, $block_leading_text, $rif_elsif_text ) = @_;
    my $csc_text = $block_leading_text;

    if ( $block_type eq 'elsif' && $rOpts_closing_side_comment_else_flag == 0 )
    {
        return $csc_text;
    }

    my $count = @{$rif_elsif_text};
    return $csc_text unless ($count);

    my $if_text = '[ if' . $rif_elsif_text->[0];

    # always show the leading 'if' text on 'else'
    if ( $block_type eq 'else' ) {
        $csc_text .= $if_text;
    }

    # see if that's all
    if ( $rOpts_closing_side_comment_else_flag == 0 ) {
        return $csc_text;
    }

    my $last_elsif_text = "";
    if ( $count > 1 ) {
        $last_elsif_text = ' [elsif' . $rif_elsif_text->[ $count - 1 ];
        if ( $count > 2 ) { $last_elsif_text = ' [...' . $last_elsif_text; }
    }

    # tentatively append one more item
    my $saved_text = $csc_text;
    if ( $block_type eq 'else' ) {
        $csc_text .= $last_elsif_text;
    }
    else {
        $csc_text .= ' ' . $if_text;
    }

    # all done if no length checks requested
    if ( $rOpts_closing_side_comment_else_flag == 2 ) {
        return $csc_text;
    }

    # undo it if line length exceeded
    my $length =
      length($csc_text) +
      length($block_type) +
      length( $rOpts->{'closing-side-comment-prefix'} ) +
      $levels_to_go[$i_terminal] * $rOpts_indent_columns + 3;
    if ( $length > $rOpts_maximum_line_length ) {
        $csc_text = $saved_text;
    }
    return $csc_text;
}

sub add_closing_side_comment {

    # add closing side comments after closing block braces if -csc used
    my $cscw_block_comment;

    #---------------------------------------------------------------
    # Step 1: loop through all tokens of this line to accumulate
    # the text needed to create the closing side comments. Also see
    # how the line ends.
    #---------------------------------------------------------------

    my ( $terminal_type, $i_terminal, $i_block_leading_text,
        $block_leading_text, $block_line_count )
      = accumulate_csc_text();

    #---------------------------------------------------------------
    # Step 2: make the closing side comment if this ends a block
    #---------------------------------------------------------------
    my $have_side_comment = $i_terminal != $max_index_to_go;

    # if this line might end in a block closure..
    if (
        $terminal_type eq '}'

        # ..and either
        && (

            # the block is long enough
            ( $block_line_count >= $rOpts->{'closing-side-comment-interval'} )

            # or there is an existing comment to check
            || (   $have_side_comment
                && $rOpts->{'closing-side-comment-warnings'} )
        )

        # .. and if this is one of the types of interest
        && $block_type_to_go[$i_terminal] =~
        /$closing_side_comment_list_pattern/o

        # .. but not an anonymous sub
        # These are not normally of interest, and their closing braces are
        # often followed by commas or semicolons anyway.  This also avoids
        # possible erratic output due to line numbering inconsistencies
        # in the cases where their closing braces terminate a line.
        && $block_type_to_go[$i_terminal] ne 'sub'

        # ..and the corresponding opening brace must is not in this batch
        # (because we do not need to tag one-line blocks, although this
        # should also be caught with a positive -csci value)
        && $mate_index_to_go[$i_terminal] < 0

        # ..and either
        && (

            # this is the last token (line doesnt have a side comment)
            !$have_side_comment

            # or the old side comment is a closing side comment
            || $tokens_to_go[$max_index_to_go] =~
            /$closing_side_comment_prefix_pattern/o
        )
      )
    {

        # then make the closing side comment text
        my $token =
"$rOpts->{'closing-side-comment-prefix'} $block_type_to_go[$i_terminal]";

        # append any extra descriptive text collected above
        if ( $i_block_leading_text == $i_terminal ) {
            $token .= $block_leading_text;
        }
        $token =~ s/\s*$//;    # trim any trailing whitespace

        # handle case of existing closing side comment
        if ($have_side_comment) {

            # warn if requested and tokens differ significantly
            if ( $rOpts->{'closing-side-comment-warnings'} ) {
                my $old_csc = $tokens_to_go[$max_index_to_go];
                my $new_csc = $token;
                $new_csc =~ s/(\.\.\.)\s*$//;    # trim trailing '...'
                my $new_trailing_dots = $1;
                $old_csc =~ s/\.\.\.\s*$//;
                $new_csc =~ s/\s+//g;            # trim all whitespace
                $old_csc =~ s/\s+//g;

                # Patch to handle multiple closing side comments at
                # else and elsif's.  These have become too complicated
                # to check, so if we see an indication of
                # '[ if' or '[ # elsif', then assume they were made
                # by perltidy.
                if ( $block_type_to_go[$i_terminal] eq 'else' ) {
                    if ( $old_csc =~ /\[\s*elsif/ ) { $old_csc = $new_csc }
                }
                elsif ( $block_type_to_go[$i_terminal] eq 'elsif' ) {
                    if ( $old_csc =~ /\[\s*if/ ) { $old_csc = $new_csc }
                }

                # if old comment is contained in new comment,
                # only compare the common part.
                if ( length($new_csc) > length($old_csc) ) {
                    $new_csc = substr( $new_csc, 0, length($old_csc) );
                }

                # if the new comment is shorter and has been limited,
                # only compare the common part.
                if ( length($new_csc) < length($old_csc) && $new_trailing_dots )
                {
                    $old_csc = substr( $old_csc, 0, length($new_csc) );
                }

                # any remaining difference?
                if ( $new_csc ne $old_csc ) {

                    # just leave the old comment if we are below the threshold
                    # for creating side comments
                    if ( $block_line_count <
                        $rOpts->{'closing-side-comment-interval'} )
                    {
                        $token = undef;
                    }

                    # otherwise we'll make a note of it
                    else {

                        warning(
"perltidy -cscw replaced: $tokens_to_go[$max_index_to_go]\n"
                        );

                     # save the old side comment in a new trailing block comment
                        my ( $day, $month, $year ) = (localtime)[ 3, 4, 5 ];
                        $year  += 1900;
                        $month += 1;
                        $cscw_block_comment =
"## perltidy -cscw $year-$month-$day: $tokens_to_go[$max_index_to_go]";
                    }
                }
                else {

                    # No differences.. we can safely delete old comment if we
                    # are below the threshold
                    if ( $block_line_count <
                        $rOpts->{'closing-side-comment-interval'} )
                    {
                        $token = undef;
                        unstore_token_to_go()
                          if ( $types_to_go[$max_index_to_go] eq '#' );
                        unstore_token_to_go()
                          if ( $types_to_go[$max_index_to_go] eq 'b' );
                    }
                }
            }

            # switch to the new csc (unless we deleted it!)
            $tokens_to_go[$max_index_to_go] = $token if $token;
        }

        # handle case of NO existing closing side comment
        else {

            # insert the new side comment into the output token stream
            my $type          = '#';
            my $block_type    = '';
            my $type_sequence = '';
            my $container_environment =
              $container_environment_to_go[$max_index_to_go];
            my $level                = $levels_to_go[$max_index_to_go];
            my $slevel               = $nesting_depth_to_go[$max_index_to_go];
            my $no_internal_newlines = 0;

            my $nesting_blocks     = $nesting_blocks_to_go[$max_index_to_go];
            my $ci_level           = $ci_levels_to_go[$max_index_to_go];
            my $in_continued_quote = 0;

            # first insert a blank token
            insert_new_token_to_go( ' ', 'b', $slevel, $no_internal_newlines );

            # then the side comment
            insert_new_token_to_go( $token, $type, $slevel,
                $no_internal_newlines );
        }
    }
    return $cscw_block_comment;
}

sub previous_nonblank_token {
    my ($i)  = @_;
    my $name = "";
    my $im   = $i - 1;
    return "" if ( $im < 0 );
    if ( $types_to_go[$im] eq 'b' ) { $im--; }
    return "" if ( $im < 0 );
    $name = $tokens_to_go[$im];

    # prepend any sub name to an isolated -> to avoid unwanted alignments
    # [test case is test8/penco.pl]
    if ( $name eq '->' ) {
        $im--;
        if ( $im >= 0 && $types_to_go[$im] ne 'b' ) {
            $name = $tokens_to_go[$im] . $name;
        }
    }
    return $name;
}

sub send_lines_to_vertical_aligner {

    my ( $ri_first, $ri_last, $do_not_pad ) = @_;

    my $rindentation_list = [0];    # ref to indentations for each line

    # define the array @matching_token_to_go for the output tokens
    # which will be non-blank for each special token (such as =>)
    # for which alignment is required.
    set_vertical_alignment_markers( $ri_first, $ri_last );

    # flush if necessary to avoid unwanted alignment
    my $must_flush = 0;
    if ( @$ri_first > 1 ) {

        # flush before a long if statement
        if ( $types_to_go[0] eq 'k' && $tokens_to_go[0] =~ /^(if|unless)$/ ) {
            $must_flush = 1;
        }
    }
    if ($must_flush) {
        Perl::Tidy::VerticalAligner::flush();
    }

    set_logical_padding( $ri_first, $ri_last );

    # loop to prepare each line for shipment
    my $n_last_line = @$ri_first - 1;
    my $in_comma_list;
    for my $n ( 0 .. $n_last_line ) {
        my $ibeg = $$ri_first[$n];
        my $iend = $$ri_last[$n];

        my ( $rtokens, $rfields, $rpatterns ) =
          make_alignment_patterns( $ibeg, $iend );

        my ( $indentation, $lev, $level_end, $terminal_type,
            $is_semicolon_terminated, $is_outdented_line )
          = set_adjusted_indentation( $ibeg, $iend, $rfields, $rpatterns,
            $ri_first, $ri_last, $rindentation_list );

        # we will allow outdenting of long lines..
        my $outdent_long_lines = (

            # which are long quotes, if allowed
            ( $types_to_go[$ibeg] eq 'Q' && $rOpts->{'outdent-long-quotes'} )

            # which are long block comments, if allowed
              || (
                   $types_to_go[$ibeg] eq '#'
                && $rOpts->{'outdent-long-comments'}

                # but not if this is a static block comment
                && !$is_static_block_comment
              )
        );

        my $level_jump =
          $nesting_depth_to_go[ $iend + 1 ] - $nesting_depth_to_go[$ibeg];

        my $rvertical_tightness_flags =
          set_vertical_tightness_flags( $n, $n_last_line, $ibeg, $iend,
            $ri_first, $ri_last );

        # flush an outdented line to avoid any unwanted vertical alignment
        Perl::Tidy::VerticalAligner::flush() if ($is_outdented_line);

        my $is_terminal_ternary = 0;
        if (   $tokens_to_go[$ibeg] eq ':'
            || $n > 0 && $tokens_to_go[ $$ri_last[ $n - 1 ] ] eq ':' )
        {
            if (   ( $terminal_type eq ';' && $level_end <= $lev )
                || ( $level_end < $lev ) )
            {
                $is_terminal_ternary = 1;
            }
        }

        # send this new line down the pipe
        my $forced_breakpoint = $forced_breakpoint_to_go[$iend];
        Perl::Tidy::VerticalAligner::append_line(
            $lev,
            $level_end,
            $indentation,
            $rfields,
            $rtokens,
            $rpatterns,
            $forced_breakpoint_to_go[$iend] || $in_comma_list,
            $outdent_long_lines,
            $is_terminal_ternary,
            $is_semicolon_terminated,
            $do_not_pad,
            $rvertical_tightness_flags,
            $level_jump,
        );
        $in_comma_list =
          $tokens_to_go[$iend] eq ',' && $forced_breakpoint_to_go[$iend];

        # flush an outdented line to avoid any unwanted vertical alignment
        Perl::Tidy::VerticalAligner::flush() if ($is_outdented_line);

        $do_not_pad = 0;

    }    # end of loop to output each line

    # remember indentation of lines containing opening containers for
    # later use by sub set_adjusted_indentation
    save_opening_indentation( $ri_first, $ri_last, $rindentation_list );
}

{        # begin make_alignment_patterns

    my %block_type_map;
    my %keyword_map;

    BEGIN {

        # map related block names into a common name to
        # allow alignment
        %block_type_map = (
            unless  => 'if',
            else    => 'if',
            elsif   => 'if',
            when    => 'if',
            default => 'if',
            case    => 'if',
            sort    => 'map',
            grep    => 'map',
        );

        # map certain keywords to the same 'if' class to align
        # long if/elsif sequences. [elsif.pl]
        %keyword_map = (
            unless  => 'if',
            else    => 'if',
            elsif   => 'if',
            when    => 'given',
            default => 'given',
            case    => 'switch',

            # treat an 'undef' similar to numbers and quotes
            'undef' => 'Q',
        );
    }

    sub make_alignment_patterns {

        # Here we do some important preliminary work for the
        # vertical aligner.  We create three arrays for one
        # output line. These arrays contain strings that can
        # be tested by the vertical aligner to see if
        # consecutive lines can be aligned vertically.
        #
        # The three arrays are indexed on the vertical
        # alignment fields and are:
        # @tokens - a list of any vertical alignment tokens for this line.
        #   These are tokens, such as '=' '&&' '#' etc which
        #   we want to might align vertically.  These are
        #   decorated with various information such as
        #   nesting depth to prevent unwanted vertical
        #   alignment matches.
        # @fields - the actual text of the line between the vertical alignment
        #   tokens.
        # @patterns - a modified list of token types, one for each alignment
        #   field.  These should normally each match before alignment is
        #   allowed, even when the alignment tokens match.
        my ( $ibeg, $iend ) = @_;
        my @tokens   = ();
        my @fields   = ();
        my @patterns = ();
        my $i_start  = $ibeg;
        my $i;

        my $depth                 = 0;
        my @container_name        = ("");
        my @multiple_comma_arrows = (undef);

        my $j = 0;    # field index

        $patterns[0] = "";
        for $i ( $ibeg .. $iend ) {

            # Keep track of containers balanced on this line only.
            # These are used below to prevent unwanted cross-line alignments.
            # Unbalanced containers already avoid aligning across
            # container boundaries.
            if ( $tokens_to_go[$i] eq '(' ) {

                # if container is balanced on this line...
                my $i_mate = $mate_index_to_go[$i];
                if ( $i_mate > $i && $i_mate <= $iend ) {
                    $depth++;
                    my $seqno = $type_sequence_to_go[$i];
                    my $count = comma_arrow_count($seqno);
                    $multiple_comma_arrows[$depth] = $count && $count > 1;

                    # Append the previous token name to make the container name
                    # more unique.  This name will also be given to any commas
                    # within this container, and it helps avoid undesirable
                    # alignments of different types of containers.
                    my $name = previous_nonblank_token($i);
                    $name =~ s/^->//;
                    $container_name[$depth] = "+" . $name;

                    # Make the container name even more unique if necessary.
                    # If we are not vertically aligning this opening paren,
                    # append a character count to avoid bad alignment because
                    # it usually looks bad to align commas within continers
                    # for which the opening parens do not align.  Here
                    # is an example very BAD alignment of commas (because
                    # the atan2 functions are not all aligned):
                    #    $XY =
                    #      $X * $RTYSQP1 * atan2( $X, $RTYSQP1 ) +
                    #      $Y * $RTXSQP1 * atan2( $Y, $RTXSQP1 ) -
                    #      $X * atan2( $X,            1 ) -
                    #      $Y * atan2( $Y,            1 );
                    #
                    # On the other hand, it is usually okay to align commas if
                    # opening parens align, such as:
                    #    glVertex3d( $cx + $s * $xs, $cy,            $z );
                    #    glVertex3d( $cx,            $cy + $s * $ys, $z );
                    #    glVertex3d( $cx - $s * $xs, $cy,            $z );
                    #    glVertex3d( $cx,            $cy - $s * $ys, $z );
                    #
                    # To distinguish between these situations, we will
                    # append the length of the line from the previous matching
                    # token, or beginning of line, to the function name.  This
                    # will allow the vertical aligner to reject undesirable
                    # matches.

                    # if we are not aligning on this paren...
                    if ( $matching_token_to_go[$i] eq '' ) {

                        # Sum length from previous alignment, or start of line.
                        # Note that we have to sum token lengths here because
                        # padding has been done and so array $lengths_to_go
                        # is now wrong.
                        my $len =
                          length(
                            join( '', @tokens_to_go[ $i_start .. $i - 1 ] ) );
                        $len += leading_spaces_to_go($i_start)
                          if ( $i_start == $ibeg );

                        # tack length onto the container name to make unique
                        $container_name[$depth] .= "-" . $len;
                    }
                }
            }
            elsif ( $tokens_to_go[$i] eq ')' ) {
                $depth-- if $depth > 0;
            }

            # if we find a new synchronization token, we are done with
            # a field
            if ( $i > $i_start && $matching_token_to_go[$i] ne '' ) {

                my $tok = my $raw_tok = $matching_token_to_go[$i];

                # make separators in different nesting depths unique
                # by appending the nesting depth digit.
                if ( $raw_tok ne '#' ) {
                    $tok .= "$nesting_depth_to_go[$i]";
                }

                # also decorate commas with any container name to avoid
                # unwanted cross-line alignments.
                if ( $raw_tok eq ',' || $raw_tok eq '=>' ) {
                    if ( $container_name[$depth] ) {
                        $tok .= $container_name[$depth];
                    }
                }

                # Patch to avoid aligning leading and trailing if, unless.
                # Mark trailing if, unless statements with container names.
                # This makes them different from leading if, unless which
                # are not so marked at present.  If we ever need to name
                # them too, we could use ci to distinguish them.
                # Example problem to avoid:
                #    return ( 2, "DBERROR" )
                #      if ( $retval == 2 );
                #    if   ( scalar @_ ) {
                #        my ( $a, $b, $c, $d, $e, $f ) = @_;
                #    }
                if ( $raw_tok eq '(' ) {
                    my $ci = $ci_levels_to_go[$ibeg];
                    if (   $container_name[$depth] =~ /^\+(if|unless)/
                        && $ci )
                    {
                        $tok .= $container_name[$depth];
                    }
                }

                # Decorate block braces with block types to avoid
                # unwanted alignments such as the following:
                # foreach ( @{$routput_array} ) { $fh->print($_) }
                # eval                          { $fh->close() };
                if ( $raw_tok eq '{' && $block_type_to_go[$i] ) {
                    my $block_type = $block_type_to_go[$i];

                    # map certain related block types to allow
                    # else blocks to align
                    $block_type = $block_type_map{$block_type}
                      if ( defined( $block_type_map{$block_type} ) );

                    # remove sub names to allow one-line sub braces to align
                    # regardless of name
                    if ( $block_type =~ /^sub / ) { $block_type = 'sub' }

                    # allow all control-type blocks to align
                    if ( $block_type =~ /^[A-Z]+$/ ) { $block_type = 'BEGIN' }

                    $tok .= $block_type;
                }

                # concatenate the text of the consecutive tokens to form
                # the field
                push( @fields,
                    join( '', @tokens_to_go[ $i_start .. $i - 1 ] ) );

                # store the alignment token for this field
                push( @tokens, $tok );

                # get ready for the next batch
                $i_start = $i;
                $j++;
                $patterns[$j] = "";
            }

            # continue accumulating tokens
            # handle non-keywords..
            if ( $types_to_go[$i] ne 'k' ) {
                my $type = $types_to_go[$i];

                # Mark most things before arrows as a quote to
                # get them to line up. Testfile: mixed.pl.
                if ( ( $i < $iend - 1 ) && ( $type =~ /^[wnC]$/ ) ) {
                    my $next_type = $types_to_go[ $i + 1 ];
                    my $i_next_nonblank =
                      ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );

                    if ( $types_to_go[$i_next_nonblank] eq '=>' ) {
                        $type = 'Q';

                        # Patch to ignore leading minus before words,
                        # by changing pattern 'mQ' into just 'Q',
                        # so that we can align things like this:
                        #  Button   => "Print letter \"~$_\"",
                        #  -command => [ sub { print "$_[0]\n" }, $_ ],
                        if ( $patterns[$j] eq 'm' ) { $patterns[$j] = "" }
                    }
                }

                # patch to make numbers and quotes align
                if ( $type eq 'n' ) { $type = 'Q' }

                # patch to ignore any ! in patterns
                if ( $type eq '!' ) { $type = '' }

                $patterns[$j] .= $type;
            }

            # for keywords we have to use the actual text
            else {

                my $tok = $tokens_to_go[$i];

                # but map certain keywords to a common string to allow
                # alignment.
                $tok = $keyword_map{$tok}
                  if ( defined( $keyword_map{$tok} ) );
                $patterns[$j] .= $tok;
            }
        }

        # done with this line .. join text of tokens to make the last field
        push( @fields, join( '', @tokens_to_go[ $i_start .. $iend ] ) );
        return ( \@tokens, \@fields, \@patterns );
    }

}    # end make_alignment_patterns

{    # begin unmatched_indexes

    # closure to keep track of unbalanced containers.
    # arrays shared by the routines in this block:
    my @unmatched_opening_indexes_in_this_batch;
    my @unmatched_closing_indexes_in_this_batch;
    my %comma_arrow_count;

    sub is_unbalanced_batch {
        @unmatched_opening_indexes_in_this_batch +
          @unmatched_closing_indexes_in_this_batch;
    }

    sub comma_arrow_count {
        my $seqno = $_[0];
        return $comma_arrow_count{$seqno};
    }

    sub match_opening_and_closing_tokens {

        # Match up indexes of opening and closing braces, etc, in this batch.
        # This has to be done after all tokens are stored because unstoring
        # of tokens would otherwise cause trouble.

        @unmatched_opening_indexes_in_this_batch = ();
        @unmatched_closing_indexes_in_this_batch = ();
        %comma_arrow_count                       = ();

        my ( $i, $i_mate, $token );
        foreach $i ( 0 .. $max_index_to_go ) {
            if ( $type_sequence_to_go[$i] ) {
                $token = $tokens_to_go[$i];
                if ( $token =~ /^[\(\[\{\?]$/ ) {
                    push @unmatched_opening_indexes_in_this_batch, $i;
                }
                elsif ( $token =~ /^[\)\]\}\:]$/ ) {

                    $i_mate = pop @unmatched_opening_indexes_in_this_batch;
                    if ( defined($i_mate) && $i_mate >= 0 ) {
                        if ( $type_sequence_to_go[$i_mate] ==
                            $type_sequence_to_go[$i] )
                        {
                            $mate_index_to_go[$i]      = $i_mate;
                            $mate_index_to_go[$i_mate] = $i;
                        }
                        else {
                            push @unmatched_opening_indexes_in_this_batch,
                              $i_mate;
                            push @unmatched_closing_indexes_in_this_batch, $i;
                        }
                    }
                    else {
                        push @unmatched_closing_indexes_in_this_batch, $i;
                    }
                }
            }
            elsif ( $tokens_to_go[$i] eq '=>' ) {
                if (@unmatched_opening_indexes_in_this_batch) {
                    my $j     = $unmatched_opening_indexes_in_this_batch[-1];
                    my $seqno = $type_sequence_to_go[$j];
                    $comma_arrow_count{$seqno}++;
                }
            }
        }
    }

    sub save_opening_indentation {

        # This should be called after each batch of tokens is output. It
        # saves indentations of lines of all unmatched opening tokens.
        # These will be used by sub get_opening_indentation.

        my ( $ri_first, $ri_last, $rindentation_list ) = @_;

        # we no longer need indentations of any saved indentations which
        # are unmatched closing tokens in this batch, because we will
        # never encounter them again.  So we can delete them to keep
        # the hash size down.
        foreach (@unmatched_closing_indexes_in_this_batch) {
            my $seqno = $type_sequence_to_go[$_];
            delete $saved_opening_indentation{$seqno};
        }

        # we need to save indentations of any unmatched opening tokens
        # in this batch because we may need them in a subsequent batch.
        foreach (@unmatched_opening_indexes_in_this_batch) {
            my $seqno = $type_sequence_to_go[$_];
            $saved_opening_indentation{$seqno} = [
                lookup_opening_indentation(
                    $_, $ri_first, $ri_last, $rindentation_list
                )
            ];
        }
    }
}    # end unmatched_indexes

sub get_opening_indentation {

    # get the indentation of the line which output the opening token
    # corresponding to a given closing token in the current output batch.
    #
    # given:
    # $i_closing - index in this line of a closing token ')' '}' or ']'
    #
    # $ri_first - reference to list of the first index $i for each output
    #               line in this batch
    # $ri_last - reference to list of the last index $i for each output line
    #              in this batch
    # $rindentation_list - reference to a list containing the indentation
    #            used for each line.
    #
    # return:
    #   -the indentation of the line which contained the opening token
    #    which matches the token at index $i_opening
    #   -and its offset (number of columns) from the start of the line
    #
    my ( $i_closing, $ri_first, $ri_last, $rindentation_list ) = @_;

    # first, see if the opening token is in the current batch
    my $i_opening = $mate_index_to_go[$i_closing];
    my ( $indent, $offset, $is_leading, $exists );
    $exists = 1;
    if ( $i_opening >= 0 ) {

        # it is..look up the indentation
        ( $indent, $offset, $is_leading ) =
          lookup_opening_indentation( $i_opening, $ri_first, $ri_last,
            $rindentation_list );
    }

    # if not, it should have been stored in the hash by a previous batch
    else {
        my $seqno = $type_sequence_to_go[$i_closing];
        if ($seqno) {
            if ( $saved_opening_indentation{$seqno} ) {
                ( $indent, $offset, $is_leading ) =
                  @{ $saved_opening_indentation{$seqno} };
            }

            # some kind of serious error
            # (example is badfile.t)
            else {
                $indent     = 0;
                $offset     = 0;
                $is_leading = 0;
                $exists     = 0;
            }
        }

        # if no sequence number it must be an unbalanced container
        else {
            $indent     = 0;
            $offset     = 0;
            $is_leading = 0;
            $exists     = 0;
        }
    }
    return ( $indent, $offset, $is_leading, $exists );
}

sub lookup_opening_indentation {

    # get the indentation of the line in the current output batch
    # which output a selected opening token
    #
    # given:
    #   $i_opening - index of an opening token in the current output batch
    #                whose line indentation we need
    #   $ri_first - reference to list of the first index $i for each output
    #               line in this batch
    #   $ri_last - reference to list of the last index $i for each output line
    #              in this batch
    #   $rindentation_list - reference to a list containing the indentation
    #            used for each line.  (NOTE: the first slot in
    #            this list is the last returned line number, and this is
    #            followed by the list of indentations).
    #
    # return
    #   -the indentation of the line which contained token $i_opening
    #   -and its offset (number of columns) from the start of the line

    my ( $i_opening, $ri_start, $ri_last, $rindentation_list ) = @_;

    my $nline = $rindentation_list->[0];    # line number of previous lookup

    # reset line location if necessary
    $nline = 0 if ( $i_opening < $ri_start->[$nline] );

    # find the correct line
    unless ( $i_opening > $ri_last->[-1] ) {
        while ( $i_opening > $ri_last->[$nline] ) { $nline++; }
    }

    # error - token index is out of bounds - shouldn't happen
    else {
        warning(
"non-fatal program bug in lookup_opening_indentation - index out of range\n"
        );
        report_definite_bug();
        $nline = $#{$ri_last};
    }

    $rindentation_list->[0] =
      $nline;    # save line number to start looking next call
    my $ibeg       = $ri_start->[$nline];
    my $offset     = token_sequence_length( $ibeg, $i_opening ) - 1;
    my $is_leading = ( $ibeg == $i_opening );
    return ( $rindentation_list->[ $nline + 1 ], $offset, $is_leading );
}

{
    my %is_if_elsif_else_unless_while_until_for_foreach;

    BEGIN {

        # These block types may have text between the keyword and opening
        # curly.  Note: 'else' does not, but must be included to allow trailing
        # if/elsif text to be appended.
        # patch for SWITCH/CASE: added 'case' and 'when'
        @_ = qw(if elsif else unless while until for foreach case when);
        @is_if_elsif_else_unless_while_until_for_foreach{@_} = (1) x scalar(@_);
    }

    sub set_adjusted_indentation {

        # This routine has the final say regarding the actual indentation of
        # a line.  It starts with the basic indentation which has been
        # defined for the leading token, and then takes into account any
        # options that the user has set regarding special indenting and
        # outdenting.

        my ( $ibeg, $iend, $rfields, $rpatterns, $ri_first, $ri_last,
            $rindentation_list )
          = @_;

        # we need to know the last token of this line
        my ( $terminal_type, $i_terminal ) =
          terminal_type( \@types_to_go, \@block_type_to_go, $ibeg, $iend );

        my $is_outdented_line = 0;

        my $is_semicolon_terminated = $terminal_type eq ';'
          && $nesting_depth_to_go[$iend] < $nesting_depth_to_go[$ibeg];

        ##########################################################
        # Section 1: set a flag and a default indentation
        #
        # Most lines are indented according to the initial token.
        # But it is common to outdent to the level just after the
        # terminal token in certain cases...
        # adjust_indentation flag:
        #       0 - do not adjust
        #       1 - outdent
        #       2 - vertically align with opening token
        #       3 - indent
        ##########################################################
        my $adjust_indentation         = 0;
        my $default_adjust_indentation = $adjust_indentation;

        my (
            $opening_indentation, $opening_offset,
            $is_leading,          $opening_exists
        );

        # if we are at a closing token of some type..
        if ( $types_to_go[$ibeg] =~ /^[\)\}\]]$/ ) {

            # get the indentation of the line containing the corresponding
            # opening token
            (
                $opening_indentation, $opening_offset,
                $is_leading,          $opening_exists
              )
              = get_opening_indentation( $ibeg, $ri_first, $ri_last,
                $rindentation_list );

            # First set the default behavior:
            # default behavior is to outdent closing lines
            # of the form:   ");  };  ];  )->xxx;"
            if (
                $is_semicolon_terminated

                # and 'cuddled parens' of the form:   ")->pack("
                || (
                       $terminal_type eq '('
                    && $types_to_go[$ibeg] eq ')'
                    && ( $nesting_depth_to_go[$iend] + 1 ==
                        $nesting_depth_to_go[$ibeg] )
                )
              )
            {
                $adjust_indentation = 1;
            }

            # TESTING: outdent something like '),'
            if (
                $terminal_type eq ','

                # allow just one character before the comma
                && $i_terminal == $ibeg + 1

                # requre LIST environment; otherwise, we may outdent too much --
                # this can happen in calls without parentheses (overload.t);
                && $container_environment_to_go[$i_terminal] eq 'LIST'
              )
            {
                $adjust_indentation = 1;
            }

            # undo continuation indentation of a terminal closing token if
            # it is the last token before a level decrease.  This will allow
            # a closing token to line up with its opening counterpart, and
            # avoids a indentation jump larger than 1 level.
            if (   $types_to_go[$i_terminal] =~ /^[\}\]\)R]$/
                && $i_terminal == $ibeg )
            {
                my $ci        = $ci_levels_to_go[$ibeg];
                my $lev       = $levels_to_go[$ibeg];
                my $next_type = $types_to_go[ $ibeg + 1 ];
                my $i_next_nonblank =
                  ( ( $next_type eq 'b' ) ? $ibeg + 2 : $ibeg + 1 );
                if (   $i_next_nonblank <= $max_index_to_go
                    && $levels_to_go[$i_next_nonblank] < $lev )
                {
                    $adjust_indentation = 1;
                }
            }

            $default_adjust_indentation = $adjust_indentation;

            # Now modify default behavior according to user request:
            # handle option to indent non-blocks of the form );  };  ];
            # But don't do special indentation to something like ')->pack('
            if ( !$block_type_to_go[$ibeg] ) {
                my $cti = $closing_token_indentation{ $tokens_to_go[$ibeg] };
                if ( $cti == 1 ) {
                    if (   $i_terminal <= $ibeg + 1
                        || $is_semicolon_terminated )
                    {
                        $adjust_indentation = 2;
                    }
                    else {
                        $adjust_indentation = 0;
                    }
                }
                elsif ( $cti == 2 ) {
                    if ($is_semicolon_terminated) {
                        $adjust_indentation = 3;
                    }
                    else {
                        $adjust_indentation = 0;
                    }
                }
                elsif ( $cti == 3 ) {
                    $adjust_indentation = 3;
                }
            }

            # handle option to indent blocks
            else {
                if (
                    $rOpts->{'indent-closing-brace'}
                    && (
                        $i_terminal == $ibeg    #  isolated terminal '}'
                        || $is_semicolon_terminated
                    )
                  )                             #  } xxxx ;
                {
                    $adjust_indentation = 3;
                }
            }
        }

        # if at ');', '};', '>;', and '];' of a terminal qw quote
        elsif ($$rpatterns[0] =~ /^qb*;$/
            && $$rfields[0] =~ /^([\)\}\]\>]);$/ )
        {
            if ( $closing_token_indentation{$1} == 0 ) {
                $adjust_indentation = 1;
            }
            else {
                $adjust_indentation = 3;
            }
        }

        # if line begins with a ':', align it with any
        # previous line leading with corresponding ?
        elsif ( $types_to_go[$ibeg] eq ':' ) {
            (
                $opening_indentation, $opening_offset,
                $is_leading,          $opening_exists
              )
              = get_opening_indentation( $ibeg, $ri_first, $ri_last,
                $rindentation_list );
            if ($is_leading) { $adjust_indentation = 2; }
        }

        ##########################################################
        # Section 2: set indentation according to flag set above
        #
        # Select the indentation object to define leading
        # whitespace.  If we are outdenting something like '} } );'
        # then we want to use one level below the last token
        # ($i_terminal) in order to get it to fully outdent through
        # all levels.
        ##########################################################
        my $indentation;
        my $lev;
        my $level_end = $levels_to_go[$iend];

        if ( $adjust_indentation == 0 ) {
            $indentation = $leading_spaces_to_go[$ibeg];
            $lev         = $levels_to_go[$ibeg];
        }
        elsif ( $adjust_indentation == 1 ) {
            $indentation = $reduced_spaces_to_go[$i_terminal];
            $lev         = $levels_to_go[$i_terminal];
        }

        # handle indented closing token which aligns with opening token
        elsif ( $adjust_indentation == 2 ) {

            # handle option to align closing token with opening token
            $lev = $levels_to_go[$ibeg];

            # calculate spaces needed to align with opening token
            my $space_count =
              get_SPACES($opening_indentation) + $opening_offset;

            # Indent less than the previous line.
            #
            # Problem: For -lp we don't exactly know what it was if there
            # were recoverable spaces sent to the aligner.  A good solution
            # would be to force a flush of the vertical alignment buffer, so
            # that we would know.  For now, this rule is used for -lp:
            #
            # When the last line did not start with a closing token we will
            # be optimistic that the aligner will recover everything wanted.
            #
            # This rule will prevent us from breaking a hierarchy of closing
            # tokens, and in a worst case will leave a closing paren too far
            # indented, but this is better than frequently leaving it not
            # indented enough.
            my $last_spaces = get_SPACES($last_indentation_written);
            if ( $last_leading_token !~ /^[\}\]\)]$/ ) {
                $last_spaces +=
                  get_RECOVERABLE_SPACES($last_indentation_written);
            }

            # reset the indentation to the new space count if it works
            # only options are all or none: nothing in-between looks good
            $lev = $levels_to_go[$ibeg];
            if ( $space_count < $last_spaces ) {
                if ($rOpts_line_up_parentheses) {
                    my $lev = $levels_to_go[$ibeg];
                    $indentation =
                      new_lp_indentation_item( $space_count, $lev, 0, 0, 0 );
                }
                else {
                    $indentation = $space_count;
                }
            }

            # revert to default if it doesnt work
            else {
                $space_count = leading_spaces_to_go($ibeg);
                if ( $default_adjust_indentation == 0 ) {
                    $indentation = $leading_spaces_to_go[$ibeg];
                }
                elsif ( $default_adjust_indentation == 1 ) {
                    $indentation = $reduced_spaces_to_go[$i_terminal];
                    $lev         = $levels_to_go[$i_terminal];
                }
            }
        }

        # Full indentaion of closing tokens (-icb and -icp or -cti=2)
        else {

            # handle -icb (indented closing code block braces)
            # Updated method for indented block braces: indent one full level if
            # there is no continuation indentation.  This will occur for major
            # structures such as sub, if, else, but not for things like map
            # blocks.
            #
            # Note: only code blocks without continuation indentation are
            # handled here (if, else, unless, ..). In the following snippet,
            # the terminal brace of the sort block will have continuation
            # indentation as shown so it will not be handled by the coding
            # here.  We would have to undo the continuation indentation to do
            # this, but it probably looks ok as is.  This is a possible future
            # update for semicolon terminated lines.
            #
            #     if ($sortby eq 'date' or $sortby eq 'size') {
            #         @files = sort {
            #             $file_data{$a}{$sortby} <=> $file_data{$b}{$sortby}
            #                 or $a cmp $b
            #                 } @files;
            #         }
            #
            if (   $block_type_to_go[$ibeg]
                && $ci_levels_to_go[$i_terminal] == 0 )
            {
                my $spaces = get_SPACES( $leading_spaces_to_go[$i_terminal] );
                $indentation = $spaces + $rOpts_indent_columns;

                # NOTE: for -lp we could create a new indentation object, but
                # there is probably no need to do it
            }

            # handle -icp and any -icb block braces which fall through above
            # test such as the 'sort' block mentioned above.
            else {

                # There are currently two ways to handle -icp...
                # One way is to use the indentation of the previous line:
                # $indentation = $last_indentation_written;

                # The other way is to use the indentation that the previous line
                # would have had if it hadn't been adjusted:
                $indentation = $last_unadjusted_indentation;

                # Current method: use the minimum of the two. This avoids
                # inconsistent indentation.
                if ( get_SPACES($last_indentation_written) <
                    get_SPACES($indentation) )
                {
                    $indentation = $last_indentation_written;
                }
            }

            # use previous indentation but use own level
            # to cause list to be flushed properly
            $lev = $levels_to_go[$ibeg];
        }

        # remember indentation except for multi-line quotes, which get
        # no indentation
        unless ( $ibeg == 0 && $starting_in_quote ) {
            $last_indentation_written    = $indentation;
            $last_unadjusted_indentation = $leading_spaces_to_go[$ibeg];
            $last_leading_token          = $tokens_to_go[$ibeg];
        }

        # be sure lines with leading closing tokens are not outdented more
        # than the line which contained the corresponding opening token.

        #############################################################
        # updated per bug report in alex_bug.pl: we must not
        # mess with the indentation of closing logical braces so
        # we must treat something like '} else {' as if it were
        # an isolated brace my $is_isolated_block_brace = (
        # $iend == $ibeg ) && $block_type_to_go[$ibeg];
        #############################################################
        my $is_isolated_block_brace = $block_type_to_go[$ibeg]
          && ( $iend == $ibeg
            || $is_if_elsif_else_unless_while_until_for_foreach{
                $block_type_to_go[$ibeg] } );

        # only do this for a ':; which is aligned with its leading '?'
        my $is_unaligned_colon = $types_to_go[$ibeg] eq ':' && !$is_leading;
        if (   defined($opening_indentation)
            && !$is_isolated_block_brace
            && !$is_unaligned_colon )
        {
            if ( get_SPACES($opening_indentation) > get_SPACES($indentation) ) {
                $indentation = $opening_indentation;
            }
        }

        # remember the indentation of each line of this batch
        push @{$rindentation_list}, $indentation;

        # outdent lines with certain leading tokens...
        if (

            # must be first word of this batch
            $ibeg == 0

            # and ...
            && (

                # certain leading keywords if requested
                (
                       $rOpts->{'outdent-keywords'}
                    && $types_to_go[$ibeg] eq 'k'
                    && $outdent_keyword{ $tokens_to_go[$ibeg] }
                )

                # or labels if requested
                || ( $rOpts->{'outdent-labels'} && $types_to_go[$ibeg] eq 'J' )

                # or static block comments if requested
                || (   $types_to_go[$ibeg] eq '#'
                    && $rOpts->{'outdent-static-block-comments'}
                    && $is_static_block_comment )
            )
          )

        {
            my $space_count = leading_spaces_to_go($ibeg);
            if ( $space_count > 0 ) {
                $space_count -= $rOpts_continuation_indentation;
                $is_outdented_line = 1;
                if ( $space_count < 0 ) { $space_count = 0 }

                # do not promote a spaced static block comment to non-spaced;
                # this is not normally necessary but could be for some
                # unusual user inputs (such as -ci = -i)
                if ( $types_to_go[$ibeg] eq '#' && $space_count == 0 ) {
                    $space_count = 1;
                }

                if ($rOpts_line_up_parentheses) {
                    $indentation =
                      new_lp_indentation_item( $space_count, $lev, 0, 0, 0 );
                }
                else {
                    $indentation = $space_count;
                }
            }
        }

        return ( $indentation, $lev, $level_end, $terminal_type,
            $is_semicolon_terminated, $is_outdented_line );
    }
}

sub set_vertical_tightness_flags {

    my ( $n, $n_last_line, $ibeg, $iend, $ri_first, $ri_last ) = @_;

    # Define vertical tightness controls for the nth line of a batch.
    # We create an array of parameters which tell the vertical aligner
    # if we should combine this line with the next line to achieve the
    # desired vertical tightness.  The array of parameters contains:
    #
    #   [0] type: 1=is opening tok 2=is closing tok  3=is opening block brace
    #   [1] flag: if opening: 1=no multiple steps, 2=multiple steps ok
    #             if closing: spaces of padding to use
    #   [2] sequence number of container
    #   [3] valid flag: do not append if this flag is false. Will be
    #       true if appropriate -vt flag is set.  Otherwise, Will be
    #       made true only for 2 line container in parens with -lp
    #
    # These flags are used by sub set_leading_whitespace in
    # the vertical aligner

    my $rvertical_tightness_flags = [ 0, 0, 0, 0, 0, 0 ];

    # For non-BLOCK tokens, we will need to examine the next line
    # too, so we won't consider the last line.
    if ( $n < $n_last_line ) {

        # see if last token is an opening token...not a BLOCK...
        my $ibeg_next = $$ri_first[ $n + 1 ];
        my $token_end = $tokens_to_go[$iend];
        my $iend_next = $$ri_last[ $n + 1 ];
        if (
               $type_sequence_to_go[$iend]
            && !$block_type_to_go[$iend]
            && $is_opening_token{$token_end}
            && (
                $opening_vertical_tightness{$token_end} > 0

                # allow 2-line method call to be closed up
                || (   $rOpts_line_up_parentheses
                    && $token_end eq '('
                    && $iend > $ibeg
                    && $types_to_go[ $iend - 1 ] ne 'b' )
            )
          )
        {

            # avoid multiple jumps in nesting depth in one line if
            # requested
            my $ovt       = $opening_vertical_tightness{$token_end};
            my $iend_next = $$ri_last[ $n + 1 ];
            unless (
                $ovt < 2
                && ( $nesting_depth_to_go[ $iend_next + 1 ] !=
                    $nesting_depth_to_go[$ibeg_next] )
              )
            {

                # If -vt flag has not been set, mark this as invalid
                # and aligner will validate it if it sees the closing paren
                # within 2 lines.
                my $valid_flag = $ovt;
                @{$rvertical_tightness_flags} =
                  ( 1, $ovt, $type_sequence_to_go[$iend], $valid_flag );
            }
        }

        # see if first token of next line is a closing token...
        # ..and be sure this line does not have a side comment
        my $token_next = $tokens_to_go[$ibeg_next];
        if (   $type_sequence_to_go[$ibeg_next]
            && !$block_type_to_go[$ibeg_next]
            && $is_closing_token{$token_next}
            && $types_to_go[$iend] !~ '#' )    # for safety, shouldn't happen!
        {
            my $ovt = $opening_vertical_tightness{$token_next};
            my $cvt = $closing_vertical_tightness{$token_next};
            if (

                # never append a trailing line like   )->pack(
                # because it will throw off later alignment
                (
                    $nesting_depth_to_go[$ibeg_next] ==
                    $nesting_depth_to_go[ $iend_next + 1 ] + 1
                )
                && (
                    $cvt == 2
                    || (
                        $container_environment_to_go[$ibeg_next] ne 'LIST'
                        && (
                            $cvt == 1

                            # allow closing up 2-line method calls
                            || (   $rOpts_line_up_parentheses
                                && $token_next eq ')' )
                        )
                    )
                )
              )
            {

                # decide which trailing closing tokens to append..
                my $ok = 0;
                if ( $cvt == 2 || $iend_next == $ibeg_next ) { $ok = 1 }
                else {
                    my $str = join( '',
                        @types_to_go[ $ibeg_next + 1 .. $ibeg_next + 2 ] );

                    # append closing token if followed by comment or ';'
                    if ( $str =~ /^b?[#;]/ ) { $ok = 1 }
                }

                if ($ok) {
                    my $valid_flag = $cvt;
                    @{$rvertical_tightness_flags} = (
                        2,
                        $tightness{$token_next} == 2 ? 0 : 1,
                        $type_sequence_to_go[$ibeg_next], $valid_flag,
                    );
                }
            }
        }

        # Opening Token Right
        # If requested, move an isolated trailing opening token to the end of
        # the previous line which ended in a comma.  We could do this
        # in sub recombine_breakpoints but that would cause problems
        # with -lp formatting.  The problem is that indentation will
        # quickly move far to the right in nested expressions.  By
        # doing it after indentation has been set, we avoid changes
        # to the indentation.  Actual movement of the token takes place
        # in sub write_leader_and_string.
        if (
            $opening_token_right{ $tokens_to_go[$ibeg_next] }

            # previous line is not opening
            # (use -sot to combine with it)
            && !$is_opening_token{$token_end}

            # previous line ended in one of these
            # (add other cases if necessary; '=>' and '.' are not necessary
            ##&& ($is_opening_token{$token_end} || $token_end eq ',')
            && !$block_type_to_go[$ibeg_next]

            # this is a line with just an opening token
            && (   $iend_next == $ibeg_next
                || $iend_next == $ibeg_next + 2
                && $types_to_go[$iend_next] eq '#' )

            # looks bad if we align vertically with the wrong container
            && $tokens_to_go[$ibeg] ne $tokens_to_go[$ibeg_next]
          )
        {
            my $valid_flag = 1;
            my $spaces = ( $types_to_go[ $ibeg_next - 1 ] eq 'b' ) ? 1 : 0;
            @{$rvertical_tightness_flags} =
              ( 2, $spaces, $type_sequence_to_go[$ibeg_next], $valid_flag, );
        }

        # Stacking of opening and closing tokens
        my $stackable;
        my $token_beg_next = $tokens_to_go[$ibeg_next];

        # patch to make something like 'qw(' behave like an opening paren
        # (aran.t)
        if ( $types_to_go[$ibeg_next] eq 'q' ) {
            if ( $token_beg_next =~ /^qw\s*([\[\(\{])$/ ) {
                $token_beg_next = $1;
            }
        }

        if (   $is_closing_token{$token_end}
            && $is_closing_token{$token_beg_next} )
        {
            $stackable = $stack_closing_token{$token_beg_next}
              unless ( $block_type_to_go[$ibeg_next] )
              ;    # shouldn't happen; just checking
        }
        elsif ($is_opening_token{$token_end}
            && $is_opening_token{$token_beg_next} )
        {
            $stackable = $stack_opening_token{$token_beg_next}
              unless ( $block_type_to_go[$ibeg_next] )
              ;    # shouldn't happen; just checking
        }

        if ($stackable) {

            my $is_semicolon_terminated;
            if ( $n + 1 == $n_last_line ) {
                my ( $terminal_type, $i_terminal ) = terminal_type(
                    \@types_to_go, \@block_type_to_go,
                    $ibeg_next,    $iend_next
                );
                $is_semicolon_terminated = $terminal_type eq ';'
                  && $nesting_depth_to_go[$iend_next] <
                  $nesting_depth_to_go[$ibeg_next];
            }

            # this must be a line with just an opening token
            # or end in a semicolon
            if (
                $is_semicolon_terminated
                || (   $iend_next == $ibeg_next
                    || $iend_next == $ibeg_next + 2
                    && $types_to_go[$iend_next] eq '#' )
              )
            {
                my $valid_flag = 1;
                my $spaces = ( $types_to_go[ $ibeg_next - 1 ] eq 'b' ) ? 1 : 0;
                @{$rvertical_tightness_flags} =
                  ( 2, $spaces, $type_sequence_to_go[$ibeg_next], $valid_flag,
                  );
            }
        }
    }

    # Check for a last line with isolated opening BLOCK curly
    elsif ($rOpts_block_brace_vertical_tightness
        && $ibeg eq $iend
        && $types_to_go[$iend] eq '{'
        && $block_type_to_go[$iend] =~
        /$block_brace_vertical_tightness_pattern/o )
    {
        @{$rvertical_tightness_flags} =
          ( 3, $rOpts_block_brace_vertical_tightness, 0, 1 );
    }

    # pack in the sequence numbers of the ends of this line
    $rvertical_tightness_flags->[4] = get_seqno($ibeg);
    $rvertical_tightness_flags->[5] = get_seqno($iend);
    return $rvertical_tightness_flags;
}

sub get_seqno {

    # get opening and closing sequence numbers of a token for the vertical
    # aligner.  Assign qw quotes a value to allow qw opening and closing tokens
    # to be treated somewhat like opening and closing tokens for stacking
    # tokens by the vertical aligner.
    my ($ii) = @_;
    my $seqno = $type_sequence_to_go[$ii];
    if ( $types_to_go[$ii] eq 'q' ) {
        my $SEQ_QW = -1;
        if ( $ii > 0 ) {
            $seqno = $SEQ_QW if ( $tokens_to_go[$ii] =~ /^qw\s*[\(\{\[]/ );
        }
        else {
            if ( !$ending_in_quote ) {
                $seqno = $SEQ_QW if ( $tokens_to_go[$ii] =~ /[\)\}\]]$/ );
            }
        }
    }
    return ($seqno);
}

{
    my %is_vertical_alignment_type;
    my %is_vertical_alignment_keyword;

    BEGIN {

        @_ = qw#
          = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=
          { ? : => =~ && || // ~~ !~~
          #;
        @is_vertical_alignment_type{@_} = (1) x scalar(@_);

        @_ = qw(if unless and or err eq ne for foreach while until);
        @is_vertical_alignment_keyword{@_} = (1) x scalar(@_);
    }

    sub set_vertical_alignment_markers {

        # This routine takes the first step toward vertical alignment of the
        # lines of output text.  It looks for certain tokens which can serve as
        # vertical alignment markers (such as an '=').
        #
        # Method: We look at each token $i in this output batch and set
        # $matching_token_to_go[$i] equal to those tokens at which we would
        # accept vertical alignment.

        # nothing to do if we aren't allowed to change whitespace
        if ( !$rOpts_add_whitespace ) {
            for my $i ( 0 .. $max_index_to_go ) {
                $matching_token_to_go[$i] = '';
            }
            return;
        }

        my ( $ri_first, $ri_last ) = @_;

        # remember the index of last nonblank token before any sidecomment
        my $i_terminal = $max_index_to_go;
        if ( $types_to_go[$i_terminal] eq '#' ) {
            if ( $i_terminal > 0 && $types_to_go[ --$i_terminal ] eq 'b' ) {
                if ( $i_terminal > 0 ) { --$i_terminal }
            }
        }

        # look at each line of this batch..
        my $last_vertical_alignment_before_index;
        my $vert_last_nonblank_type;
        my $vert_last_nonblank_token;
        my $vert_last_nonblank_block_type;
        my $max_line = @$ri_first - 1;
        my ( $i, $type, $token, $block_type, $alignment_type );
        my ( $ibeg, $iend, $line );

        foreach $line ( 0 .. $max_line ) {
            $ibeg                                 = $$ri_first[$line];
            $iend                                 = $$ri_last[$line];
            $last_vertical_alignment_before_index = -1;
            $vert_last_nonblank_type              = '';
            $vert_last_nonblank_token             = '';
            $vert_last_nonblank_block_type        = '';

            # look at each token in this output line..
            foreach $i ( $ibeg .. $iend ) {
                $alignment_type = '';
                $type           = $types_to_go[$i];
                $block_type     = $block_type_to_go[$i];
                $token          = $tokens_to_go[$i];

                # check for flag indicating that we should not align
                # this token
                if ( $matching_token_to_go[$i] ) {
                    $matching_token_to_go[$i] = '';
                    next;
                }

                #--------------------------------------------------------
                # First see if we want to align BEFORE this token
                #--------------------------------------------------------

                # The first possible token that we can align before
                # is index 2 because: 1) it doesn't normally make sense to
                # align before the first token and 2) the second
                # token must be a blank if we are to align before
                # the third
                if ( $i < $ibeg + 2 ) { }

                # must follow a blank token
                elsif ( $types_to_go[ $i - 1 ] ne 'b' ) { }

                # align a side comment --
                elsif ( $type eq '#' ) {

                    unless (

                        # it is a static side comment
                        (
                               $rOpts->{'static-side-comments'}
                            && $token =~ /$static_side_comment_pattern/o
                        )

                        # or a closing side comment
                        || (   $vert_last_nonblank_block_type
                            && $token =~
                            /$closing_side_comment_prefix_pattern/o )
                      )
                    {
                        $alignment_type = $type;
                    }    ## Example of a static side comment
                }

                # otherwise, do not align two in a row to create a
                # blank field
                elsif ( $last_vertical_alignment_before_index == $i - 2 ) { }

                # align before one of these keywords
                # (within a line, since $i>1)
                elsif ( $type eq 'k' ) {

                    #  /^(if|unless|and|or|eq|ne)$/
                    if ( $is_vertical_alignment_keyword{$token} ) {
                        $alignment_type = $token;
                    }
                }

                # align before one of these types..
                # Note: add '.' after new vertical aligner is operational
                elsif ( $is_vertical_alignment_type{$type} ) {
                    $alignment_type = $token;

                    # Do not align a terminal token.  Although it might
                    # occasionally look ok to do this, it has been found to be
                    # a good general rule.  The main problems are:
                    # (1) that the terminal token (such as an = or :) might get
                    # moved far to the right where it is hard to see because
                    # nothing follows it, and
                    # (2) doing so may prevent other good alignments.
                    if ( $i == $iend || $i >= $i_terminal ) {
                        $alignment_type = "";
                    }

                    # Do not align leading ': (' or '. ('.  This would prevent
                    # alignment in something like the following:
                    #   $extra_space .=
                    #       ( $input_line_number < 10 )  ? "  "
                    #     : ( $input_line_number < 100 ) ? " "
                    #     :                                "";
                    # or
                    #  $code =
                    #      ( $case_matters ? $accessor : " lc($accessor) " )
                    #    . ( $yesno        ? " eq "       : " ne " )
                    if (   $i == $ibeg + 2
                        && $types_to_go[$ibeg] =~ /^[\.\:]$/
                        && $types_to_go[ $i - 1 ] eq 'b' )
                    {
                        $alignment_type = "";
                    }

                    # For a paren after keyword, only align something like this:
                    #    if    ( $a ) { &a }
                    #    elsif ( $b ) { &b }
                    if ( $token eq '(' && $vert_last_nonblank_type eq 'k' ) {
                        $alignment_type = ""
                          unless $vert_last_nonblank_token =~
                              /^(if|unless|elsif)$/;
                    }

                    # be sure the alignment tokens are unique
                    # This didn't work well: reason not determined
                    # if ($token ne $type) {$alignment_type .= $type}
                }

                # NOTE: This is deactivated because it causes the previous
                # if/elsif alignment to fail
                #elsif ( $type eq '}' && $token eq '}' && $block_type_to_go[$i])
                #{ $alignment_type = $type; }

                if ($alignment_type) {
                    $last_vertical_alignment_before_index = $i;
                }

                #--------------------------------------------------------
                # Next see if we want to align AFTER the previous nonblank
                #--------------------------------------------------------

                # We want to line up ',' and interior ';' tokens, with the added
                # space AFTER these tokens.  (Note: interior ';' is included
                # because it may occur in short blocks).
                if (

                    # we haven't already set it
                    !$alignment_type

                    # and its not the first token of the line
                    && ( $i > $ibeg )

                    # and it follows a blank
                    && $types_to_go[ $i - 1 ] eq 'b'

                    # and previous token IS one of these:
                    && ( $vert_last_nonblank_type =~ /^[\,\;]$/ )

                    # and it's NOT one of these
                    && ( $type !~ /^[b\#\)\]\}]$/ )

                    # then go ahead and align
                  )

                {
                    $alignment_type = $vert_last_nonblank_type;
                }

                #--------------------------------------------------------
                # then store the value
                #--------------------------------------------------------
                $matching_token_to_go[$i] = $alignment_type;
                if ( $type ne 'b' ) {
                    $vert_last_nonblank_type       = $type;
                    $vert_last_nonblank_token      = $token;
                    $vert_last_nonblank_block_type = $block_type;
                }
            }
        }
    }
}

sub terminal_type {

    #    returns type of last token on this line (terminal token), as follows:
    #    returns # for a full-line comment
    #    returns ' ' for a blank line
    #    otherwise returns final token type

    my ( $rtype, $rblock_type, $ibeg, $iend ) = @_;

    # check for full-line comment..
    if ( $$rtype[$ibeg] eq '#' ) {
        return wantarray ? ( $$rtype[$ibeg], $ibeg ) : $$rtype[$ibeg];
    }
    else {

        # start at end and walk bakwards..
        for ( my $i = $iend ; $i >= $ibeg ; $i-- ) {

            # skip past any side comment and blanks
            next if ( $$rtype[$i] eq 'b' );
            next if ( $$rtype[$i] eq '#' );

            # found it..make sure it is a BLOCK termination,
            # but hide a terminal } after sort/grep/map because it is not
            # necessarily the end of the line.  (terminal.t)
            my $terminal_type = $$rtype[$i];
            if (
                $terminal_type eq '}'
                && ( !$$rblock_type[$i]
                    || ( $is_sort_map_grep_eval_do{ $$rblock_type[$i] } ) )
              )
            {
                $terminal_type = 'b';
            }
            return wantarray ? ( $terminal_type, $i ) : $terminal_type;
        }

        # empty line
        return wantarray ? ( ' ', $ibeg ) : ' ';
    }
}

{
    my %is_good_keyword_breakpoint;
    my %is_lt_gt_le_ge;

    sub set_bond_strengths {

        BEGIN {

            @_ = qw(if unless while until for foreach);
            @is_good_keyword_breakpoint{@_} = (1) x scalar(@_);

            @_ = qw(lt gt le ge);
            @is_lt_gt_le_ge{@_} = (1) x scalar(@_);

            ###############################################################
            # NOTE: NO_BREAK's set here are HINTS which may not be honored;
            # essential NO_BREAKS's must be enforced in section 2, below.
            ###############################################################

            # adding NEW_TOKENS: add a left and right bond strength by
            # mimmicking what is done for an existing token type.  You
            # can skip this step at first and take the default, then
            # tweak later to get desired results.

            # The bond strengths should roughly follow precenence order where
            # possible.  If you make changes, please check the results very
            # carefully on a variety of scripts.

            # no break around possible filehandle
            $left_bond_strength{'Z'}  = NO_BREAK;
            $right_bond_strength{'Z'} = NO_BREAK;

            # never put a bare word on a new line:
            # example print (STDERR, "bla"); will fail with break after (
            $left_bond_strength{'w'} = NO_BREAK;

        # blanks always have infinite strength to force breaks after real tokens
            $right_bond_strength{'b'} = NO_BREAK;

            # try not to break on exponentation
            @_                       = qw" ** .. ... <=> ";
            @left_bond_strength{@_}  = (STRONG) x scalar(@_);
            @right_bond_strength{@_} = (STRONG) x scalar(@_);

            # The comma-arrow has very low precedence but not a good break point
            $left_bond_strength{'=>'}  = NO_BREAK;
            $right_bond_strength{'=>'} = NOMINAL;

            # ok to break after label
            $left_bond_strength{'J'}  = NO_BREAK;
            $right_bond_strength{'J'} = NOMINAL;
            $left_bond_strength{'j'}  = STRONG;
            $right_bond_strength{'j'} = STRONG;
            $left_bond_strength{'A'}  = STRONG;
            $right_bond_strength{'A'} = STRONG;

            $left_bond_strength{'->'}  = STRONG;
            $right_bond_strength{'->'} = VERY_STRONG;

            # breaking AFTER modulus operator is ok:
            @_ = qw" % ";
            @left_bond_strength{@_} = (STRONG) x scalar(@_);
            @right_bond_strength{@_} =
              ( 0.1 * NOMINAL + 0.9 * STRONG ) x scalar(@_);

            # Break AFTER math operators * and /
            @_                       = qw" * / x  ";
            @left_bond_strength{@_}  = (STRONG) x scalar(@_);
            @right_bond_strength{@_} = (NOMINAL) x scalar(@_);

            # Break AFTER weakest math operators + and -
            # Make them weaker than * but a bit stronger than '.'
            @_ = qw" + - ";
            @left_bond_strength{@_} = (STRONG) x scalar(@_);
            @right_bond_strength{@_} =
              ( 0.91 * NOMINAL + 0.09 * WEAK ) x scalar(@_);

            # breaking BEFORE these is just ok:
            @_                       = qw" >> << ";
            @right_bond_strength{@_} = (STRONG) x scalar(@_);
            @left_bond_strength{@_}  = (NOMINAL) x scalar(@_);

            # breaking before the string concatenation operator seems best
            # because it can be hard to see at the end of a line
            $right_bond_strength{'.'} = STRONG;
            $left_bond_strength{'.'}  = 0.9 * NOMINAL + 0.1 * WEAK;

            @_                       = qw"} ] ) ";
            @left_bond_strength{@_}  = (STRONG) x scalar(@_);
            @right_bond_strength{@_} = (NOMINAL) x scalar(@_);

            # make these a little weaker than nominal so that they get
            # favored for end-of-line characters
            @_ = qw"!= == =~ !~ ~~ !~~";
            @left_bond_strength{@_} = (STRONG) x scalar(@_);
            @right_bond_strength{@_} =
              ( 0.9 * NOMINAL + 0.1 * WEAK ) x scalar(@_);

            # break AFTER these
            @_ = qw" < >  | & >= <=";
            @left_bond_strength{@_} = (VERY_STRONG) x scalar(@_);
            @right_bond_strength{@_} =
              ( 0.8 * NOMINAL + 0.2 * WEAK ) x scalar(@_);

            # breaking either before or after a quote is ok
            # but bias for breaking before a quote
            $left_bond_strength{'Q'}  = NOMINAL;
            $right_bond_strength{'Q'} = NOMINAL + 0.02;
            $left_bond_strength{'q'}  = NOMINAL;
            $right_bond_strength{'q'} = NOMINAL;

            # starting a line with a keyword is usually ok
            $left_bond_strength{'k'} = NOMINAL;

            # we usually want to bond a keyword strongly to what immediately
            # follows, rather than leaving it stranded at the end of a line
            $right_bond_strength{'k'} = STRONG;

            $left_bond_strength{'G'}  = NOMINAL;
            $right_bond_strength{'G'} = STRONG;

            # it is good to break AFTER various assignment operators
            @_ = qw(
              = **= += *= &= <<= &&=
              -= /= |= >>= ||= //=
              .= %= ^=
              x=
            );
            @left_bond_strength{@_} = (STRONG) x scalar(@_);
            @right_bond_strength{@_} =
              ( 0.4 * WEAK + 0.6 * VERY_WEAK ) x scalar(@_);

            # break BEFORE '&&' and '||' and '//'
            # set strength of '||' to same as '=' so that chains like
            # $a = $b || $c || $d   will break before the first '||'
            $right_bond_strength{'||'} = NOMINAL;
            $left_bond_strength{'||'}  = $right_bond_strength{'='};

            # same thing for '//'
            $right_bond_strength{'//'} = NOMINAL;
            $left_bond_strength{'//'}  = $right_bond_strength{'='};

            # set strength of && a little higher than ||
            $right_bond_strength{'&&'} = NOMINAL;
            $left_bond_strength{'&&'}  = $left_bond_strength{'||'} + 0.1;

            $left_bond_strength{';'}  = VERY_STRONG;
            $right_bond_strength{';'} = VERY_WEAK;
            $left_bond_strength{'f'}  = VERY_STRONG;

            # make right strength of for ';' a little less than '='
            # to make for contents break after the ';' to avoid this:
            #   for ( $j = $number_of_fields - 1 ; $j < $item_count ; $j +=
            #     $number_of_fields )
            # and make it weaker than ',' and 'and' too
            $right_bond_strength{'f'} = VERY_WEAK - 0.03;

            # The strengths of ?/: should be somewhere between
            # an '=' and a quote (NOMINAL),
            # make strength of ':' slightly less than '?' to help
            # break long chains of ? : after the colons
            $left_bond_strength{':'}  = 0.4 * WEAK + 0.6 * NOMINAL;
            $right_bond_strength{':'} = NO_BREAK;
            $left_bond_strength{'?'}  = $left_bond_strength{':'} + 0.01;
            $right_bond_strength{'?'} = NO_BREAK;

            $left_bond_strength{','}  = VERY_STRONG;
            $right_bond_strength{','} = VERY_WEAK;

            # Set bond strengths of certain keywords
            # make 'or', 'err', 'and' slightly weaker than a ','
            $left_bond_strength{'and'}  = VERY_WEAK - 0.01;
            $left_bond_strength{'or'}   = VERY_WEAK - 0.02;
            $left_bond_strength{'err'}  = VERY_WEAK - 0.02;
            $left_bond_strength{'xor'}  = NOMINAL;
            $right_bond_strength{'and'} = NOMINAL;
            $right_bond_strength{'or'}  = NOMINAL;
            $right_bond_strength{'err'} = NOMINAL;
            $right_bond_strength{'xor'} = STRONG;
        }

        # patch-its always ok to break at end of line
        $nobreak_to_go[$max_index_to_go] = 0;

        # adding a small 'bias' to strengths is a simple way to make a line
        # break at the first of a sequence of identical terms.  For example,
        # to force long string of conditional operators to break with
        # each line ending in a ':', we can add a small number to the bond
        # strength of each ':'
        my $colon_bias = 0;
        my $amp_bias   = 0;
        my $bar_bias   = 0;
        my $and_bias   = 0;
        my $or_bias    = 0;
        my $dot_bias   = 0;
        my $f_bias     = 0;
        my $code_bias  = -.01;
        my $type       = 'b';
        my $token      = ' ';
        my $last_type;
        my $last_nonblank_type  = $type;
        my $last_nonblank_token = $token;
        my $delta_bias          = 0.0001;
        my $list_str            = $left_bond_strength{'?'};

        my ( $block_type, $i_next, $i_next_nonblank, $next_nonblank_token,
            $next_nonblank_type, $next_token, $next_type, $total_nesting_depth,
        );

        # preliminary loop to compute bond strengths
        for ( my $i = 0 ; $i <= $max_index_to_go ; $i++ ) {
            $last_type = $type;
            if ( $type ne 'b' ) {
                $last_nonblank_type  = $type;
                $last_nonblank_token = $token;
            }
            $type = $types_to_go[$i];

            # strength on both sides of a blank is the same
            if ( $type eq 'b' && $last_type ne 'b' ) {
                $bond_strength_to_go[$i] = $bond_strength_to_go[ $i - 1 ];
                next;
            }

            $token               = $tokens_to_go[$i];
            $block_type          = $block_type_to_go[$i];
            $i_next              = $i + 1;
            $next_type           = $types_to_go[$i_next];
            $next_token          = $tokens_to_go[$i_next];
            $total_nesting_depth = $nesting_depth_to_go[$i_next];
            $i_next_nonblank     = ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );
            $next_nonblank_type  = $types_to_go[$i_next_nonblank];
            $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

            # Some token chemistry...  The decision about where to break a
            # line depends upon a "bond strength" between tokens.  The LOWER
            # the bond strength, the MORE likely a break.  The strength
            # values are based on trial-and-error, and need to be tweaked
            # occasionally to get desired results.  Things to keep in mind
            # are:
            #   1. relative strengths are important.  small differences
            #      in strengths can make big formatting differences.
            #   2. each indentation level adds one unit of bond strength
            #   3. a value of NO_BREAK makes an unbreakable bond
            #   4. a value of VERY_WEAK is the strength of a ','
            #   5. values below NOMINAL are considered ok break points
            #   6. values above NOMINAL are considered poor break points
            # We are computing the strength of the bond between the current
            # token and the NEXT token.
            my $bond_str = VERY_STRONG;    # a default, high strength

            #---------------------------------------------------------------
            # section 1:
            # use minimum of left and right bond strengths if defined;
            # digraphs and trigraphs like to break on their left
            #---------------------------------------------------------------
            my $bsr = $right_bond_strength{$type};

            if ( !defined($bsr) ) {

                if ( $is_digraph{$type} || $is_trigraph{$type} ) {
                    $bsr = STRONG;
                }
                else {
                    $bsr = VERY_STRONG;
                }
            }

            # define right bond strengths of certain keywords
            if ( $type eq 'k' && defined( $right_bond_strength{$token} ) ) {
                $bsr = $right_bond_strength{$token};
            }
            elsif ( $token eq 'ne' or $token eq 'eq' ) {
                $bsr = NOMINAL;
            }
            my $bsl = $left_bond_strength{$next_nonblank_type};

            # set terminal bond strength to the nominal value
            # this will cause good preceding breaks to be retained
            if ( $i_next_nonblank > $max_index_to_go ) {
                $bsl = NOMINAL;
            }

            if ( !defined($bsl) ) {

                if (   $is_digraph{$next_nonblank_type}
                    || $is_trigraph{$next_nonblank_type} )
                {
                    $bsl = WEAK;
                }
                else {
                    $bsl = VERY_STRONG;
                }
            }

            # define right bond strengths of certain keywords
            if ( $next_nonblank_type eq 'k'
                && defined( $left_bond_strength{$next_nonblank_token} ) )
            {
                $bsl = $left_bond_strength{$next_nonblank_token};
            }
            elsif ($next_nonblank_token eq 'ne'
                or $next_nonblank_token eq 'eq' )
            {
                $bsl = NOMINAL;
            }
            elsif ( $is_lt_gt_le_ge{$next_nonblank_token} ) {
                $bsl = 0.9 * NOMINAL + 0.1 * STRONG;
            }

            # Note: it might seem that we would want to keep a NO_BREAK if
            # either token has this value.  This didn't work, because in an
            # arrow list, it prevents the comma from separating from the
            # following bare word (which is probably quoted by its arrow).
            # So necessary NO_BREAK's have to be handled as special cases
            # in the final section.
            $bond_str = ( $bsr < $bsl ) ? $bsr : $bsl;
            my $bond_str_1 = $bond_str;

            #---------------------------------------------------------------
            # section 2:
            # special cases
            #---------------------------------------------------------------

            # allow long lines before final { in an if statement, as in:
            #    if (..........
            #      ..........)
            #    {
            #
            # Otherwise, the line before the { tends to be too short.
            if ( $type eq ')' ) {
                if ( $next_nonblank_type eq '{' ) {
                    $bond_str = VERY_WEAK + 0.03;
                }
            }

            elsif ( $type eq '(' ) {
                if ( $next_nonblank_type eq '{' ) {
                    $bond_str = NOMINAL;
                }
            }

            # break on something like '} (', but keep this stronger than a ','
            # example is in 'howe.pl'
            elsif ( $type eq 'R' or $type eq '}' ) {
                if ( $next_nonblank_type eq '(' ) {
                    $bond_str = 0.8 * VERY_WEAK + 0.2 * WEAK;
                }
            }

            #-----------------------------------------------------------------
            # adjust bond strength bias
            #-----------------------------------------------------------------

            # TESTING: add any bias set by sub scan_list at old comma
            # break points.
            elsif ( $type eq ',' ) {
                $bond_str += $bond_strength_to_go[$i];
            }

            elsif ( $type eq 'f' ) {
                $bond_str += $f_bias;
                $f_bias   += $delta_bias;
            }

          # in long ?: conditionals, bias toward just one set per line (colon.t)
            elsif ( $type eq ':' ) {
                if ( !$want_break_before{$type} ) {
                    $bond_str   += $colon_bias;
                    $colon_bias += $delta_bias;
                }
            }

            if (   $next_nonblank_type eq ':'
                && $want_break_before{$next_nonblank_type} )
            {
                $bond_str   += $colon_bias;
                $colon_bias += $delta_bias;
            }

            # if leading '.' is used, align all but 'short' quotes;
            # the idea is to not place something like "\n" on a single line.
            elsif ( $next_nonblank_type eq '.' ) {
                if ( $want_break_before{'.'} ) {
                    unless (
                        $last_nonblank_type eq '.'
                        && (
                            length($token) <=
                            $rOpts_short_concatenation_item_length )
                        && ( $token !~ /^[\)\]\}]$/ )
                      )
                    {
                        $dot_bias += $delta_bias;
                    }
                    $bond_str += $dot_bias;
                }
            }
            elsif ($next_nonblank_type eq '&&'
                && $want_break_before{$next_nonblank_type} )
            {
                $bond_str += $amp_bias;
                $amp_bias += $delta_bias;
            }
            elsif ($next_nonblank_type eq '||'
                && $want_break_before{$next_nonblank_type} )
            {
                $bond_str += $bar_bias;
                $bar_bias += $delta_bias;
            }
            elsif ( $next_nonblank_type eq 'k' ) {

                if (   $next_nonblank_token eq 'and'
                    && $want_break_before{$next_nonblank_token} )
                {
                    $bond_str += $and_bias;
                    $and_bias += $delta_bias;
                }
                elsif ($next_nonblank_token =~ /^(or|err)$/
                    && $want_break_before{$next_nonblank_token} )
                {
                    $bond_str += $or_bias;
                    $or_bias  += $delta_bias;
                }

                # FIXME: needs more testing
                elsif ( $is_keyword_returning_list{$next_nonblank_token} ) {
                    $bond_str = $list_str if ( $bond_str > $list_str );
                }
                elsif ( $token eq 'err'
                    && !$want_break_before{$token} )
                {
                    $bond_str += $or_bias;
                    $or_bias  += $delta_bias;
                }
            }

            if ( $type eq ':'
                && !$want_break_before{$type} )
            {
                $bond_str   += $colon_bias;
                $colon_bias += $delta_bias;
            }
            elsif ( $type eq '&&'
                && !$want_break_before{$type} )
            {
                $bond_str += $amp_bias;
                $amp_bias += $delta_bias;
            }
            elsif ( $type eq '||'
                && !$want_break_before{$type} )
            {
                $bond_str += $bar_bias;
                $bar_bias += $delta_bias;
            }
            elsif ( $type eq 'k' ) {

                if ( $token eq 'and'
                    && !$want_break_before{$token} )
                {
                    $bond_str += $and_bias;
                    $and_bias += $delta_bias;
                }
                elsif ( $token eq 'or'
                    && !$want_break_before{$token} )
                {
                    $bond_str += $or_bias;
                    $or_bias  += $delta_bias;
                }
            }

            # keep matrix and hash indices together
            # but make them a little below STRONG to allow breaking open
            # something like {'some-word'}{'some-very-long-word'} at the }{
            # (bracebrk.t)
            if (   ( $type eq ']' or $type eq 'R' )
                && ( $next_nonblank_type eq '[' or $next_nonblank_type eq 'L' )
              )
            {
                $bond_str = 0.9 * STRONG + 0.1 * NOMINAL;
            }

            if ( $next_nonblank_token =~ /^->/ ) {

                # increase strength to the point where a break in the following
                # will be after the opening paren rather than at the arrow:
                #    $a->$b($c);
                if ( $type eq 'i' ) {
                    $bond_str = 1.45 * STRONG;
                }

                elsif ( $type =~ /^[\)\]\}R]$/ ) {
                    $bond_str = 0.1 * STRONG + 0.9 * NOMINAL;
                }

                # otherwise make strength before an '->' a little over a '+'
                else {
                    if ( $bond_str <= NOMINAL ) {
                        $bond_str = NOMINAL + 0.01;
                    }
                }
            }

            if ( $token eq ')' && $next_nonblank_token eq '[' ) {
                $bond_str = 0.2 * STRONG + 0.8 * NOMINAL;
            }

            # map1.t -- correct for a quirk in perl
            if (   $token eq '('
                && $next_nonblank_type eq 'i'
                && $last_nonblank_type eq 'k'
                && $is_sort_map_grep{$last_nonblank_token} )

              #     /^(sort|map|grep)$/ )
            {
                $bond_str = NO_BREAK;
            }

            # extrude.t: do not break before paren at:
            #    -l pid_filename(
            if ( $last_nonblank_type eq 'F' && $next_nonblank_token eq '(' ) {
                $bond_str = NO_BREAK;
            }

            # good to break after end of code blocks
            if ( $type eq '}' && $block_type ) {

                $bond_str = 0.5 * WEAK + 0.5 * VERY_WEAK + $code_bias;
                $code_bias += $delta_bias;
            }

            if ( $type eq 'k' ) {

                # allow certain control keywords to stand out
                if (   $next_nonblank_type eq 'k'
                    && $is_last_next_redo_return{$token} )
                {
                    $bond_str = 0.45 * WEAK + 0.55 * VERY_WEAK;
                }

# Don't break after keyword my.  This is a quick fix for a
# rare problem with perl. An example is this line from file
# Container.pm:
# foreach my $question( Debian::DebConf::ConfigDb::gettree( $this->{'question'} ) )

                if ( $token eq 'my' ) {
                    $bond_str = NO_BREAK;
                }

            }

            # good to break before 'if', 'unless', etc
            if ( $is_if_brace_follower{$next_nonblank_token} ) {
                $bond_str = VERY_WEAK;
            }

            if ( $next_nonblank_type eq 'k' ) {

                # keywords like 'unless', 'if', etc, within statements
                # make good breaks
                if ( $is_good_keyword_breakpoint{$next_nonblank_token} ) {
                    $bond_str = VERY_WEAK / 1.05;
                }
            }

            # try not to break before a comma-arrow
            elsif ( $next_nonblank_type eq '=>' ) {
                if ( $bond_str < STRONG ) { $bond_str = STRONG }
            }

         #----------------------------------------------------------------------
         # only set NO_BREAK's from here on
         #----------------------------------------------------------------------
            if ( $type eq 'C' or $type eq 'U' ) {

                # use strict requires that bare word and => not be separated
                if ( $next_nonblank_type eq '=>' ) {
                    $bond_str = NO_BREAK;
                }

                # Never break between a bareword and a following paren because
                # perl may give an error.  For example, if a break is placed
                # between 'to_filehandle' and its '(' the following line will
                # give a syntax error [Carp.pm]: my( $no) =fileno(
                # to_filehandle( $in)) ;
                if ( $next_nonblank_token eq '(' ) {
                    $bond_str = NO_BREAK;
                }
            }

           # use strict requires that bare word within braces not start new line
            elsif ( $type eq 'L' ) {

                if ( $next_nonblank_type eq 'w' ) {
                    $bond_str = NO_BREAK;
                }
            }

            # in older version of perl, use strict can cause problems with
            # breaks before bare words following opening parens.  For example,
            # this will fail under older versions if a break is made between
            # '(' and 'MAIL':
            #  use strict;
            #  open( MAIL, "a long filename or command");
            #  close MAIL;
            elsif ( $type eq '{' ) {

                if ( $token eq '(' && $next_nonblank_type eq 'w' ) {

                    # but it's fine to break if the word is followed by a '=>'
                    # or if it is obviously a sub call
                    my $i_next_next_nonblank = $i_next_nonblank + 1;
                    my $next_next_type = $types_to_go[$i_next_next_nonblank];
                    if (   $next_next_type eq 'b'
                        && $i_next_nonblank < $max_index_to_go )
                    {
                        $i_next_next_nonblank++;
                        $next_next_type = $types_to_go[$i_next_next_nonblank];
                    }

                    ##if ( $next_next_type ne '=>' ) {
                    # these are ok: '->xxx', '=>', '('

                    # We'll check for an old breakpoint and keep a leading
                    # bareword if it was that way in the input file.
                    # Presumably it was ok that way.  For example, the
                    # following would remain unchanged:
                    #
                    # @months = (
                    #   January,   February, March,    April,
                    #   May,       June,     July,     August,
                    #   September, October,  November, December,
                    # );
                    #
                    # This should be sufficient:
                    if ( !$old_breakpoint_to_go[$i]
                        && ( $next_next_type eq ',' || $next_next_type eq '}' )
                      )
                    {
                        $bond_str = NO_BREAK;
                    }
                }
            }

            elsif ( $type eq 'w' ) {

                if ( $next_nonblank_type eq 'R' ) {
                    $bond_str = NO_BREAK;
                }

                # use strict requires that bare word and => not be separated
                if ( $next_nonblank_type eq '=>' ) {
                    $bond_str = NO_BREAK;
                }
            }

            # in fact, use strict hates bare words on any new line.  For
            # example, a break before the underscore here provokes the
            # wrath of use strict:
            # if ( -r $fn && ( -s _ || $AllowZeroFilesize)) {
            elsif ( $type eq 'F' ) {
                $bond_str = NO_BREAK;
            }

            # use strict does not allow separating type info from trailing { }
            # testfile is readmail.pl
            elsif ( $type eq 't' or $type eq 'i' ) {

                if ( $next_nonblank_type eq 'L' ) {
                    $bond_str = NO_BREAK;
                }
            }

            # Do not break between a possible filehandle and a ? or / and do
            # not introduce a break after it if there is no blank
            # (extrude.t)
            elsif ( $type eq 'Z' ) {

                # dont break..
                if (

                    # if there is no blank and we do not want one. Examples:
                    #    print $x++    # do not break after $x
                    #    print HTML"HELLO"   # break ok after HTML
                    (
                           $next_type ne 'b'
                        && defined( $want_left_space{$next_type} )
                        && $want_left_space{$next_type} == WS_NO
                    )

                    # or we might be followed by the start of a quote
                    || $next_nonblank_type =~ /^[\/\?]$/
                  )
                {
                    $bond_str = NO_BREAK;
                }
            }

            # Do not break before a possible file handle
            if ( $next_nonblank_type eq 'Z' ) {
                $bond_str = NO_BREAK;
            }

            # As a defensive measure, do not break between a '(' and a
            # filehandle.  In some cases, this can cause an error.  For
            # example, the following program works:
            #    my $msg="hi!\n";
            #    print
            #    ( STDOUT
            #    $msg
            #    );
            #
            # But this program fails:
            #    my $msg="hi!\n";
            #    print
            #    (
            #    STDOUT
            #    $msg
            #    );
            #
            # This is normally only a problem with the 'extrude' option
            if ( $next_nonblank_type eq 'Y' && $token eq '(' ) {
                $bond_str = NO_BREAK;
            }

            # Breaking before a ++ can cause perl to guess wrong. For
            # example the following line will cause a syntax error
            # with -extrude if we break between '$i' and '++' [fixstyle2]
            #   print( ( $i++ & 1 ) ? $_ : ( $change{$_} || $_ ) );
            elsif ( $next_nonblank_type eq '++' ) {
                $bond_str = NO_BREAK;
            }

            # Breaking before a ? before a quote can cause trouble if
            # they are not separated by a blank.
            # Example: a syntax error occurs if you break before the ? here
            #  my$logic=join$all?' && ':' || ',@regexps;
            # From: Professional_Perl_Programming_Code/multifind.pl
            elsif ( $next_nonblank_type eq '?' ) {
                $bond_str = NO_BREAK
                  if ( $types_to_go[ $i_next_nonblank + 1 ] eq 'Q' );
            }

            # Breaking before a . followed by a number
            # can cause trouble if there is no intervening space
            # Example: a syntax error occurs if you break before the .2 here
            #  $str .= pack($endian.2, ensurrogate($ord));
            # From: perl58/Unicode.pm
            elsif ( $next_nonblank_type eq '.' ) {
                $bond_str = NO_BREAK
                  if ( $types_to_go[ $i_next_nonblank + 1 ] eq 'n' );
            }

            # patch to put cuddled elses back together when on multiple
            # lines, as in: } \n else \n { \n
            if ($rOpts_cuddled_else) {

                if (   ( $token eq 'else' ) && ( $next_nonblank_type eq '{' )
                    || ( $type eq '}' ) && ( $next_nonblank_token eq 'else' ) )
                {
                    $bond_str = NO_BREAK;
                }
            }

            # keep '}' together with ';'
            if ( ( $token eq '}' ) && ( $next_nonblank_type eq ';' ) ) {
                $bond_str = NO_BREAK;
            }

            # never break between sub name and opening paren
            if ( ( $type eq 'w' ) && ( $next_nonblank_token eq '(' ) ) {
                $bond_str = NO_BREAK;
            }

            #---------------------------------------------------------------
            # section 3:
            # now take nesting depth into account
            #---------------------------------------------------------------
            # final strength incorporates the bond strength and nesting depth
            my $strength;

            if ( defined($bond_str) && !$nobreak_to_go[$i] ) {
                if ( $total_nesting_depth > 0 ) {
                    $strength = $bond_str + $total_nesting_depth;
                }
                else {
                    $strength = $bond_str;
                }
            }
            else {
                $strength = NO_BREAK;
            }

            # always break after side comment
            if ( $type eq '#' ) { $strength = 0 }

            $bond_strength_to_go[$i] = $strength;

            FORMATTER_DEBUG_FLAG_BOND && do {
                my $str = substr( $token, 0, 15 );
                $str .= ' ' x ( 16 - length($str) );
                print
"BOND:  i=$i $str $type $next_nonblank_type depth=$total_nesting_depth strength=$bond_str_1 -> $bond_str -> $strength \n";
            };
        }
    }

}

sub pad_array_to_go {

    # to simplify coding in scan_list and set_bond_strengths, it helps
    # to create some extra blank tokens at the end of the arrays
    $tokens_to_go[ $max_index_to_go + 1 ] = '';
    $tokens_to_go[ $max_index_to_go + 2 ] = '';
    $types_to_go[ $max_index_to_go + 1 ]  = 'b';
    $types_to_go[ $max_index_to_go + 2 ]  = 'b';
    $nesting_depth_to_go[ $max_index_to_go + 1 ] =
      $nesting_depth_to_go[$max_index_to_go];

    #    /^[R\}\)\]]$/
    if ( $is_closing_type{ $types_to_go[$max_index_to_go] } ) {
        if ( $nesting_depth_to_go[$max_index_to_go] <= 0 ) {

            # shouldn't happen:
            unless ( get_saw_brace_error() ) {
                warning(
"Program bug in scan_list: hit nesting error which should have been caught\n"
                );
                report_definite_bug();
            }
        }
        else {
            $nesting_depth_to_go[ $max_index_to_go + 1 ] -= 1;
        }
    }

    #       /^[L\{\(\[]$/
    elsif ( $is_opening_type{ $types_to_go[$max_index_to_go] } ) {
        $nesting_depth_to_go[ $max_index_to_go + 1 ] += 1;
    }
}

{    # begin scan_list

    my (
        $block_type,                $current_depth,
        $depth,                     $i,
        $i_last_nonblank_token,     $last_colon_sequence_number,
        $last_nonblank_token,       $last_nonblank_type,
        $last_old_breakpoint_count, $minimum_depth,
        $next_nonblank_block_type,  $next_nonblank_token,
        $next_nonblank_type,        $old_breakpoint_count,
        $starting_breakpoint_count, $starting_depth,
        $token,                     $type,
        $type_sequence,
    );

    my (
        @breakpoint_stack,              @breakpoint_undo_stack,
        @comma_index,                   @container_type,
        @identifier_count_stack,        @index_before_arrow,
        @interrupted_list,              @item_count_stack,
        @last_comma_index,              @last_dot_index,
        @last_nonblank_type,            @old_breakpoint_count_stack,
        @opening_structure_index_stack, @rfor_semicolon_list,
        @has_old_logical_breakpoints,   @rand_or_list,
        @i_equals,
    );

    # routine to define essential variables when we go 'up' to
    # a new depth
    sub check_for_new_minimum_depth {
        my $depth = shift;
        if ( $depth < $minimum_depth ) {

            $minimum_depth = $depth;

            # these arrays need not retain values between calls
            $breakpoint_stack[$depth]              = $starting_breakpoint_count;
            $container_type[$depth]                = "";
            $identifier_count_stack[$depth]        = 0;
            $index_before_arrow[$depth]            = -1;
            $interrupted_list[$depth]              = 1;
            $item_count_stack[$depth]              = 0;
            $last_nonblank_type[$depth]            = "";
            $opening_structure_index_stack[$depth] = -1;

            $breakpoint_undo_stack[$depth]       = undef;
            $comma_index[$depth]                 = undef;
            $last_comma_index[$depth]            = undef;
            $last_dot_index[$depth]              = undef;
            $old_breakpoint_count_stack[$depth]  = undef;
            $has_old_logical_breakpoints[$depth] = 0;
            $rand_or_list[$depth]                = [];
            $rfor_semicolon_list[$depth]         = [];
            $i_equals[$depth]                    = -1;

            # these arrays must retain values between calls
            if ( !defined( $has_broken_sublist[$depth] ) ) {
                $dont_align[$depth]         = 0;
                $has_broken_sublist[$depth] = 0;
                $want_comma_break[$depth]   = 0;
            }
        }
    }

    # routine to decide which commas to break at within a container;
    # returns:
    #   $bp_count = number of comma breakpoints set
    #   $do_not_break_apart = a flag indicating if container need not
    #     be broken open
    sub set_comma_breakpoints {

        my $dd                 = shift;
        my $bp_count           = 0;
        my $do_not_break_apart = 0;

        # anything to do?
        if ( $item_count_stack[$dd] ) {

            # handle commas not in containers...
            if ( $dont_align[$dd] ) {
                do_uncontained_comma_breaks($dd);
            }

            # handle commas within containers...
            else {
                my $fbc = $forced_breakpoint_count;

                # always open comma lists not preceded by keywords,
                # barewords, identifiers (that is, anything that doesn't
                # look like a function call)
                my $must_break_open = $last_nonblank_type[$dd] !~ /^[kwiU]$/;

                set_comma_breakpoints_do(
                    $dd,
                    $opening_structure_index_stack[$dd],
                    $i,
                    $item_count_stack[$dd],
                    $identifier_count_stack[$dd],
                    $comma_index[$dd],
                    $next_nonblank_type,
                    $container_type[$dd],
                    $interrupted_list[$dd],
                    \$do_not_break_apart,
                    $must_break_open,
                );
                $bp_count = $forced_breakpoint_count - $fbc;
                $do_not_break_apart = 0 if $must_break_open;
            }
        }
        return ( $bp_count, $do_not_break_apart );
    }

    sub do_uncontained_comma_breaks {

        # Handle commas not in containers...
        # This is a catch-all routine for commas that we
        # don't know what to do with because the don't fall
        # within containers.  We will bias the bond strength
        # to break at commas which ended lines in the input
        # file.  This usually works better than just trying
        # to put as many items on a line as possible.  A
        # downside is that if the input file is garbage it
        # won't work very well. However, the user can always
        # prevent following the old breakpoints with the
        # -iob flag.
        my $dd   = shift;
        my $bias = -.01;
        foreach my $ii ( @{ $comma_index[$dd] } ) {
            if ( $old_breakpoint_to_go[$ii] ) {
                $bond_strength_to_go[$ii] = $bias;

                # reduce bias magnitude to force breaks in order
                $bias *= 0.99;
            }
        }

        # Also put a break before the first comma if
        # (1) there was a break there in the input, and
        # (2) that was exactly one previous break in the input
        #
        # For example, we will follow the user and break after
        # 'print' in this snippet:
        #    print
        #      "conformability (Not the same dimension)\n",
        #      "\t", $have, " is ", text_unit($hu), "\n",
        #      "\t", $want, " is ", text_unit($wu), "\n",
        #      ;
        my $i_first_comma = $comma_index[$dd]->[0];
        if ( $old_breakpoint_to_go[$i_first_comma] ) {
            my $level_comma = $levels_to_go[$i_first_comma];
            my $ibreak      = -1;
            my $obp_count   = 0;
            for ( my $ii = $i_first_comma - 1 ; $ii >= 0 ; $ii -= 1 ) {
                if ( $old_breakpoint_to_go[$ii] ) {
                    $obp_count++;
                    last if ( $obp_count > 1 );
                    $ibreak = $ii
                      if ( $levels_to_go[$ii] == $level_comma );
                }
            }
            if ( $ibreak >= 0 && $obp_count == 1 ) {
                set_forced_breakpoint($ibreak);
            }
        }
    }

    my %is_logical_container;

    BEGIN {
        @_ = qw# if elsif unless while and or err not && | || ? : ! #;
        @is_logical_container{@_} = (1) x scalar(@_);
    }

    sub set_for_semicolon_breakpoints {
        my $dd = shift;
        foreach ( @{ $rfor_semicolon_list[$dd] } ) {
            set_forced_breakpoint($_);
        }
    }

    sub set_logical_breakpoints {
        my $dd = shift;
        if (
               $item_count_stack[$dd] == 0
            && $is_logical_container{ $container_type[$dd] }

            # TESTING:
            || $has_old_logical_breakpoints[$dd]
          )
        {

            # Look for breaks in this order:
            # 0   1    2   3
            # or  and  ||  &&
            foreach my $i ( 0 .. 3 ) {
                if ( $rand_or_list[$dd][$i] ) {
                    foreach ( @{ $rand_or_list[$dd][$i] } ) {
                        set_forced_breakpoint($_);
                    }

                    # break at any 'if' and 'unless' too
                    foreach ( @{ $rand_or_list[$dd][4] } ) {
                        set_forced_breakpoint($_);
                    }
                    $rand_or_list[$dd] = [];
                    last;
                }
            }
        }
    }

    sub is_unbreakable_container {

        # never break a container of one of these types
        # because bad things can happen (map1.t)
        my $dd = shift;
        $is_sort_map_grep{ $container_type[$dd] };
    }

    sub scan_list {

        # This routine is responsible for setting line breaks for all lists,
        # so that hierarchical structure can be displayed and so that list
        # items can be vertically aligned.  The output of this routine is
        # stored in the array @forced_breakpoint_to_go, which is used to set
        # final breakpoints.

        $starting_depth = $nesting_depth_to_go[0];

        $block_type                 = ' ';
        $current_depth              = $starting_depth;
        $i                          = -1;
        $last_colon_sequence_number = -1;
        $last_nonblank_token        = ';';
        $last_nonblank_type         = ';';
        $last_nonblank_block_type   = ' ';
        $last_old_breakpoint_count  = 0;
        $minimum_depth = $current_depth + 1;    # forces update in check below
        $old_breakpoint_count      = 0;
        $starting_breakpoint_count = $forced_breakpoint_count;
        $token                     = ';';
        $type                      = ';';
        $type_sequence             = '';

        check_for_new_minimum_depth($current_depth);

        my $is_long_line = excess_line_length( 0, $max_index_to_go ) > 0;
        my $want_previous_breakpoint = -1;

        my $saw_good_breakpoint;
        my $i_line_end   = -1;
        my $i_line_start = -1;

        # loop over all tokens in this batch
        while ( ++$i <= $max_index_to_go ) {
            if ( $type ne 'b' ) {
                $i_last_nonblank_token    = $i - 1;
                $last_nonblank_type       = $type;
                $last_nonblank_token      = $token;
                $last_nonblank_block_type = $block_type;
            }
            $type          = $types_to_go[$i];
            $block_type    = $block_type_to_go[$i];
            $token         = $tokens_to_go[$i];
            $type_sequence = $type_sequence_to_go[$i];
            my $next_type       = $types_to_go[ $i + 1 ];
            my $next_token      = $tokens_to_go[ $i + 1 ];
            my $i_next_nonblank = ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );
            $next_nonblank_type       = $types_to_go[$i_next_nonblank];
            $next_nonblank_token      = $tokens_to_go[$i_next_nonblank];
            $next_nonblank_block_type = $block_type_to_go[$i_next_nonblank];

            # set break if flag was set
            if ( $want_previous_breakpoint >= 0 ) {
                set_forced_breakpoint($want_previous_breakpoint);
                $want_previous_breakpoint = -1;
            }

            $last_old_breakpoint_count = $old_breakpoint_count;
            if ( $old_breakpoint_to_go[$i] ) {
                $i_line_end   = $i;
                $i_line_start = $i_next_nonblank;

                $old_breakpoint_count++;

                # Break before certain keywords if user broke there and
                # this is a 'safe' break point. The idea is to retain
                # any preferred breaks for sequential list operations,
                # like a schwartzian transform.
                if ($rOpts_break_at_old_keyword_breakpoints) {
                    if (
                           $next_nonblank_type eq 'k'
                        && $is_keyword_returning_list{$next_nonblank_token}
                        && (   $type =~ /^[=\)\]\}Riw]$/
                            || $type eq 'k'
                            && $is_keyword_returning_list{$token} )
                      )
                    {

                        # we actually have to set this break next time through
                        # the loop because if we are at a closing token (such
                        # as '}') which forms a one-line block, this break might
                        # get undone.
                        $want_previous_breakpoint = $i;
                    }
                }
            }
            next if ( $type eq 'b' );
            $depth = $nesting_depth_to_go[ $i + 1 ];

            # safety check - be sure we always break after a comment
            # Shouldn't happen .. an error here probably means that the
            # nobreak flag did not get turned off correctly during
            # formatting.
            if ( $type eq '#' ) {
                if ( $i != $max_index_to_go ) {
                    warning(
"Non-fatal program bug: backup logic needed to break after a comment\n"
                    );
                    report_definite_bug();
                    $nobreak_to_go[$i] = 0;
                    set_forced_breakpoint($i);
                }
            }

            # Force breakpoints at certain tokens in long lines.
            # Note that such breakpoints will be undone later if these tokens
            # are fully contained within parens on a line.
            if (

                # break before a keyword within a line
                $type eq 'k'
                && $i > 0

                # if one of these keywords:
                && $token =~ /^(if|unless|while|until|for)$/

                # but do not break at something like '1 while'
                && ( $last_nonblank_type ne 'n' || $i > 2 )

                # and let keywords follow a closing 'do' brace
                && $last_nonblank_block_type ne 'do'

                && (
                    $is_long_line

                    # or container is broken (by side-comment, etc)
                    || (   $next_nonblank_token eq '('
                        && $mate_index_to_go[$i_next_nonblank] < $i )
                )
              )
            {
                set_forced_breakpoint( $i - 1 );
            }

            # remember locations of '||'  and '&&' for possible breaks if we
            # decide this is a long logical expression.
            if ( $type eq '||' ) {
                push @{ $rand_or_list[$depth][2] }, $i;
                ++$has_old_logical_breakpoints[$depth]
                  if ( ( $i == $i_line_start || $i == $i_line_end )
                    && $rOpts_break_at_old_logical_breakpoints );
            }
            elsif ( $type eq '&&' ) {
                push @{ $rand_or_list[$depth][3] }, $i;
                ++$has_old_logical_breakpoints[$depth]
                  if ( ( $i == $i_line_start || $i == $i_line_end )
                    && $rOpts_break_at_old_logical_breakpoints );
            }
            elsif ( $type eq 'f' ) {
                push @{ $rfor_semicolon_list[$depth] }, $i;
            }
            elsif ( $type eq 'k' ) {
                if ( $token eq 'and' ) {
                    push @{ $rand_or_list[$depth][1] }, $i;
                    ++$has_old_logical_breakpoints[$depth]
                      if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints );
                }

                # break immediately at 'or's which are probably not in a logical
                # block -- but we will break in logical breaks below so that
                # they do not add to the forced_breakpoint_count
                elsif ( $token eq 'or' ) {
                    push @{ $rand_or_list[$depth][0] }, $i;
                    ++$has_old_logical_breakpoints[$depth]
                      if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints );
                    if ( $is_logical_container{ $container_type[$depth] } ) {
                    }
                    else {
                        if ($is_long_line) { set_forced_breakpoint($i) }
                        elsif ( ( $i == $i_line_start || $i == $i_line_end )
                            && $rOpts_break_at_old_logical_breakpoints )
                        {
                            $saw_good_breakpoint = 1;
                        }
                    }
                }
                elsif ( $token eq 'if' || $token eq 'unless' ) {
                    push @{ $rand_or_list[$depth][4] }, $i;
                    if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints )
                    {
                        set_forced_breakpoint($i);
                    }
                }
            }
            elsif ( $is_assignment{$type} ) {
                $i_equals[$depth] = $i;
            }

            if ($type_sequence) {

                # handle any postponed closing breakpoints
                if ( $token =~ /^[\)\]\}\:]$/ ) {
                    if ( $type eq ':' ) {
                        $last_colon_sequence_number = $type_sequence;

                        # TESTING: retain break at a ':' line break
                        if ( ( $i == $i_line_start || $i == $i_line_end )
                            && $rOpts_break_at_old_ternary_breakpoints )
                        {

                            # TESTING:
                            set_forced_breakpoint($i);

                            # break at previous '='
                            if ( $i_equals[$depth] > 0 ) {
                                set_forced_breakpoint( $i_equals[$depth] );
                                $i_equals[$depth] = -1;
                            }
                        }
                    }
                    if ( defined( $postponed_breakpoint{$type_sequence} ) ) {
                        my $inc = ( $type eq ':' ) ? 0 : 1;
                        set_forced_breakpoint( $i - $inc );
                        delete $postponed_breakpoint{$type_sequence};
                    }
                }

                # set breaks at ?/: if they will get separated (and are
                # not a ?/: chain), or if the '?' is at the end of the
                # line
                elsif ( $token eq '?' ) {
                    my $i_colon = $mate_index_to_go[$i];
                    if (
                        $i_colon <= 0  # the ':' is not in this batch
                        || $i == 0     # this '?' is the first token of the line
                        || $i ==
                        $max_index_to_go    # or this '?' is the last token
                      )
                    {

                        # don't break at a '?' if preceded by ':' on
                        # this line of previous ?/: pair on this line.
                        # This is an attempt to preserve a chain of ?/:
                        # expressions (elsif2.t).  And don't break if
                        # this has a side comment.
                        set_forced_breakpoint($i)
                          unless (
                            $type_sequence == (
                                $last_colon_sequence_number +
                                  TYPE_SEQUENCE_INCREMENT
                            )
                            || $tokens_to_go[$max_index_to_go] eq '#'
                          );
                        set_closing_breakpoint($i);
                    }
                }
            }

#print "LISTX sees: i=$i type=$type  tok=$token  block=$block_type depth=$depth\n";

            #------------------------------------------------------------
            # Handle Increasing Depth..
            #
            # prepare for a new list when depth increases
            # token $i is a '(','{', or '['
            #------------------------------------------------------------
            if ( $depth > $current_depth ) {

                $breakpoint_stack[$depth]       = $forced_breakpoint_count;
                $breakpoint_undo_stack[$depth]  = $forced_breakpoint_undo_count;
                $has_broken_sublist[$depth]     = 0;
                $identifier_count_stack[$depth] = 0;
                $index_before_arrow[$depth]     = -1;
                $interrupted_list[$depth]       = 0;
                $item_count_stack[$depth]       = 0;
                $last_comma_index[$depth]       = undef;
                $last_dot_index[$depth]         = undef;
                $last_nonblank_type[$depth]     = $last_nonblank_type;
                $old_breakpoint_count_stack[$depth]    = $old_breakpoint_count;
                $opening_structure_index_stack[$depth] = $i;
                $rand_or_list[$depth]                  = [];
                $rfor_semicolon_list[$depth]           = [];
                $i_equals[$depth]                      = -1;
                $want_comma_break[$depth]              = 0;
                $container_type[$depth] =
                  ( $last_nonblank_type =~ /^(k|=>|&&|\|\||\?|\:|\.)$/ )
                  ? $last_nonblank_token
                  : "";
                $has_old_logical_breakpoints[$depth] = 0;

                # if line ends here then signal closing token to break
                if ( $next_nonblank_type eq 'b' || $next_nonblank_type eq '#' )
                {
                    set_closing_breakpoint($i);
                }

                # Not all lists of values should be vertically aligned..
                $dont_align[$depth] =

                  # code BLOCKS are handled at a higher level
                  ( $block_type ne "" )

                  # certain paren lists
                  || ( $type eq '(' ) && (

                    # it does not usually look good to align a list of
                    # identifiers in a parameter list, as in:
                    #    my($var1, $var2, ...)
                    # (This test should probably be refined, for now I'm just
                    # testing for any keyword)
                    ( $last_nonblank_type eq 'k' )

                    # a trailing '(' usually indicates a non-list
                    || ( $next_nonblank_type eq '(' )
                  );

                # patch to outdent opening brace of long if/for/..
                # statements (like this one).  See similar coding in
                # set_continuation breaks.  We have also catch it here for
                # short line fragments which otherwise will not go through
                # set_continuation_breaks.
                if (
                    $block_type

                    # if we have the ')' but not its '(' in this batch..
                    && ( $last_nonblank_token eq ')' )
                    && $mate_index_to_go[$i_last_nonblank_token] < 0

                    # and user wants brace to left
                    && !$rOpts->{'opening-brace-always-on-right'}

                    && ( $type  eq '{' )    # should be true
                    && ( $token eq '{' )    # should be true
                  )
                {
                    set_forced_breakpoint( $i - 1 );
                }
            }

            #------------------------------------------------------------
            # Handle Decreasing Depth..
            #
            # finish off any old list when depth decreases
            # token $i is a ')','}', or ']'
            #------------------------------------------------------------
            elsif ( $depth < $current_depth ) {

                check_for_new_minimum_depth($depth);

                # force all outer logical containers to break after we see on
                # old breakpoint
                $has_old_logical_breakpoints[$depth] ||=
                  $has_old_logical_breakpoints[$current_depth];

                # Patch to break between ') {' if the paren list is broken.
                # There is similar logic in set_continuation_breaks for
                # non-broken lists.
                if (   $token eq ')'
                    && $next_nonblank_block_type
                    && $interrupted_list[$current_depth]
                    && $next_nonblank_type eq '{'
                    && !$rOpts->{'opening-brace-always-on-right'} )
                {
                    set_forced_breakpoint($i);
                }

#print "LISTY sees: i=$i type=$type  tok=$token  block=$block_type depth=$depth next=$next_nonblank_type next_block=$next_nonblank_block_type inter=$interrupted_list[$current_depth]\n";

                # set breaks at commas if necessary
                my ( $bp_count, $do_not_break_apart ) =
                  set_comma_breakpoints($current_depth);

                my $i_opening = $opening_structure_index_stack[$current_depth];
                my $saw_opening_structure = ( $i_opening >= 0 );

                # this term is long if we had to break at interior commas..
                my $is_long_term = $bp_count > 0;

                # ..or if the length between opening and closing parens exceeds
                # allowed line length
                if ( !$is_long_term && $saw_opening_structure ) {
                    my $i_opening_minus = find_token_starting_list($i_opening);

                    # Note: we have to allow for one extra space after a
                    # closing token so that we do not strand a comma or
                    # semicolon, hence the '>=' here (oneline.t)
                    $is_long_term =
                      excess_line_length( $i_opening_minus, $i ) >= 0;
                }

                # We've set breaks after all comma-arrows.  Now we have to
                # undo them if this can be a one-line block
                # (the only breakpoints set will be due to comma-arrows)
                if (

                    # user doesn't require breaking after all comma-arrows
                    ( $rOpts_comma_arrow_breakpoints != 0 )

                    # and if the opening structure is in this batch
                    && $saw_opening_structure

                    # and either on the same old line
                    && (
                        $old_breakpoint_count_stack[$current_depth] ==
                        $last_old_breakpoint_count

                        # or user wants to form long blocks with arrows
                        || $rOpts_comma_arrow_breakpoints == 2
                    )

                  # and we made some breakpoints between the opening and closing
                    && ( $breakpoint_undo_stack[$current_depth] <
                        $forced_breakpoint_undo_count )

                    # and this block is short enough to fit on one line
                    # Note: use < because need 1 more space for possible comma
                    && !$is_long_term

                  )
                {
                    undo_forced_breakpoint_stack(
                        $breakpoint_undo_stack[$current_depth] );
                }

                # now see if we have any comma breakpoints left
                my $has_comma_breakpoints =
                  ( $breakpoint_stack[$current_depth] !=
                      $forced_breakpoint_count );

                # update broken-sublist flag of the outer container
                $has_broken_sublist[$depth] =
                     $has_broken_sublist[$depth]
                  || $has_broken_sublist[$current_depth]
                  || $is_long_term
                  || $has_comma_breakpoints;

# Having come to the closing ')', '}', or ']', now we have to decide if we
# should 'open up' the structure by placing breaks at the opening and
# closing containers.  This is a tricky decision.  Here are some of the
# basic considerations:
#
# -If this is a BLOCK container, then any breakpoints will have already
# been set (and according to user preferences), so we need do nothing here.
#
# -If we have a comma-separated list for which we can align the list items,
# then we need to do so because otherwise the vertical aligner cannot
# currently do the alignment.
#
# -If this container does itself contain a container which has been broken
# open, then it should be broken open to properly show the structure.
#
# -If there is nothing to align, and no other reason to break apart,
# then do not do it.
#
# We will not break open the parens of a long but 'simple' logical expression.
# For example:
#
# This is an example of a simple logical expression and its formatting:
#
#     if ( $bigwasteofspace1 && $bigwasteofspace2
#         || $bigwasteofspace3 && $bigwasteofspace4 )
#
# Most people would prefer this than the 'spacey' version:
#
#     if (
#         $bigwasteofspace1 && $bigwasteofspace2
#         || $bigwasteofspace3 && $bigwasteofspace4
#     )
#
# To illustrate the rules for breaking logical expressions, consider:
#
#             FULLY DENSE:
#             if ( $opt_excl
#                 and ( exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc ))
#
# This is on the verge of being difficult to read.  The current default is to
# open it up like this:
#
#             DEFAULT:
#             if (
#                 $opt_excl
#                 and ( exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc )
#               )
#
# This is a compromise which tries to avoid being too dense and to spacey.
# A more spaced version would be:
#
#             SPACEY:
#             if (
#                 $opt_excl
#                 and (
#                     exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc
#                 )
#               )
#
# Some people might prefer the spacey version -- an option could be added.  The
# innermost expression contains a long block '( exists $ids_...  ')'.
#
# Here is how the logic goes: We will force a break at the 'or' that the
# innermost expression contains, but we will not break apart its opening and
# closing containers because (1) it contains no multi-line sub-containers itself,
# and (2) there is no alignment to be gained by breaking it open like this
#
#             and (
#                 exists $ids_excl_uc{$id_uc}
#                 or grep $id_uc =~ /$_/, @ids_excl_uc
#             )
#
# (although this looks perfectly ok and might be good for long expressions).  The
# outer 'if' container, though, contains a broken sub-container, so it will be
# broken open to avoid too much density.  Also, since it contains no 'or's, there
# will be a forced break at its 'and'.

                # set some flags telling something about this container..
                my $is_simple_logical_expression = 0;
                if (   $item_count_stack[$current_depth] == 0
                    && $saw_opening_structure
                    && $tokens_to_go[$i_opening] eq '('
                    && $is_logical_container{ $container_type[$current_depth] }
                  )
                {

                    # This seems to be a simple logical expression with
                    # no existing breakpoints.  Set a flag to prevent
                    # opening it up.
                    if ( !$has_comma_breakpoints ) {
                        $is_simple_logical_expression = 1;
                    }

                    # This seems to be a simple logical expression with
                    # breakpoints (broken sublists, for example).  Break
                    # at all 'or's and '||'s.
                    else {
                        set_logical_breakpoints($current_depth);
                    }
                }

                if ( $is_long_term
                    && @{ $rfor_semicolon_list[$current_depth] } )
                {
                    set_for_semicolon_breakpoints($current_depth);

                    # open up a long 'for' or 'foreach' container to allow
                    # leading term alignment unless -lp is used.
                    $has_comma_breakpoints = 1
                      unless $rOpts_line_up_parentheses;
                }

                if (

                    # breaks for code BLOCKS are handled at a higher level
                    !$block_type

                    # we do not need to break at the top level of an 'if'
                    # type expression
                    && !$is_simple_logical_expression

                    ## modification to keep ': (' containers vertically tight;
                    ## but probably better to let user set -vt=1 to avoid
                    ## inconsistency with other paren types
                    ## && ($container_type[$current_depth] ne ':')

                    # otherwise, we require one of these reasons for breaking:
                    && (

                        # - this term has forced line breaks
                        $has_comma_breakpoints

                       # - the opening container is separated from this batch
                       #   for some reason (comment, blank line, code block)
                       # - this is a non-paren container spanning multiple lines
                        || !$saw_opening_structure

                        # - this is a long block contained in another breakable
                        #   container
                        || (   $is_long_term
                            && $container_environment_to_go[$i_opening] ne
                            'BLOCK' )
                    )
                  )
                {

                    # For -lp option, we must put a breakpoint before
                    # the token which has been identified as starting
                    # this indentation level.  This is necessary for
                    # proper alignment.
                    if ( $rOpts_line_up_parentheses && $saw_opening_structure )
                    {
                        my $item = $leading_spaces_to_go[ $i_opening + 1 ];
                        if (   $i_opening + 1 < $max_index_to_go
                            && $types_to_go[ $i_opening + 1 ] eq 'b' )
                        {
                            $item = $leading_spaces_to_go[ $i_opening + 2 ];
                        }
                        if ( defined($item) ) {
                            my $i_start_2 = $item->get_STARTING_INDEX();
                            if (
                                defined($i_start_2)

                                # we are breaking after an opening brace, paren,
                                # so don't break before it too
                                && $i_start_2 ne $i_opening
                              )
                            {

                                # Only break for breakpoints at the same
                                # indentation level as the opening paren
                                my $test1 = $nesting_depth_to_go[$i_opening];
                                my $test2 = $nesting_depth_to_go[$i_start_2];
                                if ( $test2 == $test1 ) {
                                    set_forced_breakpoint( $i_start_2 - 1 );
                                }
                            }
                        }
                    }

                    # break after opening structure.
                    # note: break before closing structure will be automatic
                    if ( $minimum_depth <= $current_depth ) {

                        set_forced_breakpoint($i_opening)
                          unless ( $do_not_break_apart
                            || is_unbreakable_container($current_depth) );

                        # break at '.' of lower depth level before opening token
                        if ( $last_dot_index[$depth] ) {
                            set_forced_breakpoint( $last_dot_index[$depth] );
                        }

                        # break before opening structure if preeced by another
                        # closing structure and a comma.  This is normally
                        # done by the previous closing brace, but not
                        # if it was a one-line block.
                        if ( $i_opening > 2 ) {
                            my $i_prev =
                              ( $types_to_go[ $i_opening - 1 ] eq 'b' )
                              ? $i_opening - 2
                              : $i_opening - 1;

                            if (   $types_to_go[$i_prev] eq ','
                                && $types_to_go[ $i_prev - 1 ] =~ /^[\)\}]$/ )
                            {
                                set_forced_breakpoint($i_prev);
                            }

                            # also break before something like ':('  or '?('
                            # if appropriate.
                            elsif (
                                $types_to_go[$i_prev] =~ /^([k\:\?]|&&|\|\|)$/ )
                            {
                                my $token_prev = $tokens_to_go[$i_prev];
                                if ( $want_break_before{$token_prev} ) {
                                    set_forced_breakpoint($i_prev);
                                }
                            }
                        }
                    }

                    # break after comma following closing structure
                    if ( $next_type eq ',' ) {
                        set_forced_breakpoint( $i + 1 );
                    }

                    # break before an '=' following closing structure
                    if (
                        $is_assignment{$next_nonblank_type}
                        && ( $breakpoint_stack[$current_depth] !=
                            $forced_breakpoint_count )
                      )
                    {
                        set_forced_breakpoint($i);
                    }

                    # break at any comma before the opening structure Added
                    # for -lp, but seems to be good in general.  It isn't
                    # obvious how far back to look; the '5' below seems to
                    # work well and will catch the comma in something like
                    #  push @list, myfunc( $param, $param, ..

                    my $icomma = $last_comma_index[$depth];
                    if ( defined($icomma) && ( $i_opening - $icomma ) < 5 ) {
                        unless ( $forced_breakpoint_to_go[$icomma] ) {
                            set_forced_breakpoint($icomma);
                        }
                    }
                }    # end logic to open up a container

                # Break open a logical container open if it was already open
                elsif ($is_simple_logical_expression
                    && $has_old_logical_breakpoints[$current_depth] )
                {
                    set_logical_breakpoints($current_depth);
                }

                # Handle long container which does not get opened up
                elsif ($is_long_term) {

                    # must set fake breakpoint to alert outer containers that
                    # they are complex
                    set_fake_breakpoint();
                }
            }

            #------------------------------------------------------------
            # Handle this token
            #------------------------------------------------------------

            $current_depth = $depth;

            # handle comma-arrow
            if ( $type eq '=>' ) {
                next if ( $last_nonblank_type eq '=>' );
                next if $rOpts_break_at_old_comma_breakpoints;
                next if $rOpts_comma_arrow_breakpoints == 3;
                $want_comma_break[$depth]   = 1;
                $index_before_arrow[$depth] = $i_last_nonblank_token;
                next;
            }

            elsif ( $type eq '.' ) {
                $last_dot_index[$depth] = $i;
            }

            # Turn off alignment if we are sure that this is not a list
            # environment.  To be safe, we will do this if we see certain
            # non-list tokens, such as ';', and also the environment is
            # not a list.  Note that '=' could be in any of the = operators
            # (lextest.t). We can't just use the reported environment
            # because it can be incorrect in some cases.
            elsif ( ( $type =~ /^[\;\<\>\~]$/ || $is_assignment{$type} )
                && $container_environment_to_go[$i] ne 'LIST' )
            {
                $dont_align[$depth]         = 1;
                $want_comma_break[$depth]   = 0;
                $index_before_arrow[$depth] = -1;
            }

            # now just handle any commas
            next unless ( $type eq ',' );

            $last_dot_index[$depth]   = undef;
            $last_comma_index[$depth] = $i;

            # break here if this comma follows a '=>'
            # but not if there is a side comment after the comma
            if ( $want_comma_break[$depth] ) {

                if ( $next_nonblank_type =~ /^[\)\}\]R]$/ ) {
                    $want_comma_break[$depth]   = 0;
                    $index_before_arrow[$depth] = -1;
                    next;
                }

                set_forced_breakpoint($i) unless ( $next_nonblank_type eq '#' );

                # break before the previous token if it looks safe
                # Example of something that we will not try to break before:
                #   DBI::SQL_SMALLINT() => $ado_consts->{adSmallInt},
                # Also we don't want to break at a binary operator (like +):
                # $c->createOval(
                #    $x + $R, $y +
                #    $R => $x - $R,
                #    $y - $R, -fill   => 'black',
                # );
                my $ibreak = $index_before_arrow[$depth] - 1;
                if (   $ibreak > 0
                    && $tokens_to_go[ $ibreak + 1 ] !~ /^[\)\}\]]$/ )
                {
                    if ( $tokens_to_go[$ibreak] eq '-' ) { $ibreak-- }
                    if ( $types_to_go[$ibreak]  eq 'b' ) { $ibreak-- }
                    if ( $types_to_go[$ibreak] =~ /^[,wiZCUG\(\{\[]$/ ) {

                        # don't break pointer calls, such as the following:
                        #  File::Spec->curdir  => 1,
                        # (This is tokenized as adjacent 'w' tokens)
                        if ( $tokens_to_go[ $ibreak + 1 ] !~ /^->/ ) {
                            set_forced_breakpoint($ibreak);
                        }
                    }
                }

                $want_comma_break[$depth]   = 0;
                $index_before_arrow[$depth] = -1;

                # handle list which mixes '=>'s and ','s:
                # treat any list items so far as an interrupted list
                $interrupted_list[$depth] = 1;
                next;
            }

            # break after all commas above starting depth
            if ( $depth < $starting_depth && !$dont_align[$depth] ) {
                set_forced_breakpoint($i) unless ( $next_nonblank_type eq '#' );
                next;
            }

            # add this comma to the list..
            my $item_count = $item_count_stack[$depth];
            if ( $item_count == 0 ) {

                # but do not form a list with no opening structure
                # for example:

                #            open INFILE_COPY, ">$input_file_copy"
                #              or die ("very long message");

                if ( ( $opening_structure_index_stack[$depth] < 0 )
                    && $container_environment_to_go[$i] eq 'BLOCK' )
                {
                    $dont_align[$depth] = 1;
                }
            }

            $comma_index[$depth][$item_count] = $i;
            ++$item_count_stack[$depth];
            if ( $last_nonblank_type =~ /^[iR\]]$/ ) {
                $identifier_count_stack[$depth]++;
            }
        }

        #-------------------------------------------
        # end of loop over all tokens in this batch
        #-------------------------------------------

        # set breaks for any unfinished lists ..
        for ( my $dd = $current_depth ; $dd >= $minimum_depth ; $dd-- ) {

            $interrupted_list[$dd] = 1;
            $has_broken_sublist[$dd] = 1 if ( $dd < $current_depth );
            set_comma_breakpoints($dd);
            set_logical_breakpoints($dd)
              if ( $has_old_logical_breakpoints[$dd] );
            set_for_semicolon_breakpoints($dd);

            # break open container...
            my $i_opening = $opening_structure_index_stack[$dd];
            set_forced_breakpoint($i_opening)
              unless (
                is_unbreakable_container($dd)

                # Avoid a break which would place an isolated ' or "
                # on a line
                || (   $type eq 'Q'
                    && $i_opening >= $max_index_to_go - 2
                    && $token =~ /^['"]$/ )
              );
        }

        # Return a flag indicating if the input file had some good breakpoints.
        # This flag will be used to force a break in a line shorter than the
        # allowed line length.
        if ( $has_old_logical_breakpoints[$current_depth] ) {
            $saw_good_breakpoint = 1;
        }
        return $saw_good_breakpoint;
    }
}    # end scan_list

sub find_token_starting_list {

    # When testing to see if a block will fit on one line, some
    # previous token(s) may also need to be on the line; particularly
    # if this is a sub call.  So we will look back at least one
    # token. NOTE: This isn't perfect, but not critical, because
    # if we mis-identify a block, it will be wrapped and therefore
    # fixed the next time it is formatted.
    my $i_opening_paren = shift;
    my $i_opening_minus = $i_opening_paren;
    my $im1             = $i_opening_paren - 1;
    my $im2             = $i_opening_paren - 2;
    my $im3             = $i_opening_paren - 3;
    my $typem1          = $types_to_go[$im1];
    my $typem2          = $im2 >= 0 ? $types_to_go[$im2] : 'b';
    if ( $typem1 eq ',' || ( $typem1 eq 'b' && $typem2 eq ',' ) ) {
        $i_opening_minus = $i_opening_paren;
    }
    elsif ( $tokens_to_go[$i_opening_paren] eq '(' ) {
        $i_opening_minus = $im1 if $im1 >= 0;

        # walk back to improve length estimate
        for ( my $j = $im1 ; $j >= 0 ; $j-- ) {
            last if ( $types_to_go[$j] =~ /^[\(\[\{L\}\]\)Rb,]$/ );
            $i_opening_minus = $j;
        }
        if ( $types_to_go[$i_opening_minus] eq 'b' ) { $i_opening_minus++ }
    }
    elsif ( $typem1 eq 'k' ) { $i_opening_minus = $im1 }
    elsif ( $typem1 eq 'b' && $im2 >= 0 && $types_to_go[$im2] eq 'k' ) {
        $i_opening_minus = $im2;
    }
    return $i_opening_minus;
}

{    # begin set_comma_breakpoints_do

    my %is_keyword_with_special_leading_term;

    BEGIN {

        # These keywords have prototypes which allow a special leading item
        # followed by a list
        @_ =
          qw(formline grep kill map printf sprintf push chmod join pack unshift);
        @is_keyword_with_special_leading_term{@_} = (1) x scalar(@_);
    }

    sub set_comma_breakpoints_do {

        # Given a list with some commas, set breakpoints at some of the
        # commas, if necessary, to make it easy to read.  This list is
        # an example:
        my (
            $depth,               $i_opening_paren,  $i_closing_paren,
            $item_count,          $identifier_count, $rcomma_index,
            $next_nonblank_type,  $list_type,        $interrupted,
            $rdo_not_break_apart, $must_break_open,
        ) = @_;

        # nothing to do if no commas seen
        return if ( $item_count < 1 );
        my $i_first_comma     = $$rcomma_index[0];
        my $i_true_last_comma = $$rcomma_index[ $item_count - 1 ];
        my $i_last_comma      = $i_true_last_comma;
        if ( $i_last_comma >= $max_index_to_go ) {
            $i_last_comma = $$rcomma_index[ --$item_count - 1 ];
            return if ( $item_count < 1 );
        }

        #---------------------------------------------------------------
        # find lengths of all items in the list to calculate page layout
        #---------------------------------------------------------------
        my $comma_count = $item_count;
        my @item_lengths;
        my @i_term_begin;
        my @i_term_end;
        my @i_term_comma;
        my $i_prev_plus;
        my @max_length = ( 0, 0 );
        my $first_term_length;
        my $i      = $i_opening_paren;
        my $is_odd = 1;

        for ( my $j = 0 ; $j < $comma_count ; $j++ ) {
            $is_odd      = 1 - $is_odd;
            $i_prev_plus = $i + 1;
            $i           = $$rcomma_index[$j];

            my $i_term_end =
              ( $types_to_go[ $i - 1 ] eq 'b' ) ? $i - 2 : $i - 1;
            my $i_term_begin =
              ( $types_to_go[$i_prev_plus] eq 'b' )
              ? $i_prev_plus + 1
              : $i_prev_plus;
            push @i_term_begin, $i_term_begin;
            push @i_term_end,   $i_term_end;
            push @i_term_comma, $i;

            # note: currently adding 2 to all lengths (for comma and space)
            my $length =
              2 + token_sequence_length( $i_term_begin, $i_term_end );
            push @item_lengths, $length;

            if ( $j == 0 ) {
                $first_term_length = $length;
            }
            else {

                if ( $length > $max_length[$is_odd] ) {
                    $max_length[$is_odd] = $length;
                }
            }
        }

        # now we have to make a distinction between the comma count and item
        # count, because the item count will be one greater than the comma
        # count if the last item is not terminated with a comma
        my $i_b =
          ( $types_to_go[ $i_last_comma + 1 ] eq 'b' )
          ? $i_last_comma + 1
          : $i_last_comma;
        my $i_e =
          ( $types_to_go[ $i_closing_paren - 1 ] eq 'b' )
          ? $i_closing_paren - 2
          : $i_closing_paren - 1;
        my $i_effective_last_comma = $i_last_comma;

        my $last_item_length = token_sequence_length( $i_b + 1, $i_e );

        if ( $last_item_length > 0 ) {

            # add 2 to length because other lengths include a comma and a blank
            $last_item_length += 2;
            push @item_lengths, $last_item_length;
            push @i_term_begin, $i_b + 1;
            push @i_term_end,   $i_e;
            push @i_term_comma, undef;

            my $i_odd = $item_count % 2;

            if ( $last_item_length > $max_length[$i_odd] ) {
                $max_length[$i_odd] = $last_item_length;
            }

            $item_count++;
            $i_effective_last_comma = $i_e + 1;

            if ( $types_to_go[ $i_b + 1 ] =~ /^[iR\]]$/ ) {
                $identifier_count++;
            }
        }

        #---------------------------------------------------------------
        # End of length calculations
        #---------------------------------------------------------------

        #---------------------------------------------------------------
        # Compound List Rule 1:
        # Break at (almost) every comma for a list containing a broken
        # sublist.  This has higher priority than the Interrupted List
        # Rule.
        #---------------------------------------------------------------
        if ( $has_broken_sublist[$depth] ) {

            # Break at every comma except for a comma between two
            # simple, small terms.  This prevents long vertical
            # columns of, say, just 0's.
            my $small_length = 10;    # 2 + actual maximum length wanted

            # We'll insert a break in long runs of small terms to
            # allow alignment in uniform tables.
            my $skipped_count = 0;
            my $columns       = table_columns_available($i_first_comma);
            my $fields        = int( $columns / $small_length );
            if (   $rOpts_maximum_fields_per_table
                && $fields > $rOpts_maximum_fields_per_table )
            {
                $fields = $rOpts_maximum_fields_per_table;
            }
            my $max_skipped_count = $fields - 1;

            my $is_simple_last_term = 0;
            my $is_simple_next_term = 0;
            foreach my $j ( 0 .. $item_count ) {
                $is_simple_last_term = $is_simple_next_term;
                $is_simple_next_term = 0;
                if (   $j < $item_count
                    && $i_term_end[$j] == $i_term_begin[$j]
                    && $item_lengths[$j] <= $small_length )
                {
                    $is_simple_next_term = 1;
                }
                next if $j == 0;
                if (   $is_simple_last_term
                    && $is_simple_next_term
                    && $skipped_count < $max_skipped_count )
                {
                    $skipped_count++;
                }
                else {
                    $skipped_count = 0;
                    my $i = $i_term_comma[ $j - 1 ];
                    last unless defined $i;
                    set_forced_breakpoint($i);
                }
            }

            # always break at the last comma if this list is
            # interrupted; we wouldn't want to leave a terminal '{', for
            # example.
            if ($interrupted) { set_forced_breakpoint($i_true_last_comma) }
            return;
        }

#my ( $a, $b, $c ) = caller();
#print "LISTX: in set_list $a $c interupt=$interrupted count=$item_count
#i_first = $i_first_comma  i_last=$i_last_comma max=$max_index_to_go\n";
#print "depth=$depth has_broken=$has_broken_sublist[$depth] is_multi=$is_multiline opening_paren=($i_opening_paren) \n";

        #---------------------------------------------------------------
        # Interrupted List Rule:
        # A list is is forced to use old breakpoints if it was interrupted
        # by side comments or blank lines, or requested by user.
        #---------------------------------------------------------------
        if (   $rOpts_break_at_old_comma_breakpoints
            || $interrupted
            || $i_opening_paren < 0 )
        {
            copy_old_breakpoints( $i_first_comma, $i_true_last_comma );
            return;
        }

        #---------------------------------------------------------------
        # Looks like a list of items.  We have to look at it and size it up.
        #---------------------------------------------------------------

        my $opening_token = $tokens_to_go[$i_opening_paren];
        my $opening_environment =
          $container_environment_to_go[$i_opening_paren];

        #-------------------------------------------------------------------
        # Return if this will fit on one line
        #-------------------------------------------------------------------

        my $i_opening_minus = find_token_starting_list($i_opening_paren);
        return
          unless excess_line_length( $i_opening_minus, $i_closing_paren ) > 0;

        #-------------------------------------------------------------------
        # Now we know that this block spans multiple lines; we have to set
        # at least one breakpoint -- real or fake -- as a signal to break
        # open any outer containers.
        #-------------------------------------------------------------------
        set_fake_breakpoint();

        # be sure we do not extend beyond the current list length
        if ( $i_effective_last_comma >= $max_index_to_go ) {
            $i_effective_last_comma = $max_index_to_go - 1;
        }

        # Set a flag indicating if we need to break open to keep -lp
        # items aligned.  This is necessary if any of the list terms
        # exceeds the available space after the '('.
        my $need_lp_break_open = $must_break_open;
        if ( $rOpts_line_up_parentheses && !$must_break_open ) {
            my $columns_if_unbroken = $rOpts_maximum_line_length -
              total_line_length( $i_opening_minus, $i_opening_paren );
            $need_lp_break_open =
                 ( $max_length[0] > $columns_if_unbroken )
              || ( $max_length[1] > $columns_if_unbroken )
              || ( $first_term_length > $columns_if_unbroken );
        }

        # Specify if the list must have an even number of fields or not.
        # It is generally safest to assume an even number, because the
        # list items might be a hash list.  But if we can be sure that
        # it is not a hash, then we can allow an odd number for more
        # flexibility.
        my $odd_or_even = 2;    # 1 = odd field count ok, 2 = want even count

        if (   $identifier_count >= $item_count - 1
            || $is_assignment{$next_nonblank_type}
            || ( $list_type && $list_type ne '=>' && $list_type !~ /^[\:\?]$/ )
          )
        {
            $odd_or_even = 1;
        }

        # do we have a long first term which should be
        # left on a line by itself?
        my $use_separate_first_term = (
            $odd_or_even == 1       # only if we can use 1 field/line
              && $item_count > 3    # need several items
              && $first_term_length >
              2 * $max_length[0] - 2    # need long first term
              && $first_term_length >
              2 * $max_length[1] - 2    # need long first term
        );

        # or do we know from the type of list that the first term should
        # be placed alone?
        if ( !$use_separate_first_term ) {
            if ( $is_keyword_with_special_leading_term{$list_type} ) {
                $use_separate_first_term = 1;

                # should the container be broken open?
                if ( $item_count < 3 ) {
                    if ( $i_first_comma - $i_opening_paren < 4 ) {
                        $$rdo_not_break_apart = 1;
                    }
                }
                elsif ($first_term_length < 20
                    && $i_first_comma - $i_opening_paren < 4 )
                {
                    my $columns = table_columns_available($i_first_comma);
                    if ( $first_term_length < $columns ) {
                        $$rdo_not_break_apart = 1;
                    }
                }
            }
        }

        # if so,
        if ($use_separate_first_term) {

            # ..set a break and update starting values
            $use_separate_first_term = 1;
            set_forced_breakpoint($i_first_comma);
            $i_opening_paren = $i_first_comma;
            $i_first_comma   = $$rcomma_index[1];
            $item_count--;
            return if $comma_count == 1;
            shift @item_lengths;
            shift @i_term_begin;
            shift @i_term_end;
            shift @i_term_comma;
        }

        # if not, update the metrics to include the first term
        else {
            if ( $first_term_length > $max_length[0] ) {
                $max_length[0] = $first_term_length;
            }
        }

        # Field width parameters
        my $pair_width = ( $max_length[0] + $max_length[1] );
        my $max_width =
          ( $max_length[0] > $max_length[1] ) ? $max_length[0] : $max_length[1];

        # Number of free columns across the page width for laying out tables
        my $columns = table_columns_available($i_first_comma);

        # Estimated maximum number of fields which fit this space
        # This will be our first guess
        my $number_of_fields_max =
          maximum_number_of_fields( $columns, $odd_or_even, $max_width,
            $pair_width );
        my $number_of_fields = $number_of_fields_max;

        # Find the best-looking number of fields
        # and make this our second guess if possible
        my ( $number_of_fields_best, $ri_ragged_break_list,
            $new_identifier_count )
          = study_list_complexity( \@i_term_begin, \@i_term_end, \@item_lengths,
            $max_width );

        if (   $number_of_fields_best != 0
            && $number_of_fields_best < $number_of_fields_max )
        {
            $number_of_fields = $number_of_fields_best;
        }

        # ----------------------------------------------------------------------
        # If we are crowded and the -lp option is being used, try to
        # undo some indentation
        # ----------------------------------------------------------------------
        if (
            $rOpts_line_up_parentheses
            && (
                $number_of_fields == 0
                || (   $number_of_fields == 1
                    && $number_of_fields != $number_of_fields_best )
            )
          )
        {
            my $available_spaces = get_AVAILABLE_SPACES_to_go($i_first_comma);
            if ( $available_spaces > 0 ) {

                my $spaces_wanted = $max_width - $columns;    # for 1 field

                if ( $number_of_fields_best == 0 ) {
                    $number_of_fields_best =
                      get_maximum_fields_wanted( \@item_lengths );
                }

                if ( $number_of_fields_best != 1 ) {
                    my $spaces_wanted_2 =
                      1 + $pair_width - $columns;             # for 2 fields
                    if ( $available_spaces > $spaces_wanted_2 ) {
                        $spaces_wanted = $spaces_wanted_2;
                    }
                }

                if ( $spaces_wanted > 0 ) {
                    my $deleted_spaces =
                      reduce_lp_indentation( $i_first_comma, $spaces_wanted );

                    # redo the math
                    if ( $deleted_spaces > 0 ) {
                        $columns = table_columns_available($i_first_comma);
                        $number_of_fields_max =
                          maximum_number_of_fields( $columns, $odd_or_even,
                            $max_width, $pair_width );
                        $number_of_fields = $number_of_fields_max;

                        if (   $number_of_fields_best == 1
                            && $number_of_fields >= 1 )
                        {
                            $number_of_fields = $number_of_fields_best;
                        }
                    }
                }
            }
        }

        # try for one column if two won't work
        if ( $number_of_fields <= 0 ) {
            $number_of_fields = int( $columns / $max_width );
        }

        # The user can place an upper bound on the number of fields,
        # which can be useful for doing maintenance on tables
        if (   $rOpts_maximum_fields_per_table
            && $number_of_fields > $rOpts_maximum_fields_per_table )
        {
            $number_of_fields = $rOpts_maximum_fields_per_table;
        }

        # How many columns (characters) and lines would this container take
        # if no additional whitespace were added?
        my $packed_columns = token_sequence_length( $i_opening_paren + 1,
            $i_effective_last_comma + 1 );
        if ( $columns <= 0 ) { $columns = 1 }    # avoid divide by zero
        my $packed_lines = 1 + int( $packed_columns / $columns );

        # are we an item contained in an outer list?
        my $in_hierarchical_list = $next_nonblank_type =~ /^[\}\,]$/;

        if ( $number_of_fields <= 0 ) {

#         #---------------------------------------------------------------
#         # We're in trouble.  We can't find a single field width that works.
#         # There is no simple answer here; we may have a single long list
#         # item, or many.
#         #---------------------------------------------------------------
#
#         In many cases, it may be best to not force a break if there is just one
#         comma, because the standard continuation break logic will do a better
#         job without it.
#
#         In the common case that all but one of the terms can fit
#         on a single line, it may look better not to break open the
#         containing parens.  Consider, for example
#
#             $color =
#               join ( '/',
#                 sort { $color_value{$::a} <=> $color_value{$::b}; }
#                 keys %colors );
#
#         which will look like this with the container broken:
#
#             $color = join (
#                 '/',
#                 sort { $color_value{$::a} <=> $color_value{$::b}; } keys %colors
#             );
#
#         Here is an example of this rule for a long last term:
#
#             log_message( 0, 256, 128,
#                 "Number of routes in adj-RIB-in to be considered: $peercount" );
#
#         And here is an example with a long first term:
#
#         $s = sprintf(
# "%2d wallclock secs (%$f usr %$f sys + %$f cusr %$f csys = %$f CPU)",
#             $r, $pu, $ps, $cu, $cs, $tt
#           )
#           if $style eq 'all';

            my $i_last_comma = $$rcomma_index[ $comma_count - 1 ];
            my $long_last_term = excess_line_length( 0, $i_last_comma ) <= 0;
            my $long_first_term =
              excess_line_length( $i_first_comma + 1, $max_index_to_go ) <= 0;

            # break at every comma ...
            if (

                # if requested by user or is best looking
                $number_of_fields_best == 1

                # or if this is a sublist of a larger list
                || $in_hierarchical_list

                # or if multiple commas and we dont have a long first or last
                # term
                || ( $comma_count > 1
                    && !( $long_last_term || $long_first_term ) )
              )
            {
                foreach ( 0 .. $comma_count - 1 ) {
                    set_forced_breakpoint( $$rcomma_index[$_] );
                }
            }
            elsif ($long_last_term) {

                set_forced_breakpoint($i_last_comma);
                $$rdo_not_break_apart = 1 unless $must_break_open;
            }
            elsif ($long_first_term) {

                set_forced_breakpoint($i_first_comma);
            }
            else {

                # let breaks be defined by default bond strength logic
            }
            return;
        }

        # --------------------------------------------------------
        # We have a tentative field count that seems to work.
        # How many lines will this require?
        # --------------------------------------------------------
        my $formatted_lines = $item_count / ($number_of_fields);
        if ( $formatted_lines != int $formatted_lines ) {
            $formatted_lines = 1 + int $formatted_lines;
        }

        # So far we've been trying to fill out to the right margin.  But
        # compact tables are easier to read, so let's see if we can use fewer
        # fields without increasing the number of lines.
        $number_of_fields =
          compactify_table( $item_count, $number_of_fields, $formatted_lines,
            $odd_or_even );

        # How many spaces across the page will we fill?
        my $columns_per_line =
          ( int $number_of_fields / 2 ) * $pair_width +
          ( $number_of_fields % 2 ) * $max_width;

        my $formatted_columns;

        if ( $number_of_fields > 1 ) {
            $formatted_columns =
              ( $pair_width * ( int( $item_count / 2 ) ) +
                  ( $item_count % 2 ) * $max_width );
        }
        else {
            $formatted_columns = $max_width * $item_count;
        }
        if ( $formatted_columns < $packed_columns ) {
            $formatted_columns = $packed_columns;
        }

        my $unused_columns = $formatted_columns - $packed_columns;

        # set some empirical parameters to help decide if we should try to
        # align; high sparsity does not look good, especially with few lines
        my $sparsity = ($unused_columns) / ($formatted_columns);
        my $max_allowed_sparsity =
            ( $item_count < 3 )    ? 0.1
          : ( $packed_lines == 1 ) ? 0.15
          : ( $packed_lines == 2 ) ? 0.4
          :                          0.7;

        # Begin check for shortcut methods, which avoid treating a list
        # as a table for relatively small parenthesized lists.  These
        # are usually easier to read if not formatted as tables.
        if (
            $packed_lines <= 2    # probably can fit in 2 lines
            && $item_count < 9    # doesn't have too many items
            && $opening_environment eq 'BLOCK'    # not a sub-container
            && $opening_token       eq '('        # is paren list
          )
        {

            # Shortcut method 1: for -lp and just one comma:
            # This is a no-brainer, just break at the comma.
            if (
                $rOpts_line_up_parentheses        # -lp
                && $item_count == 2               # two items, one comma
                && !$must_break_open
              )
            {
                my $i_break = $$rcomma_index[0];
                set_forced_breakpoint($i_break);
                $$rdo_not_break_apart = 1;
                set_non_alignment_flags( $comma_count, $rcomma_index );
                return;

            }

            # method 2 is for most small ragged lists which might look
            # best if not displayed as a table.
            if (
                ( $number_of_fields == 2 && $item_count == 3 )
                || (
                    $new_identifier_count > 0    # isn't all quotes
                    && $sparsity > 0.15
                )    # would be fairly spaced gaps if aligned
              )
            {

                my $break_count = set_ragged_breakpoints( \@i_term_comma,
                    $ri_ragged_break_list );
                ++$break_count if ($use_separate_first_term);

                # NOTE: we should really use the true break count here,
                # which can be greater if there are large terms and
                # little space, but usually this will work well enough.
                unless ($must_break_open) {

                    if ( $break_count <= 1 ) {
                        $$rdo_not_break_apart = 1;
                    }
                    elsif ( $rOpts_line_up_parentheses && !$need_lp_break_open )
                    {
                        $$rdo_not_break_apart = 1;
                    }
                }
                set_non_alignment_flags( $comma_count, $rcomma_index );
                return;
            }

        }    # end shortcut methods

        # debug stuff

        FORMATTER_DEBUG_FLAG_SPARSE && do {
            print
"SPARSE:cols=$columns commas=$comma_count items:$item_count ids=$identifier_count pairwidth=$pair_width fields=$number_of_fields lines packed: $packed_lines packed_cols=$packed_columns fmtd:$formatted_lines cols /line:$columns_per_line  unused:$unused_columns fmtd:$formatted_columns sparsity=$sparsity allow=$max_allowed_sparsity\n";

        };

        #---------------------------------------------------------------
        # Compound List Rule 2:
        # If this list is too long for one line, and it is an item of a
        # larger list, then we must format it, regardless of sparsity
        # (ian.t).  One reason that we have to do this is to trigger
        # Compound List Rule 1, above, which causes breaks at all commas of
        # all outer lists.  In this way, the structure will be properly
        # displayed.
        #---------------------------------------------------------------

        # Decide if this list is too long for one line unless broken
        my $total_columns = table_columns_available($i_opening_paren);
        my $too_long      = $packed_columns > $total_columns;

        # For a paren list, include the length of the token just before the
        # '(' because this is likely a sub call, and we would have to
        # include the sub name on the same line as the list.  This is still
        # imprecise, but not too bad.  (steve.t)
        if ( !$too_long && $i_opening_paren > 0 && $opening_token eq '(' ) {

            $too_long = excess_line_length( $i_opening_minus,
                $i_effective_last_comma + 1 ) > 0;
        }

        # FIXME: For an item after a '=>', try to include the length of the
        # thing before the '=>'.  This is crude and should be improved by
        # actually looking back token by token.
        if ( !$too_long && $i_opening_paren > 0 && $list_type eq '=>' ) {
            my $i_opening_minus = $i_opening_paren - 4;
            if ( $i_opening_minus >= 0 ) {
                $too_long = excess_line_length( $i_opening_minus,
                    $i_effective_last_comma + 1 ) > 0;
            }
        }

        # Always break lists contained in '[' and '{' if too long for 1 line,
        # and always break lists which are too long and part of a more complex
        # structure.
        my $must_break_open_container = $must_break_open
          || ( $too_long
            && ( $in_hierarchical_list || $opening_token ne '(' ) );

#print "LISTX: next=$next_nonblank_type  avail cols=$columns packed=$packed_columns must format = $must_break_open_container too-long=$too_long  opening=$opening_token list_type=$list_type formatted_lines=$formatted_lines  packed=$packed_lines max_sparsity= $max_allowed_sparsity sparsity=$sparsity \n";

        #---------------------------------------------------------------
        # The main decision:
        # Now decide if we will align the data into aligned columns.  Do not
        # attempt to align columns if this is a tiny table or it would be
        # too spaced.  It seems that the more packed lines we have, the
        # sparser the list that can be allowed and still look ok.
        #---------------------------------------------------------------

        if (   ( $formatted_lines < 3 && $packed_lines < $formatted_lines )
            || ( $formatted_lines < 2 )
            || ( $unused_columns > $max_allowed_sparsity * $formatted_columns )
          )
        {

            #---------------------------------------------------------------
            # too sparse: would look ugly if aligned in a table;
            #---------------------------------------------------------------

            # use old breakpoints if this is a 'big' list
            # FIXME: goal is to improve set_ragged_breakpoints so that
            # this is not necessary.
            if ( $packed_lines > 2 && $item_count > 10 ) {
                write_logfile_entry("List sparse: using old breakpoints\n");
                copy_old_breakpoints( $i_first_comma, $i_last_comma );
            }

            # let the continuation logic handle it if 2 lines
            else {

                my $break_count = set_ragged_breakpoints( \@i_term_comma,
                    $ri_ragged_break_list );
                ++$break_count if ($use_separate_first_term);

                unless ($must_break_open_container) {
                    if ( $break_count <= 1 ) {
                        $$rdo_not_break_apart = 1;
                    }
                    elsif ( $rOpts_line_up_parentheses && !$need_lp_break_open )
                    {
                        $$rdo_not_break_apart = 1;
                    }
                }
                set_non_alignment_flags( $comma_count, $rcomma_index );
            }
            return;
        }

        #---------------------------------------------------------------
        # go ahead and format as a table
        #---------------------------------------------------------------
        write_logfile_entry(
            "List: auto formatting with $number_of_fields fields/row\n");

        my $j_first_break =
          $use_separate_first_term ? $number_of_fields : $number_of_fields - 1;

        for (
            my $j = $j_first_break ;
            $j < $comma_count ;
            $j += $number_of_fields
          )
        {
            my $i = $$rcomma_index[$j];
            set_forced_breakpoint($i);
        }
        return;
    }
}

sub set_non_alignment_flags {

    # set flag which indicates that these commas should not be
    # aligned
    my ( $comma_count, $rcomma_index ) = @_;
    foreach ( 0 .. $comma_count - 1 ) {
        $matching_token_to_go[ $$rcomma_index[$_] ] = 1;
    }
}

sub study_list_complexity {

    # Look for complex tables which should be formatted with one term per line.
    # Returns the following:
    #
    #  \@i_ragged_break_list = list of good breakpoints to avoid lines
    #    which are hard to read
    #  $number_of_fields_best = suggested number of fields based on
    #    complexity; = 0 if any number may be used.
    #
    my ( $ri_term_begin, $ri_term_end, $ritem_lengths, $max_width ) = @_;
    my $item_count            = @{$ri_term_begin};
    my $complex_item_count    = 0;
    my $number_of_fields_best = $rOpts_maximum_fields_per_table;
    my $i_max                 = @{$ritem_lengths} - 1;
    ##my @item_complexity;

    my $i_last_last_break = -3;
    my $i_last_break      = -2;
    my @i_ragged_break_list;

    my $definitely_complex = 30;
    my $definitely_simple  = 12;
    my $quote_count        = 0;

    for my $i ( 0 .. $i_max ) {
        my $ib = $ri_term_begin->[$i];
        my $ie = $ri_term_end->[$i];

        # define complexity: start with the actual term length
        my $weighted_length = ( $ritem_lengths->[$i] - 2 );

        ##TBD: join types here and check for variations
        ##my $str=join "", @tokens_to_go[$ib..$ie];

        my $is_quote = 0;
        if ( $types_to_go[$ib] =~ /^[qQ]$/ ) {
            $is_quote = 1;
            $quote_count++;
        }
        elsif ( $types_to_go[$ib] =~ /^[w\-]$/ ) {
            $quote_count++;
        }

        if ( $ib eq $ie ) {
            if ( $is_quote && $tokens_to_go[$ib] =~ /\s/ ) {
                $complex_item_count++;
                $weighted_length *= 2;
            }
            else {
            }
        }
        else {
            if ( grep { $_ eq 'b' } @types_to_go[ $ib .. $ie ] ) {
                $complex_item_count++;
                $weighted_length *= 2;
            }
            if ( grep { $_ eq '..' } @types_to_go[ $ib .. $ie ] ) {
                $weighted_length += 4;
            }
        }

        # add weight for extra tokens.
        $weighted_length += 2 * ( $ie - $ib );

##        my $BUB = join '', @tokens_to_go[$ib..$ie];
##        print "# COMPLEXITY:$weighted_length   $BUB\n";

##push @item_complexity, $weighted_length;

        # now mark a ragged break after this item it if it is 'long and
        # complex':
        if ( $weighted_length >= $definitely_complex ) {

            # if we broke after the previous term
            # then break before it too
            if (   $i_last_break == $i - 1
                && $i > 1
                && $i_last_last_break != $i - 2 )
            {

                ## FIXME: don't strand a small term
                pop @i_ragged_break_list;
                push @i_ragged_break_list, $i - 2;
                push @i_ragged_break_list, $i - 1;
            }

            push @i_ragged_break_list, $i;
            $i_last_last_break = $i_last_break;
            $i_last_break      = $i;
        }

        # don't break before a small last term -- it will
        # not look good on a line by itself.
        elsif ($i == $i_max
            && $i_last_break == $i - 1
            && $weighted_length <= $definitely_simple )
        {
            pop @i_ragged_break_list;
        }
    }

    my $identifier_count = $i_max + 1 - $quote_count;

    # Need more tuning here..
    if (   $max_width > 12
        && $complex_item_count > $item_count / 2
        && $number_of_fields_best != 2 )
    {
        $number_of_fields_best = 1;
    }

    return ( $number_of_fields_best, \@i_ragged_break_list, $identifier_count );
}

sub get_maximum_fields_wanted {

    # Not all tables look good with more than one field of items.
    # This routine looks at a table and decides if it should be
    # formatted with just one field or not.
    # This coding is still under development.
    my ($ritem_lengths) = @_;

    my $number_of_fields_best = 0;

    # For just a few items, we tentatively assume just 1 field.
    my $item_count = @{$ritem_lengths};
    if ( $item_count <= 5 ) {
        $number_of_fields_best = 1;
    }

    # For larger tables, look at it both ways and see what looks best
    else {

        my $is_odd            = 1;
        my @max_length        = ( 0, 0 );
        my @last_length_2     = ( undef, undef );
        my @first_length_2    = ( undef, undef );
        my $last_length       = undef;
        my $total_variation_1 = 0;
        my $total_variation_2 = 0;
        my @total_variation_2 = ( 0, 0 );
        for ( my $j = 0 ; $j < $item_count ; $j++ ) {

            $is_odd = 1 - $is_odd;
            my $length = $ritem_lengths->[$j];
            if ( $length > $max_length[$is_odd] ) {
                $max_length[$is_odd] = $length;
            }

            if ( defined($last_length) ) {
                my $dl = abs( $length - $last_length );
                $total_variation_1 += $dl;
            }
            $last_length = $length;

            my $ll = $last_length_2[$is_odd];
            if ( defined($ll) ) {
                my $dl = abs( $length - $ll );
                $total_variation_2[$is_odd] += $dl;
            }
            else {
                $first_length_2[$is_odd] = $length;
            }
            $last_length_2[$is_odd] = $length;
        }
        $total_variation_2 = $total_variation_2[0] + $total_variation_2[1];

        my $factor = ( $item_count > 10 ) ? 1 : ( $item_count > 5 ) ? 0.75 : 0;
        unless ( $total_variation_2 < $factor * $total_variation_1 ) {
            $number_of_fields_best = 1;
        }
    }
    return ($number_of_fields_best);
}

sub table_columns_available {
    my $i_first_comma = shift;
    my $columns =
      $rOpts_maximum_line_length - leading_spaces_to_go($i_first_comma);

    # Patch: the vertical formatter does not line up lines whose lengths
    # exactly equal the available line length because of allowances
    # that must be made for side comments.  Therefore, the number of
    # available columns is reduced by 1 character.
    $columns -= 1;
    return $columns;
}

sub maximum_number_of_fields {

    # how many fields will fit in the available space?
    my ( $columns, $odd_or_even, $max_width, $pair_width ) = @_;
    my $max_pairs        = int( $columns / $pair_width );
    my $number_of_fields = $max_pairs * 2;
    if (   $odd_or_even == 1
        && $max_pairs * $pair_width + $max_width <= $columns )
    {
        $number_of_fields++;
    }
    return $number_of_fields;
}

sub compactify_table {

    # given a table with a certain number of fields and a certain number
    # of lines, see if reducing the number of fields will make it look
    # better.
    my ( $item_count, $number_of_fields, $formatted_lines, $odd_or_even ) = @_;
    if ( $number_of_fields >= $odd_or_even * 2 && $formatted_lines > 0 ) {
        my $min_fields;

        for (
            $min_fields = $number_of_fields ;
            $min_fields >= $odd_or_even
            && $min_fields * $formatted_lines >= $item_count ;
            $min_fields -= $odd_or_even
          )
        {
            $number_of_fields = $min_fields;
        }
    }
    return $number_of_fields;
}

sub set_ragged_breakpoints {

    # Set breakpoints in a list that cannot be formatted nicely as a
    # table.
    my ( $ri_term_comma, $ri_ragged_break_list ) = @_;

    my $break_count = 0;
    foreach (@$ri_ragged_break_list) {
        my $j = $ri_term_comma->[$_];
        if ($j) {
            set_forced_breakpoint($j);
            $break_count++;
        }
    }
    return $break_count;
}

sub copy_old_breakpoints {
    my ( $i_first_comma, $i_last_comma ) = @_;
    for my $i ( $i_first_comma .. $i_last_comma ) {
        if ( $old_breakpoint_to_go[$i] ) {
            set_forced_breakpoint($i);
        }
    }
}

sub set_nobreaks {
    my ( $i, $j ) = @_;
    if ( $i >= 0 && $i <= $j && $j <= $max_index_to_go ) {

        FORMATTER_DEBUG_FLAG_NOBREAK && do {
            my ( $a, $b, $c ) = caller();
            print(
"NOBREAK: forced_breakpoint $forced_breakpoint_count from $a $c with i=$i max=$max_index_to_go type=$types_to_go[$i]\n"
            );
        };

        @nobreak_to_go[ $i .. $j ] = (1) x ( $j - $i + 1 );
    }

    # shouldn't happen; non-critical error
    else {
        FORMATTER_DEBUG_FLAG_NOBREAK && do {
            my ( $a, $b, $c ) = caller();
            print(
"NOBREAK ERROR: from $a $c with i=$i j=$j max=$max_index_to_go\n"
            );
        };
    }
}

sub set_fake_breakpoint {

    # Just bump up the breakpoint count as a signal that there are breaks.
    # This is useful if we have breaks but may want to postpone deciding where
    # to make them.
    $forced_breakpoint_count++;
}

sub set_forced_breakpoint {
    my $i = shift;

    return unless defined $i && $i >= 0;

    # when called with certain tokens, use bond strengths to decide
    # if we break before or after it
    my $token = $tokens_to_go[$i];

    if ( $token =~ /^([\=\.\,\:\?]|and|or|xor|&&|\|\|)$/ ) {
        if ( $want_break_before{$token} && $i >= 0 ) { $i-- }
    }

    # breaks are forced before 'if' and 'unless'
    elsif ( $is_if_unless{$token} ) { $i-- }

    if ( $i >= 0 && $i <= $max_index_to_go ) {
        my $i_nonblank = ( $types_to_go[$i] ne 'b' ) ? $i : $i - 1;

        FORMATTER_DEBUG_FLAG_FORCE && do {
            my ( $a, $b, $c ) = caller();
            print
"FORCE forced_breakpoint $forced_breakpoint_count from $a $c with i=$i_nonblank max=$max_index_to_go tok=$tokens_to_go[$i_nonblank] type=$types_to_go[$i_nonblank] nobr=$nobreak_to_go[$i_nonblank]\n";
        };

        if ( $i_nonblank >= 0 && $nobreak_to_go[$i_nonblank] == 0 ) {
            $forced_breakpoint_to_go[$i_nonblank] = 1;

            if ( $i_nonblank > $index_max_forced_break ) {
                $index_max_forced_break = $i_nonblank;
            }
            $forced_breakpoint_count++;
            $forced_breakpoint_undo_stack[ $forced_breakpoint_undo_count++ ] =
              $i_nonblank;

            # if we break at an opening container..break at the closing
            if ( $tokens_to_go[$i_nonblank] =~ /^[\{\[\(\?]$/ ) {
                set_closing_breakpoint($i_nonblank);
            }
        }
    }
}

sub clear_breakpoint_undo_stack {
    $forced_breakpoint_undo_count = 0;
}

sub undo_forced_breakpoint_stack {

    my $i_start = shift;
    if ( $i_start < 0 ) {
        $i_start = 0;
        my ( $a, $b, $c ) = caller();
        warning(
"Program Bug: undo_forced_breakpoint_stack from $a $c has i=$i_start "
        );
    }

    while ( $forced_breakpoint_undo_count > $i_start ) {
        my $i =
          $forced_breakpoint_undo_stack[ --$forced_breakpoint_undo_count ];
        if ( $i >= 0 && $i <= $max_index_to_go ) {
            $forced_breakpoint_to_go[$i] = 0;
            $forced_breakpoint_count--;

            FORMATTER_DEBUG_FLAG_UNDOBP && do {
                my ( $a, $b, $c ) = caller();
                print(
"UNDOBP: undo forced_breakpoint i=$i $forced_breakpoint_undo_count from $a $c max=$max_index_to_go\n"
                );
            };
        }

        # shouldn't happen, but not a critical error
        else {
            FORMATTER_DEBUG_FLAG_UNDOBP && do {
                my ( $a, $b, $c ) = caller();
                print(
"Program Bug: undo_forced_breakpoint from $a $c has i=$i but max=$max_index_to_go"
                );
            };
        }
    }
}

{    # begin recombine_breakpoints

    my %is_amp_amp;
    my %is_ternary;
    my %is_math_op;

    BEGIN {

        @_ = qw( && || );
        @is_amp_amp{@_} = (1) x scalar(@_);

        @_ = qw( ? : );
        @is_ternary{@_} = (1) x scalar(@_);

        @_ = qw( + - * / );
        @is_math_op{@_} = (1) x scalar(@_);
    }

    sub recombine_breakpoints {

        # sub set_continuation_breaks is very liberal in setting line breaks
        # for long lines, always setting breaks at good breakpoints, even
        # when that creates small lines.  Occasionally small line fragments
        # are produced which would look better if they were combined.
        # That's the task of this routine, recombine_breakpoints.
        #
        # $ri_beg = ref to array of BEGinning indexes of each line
        # $ri_end = ref to array of ENDing indexes of each line
        my ( $ri_beg, $ri_end ) = @_;

        my $more_to_do = 1;

        # We keep looping over all of the lines of this batch
        # until there are no more possible recombinations
        my $nmax_last = @$ri_end;
        while ($more_to_do) {
            my $n_best = 0;
            my $bs_best;
            my $n;
            my $nmax = @$ri_end - 1;

            # safety check for infinite loop
            unless ( $nmax < $nmax_last ) {

            # shouldn't happen because splice below decreases nmax on each pass:
            # but i get paranoid sometimes
                die "Program bug-infinite loop in recombine breakpoints\n";
            }
            $nmax_last  = $nmax;
            $more_to_do = 0;
            my $previous_outdentable_closing_paren;
            my $leading_amp_count = 0;
            my $this_line_is_semicolon_terminated;

            # loop over all remaining lines in this batch
            for $n ( 1 .. $nmax ) {

                #----------------------------------------------------------
                # If we join the current pair of lines,
                # line $n-1 will become the left part of the joined line
                # line $n will become the right part of the joined line
                #
                # Here are Indexes of the endpoint tokens of the two lines:
                #
                #  -----line $n-1--- | -----line $n-----
                #  $ibeg_1   $iend_1 | $ibeg_2   $iend_2
                #                    ^
                #                    |
                # We want to decide if we should remove the line break
                # betwen the tokens at $iend_1 and $ibeg_2
                #
                # We will apply a number of ad-hoc tests to see if joining
                # here will look ok.  The code will just issue a 'next'
                # command if the join doesn't look good.  If we get through
                # the gauntlet of tests, the lines will be recombined.
                #----------------------------------------------------------
                #
                # beginning and ending tokens of the lines we are working on
                my $ibeg_1 = $$ri_beg[ $n - 1 ];
                my $iend_1 = $$ri_end[ $n - 1 ];
                my $iend_2 = $$ri_end[$n];
                my $ibeg_2 = $$ri_beg[$n];

                my $ibeg_nmax = $$ri_beg[$nmax];

                # some beginning indexes of other lines, which may not exist
                my $ibeg_0 = $n > 1          ? $$ri_beg[ $n - 2 ] : -1;
                my $ibeg_3 = $n < $nmax      ? $$ri_beg[ $n + 1 ] : -1;
                my $ibeg_4 = $n + 2 <= $nmax ? $$ri_beg[ $n + 2 ] : -1;

                my $bs_tweak = 0;

                #my $depth_increase=( $nesting_depth_to_go[$ibeg_2] -
                #        $nesting_depth_to_go[$ibeg_1] );

##print "RECOMBINE: n=$n imid=$iend_1 if=$ibeg_1 type=$types_to_go[$ibeg_1] =$tokens_to_go[$ibeg_1] next_type=$types_to_go[$ibeg_2] next_tok=$tokens_to_go[$ibeg_2]\n";

                # If line $n is the last line, we set some flags and
                # do any special checks for it
                if ( $n == $nmax ) {

                    # a terminal '{' should stay where it is
                    next if $types_to_go[$ibeg_2] eq '{';

                    # set flag if statement $n ends in ';'
                    $this_line_is_semicolon_terminated =
                      $types_to_go[$iend_2] eq ';'

                      # with possible side comment
                      || ( $types_to_go[$iend_2] eq '#'
                        && $iend_2 - $ibeg_2 >= 2
                        && $types_to_go[ $iend_2 - 2 ] eq ';'
                        && $types_to_go[ $iend_2 - 1 ] eq 'b' );
                }

                #----------------------------------------------------------
                # Section 1: examine token at $iend_1 (right end of first line
                # of pair)
                #----------------------------------------------------------

                # an isolated '}' may join with a ';' terminated segment
                if ( $types_to_go[$iend_1] eq '}' ) {

                    # Check for cases where combining a semicolon terminated
                    # statement with a previous isolated closing paren will
                    # allow the combined line to be outdented.  This is
                    # generally a good move.  For example, we can join up
                    # the last two lines here:
                    #  (
                    #      $dev,  $ino,   $mode,  $nlink, $uid,     $gid, $rdev,
                    #      $size, $atime, $mtime, $ctime, $blksize, $blocks
                    #    )
                    #    = stat($file);
                    #
                    # to get:
                    #  (
                    #      $dev,  $ino,   $mode,  $nlink, $uid,     $gid, $rdev,
                    #      $size, $atime, $mtime, $ctime, $blksize, $blocks
                    #  ) = stat($file);
                    #
                    # which makes the parens line up.
                    #
                    # Another example, from Joe Matarazzo, probably looks best
                    # with the 'or' clause appended to the trailing paren:
                    #  $self->some_method(
                    #      PARAM1 => 'foo',
                    #      PARAM2 => 'bar'
                    #  ) or die "Some_method didn't work";
                    #
                    $previous_outdentable_closing_paren =
                      $this_line_is_semicolon_terminated    # ends in ';'
                      && $ibeg_1 == $iend_1    # only one token on last line
                      && $tokens_to_go[$iend_1] eq
                      ')'                      # must be structural paren

                      # only &&, ||, and : if no others seen
                      # (but note: our count made below could be wrong
                      # due to intervening comments)
                      && ( $leading_amp_count == 0
                        || $types_to_go[$ibeg_2] !~ /^(:|\&\&|\|\|)$/ )

                      # but leading colons probably line up with with a
                      # previous colon or question (count could be wrong).
                      && $types_to_go[$ibeg_2] ne ':'

                      # only one step in depth allowed.  this line must not
                      # begin with a ')' itself.
                      && ( $nesting_depth_to_go[$iend_1] ==
                        $nesting_depth_to_go[$iend_2] + 1 );

                    next
                      unless (
                        $previous_outdentable_closing_paren

                        # handle '.' and '?' specially below
                        || ( $types_to_go[$ibeg_2] =~ /^[\.\?]$/ )
                      );
                }

                # do not recombine lines with ending &&, ||,
                elsif ( $is_amp_amp{ $types_to_go[$iend_1] } ) {
                    next unless $want_break_before{ $types_to_go[$iend_1] };
                }

                # keep a terminal colon
                elsif ( $types_to_go[$iend_1] eq ':' ) {
                    next unless $want_break_before{ $types_to_go[$iend_1] };
                }

                # Identify and recombine a broken ?/: chain
                elsif ( $types_to_go[$iend_1] eq '?' ) {

                    # Do not recombine different levels
                    next
                      if ( $levels_to_go[$ibeg_1] ne $levels_to_go[$ibeg_2] );

                    # do not recombine unless next line ends in :
                    next unless $types_to_go[$iend_2] eq ':';
                }

                # for lines ending in a comma...
                elsif ( $types_to_go[$iend_1] eq ',' ) {

                    # Do not recombine at comma which is following the
                    # input bias.
                    # TODO: might be best to make a special flag
                    next if ( $old_breakpoint_to_go[$iend_1] );

                 # an isolated '},' may join with an identifier + ';'
                 # this is useful for the class of a 'bless' statement (bless.t)
                    if (   $types_to_go[$ibeg_1] eq '}'
                        && $types_to_go[$ibeg_2] eq 'i' )
                    {
                        next
                          unless ( ( $ibeg_1 == ( $iend_1 - 1 ) )
                            && ( $iend_2 == ( $ibeg_2 + 1 ) )
                            && $this_line_is_semicolon_terminated );

                        # override breakpoint
                        $forced_breakpoint_to_go[$iend_1] = 0;
                    }

                    # but otherwise ..
                    else {

                        # do not recombine after a comma unless this will leave
                        # just 1 more line
                        next unless ( $n + 1 >= $nmax );

                    # do not recombine if there is a change in indentation depth
                        next
                          if (
                            $levels_to_go[$iend_1] != $levels_to_go[$iend_2] );

                        # do not recombine a "complex expression" after a
                        # comma.  "complex" means no parens.
                        my $saw_paren;
                        foreach my $ii ( $ibeg_2 .. $iend_2 ) {
                            if ( $tokens_to_go[$ii] eq '(' ) {
                                $saw_paren = 1;
                                last;
                            }
                        }
                        next if $saw_paren;
                    }
                }

                # opening paren..
                elsif ( $types_to_go[$iend_1] eq '(' ) {

                    # No longer doing this
                }

                elsif ( $types_to_go[$iend_1] eq ')' ) {

                    # No longer doing this
                }

                # keep a terminal for-semicolon
                elsif ( $types_to_go[$iend_1] eq 'f' ) {
                    next;
                }

                # if '=' at end of line ...
                elsif ( $is_assignment{ $types_to_go[$iend_1] } ) {

                    my $is_short_quote =
                      (      $types_to_go[$ibeg_2] eq 'Q'
                          && $ibeg_2 == $iend_2
                          && length( $tokens_to_go[$ibeg_2] ) <
                          $rOpts_short_concatenation_item_length );
                    my $is_ternary =
                      ( $types_to_go[$ibeg_1] eq '?'
                          && ( $ibeg_3 >= 0 && $types_to_go[$ibeg_3] eq ':' ) );

                    # always join an isolated '=', a short quote, or if this
                    # will put ?/: at start of adjacent lines
                    if (   $ibeg_1 != $iend_1
                        && !$is_short_quote
                        && !$is_ternary )
                    {
                        next
                          unless (
                            (

                                # unless we can reduce this to two lines
                                $nmax < $n + 2

                             # or three lines, the last with a leading semicolon
                                || (   $nmax == $n + 2
                                    && $types_to_go[$ibeg_nmax] eq ';' )

                                # or the next line ends with a here doc
                                || $types_to_go[$iend_2] eq 'h'

                               # or the next line ends in an open paren or brace
                               # and the break hasn't been forced [dima.t]
                                || (  !$forced_breakpoint_to_go[$iend_1]
                                    && $types_to_go[$iend_2] eq '{' )
                            )

                            # do not recombine if the two lines might align well
                            # this is a very approximate test for this
                            && (   $ibeg_3 >= 0
                                && $types_to_go[$ibeg_2] ne
                                $types_to_go[$ibeg_3] )
                          );

                        # -lp users often prefer this:
                        #  my $title = function($env, $env, $sysarea,
                        #                       "bubba Borrower Entry");
                        #  so we will recombine if -lp is used we have ending
                        #  comma
                        if (  !$rOpts_line_up_parentheses
                            || $types_to_go[$iend_2] ne ',' )
                        {

                           # otherwise, scan the rhs line up to last token for
                           # complexity.  Note that we are not counting the last
                           # token in case it is an opening paren.
                            my $tv    = 0;
                            my $depth = $nesting_depth_to_go[$ibeg_2];
                            for ( my $i = $ibeg_2 + 1 ; $i < $iend_2 ; $i++ ) {
                                if ( $nesting_depth_to_go[$i] != $depth ) {
                                    $tv++;
                                    last if ( $tv > 1 );
                                }
                                $depth = $nesting_depth_to_go[$i];
                            }

                         # ok to recombine if no level changes before last token
                            if ( $tv > 0 ) {

                                # otherwise, do not recombine if more than two
                                # level changes.
                                next if ( $tv > 1 );

                              # check total complexity of the two adjacent lines
                              # that will occur if we do this join
                                my $istop =
                                  ( $n < $nmax ) ? $$ri_end[ $n + 1 ] : $iend_2;
                                for ( my $i = $iend_2 ; $i <= $istop ; $i++ ) {
                                    if ( $nesting_depth_to_go[$i] != $depth ) {
                                        $tv++;
                                        last if ( $tv > 2 );
                                    }
                                    $depth = $nesting_depth_to_go[$i];
                                }

                        # do not recombine if total is more than 2 level changes
                                next if ( $tv > 2 );
                            }
                        }
                    }

                    unless ( $tokens_to_go[$ibeg_2] =~ /^[\{\(\[]$/ ) {
                        $forced_breakpoint_to_go[$iend_1] = 0;
                    }
                }

                # for keywords..
                elsif ( $types_to_go[$iend_1] eq 'k' ) {

                    # make major control keywords stand out
                    # (recombine.t)
                    next
                      if (

                        #/^(last|next|redo|return)$/
                        $is_last_next_redo_return{ $tokens_to_go[$iend_1] }

                        # but only if followed by multiple lines
                        && $n < $nmax
                      );

                    if ( $is_and_or{ $tokens_to_go[$iend_1] } ) {
                        next
                          unless $want_break_before{ $tokens_to_go[$iend_1] };
                    }
                }

                # handle trailing + - * /
                elsif ( $is_math_op{ $types_to_go[$iend_1] } ) {

                    # combine lines if next line has single number
                    # or a short term followed by same operator
                    my $i_next_nonblank = $ibeg_2;
                    my $i_next_next     = $i_next_nonblank + 1;
                    $i_next_next++ if ( $types_to_go[$i_next_next] eq 'b' );
                    my $number_follows = $types_to_go[$i_next_nonblank] eq 'n'
                      && (
                        $i_next_nonblank == $iend_2
                        || (   $i_next_next == $iend_2
                            && $is_math_op{ $types_to_go[$i_next_next] } )
                        || $types_to_go[$i_next_next] eq ';'
                      );

                    # find token before last operator of previous line
                    my $iend_1_minus = $iend_1;
                    $iend_1_minus--
                      if ( $iend_1_minus > $ibeg_1 );
                    $iend_1_minus--
                      if ( $types_to_go[$iend_1_minus] eq 'b'
                        && $iend_1_minus > $ibeg_1 );

                    my $short_term_follows =
                      (      $types_to_go[$iend_2] eq $types_to_go[$iend_1]
                          && $types_to_go[$iend_1_minus] =~ /^[in]$/
                          && $iend_2 <= $ibeg_2 + 2
                          && length( $tokens_to_go[$ibeg_2] ) <
                          $rOpts_short_concatenation_item_length );

                    next
                      unless ( $number_follows || $short_term_follows );
                }

                #----------------------------------------------------------
                # Section 2: Now examine token at $ibeg_2 (left end of second
                # line of pair)
                #----------------------------------------------------------

                # join lines identified above as capable of
                # causing an outdented line with leading closing paren
                if ($previous_outdentable_closing_paren) {
                    $forced_breakpoint_to_go[$iend_1] = 0;
                }

                # do not recombine lines with leading :
                elsif ( $types_to_go[$ibeg_2] eq ':' ) {
                    $leading_amp_count++;
                    next if $want_break_before{ $types_to_go[$ibeg_2] };
                }

                # handle lines with leading &&, ||
                elsif ( $is_amp_amp{ $types_to_go[$ibeg_2] } ) {

                    $leading_amp_count++;

                    # ok to recombine if it follows a ? or :
                    # and is followed by an open paren..
                    my $ok =
                      (      $is_ternary{ $types_to_go[$ibeg_1] }
                          && $tokens_to_go[$iend_2] eq '(' )

                    # or is followed by a ? or : at same depth
                    #
                    # We are looking for something like this. We can
                    # recombine the && line with the line above to make the
                    # structure more clear:
                    #  return
                    #    exists $G->{Attr}->{V}
                    #    && exists $G->{Attr}->{V}->{$u}
                    #    ? %{ $G->{Attr}->{V}->{$u} }
                    #    : ();
                    #
                    # We should probably leave something like this alone:
                    #  return
                    #       exists $G->{Attr}->{E}
                    #    && exists $G->{Attr}->{E}->{$u}
                    #    && exists $G->{Attr}->{E}->{$u}->{$v}
                    #    ? %{ $G->{Attr}->{E}->{$u}->{$v} }
                    #    : ();
                    # so that we either have all of the &&'s (or ||'s)
                    # on one line, as in the first example, or break at
                    # each one as in the second example.  However, it
                    # sometimes makes things worse to check for this because
                    # it prevents multiple recombinations.  So this is not done.
                      || ( $ibeg_3 >= 0
                        && $is_ternary{ $types_to_go[$ibeg_3] }
                        && $nesting_depth_to_go[$ibeg_3] ==
                        $nesting_depth_to_go[$ibeg_2] );

                    next if !$ok && $want_break_before{ $types_to_go[$ibeg_2] };
                    $forced_breakpoint_to_go[$iend_1] = 0;

                    # tweak the bond strength to give this joint priority
                    # over ? and :
                    $bs_tweak = 0.25;
                }

                # Identify and recombine a broken ?/: chain
                elsif ( $types_to_go[$ibeg_2] eq '?' ) {

                    # Do not recombine different levels
                    my $lev = $levels_to_go[$ibeg_2];
                    next if ( $lev ne $levels_to_go[$ibeg_1] );

                    # Do not recombine a '?' if either next line or
                    # previous line does not start with a ':'.  The reasons
                    # are that (1) no alignment of the ? will be possible
                    # and (2) the expression is somewhat complex, so the
                    # '?' is harder to see in the interior of the line.
                    my $follows_colon =
                      $ibeg_1 >= 0 && $types_to_go[$ibeg_1] eq ':';
                    my $precedes_colon =
                      $ibeg_3 >= 0 && $types_to_go[$ibeg_3] eq ':';
                    next unless ( $follows_colon || $precedes_colon );

                    # we will always combining a ? line following a : line
                    if ( !$follows_colon ) {

                        # ...otherwise recombine only if it looks like a chain.
                        # we will just look at a few nearby lines to see if
                        # this looks like a chain.
                        my $local_count = 0;
                        foreach my $ii ( $ibeg_0, $ibeg_1, $ibeg_3, $ibeg_4 ) {
                            $local_count++
                              if $ii >= 0
                                  && $types_to_go[$ii] eq ':'
                                  && $levels_to_go[$ii] == $lev;
                        }
                        next unless ( $local_count > 1 );
                    }
                    $forced_breakpoint_to_go[$iend_1] = 0;
                }

                # do not recombine lines with leading '.'
                elsif ( $types_to_go[$ibeg_2] =~ /^(\.)$/ ) {
                    my $i_next_nonblank = $ibeg_2 + 1;
                    if ( $types_to_go[$i_next_nonblank] eq 'b' ) {
                        $i_next_nonblank++;
                    }

                    next
                      unless (

                   # ... unless there is just one and we can reduce
                   # this to two lines if we do.  For example, this
                   #
                   #
                   #  $bodyA .=
                   #    '($dummy, $pat) = &get_next_tex_cmd;' . '$args .= $pat;'
                   #
                   #  looks better than this:
                   #  $bodyA .= '($dummy, $pat) = &get_next_tex_cmd;'
                   #    . '$args .= $pat;'

                        (
                               $n == 2
                            && $n == $nmax
                            && $types_to_go[$ibeg_1] ne $types_to_go[$ibeg_2]
                        )

                        #  ... or this would strand a short quote , like this
                        #                . "some long qoute"
                        #                . "\n";
                        || (   $types_to_go[$i_next_nonblank] eq 'Q'
                            && $i_next_nonblank >= $iend_2 - 1
                            && length( $tokens_to_go[$i_next_nonblank] ) <
                            $rOpts_short_concatenation_item_length )
                      );
                }

                # handle leading keyword..
                elsif ( $types_to_go[$ibeg_2] eq 'k' ) {

                    # handle leading "or"
                    if ( $tokens_to_go[$ibeg_2] eq 'or' ) {
                        next
                          unless (
                            $this_line_is_semicolon_terminated
                            && (

                                # following 'if' or 'unless' or 'or'
                                $types_to_go[$ibeg_1] eq 'k'
                                && $is_if_unless{ $tokens_to_go[$ibeg_1] }

                                # important: only combine a very simple or
                                # statement because the step below may have
                                # combined a trailing 'and' with this or,
                                # and we do not want to then combine
                                # everything together
                                && ( $iend_2 - $ibeg_2 <= 7 )
                            )
                          );
                    }

                    # handle leading 'and'
                    elsif ( $tokens_to_go[$ibeg_2] eq 'and' ) {

                        # Decide if we will combine a single terminal 'and'
                        # after an 'if' or 'unless'.

                        #     This looks best with the 'and' on the same
                        #     line as the 'if':
                        #
                        #         $a = 1
                        #           if $seconds and $nu < 2;
                        #
                        #     But this looks better as shown:
                        #
                        #         $a = 1
                        #           if !$this->{Parents}{$_}
                        #           or $this->{Parents}{$_} eq $_;
                        #
                        next
                          unless (
                            $this_line_is_semicolon_terminated
                            && (

                                # following 'if' or 'unless' or 'or'
                                $types_to_go[$ibeg_1] eq 'k'
                                && (   $is_if_unless{ $tokens_to_go[$ibeg_1] }
                                    || $tokens_to_go[$ibeg_1] eq 'or' )
                            )
                          );
                    }

                    # handle leading "if" and "unless"
                    elsif ( $is_if_unless{ $tokens_to_go[$ibeg_2] } ) {

                      # FIXME: This is still experimental..may not be too useful
                        next
                          unless (
                            $this_line_is_semicolon_terminated

                            #  previous line begins with 'and' or 'or'
                            && $types_to_go[$ibeg_1] eq 'k'
                            && $is_and_or{ $tokens_to_go[$ibeg_1] }

                          );
                    }

                    # handle all other leading keywords
                    else {

                        # keywords look best at start of lines,
                        # but combine things like "1 while"
                        unless ( $is_assignment{ $types_to_go[$iend_1] } ) {
                            next
                              if ( ( $types_to_go[$iend_1] ne 'k' )
                                && ( $tokens_to_go[$ibeg_2] ne 'while' ) );
                        }
                    }
                }

                # similar treatment of && and || as above for 'and' and 'or':
                # NOTE: This block of code is currently bypassed because
                # of a previous block but is retained for possible future use.
                elsif ( $is_amp_amp{ $types_to_go[$ibeg_2] } ) {

                    # maybe looking at something like:
                    # unless $TEXTONLY || $item =~ m%</?(hr>|p>|a|img)%i;

                    next
                      unless (
                        $this_line_is_semicolon_terminated

                        # previous line begins with an 'if' or 'unless' keyword
                        && $types_to_go[$ibeg_1] eq 'k'
                        && $is_if_unless{ $tokens_to_go[$ibeg_1] }

                      );
                }

                # handle leading + - * /
                elsif ( $is_math_op{ $types_to_go[$ibeg_2] } ) {
                    my $i_next_nonblank = $ibeg_2 + 1;
                    if ( $types_to_go[$i_next_nonblank] eq 'b' ) {
                        $i_next_nonblank++;
                    }

                    my $i_next_next = $i_next_nonblank + 1;
                    $i_next_next++ if ( $types_to_go[$i_next_next] eq 'b' );

                    my $is_number = (
                        $types_to_go[$i_next_nonblank] eq 'n'
                          && ( $i_next_nonblank >= $iend_2 - 1
                            || $types_to_go[$i_next_next] eq ';' )
                    );

                    my $iend_1_nonblank =
                      $types_to_go[$iend_1] eq 'b' ? $iend_1 - 1 : $iend_1;
                    my $iend_2_nonblank =
                      $types_to_go[$iend_2] eq 'b' ? $iend_2 - 1 : $iend_2;

                    my $is_short_term =
                      (      $types_to_go[$ibeg_2] eq $types_to_go[$ibeg_1]
                          && $types_to_go[$iend_2_nonblank] =~ /^[in]$/
                          && $types_to_go[$iend_1_nonblank] =~ /^[in]$/
                          && $iend_2_nonblank <= $ibeg_2 + 2
                          && length( $tokens_to_go[$iend_2_nonblank] ) <
                          $rOpts_short_concatenation_item_length );

                    # Combine these lines if this line is a single
                    # number, or if it is a short term with same
                    # operator as the previous line.  For example, in
                    # the following code we will combine all of the
                    # short terms $A, $B, $C, $D, $E, $F, together
                    # instead of leaving them one per line:
                    #  my $time =
                    #    $A * $B * $C * $D * $E * $F *
                    #    ( 2. * $eps * $sigma * $area ) *
                    #    ( 1. / $tcold**3 - 1. / $thot**3 );
                    # This can be important in math-intensive code.
                    next
                      unless (
                           $is_number
                        || $is_short_term

                        # or if we can reduce this to two lines if we do.
                        || (   $n == 2
                            && $n == $nmax
                            && $types_to_go[$ibeg_1] ne $types_to_go[$ibeg_2] )
                      );
                }

                # handle line with leading = or similar
                elsif ( $is_assignment{ $types_to_go[$ibeg_2] } ) {
                    next unless $n == 1;
                    next
                      unless (

                        # unless we can reduce this to two lines
                        $nmax == 2

                        # or three lines, the last with a leading semicolon
                        || ( $nmax == 3 && $types_to_go[$ibeg_nmax] eq ';' )

                        # or the next line ends with a here doc
                        || $types_to_go[$iend_2] eq 'h'
                      );
                }

                #----------------------------------------------------------
                # Section 3:
                # Combine the lines if we arrive here and it is possible
                #----------------------------------------------------------

                # honor hard breakpoints
                next if ( $forced_breakpoint_to_go[$iend_1] > 0 );

                my $bs = $bond_strength_to_go[$iend_1] + $bs_tweak;

                # combined line cannot be too long
                next
                  if excess_line_length( $ibeg_1, $iend_2 ) > 0;

                # do not recombine if we would skip in indentation levels
                if ( $n < $nmax ) {
                    my $if_next = $$ri_beg[ $n + 1 ];
                    next
                      if (
                           $levels_to_go[$ibeg_1] < $levels_to_go[$ibeg_2]
                        && $levels_to_go[$ibeg_2] < $levels_to_go[$if_next]

                        # but an isolated 'if (' is undesirable
                        && !(
                               $n == 1
                            && $iend_1 - $ibeg_1 <= 2
                            && $types_to_go[$ibeg_1]  eq 'k'
                            && $tokens_to_go[$ibeg_1] eq 'if'
                            && $tokens_to_go[$iend_1] ne '('
                        )
                      );
                }

                # honor no-break's
                next if ( $bs == NO_BREAK );

                # remember the pair with the greatest bond strength
                if ( !$n_best ) {
                    $n_best  = $n;
                    $bs_best = $bs;
                }
                else {

                    if ( $bs > $bs_best ) {
                        $n_best  = $n;
                        $bs_best = $bs;
                    }
                }
            }

            # recombine the pair with the greatest bond strength
            if ($n_best) {
                splice @$ri_beg, $n_best, 1;
                splice @$ri_end, $n_best - 1, 1;

                # keep going if we are still making progress
                $more_to_do++;
            }
        }
        return ( $ri_beg, $ri_end );
    }
}    # end recombine_breakpoints

sub break_all_chain_tokens {

    # scan the current breakpoints looking for breaks at certain "chain
    # operators" (. : && || + etc) which often occur repeatedly in a long
    # statement.  If we see a break at any one, break at all similar tokens
    # within the same container.
    #
    my ( $ri_left, $ri_right ) = @_;

    my %saw_chain_type;
    my %left_chain_type;
    my %right_chain_type;
    my %interior_chain_type;
    my $nmax = @$ri_right - 1;

    # scan the left and right end tokens of all lines
    my $count = 0;
    for my $n ( 0 .. $nmax ) {
        my $il    = $$ri_left[$n];
        my $ir    = $$ri_right[$n];
        my $typel = $types_to_go[$il];
        my $typer = $types_to_go[$ir];
        $typel = '+' if ( $typel eq '-' );    # treat + and - the same
        $typer = '+' if ( $typer eq '-' );
        $typel = '*' if ( $typel eq '/' );    # treat * and / the same
        $typer = '*' if ( $typer eq '/' );
        my $tokenl = $tokens_to_go[$il];
        my $tokenr = $tokens_to_go[$ir];

        if ( $is_chain_operator{$tokenl} && $want_break_before{$typel} ) {
            next if ( $typel eq '?' );
            push @{ $left_chain_type{$typel} }, $il;
            $saw_chain_type{$typel} = 1;
            $count++;
        }
        if ( $is_chain_operator{$tokenr} && !$want_break_before{$typer} ) {
            next if ( $typer eq '?' );
            push @{ $right_chain_type{$typer} }, $ir;
            $saw_chain_type{$typer} = 1;
            $count++;
        }
    }
    return unless $count;

    # now look for any interior tokens of the same types
    $count = 0;
    for my $n ( 0 .. $nmax ) {
        my $il = $$ri_left[$n];
        my $ir = $$ri_right[$n];
        for ( my $i = $il + 1 ; $i < $ir ; $i++ ) {
            my $type = $types_to_go[$i];
            $type = '+' if ( $type eq '-' );
            $type = '*' if ( $type eq '/' );
            if ( $saw_chain_type{$type} ) {
                push @{ $interior_chain_type{$type} }, $i;
                $count++;
            }
        }
    }
    return unless $count;

    # now make a list of all new break points
    my @insert_list;

    # loop over all chain types
    foreach my $type ( keys %saw_chain_type ) {

        # quit if just ONE continuation line with leading .  For example--
        # print LATEXFILE '\framebox{\parbox[c][' . $h . '][t]{' . $w . '}{'
        #  . $contents;
        last if ( $nmax == 1 && $type =~ /^[\.\+]$/ );

        # loop over all interior chain tokens
        foreach my $itest ( @{ $interior_chain_type{$type} } ) {

            # loop over all left end tokens of same type
            if ( $left_chain_type{$type} ) {
                next if $nobreak_to_go[ $itest - 1 ];
                foreach my $i ( @{ $left_chain_type{$type} } ) {
                    next unless in_same_container( $i, $itest );
                    push @insert_list, $itest - 1;

                    # Break at matching ? if this : is at a different level.
                    # For example, the ? before $THRf_DEAD in the following
                    # should get a break if its : gets a break.
                    #
                    # my $flags =
                    #     ( $_ & 1 ) ? ( $_ & 4 ) ? $THRf_DEAD : $THRf_ZOMBIE
                    #   : ( $_ & 4 ) ? $THRf_R_DETACHED
                    #   :              $THRf_R_JOINABLE;
                    if (   $type eq ':'
                        && $levels_to_go[$i] != $levels_to_go[$itest] )
                    {
                        my $i_question = $mate_index_to_go[$itest];
                        if ( $i_question > 0 ) {
                            push @insert_list, $i_question - 1;
                        }
                    }
                    last;
                }
            }

            # loop over all right end tokens of same type
            if ( $right_chain_type{$type} ) {
                next if $nobreak_to_go[$itest];
                foreach my $i ( @{ $right_chain_type{$type} } ) {
                    next unless in_same_container( $i, $itest );
                    push @insert_list, $itest;

                    # break at matching ? if this : is at a different level
                    if (   $type eq ':'
                        && $levels_to_go[$i] != $levels_to_go[$itest] )
                    {
                        my $i_question = $mate_index_to_go[$itest];
                        if ( $i_question >= 0 ) {
                            push @insert_list, $i_question;
                        }
                    }
                    last;
                }
            }
        }
    }

    # insert any new break points
    if (@insert_list) {
        insert_additional_breaks( \@insert_list, $ri_left, $ri_right );
    }
}

sub break_equals {

    # Look for assignment operators that could use a breakpoint.
    # For example, in the following snippet
    #
    #    $HOME = $ENV{HOME}
    #      || $ENV{LOGDIR}
    #      || $pw[7]
    #      || die "no home directory for user $<";
    #
    # we could break at the = to get this, which is a little nicer:
    #    $HOME =
    #         $ENV{HOME}
    #      || $ENV{LOGDIR}
    #      || $pw[7]
    #      || die "no home directory for user $<";
    #
    # The logic here follows the logic in set_logical_padding, which
    # will add the padding in the second line to improve alignment.
    #
    my ( $ri_left, $ri_right ) = @_;
    my $nmax = @$ri_right - 1;
    return unless ( $nmax >= 2 );

    # scan the left ends of first two lines
    my $tokbeg = "";
    my $depth_beg;
    for my $n ( 1 .. 2 ) {
        my $il     = $$ri_left[$n];
        my $typel  = $types_to_go[$il];
        my $tokenl = $tokens_to_go[$il];

        my $has_leading_op = ( $tokenl =~ /^\w/ )
          ? $is_chain_operator{$tokenl}    # + - * / : ? && ||
          : $is_chain_operator{$typel};    # and, or
        return unless ($has_leading_op);
        if ( $n > 1 ) {
            return
              unless ( $tokenl eq $tokbeg
                && $nesting_depth_to_go[$il] eq $depth_beg );
        }
        $tokbeg    = $tokenl;
        $depth_beg = $nesting_depth_to_go[$il];
    }

    # now look for any interior tokens of the same types
    my $il = $$ri_left[0];
    my $ir = $$ri_right[0];

    # now make a list of all new break points
    my @insert_list;
    for ( my $i = $ir - 1 ; $i > $il ; $i-- ) {
        my $type = $types_to_go[$i];
        if (   $is_assignment{$type}
            && $nesting_depth_to_go[$i] eq $depth_beg )
        {
            if ( $want_break_before{$type} ) {
                push @insert_list, $i - 1;
            }
            else {
                push @insert_list, $i;
            }
        }
    }

    # Break after a 'return' followed by a chain of operators
    #  return ( $^O !~ /win32|dos/i )
    #    && ( $^O ne 'VMS' )
    #    && ( $^O ne 'OS2' )
    #    && ( $^O ne 'MacOS' );
    # To give:
    #  return
    #       ( $^O !~ /win32|dos/i )
    #    && ( $^O ne 'VMS' )
    #    && ( $^O ne 'OS2' )
    #    && ( $^O ne 'MacOS' );
    my $i = 0;
    if (   $types_to_go[$i] eq 'k'
        && $tokens_to_go[$i] eq 'return'
        && $ir > $il
        && $nesting_depth_to_go[$i] eq $depth_beg )
    {
        push @insert_list, $i;
    }

    return unless (@insert_list);

    # One final check...
    # scan second and thrid lines and be sure there are no assignments
    # we want to avoid breaking at an = to make something like this:
    #    unless ( $icon =
    #           $html_icons{"$type-$state"}
    #        or $icon = $html_icons{$type}
    #        or $icon = $html_icons{$state} )
    for my $n ( 1 .. 2 ) {
        my $il = $$ri_left[$n];
        my $ir = $$ri_right[$n];
        for ( my $i = $il + 1 ; $i <= $ir ; $i++ ) {
            my $type = $types_to_go[$i];
            return
              if ( $is_assignment{$type}
                && $nesting_depth_to_go[$i] eq $depth_beg );
        }
    }

    # ok, insert any new break point
    if (@insert_list) {
        insert_additional_breaks( \@insert_list, $ri_left, $ri_right );
    }
}

sub insert_final_breaks {

    my ( $ri_left, $ri_right ) = @_;

    my $nmax = @$ri_right - 1;

    # scan the left and right end tokens of all lines
    my $count         = 0;
    my $i_first_colon = -1;
    for my $n ( 0 .. $nmax ) {
        my $il    = $$ri_left[$n];
        my $ir    = $$ri_right[$n];
        my $typel = $types_to_go[$il];
        my $typer = $types_to_go[$ir];
        return if ( $typel eq '?' );
        return if ( $typer eq '?' );
        if    ( $typel eq ':' ) { $i_first_colon = $il; last; }
        elsif ( $typer eq ':' ) { $i_first_colon = $ir; last; }
    }

    # For long ternary chains,
    # if the first : we see has its # ? is in the interior
    # of a preceding line, then see if there are any good
    # breakpoints before the ?.
    if ( $i_first_colon > 0 ) {
        my $i_question = $mate_index_to_go[$i_first_colon];
        if ( $i_question > 0 ) {
            my @insert_list;
            for ( my $ii = $i_question - 1 ; $ii >= 0 ; $ii -= 1 ) {
                my $token = $tokens_to_go[$ii];
                my $type  = $types_to_go[$ii];

                # For now, a good break is either a comma or a 'return'.
                if ( ( $type eq ',' || $type eq 'k' && $token eq 'return' )
                    && in_same_container( $ii, $i_question ) )
                {
                    push @insert_list, $ii;
                    last;
                }
            }

            # insert any new break points
            if (@insert_list) {
                insert_additional_breaks( \@insert_list, $ri_left, $ri_right );
            }
        }
    }
}

sub in_same_container {

    # check to see if tokens at i1 and i2 are in the
    # same container, and not separated by a comma, ? or :
    my ( $i1, $i2 ) = @_;
    my $type  = $types_to_go[$i1];
    my $depth = $nesting_depth_to_go[$i1];
    return unless ( $nesting_depth_to_go[$i2] == $depth );
    if ( $i2 < $i1 ) { ( $i1, $i2 ) = ( $i2, $i1 ) }
    for ( my $i = $i1 + 1 ; $i < $i2 ; $i++ ) {
        next   if ( $nesting_depth_to_go[$i] > $depth );
        return if ( $nesting_depth_to_go[$i] < $depth );

        my $tok = $tokens_to_go[$i];
        $tok = ',' if $tok eq '=>';    # treat => same as ,

        # Example: we would not want to break at any of these .'s
        #  : "<A HREF=\"#item_" . htmlify( 0, $s2 ) . "\">$str</A>"
        if ( $type ne ':' ) {
            return if ( $tok =~ /^[\,\:\?]$/ ) || $tok eq '||' || $tok eq 'or';
        }
        else {
            return if ( $tok =~ /^[\,]$/ );
        }
    }
    return 1;
}

sub set_continuation_breaks {

    # Define an array of indexes for inserting newline characters to
    # keep the line lengths below the maximum desired length.  There is
    # an implied break after the last token, so it need not be included.

    # Method:
    # This routine is part of series of routines which adjust line
    # lengths.  It is only called if a statement is longer than the
    # maximum line length, or if a preliminary scanning located
    # desirable break points.   Sub scan_list has already looked at
    # these tokens and set breakpoints (in array
    # $forced_breakpoint_to_go[$i]) where it wants breaks (for example
    # after commas, after opening parens, and before closing parens).
    # This routine will honor these breakpoints and also add additional
    # breakpoints as necessary to keep the line length below the maximum
    # requested.  It bases its decision on where the 'bond strength' is
    # lowest.

    # Output: returns references to the arrays:
    #  @i_first
    #  @i_last
    # which contain the indexes $i of the first and last tokens on each
    # line.

    # In addition, the array:
    #   $forced_breakpoint_to_go[$i]
    # may be updated to be =1 for any index $i after which there must be
    # a break.  This signals later routines not to undo the breakpoint.

    my $saw_good_break = shift;
    my @i_first        = ();      # the first index to output
    my @i_last         = ();      # the last index to output
    my @i_colon_breaks = ();      # needed to decide if we have to break at ?'s
    if ( $types_to_go[0] eq ':' ) { push @i_colon_breaks, 0 }

    set_bond_strengths();

    my $imin = 0;
    my $imax = $max_index_to_go;
    if ( $types_to_go[$imin] eq 'b' ) { $imin++ }
    if ( $types_to_go[$imax] eq 'b' ) { $imax-- }
    my $i_begin = $imin;          # index for starting next iteration

    my $leading_spaces          = leading_spaces_to_go($imin);
    my $line_count              = 0;
    my $last_break_strength     = NO_BREAK;
    my $i_last_break            = -1;
    my $max_bias                = 0.001;
    my $tiny_bias               = 0.0001;
    my $leading_alignment_token = "";
    my $leading_alignment_type  = "";

    # see if any ?/:'s are in order
    my $colons_in_order = 1;
    my $last_tok        = "";
    my @colon_list  = grep /^[\?\:]$/, @tokens_to_go[ 0 .. $max_index_to_go ];
    my $colon_count = @colon_list;
    foreach (@colon_list) {
        if ( $_ eq $last_tok ) { $colons_in_order = 0; last }
        $last_tok = $_;
    }

    # This is a sufficient but not necessary condition for colon chain
    my $is_colon_chain = ( $colons_in_order && @colon_list > 2 );

    #-------------------------------------------------------
    # BEGINNING of main loop to set continuation breakpoints
    # Keep iterating until we reach the end
    #-------------------------------------------------------
    while ( $i_begin <= $imax ) {
        my $lowest_strength        = NO_BREAK;
        my $starting_sum           = $lengths_to_go[$i_begin];
        my $i_lowest               = -1;
        my $i_test                 = -1;
        my $lowest_next_token      = '';
        my $lowest_next_type       = 'b';
        my $i_lowest_next_nonblank = -1;

        #-------------------------------------------------------
        # BEGINNING of inner loop to find the best next breakpoint
        #-------------------------------------------------------
        for ( $i_test = $i_begin ; $i_test <= $imax ; $i_test++ ) {
            my $type       = $types_to_go[$i_test];
            my $token      = $tokens_to_go[$i_test];
            my $next_type  = $types_to_go[ $i_test + 1 ];
            my $next_token = $tokens_to_go[ $i_test + 1 ];
            my $i_next_nonblank =
              ( ( $next_type eq 'b' ) ? $i_test + 2 : $i_test + 1 );
            my $next_nonblank_type       = $types_to_go[$i_next_nonblank];
            my $next_nonblank_token      = $tokens_to_go[$i_next_nonblank];
            my $next_nonblank_block_type = $block_type_to_go[$i_next_nonblank];
            my $strength                 = $bond_strength_to_go[$i_test];
            my $must_break               = 0;

            # FIXME: TESTING: Might want to be able to break after these
            # force an immediate break at certain operators
            # with lower level than the start of the line
            if (
                (
                    $next_nonblank_type =~ /^(\.|\&\&|\|\|)$/
                    || (   $next_nonblank_type eq 'k'
                        && $next_nonblank_token =~ /^(and|or)$/ )
                )
                && ( $nesting_depth_to_go[$i_begin] >
                    $nesting_depth_to_go[$i_next_nonblank] )
              )
            {
                set_forced_breakpoint($i_next_nonblank);
            }

            if (

                # Try to put a break where requested by scan_list
                $forced_breakpoint_to_go[$i_test]

                # break between ) { in a continued line so that the '{' can
                # be outdented
                # See similar logic in scan_list which catches instances
                # where a line is just something like ') {'
                || (   $line_count
                    && ( $token              eq ')' )
                    && ( $next_nonblank_type eq '{' )
                    && ($next_nonblank_block_type)
                    && !$rOpts->{'opening-brace-always-on-right'} )

                # There is an implied forced break at a terminal opening brace
                || ( ( $type eq '{' ) && ( $i_test == $imax ) )
              )
            {

                # Forced breakpoints must sometimes be overridden, for example
                # because of a side comment causing a NO_BREAK.  It is easier
                # to catch this here than when they are set.
                if ( $strength < NO_BREAK ) {
                    $strength   = $lowest_strength - $tiny_bias;
                    $must_break = 1;
                }
            }

            # quit if a break here would put a good terminal token on
            # the next line and we already have a possible break
            if (
                   !$must_break
                && ( $next_nonblank_type =~ /^[\;\,]$/ )
                && (
                    (
                        $leading_spaces +
                        $lengths_to_go[ $i_next_nonblank + 1 ] -
                        $starting_sum
                    ) > $rOpts_maximum_line_length
                )
              )
            {
                last if ( $i_lowest >= 0 );
            }

            # Avoid a break which would strand a single punctuation
            # token.  For example, we do not want to strand a leading
            # '.' which is followed by a long quoted string.
            if (
                   !$must_break
                && ( $i_test == $i_begin )
                && ( $i_test < $imax )
                && ( $token eq $type )
                && (
                    (
                        $leading_spaces +
                        $lengths_to_go[ $i_test + 1 ] -
                        $starting_sum
                    ) <= $rOpts_maximum_line_length
                )
              )
            {
                $i_test++;

                if ( ( $i_test < $imax ) && ( $next_type eq 'b' ) ) {
                    $i_test++;
                }
                redo;
            }

            if ( ( $strength <= $lowest_strength ) && ( $strength < NO_BREAK ) )
            {

                # break at previous best break if it would have produced
                # a leading alignment of certain common tokens, and it
                # is different from the latest candidate break
                last
                  if ($leading_alignment_type);

                # Force at least one breakpoint if old code had good
                # break It is only called if a breakpoint is required or
                # desired.  This will probably need some adjustments
                # over time.  A goal is to try to be sure that, if a new
                # side comment is introduced into formated text, then
                # the same breakpoints will occur.  scbreak.t
                last
                  if (
                    $i_test == $imax                # we are at the end
                    && !$forced_breakpoint_count    #
                    && $saw_good_break              # old line had good break
                    && $type =~ /^[#;\{]$/          # and this line ends in
                                                    # ';' or side comment
                    && $i_last_break < 0        # and we haven't made a break
                    && $i_lowest > 0            # and we saw a possible break
                    && $i_lowest < $imax - 1    # (but not just before this ;)
                    && $strength - $lowest_strength < 0.5 * WEAK # and it's good
                  );

                $lowest_strength        = $strength;
                $i_lowest               = $i_test;
                $lowest_next_token      = $next_nonblank_token;
                $lowest_next_type       = $next_nonblank_type;
                $i_lowest_next_nonblank = $i_next_nonblank;
                last if $must_break;

                # set flags to remember if a break here will produce a
                # leading alignment of certain common tokens
                if (   $line_count > 0
                    && $i_test < $imax
                    && ( $lowest_strength - $last_break_strength <= $max_bias )
                  )
                {
                    my $i_last_end = $i_begin - 1;
                    if ( $types_to_go[$i_last_end] eq 'b' ) { $i_last_end -= 1 }
                    my $tok_beg  = $tokens_to_go[$i_begin];
                    my $type_beg = $types_to_go[$i_begin];
                    if (

                        # check for leading alignment of certain tokens
                        (
                               $tok_beg eq $next_nonblank_token
                            && $is_chain_operator{$tok_beg}
                            && (   $type_beg eq 'k'
                                || $type_beg eq $tok_beg )
                            && $nesting_depth_to_go[$i_begin] >=
                            $nesting_depth_to_go[$i_next_nonblank]
                        )

                        || (   $tokens_to_go[$i_last_end] eq $token
                            && $is_chain_operator{$token}
                            && ( $type eq 'k' || $type eq $token )
                            && $nesting_depth_to_go[$i_last_end] >=
                            $nesting_depth_to_go[$i_test] )
                      )
                    {
                        $leading_alignment_token = $next_nonblank_token;
                        $leading_alignment_type  = $next_nonblank_type;
                    }
                }
            }

            my $too_long =
              ( $i_test >= $imax )
              ? 1
              : (
                (
                    $leading_spaces +
                      $lengths_to_go[ $i_test + 2 ] -
                      $starting_sum
                ) > $rOpts_maximum_line_length
              );

            FORMATTER_DEBUG_FLAG_BREAK
              && print
"BREAK: testing i = $i_test imax=$imax $types_to_go[$i_test] $next_nonblank_type leading sp=($leading_spaces) next length = $lengths_to_go[$i_test+2] too_long=$too_long str=$strength\n";

            # allow one extra terminal token after exceeding line length
            # if it would strand this token.
            if (   $rOpts_fuzzy_line_length
                && $too_long
                && ( $i_lowest == $i_test )
                && ( length($token) > 1 )
                && ( $next_nonblank_type =~ /^[\;\,]$/ ) )
            {
                $too_long = 0;
            }

            last
              if (
                ( $i_test == $imax )    # we're done if no more tokens,
                || (
                    ( $i_lowest >= 0 )    # or no more space and we have a break
                    && $too_long
                )
              );
        }

        #-------------------------------------------------------
        # END of inner loop to find the best next breakpoint
        # Now decide exactly where to put the breakpoint
        #-------------------------------------------------------

        # it's always ok to break at imax if no other break was found
        if ( $i_lowest < 0 ) { $i_lowest = $imax }

        # semi-final index calculation
        my $i_next_nonblank = (
            ( $types_to_go[ $i_lowest + 1 ] eq 'b' )
            ? $i_lowest + 2
            : $i_lowest + 1
        );
        my $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        my $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        #-------------------------------------------------------
        # ?/: rule 1 : if a break here will separate a '?' on this
        # line from its closing ':', then break at the '?' instead.
        #-------------------------------------------------------
        my $i;
        foreach $i ( $i_begin + 1 .. $i_lowest - 1 ) {
            next unless ( $tokens_to_go[$i] eq '?' );

            # do not break if probable sequence of ?/: statements
            next if ($is_colon_chain);

            # do not break if statement is broken by side comment
            next
              if (
                $tokens_to_go[$max_index_to_go] eq '#'
                && terminal_type( \@types_to_go, \@block_type_to_go, 0,
                    $max_index_to_go ) !~ /^[\;\}]$/
              );

            # no break needed if matching : is also on the line
            next
              if ( $mate_index_to_go[$i] >= 0
                && $mate_index_to_go[$i] <= $i_next_nonblank );

            $i_lowest = $i;
            if ( $want_break_before{'?'} ) { $i_lowest-- }
            last;
        }

        #-------------------------------------------------------
        # END of inner loop to find the best next breakpoint:
        # Break the line after the token with index i=$i_lowest
        #-------------------------------------------------------

        # final index calculation
        $i_next_nonblank = (
            ( $types_to_go[ $i_lowest + 1 ] eq 'b' )
            ? $i_lowest + 2
            : $i_lowest + 1
        );
        $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        FORMATTER_DEBUG_FLAG_BREAK
          && print "BREAK: best is i = $i_lowest strength = $lowest_strength\n";

        #-------------------------------------------------------
        # ?/: rule 2 : if we break at a '?', then break at its ':'
        #
        # Note: this rule is also in sub scan_list to handle a break
        # at the start and end of a line (in case breaks are dictated
        # by side comments).
        #-------------------------------------------------------
        if ( $next_nonblank_type eq '?' ) {
            set_closing_breakpoint($i_next_nonblank);
        }
        elsif ( $types_to_go[$i_lowest] eq '?' ) {
            set_closing_breakpoint($i_lowest);
        }

        #-------------------------------------------------------
        # ?/: rule 3 : if we break at a ':' then we save
        # its location for further work below.  We may need to go
        # back and break at its '?'.
        #-------------------------------------------------------
        if ( $next_nonblank_type eq ':' ) {
            push @i_colon_breaks, $i_next_nonblank;
        }
        elsif ( $types_to_go[$i_lowest] eq ':' ) {
            push @i_colon_breaks, $i_lowest;
        }

        # here we should set breaks for all '?'/':' pairs which are
        # separated by this line

        $line_count++;

        # save this line segment, after trimming blanks at the ends
        push( @i_first,
            ( $types_to_go[$i_begin] eq 'b' ) ? $i_begin + 1 : $i_begin );
        push( @i_last,
            ( $types_to_go[$i_lowest] eq 'b' ) ? $i_lowest - 1 : $i_lowest );

        # set a forced breakpoint at a container opening, if necessary, to
        # signal a break at a closing container.  Excepting '(' for now.
        if ( $tokens_to_go[$i_lowest] =~ /^[\{\[]$/
            && !$forced_breakpoint_to_go[$i_lowest] )
        {
            set_closing_breakpoint($i_lowest);
        }

        # get ready to go again
        $i_begin                 = $i_lowest + 1;
        $last_break_strength     = $lowest_strength;
        $i_last_break            = $i_lowest;
        $leading_alignment_token = "";
        $leading_alignment_type  = "";
        $lowest_next_token       = '';
        $lowest_next_type        = 'b';

        if ( ( $i_begin <= $imax ) && ( $types_to_go[$i_begin] eq 'b' ) ) {
            $i_begin++;
        }

        # update indentation size
        if ( $i_begin <= $imax ) {
            $leading_spaces = leading_spaces_to_go($i_begin);
        }
    }

    #-------------------------------------------------------
    # END of main loop to set continuation breakpoints
    # Now go back and make any necessary corrections
    #-------------------------------------------------------

    #-------------------------------------------------------
    # ?/: rule 4 -- if we broke at a ':', then break at
    # corresponding '?' unless this is a chain of ?: expressions
    #-------------------------------------------------------
    if (@i_colon_breaks) {

        # using a simple method for deciding if we are in a ?/: chain --
        # this is a chain if it has multiple ?/: pairs all in order;
        # otherwise not.
        # Note that if line starts in a ':' we count that above as a break
        my $is_chain = ( $colons_in_order && @i_colon_breaks > 1 );

        unless ($is_chain) {
            my @insert_list = ();
            foreach (@i_colon_breaks) {
                my $i_question = $mate_index_to_go[$_];
                if ( $i_question >= 0 ) {
                    if ( $want_break_before{'?'} ) {
                        $i_question--;
                        if (   $i_question > 0
                            && $types_to_go[$i_question] eq 'b' )
                        {
                            $i_question--;
                        }
                    }

                    if ( $i_question >= 0 ) {
                        push @insert_list, $i_question;
                    }
                }
                insert_additional_breaks( \@insert_list, \@i_first, \@i_last );
            }
        }
    }
    return ( \@i_first, \@i_last, $colon_count );
}

sub insert_additional_breaks {

    # this routine will add line breaks at requested locations after
    # sub set_continuation_breaks has made preliminary breaks.

    my ( $ri_break_list, $ri_first, $ri_last ) = @_;
    my $i_f;
    my $i_l;
    my $line_number = 0;
    my $i_break_left;
    foreach $i_break_left ( sort { $a <=> $b } @$ri_break_list ) {

        $i_f = $$ri_first[$line_number];
        $i_l = $$ri_last[$line_number];
        while ( $i_break_left >= $i_l ) {
            $line_number++;

            # shouldn't happen unless caller passes bad indexes
            if ( $line_number >= @$ri_last ) {
                warning(
"Non-fatal program bug: couldn't set break at $i_break_left\n"
                );
                report_definite_bug();
                return;
            }
            $i_f = $$ri_first[$line_number];
            $i_l = $$ri_last[$line_number];
        }

        my $i_break_right = $i_break_left + 1;
        if ( $types_to_go[$i_break_right] eq 'b' ) { $i_break_right++ }

        if (   $i_break_left >= $i_f
            && $i_break_left < $i_l
            && $i_break_right > $i_f
            && $i_break_right <= $i_l )
        {
            splice( @$ri_first, $line_number, 1, ( $i_f, $i_break_right ) );
            splice( @$ri_last, $line_number, 1, ( $i_break_left, $i_l ) );
        }
    }
}

sub set_closing_breakpoint {

    # set a breakpoint at a matching closing token
    # at present, this is only used to break at a ':' which matches a '?'
    my $i_break = shift;

    if ( $mate_index_to_go[$i_break] >= 0 ) {

        # CAUTION: infinite recursion possible here:
        #   set_closing_breakpoint calls set_forced_breakpoint, and
        #   set_forced_breakpoint call set_closing_breakpoint
        #   ( test files attrib.t, BasicLyx.pm.html).
        # Don't reduce the '2' in the statement below
        if ( $mate_index_to_go[$i_break] > $i_break + 2 ) {

            # break before } ] and ), but sub set_forced_breakpoint will decide
            # to break before or after a ? and :
            my $inc = ( $tokens_to_go[$i_break] eq '?' ) ? 0 : 1;
            set_forced_breakpoint( $mate_index_to_go[$i_break] - $inc );
        }
    }
    else {
        my $type_sequence = $type_sequence_to_go[$i_break];
        if ($type_sequence) {
            my $closing_token = $matching_token{ $tokens_to_go[$i_break] };
            $postponed_breakpoint{$type_sequence} = 1;
        }
    }
}

# check to see if output line tabbing agrees with input line
# this can be very useful for debugging a script which has an extra
# or missing brace
sub compare_indentation_levels {

    my ( $python_indentation_level, $structural_indentation_level ) = @_;
    if ( ( $python_indentation_level ne $structural_indentation_level ) ) {
        $last_tabbing_disagreement = $input_line_number;

        if ($in_tabbing_disagreement) {
        }
        else {
            $tabbing_disagreement_count++;

            if ( $tabbing_disagreement_count <= MAX_NAG_MESSAGES ) {
                write_logfile_entry(
"Start indentation disagreement: input=$python_indentation_level; output=$structural_indentation_level\n"
                );
            }
            $in_tabbing_disagreement    = $input_line_number;
            $first_tabbing_disagreement = $in_tabbing_disagreement
              unless ($first_tabbing_disagreement);
        }
    }
    else {

        if ($in_tabbing_disagreement) {

            if ( $tabbing_disagreement_count <= MAX_NAG_MESSAGES ) {
                write_logfile_entry(
"End indentation disagreement from input line $in_tabbing_disagreement\n"
                );

                if ( $tabbing_disagreement_count == MAX_NAG_MESSAGES ) {
                    write_logfile_entry(
                        "No further tabbing disagreements will be noted\n");
                }
            }
            $in_tabbing_disagreement = 0;
        }
    }
}

1;
