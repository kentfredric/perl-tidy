# These required a special rule in sub store_token_to_go in order
# to line up.  Problem is the blank field between = and {, which must
# be eliminated.
sub init {
    $c->{Lines}     =    [];  # 1
    $c->{TempLine}     = '';
    $self->{Items}     =    {};
    $self->{MaxGroup}  = 0;	 # 2
    $self->{ItemOrder} =    [];
}
