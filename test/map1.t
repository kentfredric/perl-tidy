# From lint.pm
# if you put a break after the opening paren, this will
# fail!
use strict;
use warnings;
my %implies_ok_context;
BEGIN {
    map ( $implies_ok_context{$_}++,
        qw(pp_scalar pp_av2arylen pp_aelem pp_aslice pp_helem pp_hslice
        pp_keys pp_values pp_hslice pp_defined pp_undef pp_delete)
    );
}

# From Msg.pm - same problem with grep
grep ( $routines{ $_ } ++ , @_ , @EXPORT) ;
