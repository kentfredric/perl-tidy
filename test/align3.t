# align3.t: this requires the rule that line with fewer fields cannot increase
# column spacing (sub append).  The second line cannot join the first
# line because of this rule, so first line gets flushed.
push ( @suffixes, '.c', '.o', '.S', '.s' );
push ( @suffixes, '.obj' ) if $seen_objext;
push ( @clean,    'compile' );

# Here is how it used to look!
push ( @suffixes, '.c',                    '.o', '.S', '.s' );
push ( @suffixes, '.obj' ) if $seen_objext;
push ( @clean,    'compile' );
