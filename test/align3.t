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

#----------------------------------------------------
# do not align both within and across statements:
# Added aligner variable $last_leading_space_count to fix this.


# Example - like here with { for 'map' and 'foreach'. 
my (@all_labels) =
  map                 { $_->[1] }
  sort                { $a->[0] cmp $b->[0] }
  map                 { [ lc($_), $_ ] } $root->all_pathnames;
foreach (@all_labels) {
    $_ = '(Root)' if !defined $_ || $_ eq '';
    $browse->insert( "end", $_ );
}

{
    # Example with brace and keyword 'while':
    my $compare =
      $sorttype eq 'string'
      ? sub   { $_[0] gt $_[1] }
      : sub   { $_[0] > $_[1] };
    while (1) {
    ##
    }

    # Example with =>
    $msg = build MIME::Entity
      Type           => 'multipart/mixed',
      From           => $email_from,
      To             => $email_to,
      Subject        => 'New kernel(s) available';
    attach $msg Data => $body;

    # another - name is indented, although this is all one stmt
    $w->insert(
        ListBox      =>
          name       => "NameList",
        origin       => [ 25, 25 ],
        size         => [ 225, 315 ],
        items        => [@fontItems],
        onSelectItem => sub {
            &$re_size(1);
            &$re_sample;
        },
    );

    # Example with paren after keyword 'if' 
    die("dblib col_create: called with duplicate column name ``$key''.\n")
      if ( defined( $colnametonum{$key} ) );
    if   ( defined($desired_n) ) {
    ##

    }

    # Example with =~
    die "package_to_path: Invalid package name"
      unless $package =~ s/^Anarres::Mud::Library//;
    $package          =~ s,::,/,g;
}

{{{{
                # Example with ',':
                &B_System(
                    &getGlobal( 'BIN', 'ch_rc' )
                      . " -a $paramstring "
                      . &getGlobal( 'FILE', 'nddconf' ),
                    &getGlobal( 'BIN',      'ch_rc' ) . " -r $paramstring"
                );

}}}}

#----------------------------------------------------

