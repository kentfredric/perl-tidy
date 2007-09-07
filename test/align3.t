# Old Problem, Unwanted alignment here opening parens of thread not aligned:
PDL::Primitive::inner(
    $this->{EigVec}->thread( 0, -1 ),
    $tmp->thread( -1,           0 ),
    $this->{ ( $inv ? "ICV" : "CV" ) }->thread( 0, 1 )
);

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

    #   never align => and ,
    #   was bad alignment, now fixed with patch in sub check_match
    $self->{editor} = $self->insert(
        Editor   => name            => 'Edit',
        textRef  => \$cap,
        origin   => [ 0,            22 ],
        size     => [ $self->width, $self->height - 22 ],
        hScroll  => 1,
        vScroll  => 1,
        growMode => gm::Client,
    );

# another example
    $self->{painter} = $self->insert(
        ImageEdit::Painter => origin               => [ 64, 64 ],
        size               => [ $self->width - 64, $self->height - 64 ],
        limitX             => $profile{imageSize}->[0],
        limitY             => $profile{imageSize}->[1],
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

#############################################################
# OLD Problem: Unwanted alignment of commas (from test8/penco.pl)
# Suggest: require same number of commas if both lines end in ;
# OR: Need to make ( a token here too, marked with '$push_array_mini1->'
# just as if the -> weren't there.  Then they won't match up.
#############################################################
    $push_array_min1->( 'phi1', 5 );
    $push_array_min1->( 'c2',   5 );
    $push_array_min1->( 'phi2', 5 );
    $push_scalars->( 'a7',      'degrad', 'bardyn', 'a9' );

#############################################################
# OLD Problem: Unwanted alignment 
#############################################################
# Shouldn't really align if's here. Not different levels!
Boucherot::log::error(
"IRC: \$socketId(=$socketId) ne \$self->{'_socketId'}=($self->{'_socketId'})"
)        if ( $self->{'_socketId'} ne $socketId );
return 0 if ( $self->{'_socketId'} ne $socketId );

# TODO: shouldn't align because of differing leading whitespace:
chmod 0666, $root
  or carp "Can't make file $root writeable: $!" if $force_writeable;
print "unlink $root\n"                          if $verbose;

# looks poor; maybe reduce gap test? 
next         if /rebuilding alias database/;
print stderr if $flags{'e'};

$hds->configure( -height       => 900 );
$hds->configure( -scrollregion => [ $hds->bbox("all") ] );
use constant SLICE    => 10000;
use constant BIGSLICE => SLICE * SLICE;

( $msg, $defstyle ) = do {
        $i == 1 ? ( "First", "Color" )
      : $i == 2 ? ( "Then",  "Rarity" )
      :           ( "Then",  "Name" );
};
