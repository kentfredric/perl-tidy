    my %ret_hash = map { $_ => $current_package{$_} } 
        qw(NAME TITLE AUTHOR VERSION ABSTRACT PERLCORE_VER);

    %SORT_ORDER = map { $_ => $n++ } (
        'forbidden', 'not auth', 'not found', 'empty',
        'dns',       'timeout',  'error',     'unknown',
        'multi',     'moved',    'redirect',  'ok',
        'unchecked'
    );

    # broken by comment
    @modified =
      map { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      # -M is when $_ was last modified
      map { [ $_, -M ] } @filenames;

    # Note the missing semicolon at the end; currently this will not
    # be added because this is a map block.
    return map {
        my $tmp = substr( $$_, $l );
        $tmp =~ s/^\s+//;
        $tmp
    } @{ $me->{'mail_hdr_hash'}{$tag} };

    # A modification has been made to prevent a semicolon from being added:
    my @ciphertext = map {
        chr(
            ( ord($_) + ord( $motcle[ $i++ % @motcle ] ) - 2 * 97 ) % 26 + 97 )
    } split ( //, $plaintext );

    # (one line block - no semicolon would be added here):
    my @plaintext =
      map { chr( ( ord($_) - ord( $motcle[ $i++ % @motcle ] ) ) % 26 + 97 ) }
      split ( //, $ciphertext );

    # These can be difficiult to break well
    push ( @{"${pkg}::$var"}, map { $eort_tags{$_} ? @{ $export_tags{$_} } : scalar( ++$nontag, $_ ); } (@$syms) ? @$syms : keys %export_tags );

    PDL::gl_triangles( ( map { $points->slice($_) } @sls1 ), ( map { $this->{Colors}->slice($_) } @sls1 ) );

    # it looks better, like this, when formatted as a list
    # The problem is deciding when to format as a list
    PDL::gl_triangles(
      ( map { $points->slice($_) } @sls1 ),
      ( map { $this->{Colors}->slice($_) } @sls1 )
    );

    # This was a problem at one time but seems ok now
    sub aaa {
        map { $_->[0] }
          sort { $a->[1] cmp $b->[1] }
          map {
            [ $_, &$xform( $_ . "" ) ]
        } @_;
    }

# retain one-line map blocks here:
sub NXArgs {
	my($parnames,$parobjs,$onames,$oobjs) = @_;
	my $pdltype = new C::Type(undef,"pdl *__foo__");
	my $nxargs = [
		( map {[$_,$pdltype]} @$parnames ),
		( map {[$_,$oobjs->{$_}]} @$onames )
	];
	return $nxargs;
}

{
    my $res = [
        map { $v[$_] + $mult1 * $cp[$_] + 
	( $mult2 - $cpl / $tl ) * $cp2[$_] } 0 .. 2 ];
}

# check for line break at comma:
{
    push @INC, map { File::Spec->catfile( $_, 'lib' ) } ( $PARENTDIR, $THISDIR );
}

# break will be at '=' here
{
    ( $y, $M, $w, $d, $h, $m, $s ) = map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );
}

# this will create single lines with opening parens
{
      (
      map {
          my $op = $_;
          (
          $op => sub {
              my $foo = $_[0]->null();
              PDL::Ops::my_biop2( &PDL::Core::rswap, $foo, $op );
              $foo;
          } );
      }
      @PDL::biops2 );
}

{{
                @list = map {
                    $frm{ ( /@(.*?)>/ ? $1 : $_ ) }++ ? () : ($_);
                } @list;
}}

# a section of code to clean up
{
    ( $y, $M, $w, $d, $h, $m, $s ) = map { 1 * $_; } (
      $y, $M
      ,   $w
      ,   $d
      ,   $h
      ,   $m
      ,   $s
    );
}

# long code block chains should go one per line
{
            map { $_->[0] } sort { $a->[1] cmp $b->[1] }
              map { [ $_, &$xform( $_ . "" ) ] } @_;
}

# Bond strength should break at } split instead of comma:
{
    my @loads = reverse map { $_ > LOAD_MAX ? LOAD_MAX: $_ } split /,\s*/,
      $up_string;
}

# break between return and map:
{{
return map { my %h; @h{@$fields} = @$_{@$fields}; \%h } 
@{ $self->{'_data'}} 
}}

return [
    map {
	$sth->{'Database'}->{'xbase_tables'}
	  ->{ $parsed_sql->{'table'}[0] }->field_decimal($_);
    } @{ $parsed_sql->{'fields'} }
];
