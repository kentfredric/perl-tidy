# some alignment tests

# good test line:
die sprintf("Usage: %s [ -r | -a | -f fmt ] file ...\n", ($0 =~ m|.*/(.*)|o))
    if ($opt_h || (! @ARGV) || (($opt_a && $opt_r) || ($opt_a && $opt_f) ||
                                ($opt_r && $opt_f)));
if ( abs($offhour) >= 24 ) {
    die ("822-date: local time offset greater than or equal to 24 hours\n");
}

# From unicode.pl
# should line up the leading commas
%low_entities = (    #
                  %iso_8859_low_ents, '255', '376'
                  ,                   '257', '256'
                  ,                   '259', '258'
                  ,                   '261', '260'
                  ,                   '263', '262'
                  );

printf(
    "%s, %2d %s %d %02d:%02d:%02d %s%02d%02d\n",
    ( Sun, Mon, Tue, Wed, Thu, Fri, Sat )[ $localtm[6] ],    # day of week
    $localtm[3],                                             # day of month
    ( Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec )
      [ $localtm[4] ],                                       # month
    $localtm[5] + 1900,                                      # year
    $localtm[2],                                             # hour
    $localtm[1],                                             # minute
    $localtm[0],                                             # sec
    ( $offset >= 0 ) ? '+' : '-',    # TZ offset direction
    abs($offhour),                   # TZ offset hour
    $offmin,                         # TZ offset minute
) || die "822-date: output error: $!\n";

{
    # Examples of alignment oppoturnities for a trailing line
    # would be nice to align the 'rgb->slice' here:
    return ( $l,
             $rgb->slice("${s},(0)"),
             $rgb->slice("${s},(1)"),
             $rgb->slice("${s},(2)") );

    # another example:
    chmod( 0666,
           $TempFiles{'numeric'}, $TempFiles{'alpha'}, $TempFiles{'blank'} );

    PDL::gl_triangles( ( map { $points->slice($_) } @sls1 ),
                       ( map { $this->{Colors}->slice($_) } @sls1 ) );

    # another - note useless parens
    printf( "%s|%d|\n",
            substr( $field3, 0, 10 ), ( substr( $field4, 1, 9 ) * 100 ) );
    # break at comma before arrow
    return (
        %::PDL_OPTIONS, 'NAME' => $mod,
        'VERSION_FROM' => "$w/Basic/Core/Version.pm",
        'TYPEMAPS'     => [ &PDL_TYPEMAP() ],
        'OBJECT'       => "$pref\$(OBJ_EXT)",
        PM             => { "$pref.pm" => "\$(INST_LIBDIR)/$pref.pm" },
        MAN3PODS       => { "$pref.pm" => "\$(INST_MAN3DIR)/$mod.\$(MAN3EXT)" },
        'INC'          => &PDL_INCLUDE(),
        'LIBS'         => [''],
        'clean' => { 'FILES' => "$pref.xs $pref.pm $pref\$(OBJ_EXT) $pref.c" },
    );

    # shouldn't match these:
    $pri = $f2pricf      {$f};
    $pri = $sourcedefault{'Priority'} if !length($pri);

    $levels_to_go[$max_index_to_go]        = $level;
    $nesting_depth_to_go[$max_index_to_go] = ( $slevel >= 0 ) ? $slevel : 0;

    # the '=' should align
    local ($article_no) = pop (@_);
    local ($sub_line)   = "";

    # Good alignment example:
    return
      unless ( ( $sl =~ m/^(.*\D)(\d+)\s*o\s*f\s*(\d+)/i )
        || ( $sl =~ m/^(.*\D)(\d+)\s*f\s*o\s*(\d+)/i )
        || ( $sl =~ m/^(.*\D)(\d+)\s*\/\s*(\d+)/ )
        || ( $sl =~ m/^(.*\D)(\d+)\s*\|\s*(\d+)/ )
        || ( $sl =~ m/^(.*\D)(\d+)\s*\\\s*(\d+)/ ) );

    # from Cookbook:
    $page  =    /Mac/            && 'm/Macintrash.html'
             || /Win(dows )?NT/  && 'e/evilandrude.html'
             || /Win|MSIE|WebTV/ && 'm/MicroslothWindows.html'
             || /Linux/          && 'l/Linux.html'
             || /HP-UX/          && 'h/HP-SUX.html'
             || /SunOS/          && 's/ScumOS.html'
             ||                     'a/AppendixB.html';

    $self->{RM_F}       ||= "rm -f";
    $self->{RM_RF}      ||= "rm -rf";
    $self->{TOUCH}      ||= "touch";
    $self->{TEST_F}     ||= "test -f";
    $self->{CP}         ||= "cp";
    $self->{MV}         ||= "mv";
    $self->{CHMOD}      ||= "chmod";
    $self->{UMASK_NULL} ||= "umask 0";
    $self->{DEV_NULL}   ||= "> /dev/null 2>&1";

    s/"//                               || next;
    s/",([x\d]+),([x\d]+),([x\d]+),.*// || next;

    if ( $menu_type == 2 ) {
        &print_nl( "a - Select all items.",                                1 );
        &print_nl( "m - Select based on a case-insensitive string match.", 1 );
        &print_nl( "c - Clear all selections.",                            1 );
    }

    $self->{javascriptErrorHandlerBaseWindowName} = "";
    $self->{linkTag}                              = [];
    $self->{baseHrefString}                       = "";
    $self->{baseTargetString}                     = "";
    $self->{linkStyleSheet}                       = "";

    $worksheet->write( 0, 0, "Index",                                $heading );
    $worksheet->write( 0, 1, "Index",                                $heading );
    $worksheet->write( 0, 3, "Style",                                $heading );
    $worksheet->write( 0, 5, "The style is highlighted in red for ", $heading );

    &logr( "              iptables -A INPUT -j DENY\n", $logr_files_aref );
    &logr( "\n",                                        $logr_files_aref );

    {
        # This was an alignment problem once:
        $data = $pkg->new(
            PeerAddr => join ( ".", @port[ 0 .. 3 ] ),
            PeerPort => $port[4] * 256 + $port[5],
            Proto    => 'tcp'
        );

        s/^([^_-]+)_([^_-]+)_([^_-]+)$/$1-$2-$3/ if (/\s/);
        s/_/-/g                                  if (/\s.+\s/);

        print '      <p><i><font size="-1">Disclaimer:</font></i></p>', "\n";
        print '      <p>',                                              "\n";
        print '      <form method="post" action="">',                   "\n";

        {
            # one per line please
            unless ( ( $line =~ /^SUBJECT>/i ) || ( $line =~ /^ADMIN>/i )
                || ( $line =~ /^POSTER>/i ) || ( $line =~ /^EMAIL>/i )
                || ( $line =~ /^DATE>/i )   || ( $line =~ /^EMAILNOTICES>/i )
                || ( $line =~ /^IP_ADDRESS>/i ) || ( $line =~ /^<!--/i )
                || ( $line =~ /^PASSWORD>/i )   || ( $line =~ /^PREVIOUS>/i )
                || ( $line =~ /^NEXT>/i )       || ( $line =~ /^IMAGE>/i )
                || ( $line =~ /^LINKNAME>/i )   || ( $line =~ /^LINKURL>/i )
                || ( $line =~ /^<([^>])*>&gt;/i )
                || ( $line =~ /^<([^>])*>$AutoQuoteChar/i )
                || ( $line =~ /^<([^>])*>$/i ) )
            {
                $quotedtext .= $line;
            }

            print STDERR "+"                                  if $verbose == 1;
            print STDERR "Error reading $File::Find::name \n" if $verbose == 2;

            {

                print "IF "                          if !$applyall;
                print "<b>[Unconditional Rule]</b> " if $applyall;

            }
        }
    }
}


# alignment of { and = works well here: 

while (<DESC>) {
next if /^#/;
chop;
@field = split (' ');
last if $field[0] eq "charset";
if ( $field[0] eq "res" )       { $resolution = $field[1]; }
if ( $field[0] eq "unitwidth" ) { $unitwidth  = $field[1]; }
if ( $field[0] eq "sizescale" ) { $sizescale  = $field[1]; }
}

# align Numbers and Quotes , and keyword 'unless'
$Indent  = 2 unless defined $Indent;
$Purity    = 0 unless defined $Purity;
$Pad  = "" unless defined $Pad;
$Varname   = "VAR" unless defined $Varname;

# another matrix
my $xyz_shield = [
    [ -0.060,  -0.060,  0. ],    [ 0.060,   -0.060,  0. ],
    [ 0.060,   0.060,   0. ],    [ -0.060,  0.060,   0. ],
    [ -0.0925, -0.0925, 0.092 ], [ 0.0925,  -0.0925, 0.092 ],
    [ 0.0925,  0.0925,  0.092 ], [ -0.0925, 0.0925,  0.092 ],
];

# this used to be aligned, but is not now because of new rules
my ( $num, $numi, $numj,   $xyza, $ka,   $xyzb,  $kb, $aff, $error );
my ( $i,   $j,    $error,  $aff,  $asum, $avec );
my ( $km,  $area, $varea );

# note that original aligns common word endings - cannot do that
my $classmac_output_file = $outdirname.$schoolcode."classmac.txt";
my  $classpc_output_file = $outdirname.$schoolcode."classpc.txt";
my   $barmac_output_file = $outdirname.$schoolcode."barmac.txt";
my    $barpc_output_file = $outdirname.$schoolcode."barpc.txt";
close    MACOUTFILE;
close     PCOUTFILE;
close BARMACOUTFILE;
close  BARPCOUTFILE;

# combinations of tokens to line up, with varying line lengths
SWITCH: {
/phone/ && do    { $field = "special",  $action        = "tel", last SWITCH; };
/tel/ && do      { $field = "special",  $action        = "tel", last SWITCH; };
/fax/ && do      { $field = "special",  $action        = "fax", last SWITCH; };
/birthday/ && do { $field = "birthday", last SWITCH; };
/email/ && do    { $field = "email",    last SWITCH; };
/letter/ && do { $field = "special", $action = "letter", last SWITCH; };
}

# At present, a break will be placed after the leading '('
# to simplify lining up the comma-arrows
%accent_type = ( '18' => 'grave',    # \`
                 '19' => 'acute',    # `'
                 '20' => 'caron',    # \v
                 '21' => 'breve',    # \u
                 '22' => 'macr',     # \=
                 '23' => 'ring',     #
                 '24' => 'cedil',    # \c
                 '94' => 'circ',     # \^
                 '95' => 'dot',      # \.
                 '7D' => 'dblac',    # \H
                 '7E' => 'tilde',    # \~
                 '7F' => 'uml',      # \"
);


# This illustrates a problem that can happen with comments:
# Comments do not get moved with hash list items.  That would
# be hard to detect and fix.
my %fallback=(
	# preferred frontend		# fall back to
	'Web'			=>	'Gtk',
	'Dialog'		=>	'Slang',
	'Gtk'			=>	'Dialog',
	'Text'			=>	'Dialog',
	'Slang'			=>	'Dialog',
);

		    {{{
		    # example of some undesirable whitespace added by aligner;
                    set_list_breakpoints(
                      $item_count,       $identifier_count_stack[$dd],
                      $comma_index[$dd], $interrupted_list[$dd],
                      $max_length[$dd] )
                      unless $dont_align[$dd];
		      }}}

# two things to note here: (1) this will be broken by the compound list rules,
# (2) vertical alignment is bad because 'undef's are not (yet) allowed
# to match with quotes and numbers.  This needs to be fixed.
my $ti =
          [ $names,
            [ 'Not Done Yet', 12, 2000, '\'', '\'', 'max length', 1, 1, 3,
                undef, '0', '0', undef, undef, undef ] ];
