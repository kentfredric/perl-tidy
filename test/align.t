# some alignment tests

{
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
        s/^([^_-]+)_([^_-]+)_([^_-]+)$/$1-$2-$3/ if (/\s/);
        s/_/-/g                                  if (/\s.+\s/);

        print '      <p><i><font size="-1">Disclaimer:</font></i></p>', "\n";
        print '      <p>',                                              "\n";
        print '      <form method="post" action="">',                   "\n";

        {
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
