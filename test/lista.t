my %EuroRates = (
         BEF => {EUR=>0.0247899055505,   BEF => 1},
         DEM => {EUR=>0.511291881196,	 DEM => 1},
         ESP => {EUR=>0.00601012104384,  ESP => 1},
         EUR => {ATS=>13.7603, BEF=>40.3399, DEM=>1.95583, EUR=>1, ESP=>166.386, FIM=>5.94573, FRF=>6.55957, GRD=>340.750, IEP=>.787564, ITL=>1936.27, LUF=>40.3399, NLG=>2.20371, PTE=>200.482}, 
         FRF => {EUR=>0.152449017237, 	 FRF => 1},
         GRD => {EUR=>0.00293470286134,  GRD => 1},
         IEP => {EUR=>1.26973807843, 	 IEP => 1},
         ITL => {EUR=>0.000516456899089, ITL => 1},    
         LUF => {EUR=>0.0247899055505,   LUF => 1},
         NLG => {EUR=>0.45378021609, 	 NLG => 1},
         ATS => {EUR=>0.0726728341679,   ATS => 1},
         PTE => {EUR=>0.00498797897068,  PTE => 1},
         FIM => {EUR=>0.168187926462,	 FIM => 1}
		                  );
# A list without parens, quite a mess
print $query->header, $query->start_html( -title => 'A Simple Example' ),
  $query->startform, "<CENTER><H3>Testing Module Pg</H3></CENTER>",
  "<P><CENTER><TABLE CELLPADDING=4 CELLSPACING=2 BORDER=1>",
  "<TR><TD>Enter conninfo string: </TD>", "<TD>",
  $query->textfield( -name => 'conninfo', -size => 40,
    -default => 'dbname=template1' ), "</TD>", "</TR>",
  "<TR><TD>Enter select command: </TD>", "<TD>",
  $query->textfield( -name => 'cmd', -size => 40 ), "</TD>", "</TR>",
  "</TABLE></CENTER><P>", "<CENTER>", $query->submit( -value => 'Submit' ),
  "</CENTER>", $query->endform;

my %matchwords;
map( $matchwords{ join "", sort split //, $_ } = $_, 'cig',
    'cog',   'cos',
    'cogs',  'cox',
    'go',    'is',
    'ism',   'iso',
    'mig',   'mix',
    'osmic', 'ox',
    'sic',   'sig',
    'six',   'smog',
    'so',    'soc',
    'sog',   'xi' );

(
  $description,   $summary_group,   $shotname,  $materials,
  $weight,        $surface_to_mass, $date,      $hob,
  $gage_file,     $range_min,       $range_max, $median_ovp_wt,
  $median_imp_wt, $avg_weight,      $avg_pct,
  )
  = @_;

{
    # a ragged list
    return $c->create(
        'polygon', $x, $y,
        $x + $rinfo->{size},
        $y + $rinfo->{size},
        $x - $rinfo->{size},
        $y + $rinfo->{size}
    );

    # long last term
    glpcOpenWindow(
        $p{'x'},      $p{'y'},      $p{'width'},
        $p{'height'}, $p{'parent'}, $p{'mask'},
        @{ $p{'attributes'} }
    );

    my @alphabet = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
    );

    (
      $key,    $date, $check, $desc,    $debit,
      $credit, $cat,  $com,   $cleared, $total
      )
      = split ( /\t/, $result );
}

# broken list - testing behavior of closing paren
my %ENTITIES = (

    sol => '/'
);

{
    printf(
        "%-5s %-20s %-7s %-9s %-3s %-5s %-7s %-7s %-7s %-4s %s\n",
        '----',  '-------', '----',  '-------', '--', '----',
        '-----', '-----',   '-----', '----',    '----'
    );

    # just above current threshold for separate first term
    print STDOUT sprintf( "%5s  %s  %-15s  %-43s\n",
        "-" x 5, "----------", "-" x 15, "-" x 43 );

    # just below current threshold for separate first term
    $output = sprintf(
        "%d/%s/%d %d:%d:%d", $time[3], $month[ $time[4] ],
        $time[5] + 1900,     $time[2], $time[1],
        $time[0]
    );

{ 
        # big gaps
        ( $junk, $w[6] ) = radlablist(
            $w,                                   pad( 'Focus',  $p - 6 ),
            $s->{$STYLE}->{Focus}->get_panel_ref, 'File:focus',
            'Click',                              'ClickToFocus',
            'Mouse',                              'MouseFocus',
            'Sloppy ',                            'SloppyFocus'
        );

        # break open with clutter like this (if contains any sublists)
        TouchFile( $TempFiles{'numeric'}, $TempFiles{'alpha'},
            $TempFiles{'blank'} );

        push (
              @{$form->{income_this_period}},
              $form->format_amount($myconfig, $form->{I}{$key}{this}, 1)
              );

{

            push (
                @{ $$self{states} },
                '64', '66', '68', '70', '72',  '74',  '76',
                '78', '80', '82', '84', '86',  '88',  '90',
                '92', '94', '96', '98', '100', '102', '104'
            );

            # we break after the '(' but dont need to break before the ')'
            $self->log(
                '# Here the miror is ended, I can do what ever I want...');

            # This was difficult
            &{
                (
                  $start_element_handlers->{ $event->data->name }
                  || $start_element_handlers->{''}
                  || sub { }
                  )
              } ( $event->data, $event );
            last SWITCH;
} } }

{{
        my ($this) = DBI::_new_dbh( $drh,
            {
                Name     => $dsn,
                User     => $user,
                ado_conn => $conn,
            } );

        # This was difficult:
        push (
            @sp,
            $t2[
              ( $t3[ ( $t1[ ( $ch + $n1 ) & MASK ] + $n2 ) & MASK ] - $n2 ) &
              MASK
              ] - $n1
        );


        # This was difficult:
        $type eq 'start_element' && do {
            &{
                (
                  $start_element_handlers->{ $event->data->name }
                  || $start_element_handlers->{''}
                  || sub { }
                  )
              } ( $event->data, $event );
            last SWITCH;
        };

}}
# this was once mis-tokenized:
@a = (1,2,3,4,5,6,7);
$b = @a[0..5]/2.;
print "b is $b\n";

{
   # break at all commas:
    $$d{"month_abb"} = [
        [
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
        ],
        [], [ "", "", "", "", "", "", "", "", "Sept" ]
    ];
    my ( %data, @row, $line, $obj, %new, $first, $last, $daily, $row, $month );

    # could use odd number of fields here
    my @alphabet = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
        'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
    );

    # a borderline case for list formatting:
    return
      join ( '!', $dircache, $itemcache, $recurse, @$podpath, $podroot,
        stat($dircache), stat($itemcache) );
}
# A list with some math operations 
%unitscale=("in",72,"pt",72.27/72,"pc",12,"mm",72/25.4,"cm",72/2.54,
"\\hsize",100,"\\vsize",100,"\\textwidth",100,"\\textheight",100,
"\\pagewidth",100,"\\linewidth",100);

            # Note extra leading comma
            %string = (
              ,       "saddr",    $stest,  "daddr",
              $dtest, "source",   $sname,  "dest",
              $dname, "shost",    $sname,  "dhost",
              $dname, "sserv",    $stype,  "dserv",
              $dtype, "version",  $vers,   "ihl",
              $chk,   "data",     $data,   "time",
              $tm,    "headers",  $headers,
            );

# note the hash appended to this list; that's what makes it hard to
# do anything other than use an even number of fields
%ignore = (
  'sloppypar',  1, 'document',     1, 'newblock', 1, ',',          1,
  '@',          1, ' ',            1, '-',        1, 'sloppy',     1,
  'hyphen',     1, 'titlepage',    1, 'htmlonly', 1, 'flushleft',  1,
  'flushright', 1, 'slide',        1, 'tiny',     1, 'Tiny',       1,
  'scriptsize', 1, 'footnotesize', 1, 'small',    1, 'normalsize', 1,
  'large',      1, 'Large',        1, 'LARGE',    1, 'huge',       1,
  'Huge',       1, %ignore
);

# a borderline case for formatting; probably looks best as is
@poweroften =
  ( 1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000,
  1_000_000_000 );

%scsi_phases = ( 'DATA_OUT', 0x00_00_00_00, 'DATA_IN', 0x01_00_00_00, 
'CMD', 0x02_00_00_00, 'STATUS',  0x03_00_00_00, 
'MSG_OUT', 0x06_00_00_00, 'MSG_IN',  0x07_00_00_00);

# a long return list.  Last line won't get lined up if there is no
# line break after the ')'.  
($id,$filename,$filesize,$width,$height,$directory,$format,$origin,$cameramake,
$cameramodel,$createdate,$insertdate,$subtype,$status,$copyright,
$lighting,$filmiso,$filmexposure,$filmaperture,$filmmetering,$filmfoclen,
$flash,$quality,$whitebal,$expmode,$country,$city,$keywords,
$comment,$caption,$owner,$thumbsize,$smallwidth,$smallheight,$thumbwidth,
$thumbheight, $pub, $createtm,$inserttm,$refcount,$crapb,$crapc,$crapd,$border) = $Db_Conn->selectrow_array($qry);

# A glitch which has been fixed: the last line was not getting lined up
# because it got continuation indentation. 
  $$d{"num_word"}=
    [["first","second","third","fourth","fifth","sixth","seventh","eighth",
      "ninth","tenth","eleventh","twelfth","thirteenth","fourteenth",
      "fifteenth","sixteenth","seventeenth","eighteenth","nineteenth",
      "twentieth","twenty-first","twenty-second","twenty-third",
      "twenty-fourth","twenty-fifth","twenty-sixth","twenty-seventh",
      "twenty-eighth","twenty-ninth","thirtieth","thirty-first"]];


  # this looks best with line length = 90 or so
  $$d{"day_name"}=
    [["Mandag","Tisdag","Onsdag","Torsdag","Fredag","Lordag","Sondag"],
     ["Måndag","Tisdag","Onsdag","Torsdag","Fredag","Lördag",
      "Söndag"]];
 
    # Both commas and arrows; this gave trouble at one time
    $c->ConfigSpecs(
	-width    => [PASSIVE => undef, undef, 0],
	-length => [PASSIVE => undef, undef, 0],
	-anchor   => [METHOD  => 'anchor', 'Anchor', 'w'],
	-resolution
		  => [PASSIVE => undef, undef, 1.0],
	-highlightthickness
		  => [SELF => 'highlightThickness','HighlightThickness',0],
	-troughcolor
		  => [PASSIVE => 'troughColor', 'Background', 'grey55'],
    );

	# double assignment
    @$stob =
      ( $st_dev, $st_ino, $st_mode, $st_nlink, $st_uid, $st_gid, $st_rdev,
        $st_size, $st_atime, $st_mtime, $st_ctime, $st_blksize, $st_blocks ) =
      @_;

	# a return list
    ( $fields{CONFFILES}, $fields{PRIORITY},
           $fields{ARCH}, $fields{GROUP},
      $fields{SLPKGVERSION},
    ) = unpack( $slp::footer_packstring, $footer );


            # This can be formatted with an odd number of columns (lvalue)
            ( $racine, $vale1, $vale2, $vale3, $vale4,
               $vale5, $vale6, $vale7, $vale8, $vale9 )
              = split;

    # This should be formatted with an odd number of columns but
    # is not currently.  Logic is needed to see that all items
    # are similar.
    my @indices = (
        0x0F, 0x08, 0x0C, 0x0E, 0x17, 0x11, 0x0B, 0x12,
        0x1D, 0x24, 0x0A, 0x16, 0x09, 0x0D
    );

        # Some functions like push and join would be best if
        # treated specially.
        # This might look better with the ";" left on the first line
        $Dat{Data}{ uc $ARG } = join (
            ";",
            (
                uc $ARG, "-- N/A --", 0,       "1/1/1970",
                "00:00", 0,           "0.00%", 0,
                "-",     "-",         "-",     "-",
                "-",     "-",         "-",     "-",
                "-",     "-",         "-",     "-"
            )
        );

    # This is ugly..
    $contents = join (
        '', ( ( $inner_math =~ /in(display|line)/ ) ? '$' : '' ),
        "\\begin{$env}", ( $color_env ? "\\bgroup\\$color_env" : '' ),
        $contents, ( $color_env ? "\\egroup" : '' ),
        "\\end{$env}", ( ( $inner_math =~ /in(display|line)/ ) ? '$' : '' )
    );

{
%morse = (
           ".-",     "a", "-...",  "b",  "-.-.",   "c",  "-..",    "d",
           ".",      "e", "..-.",  "f",  "--.",    "g",  "....",   "h",
           "..",     "i", ".---",  "j",  "-.-",    "k",  ".-..",   "l",
           "--",     "m", "-.",    "n",  "---",    "o",  ".--.",   "p",
           "--.-",   "q", ".-.",   "r",  "...",    "s",  "-",      "t",
           "..-",    "u", "...-",  "v",  ".--",    "w",  "-..-",   "x",
           "-.--",   "y", "--..",  "z",  ".-.-.-", ".",  "--..--", ",",
           "..--..", "?", "-..-.", "/",  ".-.-.",  "+",  "-----",  "0",
           ".----",  "1", "..---", "2",  "...--",  "3",  "....-",  "4",
           ".....",  "5", "-....", "6",  "--...",  "7",  "---..",  "8",
           "----.",  "9", "-...-", "BT", ".-.-.",  "AR", "...-.-", "SK",
           );
}

# a broken list because of side comment
make_node(
    $nn_cnv1 = ++$nn,
    "dc converter cnv1",
    $tzero,
    123.4,    # incorrect, to be updated
    $q_cnv1,
    "\$ power to power board cnv1",
    \@nodes
);

# broken list:
  @symbol = (
       (undef) x 8,		# 1st row
       (undef) x 8,
       (undef) x 8,		# 2nd row
       (undef) x 8,
       undef, undef, '\forall', undef, '\exists', undef, undef, '\???', # 3rd: symbols
       (undef) x 8,
       (undef) x 8,     # 4th: numbers and symbols
       (undef) x 8,
       '\???', ( map {"\\$_"} 
		 qw(Alpha Beta Chi Delta Epsilon Phi Gamma 
		 Eta Iota vartheta Kappa Lambda Mu Nu Omicron 
		 Pi Theta Rho Sigma Tau Ypsilon varsigma Omega
		 Xi Psi Zeta)), undef, '\therefore', undef, '\perp', undef,
       undef, ( map {"\\$_"} 
	        qw(alpha beta chi delta varepsilon phi gamma
		   eta iota varphi kappa lambda mu nu omicron
		   pi theta rho sigma tau ypsilon varpi omega
		   xi psi zeta)), undef, undef, undef, undef, undef,
       (undef) x 8,		# 9st row
       (undef) x 8,
       (undef) x 8,		# 10nd row
       (undef) x 8,
       undef, undef, undef, '\leq', undef, '\infty', undef, undef, # 11th row
       undef, undef, undef, undef, '\from', undef, '\to', undef,
       '\circ', '\pm', undef, '\geq', '\times', undef, '\partial', '\bullet', # 12th row
       undef, '\neq', '\equiv', '\approx', '\dots', '\mid', '\hline', undef,
       '\Aleph', undef, undef, undef, '\otimes', '\oplus', '\empty', '\cap', # 13th row
       '\cup', undef, undef, undef, undef, undef, '\in', '\notin',
       undef, '\nabla', undef, undef, undef, '\prod', undef, '\cdot', # 14th row
       undef, '\wedge', '\vee', undef, undef, undef, undef, undef,
       undef, '\<', undef, undef, undef, '\sum', undef, undef, # 15th row
       (undef) x 8,
       undef, '\>', '\int', (undef) x 5, # 16th row
       (undef) x 8,
      );

# Broken list with =>'s
%Slinke::COMMANDS = ( CMD_PORT_DONE      => 0x00,  CMD_PORT_SM        => 0x1F,
		      # port commands
		      # general
		      CMD_DISABLE        => 0x02,  CMD_ENABLE         => 0x03,
		      # S-Link
		      CMD_SENDBITMODE    => 0x04,
		      # ir
		      CMD_SETIRFS        => 0x04,  CMD_GETIRFS        => 0x05,
		      CMD_SETIRCFS       => 0x06,  CMD_GETIRCFS       => 0x07,
		      CMD_SETIRTIMEOUT   => 0x0C,  CMD_GETIRTIMEOUT   => 0x0D,
		      CMD_SETIRMINLEN    => 0x0E,  CMD_GETIRMINLEN    => 0x0F,
		      CMD_SETIRTXPORTS   => 0x08,  CMD_GETIRTXPORTS   => 0x13,
		      CMD_SETIRRXPORTEN  => 0x09,  CMD_GETIRRXPORTEN  => 0x12,
		      CMD_SETIRPORTECHO  => 0x0A,  CMD_GETIRPORTECHO  => 0x10,
		      CMD_SETIRRXPORTPOL => 0x0B,  CMD_GETIRRXPORTPOL => 0x11,
		      # serial
		      CMD_SETBAUD        => 0x08,  CMD_GETBAUD        => 0x09,
		      # parallel
		      CMD_SETHSMODE      => 0x10,  CMD_GETHSMODE      => 0x11,
		      CMD_SETDIR         => 0x12,  CMD_GETDIR         => 0x13,
		      CMD_SAMPLE         => 0x14,
		      # system
		      CMD_GETVERSION     => 0x0B,  CMD_GETSERIALNO    => 0x0C,
		      CMD_SETSERIALNO    => 0x0D,  CMD_SAVEDEFAULTS   => 0x0E,
		      CMD_LOADDEFAULTS   => 0x0F,  CMD_RESUME         => 0xAA,
		      CMD_RESET          => 0xFF,
		      # custom for SEG
		      CMD_PLAYMACRO1     => 0x10,  CMD_PLAYMACRO2     => 0x11,
		      CMD_STOREMACRO1    => 0x12,  CMD_STOREMACRO2    => 0x13,
		    );
