# this was once mis-tokenized:
@a = (1,2,3,4,5,6,7);
$b = @a[0..5]/2.;
print "b is $b\n";

{
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

