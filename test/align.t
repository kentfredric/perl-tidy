# some alignment tests

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
