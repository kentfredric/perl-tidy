# This is deeply nested. Note that the deepest nesting of parens, near
# ->Show, does not produce extra indentation.  This is because, at
# present, parens do not get structural indentation, rather only
# continuation indentation.  There may be no really good solution to the
# problems posed by parens, partly because perl flattens lists, but
# except for really deep nesting like this, things usually look ok.
my $file_menu = $mframe->Menubutton(
-text => 'File',
-width => '10',
-menuitems => [
['Button' => '~Disassemble',
-command => \&disassemble 
],
[Button => '~Load file',
-command => sub { 
my $file = $fs
->Show(-popover => $top,
	   -create => 0, 
	   -verify => ['-r']
	   );
$text->Load($file) if (defined $file);
}
],
['Button' => '~Save', 
-command => [ $text , 'Save' ]
],
['Button' => 'Save ~As',
-command => sub { 
my $file = $fs->Show(
		 -popover => $top, 
		 -create => 1, 
		 -verify => ['-w']
		 );
$text->Save($file) if (defined $file);
}
],
'-',
['Button' => '~Exit',
-command => sub { exit; }
]
]
)
->pack( -side => 'left' );

    # example of outdenting terminal '} ) ;'
    $c->Tk::bind(
      '<Control-f>' => sub {
          my ($c) = @_;
          my $e = $c->XEvent;
          itemsUnderArea $c;
    } );
