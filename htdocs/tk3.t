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

