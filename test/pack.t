$funcframe[0]->Button(-text => '+',
		      -command => \&funcadd
		     )->pack(-side => 'left');

$yframe->LabEntry(-label => 'Y von',
		  -textvariable => \$y_from,
		  -width => 6,
		  -labelPack => [-side => 'left'])->pack(-side => 'left');

$database{'Lundin Links'} = new Megalith(
                                         'Lundin Links',
                                         'Fife',
                                         'NO 404 027',
                                         'Standing Stones',
                                         'Description of Lundin Links'
)->pack();

my $c = $top->Canvas(-border => 0,
		     -height => $height,
		     -width => $width,
		     -highlightthickness => 0)->pack;

{
                # break open container for a complex arg like this:
                $res .=
                  pack($hints[$num][2] == 1 ? 'C*' : 'n*',
                       @words[1 .. $#words]);
}

# cuddled paren test
$mw->Button(-text => "New Document",
            -command => \&new_document)->pack(-side => 'bottom',
                                              -anchor => 'e');
        # pack.t: test for breaking after the final '}'
        # possible flag to break after closing }
        $f_offset->Button(
          -text    => 'Calculate',
          -command => sub {
              $gotton = calc($offset);
          } )->pack();


