        # pack.t: Test file for future update to scan_list
        # possible flag to break after closing }
        $f_offset->Button(
          -text    => 'Calculate',
          -command => sub {
              $gotton = calc($offset);
          } )->pack();

# cuddled paren test
$mw->Button(-text => "New Document",
            -command => \&new_document)->pack(-side => 'bottom',
                                              -anchor => 'e');
