    # terminal arrow should get type 't'
    $w->Button(-text => 'OK', -command => [$w => 'destroy'])->
      pack(-side => 'bottom', -pady => 2);
