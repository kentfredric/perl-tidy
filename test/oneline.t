# These should remain one-line:
$$d{"day_abb"}  = [ [qw(Lun Mar Mer Gio Ven Sab Dom)] ];
$$d{"day_char"} = [ [qw(L Ma Me G V S D)] ];

# This should break apart: the block plus opening tokens is 80 characters but
# the line is 81 characters so this has to be broken
$scoresnam->pack('expand' => 'yes', 'side' => 'top', 'fill' => 'y', pady => 5);

