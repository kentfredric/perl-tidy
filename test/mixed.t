# Mixed commas and comma-arrows; formatter will decide to use old break points 
my @Option_spec = (
    'arch=s',
    'binary|b!',
    'B'			=> sub { $O{'binary'} = 0 },
    'build!',
    'debug!',
    'dir=s',
    'dist|d=s',
    'help!'		=> sub { usage() },
    'host|h=s',
    'install|i!',
    'no|n!',
    'no-config|f!',
    'no-dscverify!',
    'no-user-config|F!',
    'non-us-dir=s',
    'non-us-host|H=s',
    'root-build|r=s',
    'root-install|R=s',
    'source|s!',
    'S'			=> sub { $O{'source'} = 0 },
    'unpack|u!',
    'verbose|v!',
    'version'		=> sub { print "$Me version $VERSION\n"; exit },
);

{{{
            # problem with mixed comma types : should break before '-outline':
            $c->create(
                'rectangle',
                sprintf( "%dc", $x ),
                sprintf( "%dc", $y ),
                sprintf( "%dc", $x + 2 ),
                sprintf( "%dc", $y + 2 ), -outline => 'black',
                -fill => $bg,
                -tags => 'rect'
            );
}}}


# Mixing commas and comma arrows in a list below the threshold for
# formatting does not work well
%e=('%',0,
'^',132918,
'~'=>18054,
'@'=>19630,
'*' =>0b0101100101,
);

# But this is currently a problem:
use constant PI => 4 * atan2 1, 1;

# unconventional mixing of ',' and '=>' will cause trouble, like here:
    $c->createOval(
        $x + $R, $y +
          $R => $x - $R,
        $y - $R,
        -fill => 'black',
    );

    # line break test:
    $last_time_label->configure( -text =>
          sprintf( "%02d/%02d %02d:%02d:%02d", $month, $day, $hour, $minute,
            $second ) );
