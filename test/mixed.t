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


# Mixing commas and comma arrows in a list below the threshold for
# formatting does not work well
%e=('%',0,
'^',132918,
'~'=>18054,
'@'=>19630,
'*' =>0b0101100101,
);
