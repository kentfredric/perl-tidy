    # the 'sub' block should stay a one-line block
    my @opts = (
      'check'   => \$check,
      'dir|d=s' => \$dir,
      'retain'  => \$retain,
      'force'   => \$force,
      'verbose' => \$verbose,
      'quiet'   => sub { $verbose = 0; },
      'patch=s' => \$patch,
      'trace'   => \$trace,
      'debug'   => \$debug,
      'help'    => \$help
    );

    # another example
    my @Option_spec = (
      'debug!'     => \$Debug,
      'help!'      => sub { usage() },
      'no-sort|n!' => \$No_sort,
      'source-override|s=s' => \$Src_override,
      'version' => sub { print "$Me version $Version\n"; exit; },
    );
