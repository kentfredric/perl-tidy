# The map block is 79 spaces and has no ci
{
    my $res = [
        map {
$v[ $_ ] + $mult1 * $cp[ $_ ] + ( $mult2 - $cpl / $tl ) * $cp2[ $_ ];
          } 0
          .. 2
    ];
}


	# note that we have ci+level indentation here (Cmd.pm) 
	# but only need really the level indentation.  When 'ci' is
	# computed in the tokenizer, it doesn't knowif the initial
	# line will be broken or not, so it can't tell.
        my $str = join ( " ", map {
              /\n/ ? do { my $n = $_; $n =~ tr/\n/ /; $n }
                : $_;
        } @_ ) . "\015\012";

# two-levels of non-structural indentation, but only one 'ci' level:
    $deps = control_fields(
      ( "Pre-Depends", "Depends",  "Recommends", "Suggests",
      "Conflicts",     "Provides" )
    );

		# This is hard to format:
        @opts{
          qw( name field_names field_types field_lengths
          field_decimals )
        } = 
        ( $filename, @{$parsed_sql}{
          qw( createfields
          createtypes createlengths createdecimals )
        } );
