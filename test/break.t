{{{
# break at '-' not '->'
                $temptotalr +=
                  $charges[$i2]->{'amount'} - $charges[$i2]
                  ->{'amountoutstanding'};

                  # break at -> not {
                  $gControlFields{$lMarcTag}->clusters->{
                    substr($gPosDefData, 0, 1)
                  }->field_length;

                # break at -> not ','
                defined $self->{_parent}->{_fh}->read($buf,
                                                $self->{_parent}->{_blocksize});
}}}
{

    # This is difficult because the map statement
    # We want to break at the ',' before 'join' and 'map' to display
    # the call parameters
    $fh->printf(
            "\n\t%s table, %s, %s, subFeatureFlags = %08x # %s (%s)\n",
            subtable_type_($type), $_->{'direction'}, $_->{'orientation'},
            $subFeatureFlags,
            "Default "
              . ( ( ( $subFeatureFlags & $defaultFlags ) != 0 ) 
              ? "On"
              : "Off" ), join (
                ", ", map {
                    join ( ": ",
                        $feat->settingName( $_->{'type'}, $_->{'setting'} ) );
                  } grep { ( $_->{'enable'} & $subFeatureFlags ) != 0 }
                  @$featureEntries
              )
        );
    }

    # '||' alignment:
    $to = $me->get('Reply-To')
       || $me->get('From')
       || $me->get('Return-Path')
       || "";

        # This used to get broken up because the comma after '1.0'
        # triggerd a fake forced breakpoint.  
	return 1 unless ( eval { $t->tagNextrange( sel => '1.0', 'end' ) });

    # do not break this mixed one-line block:
    $over->Apply( -entercommand => $X, $Y, 0 );

# should break after the initial '[' and before the ending ] 
my $ado_info = [ qw{
      TYPE_NAME DATA_TYPE COLUMN_SIZE LITERAL_PREFIX
      LITERAL_SUFFIX CREATE_PARAMS IS_NULLABLE CASE_SENSITIVE
      SEARCHABLE UNSIGNED_ATTRIBUTE FIXED_PREC_SCALE AUTO_UNIQUE_VALUE
      LOCAL_TYPE_NAME MINIMUM_SCALE MAXIMUM_SCALE GUID TYPELIB
      VERSION IS_LONG BEST_MATCH IS_FIXEDLENGTH
      } ];

    # do not break before the ');'
    open INFILE_COPY, ">$input_file_copy"
      or die ( "Couldn't open $input_file_copy: $!\n
                	It is needed to check syntax; deactivate with -nsyn" );


    # do not break before the ');'
            last
              if ( ( $i_test == $imax )
              || ( ( $i_lowest >= 0 ) && $too_long ) );

    {
        return $pdl->slice(join ',',(map {
                        $_ eq "X" ? ":" :
                        ref $_ eq "ARRAY" ? join ':',@$_ :
                        !ref $_ ? $_ :
                        die "INVALID SLICE DEF $_"
                } @_));
    }

    # long first term but all other items fit on one line; do not format
    $s = sprintf(
        "%2d wallclock secs (%$f usr %$f sys + %$f cusr %$f csys = %$f CPU)",
        $r, $pu, $ps, $cu, $cs, $tt
      )
      if $style eq 'all';
