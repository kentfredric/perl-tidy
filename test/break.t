	# This currently gets broken up even though it is a single line
	# The reason is that the comma after '1.0' triggers a fake forced
	# breakpoint.  But it looks ok broken up because the nesting
	# depth is so great.
	return 1 unless ( eval { $t->tagNextrange( sel => '1.0', 'end' ) });


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

    # a complex example, with multiple levels and one comma
    grep( do {
          if ( $i == $depth ) { $_++; }
          elsif ( $i > $depth ) { $_ = 0; }
          $i++;
          0;
      },
      @curr_sec_id );
