	    # This is ugly; be sure the ending ');' goes out to zero level here
            &write_warnings(
"\n\\end{$env $br_id} not found (truncated at next section boundary).\n");
        ( defined(&$funct) ? $_ = &$funct($_) : do {
              &write_warnings(
              "\nCould not find translation function for $default_language.\n\n"
              );
          }
          );

	    # be sure the ');' gets outdented back in line with '&'
            &write_warnings(
"\n\\end{$env $br_id} not found (truncated at next section boundary).\n"
              );
