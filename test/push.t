                # break before the if:
                push @m, $obj if $obj->id =~ /$regex/i or ( (
                  $] < 5.00303  
                  || $obj->can('name') ) && $obj->name =~ /$regex/i );

                # hard to break well:
                push @add_coords_attr, "$c1 $c2\t" . join (
                    ";", join ( ",", @both ),
                    join ( ",", @forth ), join ( ",", @back )
                );

      # a long block to break
      while (@fields) { push @{ $self->{'fields'} }, shift @fields; shift @fields; }

