#  break before the if:
                push @m, $obj if $obj->id =~ /$regex/i or ( (
                  $] < 5.00303  
                  || $obj->can('name') ) && $obj->name =~ /$regex/i );


      # a long block to break
      while (@fields) { push @{ $self->{'fields'} }, shift @fields; shift @fields; }
