    # som test cases for -lp


    # nested
    $tgt->bind( find build::command::linkedmodule( $progenv,
                                                   $progenv->{LINKMODULECOM} ),
                $env->_Objects( map( $dir::cwd->lookup($_), @_ ) ) );

    &require_file_with_conf_line ($ac_output_line, $FOREIGN,
				      &rewrite_inputs_into_dependencies (0, @inputs));

    # complex; introduced by '='
    my ($this) = DBI::_new_dbh(
                     $drh,
                     {
                         'Name' => $dbname,
                         'User' => $user,
                     }
    );

    # complex list items
    make_node( $rnodes, $nn_box = ++$nn, "housing", $tzero, $tmass,
             $qir_box_nadir + $qs_box_nadir + $qs_box_front,
             "\$ solar albedo + earth infrared +direct sun", \@nodes, \%nname );

    $pdl->set_data_by_mmap( $name, $s, 1, ( $opts->{ReadOnly} ? 0 : 1 ),
        ( $opts->{Creat} ? 1 : 0 ), (0644),
        ( $opts->{Creat} || $opts->{Trunc} ? 1 : 0 ) );

    # broken with side comment
    push @contents, $c->hidden(
               -name => "itemID",     # (unique) code for the item
               -value => "$itemID" );

    # should not break these two long blocks 
    transform([$s, 0, 0, 0, 0, $s, 0, 0, 0, 0, $s, 0, 0, 0, 0, 1],
             [1/$s, 0, 0, 0, 0, 1/$s, 0, 0, 0, 0, 1/$s, 0, 0, 0, 0, 1]);

    # another one-line block 
    local (%preinfo) = ( 'PROTOCOL', 'http', 'HOST', $host, 'PORT', $port, 'PATH', $path, );

    # this should retain the 1-line block in parens
    {
        brenda::talk($self, $to, 100,
         ("Yes?", "Don't touch me.", "brenda pokes $nick back", "What??!"));
    }

    # nested - this is too complex and long & will break into 1 item per line
    brenda::talk($self, $to, 100, 
                ("I won't take such abuse.", "brenda glares at $nick", 
                "I refuse to work in such a hostile workplace!"));

    {
        # nested calls:
        print p( "Download full sourcecode for ",
                 a( { href => "/deckmaster/admin/MxScreen" },
                     "Query Compiler" ), "(" . get_my_size() . ")" );
    }

    # list in a slice
    PDL::PP->printxsc(
                       join "\n\n",
                       @$obj{ StructDecl, RedoDimsFunc,
                       CopyFunc,          ReadDataFunc,
                       WriteBackDataFunc, FreeFunc,
                       FooFunc,           VTableDef,
                       NewXSInPrelude,
                       }
    );
