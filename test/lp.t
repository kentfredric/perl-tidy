    # som test cases for -lp

    # nested
    $tgt->bind( find build::command::linkedmodule( $progenv,
                                                   $progenv->{LINKMODULECOM} ),
                $env->_Objects( map( $dir::cwd->lookup($_), @_ ) ) );

    # complex; introduced by '='
    my ($this) = DBI::_new_dbh(
                     $drh,
                     {
                         'Name' => $dbname,
                         'User' => $user,
                     }
    );

    # broken with side comment
    push @contents, $c->hidden(
               -name => "itemID",     # (unique) code for the item
               -value => "$itemID" );

    # nested - this will break into 1 item per line
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
