async {
    my $sig;
      while ( $sig = await_signal() ) {
        &$sig();
    }
};

# Illustration of tokenizer code block types
# The tokenizer uses the token just before the opening '{' to label the block
doit();
sub doit {
print "I'm a block named 'sub doit'\n";
{
    {
        {
            print "hello this is a '{' block\n";
            { print "But I'm a ';'"; print " block \n"; }
            funny: { print "I'm a ':' "; print "block \n"; }
            print "goodbye from the '{' block\n";
        }
    }
}

{ print "hello this is a '}' block\n"; }

return { print "I'm a hash reference executing some code!\n" }

}

# Some block tests
print "start main running\n";
die "main now dying\n";
END {$a=6; print "1st end, a=$a\n"} 
CHECK {$a=8; print "1st check, a=$a\n"} 
INIT {$a=10; print "1st init, a=$a\n"} 
END {$a=12; print "2nd end, a=$a\n"} 
BEGIN {$a=14; print "1st begin, a=$a\n"} 
INIT {$a=16; print "2nd init, a=$a\n"} 
BEGIN {$a=18; print "2nd begin, a=$a\n"} 
CHECK {$a=20; print "2nd check, a=$a\n"} 
END {$a=23; print "3rd end, a=$a\n"} 

            # FIXME: at present the '{' after the '@' is not set as type BLOCK, even
            # though it is.  This should be changed eventually after some
            # associated formatting issues are resolved.
            my ( $pre, $post ) = @{
                {
                    "pp_anonlist" => [ "[", "]" ],
                    "pp_anonhash" => [ "{", "}" ]
                }->{ $kid->ppaddr }
              };
