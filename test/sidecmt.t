# some side comments to line up

# simple vertical alignment of '=' and '#'
my $lines  = 0; # checksum: #lines
my $bytes  = 0; # checksum: #bytes
my $sum  = 0; # checksum: system V sum
my $patchdata = 0; # saw patch data
my $pos  = 0; # start of patch data
my $endkit = 0; # saw end of kit
my $fail  = 0; # failed

# keep '=' lined up even with hanging side comments
$ax         = 1;    # side comment
                    # hanging side comment
$boondoggle = 5;    # side comment
$beetle     = 5;    # side comment
                    # hanging side comment
$d          = 3;

# A more complicated case.  
# Formerly the first line did not get lined up because the ',' gives it
# a different pattern from the others.
# The 6th and 7th go through sub 'combine_fields' to get lined up
local ( $qL, $qR );  # left and right quote chars, like `' or ()
local ($quote_level);   # current quote level
local ($max_quote);# deepest we've gotten
local ($qstring);  # tmp space for quote
local (@quotes);     # list of quotes to return
local ($d) = '\$';       # this line has an = and so does the next
local ($b) = '\\';   # they go through 'combine_fields'
local (@done);   # which quotes we've finished so far

my %fields = ( tag => 1,    # old/new
          name  => 2,    # given name on command line
    root  => 3,    # real (physical) directory
            base  => 4,    # basename (for archives)
         man   => 5,    # name of manifest
            manfn => 6,    # same, real file name
       files => 7,    # list of files
);

# Note that side comments at different indentation levels should not be aligned
{
    # part of table from banner..last side comment was not aligned
    @data_table = (

        #0     1     2     3     4     5     6     7     8     9
        129, 227, 130, 34,  6,   90, 19,  129, 32, 10,     #    0
        74,  40,  129, 31,  12,  64, 53,  129, 30, 14,     #   10
        63,  13,  80,  4,   129, 67, 17,  129, 71, 13,     # 9250
        129, 74,  10,  129, 78,  6,  129, 80,  4,  131,    # 9260
        193                                                # 9270
    );

    {
        {
            {
                {
                    { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
                } #end level 4
            } # end level 3
        } # end level 2
    } # end level 1
} # end level 0

# should outdent '};' with following side comment
$VERSION = do {
    my @r = (q$rEvIsIoN: 1.2 $ =~ /\d+/g);
    sprintf "%d." . "%02d" x $#r, @r;
  };    # must be all one line, for MakeMaker

# Side comments problems...
# The main problem here is that any memory of the side comment alignment
# column is thrown away whenever indentation level changes (and so is all
# other vertical alignment -- I found that aligning across indentation
# level changes is not a good idea).  Since this particular block has lots
# of indentation changes, it comes out very messy at present.

    if ( not $on ) {    # --- ....
        return "ignore" if ( not $Dn and not $oD );    # --- --- ---
        return "ignore" if ( $Dn eq "ref" and $oD eq "ref" )    # --- ref ref
    } elsif ( $on eq "ref" ) {    # ref ....
        return "ignore" if ( not $Dn and $oD eq "ref" );    # ref --- ref
        if ( $Dn eq "ref" ) {                               # ref ref ....
            return "recurse_into" if ( not $oD );         # ref ref ---
            return "recurse_into" if ( $oD eq "ref" );    # ref ref ref
        }
        return "ignore" if ( $Dn eq "new" and $od eq "missing" ); # ref new miss
    } elsif ( $on eq "new" and not $oD ) {    # new .... ---
        return "ignore" if ( not $Dn );         # new --- ---
        return "ignore" if ( $Dn eq "ref" );    # new ref ---
        return "insert" if ( $Dn eq "new" ),    # new new ---
    } elsif ( $on eq "miss" and not $Dn ) {    # miss --- ....
        return "delete" if ( not $oD );          # miss --- ---
        return "delete" if ( $oD eq "ref" );     # miss --- ref
        return "ignore" if ( $oD eq "miss" );    # miss --- miss
    }

        # A current problem: The hanging side comment does not line up because
        # of the indentation level change introduced by the '('
        push @m, $obj
          if $obj->id =~ /$regex/i
          or (
            ( $] < 5.00303    ### provide sort of
                    ### compatibility with 5.003
                || $obj->can('name')
            ) && $obj->name =~ /$regex/i
        );
