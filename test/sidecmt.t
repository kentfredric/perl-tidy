# some side comments to line up

# simple vertical alignment of '=' and '#'
my $lines  = 0; # checksum: #lines
my $bytes  = 0; # checksum: #bytes
my $sum  = 0; # checksum: system V sum
my $patchdata = 0; # saw patch data
my $pos  = 0; # start of patch data
my $endkit = 0; # saw end of kit
my $fail  = 0; # failed

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
