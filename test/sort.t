	    # These are identical code
	    # the sort block is broken here, and perltidy reforms it, 
	    # but 'keys' will go on a new line
            $color = join ( '/', sort {
                  $color_value{$::a} <=> $color_value{$::b};
              } keys %colors );

	    # the sort block is on one-line, and perltidy keeps it
            $color =
              join ( '/',
              sort { $color_value{$::a} <=> $color_value{$::b} } keys %colors );

    # long sort block
    if ($sortby eq 'date' or $sortby eq 'size') {
        @files = sort {$file_data{$a}{$sortby} <=> $file_data{$b}{$sortby} or $a cmp $b} @files;
    }
    elsif ($sortby eq 'type') {
        @files = sort {$file_data{$a}{$sortby} cmp $file_data{$b}{$sortby} or $a cmp $b} @files;
    }

# from original script:
@sorted=sort {
    $SortDir*$PageTotal{$a} <=> $SortDir*$PageTotal{$b} }
        keys(%PageTotal);

# starting with all on one line:
@sorted=sort { $SortDir*$PageTotal{$a} <=> $SortDir*$PageTotal{$b} } keys(%PageTotal);

# sort with a block
print sort {$a cmp $b} @list;

# sort with a named subroutine
sub sortsub { return $a cmp $b }
print sort sortsub @list;

# sort with an anonymous subroutine
my $sortsubref = sub {return $a cmp $b;};
print sort $sortsubref @list;

# numerically has not been seen yet
  foreach $mon (reverse sort numerically (keys(%flippie))) {
}
sub numerically {$a <=> $b};
