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

# The semicolon in this stmt was once mis-tokenized as type 'f'
for (sort {strcoll($a,$b);} keys %investments) {
}

foreach my $e ( map { $_->[0] }
                    sort { $b->[3]      <=> $b->[3] ||
                           $b->[2]      <=> $a->[2] ||
                           $a->[1]->[0] cmp $b->[1]->[0] ||
                           $a->[1]->[1] cmp $b->[1]->[1] }
                        map { [ $_, $E[$_], $C[$_], $F[$_] ] }
                            0..$#E ) {
    printf "%-40s %2d/%2d\n",
           $E[$e]->[0] . "-" . $E[$e]->[1], $F[$e], $C[$e]
}

@dictionary_sorted =
    map { /^\w* (.*)/ }
       sort
          map {
               my $d = lc;          # Convert into lowercase.
               $d =~ s/[\W_]+//g;   # Remove nonalphanumerics.
               "$d $_"              # Concatenate new and original words.
              }
        @array;
