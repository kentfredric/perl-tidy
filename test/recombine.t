# recombination logic is used to keep this as a sequence
    (/^$/) ? $filecol
      : (s/^\+//) ? $filecol + $_
      : (s/^\-//) ? $filecol - $_
      : (s/^>//)  ? ( $filecol + $_ ) % $pages
      : (s/^<//)  ? ( $filecol - $_ ) % $pages
      : (s/^<//)  ? ( $a ? ( $filecol - $_ ) % $pages : $b )
      : $d;

# perltidy will break after the '=' here
my @host_seq = $level eq "easy" ?
	    @reordered : 0..$last;  # reordered has CDROM up front

# here breaking after the '=' creates two additional lines
{{
my @host_seq = $level eq "easy" ?
	    @reordered : 0..$last;  # reordered has CDROM up front
}}   

# sub recombine keeps keywords return, last, next, redo on separate lines
# if the line is broken, so that they standout
sub Restore {
    my $w = shift;
    return if !$w->toplevel->IsMapped || !$w->slave->IsMapped
      || !$w->cget('-restore');
    $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}

			# Another example
            next if ( ( $types_to_go[$imid] eq 'k' )
              && ( $tokens_to_go[$imid] =~ /^(last|next|redo|return)$/ ) );
