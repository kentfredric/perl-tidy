# This is a little tricky when the 'do' starts to look
# for a one-line block and runs into the one-line if block
grep( do {
  if ( $i == $depth ) { $_++; }
  elsif ( $i > $depth ) { $_ = 0; }
  $i++;
  0;
}, @curr_sec_id );
