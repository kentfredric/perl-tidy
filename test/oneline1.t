# 2nd to last line should go out as a one-line block
sub kdp {
  my ($self, $n, $k => $d) = @_;
  if (defined $k) {
    $self->[$KEYS][$n] = $k;
    $self->[$DATA][$n] = $d;
  }
  [$self->[$KEYS][$n], $self->[$DATA][$n]];
}

# This if block will not qualify as a one-line block because of the
# side comment, and so it will get broken:

if ($opt_4)                     # Make 1/2 and 1/4 downsample the same
  { $args = $args . "-2 "; }
