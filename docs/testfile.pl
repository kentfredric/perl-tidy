#Learning Perl Appendix A, Exercise 4.2
print "What temperature is it? ";
chop($temperature = <STDIN>);
if ($temperature > 75) {
  print "Too hot!\n";
} elsif ($temperature < 68) {
  print "Too cold!\n";
} else {
  print "Just right!\n";
}
