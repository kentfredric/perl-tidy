# hard to decide where to break this
%PDL::bop_sbclasschk = map {my $op = $_;
     	    ($op => sub {my $foo; # print "OP: $op\n";
			 ref $_[1] && (ref $_[1] ne __PACKAGE__) 
			   && defined ($foo = overload::Method($_[1],$op)) ?
			     &$foo($_[1],$_[0],!$_[2]) :
			       ($foo = $_[0]->null(),
				PDL::_my_biop1_int(&PDL::Core::rswap,$foo,$op),
				$foo)})}
  @PDL::biops1;

