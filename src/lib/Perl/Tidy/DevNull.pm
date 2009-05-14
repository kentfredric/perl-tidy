#####################################################################
#
# The Perl::Tidy::DevNull class supplies a dummy print method
#
#####################################################################

package Perl::Tidy::DevNull;
sub new { return bless {}, $_[0] }
sub print { return }
sub close { return }

1;
