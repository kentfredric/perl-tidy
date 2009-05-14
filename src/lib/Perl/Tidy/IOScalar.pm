#####################################################################
#
# This is a stripped down version of IO::Scalar
# Given a reference to a scalar, it supplies either:
# a getline method which reads lines (mode='r'), or
# a print method which reads lines (mode='w')
#
#####################################################################
package Perl::Tidy::IOScalar;
use Carp;

sub new {
    my ( $package, $rscalar, $mode ) = @_;
    my $ref = ref $rscalar;
    if ( $ref ne 'SCALAR' ) {
        confess <<EOM;
------------------------------------------------------------------------
expecting ref to SCALAR but got ref to ($ref); trace follows:
------------------------------------------------------------------------
EOM

    }
    if ( $mode eq 'w' ) {
        $$rscalar = "";
        return bless [ $rscalar, $mode ], $package;
    }
    elsif ( $mode eq 'r' ) {

        # Convert a scalar to an array.
        # This avoids looking for "\n" on each call to getline
        my @array = map { $_ .= "\n" } split /\n/, ${$rscalar};
        my $i_next = 0;
        return bless [ \@array, $mode, $i_next ], $package;
    }
    else {
        confess <<EOM;
------------------------------------------------------------------------
expecting mode = 'r' or 'w' but got mode ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
}

sub getline {
    my $self = shift;
    my $mode = $self->[1];
    if ( $mode ne 'r' ) {
        confess <<EOM;
------------------------------------------------------------------------
getline call requires mode = 'r' but mode = ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
    my $i = $self->[2]++;
    ##my $line = $self->[0]->[$i];
    return $self->[0]->[$i];
}

sub print {
    my $self = shift;
    my $mode = $self->[1];
    if ( $mode ne 'w' ) {
        confess <<EOM;
------------------------------------------------------------------------
print call requires mode = 'w' but mode = ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
    ${ $self->[0] } .= $_[0];
}
sub close { return }

1;
