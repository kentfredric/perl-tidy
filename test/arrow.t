# remove spaces around arrows
my $obj = Bio::Variation::AAChange -> new;
my $termcap = Term::Cap -> Tgetent( { TERM => undef } );

    # this is long and nasty, with leading arrows
    $body = SOAP::Data
      -> name('~V:Fault')
      -> attr({'xmlns' => $SOAP::Constants::NS_ENV})
      -> value(\SOAP::Data->set_value(
        SOAP::Data->name(faultcode => qualify($self->namespace => shift(@parameters))),
        SOAP::Data->name(faultstring => shift(@parameters)),
        @parameters ? SOAP::Data->name(detail => do{my $detail = shift(@parameters); ref $detail ? \$detail : $detail}) : (),
        @parameters ? SOAP::Data->name(faultactor => shift(@parameters)) : (),
      ));
    # terminal arrow should get type 't'
    $w->Button(-text => 'OK', -command => [$w => 'destroy'])->
      pack(-side => 'bottom', -pady => 2);

# arrow-less call
$ra=[sub {print "s1\n"}, sub {print "s2\n"}, sub {print "$_[0]\n"}];
$ra->[2]("hi");

        # tests of type of anything following an ->
        # these are type 'i'
        $$buf .= ( $self->pad x ( $len - length($$buf) ) );
        if ( $self->Delayed->Priority < $self-> Priority ) {
        }
        $a if ( $ph->debug &2 );

   $_[ 0]-> Blue -> backColor(( $_[ 0]-> Blue -> backColor == cl::Blue ) ? cl::LightBlue  : cl::Blue );

package A::B;

sub import {
    my $self=shift;
    print "SELF:$self\n";
    print "ARGS:@_\n";
}

package main;
# this is type 'w'
A::B->import qw(time sleep);
