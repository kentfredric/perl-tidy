# lost alignment at one time (side comments)
my (%protonum) = (    ## protocol table
    'TCP'  => 6,      ## tcp
    'tcp'  => 6,      ## tcp
    'UDP'  => 17,     ## udp
    'udp'  => 17,     ## udp
    'ICMP' => 1,      ## icmp
    'icmp' => 1       ## icmp
);
# I dont know if this is a bug or a feature, but you might want to add it to
# unwanted '=>' alignment
$logo->Label( -image => $image, -padx => 50 )->pack( -ipadx => 25 );
$logo->Label( -text => $address )->pack( -ipadx => 25 );
$info->Label( -text => $copy )->pack( -side     => 'right', -ipadx => 25);

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
