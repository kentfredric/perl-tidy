# This was troublesome at one time
push @contents, $c->table(
  { -width => '100%' }, 
  $c->Tr(
    $c->td( 
      { -align => 'left' },
      "The emboldened field names are mandatory, ",
      "the remainder are optional",
      ),
    $c->td(
      { -align => 'right' },
      $c->a(
        {
           -href   => 'help.cgi',
          -target => '_blank'
        },
        "What are the various fields?"
      )
    )
  )
);

