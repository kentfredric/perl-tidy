    # Avoid too much space between columns, caused here by the leading
    # list element.  
    $widget->window->draw_pixmap(
      $widget->style->fg_gc('normal'), $pixmapChart,
      ${$event_area} [0],              ${$event_area} [1],
      ${$event_area} [0],              ${$event_area} [1],
      ${$event_area} [2],              ${$event_area} [3]
    );

	# Another example of potential bad vertical alignment
        # and this used to cause trouble with -gnu
        $f1->pack();
        ( $junk,                     $w[10] )        = radlablist(
          $f1,                       pad( 'Initial', $p ),
          $b->{Init}->get_panel_ref, 'None ',
          'None',                    'Default',
          'Default',                 'Simple',
          'Simple'
        );

	# and another:
        # and this used to cause trouble with -gnu
        ( $junk,                         $w[13] )                = radlablist(
          $f1,                           pad( 'Inactive Relief', $p ),
          $b->{ReliefIA}->get_panel_ref, 'sunken  ',
          'sunken',                      'flat',
          'flat',                        'raised',
          'raised'
        );
