# Some Tk code  - very tough to format well
# Note the side comments in this first sample:
#  - They force line breaks 
#  - They are at different levels and look best NOT vertically aligned
my $file = $menubar->Menubutton(qw/-text File -underline 0 -menuitems/ =>
    [
     [Cascade   => '~View', -menuitems =>
      [
       [Button  => '~widget', -command => [\&view_widget_code, __FILE__]],
       [Button  => '~WidgetDemo', -command => [\&view_widget_code, $wd]],
      ], # end cascade menuitems
     ], # end view cascade
     [Separator => ''],
     [Button    => '~Quit', -command => [\&exit]],
    ])->grid(qw/-sticky w/); 


    $VARS->Button(-text => 'OK', -command => [$VARS => 'destroy'])->
        pack(qw/-side bottom -pady 2/);


# This is challenging .. Note that we do not want to outdent the ');' here
my $tools_menu = $mframe->Menubutton(
  -menuitems => [ [ 'Button' => 'Biew', ], ] )->pack( -side => 'left' );

# outdent the "cuddled parens" here:
my $hlist = $control::control->Scrolled(
  'HList', drawbranch => 1,
  width       => 20,
  -scrollbars => 'w'
)->pack(
  -side   => 'bottom',
  -expand => 1
);
