# One of the perlstyle suggestions is to outdent a label, such as
# 'SWITCH:' here.  But perltidy does not do this because it cannot
# be implemented easily and uniformly.  For example, the label may
# already be at level zero, or the programmer may be using one tab
# per indentation level, or one single space per level.
while ($event = $parse->next_event) {
    my $type = $event->type;
  SWITCH: {
      $type eq 'start_element' && do {
	  &{($start_element_handlers->{$event->data->name}||
		$start_element_handlers->{''} || sub {})}($event->data,$event);
	  last SWITCH;
      };
      $type eq 'end_element' && do {
	  &{($end_element_handlers->{$event->data->name}||
		$end_element_handlers->{''} || sub {})}($event->data,$event);
	  last SWITCH;
      };
  }
}

# Here are some label parsing tests
my $a=0;
INIT : { 
$a++;
print "looping with label INIT:, a=$a\n";
  if ($a<10) {goto INIT}
}

# __END__ can be a statement label but you have to be tricky to use it
my $a=0;
__END__            : { 
$a++;
print "looping with label __END__:, a=$a\n";
my $b = "__END__";
if ($a<10) {goto $b}
}

# format can be a statement label if you really want..
my $a=0;
format : { 
$a++;
print "looping with label format:, a=$a\n";
if ($a<10) {goto "format"}
}
