# These caused trouble at one time with -gnu and required a patch
# in sub scan_list.
{{{
            $content_size =
              &maxTextHeight(
                                           $this,
                                           $cell_content,
                                           $fontsize,
                                           &getCellAttrib(
                                                   $this, $cell_position,
                                                   'leading'
                                           )
                                           );

            $color =
              join ('/',
                           sort { $color_value{$::a} <=> $color_value{$::b}; }
                             keys %colors);
}}}

