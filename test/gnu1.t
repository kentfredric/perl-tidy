# This caused trouble at one time with -gnu and required a patch
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
}}}
