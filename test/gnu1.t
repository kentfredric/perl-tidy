{{{{
# The parens were messed up once:
                $contents .= join (
                                   (
                                    ($USING_STYLES && $image_style)
                                    ? " CLASS=\"$image_style\""
                                    : ''
                                   ),
                                   );
}}}}
# lots of side comments
$gif->arc(
          50, 50,     # Center x, y.
          30, 30,     # Width, Height.
          0,  360,    # Start Angle, End Angle.
          $red
         );     # Color.

# former closing paren problem
$self->set_displayname(
                       $nse,
                       sprintf("%s/%d-%d",
                               $seq->id(), $seq->start(), $seq->end())
                      );

die sprintf("Usage: %s [ -r | -a | -f fmt ] file ...\n", ($0 =~ m|.*/(.*)|o))
    if ($opt_h || (! @ARGV) || (($opt_a && $opt_r) || ($opt_a && $opt_f) ||
                                ($opt_r && $opt_f)));
until (
    bind(
        Server,
        sockaddr_in(
            ( $myport = int( rand( 32768 - 1024 ) ) + 1025 ), INADDR_ANY
        )
    )
  )
{
    ;
}

{ #------------- Level 1 ------------------

    # qw gets a little too much indentation here:
    ok(
        defined(
            $seqio = $gb->get_Stream_by_batch(
                [
                    qw(J00522 AF303112
                      2981014)
                ]
            )
        )
      );

    &PDL::_conv2d_int(
                      $a, $kern, $c,
                      (!(defined $opt && exists $$opt{Boundary}))
                      ? 0
                      : (($$opt{Boundary} eq "Reflect") + 2 *
                         ($$opt{Boundary} eq "Truncate"))
                     );

    # must indent to retain 2 fields here:
    %copyhash = (
              'keep',  "Keep a copy in your Inbox",
              'copy',  "Continue checking other rules after applying this rule",
              'regex', "Use regular expressions"
              );

    my @a =
      grep ($_->name eq "amnesia"
              || $_->name eq "confusion"
              || $_->name eq "charmperson"
              || $_->name eq "charmmonster"
              || $_->name eq "paralysis"
              || $_->name eq "fear",
            @{$self->holder->attribbox->attrib});

# borderline case for breaking at the '='
my %pods =
  pod_find(
           {
               -verbose => $verbose,
               -inc     => $include,
               -script  => $scripts,
               -perl    => 1
           },
           @ARGV
           );

# This illustrates a very minor glitch under -gnu.  
# Note how the block starting with '$bxfeat' seems to be off by 1,
# and should be one more character to the right to line up properly
# with the previous '{'.  This is because the characters '=> {'
# have been shifted by 1 character to align the '=>' with the previous
# lines.  This happens in the aligner, after the indentation of the
# following lines have been defined.
    my $writer =
      new XML::Writer(
                      OUTPUT      => $output,
                      NAMESPACES  => 1,
                      DATA_MODE   => 1,
                      DATA_INDENT => 4,
                      PREFIX_MAP  => {
                                     $bxfeat => 'bx-feature',
                                     $bxann  => 'bx-annotation',
                                     $bxcomp => 'bx-computation',
                                     $bxgame => 'bx-game',
                                     $bxlink => 'bx-link',
                                     $bxseq  => 'bx-seq'
                      }
                      );

# These caused trouble at one time with -gnu and required a patch
# in sub scan_list.

{ #------------- Level 2 ------------------
        my $borrowers = join(
                             ' ',
                             (
                              $borrower->{'title'},
                              $borrower->{'firstname'},
                              $borrower->{'surname'}
                             )
                            );
        if (
            (
                 $strand == 1
             and $self->mutation->prelabel >= $before
             and $self->mutation->postlabel <= $after
            )
            or (    $strand == -1
                and $self->mutation->prelabel <= $before
                and $self->mutation->postlabel >= $after)
           )
        {
        }
        $msg = $listobj->check_list(
                                    $main,          $prof,
                                    $csdb,          $user,
                                    $csdb->{"dbh"}, $par_values{"r_groups"},
                                    $user_group_tab
                                   );

        # This mixed list must be treated specially in set_leading_whitespace
        # to avoid breaking the first line
        my $ftp =
          $pkg->SUPER::new(
                         Timeout => defined $arg{Timeout} ? $arg{Timeout} : 120,
                         PeerAddr => $peer,
                         );

        # This should now break at the '='
        # Required testing nesting depth instead of indentation
        $Tk_objects{msg_window} = $Tk_objects{ftl}->Scrolled(
                                                             'Text',
                                                             -height => 5,
                                                             -width  => 30,
                                                             -bg     => 'white',
                                                             -wrap   => 'none',
                                                             -scrollbars => 'se'

        )->pack(qw/-side top -expand 0 -fill both/);
        $where_clause = join ( " and ",
            map ( $_ . " = \"" . $criteria{$_} . "\"", ( keys %criteria ) ) );

        printf STDERR (
                       "\a\n%s %0.3f.\n",
                       "** Sorry. This Perl script requires at least version",
                       $reqVersion
                       );

        if (
            defined($equal) 
            ? $equal->($a->[$ai], $b->[$bi])
            : $a->[$ai] eq $b->[$bi])
        {
            $dispf = $dispatcher->{MATCH};
            $ap--;
            $bp--;
        }

{ #------------- Level 3 ------------------
            $output_rules .= &file_contents_with_transform(
                's/\@TEXI\@/'
                  . $info_cursor . '/g; '
                  . 's/\@VTI\@/'
                  . $vti . '/g; '
                  . 's/\@VTEXI\@/'
                  . $vtexi . '/g;'
                  . 's,\@MDDIR\@,'
                  . $conf_pat . ',g;',
                'texi-vers'
            );

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
