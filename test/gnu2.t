{
    {
        {
            @vstate = ("I'm feeling pretty chatty right now.",
                       "Hope I'm not too noisy, but I like to talk.");
            {

                # This was a problem at one time
                $win->{ArcBall1} =
                  new PDL::Graphics::TriD::ArcCone(
                                                   $win->{Window},
                                                   0,
                                                   $win->{Control}{WRotation}
                                                   );
            }
        }
    }
}

# several test cases follow for undo_lp_ci:
{
    $self->error('usage: Net::Telnet->new('
                 . '[Binmode => $mode,] '
                 . '[Cmd_remove_mode => $mode,] '
                 . '[Dump_Log => $filename,] '
                 . '[Errmode => $errmode,] '
                 . '[Fhopen => $filehandle,] '
                 . '[Host => $host,] '
                 . '[Input_log => $file,] '
                 . '[Input_record_separator => $char,] '
                 . '[Option_log => $file,] '
                 . '[Output_log => $file,] '
                 . '[Output_record_separator => $char,] '
                 . '[Port => $port,] '
                 . '[Prompt => $matchop,] '
                 . '[Telnetmode => $mode,] '
                 . '[Timeout => $secs,])');
}

{{
        my ($qualifier, $value) = m{^/([^=]+)(?:=(.+))?}
          or $self->throw("Can't see new qualifier in: $_\nfrom:\n"
                          . join ('', map "$_\n", @qual));
        $self->throw("You gave an invalid server type ($servertype)"
                     . " - available types are "
                     . keys %HOSTS)
          unless ($HOSTS{$servertype});
}}

$self->command("/msg "
               . $infoline->chan
               . " You said $1, but did you know that it's square was "
               . $1 * $1 . " ?");

$transnumber
  && $self->warn("The alternative transcript number "
                 . $transnumber + 1
                 . "- does not exist. Reverting to the 1st transcript\n");

&gettext("The file debian/changelog.dch already exists --\n"
         . "please move it before trying again");

$self->throw(
           "Transcript mixes plus and minus strand. " . "This makes no sense.");

# and so is this:
$self->setError(errorString => "DBIWrapper::write - Execute failed!\n"
                . $DBI::errstr
                . "\nsql='$sql'.\nplug='@plug'.");
$self->command("/msg "
               . $infoline->chan
               . " You said $1, but did you know that it's square was "
               . $1 * $1 . " ?");

$self->command("/msg ="
               . $nicktosend
               . " Nick <\00307" . $nick
               . "\003> Chan <\00303"
               . $fserveinfo->{"chan"}
               . "\003> Trigger <\00304" . $trigger
               . "\003> last seen <"
               . localtime($fserveinfo->{"time"}) . ">");

# Without the $a, we don't get full indentation:
my_error("package $p has priority $pri in control"
           . " file but $f2pri{$f} in files list",
         $a);
