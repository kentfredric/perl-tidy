############################################################
#
#    perltidy - a perl script indenter and formatter
#
#    Copyright (c) 2000, 2001, 2002 by Steve Hancock
#    Distributed under the GPL license agreement; see file COPYING
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#    For brief instructions instructions, try 'perltidy -h'.
#    For more complete documentation, try 'man perltidy'
#    or visit http://perltidy.sourceforge.net
#
#    This script is an example of the default style.  It was formatted with: 
#
#      perltidy -isbc perltidy
#
#    Code Contributions:
#      Michael Cartmell supplied code for adaptation to VMS and helped with
#        v-strings.
#      Hugh S. Myers supplied sub streamhandle and the supporting code to
#        create a Perl::Tidy module which can operate on strings, arrays, etc.
#      Yves Orton supplied coding to help detect Windows versions.
#      Axel Rose supplied a patch for MacPerl.
#      Many others have supplied key ideas, suggestions, and bug reports;
#        see the CHANGES file.
#
############################################################

package Perl::Tidy;
use 5.004;    # need IO::File from 5.004 or later
BEGIN { $^W = 1; }    # turn on warnings

use strict;
use Exporter;
use Carp;

use vars qw{
  $VERSION
  @ISA
  @EXPORT
  $missing_file_spec
};

@ISA    = qw( Exporter );
@EXPORT = qw( &perltidy );

use IO::File;
use File::Basename;

BEGIN {
    ( $VERSION = q($Id: Tidy.pm,v 1.28 2002/09/20 17:40:28 perltidy Exp $) ) =~ s/^.*\s+(\d+)\/(\d+)\/(\d+).*$/$1$2$3/; # all one line for MakeMaker
}

# Preloaded methods go here.
sub streamhandle {

    # given filename and mode (r or w), create an object which:
    #   has a 'getline' method if mode='r', and 
    #   has a 'print' method if mode='w'.  
    # The objects also need a 'close' method.
    #
    # How the object is made:
    #
    # if $filename is:     Make object using:
    # ----------------     -----------------
    # '-'                  (STDIN if mode = 'r', STDOUT if mode='w')
    # string               IO::File
    # ARRAY  ref           IO::ScalarArray          
    # STRING ref           IO::Scalar
    # object               object 
    #                      (check for 'print' method for 'w' mode)
    #                      (check for 'getline' method for 'r' mode)
    my $ref = ref( my $filename = shift );
    my $mode = shift;
    my $New;
    my $fh;

    # handle a reference
    if ($ref) {
        if ( $ref eq 'ARRAY' ) {
            eval "use IO::ScalarArray";
            confess <<EOM if $@;
------------------------------------------------------------------------
Your call to Perl::Tidy::perltidy has an ARRAY reference, which requires
IO::ScalarArray.  Please install it and try again.  Trace follows:
------------------------------------------------------------------------

$@
EOM
            $New = sub { IO::ScalarArray->new(@_) };
        }
        elsif ( $ref eq 'SCALAR' ) {
            eval "use IO::Scalar";
            confess <<EOM if $@;
------------------------------------------------------------------------
Your call to Perl::Tidy::perltidy has an SCALAR reference, which requires
IO::Scalar.  Please install it and try again.  Trace follows:
------------------------------------------------------------------------

$@
EOM
            $New = sub { IO::Scalar->new(@_) };
        }
        else {

            # Accept an object with a getline method for reading. Note:
            # IO::File is built-in and does not respond to the defined
            # operator.  If this causes trouble, the check can be
            # skipped and we can just let it crash if there is no
            # getline.
            if ( $mode =~ /[rR]/ ) {
                if ( $ref eq 'IO::File' || defined &{ $ref . "::getline" } ) {
                    $New = sub { $filename };
                }
                else {
                    $New = sub { undef };
                    confess <<EOM;
------------------------------------------------------------------------
No 'getline' method is defined for object of class $ref
Please check your call to Perl::Tidy::perltidy.  Trace follows.
------------------------------------------------------------------------
EOM
                }
            }

            # Accept an object with a print method for writing.
            # See note above about IO::File
            if ( $mode =~ /[wW]/ ) {
                if ( $ref eq 'IO::File' || defined &{ $ref . "::print" } ) {
                    $New = sub { $filename };
                }
                else {
                    $New = sub { undef };
                    confess <<EOM;
------------------------------------------------------------------------
No 'print' method is defined for object of class $ref
Please check your call to Perl::Tidy::perltidy. Trace follows.
------------------------------------------------------------------------
EOM
                }
            }
        }
    }

    # handle a string
    else {
        if ( $filename eq '-' ) {
            $New = sub { $mode eq 'w' ? *STDOUT : *STDIN }
        }
        else {
            $New = sub { IO::File->new(@_) };
        }
    }
    $fh = $New->( $filename, $mode )
      or warn "Couldn't open file:$filename in mode:$mode : $!\n";
    return $fh, ( $ref or $filename );
}

sub catfile {

    # concatenate a path and file basename
    # returns undef in case of error

    BEGIN { eval "require File::Spec"; $missing_file_spec = $@; }

    # use File::Spec if we can
    unless ($missing_file_spec) {
        return File::Spec->catfile(@_);
    }

    # Perl 5.004 systems may not have File::Spec so we'll make 
    # a simple try.  We assume File::Basename is available.
    # return undef if not successful.
    my $name      = pop @_;
    my $path      = join '/', @_;
    my $test_file = $path . $name;
    my ( $test_name, $test_path ) = fileparse($test_file);
    return $test_file if ( $test_name eq $name );
    return undef if ( $^O eq 'VMS' );

    # this should work at least for Windows and Unix:
    $test_file = $path . '/' . $name;
    ( $test_name, $test_path ) = fileparse($test_file);
    return $test_file if ( $test_name eq $name );
    return undef;
}

# Here is a map of the flow of data from the input source to the output
# line sink:
# 
# LineSource-->Tokenizer-->Formatter-->VerticalAligner-->FileWriter-->
#       input                         groups                 output
#       lines   tokens      lines       of     lines          lines
#                                      lines
# 
# The names correspond to the package names responsible for the unit processes.
# 
# The overall process is controlled by the "main" package.
# 
# LineSource is the stream of input lines
# 
# Tokenizer analyzes a line and breaks it into tokens, peeking ahead
# if necessary.  A token is any section of the input line which should be
# manipulated as a single entity during formatting.  For example, a single
# ',' character is a token, and so is an entire side comment.  It handles
# the complexities of Perl syntax, such as distinguishing between '<<' as
# a shift operator and as a here-document, or distinguishing between '/'
# as a divide symbol and as a pattern delimiter.  
# 
# Formatter inserts and deletes whitespace between tokens, and breaks
# sequences of tokens at appropriate points as output lines.  It bases its
# decisions on the default rules as modified by any command-line options. 
# 
# VerticalAligner collects groups of lines together and tries to line up
# certain tokens, such as '=>', '#', and '=' by adding whitespace. 
# 
# FileWriter simply writes lines to the output stream.
# 
# The Logger package, not shown, records significant events and warning
# messages.  It writes a .LOG file, which may be saved with a
# '-log' or a '-g' flag.
# 
# Some comments in this file refer to separate test files, most of which
# are in the test directory which can be downloaded in addition to the
# basic perltidy distribution.

{

    # variables needed by interrupt handler:
    my $tokenizer;
    my $input_file;

    # this routine may be called to give a status report if interrupted.  If a
    # parameter is given, it will call exit with that parameter.  This is no
    # longer used because it works under Unix but not under Windows.
    sub interrupt_handler {

        my $exit_flag = shift;
        print STDERR "perltidy interrupted";
        if ($tokenizer) {
            my $input_line_number =
              Perl::Tidy::Tokenizer::get_input_line_number();
            print STDERR " at line $input_line_number";
        }
        if ($input_file) {

            if ( ref $input_file ) { print STDERR " of reference to:" }
            else { print STDERR " of file:" }
            print STDERR " $input_file";
        }
        print STDERR "\n";
        exit $exit_flag if defined($exit_flag);
    }

    sub perltidy {

        my %defaults = (
            argv        => undef,
            destination => undef,
            formatter   => undef,
            logfile     => undef,
            errorfile   => undef,
            perltidyrc  => undef,
            source      => undef,
            stderr      => undef,
        );

        my %input_hash = @_;
        if ( my @bad_keys = grep { !exists $defaults{$_} } keys %input_hash ) {
            local $" = ')(';
            my @good_keys = sort keys %defaults;
            @bad_keys = sort @bad_keys;
            confess <<EOM;
------------------------------------------------------------------------
Unknown perltidy parameter : (@bad_keys)
perltidy only understands : (@good_keys)
------------------------------------------------------------------------

EOM
        }

        %input_hash = ( %defaults, %input_hash );
        my $argv               = $input_hash{'argv'};
        my $destination_stream = $input_hash{'destination'};
        my $errorfile_stream   = $input_hash{'errorfile'};
        my $logfile_stream     = $input_hash{'logfile'};
        my $perltidyrc_stream  = $input_hash{'perltidyrc'};
        my $source_stream      = $input_hash{'source'};
        my $stderr_stream      = $input_hash{'stderr'};
        my $user_formatter     = $input_hash{'formatter'};

        # future checks on $user_formatter go here
        if ($user_formatter) {
        }

        # see if ARGV is overridden
        if ($argv) {

            my $rargv = ref $argv;
            if ( $rargv eq 'SCALAR' ) { $argv = $$argv; $rargv = undef }

            # ref to ARRAY
            if ($rargv) {
                if ( $rargv eq 'ARRAY' ) {
                    @ARGV = @$argv;
                }
                else {
                    croak <<EOM;
------------------------------------------------------------------------
Please check value of -argv in call to perltidy;
it must be a string or ref to ARRAY but is: $rargv
------------------------------------------------------------------------
EOM
                }
            }

            # string
            else {
                my ( $rargv, $msg ) = parse_args($argv);
                if ($msg) {
                    die <<EOM;
Error parsing this string passed to to perltidy with 'argv': 
$msg
EOM
                }
                @ARGV = @{$rargv};
            }
        }

        # redirect STDERR if requested
        if ($stderr_stream) {
            my ( $fh_stderr, $stderr_file ) =
              Perl::Tidy::streamhandle( $stderr_stream, 'w' );
            if ($fh_stderr) { *STDERR = $fh_stderr }
            else {
                croak <<EOM;
------------------------------------------------------------------------
Unable to redirect STDERR to $stderr_stream
Please check value of -stderr in call to perltidy
------------------------------------------------------------------------
EOM
            }
        }

        my $rpending_complaint;
        $$rpending_complaint = "";
        my $rpending_logfile_message;
        $$rpending_logfile_message = "";

        my ( $is_Windows, $Windows_type ) =
          look_for_Windows($rpending_complaint);

        # VMS file names are restricted to a 40.40 format, so we append _tdy
        # instead of .tdy, etc. (but see also sub check_vms_filename)
        my $dot;
        my $dot_pattern;
        if ( $^O eq 'VMS' ) {
            $dot         = '_';
            $dot_pattern = '_';
        }
        else {
            $dot         = '.';
            $dot_pattern = '\.';    # must escape for use in regex
        }

        # handle command line options
        my ( $rOpts, $config_file, $rraw_options, $saw_extrude ) =
          process_command_line(
            $perltidyrc_stream, $is_Windows,
            $Windows_type,      $rpending_complaint
          );

        # there must be one entry here for every possible format
        my %default_file_extension = (
            tidy => 'tdy',
            html => 'html',
            user => '',
        );

        # be sure we have a valid output format
        unless ( exists $default_file_extension{ $rOpts->{'format'} } ) {
            my $formats = join ' ',
              sort map { "'" . $_ . "'" } keys %default_file_extension;
            my $fmt = $rOpts->{'format'};
            die "-format='$fmt' but must be one of: $formats\n";
        }

        my $output_extension = $default_file_extension{ $rOpts->{'format'} };
        if ( defined( $rOpts->{'output-file-extension'} ) ) {
            $output_extension = $rOpts->{'output-file-extension'};
        }
        if ( $output_extension =~ /^[a-zA-Z0-9]/ ) {
            $output_extension = $dot . $output_extension;
        }

        my $backup_extension = 'bak';
        if ( defined( $rOpts->{'backup-file-extension'} ) ) {
            $backup_extension = $rOpts->{'backup-file-extension'};
        }
        if ( $backup_extension =~ /^[a-zA-Z0-9]/ ) {
            $backup_extension = $dot . $backup_extension;
        }

        # check for -b option; 
        my $in_place_modify = $rOpts->{'backup-and-modify-in-place'}
          && $rOpts->{'format'} eq 'tidy' # silently ignore unless beautify mode
          && @ARGV > 0;    # silently ignore if standard input;
                           # this allows -b to be in a .perltidyrc file
                           # without error messages when running from an editor

        # turn off -b with warnings in case of conflicts with other options
        if ($in_place_modify) {
            if ( $rOpts->{'standard-output'} ) {
                print STDERR
                  "Ignoring -b; you may not use -b and -st together\n";
                $in_place_modify = 0;
            }
            if ($destination_stream) {
                print STDERR
"Ignoring -b; you may not specify a destination array and -b together\n";
                $in_place_modify = 0;
            }
            if ($source_stream) {
                print STDERR
"Ignoring -b; you may not specify a source array and -b together\n";
                $in_place_modify = 0;
            }
            if ( $rOpts->{'outfile'} ) {
                print STDERR
                  "Ignoring -b; you may not use -b and -o together\n";
                $in_place_modify = 0;
            }
            if ( defined( $rOpts->{'output-path'} ) ) {
                print STDERR
                  "Ignoring -b; you may not use -b and -opath together\n";
                $in_place_modify = 0;
            }
        }

        Perl::Tidy::Formatter::check_options($rOpts);
        if ( $rOpts->{'format'} eq 'html' ) {
            Perl::Tidy::HtmlWriter->check_options($rOpts);
        }

        # make the pattern of file extensions that we shouldn't touch
        $_ = quotemeta($output_extension);
        my $forbidden_file_extensions = "(($dot_pattern)(LOG|DEBUG|ERR|TEE)|$_";
        if ($in_place_modify) {
            $_ = quotemeta($backup_extension);
            $forbidden_file_extensions .= "|$_";
        }
        $forbidden_file_extensions .= ')$';

        # Create a diagnostics object if requested;
        # This is only useful for code development
        my $diagnostics_object = undef;
        if ( $rOpts->{'DIAGNOSTICS'} ) {
            $diagnostics_object = Perl::Tidy::Diagnostics->new();
        }

        # no filenames should be given if input is from an array
        if ($source_stream) {
            if ( @ARGV > 0 ) {
                die
"You may not specify any filenames when a source array is given\n";
            }

            # we'll stuff the source array into ARGV 
            unshift ( @ARGV, $source_stream );
        }

        # use stdin by default if no source array and no args
        else {
            unshift ( @ARGV, '-' ) unless @ARGV;
        }

        # Set a flag here for any system which does not have a shell to
        # expand wildcard filenames like '*.pl'.  In theory it should also
        # be ok to set the flag for any system, but I prefer not to do so
        # out of robustness concerns.
        my $use_glob = $is_Windows;

        # loop to process all files in argument list
        my $number_of_files = @ARGV;
        my $formatter       = undef;
        $tokenizer = undef;
        while ( $input_file = shift @ARGV ) {
            my $fileroot;
            my $input_file_permissions;

            #---------------------------------------------------------------
            # determine the input file name
            #---------------------------------------------------------------
            if ($source_stream) {
                $fileroot = "perltidy";
            }
            elsif ( $input_file eq '-' ) {    # '-' indicates input from STDIN
                $fileroot = "perltidy";   # root name to use for .ERR, .LOG, etc
                $in_place_modify = 0;
            }
            else {
                $fileroot = $input_file;

                unless ( -e $input_file ) {

                    # file doesn't exist, maybe we have a wildcard
                    if ($use_glob) {

                        # be sure files exist, because glob('p.q') always
                        # returns 'p.q' even if 'p.q' doesn't exist.
                        my @files = grep { -e $_ } glob($input_file);
                        if (@files) {
                            unshift @ARGV, @files;
                            next;
                        }
                    }

                    print "skipping file: $input_file: does not exist\n";
                    next;
                }

                unless ( -f $input_file ) {
                    print "skipping: $input_file: not a regular file\n";
                    next;
                }

                unless ( ( -T $input_file ) || $rOpts->{'force-read-binary'} ) {
                    print
"skipping file: $input_file: Non-text (override with -f)\n";
                    next;
                }

                # we should have a valid filename now
                $fileroot               = $input_file;
                $input_file_permissions = ( stat $input_file )[2] & 07777;

                if ( $^O eq 'VMS' ) {
                    ( $fileroot, $dot ) = check_vms_filename($fileroot);
                }

                # add option to change path here
                if ( defined( $rOpts->{'output-path'} ) ) {

                    my ( $base, $old_path ) = fileparse($fileroot);
                    my $new_path = $rOpts->{'output-path'};
                    unless ( -d $new_path ) {
                        die <<EOM;
------------------------------------------------------------------------
The path $new_path does not exist; please check value of -opath 
------------------------------------------------------------------------
EOM
                    }
                    my $path = $new_path;
                    $fileroot = catfile( $path, $base );
                    unless ($fileroot) {
                        die <<EOM;
------------------------------------------------------------------------
Problem combining $new_path and $base to make a filename; check -opath
------------------------------------------------------------------------
EOM
                    }
                }
            }

            # Skip files with same extension as the output files because
            # this can lead to a messy situation with files like
            # script.tdy.tdy.tdy ... or worse problems ...  when you
            # rerun perltidy over and over with wildcard input.
            if (
                !$source_stream
                && (   $input_file =~ /$forbidden_file_extensions/
                    || $input_file eq 'DIAGNOSTICS' )
              )
            {
                print "skipping file: $input_file: wrong extension\n";
                next;
            }

            # the 'source_object' supplies a method to read the input file
            my $source_object =
              Perl::Tidy::LineSource->new( $input_file, $rOpts,
                $rpending_logfile_message );
            next unless ($source_object);

            # register this file name with the Diagnostics package
            $diagnostics_object->set_input_file($input_file)
              if $diagnostics_object;

            #---------------------------------------------------------------
            # determine the output file name
            #---------------------------------------------------------------
            my $output_file = undef;

            if ( $rOpts->{'outfile'} ) {

                if ( $number_of_files <= 1 ) {

                    if ( $rOpts->{'standard-output'} ) {
                        die "You may not use -o and -st together\n";
                    }
                    elsif ($destination_stream) {
                        die
"You may not specify a destination array and -o together\n";
                    }
                    $output_file = $rOpts->{outfile};

                    # make sure user gives a file name after -o
                    if ( $output_file =~ /^-/ ) {
                        die "You must specify a valid filename after -o\n";
                    }
                }
                else {
                    die "You may not use -o with more than one input file\n";
                }
            }
            elsif ( $rOpts->{'standard-output'} ) {
                if ($destination_stream) {
                    die
"You may not specify a destination array and -st together\n";
                }
                $output_file = '-';

                if ( $number_of_files <= 1 ) {
                }
                else {
                    die "You may not use -st with more than one input file\n";
                }
            }
            elsif ($destination_stream) {
                $output_file = $destination_stream;
            }
            elsif ($source_stream) {  # source but no destination goes to stdout
                $output_file = '-';
            }
            elsif ( $input_file eq '-' ) {
                $output_file = '-';
            }
            else {
                if ($in_place_modify) {
                    $output_file = IO::File->new_tmpfile()
                      or die "cannot open temp file for -b option: $!\n";
                }
                else {
                    $output_file = $fileroot . $output_extension;
                }
            }

            # the 'sink_object' knows how to write the output file
            my $tee_file    = $fileroot . $dot . "TEE";
            my $sink_object =
              Perl::Tidy::LineSink->new( $output_file, $tee_file, $rOpts,
                $rpending_logfile_message );

            #---------------------------------------------------------------
            # initialize the error logger
            #---------------------------------------------------------------
            my $warning_file = $fileroot . $dot . "ERR";
            if ($errorfile_stream) { $warning_file = $errorfile_stream }
            my $log_file = $fileroot . $dot . "LOG";
            if ($logfile_stream) { $log_file = $logfile_stream }

            my $logger_object =
              Perl::Tidy::Logger->new( $rOpts, $log_file, $warning_file,
                $saw_extrude );
            write_logfile_header(
                $rOpts,        $logger_object, $config_file,
                $rraw_options, $Windows_type
            );
            if ($$rpending_logfile_message) {
                $logger_object->write_logfile_entry($$rpending_logfile_message);
            }
            if ($$rpending_complaint) {
                $logger_object->complain($$rpending_complaint);
            }

            #---------------------------------------------------------------
            # initialize the debug object, if any
            #---------------------------------------------------------------
            my $debugger_object = undef;
            if ( $rOpts->{DEBUG} ) {
                $debugger_object =
                  Perl::Tidy::Debugger->new( $fileroot . $dot . "DEBUG" );
            }

            #---------------------------------------------------------------
            # create a formatter for this file : html writer or pretty printer
            #---------------------------------------------------------------

            # we have to delete any old formatter because, for safety, 
            # the formatter will check to see that there is only one.
            $formatter = undef;

            if ($user_formatter) {
                $formatter = $user_formatter;
            }
            elsif ( $rOpts->{'format'} eq 'html' ) {
                $formatter =
                  Perl::Tidy::HtmlWriter->new( $fileroot, $output_file );
            }
            elsif ( $rOpts->{'format'} eq 'tidy' ) {
                $formatter = Perl::Tidy::Formatter->new(
                    logger_object      => $logger_object,
                    diagnostics_object => $diagnostics_object,
                    sink_object        => $sink_object,
                );
            }
            else {
                die "I don't know how to do -format=$rOpts->{'format'}\n";
            }

            unless ($formatter) {
                die "Unable to continue with $rOpts->{'format'} formatting\n";
            }

            #---------------------------------------------------------------
            # create the tokenizer for this file
            #---------------------------------------------------------------
            $tokenizer = undef;                     # must destroy old tokenizer
            $tokenizer = Perl::Tidy::Tokenizer->new(
                source_object       => $source_object,
                logger_object       => $logger_object,
                debugger_object     => $debugger_object,
                diagnostics_object  => $diagnostics_object,
                starting_level      => $rOpts->{'starting-indentation-level'},
                tabs                => $rOpts->{'tabs'},
                indent_columns      => $rOpts->{'indent-columns'},
                look_for_hash_bang  => $rOpts->{'look-for-hash-bang'},
                look_for_autoloader => $rOpts->{'look-for-autoloader'},
                look_for_selfloader => $rOpts->{'look-for-selfloader'},
                trim_qw             => $rOpts->{'trim-qw'},
            );

            #---------------------------------------------------------------
            # now we can do it
            #---------------------------------------------------------------
            process_this_file( $tokenizer, $formatter );

            #---------------------------------------------------------------
            # close the input source and report errors
            #---------------------------------------------------------------
            $source_object->close_input_file();

            # get file names to use for syntax check
            my $ifname = $source_object->get_input_file_copy_name();
            my $ofname = $sink_object->get_output_file_copy();

            #---------------------------------------------------------------
            # handle the -b option (backup and modify in-place)
            #---------------------------------------------------------------
            if ($in_place_modify) {
                unless ( -f $input_file ) {

                    # oh, oh, no real file to backup .. 
                    # shouldn't happen because of numerous preliminary checks
                    die print
"problem with -b backing up input file '$input_file': not a file\n";
                }
                my $backup_name = $input_file . $backup_extension;
                if ( -f $backup_name ) {
                    unlink($backup_name)
                      or die
"unable to remove previous '$backup_name' for -b option; check permissions: $!\n";
                }
                rename( $input_file, $backup_name )
                  or die
"problem renaming $input_file to $backup_name for -b option: $!\n";
                $ifname = $backup_name;

                seek( $output_file, 0, 0 )
                  or die "unable to rewind tmp file for -b option: $!\n";

                my $fout = IO::File->new("> $input_file")
                  or die
"problem opening $input_file for write for -b option; check directory permissions: $!\n";
                my $line;
                while ( $line = $output_file->getline() ) {
                    $fout->print($line);
                }
                $fout->close();
                $output_file = $input_file;
                $ofname      = $input_file;
            }

            #---------------------------------------------------------------
            # clean up and report errors
            #---------------------------------------------------------------
            $sink_object->close_output_file()    if $sink_object;
            $debugger_object->close_debug_file() if $debugger_object;

            my $infile_syntax_ok = 0;    # -1 no  0=don't know   1 yes
            if ($output_file) {

                if ($input_file_permissions) {

                    # give output script same permissions as input script, but
                    # make it user-writable or else we can't run perltidy again.
                    # Thus we retain whatever executable flags were set.
                    if ( $rOpts->{'format'} eq 'tidy' ) {
                        chmod( $input_file_permissions | 0600, $output_file );
                    }

                    # else use default permissions for html and any other format

                }
                if ( $logger_object && $rOpts->{'check-syntax'} ) {
                    $infile_syntax_ok =
                      check_syntax( $ifname, $ofname, $logger_object, $rOpts );
                }
            }

            $logger_object->finish( $infile_syntax_ok, $formatter )
              if $logger_object;

        }    # end of loop to process all files

    }    # end of main program
}

sub write_logfile_header {
    my ( $rOpts, $logger_object, $config_file, $rraw_options, $Windows_type ) =
      @_;
    $logger_object->write_logfile_entry(
"perltidy version $VERSION log file on a $^O system, OLD_PERL_VERSION=$]\n"
    );
    if ($Windows_type) {
        $logger_object->write_logfile_entry("Windows type is $Windows_type\n");
    }
    my $options_string = join ( ' ', @$rraw_options );

    if ($config_file) {
        $logger_object->write_logfile_entry(
            "Found Configuration File >>> $config_file \n");
    }
    $logger_object->write_logfile_entry(
        "Configuration and command line parameters for this run:\n");
    $logger_object->write_logfile_entry("$options_string\n");

    if ( $rOpts->{'DEBUG'} || $rOpts->{'show-options'} ) {
        $rOpts->{'logfile'} = 1;    # force logfile to be saved
        $logger_object->write_logfile_entry(
            "Final parameter set for this run\n");
        $logger_object->write_logfile_entry(
            "------------------------------------\n");

        foreach ( keys %{$rOpts} ) {
            $logger_object->write_logfile_entry( '--' . "$_=$rOpts->{$_}\n" );
        }
        $logger_object->write_logfile_entry(
            "------------------------------------\n");
    }
    $logger_object->write_logfile_entry(
        "To find error messages search for 'WARNING' with your editor\n");
}

sub process_command_line {

    my ( $perltidyrc_stream, $is_Windows, $Windows_type, $rpending_complaint ) =
      @_;

    use Getopt::Long;

    ######################################################################
    # Note: a few options are not documented in the man page and usage
    # message. This is because these are experimental or debug options and
    # may or may not be retained in future versions.  
    #
    # Here are the undocumented flags as far as I know.  Any of them
    # may disappear at any time.  They are mainly for fine-tuning
    # and debugging. 
    #
    # fll --> fuzzy-line-length           # a trivial parameter which gets
    #                                       turned off for the extrude option
    #                                       which is mainly for debugging
    # chk --> check-multiline-quotes      # check for old bug; to be deleted
    # scl --> short-concatenation-item-length   # helps break at '.'
    # recombine                           # for debugging line breaks
    # I   --> DIAGNOSTICS                 # for debugging 
    ######################################################################

    # here is a summary of the Getopt codes:
    # <none> does not take an argument
    # =s takes a mandatory string
    # :s takes an optional string  (DO NOT USE - filenames will get eaten up)
    # =i takes a mandatory integer
    # :i takes an optional integer (NOT RECOMMENDED - can cause trouble)
    # ! does not take an argument and may be negated
    #  i.e., -foo and -nofoo are allowed
    # a double dash signals the end of the options list
    #
    #---------------------------------------------------------------
    # Define the option string passed to GetOptions.
    #---------------------------------------------------------------

    my @option_string = ();
    my %expansion     = ();
    my $rexpansion    = \%expansion;

    #  These options are parsed directly by perltidy:
    #    help h
    #    version v
    #  However, they are included in the option set so that they will
    #  be seen in the options dump.

    # These long option names have no abbreviations or are treated specially
    @option_string = qw(
      html!
      noprofile
      npro
      recombine!
    );

    # routine to install and check options
    my $add_option = sub {
        my ( $long_name, $short_name, $flag ) = @_;
        push @option_string, $long_name . $flag;
        if ($short_name) {
            if ( $expansion{$short_name} ) {
                my $existing_name = $expansion{$short_name}[0];
                die
"redefining abbreviation $short_name for $long_name; already used for $existing_name\n";
            }
            $expansion{$short_name} = [$long_name];
            if ( $flag eq '!' ) {
                my $nshort_name = 'n' . $short_name;
                my $nolong_name = 'no' . $long_name;
                if ( $expansion{$nshort_name} ) {
                    my $existing_name = $expansion{$nshort_name}[0];
                    die
"attempting to redefine abbreviation $nshort_name for $nolong_name; already used for $existing_name\n";
                }
                $expansion{$nshort_name} = [$nolong_name];
            }
        }
    };

    # Install long option names which have a simple abbreviation.
    # Options with code '!' get standard negation ('no' for long names,
    # 'n' for abbreviations)
    $add_option->( 'DEBUG',                                     'D',     '!' );
    $add_option->( 'DIAGNOSTICS',                               'I',     '!' );
    $add_option->( 'add-newlines',                              'anl',   '!' );
    $add_option->( 'add-semicolons',                            'asc',   '!' );
    $add_option->( 'add-whitespace',                            'aws',   '!' );
    $add_option->( 'backup-and-modify-in-place',                'b',     '!' );
    $add_option->( 'backup-file-extension',                     'bext',  '=s' );
    $add_option->( 'blanks-before-blocks',                      'bbb',   '!' );
    $add_option->( 'blanks-before-comments',                    'bbc',   '!' );
    $add_option->( 'blanks-before-subs',                        'bbs',   '!' );
    $add_option->( 'block-brace-tightness',                     'bbt',   '=i' );
    $add_option->( 'block-brace-vertical-tightness',            'bbvt',  '=i' );
    $add_option->( 'block-brace-vertical-tightness-list',       'bbvtl', '=s' );
    $add_option->( 'brace-left-and-indent',                     'bli',   '!' );
    $add_option->( 'brace-left-and-indent-list',                'blil',  '=s' );
    $add_option->( 'brace-tightness',                           'bt',    '=i' );
    $add_option->( 'brace-vertical-tightness',                  'bvt',   '=i' );
    $add_option->( 'brace-vertical-tightness-closing',          'bvtc',  '=i' );
    $add_option->( 'break-at-old-comma-breakpoints',            'boc',   '!' );
    $add_option->( 'break-at-old-keyword-breakpoints',          'bok',   '!' );
    $add_option->( 'break-at-old-logical-breakpoints',          'bol',   '!' );
    $add_option->( 'break-at-old-trinary-breakpoints',          'bot',   '!' );
    $add_option->( 'check-multiline-quotes',                    'chk',   '!' );
    $add_option->( 'check-syntax',                              'syn',   '!' );
    $add_option->( 'closing-side-comment-else-flag',            'csce',  '=i' );
    $add_option->( 'closing-side-comment-interval',             'csci',  '=i' );
    $add_option->( 'closing-side-comment-list',                 'cscl',  '=s' );
    $add_option->( 'closing-side-comment-maximum-text',         'csct',  '=i' );
    $add_option->( 'closing-side-comment-prefix',               'cscp',  '=s' );
    $add_option->( 'closing-side-comment-warnings',             'cscw',  '!' );
    $add_option->( 'closing-side-comments',                     'csc',   '!' );
    $add_option->( 'continuation-indentation',                  'ci',    '=i' );
    $add_option->( 'comma-arrow-breakpoints',                   'cab',   '=i' );
    $add_option->( 'cuddled-else',                              'ce',    '!' );
    $add_option->( 'delete-block-comments',                     'dbc',   '!' );
    $add_option->( 'delete-closing-side-comments',              'dcsc',  '!' );
    $add_option->( 'delete-old-newlines',                       'dnl',   '!' );
    $add_option->( 'delete-old-whitespace',                     'dws',   '!' );
    $add_option->( 'delete-pod',                                'dp',    '!' );
    $add_option->( 'delete-semicolons',                         'dsm',   '!' );
    $add_option->( 'delete-side-comments',                      'dsc',   '!' );
    $add_option->( 'dump-defaults',                             'ddf',   '!' );
    $add_option->( 'dump-long-names',                           'dln',   '!' );
    $add_option->( 'dump-options',                              'dop',   '!' );
    $add_option->( 'dump-profile',                              'dpro',  '!' );
    $add_option->( 'dump-short-names',                          'dsn',   '!' );
    $add_option->( 'dump-token-types',                          'dtt',   '!' );
    $add_option->( 'dump-want-left-space',                      'dwls',  '!' );
    $add_option->( 'dump-want-right-space',                     'dwrs',  '!' );
    $add_option->( 'entab-leading-whitespace',                  'et',    '=i' );
    $add_option->( 'force-read-binary',                         'f',     '!' );
    $add_option->( 'format',                                    'fmt',   '=s' );
    $add_option->( 'fuzzy-line-length',                         'fll',   '!' );
    $add_option->( 'hanging-side-comments',                     'hsc',   '!' );
    $add_option->( 'help',                                      'h',     '' );
    $add_option->( 'ignore-old-line-breaks',                    'iob',   '!' );
    $add_option->( 'indent-block-comments',                     'ibc',   '!' );
    $add_option->( 'indent-closing-brace',                      'icb',   '!' );
    $add_option->( 'indent-closing-paren',                      'icp',   '!' );
    $add_option->( 'indent-columns',                            'i',     '=i' );
    $add_option->( 'indent-spaced-block-comments',              'isbc',  '!' );
    $add_option->( 'line-up-parentheses',                       'lp',    '!' );
    $add_option->( 'logfile',                                   'log',   '!' );
    $add_option->( 'logfile-gap',                               'g',     ':i' );
    $add_option->( 'long-block-line-count',                     'lbl',   '=i' );
    $add_option->( 'look-for-autoloader',                       'lal',   '!' );
    $add_option->( 'look-for-hash-bang',                        'x',     '!' );
    $add_option->( 'look-for-selfloader',                       'lsl',   '!' );
    $add_option->( 'maximum-consecutive-blank-lines',           'mbl',   '=i' );
    $add_option->( 'maximum-fields-per-table',                  'mft',   '=i' );
    $add_option->( 'maximum-line-length',                       'l',     '=i' );
    $add_option->( 'minimum-space-to-comment',                  'msc',   '=i' );
    $add_option->( 'nowant-left-space',                         'nwls',  '=s' );
    $add_option->( 'nowant-right-space',                        'nwrs',  '=s' );
    $add_option->( 'opening-brace-always-on-right',             'bar',   '' );
    $add_option->( 'opening-brace-on-new-line',                 'bl',    '!' );
    $add_option->( 'opening-sub-brace-on-new-line',             'sbl',   '!' );
    $add_option->( 'outdent-keyword-list',                      'okwl',  '=s' );
    $add_option->( 'outdent-keywords',                          'okw',   '!' );
    $add_option->( 'outdent-labels',                            'ola',   '!' );
    $add_option->( 'outdent-long-comments',                     'olc',   '!' );
    $add_option->( 'outdent-long-quotes',                       'olq',   '!' );
    $add_option->( 'outdent-static-block-comments',             'osbc',  '!' );
    $add_option->( 'outfile',                                   'o',     '=s' );
    $add_option->( 'output-file-extension',                     'oext',  '=s' );
    $add_option->( 'output-path',                               'opath', '=s' );
    $add_option->( 'paren-tightness',                           'pt',    '=i' );
    $add_option->( 'paren-vertical-tightness',                  'pvt',   '=i' );
    $add_option->( 'paren-vertical-tightness-closing',          'pvtc',  '=i' );
    $add_option->( 'pass-version-line',                         'pvl',   '!' );
    $add_option->( 'perl-syntax-check-flags',                   'pscf',  '=s' );
    $add_option->( 'profile',                                   'pro',   '=s' );
    $add_option->( 'quiet',                                     'q',     '!' );
    $add_option->( 'short-concatenation-item-length',           'scl',   '=i' );
    $add_option->( 'show-options',                              'opt',   '!' );
    $add_option->( 'space-for-semicolon',                       'sfs',   '!' );
    $add_option->( 'space-terminal-semicolon',                  'sts',   '!' );
    $add_option->( 'square-bracket-tightness',                  'sbt',   '=i' );
    $add_option->( 'square-bracket-vertical-tightness',         'sbvt',  '=i' );
    $add_option->( 'square-bracket-vertical-tightness-closing', 'sbvtc', '=i' );
    $add_option->( 'standard-error-output',                     'se',    '!' );
    $add_option->( 'standard-output',                           'st',    '!' );
    $add_option->( 'starting-indentation-level',                'sil',   '=i' );
    $add_option->( 'static-block-comment-prefix',               'sbcp',  '=s' );
    $add_option->( 'static-block-comments',                     'sbc',   '!' );
    $add_option->( 'static-side-comment-prefix',                'sscp',  '=s' );
    $add_option->( 'static-side-comments',                      'ssc',   '!' );
    $add_option->( 'swallow-optional-blank-lines',              'sob',   '!' );
    $add_option->( 'tabs',                                      't',     '!' );
    $add_option->( 'tee-block-comments',                        'tbc',   '!' );
    $add_option->( 'tee-pod',                                   'tp',    '!' );
    $add_option->( 'tee-side-comments',                         'tsc',   '!' );
    $add_option->( 'trim-qw',                                   'tqw',   '!' );
    $add_option->( 'version',                                   'v',     '' );
    $add_option->( 'vertical-tightness',                        'vt',    '=i' );
    $add_option->( 'vertical-tightness-closing',                'vtc',   '=i' );
    $add_option->( 'want-break-after',                          'wba',   '=s' );
    $add_option->( 'want-break-before',                         'wbb',   '=s' );
    $add_option->( 'want-left-space',                           'wls',   '=s' );
    $add_option->( 'want-right-space',                          'wrs',   '=s' );
    $add_option->( 'warning-output',                            'w',     '!' );

    # The Perl::Tidy::HtmlWriter will add its own options to the string
    Perl::Tidy::HtmlWriter->make_getopt_long_names( \@option_string );

    #---------------------------------------------------------------
    # Assign default values to the above options here, except
    # for 'outfile' and 'help'.
    # These settings should approximate the perlstyle(1) suggestions.
    #---------------------------------------------------------------
    my @defaults = qw(
      add-newlines
      add-semicolons
      add-whitespace
      blanks-before-blocks
      blanks-before-comments
      blanks-before-subs
      block-brace-tightness=0
      block-brace-vertical-tightness=0
      brace-tightness=1
      brace-vertical-tightness-closing=0
      brace-vertical-tightness=0
      break-at-old-logical-breakpoints
      break-at-old-trinary-breakpoints
      break-at-old-keyword-breakpoints
      comma-arrow-breakpoints=1
      check-syntax
      closing-side-comment-interval=6
      closing-side-comment-maximum-text=20
      closing-side-comment-else-flag=0
      continuation-indentation=2
      delete-old-newlines
      delete-semicolons
      fuzzy-line-length
      hanging-side-comments
      indent-block-comments
      indent-columns=4
      long-block-line-count=8
      look-for-autoloader
      look-for-selfloader
      maximum-consecutive-blank-lines=1
      maximum-fields-per-table=0
      maximum-line-length=80
      minimum-space-to-comment=4
      nobrace-left-and-indent
      nocuddled-else
      nodelete-old-whitespace
      nohtml
      noindent-closing-brace
      noindent-closing-paren
      nologfile
      noquiet
      noshow-options
      nostatic-side-comments
      noswallow-optional-blank-lines
      notabs
      nowarning-output
      outdent-labels
      outdent-long-quotes
      paren-tightness=1
      paren-vertical-tightness-closing=0
      paren-vertical-tightness=0
      pass-version-line
      recombine
      short-concatenation-item-length=8
      space-for-semicolon
      square-bracket-tightness=1
      square-bracket-vertical-tightness-closing=0
      square-bracket-vertical-tightness=0
      static-block-comments
      trim-qw
      format=tidy
      backup-file-extension=bak
    );

    push @defaults, "perl-syntax-check-flags=-c -T";

    #---------------------------------------------------------------
    # set the defaults by passing the above list through GetOptions
    #---------------------------------------------------------------
    my %Opts = ();
    {
        local @ARGV;
        my $i;

        for $i (@defaults) { push @ARGV, "--" . $i }

        if ( !GetOptions( \%Opts, @option_string ) ) {
            die "Programming Bug: error in setting default options";
        }
    }

    #---------------------------------------------------------------
    # Define abbreviations which will be expanded into the above primitives.
    # These may be defined recursively.
    #---------------------------------------------------------------
    %expansion = (
        %expansion,
        'freeze-newlines'    => [qw(noadd-newlines nodelete-old-newlines)],
        'fnl'                => [qw(freeze-newlines)],
        'freeze-whitespace'  => [qw(noadd-whitespace nodelete-old-whitespace)],
        'fws'                => [qw(freeze-whitespace)],
        'indent-only'        => [qw(freeze-newlines freeze-whitespace)],
        'outdent-long-lines' => [qw(outdent-long-quotes outdent-long-comments)],
        'nooutdent-long-lines' =>
          [qw(nooutdent-long-quotes nooutdent-long-comments)],
        'noll'                => [qw(nooutdent-long-lines)],
        'io'                  => [qw(indent-only)],
        'delete-all-comments' =>
          [qw(delete-block-comments delete-side-comments delete-pod)],
        'nodelete-all-comments' =>
          [qw(nodelete-block-comments nodelete-side-comments nodelete-pod)],
        'dac'              => [qw(delete-all-comments)],
        'ndac'             => [qw(nodelete-all-comments)],
        'gnu'              => [qw(gnu-style)],
        'tee-all-comments' =>
          [qw(tee-block-comments tee-side-comments tee-pod)],
        'notee-all-comments' =>
          [qw(notee-block-comments notee-side-comments notee-pod)],
        'tac'   => [qw(tee-all-comments)],
        'ntac'  => [qw(notee-all-comments)],
        'html'  => [qw(format=html)],
        'nhtml' => [qw(format=tidy)],
        'tidy'  => [qw(format=tidy)],

        'break-after-comma-arrows'   => [qw(cab=0)],
        'nobreak-after-comma-arrows' => [qw(cab=1)],
        'baa'                        => [qw(cab=0)],
        'nbaa'                       => [qw(cab=1)],

        'vt=0' => [qw(pvt=0 bvt=0 sbvt=0)],
        'vt=1' => [qw(pvt=1 bvt=1 sbvt=1)],
        'vt=2' => [qw(pvt=2 bvt=2 sbvt=2)],

        'vertical-tightness=0' => [qw(pvt=0 bvt=0 sbvt=0)],
        'vertical-tightness=1' => [qw(pvt=1 bvt=1 sbvt=1)],
        'vertical-tightness=2' => [qw(pvt=2 bvt=2 sbvt=2)],

        'vtc=0' => [qw(pvtc=0 bvtc=0 sbvtc=0)],
        'vtc=1' => [qw(pvtc=1 bvtc=1 sbvtc=1)],
        'vtc=2' => [qw(pvtc=2 bvtc=2 sbvtc=2)],

        'vertical-tightness-closing=0' => [qw(pvtc=0 bvtc=0 sbvtc=0)],
        'vertical-tightness-closing=1' => [qw(pvtc=1 bvtc=1 sbvtc=1)],
        'vertical-tightness-closing=2' => [qw(pvtc=2 bvtc=2 sbvtc=2)],

        # 'mangle' originally deleted pod and comments, but to keep it
        # reversible, it no longer does.  But if you really want to
        # delete them, just use:
        #   -mangle -dac

        # An interesting use for 'mangle' is to do this:
        #    perltidy -mangle myfile.pl -st | perltidy -o myfile.pl.new
        # which will form as many one-line blocks as possible

        'mangle' => [
            qw(
              check-syntax
              delete-old-newlines
              delete-old-whitespace
              delete-semicolons
              indent-columns=0
              maximum-consecutive-blank-lines=0
              maximum-line-length=100000
              noadd-newlines
              noadd-semicolons
              noadd-whitespace
              noblanks-before-blocks
              noblanks-before-subs
              notabs
              )
        ],

        # 'extrude' originally deleted pod and comments, but to keep it
        # reversible, it no longer does.  But if you really want to
        # delete them, just use
        #   extrude -dac
        #
        # An interesting use for 'extrude' is to do this:
        #    perltidy -extrude myfile.pl -st | perltidy -o myfile.pl.new
        # which will break up all one-line blocks.

        'extrude' => [
            qw(
              check-syntax
              ci=0
              delete-old-newlines
              delete-old-whitespace
              delete-semicolons
              indent-columns=0
              maximum-consecutive-blank-lines=0
              maximum-line-length=1
              noadd-semicolons
              noadd-whitespace
              noblanks-before-blocks
              noblanks-before-subs
              nofuzzy-line-length
              notabs
              )
        ],

        # this style tries to follow the GNU Coding Standards (which do
        # not really apply to perl but which are followed by some perl
        # programmers).
        'gnu-style' => [
            qw(
              lp bl noll pt=2 bt=2 sbt=2 icp
              )
        ],

        # Additional styles can be added here
    );

    Perl::Tidy::HtmlWriter->make_abbreviated_names( \%expansion );

    # Uncomment next line to dump all expansions for debugging:
    # dump_short_names(\%expansion);

    my $word;
    my @raw_options        = ();
    my $config_file        = "";
    my $saw_ignore_profile = 0;
    my $saw_extrude        = 0;
    my $saw_dump_profile   = 0;
    my $i;

    #---------------------------------------------------------------
    # Take a first look at the command-line parameters.  Do as many
    # immediate dumps as possible, which can avoid confusion if the
    # perltidyrc file has an error.
    #---------------------------------------------------------------
    foreach $i (@ARGV) {

        if ( $i =~ /-(npro|noprofile)$/ ) {
            $saw_ignore_profile = 1;
        }

        # note: this must come before -pro and -profile, below:
        elsif ( $i =~ /-(dump-profile|dpro)$/ ) {
            $saw_dump_profile = 1;
        }
        elsif ( $i =~ /-(pro|profile)=(.+)/ ) {
            if ($config_file) {
                print STDERR
"Only one -pro=filename allowed, using '$2' instead of '$config_file'\n";
            }
            $config_file = $2;
            unless ( -e $config_file ) {
                warn "cannot find file given with -pro=$config_file: $!\n";
                $config_file = "";
            }
        }
        elsif ( $i =~ /-(pro|profile)=?$/ ) {
            print STDERR
              "usage: -pro=filename or --profile=filename, no spaces\n";
            exit 1;
        }
        elsif ( $i =~ /-extrude$/ ) {
            $saw_extrude = 1;
        }
        elsif ( $i =~ /-(help|h|HELP|H)$/ ) {
            usage();
            exit 1;
        }
        elsif ( $i =~ /-(version|v)$/ ) {
            show_version();
            exit 1;
        }
        elsif ( $i =~ /-(dump-defaults|ddf)$/ ) {
            dump_defaults(@defaults);
            exit 1;
        }
        elsif ( $i =~ /-(dump-long-names|dln)$/ ) {
            dump_long_names(@option_string);
            exit 1;
        }
        elsif ( $i =~ /-(dump-short-names|dsn)$/ ) {
            dump_short_names( \%expansion );
            exit 1;
        }
        elsif ( $i =~ /-(dump-token-types|dtt)$/ ) {
            Perl::Tidy::Tokenizer->dump_token_types(*STDOUT);
            exit 1;
        }
    }

    if ( $saw_dump_profile && $saw_ignore_profile ) {
        print STDERR "No profile to dump because of -npro\n";
        exit 1;
    }

    #---------------------------------------------------------------
    # read any .perltidyrc configuration file
    #---------------------------------------------------------------
    unless ($saw_ignore_profile) {

        # resolve possible conflict between $perltidyrc_stream passed
        # as call parameter to perltidy and -pro=filename on command
        # line.
        if ($perltidyrc_stream) {
            if ($config_file) {
                print STDERR <<EOM;
 Conflict: a perltidyrc configuration file was specified both as this
 perltidy call parameter: $perltidyrc_stream 
 and with this -profile=$config_file.
 Using -profile=$config_file.
EOM
            }
            else {
                $config_file = $perltidyrc_stream;
            }
        }

        # look for a config file if we don't have one yet
        my $rconfig_file_chatter;
        $$rconfig_file_chatter = "";
        $config_file           =
          find_config_file( $is_Windows, $Windows_type, $rconfig_file_chatter,
            $rpending_complaint )
          unless $config_file;

        # open any config file
        my $fh_config;
        if ($config_file) {
            ( $fh_config, $config_file ) =
              Perl::Tidy::streamhandle( $config_file, 'r' );
            unless ($fh_config) {
                $$rconfig_file_chatter .=
                  "# $config_file exists but cannot be opened\n";
            }
        }

        if ($saw_dump_profile) {
            if ($saw_dump_profile) {
                dump_config_file( $fh_config, $config_file,
                    $rconfig_file_chatter );
                exit 1;
            }
        }

        if ($fh_config) {

            my $rconfig_list =
              read_config_file( $fh_config, $config_file, \%expansion );

            # process any .perltidyrc parameters right now so we can
            # localize errors
            if (@$rconfig_list) {
                local @ARGV = @$rconfig_list;

                expand_command_abbreviations( \%expansion, \@raw_options,
                    $config_file );

                if ( !GetOptions( \%Opts, @option_string ) ) {
                    die
"Error in this config file: $config_file  \nUse -npro to ignore this file, -h for help'\n";
                }

                # Undo any options which cause premature exit.  They are not
                # appropriate for a config file, and it could be hard to
                # diagnose the cause of the premature exit.
                foreach (
                    qw{
                    dump-defaults
                    dump-long-names
                    dump-options
                    dump-profile
                    dump-short-names
                    dump-token-types
                    dump-want-left-space
                    dump-want-right-space
                    help
                    stylesheet
                    version
                    }
                  )
                {
                    if ( defined( $Opts{$_} ) ) {
                        delete $Opts{$_};
                        print STDERR
                          "ignoring --$_ in config file: $config_file\n";
                    }
                }
            }
        }
    }

    #---------------------------------------------------------------
    # now process the command line parameters
    #---------------------------------------------------------------
    expand_command_abbreviations( \%expansion, \@raw_options, $config_file );

    if ( !GetOptions( \%Opts, @option_string ) ) {
        die "Error on command line; for help try 'perltidy -h'\n";
    }

    if ( $Opts{'dump-options'} ) {
        dump_options( \%Opts );
        exit 1;
    }

    #---------------------------------------------------------------
    # Now we have to handle any interactions among the options..
    #---------------------------------------------------------------

    # Since -vt and -vtc are really abbreviations, we cannot allow
    # any spaces around the equal sign.  The error messages would
    # be confusing without these checks:
    if ( defined $Opts{'vertical-tightness'} ) {
        die "Please enter -vt=0, -vt=1, or -vt=2 without any spaces";
    }
    if ( defined $Opts{'vertical-tightness-closing'} ) {
        die "Please enter -vtc=0, -vtc=1, or -vtc=2 without any spaces";
    }

    # In quiet mode, there is no log file and hence no way to report
    # results of syntax check, so don't do it.
    if ( $Opts{'quiet'} ) {
        $Opts{'check-syntax'} = 0;
    }

    # can't check syntax if no output
    if ( $Opts{'format'} ne 'tidy' ) {
        $Opts{'check-syntax'} = 0;
    }

    # Never let Windows 9x/Me systems run syntax check -- this will prevent a
    # wide variety of nasty problems on these systems, because they cannot
    # reliably run backticks.  Don't even think about changing this!
    if (   $Opts{'check-syntax'}
        && $is_Windows
        && ( !$Windows_type || $Windows_type =~ /^(9|Me)/ ) )
    {
        $Opts{'check-syntax'} = 0;
    }

    # It's really a bad idea to check syntax as root unless you wrote 
    # the script yourself.  FIXME: not sure if this works with VMS
    unless ($is_Windows) {

        if ( $< == 0 && $Opts{'check-syntax'} ) {
            $Opts{'check-syntax'} = 0;
            $$rpending_complaint .=
"Syntax check deactivated for safety; you shouldn't run this as root\n";
        }
    }

    # see if user set a non-negative logfile-gap
    if ( defined( $Opts{'logfile-gap'} ) && $Opts{'logfile-gap'} >= 0 ) {

        # a zero gap will be taken as a 1
        if ( $Opts{'logfile-gap'} == 0 ) {
            $Opts{'logfile-gap'} = 1;
        }

        # setting a non-negative logfile gap causes logfile to be saved
        $Opts{'logfile'} = 1;
    }

    # not setting logfile gap, or setting it negative, causes default of 50 
    else {
        $Opts{'logfile-gap'} = 50;
    }

    # set short-cut flag when only indentation is to be done.
    # Note that the user may or may not have already set the
    # indent-only flag.
    if (   !$Opts{'add-whitespace'}
        && !$Opts{'delete-old-whitespace'}
        && !$Opts{'add-newlines'}
        && !$Opts{'delete-old-newlines'} )
    {
        $Opts{'indent-only'} = 1;
    }

    # -isbc implies -ibc
    if ( $Opts{'indent-spaced-block-comments'} ) {
        $Opts{'indent-block-comments'} = 1;
    }

    # -bli flag implies -bl
    if ( $Opts{'brace-left-and-indent'} ) {
        $Opts{'opening-brace-on-new-line'} = 1;
    }

    if (   $Opts{'opening-brace-always-on-right'}
        && $Opts{'opening-brace-on-new-line'} )
    {
        print STDERR <<EOM;
 Conflict: you specified both 'opening-brace-always-on-right' (-bar) and 
  'opening-brace-on-new-line' (-bl).  Ignoring -bl. 
EOM
        $Opts{'opening-brace-on-new-line'} = 0;
    }

    # it simplifies things if -bl is 0 rather than undefined
    if ( !defined( $Opts{'opening-brace-on-new-line'} ) ) {
        $Opts{'opening-brace-on-new-line'} = 0;
    }

    # -sbl defaults to -bl if not defined
    if ( !defined( $Opts{'opening-sub-brace-on-new-line'} ) ) {
        $Opts{'opening-sub-brace-on-new-line'} =
          $Opts{'opening-brace-on-new-line'};
    }

    # set shortcut flag if no blanks to be written
    unless ( $Opts{'maximum-consecutive-blank-lines'} ) {
        $Opts{'swallow-optional-blank-lines'} = 1;
    }

    if ( $Opts{'entab-leading-whitespace'} ) {
        if ( $Opts{'entab-leading-whitespace'} < 0 ) {
            print STDERR "-et=n must use a positive integer; ignoring -et\n";
            $Opts{'entab-leading-whitespace'} = undef;
        }

        # entab leading whitespace has priority over the older 'tabs' option
        if ( $Opts{'tabs'} ) { $Opts{'tabs'} = 0; }
    }

    return ( \%Opts, $config_file, \@raw_options, $saw_extrude );

}    # end of process_command_line

sub expand_command_abbreviations {

    # go through @ARGV and expand any abbreviations

    my ( $rexpansion, $rraw_options, $config_file ) = @_;
    my ($word);

    # set a pass limit to prevent an infinite loop;
    # 10 should be plenty, but it may be increased to allow deeply 
    # nested expansions.
    my $max_passes = 10;
    my @new_argv   = ();

    # keep looping until all expansions have been converted into actual
    # dash parameters..
    for ( my $pass_count = 0 ; $pass_count <= $max_passes ; $pass_count++ ) {
        my @new_argv     = ();
        my $abbrev_count = 0;

        # loop over each item in @ARGV..
        foreach $word (@ARGV) {

            # if it is a dash flag (instead of a file name)..
            if ( $word =~ /^-[-]?([\w\-]+)(.*)/ ) {

                my $abr   = $1;
                my $flags = $2;

                # save the raw input for debug output in case of circular refs
                if ( $pass_count == 0 ) {
                    push ( @$rraw_options, $word );
                }

                # recombine abbreviation and flag, if necessary,
                # to allow abbreviations with arguments such as '-vt=1'
                if ( $rexpansion->{ $abr . $flags } ) {
                    $abr   = $abr . $flags;
                    $flags = "";
                }

                # if we see this dash item in the expansion hash..
                if ( $rexpansion->{$abr} ) {
                    $abbrev_count++;

                    # stuff all of the words that it expands to into the
                    # new arg list for the next pass
                    foreach my $abbrev ( @{ $rexpansion->{$abr} } ) {
                        next unless $abbrev;    # for safety; shouldn't happen
                        push ( @new_argv, '--' . $abbrev . $flags );
                    }
                }

                # not in expansion hash, must be actual long name
                else {
                    push ( @new_argv, $word );
                }
            }

            # not a dash item, so just save it for the next pass
            else {
                push ( @new_argv, $word );
            }
        }    # end of this pass

        # update parameter list @ARGV to the new one
        @ARGV = @new_argv;
        last unless ( $abbrev_count > 0 );

        # make sure we are not in an infinite loop
        if ( $pass_count == $max_passes ) {
            print STDERR
"I'm tired. We seem to be in an infinite loop trying to expand aliases.\n";
            print STDERR "Here are the raw options\n";
            local $" = ')(';
            print STDERR "(@$rraw_options)\n";
            my $num = @new_argv;

            if ( $num < 50 ) {
                print STDERR "After $max_passes passes here is ARGV\n";
                print STDERR "(@new_argv)\n";
            }
            else {
                print STDERR "After $max_passes passes ARGV has $num entries\n";
            }

            if ($config_file) {
                die <<"DIE";
Please check your configuration file $config_file for circular-references. 
To deactivate it, use -npro.
DIE
            }
            else {
                die <<'DIE';
Program bug - circular-references in the %expansion hash, probably due to
a recent program change.
DIE
            }
        }    # end of check for circular references
    }    # end of loop over all passes
}

# Debug routine -- this will dump the expansion hash
sub dump_short_names {
    my $rexpansion = shift;
    print STDOUT <<EOM;
List of short names.  This list shows how all abbreviations are
translated into other abbreviations and, eventually, into long names.
New abbreviations may be defined in a .perltidyrc file.  
For a list of all long names, use perltidy --dump-long-names (-dln).
--------------------------------------------------------------------------
EOM
    foreach my $abbrev ( sort keys %$rexpansion ) {
        my @list = @{ $$rexpansion{$abbrev} };
        print STDOUT "$abbrev --> @list\n";
    }
}

sub check_vms_filename {

    # given a valid filename (the perltidy input file)
    # create a modified filename and separator character
    # suitable for VMS.
    #
    # Contributed by Michael Cartmell
    #
    my ( $base, $path ) = fileparse( $_[0] );

    # remove explicit ; version
    $base =~ s/;-?\d*$//

      # remove explicit . version ie two dots in filename NB ^ escapes a dot
      or $base =~ s/(          # begin capture $1
                  (?:^|[^^])\. # match a dot not preceded by a caret
                  (?:          # followed by nothing
                    |          # or
                    .*[^^]     # anything ending in a non caret
                  )
                )              # end capture $1
                \.-?\d*$       # match . version number
              /$1/x;

    # normalise filename, if there are no unescaped dots then append one
    $base .= '.' unless $base =~ /(?:^|[^^])\./;

    # if we don't already have an extension then we just append the extention
    my $separator = ( $base =~ /\.$/ ) ? "" : "_";
    return ( $path . $base, $separator );
}

sub Win_OS_Type {

    # Returns a string that determines what MS OS we are on.
    # Returns win32s,95,98,Me,NT3.51,NT4,2000,XP/.Net
    # Returns nothing if not an MS system.
    # Contributed by: Yves Orton

    my $rpending_complaint = shift;
    return unless $^O =~ /win32|dos/i;    # is it a MS box?

    # It _should_ have Win32 unless something is really weird
    return unless eval('require Win32');

    # Use the standard API call to determine the version
    my ( $undef, $major, $minor, $build, $id ) = Win32::GetOSVersion();

    return "win32s" unless $id;           # If id==0 then its a win32s box.
    my $os = {                            # Magic numbers from MSDN
                                          # documentation of GetOSVersion
        1 => {
            0  => "95",
            10 => "98",
            90 => "Me"
        },
        2 => {
            0  => "2000",
            1  => "XP/.Net",
            51 => "NT3.51"
        }
    }->{$id}->{$minor};

    # This _really_ shouldnt happen. At least not for quite a while
    unless ( defined $os ) {
        $$rpending_complaint .= <<EOS;
Error trying to discover Win_OS_Type: $id:$major:$minor Has no name of record!
We won't be able to look for a system-wide config file.
EOS
    }

    # Unfortunately the logic used for the various versions isnt so clever..
    # so we have to handle an outside case.
    return ( $os eq "2000" & $major != 5 ) ? "NT4" : $os;
}

sub look_for_Windows {

    # determine Windows sub-type and location of 
    # system-wide configuration files
    my $rpending_complaint = shift;
    my $is_Windows         = ( $^O =~ /win32|dos/i );
    my $Windows_type       = Win_OS_Type($rpending_complaint) if $is_Windows;
    return ( $is_Windows, $Windows_type );
}

sub find_config_file {

    # look for a .perltidyrc configuration file
    my ( $is_Windows, $Windows_type, $rconfig_file_chatter,
        $rpending_complaint ) = @_;

    $$rconfig_file_chatter .= "# Config file search...system reported as:";
    if ($is_Windows) {
        $$rconfig_file_chatter .= "Windows $Windows_type\n";
    }
    else {
        $$rconfig_file_chatter .= " $^O\n";
    }

    # sub to check file existance and record all tests
    my $exists_config_file = sub {
        my $config_file = shift;
        return 0 unless $config_file;
        $$rconfig_file_chatter .= "# Testing: $config_file\n";
        return -f $config_file;
    };

    my $config_file;

    # look in current directory first
    $config_file = ".perltidyrc";
    return $config_file if $exists_config_file->($config_file);

    # Default environment vars.
    my @envs = qw(PERLTIDY HOME);

    # Check the NT/2k/XP locations, first a local machine def, then a
    # network def
    push @envs, qw(USERPROFILE HOMESHARE) if $^O =~ /win32/i;

    # Now go through the enviornment ...
    foreach my $var (@envs) {
        $$rconfig_file_chatter .= "# Examining: \$ENV{$var}";
        if ( defined( $ENV{$var} ) ) {
            $$rconfig_file_chatter .= " = $ENV{$var}\n";

            # test ENV{ PERLTIDY } as file:
            if ( $var eq 'PERLTIDY' ) {
                $config_file = "$ENV{$var}";
                return $config_file if $exists_config_file->($config_file);
            }

            # test ENV as directory:
            $config_file = catfile( $ENV{$var}, ".perltidyrc" );
            return $config_file if $exists_config_file->($config_file);
        }
        else {
            $$rconfig_file_chatter .= "\n";
        }
    }

    # then look for a system-wide definition
    # where to look varies with OS
    if ($is_Windows) {

        if ($Windows_type) {
            my ( $os, $system, $allusers ) =
              Win_Config_Locs( $rpending_complaint, $Windows_type );

            # Check All Users directory, if there is one.
            if ($allusers) {
                $config_file = catfile( $allusers, ".perltidyrc" );
                return $config_file if $exists_config_file->($config_file);
            }

            # Check system directory.
            $config_file = catfile( $system, ".perltidyrc" );
            return $config_file if $exists_config_file->($config_file);
        }
    }

    # Place to add customization code for other systems 
    elsif ( $^O eq 'OS2' ) {
    }
    elsif ( $^O eq 'MacOS' ) {
    }
    elsif ( $^O eq 'VMS' ) {
    }

    # Assume some kind of Unix
    else {

        $config_file = "/usr/local/etc/perltidyrc";
        return $config_file if $exists_config_file->($config_file);

        $config_file = "/etc/perltidyrc";
        return $config_file if $exists_config_file->($config_file);
    }

    # Couldn't find a config file
    return;
}

sub Win_Config_Locs {

    # In scalar context returns the OS name (95 98 ME NT3.51 NT4 2000 XP),
    # or undef if its not a win32 OS.  In list context returns OS, System
    # Directory, and All Users Directory.  All Users will be empty on a
    # 9x/Me box.  Contributed by: Yves Orton.

    my $rpending_complaint = shift;
    my $os = (@_) ? shift: Win_OS_Type();
    return unless $os;

    my $system   = "";
    my $allusers = "";

    if ( $os =~ /9[58]|Me/ ) {
        $system = "C:/Windows";
    }
    elsif ( $os =~ /NT|XP|2000/ ) {
        $system = ( $os =~ /XP/ ) ? "C:/Windows/" : "C:/WinNT/";
        $allusers =
          ( $os =~ /NT/ )
          ? "C:/WinNT/profiles/All Users/"
          : "C:/Documents and Settings/All Users/";
    }
    else {

        # This currently would only happen on a win32s computer.
        # I dont have one to test So I am unsure how to proceed.
        # Sorry. :-)
        $$rpending_complaint .=
"I dont know a sensible place to look for config files on an $os system.\n";
        return;
    }
    return wantarray ? ( $os, $system, $allusers ) : $os;
}

sub dump_config_file {
    my $fh                   = shift;
    my $config_file          = shift;
    my $rconfig_file_chatter = shift;
    print STDOUT "$$rconfig_file_chatter";
    if ($fh) {
        print STDOUT "# Dump of file: '$config_file'\n";
        while ( $_ = $fh->getline() ) { print STDOUT }
        eval { $fh->close() };
    }
    else {
        print STDOUT "# ...no config file found\n";
    }
}

sub read_config_file {

    my ( $fh, $config_file, $rexpansion ) = @_;
    my @config_list = ();

    my $name = undef;
    my $line_no;
    while ( $_ = $fh->getline() ) {
        $line_no++;
        chomp;
        next if /^\s*#/;    # skip full-line comment
        $_ = strip_comment( $_, $config_file, $line_no );
        s/^\s*(.*?)\s*$/$1/;    # trim both ends
        next unless $_;

        # look for something of the general form
        #    newname { body }
        # or just
        #    body

        if ( $_ =~ /^((\w+)\s*\{)?([^}]*)(\})?$/ ) {
            my ( $newname, $body, $curly ) = ( $2, $3, $4 );

            # handle a new alias definition
            if ($newname) {
                if ($name) {
                    die
"No '}' seen after $name and before $newname in config file $config_file line $.\n";
                }
                $name = $newname;

                if ( ${$rexpansion}{$name} ) {
                    local $" = ')(';
                    my @names = sort keys %$rexpansion;
                    print "Here is a list of all installed aliases\n(@names)\n";
                    die
"Attempting to redefine alias ($name) in config file $config_file line $.\n";
                }
                ${$rexpansion}{$name} = [];
            }

            # now do the body
            if ($body) {

                my ( $rbody_parts, $msg ) = parse_args($body);
                if ($msg) {
                    die <<EOM;
Error reading file $config_file at line number $line_no.
$msg
Please fix this line or use -npro to avoid reading this file
EOM
                }

                if ($name) {

                    # remove leading dashes if this is an alias
                    foreach (@$rbody_parts) { s/^\-+//; }
                    push @{ ${$rexpansion}{$name} }, @$rbody_parts;
                }

                else {
                    push ( @config_list, @$rbody_parts );
                }
            }

            if ($curly) {
                unless ($name) {
                    die
"Unexpected '}' seen in config file $config_file line $.\n";
                }
                $name = undef;
            }
        }
    }
    eval { $fh->close() };
    return ( \@config_list );
}

sub strip_comment {

    my ( $instr, $config_file, $line_no ) = @_;

    # nothing to do if no comments
    if ( $instr !~ /#/ ) {
        return $instr;
    }

    # use simple method of no quotes
    elsif ( $instr !~ /['"]/ ) {
        $instr =~ s/\s*\#.*$//;    # simple trim
        return $instr;
    }

    # handle comments and quotes
    my $outstr     = "";
    my $quote_char = "";
    while (1) {

        # looking for ending quote character
        if ($quote_char) {
            if ( $instr =~ /\G($quote_char)/gc ) {
                $quote_char = "";
                $outstr .= $1;
            }
            elsif ( $instr =~ /\G(.)/gc ) {
                $outstr .= $1;
            }

            # error..we reached the end without seeing the ending quote char
            else {
                die <<EOM;
Error reading file $config_file at line number $line_no.
Did not see ending quote character <$quote_char> in this text:
$instr
Please fix this line or use -npro to avoid reading this file
EOM
                last;
            }
        }

        # accumulating characters and looking for start of a quoted string
        else {
            if ( $instr =~ /\G([\"\'])/gc ) {
                $outstr .= $1;
                $quote_char = $1;
            }
            elsif ( $instr =~ /\G#/gc ) {
                last;
            }
            elsif ( $instr =~ /\G(.)/gc ) {
                $outstr .= $1;
            }
            else {
                last;
            }
        }
    }
    return $outstr;
}

sub parse_args {

# Parse a command string containing multiple string with possible
# quotes, into individual commands.  It might look like this, for example:
# 
#    -wba=" + - "  -some-thing -wbb='. && ||'
# 
# There is no need, at present, to handle escaped quote characters.
# (They are not perltidy tokens, so needn't be in strings).

    my ($body) = @_;
    my @body_parts = ();
    my $quote_char = "";
    my $part       = "";
    my $msg        = "";
    while (1) {

        # looking for ending quote character
        if ($quote_char) {
            if ( $body =~ /\G($quote_char)/gc ) {
                $quote_char = "";
            }
            elsif ( $body =~ /\G(.)/gc ) {
                $part .= $1;
            }

            # error..we reached the end without seeing the ending quote char
            else {
                if ($part) { push @body_parts, $part; }
                $msg = <<EOM;
Did not see ending quote character <$quote_char> in this text:
$body
EOM
                last;
            }
        }

        # accumulating characters and looking for start of a quoted string
        else {
            if ( $body =~ /\G([\"\'])/gc ) {
                $quote_char = $1;
            }
            elsif ( $body =~ /\G(\s+)/gc ) {
                if ($part) { push @body_parts, $part; }
                $part = "";
            }
            elsif ( $body =~ /\G(.)/gc ) {
                $part .= $1;
            }
            else {
                if ($part) { push @body_parts, $part; }
                last;
            }
        }
    }
    return ( \@body_parts, $msg );
}

sub dump_long_names {

    my @names = sort @_;
    print STDOUT <<EOM;
# Command line long names (passed to GetOptions)
#---------------------------------------------------------------
# here is a summary of the Getopt codes:
# <none> does not take an argument
# =s takes a mandatory string
# :s takes an optional string
# =i takes a mandatory integer
# :i takes an optional integer
# ! does not take an argument and may be negated
#  i.e., -foo and -nofoo are allowed
# a double dash signals the end of the options list
#
#---------------------------------------------------------------
EOM

    foreach (@names) { print STDOUT "$_\n" }
}

sub dump_defaults {
    my @defaults = sort @_;
    print STDOUT "Default command line options:\n";
    foreach (@_) { print STDOUT "$_\n" }
}

sub dump_options {
    my ($rOpts) = @_;
    local $" = "\n";
    print STDOUT "Final parameter set for this run\n";
    foreach ( sort keys %{$rOpts} ) {
        print STDOUT "$_=$rOpts->{$_}\n";
    }
}

sub show_version {
    print <<"EOM";
This is perltidy, v$VERSION 

Copyright 2000-2002, Steve Hancock

Perltidy is free software and may be copied under the terms of the GNU
General Public License, which is included in the distribution files.

Complete documentation for perltidy can be found using 'man perltidy'
or on the internet at http://perltidy.sourceforge.net.
EOM
}

sub usage {

    print STDOUT <<EOF;
This is perltidy version $VERSION, a perl script indenter.  Usage:

    perltidy [ options ] file1 file2 file3 ...
            (output goes to file1.tdy, file2.tdy, file3.tdy, ...)
    perltidy [ options ] file1 -o outfile
    perltidy [ options ] file1 -st >outfile
    perltidy [ options ] <infile >outfile

Options have short and long forms. Short forms are shown; see
man pages for long forms.  Note: '=s' indicates a required string,
and '=n' indicates a required integer.

I/O control
 -h      show this help
 -o=file name of the output file (only if single input file)
 -oext=s change output extension from 'tdy' to s
 -opath=path  change path to be 'path' for output files
 -b      backup original to .bak and modify file in-place
 -bext=s change default backup extension from 'bak' to s
 -q      deactivate error messages (for running under editor)
 -w      include non-critical warning messages in the .ERR error output
 -syn    run perl -c to check syntax (default under unix systems)
 -log    save .LOG file, which has useful diagnostics
 -f      force perltidy to read a binary file
 -g      like -log but writes more detailed .LOG file, for debugging scripts
 -opt    write the set of options actually used to a .LOG file
 -npro   ignore .perltidyrc configuration command file 
 -pro=file   read configuration commands from file instead of .perltidyrc 
 -st     send output to standard output, STDOUT
 -se     send error output to standard error output, STDERR
 -v      display version number to standard output and quit

Basic Options:
 -i=n    use n columns per indentation level (default n=4)
 -t      tabs: use one tab character per indentation level, not recommeded
 -nt     no tabs: use n spaces per indentation level (default)
 -et=n   entab leading whitespace n spaces per tab; not recommended
 -io     "indent only": just do indentation, no other formatting.
 -sil=n  set starting indentation level to n;  use if auto detection fails

Whitespace Control
 -fws    freeze whitespace; this disables all whitespace changes
           and disables the following switches:
 -bt=n   sets brace tightness,  n= (0 = loose, 1=default, 2 = tight)
 -bbt    same as -bt but for code block braces; same as -bt if not given
 -bbvt   block braces vertically tight; use with -bl or -bli
 -bbvtl=s  make -bbvt to apply to selected list of block types
 -pt=n   paren tightness (n=0, 1 or 2)
 -sbt=n  square bracket tightness (n=0, 1, or 2)
 -bvt=n  brace vertical tightness, 
         n=(0=open, 1=close unless multiple steps on a line, 2=always close)
 -pvt=n  paren vertical tightness (see -bvt for n)
 -sbvt=n square bracket vertical tightness (see -bvt for n)
 -bvtc=n closing brace vertical tightness: 
         n=(0=open, 1=sometimes close, 2=always close)
 -pvtc=n closing paren vertical tightness, see -bvtc for n.
 -sbvtc=n closing square bracket vertical tightness, see -bvtc for n.
 -ci=n   sets continuation indentation=n,  default is n=2 spaces
 -lp     line up parentheses, brackets, and non-BLOCK braces
 -sfs    add space before semicolon in for( ; ; )
 -aws    allow perltidy to add whitespace (default)
 -dws    delete all old non-essential whitespace 
 -icb    indent closing brace of a code block
 -icp    indent closing paren, square-bracket, or brace of non code block
 -wls=s  want space left of tokens in string; i.e. -nwls='+ - * /'
 -wrs=s  want space right of tokens in string;
 -sts    put space before terminal semicolon of a statement

Line Break Control
 -fnl    freeze newlines; this disables all line break changes
            and disables the following switches:
 -anl    add newlines;  ok to introduce new line breaks
 -bbs    add blank line before subs and packages
 -bbc    add blank line before block comments
 -bbb    add blank line between major blocks
 -sob    swallow optional blank lines
 -ce     cuddled else; use this style: '} else {'
 -dnl    delete old newlines (default)
 -mbl=n  maximum consecutive blank lines (default=1)
 -l=n    maximum line length;  default n=80
 -bl     opening brace on new line 
 -sbl    opening sub brace on new line.  value of -bl is used if not given.
 -bli    opening brace on new line and indented
 -bar    opening brace always on right, even for long clauses
 -vt=n   vertical tightness (requires -lp); n controls break after opening
         token: 0=never  1=no break if next line balanced   2=no break
 -vtc=n  vertical tightness of closing container; n controls if closing
         token starts new line: 0=always  1=not unless list  1=never
 -wba=s  want break after tokens in string; i.e. wba=': .'
 -wbb=s  want break before tokens in string

Following Old Breakpoints
 -boc    break at old comma breaks: turns off all automatic list formatting
 -bol    break at old logical breakpoints: or, and, ||, && (default)
 -bok    break at old list keyword breakpoints such as map, sort (default)
 -bot    break at old conditional (trinary ?:) operator breakpoints (default)
 -cab=n  break at commas after a comma-arrow (=>):
         n=0 break at all commas after =>
         n=1 stable: break unless this breaks an existing one-line container
         n=2 break only if a one-line container cannot be formed
         n=3 do not treat commas after => specially at all

Comment controls
 -ibc    indent block comments (default)
 -isbc   indent spaced block comments; may indent unless no leading space
 -msc=n  minimum desired spaces to side comment, default 4
 -csc    add or update closing side comments after closing BLOCK brace
 -dcsc   delete closing side comments created by a -csc command
 -cscp=s change closing side comment prefix to be other than '## end'
 -cscl=s change closing side comment to apply to selected list of blocks
 -csci=n minimum number of lines needed to apply a -csc tag, default n=6
 -csct=n maximum number of columns of appended text, default n=20 
 -cscw   causes warning if old side comment is overwritten with -csc

 -sbc    use 'static block comments' identified by leading '##' (default)
 -sbcp=s change static block comment identifier to be other than '##'
 -osbc   outdent static block comments

 -ssc    use 'static side comments' identified by leading '##' (default)
 -sscp=s change static side comment identifier to be other than '##'

Delete selected text
 -dac    delete all comments AND pod
 -dbc    delete block comments     
 -dsc    delete side comments  
 -dp     delete pod

Send selected text to a '.TEE' file
 -tac    tee all comments AND pod
 -tbc    tee block comments       
 -tsc    tee side comments       
 -tp     tee pod           

Outdenting
 -olq    outdent long quoted strings (default) 
 -olc    outdent a long block comment line
 -ola    outdent statement labels
 -okw    outdent control keywords (redo, next, last, goto, return)
 -okwl=s specify alternative keywords for -okw command

Other controls
 -mft=n  maximum fields per table; default n=40
 -x      do not format lines before hash-bang line (i.e., for VMS)
 -asc    allows perltidy to add a ';' when missing (default)
 -dsm    allows perltidy to delete an unnecessary ';'  (default)

Combinations of other parameters
 -gnu     attempt to follow GNU Coding Standards as applied to perl
 -mangle  remove as many newlines as possible (but keep comments and pods)
 -extrude  insert as many newlines as possible

Dump and die, debugging
 -dop    dump options used in this run to standard output and quit
 -ddf    dump default options to standard output and quit
 -dsn    dump all option short names to standard output and quit
 -dln    dump option long names to standard output and quit
 -dpro   dump whatever configuration file is in effect to standard output
 -dtt    dump all token types to standard output and quit

HTML
 -html   write an html file (see 'man perl2web' for many options)
         Note: when -html is used, no indentation or formatting are done.
         Hint: try perltidy -html -css=mystyle.css filename.pl
         and edit mystyle.css to change the appearance of filename.html.
         -nnn gives line numbers
         -pre only writes out <pre>..</pre> code section

A prefix of "n" negates short form toggle switches, and a prefix of "no"
negates the long forms.  For example, -nasc means don't add missing
semicolons.  

If you are unable to see this entire text, try "perltidy -h | more"
For more detailed information, and additional options, try "man perltidy",
or go to the perltidy home page at http://perltidy.sourceforge.net
EOF

}

sub process_this_file {

    my ( $truth, $beauty ) = @_;

    # loop to process each line of this file
    while ( my $line_of_tokens = $truth->get_line() ) {
        $beauty->write_line($line_of_tokens);
    }

    # finish up
    $beauty->finish_formatting();
    $truth->report_tokenization_errors();
}

sub check_syntax {

    # Use 'perl -c' to make sure that we did not create bad syntax
    # This is a very good independent check for programming errors
    #
    # Given names of the input and output files, ($ifname, $ofname), 
    # we do the following:
    # - check syntax of the input file 
    # - if bad, all done (could be an incomplete code snippet)
    # - if infile syntax ok, then check syntax of the output file; 
    #   - if outfile syntax bad, issue warning; this implies a code bug! 
    # - set and return flag "infile_syntax_ok" : =-1 bad 0 unknown 1 good

    my ( $ifname, $ofname, $logger_object, $rOpts ) = @_;
    my $infile_syntax_ok = 0;
    my $line_of_dashes   = '-' x 42 . "\n";

    my $flags = $rOpts->{'perl-syntax-check-flags'};

    # be sure we invoke perl with -c
    # note: perl will accept repeated flags like '-c -c'.  It is safest
    # to append another -c than try to find an interior bundled c, as
    # in -Tc, because such a 'c' might be in a quoted string, for example.
    if ( $flags !~ /(^-c|\s+-c)/ ) { $flags .= " -c" }

    # be sure we invoke perl with -x if requested
    # same comments about repeated parameters applies
    if ( $rOpts->{'look-for-hash-bang'} ) {
        if ( $flags !~ /(^-x|\s+-x)/ ) { $flags .= " -x" }
    }

    # this shouldn't happen unless a termporary file couldn't be made
    if ( $ifname eq '-' ) {
        $logger_object->write_logfile_entry(
            "Cannot run perl -c on STDIN and STDOUT\n");
        return $infile_syntax_ok;
    }

    $logger_object->write_logfile_entry(
        "checking input file syntax with perl $flags\n");
    $logger_object->write_logfile_entry($line_of_dashes);

    # Not all operating systems/shells support redirection of the standard
    # error output.
    my $error_redirection = ( $^O eq 'VMS' ) ? "" : '2>&1';

    my $perl_output = do_syntax_check( $ifname, $flags, $error_redirection );
    $logger_object->write_logfile_entry("$perl_output\n");

    if ( $perl_output =~ /syntax\s*OK/ ) {
        $infile_syntax_ok = 1;
        $logger_object->write_logfile_entry($line_of_dashes);
        $logger_object->write_logfile_entry(
            "checking output file syntax with perl $flags ...\n");
        $logger_object->write_logfile_entry($line_of_dashes);

        my $perl_output =
          do_syntax_check( $ofname, $flags, $error_redirection );
        $logger_object->write_logfile_entry("$perl_output\n");

        unless ( $perl_output =~ /syntax\s*OK/ ) {
            $logger_object->write_logfile_entry($line_of_dashes);
            $logger_object->warning(
"The output file has a syntax error when tested with perl $flags $ofname !\n"
            );
            $logger_object->warning(
                "This implies an error in perltidy; the file $ofname is bad\n");
            $logger_object->report_definite_bug();

            # the perl version number will be helpful for diagnosing the problem
            $logger_object->write_logfile_entry(
                qx/perl -v $error_redirection/ . "\n" );
        }
    }
    else {

        # Only warn of perl -c syntax errors.  Other messages,
        # such as missing modules, are too common.  They can be
        # seen by running with perltidy -w
        $logger_object->complain("A syntax check using perl $flags gives: \n");
        $logger_object->complain($line_of_dashes);
        $logger_object->complain("$perl_output\n");
        $logger_object->complain($line_of_dashes);
        $infile_syntax_ok = -1;
        $logger_object->write_logfile_entry($line_of_dashes);
        $logger_object->write_logfile_entry(
"The output file will not be checked because of input file problems\n"
        );
    }
    return $infile_syntax_ok;
}

sub do_syntax_check {
    my ( $fname, $flags, $error_redirection ) = @_;

    # We have to quote the filename in case it has unusual characters
    # or spaces.  Example: this filename #CM11.pm# gives trouble.
    $fname = '"' . $fname . '"';

    # Under VMS something like -T will become -t (and an error) so we
    # will put quotes around the flags.  Double quotes seem to work on
    # Unix/Windows/VMS, but this may not work on all systems.  (Single
    # quotes do not work under Windows).  It could become necessary to
    # put double quotes around each flag, such as:  -"c"  -"T"
    # We may eventually need some system-dependent coding here.
    $flags = '"' . $flags . '"';

    # now wish for luck...
    return qx/perl $flags $fname $error_redirection/;
}

#####################################################################
#
# the Perl::Tidy::LineSource class supplies an object with a 'get_line()' method
# which returns the next line to be parsed
#
#####################################################################

package Perl::Tidy::LineSource;

sub new {

    my ( $class, $input_file, $rOpts, $rpending_logfile_message ) = @_;
    my $input_file_copy = undef;
    my $fh_copy;

    ( my $fh, $input_file ) = Perl::Tidy::streamhandle( $input_file, 'r' );
    return undef unless $fh;

    # in order to check output syntax when standard output is used, 
    # or when it is an object, we have to make a copy of the file
    if ( ( $input_file eq '-' || ref $input_file ) && $rOpts->{'check-syntax'} )
    {

        # Turning off syntax check when input output is used.
        # The reason is that temporary files cause problems on
        # on many systems.
        $rOpts->{'check-syntax'} = 0;
        $input_file_copy = '-';

        $$rpending_logfile_message .= <<EOM;
Note: --syntax check will be skipped because standard input is used
EOM

    }

    return bless {
        _fh              => $fh,
        _fh_copy         => $fh_copy,
        _filename        => $input_file,
        _input_file_copy => $input_file_copy,
    }, $class;
}

sub get_input_file_copy_name {
    my $self   = shift;
    my $ifname = $self->{_input_file_copy};
    unless ($ifname) {
        $ifname = $self->{_filename};
    }
    return $ifname;
}

sub close_input_file {
    my $self = shift;
    eval { $self->{_fh}->close() };
    eval { $self->{_fh_copy}->close() } if $self->{_fh_copy};
}

sub get_line {
    my $self    = shift;
    my $line    = undef;
    my $fh      = $self->{_fh};
    my $fh_copy = $self->{_fh_copy};
    $line = $fh->getline();
    if ( $line && $fh_copy ) { $fh_copy->print($line); }
    return $line;
}

#####################################################################
#
# the Perl::Tidy::LineSink class supplies a write_line method for 
# actual file writing
#
#####################################################################

package Perl::Tidy::LineSink;

sub new {

    my ( $class, $output_file, $tee_file, $rOpts, $rpending_logfile_message ) =
      @_;
    my $fh               = undef;
    my $fh_copy          = undef;
    my $fh_tee           = undef;
    my $output_file_copy = "";
    my $output_file_open = 0;

    if ( $rOpts->{'format'} eq 'tidy' ) {
        ( $fh, $output_file ) = Perl::Tidy::streamhandle( $output_file, 'w' );
        unless ($fh) { die "Cannot write to output stream\n"; }
        $output_file_open = 1;
    }

    # in order to check output syntax when standard output is used, 
    # or when it is an object, we have to make a copy of the file
    if ( $output_file eq '-' || ref $output_file ) {
        if ( $rOpts->{'check-syntax'} ) {

            # Turning off syntax check when standard output is used.
            # The reason is that temporary files cause problems on
            # on many systems.
            $rOpts->{'check-syntax'} = 0;
            $output_file_copy = '-';
            $$rpending_logfile_message .= <<EOM;
Note: --syntax check will be skipped because standard output is used
EOM

        }
    }

    bless {
        _fh               => $fh,
        _fh_copy          => $fh_copy,
        _fh_tee           => $fh_tee,
        _output_file      => $output_file,
        _output_file_open => $output_file_open,
        _output_file_copy => $output_file_copy,
        _tee_flag         => 0,
        _tee_file         => $tee_file,
        _tee_file_opened  => 0,
    }, $class;
}

sub write_line {

    my $self    = shift;
    my $fh      = $self->{_fh};
    my $fh_copy = $self->{_fh_copy};

    my $output_file_open = $self->{_output_file_open};

    $fh->print( $_[0] ) if ( $self->{_output_file_open} );
    print $fh_copy $_[0] if ( $fh_copy && $self->{_output_file_copy} );

    if ( $self->{_tee_flag} ) {
        unless ( $self->{_tee_file_opened} ) { $self->really_open_tee_file() }
        my $fh_tee = $self->{_fh_tee};
        print $fh_tee $_[0];
    }
}

sub get_output_file_copy {
    my $self   = shift;
    my $ofname = $self->{_output_file_copy};
    unless ($ofname) {
        $ofname = $self->{_output_file};
    }
    return $ofname;
}

sub tee_on {
    my $self = shift;
    $self->{_tee_flag} = 1;
}

sub tee_off {
    my $self = shift;
    $self->{_tee_flag} = 0;
}

sub really_open_tee_file {
    my $self     = shift;
    my $tee_file = $self->{_tee_file};
    my $fh_tee;
    $fh_tee = IO::File->new(">$tee_file")
      or die ("couldn't open TEE file $tee_file: $!\n");
    $self->{_tee_file_opened} = 1;
    $self->{_fh_tee}          = $fh_tee;
}

sub close_output_file {
    my $self = shift;
    eval { $self->{_fh}->close() }      if $self->{_output_file_open};
    eval { $self->{_fh_copy}->close() } if ( $self->{_output_file_copy} );
    $self->close_tee_file();
}

sub close_tee_file {
    my $self = shift;

    if ( $self->{_tee_file_opened} ) {
        eval { $self->{_fh_tee}->close() };
        $self->{_tee_file_opened} = 0;
    }
}

#####################################################################
#
# The Perl::Tidy::Diagnostics class writes the DIAGNOSTICS file, which is 
# useful for program development.
#
# Only one such file is created regardless of the number of input
# files processed.  This allows the results of processing many files 
# to be summarized in a single file.
#
#####################################################################

package Perl::Tidy::Diagnostics;

sub new {

    my $class = shift;
    bless {
        _write_diagnostics_count => 0,
        _last_diagnostic_file    => "",
        _input_file              => "",
        _fh                      => undef,
    }, $class;
}

sub set_input_file {
    my $self = shift;
    $self->{_input_file} = $_[0];
}

# This is a diagnostic routine which is useful for program development.
# Output from debug messages go to a file named DIAGNOSTICS, where
# they are labeled by file and line.  This allows many files to be
# scanned at once for some particular condition of interest.
sub write_diagnostics {
    my $self = shift;

    unless ( $self->{_write_diagnostics_count} ) {
        open DIAGNOSTICS, ">DIAGNOSTICS"
          or death("couldn't open DIAGNOSTICS: $!\n");
    }

    my $last_diagnostic_file = $self->{_last_diagnostic_file};
    my $input_file           = $self->{_input_file};
    if ( $last_diagnostic_file ne $input_file ) {
        print DIAGNOSTICS "\nFILE:$input_file\n";
    }
    $self->{_last_diagnostic_file} = $input_file;
    my $input_line_number = Perl::Tidy::Tokenizer::get_input_line_number();
    print DIAGNOSTICS "$input_line_number:\t@_";
    $self->{_write_diagnostics_count}++;
}

#####################################################################
#
# The Perl::Tidy::Logger class writes the .LOG and .ERR files
#
#####################################################################

package Perl::Tidy::Logger;

sub new {
    my $class = shift;
    my $fh;
    my ( $rOpts, $log_file, $warning_file, $saw_extrude ) = @_;

    # remove any old error output file
    unless ( ref($warning_file) ) {
        if ( -e $warning_file ) { unlink($warning_file) }
    }

    bless {
        _log_file                      => $log_file,
        _fh_warnings                   => undef,
        _rOpts                         => $rOpts,
        _fh_warnings                   => undef,
        _last_input_line_written       => 0,
        _at_end_of_file                => 0,
        _use_prefix                    => 1,
        _block_log_output              => 0,
        _line_of_tokens                => undef,
        _output_line_number            => undef,
        _wrote_line_information_string => 0,
        _wrote_column_headings         => 0,
        _warning_file                  => $warning_file,
        _warning_count                 => 0,
        _complaint_count               => 0,
        _saw_code_bug    => -1,             # -1=no 0=maybe 1=for sure
        _saw_brace_error => 0,
        _saw_extrude     => $saw_extrude,
        _output_array    => [],
    }, $class;
}

sub close_log_file {

    my $self = shift;
    if ( $self->{_fh_warnings} ) {
        eval { $self->{_fh_warnings}->close() };
        $self->{_fh_warnings} = undef;
    }
}

sub get_warning_count {
    my $self = shift;
    return $self->{_warning_count};
}

sub get_use_prefix {
    my $self = shift;
    return $self->{_use_prefix};
}

sub block_log_output {
    my $self = shift;
    $self->{_block_log_output} = 1;
}

sub unblock_log_output {
    my $self = shift;
    $self->{_block_log_output} = 0;
}

sub interrupt_logfile {
    my $self = shift;
    $self->{_use_prefix} = 0;
    $self->warning("\n");
    $self->write_logfile_entry( '#' x 24 . "  WARNING  " . '#' x 25 . "\n" );
}

sub resume_logfile {
    my $self = shift;
    $self->write_logfile_entry( '#' x 60 . "\n" );
    $self->{_use_prefix} = 1;
}

sub we_are_at_the_last_line {
    my $self = shift;
    unless ( $self->{_wrote_line_information_string} ) {
        $self->write_logfile_entry("Last line\n\n");
    }
    $self->{_at_end_of_file} = 1;
}

# record some stuff in case we go down in flames
sub black_box {
    my $self = shift;
    my ( $line_of_tokens, $output_line_number ) = @_;
    my $input_line        = $line_of_tokens->{_line_text};
    my $input_line_number = $line_of_tokens->{_line_number};

    # save line information in case we have to write a logfile message
    $self->{_line_of_tokens}                = $line_of_tokens;
    $self->{_output_line_number}            = $output_line_number;
    $self->{_wrote_line_information_string} = 0;

    my $last_input_line_written = $self->{_last_input_line_written};
    my $rOpts                   = $self->{_rOpts};
    if (
        (
            ( $input_line_number - $last_input_line_written ) >=
            $rOpts->{'logfile-gap'}
        )
        || ( $input_line =~ /^\s*(sub|package)\s+(\w+)/ )
      )
    {
        my $rlevels                      = $line_of_tokens->{_rlevels};
        my $structural_indentation_level = $$rlevels[0];
        $self->{_last_input_line_written} = $input_line_number;
        ( my $out_str = $input_line ) =~ s/^\s*//;
        chomp $out_str;

        $out_str = ( '.' x $structural_indentation_level ) . $out_str;

        if ( length($out_str) > 35 ) {
            $out_str = substr( $out_str, 0, 35 ) . " ....";
        }
        $self->logfile_output( "", "$out_str\n" );
    }
}

sub write_logfile_entry {
    my $self = shift;

    # add leading >>> to avoid confusing error mesages and code
    $self->logfile_output( ">>>", "@_" );
}

sub write_column_headings {
    my $self = shift;

    $self->{_wrote_column_headings} = 1;
    my $routput_array = $self->{_output_array};
    push @{$routput_array}, <<EOM;
The nesting depths in the table below are at the start of the lines.
The indicated output line numbers are not always exact.
ci = levels of continuation indentation; bk = 1 if in BLOCK, 0 if not.

in:out indent c b  nesting   code + messages; (messages begin with >>>)
lines  levels i k            (code begins with one '.' per indent level)
------  ----- - - --------   -------------------------------------------
EOM
}

sub make_line_information_string {

    # make columns of information when a logfile message needs to go out
    my $self                    = shift;
    my $line_of_tokens          = $self->{_line_of_tokens};
    my $input_line_number       = $line_of_tokens->{_line_number};
    my $line_information_string = "";
    if ($input_line_number) {

        my $output_line_number       = $self->{_output_line_number};
        my $brace_depth              = $line_of_tokens->{_curly_brace_depth};
        my $paren_depth              = $line_of_tokens->{_paren_depth};
        my $square_bracket_depth     = $line_of_tokens->{_square_bracket_depth};
        my $python_indentation_level =
          $line_of_tokens->{_python_indentation_level};
        my $rlevels         = $line_of_tokens->{_rlevels};
        my $rnesting_tokens = $line_of_tokens->{_rnesting_tokens};
        my $rci_levels      = $line_of_tokens->{_rci_levels};
        my $rnesting_blocks = $line_of_tokens->{_rnesting_blocks};

        my $structural_indentation_level = $$rlevels[0];

        $self->write_column_headings() unless $self->{_wrote_column_headings};

        # keep logfile columns aligned for scripts up to 999 lines;
        # for longer scripts it doesn't really matter
        my $extra_space = "";
        $extra_space .= ( $input_line_number < 10 ) ? "  "
          : ( $input_line_number < 100 ) ? " "
          : "";
        $extra_space .= ( $output_line_number < 10 ) ? "  "
          : ( $output_line_number < 100 ) ? " "
          : "";

        # there are 2 possible nesting strings:
        # the original which looks like this:  (0 [1 {2
        # the new one, which looks like this:  {{[
        # the new one is easier to read, and shows the order, but 
        # could be arbitrarily long, so we use it unless it is too long
        my $nesting_string =
          "($paren_depth [$square_bracket_depth {$brace_depth";
        my $nesting_string_new = $$rnesting_tokens[0];

        my $ci_level = $$rci_levels[0];
        if ( $ci_level > 9 ) { $ci_level = '*' }
        my $bk = ( $$rnesting_blocks[0] =~ /1$/ ) ? '1' : '0';

        if ( length($nesting_string_new) <= 8 ) {
            $nesting_string =
              $nesting_string_new . " " x ( 8 - length($nesting_string_new) );
        }
        if ( $python_indentation_level < 0 ) { $python_indentation_level = 0 }
        $line_information_string =
"L$input_line_number:$output_line_number$extra_space i$python_indentation_level:$structural_indentation_level $ci_level $bk $nesting_string";
    }
    return $line_information_string;
}

sub logfile_output {
    my $self = shift;
    my ( $prompt, $msg ) = @_;
    return if ( $self->{_block_log_output} );

    my $routput_array = $self->{_output_array};
    if ( $self->{_at_end_of_file} || !$self->{_use_prefix} ) {
        push @{$routput_array}, "$msg";
    }
    else {
        my $line_information_string = $self->make_line_information_string();
        $self->{_wrote_line_information_string} = 1;

        if ($line_information_string) {
            push @{$routput_array}, "$line_information_string   $prompt$msg";
        }
        else {
            push @{$routput_array}, "$msg";
        }
    }
}

sub get_saw_brace_error {
    my $self = shift;
    return $self->{_saw_brace_error};
}

sub increment_brace_error {
    my $self = shift;
    $self->{_saw_brace_error}++;
}

sub brace_warning {
    my $self = shift;
    use constant BRACE_WARNING_LIMIT => 10;
    my $saw_brace_error = $self->{_saw_brace_error};

    if ( $saw_brace_error < BRACE_WARNING_LIMIT ) {
        $self->warning(@_);
    }
    $saw_brace_error++;
    $self->{_saw_brace_error} = $saw_brace_error;

    if ( $saw_brace_error == BRACE_WARNING_LIMIT ) {
        $self->warning("No further warnings of this type will be given\n");
    }
}

sub complain {

    # handle non-critical warning messages based on input flag
    my $self  = shift;
    my $rOpts = $self->{_rOpts};

    # these appear in .ERR output only if -w flag is used
    if ( $rOpts->{'warning-output'} ) {
        $self->warning(@_);
    }

    # otherwise, they go to the .LOG file
    else {
        $self->{_complaint_count}++;
        $self->write_logfile_entry(@_);
    }
}

sub warning {

    # report errors to .ERR file (or stdout)
    my $self = shift;
    use constant WARNING_LIMIT => 50;

    my $rOpts = $self->{_rOpts};
    unless ( $rOpts->{'quiet'} ) {

        my $warning_count = $self->{_warning_count};
        unless ($warning_count) {
            my $warning_file = $self->{_warning_file};
            my $fh_warnings;
            if ( $rOpts->{'standard-error-output'} ) {
                $fh_warnings = *STDERR;
            }
            else {
                ( $fh_warnings, my $filename ) =
                  Perl::Tidy::streamhandle( $warning_file, 'w' );
                $fh_warnings or die ("couldn't open $filename $!\n");
                print STDERR "## Please see file $filename\n";
            }
            $self->{_fh_warnings} = $fh_warnings;
        }

        my $fh_warnings = $self->{_fh_warnings};
        if ( $warning_count < WARNING_LIMIT ) {
            if ( $self->get_use_prefix() > 0 ) {
                my $input_line_number =
                  Perl::Tidy::Tokenizer::get_input_line_number();
                print $fh_warnings "$input_line_number:\t@_";
                $self->write_logfile_entry("WARNING: @_");
            }
            else {
                print $fh_warnings @_;
                $self->write_logfile_entry(@_);
            }
        }
        $warning_count++;
        $self->{_warning_count} = $warning_count;

        if ( $warning_count == WARNING_LIMIT ) {
            print $fh_warnings "No further warnings will be given";
        }
    }
}

# programming bug codes:
#   -1 = no bug
#    0 = maybe, not sure.
#    1 = definitely
sub report_possible_bug {
    my $self         = shift;
    my $saw_code_bug = $self->{_saw_code_bug};
    $self->{_saw_code_bug} = ( $saw_code_bug < 0 ) ? 0 : $saw_code_bug;
}

sub report_definite_bug {
    my $self = shift;
    $self->{_saw_code_bug} = 1;
}

sub ask_user_for_bug_report {
    my $self = shift;

    my ( $infile_syntax_ok, $formatter ) = @_;
    my $saw_code_bug = $self->{_saw_code_bug};
    if ( ( $saw_code_bug == 0 ) && ( $infile_syntax_ok == 1 ) ) {
        $self->warning(<<EOM);

You may have encountered a code bug in perltidy.  If you think so, and
the problem is not listed in the BUGS file at
http://perltidy.sourceforge.net, please report it so that it can be
corrected.  Include the smallest possible script which has the problem,
along with the .LOG file. See the manual pages for contact information.
Thank you!
EOM

    }
    elsif ( $saw_code_bug == 1 ) {
        if ( $self->{_saw_extrude} ) {
            $self->warning(<<EOM);
You may have encountered a bug in perltidy.  However, since you are
using the -extrude option, the problem may be with perl itself, which
has occasional parsing problems with this type of file.  If you believe
that the problem is with perltidy, and the problem is not listed in the
BUGS file at http://perltidy.sourceforge.net, please report it so that
it can be corrected.  Include the smallest possible script which has the
problem, along with the .LOG file. See the manual pages for contact
information.
Thank you!
EOM
        }
        else {
            $self->warning(<<EOM);

Oops, you seem to have encountered a bug in perltidy.  Please check the
BUGS file at http://perltidy.sourceforge.net.  If the problem is not
listed there, please report it so that it can be corrected.  Include the
smallest possible script which produces this message, along with the
.LOG file if appropriate.  See the manual pages for contact information.
Your efforts are appreciated.  
Thank you!
EOM
            my $added_semicolon_count = $formatter->get_added_semicolon_count();
            if ( $added_semicolon_count > 0 ) {
                $self->warning(<<EOM);

The log file shows that perltidy added $added_semicolon_count semicolons.
Please rerun with -nasc to see if that is the cause of the syntax error.  Even
if that is the problem, please report it so that it can be fixed.
EOM

            }
        }
    }
}

sub finish {

    # called after all formatting to summarize errors
    my $self = shift;
    my ( $infile_syntax_ok, $formatter ) = @_;

    my $rOpts         = $self->{_rOpts};
    my $warning_count = $self->{_warning_count};
    my $saw_code_bug  = $self->{_saw_code_bug};

    my $save_logfile = ( $saw_code_bug == 0 && $infile_syntax_ok == 1 )
      || $saw_code_bug == 1
      || $rOpts->{'logfile'};
    my $log_file = $self->{_log_file};
    if ($warning_count) {
        if ($save_logfile) {
            $self->block_log_output();    # avoid echoing this to the logfile
            $self->warning(
                "The logfile $log_file may contain useful information\n");
            $self->unblock_log_output();
        }

        if ( $self->{_complaint_count} > 0 ) {
            $self->warning(
"To see $self->{_complaint_count} non-critical warnings rerun with -w\n"
            );
        }

        if ( $self->{_saw_brace_error}
            && ( $rOpts->{'logfile-gap'} > 1 || !$save_logfile ) )
        {
            $self->warning("To save a full .LOG file rerun with -g\n");
        }
    }
    $self->ask_user_for_bug_report( $infile_syntax_ok, $formatter );

    if ($save_logfile) {
        my $log_file = $self->{_log_file};
        my ( $fh, $filename ) = Perl::Tidy::streamhandle( $log_file, 'w' );
        if ($fh) {
            my $routput_array = $self->{_output_array};
            foreach ( @{$routput_array} ) { $fh->print($_) }
            eval { $fh->close() };
        }
    }
}

#####################################################################
#
# The Perl::Tidy::DevNull class supplies a dummy print method
#
#####################################################################

package Perl::Tidy::DevNull;
sub new { return bless {}, $_[0] }
sub print { return }
sub close { return }

#####################################################################
#
# The Perl::Tidy::HtmlWriter class writes a copy of the input stream in html
#
#####################################################################

package Perl::Tidy::HtmlWriter;

# class variables
use vars qw{
  %html_color
  %html_bold
  %html_italic
  %token_short_names
  %short_to_long_names
  $rOpts
  $css_filename
  $css_linkname
  $missing_html_entities
};

# replace unsafe characters with HTML entity representation if HTML::Entities 
# is available
{ eval "use HTML::Entities"; $missing_html_entities = $@; }

sub new {

    my ( $class, $input_file, $html_file ) = @_;

    my $html_file_opened = 0;
    my $html_fh;
    ( $html_fh, my $html_filename ) =
      Perl::Tidy::streamhandle( $html_file, 'w' );
    unless ($html_fh) {
        warn("can't open $html_file: $!\n");
        return undef;
    }
    $html_file_opened = 1;

    if ( !$input_file || $input_file eq '-' || ref($input_file) ) {
        $input_file = "NONAME";
    }

    my $html_pre_fh;
    my $html_toc_fh;
    if ( $rOpts->{'html-pre-only'} ) {

        # the table of contents goes to /dev/null
        $html_toc_fh = Perl::Tidy::DevNull->new();

        # pre section goes to output stream
        $html_pre_fh = $html_fh;
    }
    else {

        # the table of contents goes to output stream
        $html_toc_fh = $html_fh;

        # The <pre> section go out to a temporary file.  
        # sub close() will copy it to the output stream.
        $html_pre_fh = IO::File->new_tmpfile()
          or die "cannot open temp file for -html: $!\n";

        # Start sending the the full html page to the output stream
        my $title = escape_html($input_file);
        $html_fh->print( <<"HTML_START");
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" 
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>$title</title>
HTML_START

        # use css linked to another file
        if ( $rOpts->{'html-linked-style-sheet'} ) {
            $html_fh->print(
qq(<link rel="stylesheet" href="$css_linkname" type="text/css" />)
            );
            $html_fh->print( <<"ENDCSS");
</head>
<body>
ENDCSS
        }

        # use css embedded in this file
        elsif ( !$rOpts->{'nohtml-style-sheets'} ) {
            $html_fh->print( <<'ENDCSS');
<style type="text/css">
<!--
ENDCSS
            write_style_sheet_data($html_fh);
            $html_fh->print( <<"ENDCSS");
-->
</style>
</head>
<body>
ENDCSS
        }

        # no css used
        else {

            $html_fh->print( <<"HTML_START");
</head>
<body bgcolor=\"$rOpts->{'html-color-background'}\" text=\"$rOpts->{'html-color-punctuation'}\">
HTML_START
        }

        $html_fh->print( <<"EOM");
<h1>$title</h1>
EOM

    }

    # ----------------------------------------------------------
    # Output is now directed as follows:
    # html_toc_fh <-- table of contents items
    # html_pre_fh <-- the <pre> section of formatted code
    # ----------------------------------------------------------

    my $fname_comment = $input_file;
    $fname_comment =~ s/--+/-/g;    # protect HTML comment tags

    $html_pre_fh->print( <<"END_PRE");
<hr />
<!-- contents of filename: $fname_comment -->
<pre>
END_PRE

    my $toc_item_count = 0;
    my $in_toc_package = "";
    my $last_level     = 0;
    bless {
        _html_file        => $html_file,           # name of file
        _html_file_opened => $html_file_opened,    # a flag
        _html_fh          => $html_fh,             # the output stream
        _html_pre_fh      => $html_pre_fh,         # pre section goes here
        _html_toc_fh      => $html_toc_fh,         # table of contents goes here
        _rtoc_item_count  => \$toc_item_count,     # how many toc items
        _rin_toc_package  => \$in_toc_package,     # package name
        _rtoc_name_count  => {},                   # hash to track unique names
        _rpackage_stack   => [],                   # stack to check for package
                                                   # name changes
        _rlast_level      => \$last_level,         # brace indentation level
    }, $class;
}

sub add_toc_item {

    # Add an item to the html table of contents.
    # This is called even if no table of contents is written,
    # because we still want to put the anchors in the <pre> text.
    # We are given an anchor name and its type;
    # possible types are:
    #      'package', 'sub', '__END__', '__DATA__', 'EOF'
    # There must be an 'EOF' call at the end to wrap things up.
    my $self = shift;
    my ( $name, $type ) = @_;
    my $html_toc_fh     = $self->{_html_toc_fh};
    my $html_pre_fh     = $self->{_html_pre_fh};
    my $rtoc_name_count = $self->{_rtoc_name_count};
    my $rtoc_item_count = $self->{_rtoc_item_count};
    my $rlast_level     = $self->{_rlast_level};
    my $rin_toc_package = $self->{_rin_toc_package};
    my $rpackage_stack  = $self->{_rpackage_stack};

    # packages contain sublists of subs, so to avoid errors all package
    # items are written and finished with the following routines
    my $end_package_list = sub {
        if ($$rin_toc_package) {
            $html_toc_fh->print("</ul>\n</li>\n");
            $$rin_toc_package = "";
        }
    };

    my $start_package_list = sub {
        my ( $unique_name, $package ) = @_;
        if ($$rin_toc_package) { $end_package_list->() }
        $html_toc_fh->print(<<EOM);
<li><a href=\"#$unique_name\">package $package</a>
<ul>
EOM
        $$rin_toc_package = $package;
    };

    # start the table of contents on the first item
    unless ($$rtoc_item_count) {

        # but just quit if we hit EOF without any other entries
        # in this case, there will be no toc
        return if ( $type eq 'EOF' );
        $html_toc_fh->print( <<"TOC_END");
<!-- BEGIN INDEX --><a name="-index-"></a>
<ul>
TOC_END
    }
    $$rtoc_item_count++;

    # make a unique anchor name for this location
    my $unique_name = $name;
    if ( $type eq 'package' ) { $unique_name = "package-$name" }

    # append '-1', '-2', etc if necessary to make unique; this will
    # be unique because subs and packages cannot have a '-'
    if ( my $count = $rtoc_name_count->{$unique_name}++ ) {
        $unique_name .= "-$count";
    }

    # start/stop lists of subs
    if ( $type eq 'sub' ) {
        my $package = $rpackage_stack->[$$rlast_level];
        unless ($package) { $package = 'main' }

        # if we're already in a package/sub list, be sure its the right
        # package or else close it
        if ( $$rin_toc_package && $$rin_toc_package ne $package ) {
            $end_package_list->();
        }

        # start a package/sub list if necessary
        unless ($$rin_toc_package) {
            $start_package_list->( $unique_name, $package );
        }
    }

    # now write an entry in the toc for this item
    if ( $type eq 'package' ) {
        $start_package_list->( $unique_name, $name );
    }
    elsif ( $type eq 'sub' ) {
        $html_toc_fh->print("<li><a href=\"#$unique_name\">$name</a></li>\n");
    }
    else {
        $end_package_list->();
        $html_toc_fh->print("<li><a href=\"#$unique_name\">$name</a></li>\n");
    }

    # write the anchor in the <pre> section
    $html_pre_fh->print("<a name=\"$unique_name\"></a>");

    # end the table of contents, if any, on the end of file
    if ( $type eq 'EOF' ) {
        $html_toc_fh->print( <<"TOC_END");
</ul>
<!-- END INDEX -->
TOC_END
    }
}

BEGIN {

    # This is the official list of tokens which may be identified by the
    # user.  Long names are used as getopt keys.  Short names are
    # convenient short abbreviations for specifying input.  Short names
    # somewhat resemble token type characters, but are often different
    # because they may only be alphanumeric, to allow command line
    # input.  Also, note that because of case insensitivity of html,
    # this table must be in a single case only (I've chosen to use all
    # lower case).
    # When adding NEW_TOKENS: update this hash table
    # short names => long names
    %short_to_long_names = (
        'n'  => 'numeric',
        'p'  => 'paren',
        'q'  => 'quote',
        's'  => 'structure',
        'c'  => 'comment',
        'v'  => 'v-string',
        'cm' => 'comma',
        'w'  => 'bareword',
        'co' => 'colon',
        'pu' => 'punctuation',
        'i'  => 'identifier',
        'j'  => 'label',
        'h'  => 'here-doc-target',
        'hh' => 'here-doc-text',
        'k'  => 'keyword',
        'sc' => 'semicolon',
        'm'  => 'subroutine',
        'pd' => 'pod-text',
    );

    # Now we have to map actual token types into one of the above short
    # names; any token types not mapped will get 'punctuation'
    # properties.  

    # The values of this hash table correspond to the keys of the
    # previous hash table.
    # The keys of this hash table are token types and can be seen
    # by running with --dump-token-types (-dtt). 

    # When adding NEW_TOKENS: update this hash table
    # $type => $short_name
    %token_short_names = (
        '#'  => 'c',
        'n'  => 'n',
        'v'  => 'v',
        'k'  => 'k',
        'F'  => 'k',
        'Q'  => 'q',
        'q'  => 'q',
        'J'  => 'j',
        'j'  => 'j',
        'h'  => 'h',
        'H'  => 'hh',
        'w'  => 'w',
        ','  => 'cm',
        '=>' => 'cm',
        ';'  => 'sc',
        ':'  => 'co',
        'f'  => 'sc',
        '('  => 'p',
        ')'  => 'p',
        'M'  => 'm',
        'P'  => 'pd',
    );

    # These token types will all be called identifiers for now
    # FIXME: need to separate user defined modules as separate type
    my @identifier = qw" i t U C Y Z G :: ";
    @token_short_names{@identifier} = ('i') x scalar(@identifier);

    # These token types will be called 'structure'
    my @structure = qw" { } ";
    @token_short_names{@structure} = ('s') x scalar(@structure);

    # OLD NOTES: save for reference
    # Any of these could be added later if it would be useful.
    # For now, they will by default become punctuation
    #    my @list = qw" L R [ ] ";
    #    @token_long_names{@list} = ('non-structure') x scalar(@list);
    #
    #    my @list = qw"
    #      / /= * *= ** **= + += - -= % %= = ++ -- << <<= >> >>= pp p m mm
    #      ";
    #    @token_long_names{@list} = ('math') x scalar(@list);
    #
    #    my @list = qw" & &= ~ ~= ^ ^= | |= ";
    #    @token_long_names{@list} = ('bit') x scalar(@list);
    #
    #    my @list = qw" == != < > <= <=> ";
    #    @token_long_names{@list} = ('numerical-comparison') x scalar(@list);
    #
    #    my @list = qw" && || ! &&= ||= ";
    #    @token_long_names{@list} = ('logical') x scalar(@list);
    #
    #    my @list = qw" . .= =~ !~ x x= ";
    #    @token_long_names{@list} = ('string-operators') x scalar(@list);
    #
    #    # Incomplete..
    #    my @list = qw" .. -> <> ... \ ? ";
    #    @token_long_names{@list} = ('misc-operators') x scalar(@list);

}

sub make_getopt_long_names {
    my $class = shift;
    my ($rgetopt_names) = @_;
    while ( my ( $short_name, $name ) = each %short_to_long_names ) {
        push @$rgetopt_names, "html-color-$name=s";
        push @$rgetopt_names, "html-italic-$name!";
        push @$rgetopt_names, "html-bold-$name!";
    }
    push @$rgetopt_names, "html-color-background=s";
    push @$rgetopt_names, "html-linked-style-sheet=s";
    push @$rgetopt_names, "nohtml-style-sheets";
    push @$rgetopt_names, "html-pre-only";
    push @$rgetopt_names, "html-line-numbers";
    push @$rgetopt_names, "stylesheet";
}

sub make_abbreviated_names {

    # We're appending things like this to the expansion list:
    #      'hcc'    => [qw(html-color-comment)],
    #      'hck'    => [qw(html-color-keyword)],
    #  etc
    my $class = shift;
    my ($rexpansion) = @_;

    # abbreviations for color/bold/italic properties
    while ( my ( $short_name, $long_name ) = each %short_to_long_names ) {
        ${$rexpansion}{"hc$short_name"}  = ["html-color-$long_name"];
        ${$rexpansion}{"hb$short_name"}  = ["html-bold-$long_name"];
        ${$rexpansion}{"hi$short_name"}  = ["html-italic-$long_name"];
        ${$rexpansion}{"nhb$short_name"} = ["nohtml-bold-$long_name"];
        ${$rexpansion}{"nhi$short_name"} = ["nohtml-italic-$long_name"];
    }

    # abbreviations for all other html options
    ${$rexpansion}{"hcbg"} = ["html-color-background"];
    ${$rexpansion}{"pre"}  = ["html-pre-only"];
    ${$rexpansion}{"nnn"}  = ["html-line-numbers"];
    ${$rexpansion}{"css"}  = ["html-linked-style-sheet"];
    ${$rexpansion}{"nss"}  = ["nohtml-style-sheets"];
    ${$rexpansion}{"ss"}   = ["stylesheet"];
}

sub check_options {

    # This will be called once after options have been parsed
    my $class = shift;
    $rOpts = shift;

    # X11 color names for default settings that seemed to look ok
    # (these color names are only used for programming clarity; the hex 
    # numbers are actually written)
    use constant ForestGreen   => "#228B22";
    use constant SaddleBrown   => "#8B4513";
    use constant IndianRed3    => "#CD5555";
    use constant DeepSkyBlue4  => "#00688B";
    use constant MediumOrchid3 => "#B452CD";
    use constant black         => "#000000";
    use constant white         => "#FFFFFF";
    use constant red           => "#FF0000";

    # set default color, bold, italic properties
    # anything not listed here will be given the default (punctuation) color --
    # these types currently not listed and get default: ws pu s sc cm co p
    # When adding NEW_TOKENS: add an entry here if you don't want defaults

    # set_default_properties( $short_name, default_color, bold?, italic? );
    set_default_properties( 'c',  ForestGreen,   0, 0 );
    set_default_properties( 'pd', ForestGreen,   0, 1 );
    set_default_properties( 'k',  SaddleBrown,   1, 0 );
    set_default_properties( 'q',  IndianRed3,    0, 0 );
    set_default_properties( 'hh', IndianRed3,    0, 1 );
    set_default_properties( 'h',  IndianRed3,    1, 0 );
    set_default_properties( 'i',  DeepSkyBlue4,  0, 0 );
    set_default_properties( 'w',  black,         0, 0 );
    set_default_properties( 'n',  MediumOrchid3, 0, 0 );
    set_default_properties( 'v',  MediumOrchid3, 0, 0 );
    set_default_properties( 'j',  black,         1, 0 );
    set_default_properties( 'm',  red,           1, 0 );

    set_default_color( 'html-color-background',  white );
    set_default_color( 'html-color-punctuation', black );

    # setup property lookup tables for tokens based on their short names
    # every token type has a short name, and will use these tables
    # to do the html markup
    while ( my ( $short_name, $long_name ) = each %short_to_long_names ) {
        $html_color{$short_name}  = $rOpts->{"html-color-$long_name"};
        $html_bold{$short_name}   = $rOpts->{"html-bold-$long_name"};
        $html_italic{$short_name} = $rOpts->{"html-italic-$long_name"};
    }

    # write style sheet to STDOUT and die if requested
    if ( defined( $rOpts->{'stylesheet'} ) ) {
        write_style_sheet_file('-');
        exit;
    }

    # make sure user gives a file name after -css
    if ( defined( $rOpts->{'html-linked-style-sheet'} ) ) {
        $css_linkname = $rOpts->{'html-linked-style-sheet'};
        if ( $css_linkname =~ /^-/ ) {
            die "You must specify a valid filename after -css\n";
        }
    }

    # check for conflict
    if ( $css_linkname && $rOpts->{'nohtml-style-sheets'} ) {
        $rOpts->{'nohtml-style-sheets'} = 0;
        warning("You can't specify both -css and -nss; -nss ignored\n");
    }

    # write a style sheet file if necessary
    if ($css_linkname) {

        # if the selected filename exists, don't write, because user may
        # have done some work by hand to create it; use backup name instead
        # Also, this will avoid a potential disaster in which the user
        # forgets to specify the style sheet, like this:
        #    perltidy -html -css myfile1.pl myfile2.pl
        # This would cause myfile1.pl to parsed as the style sheet by GetOpts

        my $css_filename = $css_linkname;
        if ( -e $css_filename ) {
        }
        else {

            write_style_sheet_file($css_filename);
        }
    }
}

sub write_style_sheet_file {

    my $css_filename = shift;
    my $fh;
    unless ( $fh = IO::File->new("> $css_filename") ) {
        die "can't open $css_filename: $!\n";
    }
    write_style_sheet_data($fh);
    eval { $fh->close };
}

sub write_style_sheet_data {

    # write the style sheet data to an open file handle
    my $fh = shift;

    my $bg_color   = $rOpts->{'html-color-background'};
    my $text_color = $rOpts->{'html-color-punctuation'};

    $fh->print(<<"EOM");
/* default style sheet generated by perltidy */
body {background: $bg_color; color: $text_color}
pre { color: $text_color; 
      background: $bg_color;
      font-family: courier;
    } 

EOM

    foreach my $short_name ( sort keys %short_to_long_names ) {
        my $long_name = $short_to_long_names{$short_name};

        my $abbrev = '.' . $short_name;
        if ( length($short_name) == 1 ) { $abbrev .= ' ' }    # for alignment
        my $color = $html_color{$short_name};
        if ( !defined($color) ) { $color = $text_color }
        $fh->print("$abbrev \{ color: $color;");

        if ( $html_bold{$short_name} ) {
            $fh->print(" font-weight:bold;");
        }

        if ( $html_italic{$short_name} ) {
            $fh->print(" font-style:italic;");
        }
        $fh->print("} /* $long_name */\n");
    }
}

sub set_default_color {

    # make sure that options hash $rOpts->{$key} contains a valid color
    my ( $key, $color ) = @_;
    if ( $rOpts->{$key} ) { $color = $rOpts->{$key} }
    $rOpts->{$key} = check_RGB($color);
}

sub check_RGB {

    # if color is a 6 digit hex RGB value, prepend a #, otherwise
    # assume that it is a valid ascii color name
    my ($color) = @_;
    if ( $color =~ /^[0-9a-fA-F]{6,6}$/ ) { $color = "#$color" }
    return $color;
}

sub set_default_properties {
    my ( $short_name, $color, $bold, $italic ) = @_;

    set_default_color( "html-color-$short_to_long_names{$short_name}", $color );
    my $key;
    $key = "html-bold-$short_to_long_names{$short_name}";
    $rOpts->{$key} = ( defined $rOpts->{$key} ) ? $rOpts->{$key} : $bold;
    $key = "html-italic-$short_to_long_names{$short_name}";
    $rOpts->{$key} = ( defined $rOpts->{$key} ) ? $rOpts->{$key} : $italic;
}

sub close_html_file {
    my $self = shift;
    return unless $self->{_html_file_opened};

    my $html_fh     = $self->{_html_fh};
    my $html_pre_fh = $self->{_html_pre_fh};

    # If we are writing an index, finish it and append
    # the <pre> section which is on a temp file
    if ( $html_pre_fh != $html_fh ) {
        $self->add_toc_item( 'EOF', 'EOF' );
        seek( $html_pre_fh, 0, 0 )
          or die "unable to rewind tmp file for -html option: $!\n";
        my $line;
        while ( $line = $html_pre_fh->getline() ) {
            $html_fh->print($line);
        }
        $html_pre_fh->close();
    }

    # finish the html page
    $html_fh->print( <<"PRE_END");
</pre>
PRE_END
    unless ( $rOpts->{'html-pre-only'} ) {
        $html_fh->print( <<"HTML_END");
</body>
</html>
HTML_END
    }
    eval { $html_fh->close() };
}

sub markup_tokens {
    my $self = shift;
    my ( $rtokens, $rtoken_type, $rlevels ) = @_;
    my ( @colored_tokens, $j, $string, $type, $token, $level );
    my $rlast_level     = $self->{_rlast_level};
    my $rin_toc_package = $self->{_rlast_level};
    my $rpackage_stack  = $self->{_rpackage_stack};

    for ( $j = 0 ; $j < @$rtoken_type ; $j++ ) {
        $type  = $$rtoken_type[$j];
        $token = $$rtokens[$j];
        $level = $$rlevels[$j];
        $level = 0 if ( $level < 0 );

        #-------------------------------------------------------
        # Update the package stack.  The package stack is needed to keep
        # the toc correct because some packages may be declared within
        # blocks and go out of scope when we leave the block.  
        if ( $level > $$rlast_level ) {
            unless ( $rpackage_stack->[ $level - 1 ] ) {
                $rpackage_stack->[ $level - 1 ] = 'main';
            }
            $rpackage_stack->[$level] = $rpackage_stack->[ $level - 1 ];
        }
        elsif ( $level < $$rlast_level ) {
            my $package = $rpackage_stack->[$level];
            unless ($package) { $package = 'main' }

            # if we change packages due to a nesting change, we
            # have to make an entry in the toc
            if ( $package ne $rpackage_stack->[ $level + 1 ] ) {
                $self->add_toc_item( $package, 'package' );
            }
        }
        $$rlast_level = $level;

        #-------------------------------------------------------
        # Intercept a sub name here; split it
        # into keyword 'sub' and sub name; and add an
        # entry in the toc
        if ( $type eq 'i' && $token =~ /^(sub\s+)(\w.*)$/ ) {
            $token = $self->markup_html_element( $1, 'k' );
            push @colored_tokens, $token;
            $token = $2;
            $type  = 'M';
            my $subname = $token;
            $subname =~ s/\s.*$//;    # remove any attributes and prototype
            $self->add_toc_item( $subname, 'sub' );
        }

        # Intercept a package name here; split it
        # into keyword 'package' and name; add to the toc,
        # and update the package stack
        if ( $type eq 'i' && $token =~ /^(package\s+)(\w.*)$/ ) {
            $token = $self->markup_html_element( $1, 'k' );
            push @colored_tokens, $token;
            $token = $2;
            $type  = 'i';
            $self->add_toc_item( "$token", 'package' );
            $rpackage_stack->[$level] = $token;
        }

        #-------------------------------------------------------

        $token = $self->markup_html_element( $token, $type );
        push @colored_tokens, $token;
    }
    return ( \@colored_tokens );
}

sub markup_html_element {
    my $self = shift;
    my ( $token, $type ) = @_;

    return $token if ( $type eq 'b' );    # skip a blank
    return $token if ( $token =~ /^\s*$/ );
    $token = escape_html($token);

    # get the short abbreviation for this token type
    my $short_name = $token_short_names{$type};
    if ( !defined($short_name) ) {
        $short_name = "pu";               # punctuation is default
    }

    # handle style sheets..
    if ( !$rOpts->{'nohtml-style-sheets'} ) {
        if ( $short_name ne 'pu' ) {
            $token = qq(<span class="$short_name">) . $token . "</span>";
        }
    }

    # handle no style sheets..
    else {
        my $color = $html_color{$short_name};

        if ( $color && ( $color ne $rOpts->{'html-color-punctuation'} ) ) {
            $token = qq(<font color="$color">) . $token . "</font>";
        }
        if ( $html_italic{$short_name} ) { $token = "<i>$token</i>" }
        if ( $html_bold{$short_name} )   { $token = "<b>$token</b>" }
    }
    return $token;
}

sub escape_html {

    my $token = shift;
    if ($missing_html_entities) {
        $token =~ s/\&/&amp;/g;
        $token =~ s/\</&lt;/g;
        $token =~ s/\>/&gt;/g;
        $token =~ s/\"/&quot;/g;
    }
    else {
        HTML::Entities::encode_entities($token);
    }
    return $token;
}

sub finish_formatting {

    # called after last line
    my $self = shift;
    $self->close_html_file();
    return;
}

sub write_line {

    my $self = shift;
    return unless $self->{_html_file_opened};
    my $html_pre_fh = $self->{_html_pre_fh};
    my ($line_of_tokens) = @_;
    my $line_type   = $line_of_tokens->{_line_type};
    my $input_line  = $line_of_tokens->{_line_text};
    my $line_number = $line_of_tokens->{_line_number};
    chomp $input_line;

    # markup line of code..
    my $html_line;
    if ( $line_type eq 'CODE' ) {
        my $rtoken_type = $line_of_tokens->{_rtoken_type};
        my $rtokens     = $line_of_tokens->{_rtokens};
        my $rlevels     = $line_of_tokens->{_rlevels};

        if ( $input_line =~ /(^\s*)/ ) {
            $html_line = $1;
        }
        else {
            $html_line = "";
        }
        my ($rcolored_tokens) =
          $self->markup_tokens( $rtokens, $rtoken_type, $rlevels );
        $html_line .= join '', @$rcolored_tokens;
    }

    # markup line of non-code..
    else {
        my $line_character;
        if    ( $line_type eq 'HERE' )       { $line_character = 'H' }
        elsif ( $line_type eq 'HERE_END' )   { $line_character = 'h' }
        elsif ( $line_type eq 'FORMAT' )     { $line_character = 'H' }
        elsif ( $line_type eq 'FORMAT_END' ) { $line_character = 'h' }
        elsif ( $line_type eq 'SYSTEM' )     { $line_character = 'c' }
        elsif ( $line_type eq 'END_START' )  {
            $line_character = 'k';
            $self->add_toc_item( '__END__', '__END__' );
        }
        elsif ( $line_type eq 'DATA_START' ) {
            $line_character = 'k';
            $self->add_toc_item( '__DATA__', '__DATA__' );
        }
        elsif ( $line_type =~ /^POD/ ) { $line_character = 'P' }
        else { $line_character = 'Q' }
        $html_line = $self->markup_html_element( $input_line, $line_character );
    }

    # add the line number if requested
    if ( $rOpts->{'html-line-numbers'} ) {
        my $extra_space .= ( $line_number < 10 ) ? "   "
          : ( $line_number < 100 )  ? "  "
          : ( $line_number < 1000 ) ? " "
          : "";
        $html_line = $extra_space . $line_number . " " . $html_line;
    }

    # write the line
    $html_pre_fh->print("$html_line\n");
}

#####################################################################
#
# The Perl::Tidy::Formatter package adds indentation, whitespace, and line breaks
# to the token stream
#
# WARNING: This is not a real class yet.  Only one Formatter my be used.
#
#####################################################################

package Perl::Tidy::Formatter;

BEGIN {

    # Caution: these debug flags produce a lot of output
    # They should all be 0 except when debugging small scripts
    use constant FORMATTER_DEBUG_FLAG_BOND    => 0;
    use constant FORMATTER_DEBUG_FLAG_BREAK   => 0;
    use constant FORMATTER_DEBUG_FLAG_CI      => 0;
    use constant FORMATTER_DEBUG_FLAG_FLUSH   => 0;
    use constant FORMATTER_DEBUG_FLAG_FORCE   => 0;
    use constant FORMATTER_DEBUG_FLAG_LIST    => 0;
    use constant FORMATTER_DEBUG_FLAG_NOBREAK => 0;
    use constant FORMATTER_DEBUG_FLAG_OUTPUT  => 0;
    use constant FORMATTER_DEBUG_FLAG_SPARSE  => 0;
    use constant FORMATTER_DEBUG_FLAG_STORE   => 0;
    use constant FORMATTER_DEBUG_FLAG_UNDOBP  => 0;
    use constant FORMATTER_DEBUG_FLAG_WHITE   => 0;

    my $debug_warning = sub {
        print "FORMATTER_DEBUGGING with key $_[0]\n";
    };

    FORMATTER_DEBUG_FLAG_BOND    && $debug_warning->('BOND');
    FORMATTER_DEBUG_FLAG_BREAK   && $debug_warning->('BREAK');
    FORMATTER_DEBUG_FLAG_CI      && $debug_warning->('CI');
    FORMATTER_DEBUG_FLAG_FLUSH   && $debug_warning->('FLUSH');
    FORMATTER_DEBUG_FLAG_FORCE   && $debug_warning->('FORCE');
    FORMATTER_DEBUG_FLAG_LIST    && $debug_warning->('LIST');
    FORMATTER_DEBUG_FLAG_NOBREAK && $debug_warning->('NOBREAK');
    FORMATTER_DEBUG_FLAG_OUTPUT  && $debug_warning->('OUTPUT');
    FORMATTER_DEBUG_FLAG_SPARSE  && $debug_warning->('SPARSE');
    FORMATTER_DEBUG_FLAG_STORE   && $debug_warning->('STORE');
    FORMATTER_DEBUG_FLAG_UNDOBP  && $debug_warning->('UNDOBP');
    FORMATTER_DEBUG_FLAG_WHITE   && $debug_warning->('WHITE');
}

use Carp;
use vars qw{

  @gnu_stack
  $max_gnu_stack_index
  $gnu_position_predictor
  $line_start_index_to_go
  $last_indentation_written
  $last_unadjusted_indentation

  $saw_VERSION_in_this_file

  @gnu_item_list
  $max_gnu_item_index
  $gnu_sequence_number
  $last_output_indentation
  %last_gnu_equals
  %gnu_comma_count
  %gnu_arrow_count

  @block_type_to_go
  @type_sequence_to_go
  @container_environment_to_go
  @bond_strength_to_go
  @forced_breakpoint_to_go
  @lengths_to_go
  @levels_to_go
  @leading_spaces_to_go
  @reduced_spaces_to_go
  @matching_token_to_go
  @mate_index_to_go
  @nesting_blocks_to_go
  @ci_levels_to_go
  @nesting_depth_to_go
  @nobreak_to_go
  @old_breakpoint_to_go
  @tokens_to_go
  @types_to_go

  %saved_opening_indentation

  $max_index_to_go
  $comma_count_in_batch
  $old_line_count_in_batch
  $last_nonblank_index_to_go
  $last_nonblank_type_to_go
  $last_nonblank_token_to_go
  $last_last_nonblank_index_to_go
  $last_last_nonblank_type_to_go
  $last_last_nonblank_token_to_go
  @nonblank_lines_at_depth

  $forced_breakpoint_count
  $forced_breakpoint_undo_count
  @forced_breakpoint_undo_stack
  %postponed_breakpoint

  $tabbing
  $embedded_tab_count
  $first_embedded_tab_at
  $last_embedded_tab_at
  $deleted_semicolon_count
  $first_deleted_semicolon_at
  $last_deleted_semicolon_at
  $added_semicolon_count
  $first_added_semicolon_at
  $last_added_semicolon_at
  $saw_negative_indentation
  $first_tabbing_disagreement
  $last_tabbing_disagreement
  $in_tabbing_disagreement
  $tabbing_disagreement_count
  $input_line_tabbing

  $last_line_type
  $last_line_leading_type
  $last_line_leading_level
  $last_last_line_leading_level

  %block_leading_text
  %block_opening_line_number
  $csc_new_statement_ok
  $accumulating_text_for_block
  $leading_block_text
  $rleading_block_if_elsif_text
  $leading_block_text_level
  $leading_block_text_length_exceeded
  $leading_block_text_line_length
  $leading_block_text_line_number
  $closing_side_comment_prefix_pattern
  $closing_side_comment_list_pattern

  $last_nonblank_token
  $last_nonblank_type
  $last_last_nonblank_token
  $last_last_nonblank_type
  $last_nonblank_block_type
  $last_output_level
  %is_do_follower
  %is_if_brace_follower
  %space_before_paren
  $rbrace_follower
  $looking_for_else
  %is_other_brace_follower
  %is_else_brace_follower
  %is_anon_sub_brace_follower
  %is_anon_sub_1_brace_follower
  %is_sort_map_grep
  %is_sort_map_grep_eval
  %is_block_without_semicolon
  %is_if_unless_and_or

  @has_broken_sublist
  @dont_align
  @want_comma_break

  $index_start_one_line_block
  $semicolons_before_block_self_destruct
  $index_max_forced_break
  $input_line_number
  $diagnostics_object
  $vertical_aligner_object
  $logger_object
  $file_writer_object
  $formatter_self
  @ci_stack
  $last_line_had_side_comment
  %want_break_before
  %outdent_keyword
  $static_block_comment_pattern
  $static_side_comment_pattern
  %opening_vertical_tightness
  %closing_vertical_tightness
  $block_brace_vertical_tightness_pattern

  $rOpts_add_newlines
  $rOpts_add_whitespace
  $rOpts_block_brace_tightness
  $rOpts_block_brace_vertical_tightness
  $rOpts_brace_left_and_indent
  $rOpts_comma_arrow_breakpoints
  $rOpts_break_at_old_keyword_breakpoints
  $rOpts_break_at_old_comma_breakpoints
  $rOpts_break_at_old_logical_breakpoints
  $rOpts_break_at_old_trinary_breakpoints
  $rOpts_closing_side_comment_else_flag
  $rOpts_closing_side_comment_maximum_text
  $rOpts_continuation_indentation
  $rOpts_cuddled_else
  $rOpts_delete_old_whitespace
  $rOpts_fuzzy_line_length
  $rOpts_indent_columns
  $rOpts_line_up_parentheses
  $rOpts_maximum_fields_per_table
  $rOpts_maximum_line_length
  $rOpts_short_concatenation_item_length
  $rOpts_swallow_optional_blank_lines
  $rOpts_ignore_old_line_breaks

  $half_maximum_line_length

  %is_opening_type
  %is_closing_type
  %is_keyword_returning_list
  %tightness
  %matching_token
  $rOpts
  %right_bond_strength
  %left_bond_strength
  %binary_ws_rules
  %want_left_space
  %want_right_space
  %is_digraph
  %is_trigraph
  $bli_pattern
  $bli_list_string
  %is_closing_type
  %is_opening_type
  %is_closing_token
  %is_opening_token
};

BEGIN {

    # default list of block types for which -bli would apply
    $bli_list_string = 'if else elsif unless while for foreach do : sub';

    @_ = qw(
      .. :: << >> ** && .. ||  -> => += -= .= %= &= |= ^= *= <>
      <= >= == =~ !~ != ++ -- /= x=
    );
    @is_digraph{@_} = (1) x scalar(@_);

    @_ = qw( ... **= <<= >>= &&= ||= <=> );
    @is_trigraph{@_} = (1) x scalar(@_);

    @_ = qw(
      grep
      keys
      map
      reverse
      sort
      split
    );
    @is_keyword_returning_list{@_} = (1) x scalar(@_);

    @_ = qw(sort map grep);
    @is_sort_map_grep{@_} = (1) x scalar(@_);

    @_ = qw(sort map grep eval);
    @is_sort_map_grep_eval{@_} = (1) x scalar(@_);

    @_ = qw(if unless and or);
    @is_if_unless_and_or{@_} = (1) x scalar(@_);

    # We can remove semicolons after blocks preceded by these keywords
    @_ = qw(BEGIN END CHECK INIT AUTOLOAD DESTROY continue if elsif else
      unless while until for foreach);
    @is_block_without_semicolon{@_} = (1) x scalar(@_);

    # 'L' is token for opening { at hash key
    @_ = qw" L { ( [ ";
    @is_opening_type{@_} = (1) x scalar(@_);

    # 'R' is token for closing } at hash key
    @_ = qw" R } ) ] ";
    @is_closing_type{@_} = (1) x scalar(@_);

    @_ = qw" { ( [ ";
    @is_opening_token{@_} = (1) x scalar(@_);

    @_ = qw" } ) ] ";
    @is_closing_token{@_} = (1) x scalar(@_);
}

# whitespace codes
use constant WS_YES      => 1;
use constant WS_OPTIONAL => 0;
use constant WS_NO       => -1;

# Token bond strengths.  
use constant NO_BREAK    => 10000;
use constant VERY_STRONG => 100;
use constant STRONG      => 2.1;
use constant NOMINAL     => 1.1;
use constant WEAK        => 0.8;
use constant VERY_WEAK   => 0.55;

# values for testing indexes in output array
use constant UNDEFINED_INDEX => -1;

# Maximum number of little messages; probably need not be changed.
use constant MAX_NAG_MESSAGES => 6;

# increment between sequence numbers for each type
# For example, ?: pairs might have numbers 7,11,15,...
use constant TYPE_SEQUENCE_INCREMENT => 4;

{

    # methods to count instances
    my $_count = 0;
    sub get_count        { $_count; }
    sub _increment_count { ++$_count }
    sub _decrement_count { --$_count }
}

# interface to Perl::Tidy::Logger routines
sub warning {
    if ($logger_object) {
        $logger_object->warning(@_);
    }
}

sub complain {
    if ($logger_object) {
        $logger_object->complain(@_);
    }
}

sub write_logfile_entry {
    if ($logger_object) {
        $logger_object->write_logfile_entry(@_);
    }
}

sub black_box {
    if ($logger_object) {
        $logger_object->black_box(@_);
    }
}

sub report_definite_bug {
    if ($logger_object) {
        $logger_object->report_definite_bug();
    }
}

sub get_saw_brace_error {
    if ($logger_object) {
        $logger_object->get_saw_brace_error();
    }
}

sub we_are_at_the_last_line {
    if ($logger_object) {
        $logger_object->we_are_at_the_last_line();
    }
}

# interface to Perl::Tidy::Diagnostics routine
sub write_diagnostics {

    if ($diagnostics_object) {
        $diagnostics_object->write_diagnostics(@_);
    }
}

sub get_added_semicolon_count {
    my $self = shift;
    return $added_semicolon_count;
}

sub DESTROY {
    $_[0]->_decrement_count();
}

sub new {

    my $class = shift;

    # we are given an object with a write_line() method to take lines
    my %defaults = (
        sink_object        => undef,
        diagnostics_object => undef,
        logger_object      => undef,
    );
    my %args = ( %defaults, @_ );

    $logger_object      = $args{logger_object};
    $diagnostics_object = $args{diagnostics_object};

    # FIXME: we create another object with a get_line() and peek_ahead() method
    my $sink_object = $args{sink_object};
    $file_writer_object =
      Perl::Tidy::FileWriter->new( $sink_object, $rOpts, $logger_object );

    # initialize the leading whitespace stack to negative levels
    # so that we can never run off the end of the stack
    $gnu_position_predictor = 0;    # where the current token is predicted to be
    $max_gnu_stack_index    = 0;
    $max_gnu_item_index     = -1;
    $gnu_stack[0] = new_lp_indentation_item( 0, -1, -1, 0, 0 );
    @gnu_item_list               = ();
    $last_output_indentation     = 0;
    $last_indentation_written    = 0;
    $last_unadjusted_indentation = 0;

    $saw_VERSION_in_this_file = !$rOpts->{'pass-version-line'};

    @block_type_to_go            = ();
    @type_sequence_to_go         = ();
    @container_environment_to_go = ();
    @bond_strength_to_go         = ();
    @forced_breakpoint_to_go     = ();
    @lengths_to_go               = ();    # line length to start of ith token
    @levels_to_go                = ();
    @matching_token_to_go        = ();
    @mate_index_to_go            = ();
    @nesting_blocks_to_go        = ();
    @ci_levels_to_go             = ();
    @nesting_depth_to_go         = (0);
    @nobreak_to_go               = ();
    @old_breakpoint_to_go        = ();
    @tokens_to_go                = ();
    @types_to_go                 = ();
    @leading_spaces_to_go        = ();
    @reduced_spaces_to_go        = ();

    @dont_align         = ();
    @has_broken_sublist = ();
    @want_comma_break   = ();

    @ci_stack                   = ("");
    $saw_negative_indentation   = 0;
    $first_tabbing_disagreement = 0;
    $last_tabbing_disagreement  = 0;
    $tabbing_disagreement_count = 0;
    $in_tabbing_disagreement    = 0;
    $input_line_tabbing         = undef;

    $last_line_type               = "";
    $last_last_line_leading_level = 0;
    $last_line_leading_level      = 0;
    $last_line_leading_type       = '#';

    $last_nonblank_token        = ';';
    $last_nonblank_type         = ';';
    $last_last_nonblank_token   = ';';
    $last_last_nonblank_type    = ';';
    $last_nonblank_block_type   = "";
    $last_output_level          = 0;
    $looking_for_else           = 0;
    $embedded_tab_count         = 0;
    $first_embedded_tab_at      = 0;
    $last_embedded_tab_at       = 0;
    $deleted_semicolon_count    = 0;
    $first_deleted_semicolon_at = 0;
    $last_deleted_semicolon_at  = 0;
    $added_semicolon_count      = 0;
    $first_added_semicolon_at   = 0;
    $last_added_semicolon_at    = 0;
    $last_line_had_side_comment = 0;
    %postponed_breakpoint       = ();

    # variables for adding side comments
    %block_leading_text        = ();
    %block_opening_line_number = ();
    $csc_new_statement_ok      = 1;

    %saved_opening_indentation = ();

    reset_block_text_accumulator();

    prepare_for_new_input_lines();

    $vertical_aligner_object =
      Perl::Tidy::VerticalAligner->initialize( $rOpts, $file_writer_object,
        $logger_object, $diagnostics_object );

    if ( $rOpts->{'entab-leading-whitespace'} ) {
        write_logfile_entry(
"Leading whitespace will be entabbed with $rOpts->{'entab-leading-whitespace'} spaces per tab\n"
        );
    }
    elsif ( $rOpts->{'tabs'} ) {
        write_logfile_entry("Indentation will be with a tab character\n");
    }
    else {
        write_logfile_entry(
            "Indentation will be with $rOpts->{'indent-columns'} spaces\n");
    }

    # This was the start of a formatter referent, but object-oriented
    # coding has turned out to be too slow here.
    $formatter_self = {};

    bless $formatter_self, $class;

    # Safety check..this is not a class yet
    if ( _increment_count() > 1 ) {
        confess
"Attempt to create more than 1 object in $class, which is not a true class yet\n";
    }
    return $formatter_self;
}

sub prepare_for_new_input_lines {

    $gnu_sequence_number++;    # increment output batch counter
    %last_gnu_equals                = ();
    %gnu_comma_count                = ();
    %gnu_arrow_count                = ();
    $line_start_index_to_go         = 0;
    $max_gnu_item_index             = UNDEFINED_INDEX;
    $index_max_forced_break         = UNDEFINED_INDEX;
    $max_index_to_go                = UNDEFINED_INDEX;
    $last_nonblank_index_to_go      = UNDEFINED_INDEX;
    $last_nonblank_type_to_go       = '';
    $last_nonblank_token_to_go      = '';
    $last_last_nonblank_index_to_go = UNDEFINED_INDEX;
    $last_last_nonblank_type_to_go  = '';
    $last_last_nonblank_token_to_go = '';
    $forced_breakpoint_count        = 0;
    $forced_breakpoint_undo_count   = 0;
    $rbrace_follower                = undef;
    $lengths_to_go[0] = 0;
    $old_line_count_in_batch = 1;
    $comma_count_in_batch    = 0;

    destroy_one_line_block();
}

sub write_line {

    my $self = shift;
    my ($line_of_tokens) = @_;

    my $line_type            = $line_of_tokens->{_line_type};
    my $input_line           = $line_of_tokens->{_line_text};
    my $want_blank_line_next = 0;

    # _line_type codes are: 
    #   SYSTEM         - system-specific code before hash-bang line
    #   CODE           - line of perl code (including comments)
    #   POD_START      - line starting pod, such as '=head'
    #   POD            - pod documentation text 
    #   POD_END        - last line of pod section, '=cut'
    #   HERE           - text of here-document 
    #   HERE_END       - last line of here-doc (target word)
    #   FORMAT         - format section
    #   FORMAT_END     - last line of format section, '.'
    #   DATA_START     - __DATA__ line
    #   DATA           - unidentified text following __DATA__ 
    #   END_START      - __END__ line
    #   END            - unidentified text following __END__ 
    #   ERROR          - we are in big trouble, probably not a perl script
    #
    # handle line of code..
    if ( $line_type eq 'CODE' ) {

        # let logger see all non-blank lines of code
        if ( $input_line !~ /^\s*$/ ) {
            my $output_line_number =
              $vertical_aligner_object->get_output_line_number();
            black_box( $line_of_tokens, $output_line_number );
        }
        print_line_of_tokens($line_of_tokens);
    }

    # handle line of non-code..
    else {

        # set special flags
        my $skip_line = 0;
        my $tee_line  = 0;
        if ( $line_type =~ /^POD/ ) {

            # Pod docs should have a preceding blank line.  But be
            # very careful in __END__ and __DATA__ sections, because:
            #   1. the user may be using this section for any purpose whatsoever
            #   2. the blank counters are not active there
            # It should be safe to request a blank line between an
            # __END__ or __DATA__ and an immediately following '=head'
            # type line, (types END_START and DATA_START), but not for
            # any other lines of type END or DATA. 
            if ( $rOpts->{'delete-pod'} ) { $skip_line = 1; }
            if ( $rOpts->{'tee-pod'} )    { $tee_line  = 1; }
            if (   !$skip_line
                && $line_type eq 'POD_START'
                && $last_line_type !~ /^(END|DATA)$/ )
            {
                want_blank_line();
            }

            # patch to put a blank line after =cut
            # (required by podchecker)
            if ( $line_type eq 'POD_END' ) {
                $file_writer_object->reset_consecutive_blank_lines();
                $want_blank_line_next = 1;
            }

        }

        # leave the blank counters in a predictable state 
        # after __END__ or __DATA__
        elsif ( $line_type =~ /^(END_START|DATA_START)$/ ) {
            $file_writer_object->reset_consecutive_blank_lines();
        }

        # write unindented non-code line
        if ( !$skip_line ) {
            if ($tee_line) { $file_writer_object->tee_on() }
            write_unindented_line($input_line);
            if ($tee_line)             { $file_writer_object->tee_off() }
            if ($want_blank_line_next) { want_blank_line(); }
        }
    }
    $last_line_type = $line_type;
}

sub create_one_line_block {
    $index_start_one_line_block            = $_[0];
    $semicolons_before_block_self_destruct = $_[1];
}

sub destroy_one_line_block {
    $index_start_one_line_block            = UNDEFINED_INDEX;
    $semicolons_before_block_self_destruct = 0;
}

sub leading_spaces_to_go {

    # return the number of indentation spaces for a token in the output stream;
    # these were previously stored by 'set_leading_whitespace'.

    return get_SPACES( $leading_spaces_to_go[ $_[0] ] );

}

sub get_SPACES {

    # return the number of leading spaces associated with an indentation
    # variable $indentation is either a constant number of spaces or an object
    # with a get_SPACES method.
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_SPACES() : $indentation;
}

sub get_AVAILABLE_SPACES_to_go {

    my $item = $leading_spaces_to_go[ $_[0] ];

    # return the number of available leading spaces associated with an
    # indentation variable.  $indentation is either a constant number of
    # spaces or an object with a get_AVAILABLE_SPACES method.
    return ref($item) ? $item->get_AVAILABLE_SPACES() : 0;
}

sub new_lp_indentation_item {

    # this is an interface to the IndentationItem class
    my ( $spaces, $level, $ci_level, $available_spaces, $align_paren ) = @_;

    # A negative level implies not to store the item in the item_list
    my $index = 0;
    if ( $level >= 0 ) { $index = ++$max_gnu_item_index; }

    my $item = Perl::Tidy::IndentationItem->new(
        $spaces,      $level,
        $ci_level,    $available_spaces,
        $index,       $gnu_sequence_number,
        $align_paren, $max_gnu_stack_index,
        $line_start_index_to_go,
    );

    if ( $level >= 0 ) {
        $gnu_item_list[$max_gnu_item_index] = $item;
    }

    return $item;
}

sub set_leading_whitespace {

    # This routine defines leading whitespace 
    # given: the level and continuation_level of a token,
    # define: space count of leading string which would apply if it
    # were the first token of a new line.

    my ( $level, $ci_level, $in_continued_quote ) = @_;

    # modify for -bli, which adds one continuation indentation for 
    # opening braces
    if (   $rOpts_brace_left_and_indent
        && $max_index_to_go == 0
        && $block_type_to_go[$max_index_to_go] =~ /$bli_pattern/o )
    {
        $ci_level++;
    }

    # patch to avoid trouble when input file has negative indentation. 
    # other logic should catch this error.
    if ( $level < 0 ) { $level = 0 }

    #-------------------------------------------
    # handle the standard indentation scheme
    #-------------------------------------------
    unless ($rOpts_line_up_parentheses) {
        my $space_count = $ci_level * $rOpts_continuation_indentation + $level *
          $rOpts_indent_columns;
        my $ci_spaces =
          ( $ci_level == 0 ) ? 0 : $rOpts_continuation_indentation;

        if ($in_continued_quote) {
            $space_count = 0;
            $ci_spaces   = 0;
        }
        $leading_spaces_to_go[$max_index_to_go] = $space_count;
        $reduced_spaces_to_go[$max_index_to_go] = $space_count - $ci_spaces;
        return;
    }

    #-------------------------------------------------------------
    # handle case of -lp indentation..
    #-------------------------------------------------------------

    # The continued_quote flag means that this is the first token of a
    # line, and it is the continuation of some kind of multi-line quote
    # or pattern.  It requires special treatment because it must have no
    # added leading whitespace. So we create a special indentation item
    # which is not in the stack.
    if ($in_continued_quote) {
        my $space_count     = 0;
        my $available_space = 0;
        $level = -1;    # flag to prevent storing in item_list
        $leading_spaces_to_go[$max_index_to_go]   =
          $reduced_spaces_to_go[$max_index_to_go] =
          new_lp_indentation_item( $space_count, $level, $ci_level,
            $available_space, 0 );
        return;
    }

    # get the top state from the stack
    my $space_count      = $gnu_stack[$max_gnu_stack_index]->get_SPACES();
    my $current_level    = $gnu_stack[$max_gnu_stack_index]->get_LEVEL();
    my $current_ci_level = $gnu_stack[$max_gnu_stack_index]->get_CI_LEVEL();

    my $type        = $types_to_go[$max_index_to_go];
    my $token       = $tokens_to_go[$max_index_to_go];
    my $total_depth = $nesting_depth_to_go[$max_index_to_go];

    if ( $type eq '{' || $type eq '(' ) {

        $gnu_comma_count{ $total_depth + 1 } = 0;
        $gnu_arrow_count{ $total_depth + 1 } = 0;

        # If we come to an opening token after an '=' token of some type,
        # see if it would be helpful to 'break' after the '=' to save space
        my $last_equals = $last_gnu_equals{$total_depth};
        if ( $last_equals && $last_equals > $line_start_index_to_go ) {

            # find the position if we break at the '='
            my $i_test = $last_equals;
            if ( $types_to_go[ $i_test + 1 ] eq 'b' ) { $i_test++ }
            my $test_position = total_line_length( $i_test, $max_index_to_go );

            if (

                # if we are beyond the midpoint
                $gnu_position_predictor > $half_maximum_line_length

                # or if we can save some space by breaking at the '='
                # without obscuring the second line by the first
                || ( $test_position > 1 +
                    total_line_length( $line_start_index_to_go, $last_equals ) )
              )
            {

                # then make the switch -- note that we do not set a real
                # breakpoint here because we may not really need one; sub
                # scan_list will do that if necessary
                $line_start_index_to_go = $i_test + 1;
                $gnu_position_predictor = $test_position;
            }
        }
    }

    # Check for decreasing depth ..
    # Note that one token may have both decreasing and then increasing
    # depth. For example, (level, ci) can go from (1,1) to (2,0).  So,
    # in this example we would first go back to (1,0) then up to (2,0)
    # in a single call.
    if ( $level < $current_level || $ci_level < $current_ci_level ) {

        # loop to find the first entry at or completely below this level
        my ( $lev, $ci_lev );
        while (1) {
            if ($max_gnu_stack_index) {

                # save index of token which closes this level
                $gnu_stack[$max_gnu_stack_index]->set_CLOSED($max_index_to_go);

                # Undo any extra indentation if we saw no commas
                my $available_spaces =
                  $gnu_stack[$max_gnu_stack_index]->get_AVAILABLE_SPACES();

                my $comma_count = 0;
                my $arrow_count = 0;
                if ( $type eq '}' || $type eq ')' ) {
                    $comma_count = $gnu_comma_count{$total_depth};
                    $arrow_count = $gnu_arrow_count{$total_depth};
                    $comma_count = 0 unless $comma_count;
                    $arrow_count = 0 unless $arrow_count;
                }
                $gnu_stack[$max_gnu_stack_index]->set_COMMA_COUNT($comma_count);
                $gnu_stack[$max_gnu_stack_index]->set_ARROW_COUNT($arrow_count);

                if ( $available_spaces > 0 ) {

                    if ( $comma_count <= 0 || $arrow_count > 0 ) {

                        my $i = $gnu_stack[$max_gnu_stack_index]->get_INDEX();
                        my $seqno =
                          $gnu_stack[$max_gnu_stack_index]
                          ->get_SEQUENCE_NUMBER();

                        # Be sure this item was created in this batch.  This
                        # should be true because we delete any available
                        # space from open items at the end of each batch.
                        if (   $gnu_sequence_number != $seqno
                            || $i > $max_gnu_item_index )
                        {
                            warning(
"Program bug with -lp.  seqno=$seqno should be $gnu_sequence_number and i=$i should be less than max=$max_gnu_item_index\n"
                            );
                            report_definite_bug();
                        }

                        else {
                            if ( $arrow_count == 0 ) {
                                $gnu_item_list[$i]
                                  ->permanently_decrease_AVAILABLE_SPACES(
                                    $available_spaces);
                            }
                            else {
                                $gnu_item_list[$i]
                                  ->tentatively_decrease_AVAILABLE_SPACES(
                                    $available_spaces);
                            }

                            my $j;
                            for (
                                $j = $i + 1 ;
                                $j <= $max_gnu_item_index ;
                                $j++
                              )
                            {
                                $gnu_item_list[$j]
                                  ->decrease_SPACES($available_spaces);
                            }
                        }
                    }
                }

                # go down one level
                --$max_gnu_stack_index;
                $lev    = $gnu_stack[$max_gnu_stack_index]->get_LEVEL();
                $ci_lev = $gnu_stack[$max_gnu_stack_index]->get_CI_LEVEL();

                # stop when we reach a level at or below the current level
                if ( $lev <= $level && $ci_lev <= $ci_level ) {
                    $space_count =
                      $gnu_stack[$max_gnu_stack_index]->get_SPACES();
                    $current_level    = $lev;
                    $current_ci_level = $ci_lev;
                    last;
                }
            }

            # reached bottom of stack .. should never happen because
            # only negative levels can get here, and $level was forced
            # to be positive above.
            else {
                warning(
"program bug with -lp: stack_error. level=$level; lev=$lev; ci_level=$ci_level; ci_lev=$ci_lev; rerun with -nlp\n"
                );
                report_definite_bug();
                last;
            }
        }
    }

    # handle increasing depth
    if ( $level > $current_level || $ci_level > $current_ci_level ) {

        # Compute the standard incremental whitespace.  This will be
        # the minimum incremental whitespace that will be used.  This
        # choice results in a smooth transition between the gnu-style
        # and the standard style.
        my $standard_increment =
          ( $level - $current_level ) * $rOpts_indent_columns +
          ( $ci_level - $current_ci_level ) * $rOpts_continuation_indentation;

        # Now we have to define how much extra incremental space
        # ("$available_space") we want.  This extra space will be
        # reduced as necessary when long lines are encountered or when
        # it becomes clear that we do not have a good list.
        my $available_space = 0;
        my $align_paren     = 0;
        my $excess          = 0;

        # initialization on empty stack..
        if ( $max_gnu_stack_index == 0 ) {
            $space_count = $level * $rOpts_indent_columns;
        }

        # if this is a BLOCK, add the standard increment
        elsif ($last_nonblank_block_type) {
            $space_count += $standard_increment;
        }

        # if last nonblank token was not structural indentation,
        # just use standard increment
        elsif ( $last_nonblank_type ne '{' ) {
            $space_count += $standard_increment;
        }

        # otherwise use the space to the first non-blank level change token
        else {

            $space_count = $gnu_position_predictor;

            my $min_gnu_indentation =
              $gnu_stack[$max_gnu_stack_index]->get_SPACES();

            $available_space = $space_count - $min_gnu_indentation;
            if ( $available_space >= $standard_increment ) {
                $min_gnu_indentation += $standard_increment;
            }
            elsif ( $available_space > 1 ) {
                $min_gnu_indentation += $available_space + 1;
            }
            elsif ( $last_nonblank_token =~ /^[\{\[\(]$/ ) {
                if ( ( $tightness{$last_nonblank_token} < 2 ) ) {
                    $min_gnu_indentation += 2;
                }
                else {
                    $min_gnu_indentation += 1;
                }
            }
            else {
                $min_gnu_indentation += $standard_increment;
            }
            $available_space = $space_count - $min_gnu_indentation;

            if ( $available_space < 0 ) {
                $space_count     = $min_gnu_indentation;
                $available_space = 0;
            }
            $align_paren = 1;
        }

        # update state, but not on a blank token
        if ( $types_to_go[$max_index_to_go] ne 'b' ) {

            $gnu_stack[$max_gnu_stack_index]->set_HAVE_CHILD(1);

            ++$max_gnu_stack_index;
            $gnu_stack[$max_gnu_stack_index] =
              new_lp_indentation_item( $space_count, $level, $ci_level,
                $available_space, $align_paren );

            # If the opening paren is beyond the half-line length, then
            # we will use the minimum (standard) indentation.  This will
            # help avoid problems associated with running out of space
            # near the end of a line.  As a result, in deeply nested
            # lists, there will be some indentations which are limited
            # to this minimum standard indentation. But the most deeply
            # nested container will still probably be able to shift its
            # parameters to the right for proper alignment, so in most
            # cases this will not be noticable.
            if (   $available_space > 0
                && $space_count > $half_maximum_line_length )
            {
                $gnu_stack[$max_gnu_stack_index]
                  ->tentatively_decrease_AVAILABLE_SPACES($available_space);
            }
        }
    }

    # Count commas and look for non-list characters.  Once we see a
    # non-list character, we give up and don't look for any more commas.
    if ( $type eq '=>' ) {
        $gnu_arrow_count{$total_depth}++;
    }

    if ( $type eq ',' ) {
        $gnu_comma_count{$total_depth}++;
    }

    elsif ( $type =~ /=/ ) {
        $last_gnu_equals{$total_depth} = $max_index_to_go;
    }

    # this token might start a new line
    # if this is a non-blank..
    if ( $type ne 'b' ) {

        # and if ..
        if (

            # this is the first nonblank token of the line
            $max_index_to_go == 1 && $types_to_go[0] eq 'b'

            # or previous character was one of these:
            || $last_nonblank_type_to_go =~ /^([\:\?\,f])$/

            # or previous character was opening and this does not close it
            || ( $last_nonblank_type_to_go eq '{' && $type ne '}' )
            || ( $last_nonblank_type_to_go eq '(' and $type ne ')' )

            # or this token is one of these:
            || $type =~ /^([\.]|\|\||\&\&)$/

            # or this is a closing structure
            || (   $last_nonblank_type_to_go eq '}'
                && $last_nonblank_token_to_go eq $last_nonblank_type_to_go )

            # or previous token was keyword 'return'
            || ( $last_nonblank_type_to_go eq 'k'
                && ( $last_nonblank_token_to_go eq 'return' && $type ne '{' ) )

            # or starting a new line at certain keywords is fine
            || ( $type eq 'k'
                && ( $token =~ /^(if|unless|and|or|last|next|redo|return)$/ ) )

            # or this is after an assignment after a closing structure
            || (
                   $last_nonblank_type_to_go =~ /=/
                && $last_nonblank_type_to_go !~ /(==|!=|>=|<=|=~|=>)/
                && (
                    $last_last_nonblank_type_to_go =~ /^[\}\)\]]$/

                    # and it is significantly to the right
                    || $gnu_position_predictor > $half_maximum_line_length
                )
            )
          )
        {
            check_for_long_gnu_style_lines();
            $line_start_index_to_go = $max_index_to_go;

            # back up 1 token if we want to break before that type
            # otherwise, we may strand tokens like '?' or ':' on a line
            if ( $line_start_index_to_go > 0 ) {
                if ( $last_nonblank_type_to_go eq 'k' ) {
                    if ( $last_nonblank_token_to_go =~ /^(and|or)$/ ) {
                        $line_start_index_to_go--;
                    }
                }
                elsif ( $want_break_before{$last_nonblank_type_to_go} ) {
                    $line_start_index_to_go--;
                }
            }
        }
    }

    # remember the predicted position of this token on the output line
    if ( $max_index_to_go > $line_start_index_to_go ) {
        $gnu_position_predictor =
          total_line_length( $line_start_index_to_go, $max_index_to_go );
    }
    else {
        $gnu_position_predictor = $space_count +
          token_sequence_length( $max_index_to_go, $max_index_to_go );
    }

    # store the indentation object for this token
    # this allows us to manipulate the leading whitespace 
    # (in case we have to reduce indentation to fit a line) without 
    # having to change any token values
    $leading_spaces_to_go[$max_index_to_go] = $gnu_stack[$max_gnu_stack_index];
    $reduced_spaces_to_go[$max_index_to_go] =
      ( $max_gnu_stack_index > 0 && $ci_level )
      ? $gnu_stack[ $max_gnu_stack_index - 1 ]
      : $gnu_stack[$max_gnu_stack_index];
    return;
}

sub check_for_long_gnu_style_lines {

    # look at the current estimated maximum line length, and
    # remove some whitespace if it exceeds the desired maximum

    # this is only for the '-lp' style
    return unless ($rOpts_line_up_parentheses);

    # nothing can be done if no stack items defined for this line
    return if ( $max_gnu_item_index == UNDEFINED_INDEX );

    # see if we have exceeded the maximum desired line length
    # keep 2 extra free because they are needed in some cases
    # (result of trial-and-error testing)
    my $spaces_needed =
      $gnu_position_predictor - $rOpts_maximum_line_length + 2;

    return if ( $spaces_needed < 0 );

    # We are over the limit, so try to remove a requested number of
    # spaces from leading whitespace.  We are only allowed to remove
    # from whitespace items created on this batch, since others have
    # already been used and cannot be undone.
    my @candidates = ();
    my $i;

    # loop over all whitespace items created for the current batch
    for ( $i = 0 ; $i <= $max_gnu_item_index ; $i++ ) {
        my $item = $gnu_item_list[$i];

        # item must still be open to be a candidate (otherwise it
        # cannot influence the current token)
        next if ( $item->get_CLOSED() >= 0 );

        my $available_spaces = $item->get_AVAILABLE_SPACES();

        if ( $available_spaces > 0 ) {
            push ( @candidates, [ $i, $available_spaces ] );
        }
    }

    return unless (@candidates);

    # sort by available whitespace so that we can remove whitespace
    # from the maximum available first
    @candidates = sort { $b->[1] <=> $a->[1] } @candidates;

    # keep removing whitespace until we are done or have no more
    my $candidate;
    foreach $candidate (@candidates) {
        my ( $i, $available_spaces ) = @{$candidate};
        my $deleted_spaces =
          ( $available_spaces > $spaces_needed )
          ? $spaces_needed
          : $available_spaces;

        # remove the incremental space from this item
        $gnu_item_list[$i]->decrease_AVAILABLE_SPACES($deleted_spaces);

        my $i_debug = $i;

        # update the leading whitespace of this item and all items
        # that came after it
        for ( ; $i <= $max_gnu_item_index ; $i++ ) {

            my $old_spaces = $gnu_item_list[$i]->get_SPACES();
            if ( $old_spaces > $deleted_spaces ) {
                $gnu_item_list[$i]->decrease_SPACES($deleted_spaces);
            }

            # shouldn't happen except for code bug:
            else {
                my $level        = $gnu_item_list[$i_debug]->get_LEVEL();
                my $ci_level     = $gnu_item_list[$i_debug]->get_CI_LEVEL();
                my $old_level    = $gnu_item_list[$i]->get_LEVEL();
                my $old_ci_level = $gnu_item_list[$i]->get_CI_LEVEL();
                warning(
"program bug with -lp: want to delete $deleted_spaces from item $i, but old=$old_spaces deleted: lev=$level ci=$ci_level  deleted: level=$old_level ci=$ci_level\n"
                );
                report_definite_bug();
            }
        }
        $gnu_position_predictor -= $deleted_spaces;
        $spaces_needed          -= $deleted_spaces;
        last unless ( $spaces_needed > 0 );
    }
}

sub finish_lp_batch {

    # This routine is called once after each each output stream batch is
    # finished to undo indentation for all incomplete -lp
    # indentation levels.  It is too risky to leave a level open,
    # because then we can't backtrack in case of a long line to follow.
    # This means that comments and blank lines will disrupt this
    # indentation style.  But the vertical aligner may be able to
    # get the space back if there are side comments.

    # this is only for the 'lp' style
    return unless ($rOpts_line_up_parentheses);

    # nothing can be done if no stack items defined for this line
    return if ( $max_gnu_item_index == UNDEFINED_INDEX );

    # loop over all whitespace items created for the current batch
    my $i;
    for ( $i = 0 ; $i <= $max_gnu_item_index ; $i++ ) {
        my $item = $gnu_item_list[$i];

        # only look for open items
        next if ( $item->get_CLOSED() >= 0 );

        # Tentatively remove all of the available space
        # (The vertical aligner will try to get it back later)
        my $available_spaces = $item->get_AVAILABLE_SPACES();
        if ( $available_spaces > 0 ) {

            # delete incremental space for this item
            $gnu_item_list[$i]
              ->tentatively_decrease_AVAILABLE_SPACES($available_spaces);

            # Reduce the total indentation space of any nodes that follow
            # Note that any such nodes must necessarily be dependents
            # of this node.
            foreach ( $i + 1 .. $max_gnu_item_index ) {
                $gnu_item_list[$_]->decrease_SPACES($available_spaces);
            }
        }
    }
    return;
}

sub reduce_lp_indentation {

    # reduce the leading whitespace at token $i if possible by $spaces_needed
    # (a large value of $spaces_needed will remove all excess space)
    # NOTE: to be called from scan_list only for a sequence of tokens
    # contained between opening and closing parens/braces/brackets

    my ( $i, $spaces_wanted ) = @_;
    my $deleted_spaces = 0;

    my $item             = $leading_spaces_to_go[$i];
    my $available_spaces = $item->get_AVAILABLE_SPACES();

    if (
        $available_spaces > 0
        && ( ( $spaces_wanted <= $available_spaces )
            || !$item->get_HAVE_CHILD() )
      )
    {

        # we'll remove these spaces, but mark them as recoverable
        $deleted_spaces =
          $item->tentatively_decrease_AVAILABLE_SPACES($spaces_wanted);
    }

    return $deleted_spaces;
}

sub token_sequence_length {

    # return length of tokens ($ifirst .. $ilast) including first & last
    # returns 0 if $ifirst > $ilast
    my $ifirst = shift;
    my $ilast  = shift;
    return 0 if ( $ilast < 0 || $ifirst > $ilast );
    return $lengths_to_go[ $ilast + 1 ] if ( $ifirst < 0 );
    return $lengths_to_go[ $ilast + 1 ] - $lengths_to_go[$ifirst];
}

sub total_line_length {

    # return length of a line of tokens ($ifirst .. $ilast)
    my $ifirst = shift;
    my $ilast  = shift;
    if ( $ifirst < 0 ) { $ifirst = 0 }

    return leading_spaces_to_go($ifirst) +
      token_sequence_length( $ifirst, $ilast );
}

sub excess_line_length {

    # return number of characters by which a line of tokens ($ifirst..$ilast)
    # exceeds the allowable line length.
    my $ifirst = shift;
    my $ilast  = shift;
    if ( $ifirst < 0 ) { $ifirst = 0 }
    return leading_spaces_to_go($ifirst) +
      token_sequence_length( $ifirst, $ilast ) - $rOpts_maximum_line_length;
}

sub finish_formatting {

    # flush buffer and write any informative messages
    my $self = shift;

    flush();
    $file_writer_object->decrement_output_line_number()
      ;    # fix up line number since it was incremented
    we_are_at_the_last_line();
    if ( $added_semicolon_count > 0 ) {
        my $first = ( $added_semicolon_count > 1 ) ? "First" : "";
        my $what =
          ( $added_semicolon_count > 1 ) ? "semicolons were" : "semicolon was";
        write_logfile_entry("$added_semicolon_count $what added:\n");
        write_logfile_entry(
            "  $first at input line $first_added_semicolon_at\n");

        if ( $added_semicolon_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_added_semicolon_at\n");
        }
        write_logfile_entry("  (Use -nasc to prevent semicolon addition)\n");
        write_logfile_entry("\n");
    }

    if ( $deleted_semicolon_count > 0 ) {
        my $first = ( $deleted_semicolon_count > 1 ) ? "First" : "";
        my $what =
          ( $deleted_semicolon_count > 1 )
          ? "semicolons were"
          : "semicolon was";
        write_logfile_entry(
            "$deleted_semicolon_count unnecessary $what deleted:\n");
        write_logfile_entry(
            "  $first at input line $first_deleted_semicolon_at\n");

        if ( $deleted_semicolon_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_deleted_semicolon_at\n");
        }
        write_logfile_entry("  (Use -ndsc to prevent semicolon deletion)\n");
        write_logfile_entry("\n");
    }

    if ( $embedded_tab_count > 0 ) {
        my $first = ( $embedded_tab_count > 1 ) ? "First" : "";
        my $what =
          ( $embedded_tab_count > 1 )
          ? "quotes or patterns"
          : "quote or pattern";
        write_logfile_entry("$embedded_tab_count $what had embedded tabs:\n");
        write_logfile_entry(
"This means the display of this script could vary with device or software\n"
        );
        write_logfile_entry("  $first at input line $first_embedded_tab_at\n");

        if ( $embedded_tab_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_embedded_tab_at\n");
        }
        write_logfile_entry("\n");
    }

    if ($first_tabbing_disagreement) {
        write_logfile_entry(
"First indentation disagreement seen at input line $first_tabbing_disagreement\n"
        );
    }

    if ($in_tabbing_disagreement) {
        write_logfile_entry(
"Ending with indentation disagreement which started at input line $in_tabbing_disagreement\n"
        );
    }
    else {

        if ($last_tabbing_disagreement) {

            write_logfile_entry(
"Last indentation disagreement seen at input line $last_tabbing_disagreement\n"
            );
        }
        else {
            write_logfile_entry("No indentation disagreement seen\n");
        }
    }
    write_logfile_entry("\n");

    $vertical_aligner_object->report_anything_unusual();

    $file_writer_object->report_line_length_errors();
}

sub check_options {

    # This routine is called to check the Opts hash after it is defined

    ($rOpts) = @_;
    my ( $tabbing_string, $tab_msg );

    make_static_block_comment_pattern();
    make_static_side_comment_pattern();
    make_closing_side_comment_prefix();
    make_closing_side_comment_list_pattern();

    # If closing side comments ARE selected, then we can safely
    # delete old closing side comments unless closing side comment
    # warnings are requested.  This is a good idea because it will
    # eliminate any old csc's which fall below the line count threshold.
    # We cannot do this if warnings are turned on, though, because we
    # might delete some text which has been added.  So that must
    # be handled when comments are created.
    if ( $rOpts->{'closing-side-comments'} ) {
        if ( !$rOpts->{'closing-side-comment-warnings'} ) {
            $rOpts->{'delete-closing-side-comments'} = 1;
        }
    }

    # If closing side comments ARE NOT selected, but warnings ARE
    # selected and we ARE DELETING csc's, then we will pretend to be
    # adding with a huge interval.  This will force the comments to be
    # generated for comparison with the old comments, but not added.
    elsif ( $rOpts->{'closing-side-comment-warnings'} ) {
        if ( $rOpts->{'delete-closing-side-comments'} ) {
            $rOpts->{'delete-closing-side-comments'}  = 0;
            $rOpts->{'closing-side-comments'}         = 1;
            $rOpts->{'closing-side-comment-interval'} = 100000000;
        }
    }

    make_bli_pattern();
    make_block_brace_vertical_tightness_pattern();

    if ( $rOpts->{'line-up-parentheses'} ) {

        if (   $rOpts->{'indent-only'}
            || !$rOpts->{'add-newlines'}
            || !$rOpts->{'delete-old-newlines'} )
        {
            print STDERR <<EOM;
-----------------------------------------------------------------------
Conflict: -lp  conflicts with -io, -fnl, -nanl, or -ndnl; ignoring -lp
    
The -lp indentation logic requires that perltidy be able to coordinate
arbitrarily large numbers of line breakpoints.  This isn't possible
with these flags. Sometimes an acceptable workaround is to use -wocb=3
-----------------------------------------------------------------------
EOM
            $rOpts->{'line-up-parentheses'} = 0;
        }
    }

    # At present, tabs are not compatable with the line-up-parentheses style
    # (it would be possible to entab the total leading whitespace
    # just prior to writing the line, if desired).
    if ( $rOpts->{'line-up-parentheses'} && $rOpts->{'tabs'} ) {
        print STDERR <<EOM;
Conflict: -t (tabs) cannot be used with the -lp  option; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    # Likewise, tabs are not compatable with outdenting..
    if ( $rOpts->{'outdent-keywords'} && $rOpts->{'tabs'} ) {
        print STDERR <<EOM;
Conflict: -t (tabs) cannot be used with the -okw options; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    if ( $rOpts->{'outdent-labels'} && $rOpts->{'tabs'} ) {
        print STDERR <<EOM;
Conflict: -t (tabs) cannot be used with the -ola  option; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    if ( !$rOpts->{'space-for-semicolon'} ) {
        $want_left_space{'f'} = -1;
    }

    if ( $rOpts->{'space-terminal-semicolon'} ) {
        $want_left_space{';'} = 1;
    }

    # implement outdenting preferences for keywords
    %outdent_keyword = ();

    # load defaults
    @_ = qw(next last redo goto return);

    # override defaults if requested
    if ( $rOpts->{'outdent-keyword-list'} ) {
        $rOpts->{'outdent-keyword-list'} =~ s/^\s*//;
        $rOpts->{'outdent-keyword-list'} =~ s/\s*$//;
        @_ = split /\s+/, $rOpts->{'outdent-keyword-list'};
    }

    # FUTURE: if not a keyword, assume that it is an identifier
    foreach (@_) {
        if ( $Perl::Tidy::Tokenizer::is_keyword{$_} ) {
            $outdent_keyword{$_} = 1;
        }
        else {
            print STDERR "ignoring '$_' in -okwl list; not a perl keyword";
        }
    }

    # implement user whitespace preferences
    if ( $rOpts->{'want-left-space'} ) {
        @_ = split /\s/, $rOpts->{'want-left-space'};
        @want_left_space{@_} = (1) x scalar(@_);
    }

    if ( $rOpts->{'want-right-space'} ) {
        @_ = split /\s/, $rOpts->{'want-right-space'};
        @want_right_space{@_} = (1) x scalar(@_);
    }
    if ( $rOpts->{'nowant-left-space'} ) {
        @_ = split /\s/, $rOpts->{'nowant-left-space'};
        @want_left_space{@_} = (-1) x scalar(@_);
    }

    if ( $rOpts->{'nowant-right-space'} ) {
        @_ = split /\s/, $rOpts->{'nowant-right-space'};
        @want_right_space{@_} = (-1) x scalar(@_);
    }
    if ( $rOpts->{'dump-want-left-space'} ) {
        dump_want_left_space(*STDOUT);
        exit 1;
    }

    if ( $rOpts->{'dump-want-right-space'} ) {
        dump_want_right_space(*STDOUT);
        exit 1;
    }

    # implement user break preferences
    if ( $rOpts->{'want-break-after'} ) {
        @_ = split /\s/, $rOpts->{'want-break-after'};
        foreach my $tok (@_) {
            if ( $tok eq '?' ) { $tok = ':' }    # patch to coordinate ?/:
            my $lbs = $left_bond_strength{$tok};
            my $rbs = $right_bond_strength{$tok};
            if ( defined($lbs) && defined($rbs) && $lbs < $rbs ) {
                ( $right_bond_strength{$tok}, $left_bond_strength{$tok} ) =
                  ( $lbs, $rbs );
            }
        }
    }

    if ( $rOpts->{'want-break-before'} ) {
        @_ = split /\s/, $rOpts->{'want-break-before'};
        foreach my $tok (@_) {
            my $lbs = $left_bond_strength{$tok};
            my $rbs = $right_bond_strength{$tok};
            if ( defined($lbs) && defined($rbs) && $rbs < $lbs ) {
                ( $right_bond_strength{$tok}, $left_bond_strength{$tok} ) =
                  ( $lbs, $rbs );
            }
        }
    }

    # make note if breaks are before certain key types
    %want_break_before = ();
    foreach my $tok ( '.', ',', ':', '?', '&&', '||' ) {
        $want_break_before{$tok} =
          $left_bond_strength{$tok} < $right_bond_strength{$tok};
    }

    # Coordinate ?/: breaks, which must be similar
    if ( !$want_break_before{':'} ) {
        $want_break_before{'?'}   = $want_break_before{':'};
        $right_bond_strength{'?'} = $right_bond_strength{':'} + 0.01;
        $left_bond_strength{'?'}  = NO_BREAK;
    }

    # Define here tokens which may follow the closing brace of a do statement
    # on the same line, as in:
    #   } while ( $something);
    @_ = qw(until while unless if ; );
    push @_, ',';
    @is_do_follower{@_} = (1) x scalar(@_);

    # These tokens may follow the closing brace of an if or elsif block.
    # In other words, for cuddled else we want code to look like:
    #   } elsif ( $something) {
    #   } else {
    if ( $rOpts->{'cuddled-else'} ) {
        @_ = qw(else elsif);
        @is_if_brace_follower{@_} = (1) x scalar(@_);
    }
    else {
        %is_if_brace_follower = ();
    }

    # nothing can follow the closing curly of an else { } block:
    %is_else_brace_follower = ();

    # what can follow a multi-line anonymous sub definition closing curly:
    @_ = qw# ; : => or and  && || ) #;
    push @_, ',';
    @is_anon_sub_brace_follower{@_} = (1) x scalar(@_);

    # what can follow a one-line anonynomous sub closing curly:
    # one-line anonumous subs also have ']' here...
    # see tk3.t and PP.pm
    @_ = qw#  ; : => or and  && || ) ] #;
    push @_, ',';
    @is_anon_sub_1_brace_follower{@_} = (1) x scalar(@_);

    # What can follow a closing curly of a block
    # which is not an if/elsif/else/do/sort/map/grep/eval/sub
    # Testfiles: 'Toolbar.pm', 'Menubar.pm', bless.t, '3rules.pl'
    @_ = qw#  ; : => or and  && || ) #;
    push @_, ',';

    # allow cuddled continue if cuddled else is specified
    if ( $rOpts->{'cuddled-else'} ) { push @_, 'continue'; }

    @is_other_brace_follower{@_} = (1) x scalar(@_);

    $right_bond_strength{'{'} = WEAK;
    $left_bond_strength{'{'}  = VERY_STRONG;

    # make -l=0  equal to -l=infinite
    if ( !$rOpts->{'maximum-line-length'} ) {
        $rOpts->{'maximum-line-length'} = 1000000;
    }

    # make -lbl=0  equal to -lbl=infinite
    if ( !$rOpts->{'long-block-line-count'} ) {
        $rOpts->{'long-block-line-count'} = 1000000;
    }

    # hashes used to simplify setting whitespace
    %tightness = (
        '{' => $rOpts->{'brace-tightness'},
        '}' => $rOpts->{'brace-tightness'},
        '(' => $rOpts->{'paren-tightness'},
        ')' => $rOpts->{'paren-tightness'},
        '[' => $rOpts->{'square-bracket-tightness'},
        ']' => $rOpts->{'square-bracket-tightness'},
    );
    %matching_token = (
        '{' => '}',
        '(' => ')',
        '[' => ']',
        '?' => ':',
    );

    # frequently used parameters
    $rOpts_add_newlines                   = $rOpts->{'add-newlines'};
    $rOpts_add_whitespace                 = $rOpts->{'add-whitespace'};
    $rOpts_block_brace_tightness          = $rOpts->{'block-brace-tightness'};
    $rOpts_block_brace_vertical_tightness =
      $rOpts->{'block-brace-vertical-tightness'};
    $rOpts_brace_left_and_indent   = $rOpts->{'brace-left-and-indent'};
    $rOpts_comma_arrow_breakpoints = $rOpts->{'comma-arrow-breakpoints'};
    $rOpts_break_at_old_trinary_breakpoints =
      $rOpts->{'break-at-old-trinary-breakpoints'};
    $rOpts_break_at_old_comma_breakpoints =
      $rOpts->{'break-at-old-comma-breakpoints'};
    $rOpts_break_at_old_keyword_breakpoints =
      $rOpts->{'break-at-old-keyword-breakpoints'};
    $rOpts_break_at_old_logical_breakpoints =
      $rOpts->{'break-at-old-logical-breakpoints'};
    $rOpts_closing_side_comment_else_flag =
      $rOpts->{'closing-side-comment-else-flag'};
    $rOpts_closing_side_comment_maximum_text =
      $rOpts->{'closing-side-comment-maximum-text'};
    $rOpts_continuation_indentation = $rOpts->{'continuation-indentation'};
    $rOpts_cuddled_else             = $rOpts->{'cuddled-else'};
    $rOpts_delete_old_whitespace    = $rOpts->{'delete-old-whitespace'};
    $rOpts_fuzzy_line_length        = $rOpts->{'fuzzy-line-length'};
    $rOpts_indent_columns           = $rOpts->{'indent-columns'};
    $rOpts_line_up_parentheses      = $rOpts->{'line-up-parentheses'};
    $rOpts_maximum_fields_per_table = $rOpts->{'maximum-fields-per-table'};
    $rOpts_maximum_line_length      = $rOpts->{'maximum-line-length'};
    $rOpts_short_concatenation_item_length =
      $rOpts->{'short-concatenation-item-length'};
    $rOpts_swallow_optional_blank_lines =
      $rOpts->{'swallow-optional-blank-lines'};
    $rOpts_ignore_old_line_breaks = $rOpts->{'ignore-old-line-breaks'};
    $half_maximum_line_length     = $rOpts_maximum_line_length / 2;

    # Note that both opening and closing tokens can access the opening
    # and closing flags of their container types.
    %opening_vertical_tightness = (
        '(' => $rOpts->{'paren-vertical-tightness'},
        '{' => $rOpts->{'brace-vertical-tightness'},
        '[' => $rOpts->{'square-bracket-vertical-tightness'},
        ')' => $rOpts->{'paren-vertical-tightness'},
        '}' => $rOpts->{'brace-vertical-tightness'},
        ']' => $rOpts->{'square-bracket-vertical-tightness'},
    );

    %closing_vertical_tightness = (
        '(' => $rOpts->{'paren-vertical-tightness-closing'},
        '{' => $rOpts->{'brace-vertical-tightness-closing'},
        '[' => $rOpts->{'square-bracket-vertical-tightness-closing'},
        ')' => $rOpts->{'paren-vertical-tightness-closing'},
        '}' => $rOpts->{'brace-vertical-tightness-closing'},
        ']' => $rOpts->{'square-bracket-vertical-tightness-closing'},
    );
}

sub make_static_block_comment_pattern {

    # create the pattern used to identify static block comments
    $static_block_comment_pattern = '^(\s*)##';

    # allow the user to change it
    if ( $rOpts->{'static-block-comment-prefix'} ) {
        my $prefix = $rOpts->{'static-block-comment-prefix'};
        $prefix =~ s/^\s*//;
        if ( $prefix !~ /^#/ ) {
            die "ERROR: the -sbcp prefix '$prefix' must begin with '#'\n";

        }
        my $pattern = '^(\s*)' . $prefix;
        eval "'##'=~/$pattern/";
        if ($@) {
            die
"ERROR: the -sbc prefix '$prefix' causes the invalid regex '$pattern'\n";
        }
        $static_block_comment_pattern = $pattern;
    }
}

sub make_closing_side_comment_list_pattern {

    # turn any input list into a regex for recognizing selected block types
    $closing_side_comment_list_pattern = '^\w+';
    if ( defined( $rOpts->{'closing-side-comment-list'} )
        && $rOpts->{'closing-side-comment-list'} )
    {
        $closing_side_comment_list_pattern =
          make_block_pattern( '-cscl', $rOpts->{'closing-side-comment-list'} );
    }
}

sub make_bli_pattern {

    if (
        defined(
                 $rOpts->{'brace-left-and-indent-list'}
              && $rOpts->{'brace-left-and-indent-list'}
        )
      )
    {
        $bli_list_string = $rOpts->{'brace-left-and-indent-list'};
    }

    $bli_pattern = make_block_pattern( '-blil', $bli_list_string );
}

sub make_block_brace_vertical_tightness_pattern {

    # turn any input list into a regex for recognizing selected block types
    $block_brace_vertical_tightness_pattern =
      '^((if|else|elsif|unless|while|for|foreach|do|\w+:)$|sub)';

    if (
        defined(
                 $rOpts->{'block-brace-vertical-tightness-list'}
              && $rOpts->{'block-brace-vertical-tightness-list'}
        )
      )
    {
        $block_brace_vertical_tightness_pattern =
          make_block_pattern( '-bbvtl',
            $rOpts->{'block-brace-vertical-tightness-list'} );
    }
}

sub make_block_pattern {

    #  given a string of block-type keywords, return a regex to match them
    #  The only tricky part is that labels are indicated with a single ':'
    #  and the 'sub' token text may have additional text after it (name of
    #  sub).
    # 
    #  Example:
    # 
    #   input string: "if else elsif unless while for foreach do : sub";
    #   pattern:  '^((if|else|elsif|unless|while|for|foreach|do|\w+:)$|sub)';

    my ( $abbrev, $string ) = @_;
    $string =~ s/^\s*//;
    $string =~ s/\s$//;
    my @list = split /\s+/, $string;
    my @words = ();
    my %seen;
    for my $i (@list) {
        next if $seen{$i};
        $seen{$i} = 1;
        if ( $i eq 'sub' ) {
        }
        elsif ( $i eq ':' ) {
            push @words, '\w+:';
        }
        elsif ( $i =~ /^\w/ ) {
            push @words, $i;
        }
        else {
            print STDERR "unrecognized block type $i after $abbrev, ignoring\n";
        }
    }
    my $pattern = '(' . join ( '|', @words ) . ')$';
    if ( $seen{'sub'} ) {
        $pattern = '(' . $pattern . '|sub)';
    }
    $pattern = '^' . $pattern;
    return $pattern;
}

sub make_static_side_comment_pattern {

    # create the pattern used to identify static side comments
    $static_side_comment_pattern = '^##';

    # allow the user to change it
    if ( $rOpts->{'static-side-comment-prefix'} ) {
        my $prefix = $rOpts->{'static-side-comment-prefix'};
        $prefix =~ s/^\s*//;
        my $pattern = '^' . $prefix;
        eval "'##'=~/$pattern/";
        if ($@) {
            die
"ERROR: the -sscp prefix '$prefix' causes the invalid regex '$pattern'\n";
        }
        $static_side_comment_pattern = $pattern;
    }
}

sub make_closing_side_comment_prefix {

    # Be sure we have a valid closing side comment prefix
    my $csc_prefix = $rOpts->{'closing-side-comment-prefix'};
    my $csc_prefix_pattern;
    if ( !defined($csc_prefix) ) {
        $csc_prefix         = '## end';
        $csc_prefix_pattern = '^##\s+end';
    }
    else {
        my $test_csc_prefix = $csc_prefix;
        if ( $test_csc_prefix !~ /^#/ ) {
            $test_csc_prefix = '#' . $test_csc_prefix;
        }

        # make a regex to recognize the prefix
        my $test_csc_prefix_pattern = $test_csc_prefix;

        # escape any special characters
        $test_csc_prefix_pattern =~ s/([^#\s\w])/\\$1/g;

        $test_csc_prefix_pattern = '^' . $test_csc_prefix_pattern;

        # allow exact number of intermediate spaces to vary
        $test_csc_prefix_pattern =~ s/\s+/\\s\+/g;

        # make sure we have a good pattern
        # if we fail this we probably have an error in escaping
        # characters.
        eval "'##'=~/$test_csc_prefix_pattern/";
        if ($@) {

            # shouldn't happen..must have screwed up escaping, above
            report_definite_bug();
            print STDERR
"Program Error: the -cscp prefix '$csc_prefix' caused the invalid regex '$csc_prefix_pattern'\n";

            # just warn and keep going with defaults
            print STDERR "Please consider using a simpler -cscp prefix\n";
            print STDERR "Using default -cscp instead; please check output\n";
        }
        else {
            $csc_prefix         = $test_csc_prefix;
            $csc_prefix_pattern = $test_csc_prefix_pattern;
        }
    }
    $rOpts->{'closing-side-comment-prefix'} = $csc_prefix;
    $closing_side_comment_prefix_pattern = $csc_prefix_pattern;
}

sub dump_want_left_space {
    my $fh = shift;
    local $" = "\n";
    print $fh <<EOM;
These values are the main control of whitespace to the left of a token type;
They may be altered with the -wls parameter.
For a list of token types, use perltidy --dump-token-types (-dtt)
 1 means the token wants a space to its left
-1 means the token does not want a space to its left
------------------------------------------------------------------------
EOM
    foreach ( sort keys %want_left_space ) {
        print $fh "$_\t$want_left_space{$_}\n";
    }
}

sub dump_want_right_space {
    my $fh = shift;
    local $" = "\n";
    print $fh <<EOM;
These values are the main control of whitespace to the right of a token type;
They may be altered with the -wrs parameter.
For a list of token types, use perltidy --dump-token-types (-dtt)
 1 means the token wants a space to its right
-1 means the token does not want a space to its right
------------------------------------------------------------------------
EOM
    foreach ( sort keys %want_right_space ) {
        print $fh "$_\t$want_right_space{$_}\n";
    }
}

{    # begin is_essential_whitespace

    my %is_sort_grep_map;
    my %is_for_foreach;

    BEGIN {

        @_ = qw(sort grep map);
        @is_sort_grep_map{@_} = (1) x scalar(@_);

        @_ = qw(for foreach);
        @is_for_foreach{@_} = (1) x scalar(@_);

    }

    sub is_essential_whitespace {

        # Essential whitespace means whitespace which cannot be safely deleted.
        # We are given three tokens and their types:
        # ($tokenl, $typel) is the token to the left of the space in question
        # ($tokenr, $typer) is the token to the right of the space in question
        # ($tokenll, $typell) is previous nonblank token to the left of $tokenl
        #
        # This is a slow routine but is not needed too often except when -mangle
        # is used.
        my ( $tokenll, $typell, $tokenl, $typel, $tokenr, $typer ) = @_;

        # never combine two bare words or numbers
        my $result = ( ( $tokenr =~ /^[\'\w]/ ) && ( $tokenl =~ /[\'\w]$/ ) )

          # do not combine a number with a concatination dot
          # example: pom.caputo:
          # $vt100_compatible ? "\e[0;0H" : ('-' x 78 . "\n");
          || ( ( $typel eq 'n' ) && ( $tokenr eq '.' ) )
          || ( ( $typer eq 'n' ) && ( $tokenl eq '.' ) )

          # do not join a minus with a bare word, because you might form
          # a file test operator.  Example from Complex.pm:
          # if (CORE::abs($z - i) < $eps); "z-i" would be taken as a file test.
          || ( ( $tokenl eq '-' ) && ( $tokenr =~ /^[_A-Za-z]$/ ) )

          # and something like this could become ambiguous without space
          # after the '-':
          #   use constant III=>1;
          #   $a = $b - III;
          # and even this:
          #   $a = - III;
          || ( ( $tokenl eq '-' )
            && ( $typer =~ /^[wC]$/ && $tokenr =~ /^[_A-Za-z]/ ) )

          # '= -' should not become =- or you will get a warning
          # about reversed -=
          # || ($tokenr eq '-')

          # keep a space between a quote and a bareword to prevent the
          # bareword from becomming a quote modifier.
          || ( ( $typel eq 'Q' ) && ( $tokenr =~ /^[a-zA-Z_]/ ) )

          # keep a space between a token ending in '$' and any word;
          # this caused trouble:  "die @$ if $@"
          || ( ( $typel eq 'i' && $tokenl =~ /\$$/ )
            && ( $tokenr =~ /^[a-zA-Z_]/ ) )

          # perl is very fussy about spaces before <<
          || ( $tokenr =~ /^\<\</ )

          # avoid combining tokens to create new meanings. Example:
          #     $a+ +$b must not become $a++$b
          || ( $is_digraph{ $tokenl . $tokenr } )
          || ( $is_trigraph{ $tokenl . $tokenr } )

          # another example: do not combine these two &'s:
          #     allow_options & &OPT_EXECCGI
          || ( $is_digraph{ $tokenl . substr( $tokenr, 0, 1 ) } )

          # don't combine $$ or $# with any alphanumeric
          # (testfile mangle.t with --mangle)
          || ( ( $tokenl =~ /^\$[\$\#]$/ ) && ( $tokenr =~ /^\w/ ) )

          # retain any space after possible filehandle
          # (testfiles prnterr1.t with --extrude and mangle.t with --mangle)
          || ( $typel eq 'Z' || $typell eq 'Z' )

          # keep paren separate in 'use Foo::Bar ()'
          || ( $tokenr eq '('
            && $typel   eq 'w'
            && $typell  eq 'k'
            && $tokenll eq 'use' )

          # keep any space between filehandle and paren:
          # file mangle.t with --mangle:
          || ( $typel eq 'Y' && $tokenr eq '(' )

          # retain any space after here doc operator ( hereerr.t)
          || ( $typel eq 'h' )

          # FIXME: this needs some further work; extrude.t has test cases
          # it is safest to retain any space after start of ? : operator
          # because of perl's quirky parser. 
          # ie, this line will fail if you remove the space after the '?':
          #    $b=join $comma ? ',' : ':', @_;   # ok
          #    $b=join $comma ?',' : ':', @_;   # error!
          # but this is ok :)
          #    $b=join $comma?',' : ':', @_;   # not a problem!
          ## || ($typel eq '?')

          # be careful with a space around ++ and --, to avoid ambiguity as to
          # which token it applies
          || ( ( $typer =~ /^(pp|mm)$/ )     && ( $tokenl !~ /^[\;\{\(\[]/ ) )
          || ( ( $typel =~ /^(\+\+|\-\-)$/ ) && ( $tokenr !~ /^[\;\}\)\]]/ ) )

          # need space after foreach my; for example, this will fail in
          # older versions of Perl:
          # foreach my$ft(@filetypes)...
          || (
            $tokenl eq 'my'

            #  /^(for|foreach)$/
            && $is_for_foreach{$tokenll} && $tokenr =~ /^\$/
          )

          # must have space between grep and left paren; "grep(" will fail
          #                       /^(sort|grep|map)$/ 
          || ( $tokenr eq '(' && $is_sort_grep_map{$tokenl} )

          # don't stick numbers next to left parens, as in:
          #use Mail::Internet 1.28 (); (see Entity.pm, Head.pm, Test.pm)
          || ( ( $typel eq 'n' ) && ( $tokenr eq '(' ) )

          # don't join something like: for bla::bla:: abc
          # example is "%overload:: and" in files Dumpvalue.pm or colonbug.pl
          || ( $tokenl =~ /\:\:$/ && ( $tokenr =~ /^[\'\w]/ ) )
          ;    # the value of this long logic sequence is the result we want
        return $result;
    }
}

sub set_white_space_flag {

    #    This routine examines each pair of nonblank tokens and
    #    sets values for array @white_space_flag. 
    # 
    #    $white_space_flag[$j] is a flag indicating whether a white space 
    #    BEFORE token $j is needed, with the following values:
    # 
    #            -1 do not want a space before token $j
    #             0 optional space or $j is a whitespace
    #             1 want a space before token $j
    # 
    # 
    #   The values for the first token will be defined based
    #   upon the contents of the "to_go" output array.  
    # 
    #   Note: retain debug print statements because they are usually
    #   required after adding new token types.

    BEGIN {

        # initialize these global hashes, which control the use of
        # whitespace around tokens:
        #
        # %binary_ws_rules
        # %want_left_space
        # %want_right_space
        # %space_before_paren
        #
        # Many token types are identical to the tokens themselves.
        # See the tokenizer for a complete list. Here are some special types:
        #   k = perl keyword
        #   f = semicolon in for statement
        #   m = unary minus
        #   p = unary plus
        # Note that :: is excluded since it should be contained in an identifier
        # Note that '->' is excluded because it never gets space
        # parentheses and brackets are excluded since they are handled specially
        # curly braces are included but may be overridden by logic, such as
        # newline logic.

        # NEW_TOKENS: create a whitespace rule here.  This can be as
        # simple as adding your new letter to @spaces_both_sides, for
        # example.

        @_ = qw" L { ( [ ";
        @is_opening_type{@_} = (1) x scalar(@_);

        @_ = qw" R } ) ] ";
        @is_closing_type{@_} = (1) x scalar(@_);

        my @spaces_both_sides = qw"
          + - * / % ? = . : x < > | & ^ .. << >> ** && .. ||  => += -=
          .= %= x= &= |= ^= *= <> <= >= == =~ !~ /= != ... <<= >>=
          &&= ||= <=> k f w F n C Y U G v
          ";

        my @spaces_left_side = qw"
          t ! ~ m p { \ h pp mm Z j
          ";
        push ( @spaces_left_side, '#' );    # avoids warning message

        my @spaces_right_side = qw"
          ; } ) ] R J ++ -- **=
          ";
        push ( @spaces_right_side, ',' );    # avoids warning message
        my @space_before_paren = qw(
          my local and or eq ne if else elsif until unless while
          for foreach push return shift unshift pop join split die
        );
        @want_left_space{@spaces_both_sides} = (1) x scalar(@spaces_both_sides);
        @want_right_space{@spaces_both_sides} =
          (1) x scalar(@spaces_both_sides);
        @want_left_space{@spaces_left_side}  = (1) x scalar(@spaces_left_side);
        @want_right_space{@spaces_left_side} = (-1) x scalar(@spaces_left_side);
        @want_left_space{@spaces_right_side} =
          (-1) x scalar(@spaces_right_side);
        @want_right_space{@spaces_right_side} =
          (1) x scalar(@spaces_right_side);
        @space_before_paren{@space_before_paren} =
          (1) x scalar(@space_before_paren);
        $want_left_space{'L'}   = WS_NO;
        $want_left_space{'->'}  = WS_NO;
        $want_right_space{'->'} = WS_NO;
        $want_left_space{'**'}  = WS_NO;
        $want_right_space{'**'} = WS_NO;

        # hash type information must stay tightly bound
        # as in :  ${xxxx}
        $binary_ws_rules{'i'}{'L'} = WS_NO;
        $binary_ws_rules{'i'}{'{'} = WS_YES;
        $binary_ws_rules{'k'}{'{'} = WS_YES;
        $binary_ws_rules{'U'}{'{'} = WS_YES;
        $binary_ws_rules{'i'}{'['} = WS_NO;
        $binary_ws_rules{'R'}{'L'} = WS_NO;
        $binary_ws_rules{'R'}{'{'} = WS_NO;
        $binary_ws_rules{'t'}{'L'} = WS_NO;
        $binary_ws_rules{'t'}{'{'} = WS_NO;
        $binary_ws_rules{'}'}{'L'} = WS_NO;
        $binary_ws_rules{'}'}{'{'} = WS_NO;
        $binary_ws_rules{'$'}{'L'} = WS_NO;
        $binary_ws_rules{'$'}{'{'} = WS_NO;
        $binary_ws_rules{'@'}{'L'} = WS_NO;
        $binary_ws_rules{'@'}{'{'} = WS_NO;
        $binary_ws_rules{'='}{'L'} = WS_YES;

        # the following includes ') {'
        # as in :    if ( xxx ) { yyy }
        $binary_ws_rules{']'}{'L'} = WS_NO;
        $binary_ws_rules{']'}{'{'} = WS_NO;
        $binary_ws_rules{')'}{'{'} = WS_YES;
        $binary_ws_rules{')'}{'['} = WS_NO;
        $binary_ws_rules{']'}{'['} = WS_NO;
        $binary_ws_rules{']'}{'{'} = WS_NO;
        $binary_ws_rules{'}'}{'['} = WS_NO;
        $binary_ws_rules{'R'}{'['} = WS_NO;

        $binary_ws_rules{']'}{'++'} = WS_NO;
        $binary_ws_rules{']'}{'--'} = WS_NO;
        $binary_ws_rules{')'}{'++'} = WS_NO;
        $binary_ws_rules{')'}{'--'} = WS_NO;

        $binary_ws_rules{'R'}{'++'} = WS_NO;
        $binary_ws_rules{'R'}{'--'} = WS_NO;

        $binary_ws_rules{'k'}{':'} = WS_NO;     # keep colon with label
        $binary_ws_rules{'w'}{':'} = WS_NO;
        $binary_ws_rules{'i'}{'Q'} = WS_YES;
        $binary_ws_rules{'n'}{'('} = WS_YES;    # occurs in 'use package n ()'

        # FIXME: we need to split 'i' into variables and functions
        # and have no space for functions but space for variables.  For now,
        # I have a special patch in the special rules below 
        $binary_ws_rules{'i'}{'('} = WS_NO;

        $binary_ws_rules{'w'}{'('} = WS_NO;
        $binary_ws_rules{'w'}{'{'} = WS_YES;
    }
    my ( $jmax, $rtokens, $rtoken_type, $rblock_type ) = @_;
    my ( $last_token, $last_type, $last_block_type, $token, $type,
        $block_type );
    my (@white_space_flag);
    my $j_tight_closing_paren = -1;

    if ( $max_index_to_go >= 0 ) {
        $token      = $tokens_to_go[$max_index_to_go];
        $type       = $types_to_go[$max_index_to_go];
        $block_type = $block_type_to_go[$max_index_to_go];
    }
    else {
        $token      = ' ';
        $type       = 'b';
        $block_type = '';
    }

    # loop over all tokens
    my ( $j, $ws );

    for ( $j = 0 ; $j <= $jmax ; $j++ ) {

        if ( $$rtoken_type[$j] eq 'b' ) {
            $white_space_flag[$j] = WS_OPTIONAL;
            next;
        }

        # set a default value, to be changed as needed
        $ws              = undef;
        $last_token      = $token;
        $last_type       = $type;
        $last_block_type = $block_type;
        $token           = $$rtokens[$j];
        $type            = $$rtoken_type[$j];
        $block_type      = $$rblock_type[$j];

        #---------------------------------------------------------------
        # section 1:
        # handle space on the inside of opening braces
        #---------------------------------------------------------------

        #    /^[L\{\(\[]$/ 
        if ( $is_opening_type{$last_type} ) {

            $j_tight_closing_paren = -1;

            # let's keep empty matched braces together: () {} []
            # except for BLOCKS
            if ( $token eq $matching_token{$last_token} ) {
                if ($block_type) {
                    $ws = WS_YES;
                }
                else {
                    $ws = WS_NO;
                }
            }
            else {

                # we're considering the right of an opening brace
                # tightness = 0 means always pad inside with space
                # tightness = 1 means pad inside if "complex"
                # tightness = 2 means never pad inside with space

                my $tightness;
                if (   $last_type eq '{'
                    && $last_token eq '{'
                    && $last_block_type )
                {
                    $tightness = $rOpts_block_brace_tightness;
                }
                else { $tightness = $tightness{$last_token} }

                if ( $tightness <= 0 ) {
                    $ws = WS_YES;
                }
                elsif ( $tightness > 1 ) {
                    $ws = WS_NO;
                }
                else {
                    my $j_next =
                      ( $$rtoken_type[ $j + 1 ] eq 'b' ) ? $j + 2 : $j + 1;
                    my $tok_next  = $$rtokens[$j_next];
                    my $type_next = $$rtoken_type[$j_next];

                    # for tightness = 1, if there is just one token
                    # within the matching pair, we will keep it tight
                    if (
                        $tok_next eq $matching_token{$last_token}

                        # but watch out for this: [ [ ]    (misc.t)
                        && $last_token ne $token
                      )
                    {

                        # remember where to put the space for the closing paren
                        $j_tight_closing_paren = $j_next;
                        $ws                    = WS_NO;
                    }
                    else {
                        $ws = WS_YES;
                    }
                }
            }
        }    # done with opening braces and brackets
        my $ws_1 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 2:
        # handle space on inside of closing brace pairs
        #---------------------------------------------------------------

        #   /[\}\)\]R]/ 
        if ( $is_closing_type{$type} ) {

            if ( $j == $j_tight_closing_paren ) {

                $j_tight_closing_paren = -1;
                $ws                    = WS_NO;
            }
            else {

                if ( !defined($ws) ) {

                    my $tightness;
                    if ( $type eq '}' && $token eq '}' && $block_type ) {
                        $tightness = $rOpts_block_brace_tightness;
                    }
                    else { $tightness = $tightness{$token} }

                    $ws = ( $tightness > 1 ) ? WS_NO : WS_YES;
                }
            }
        }

        my $ws_2 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 3:
        # use the binary table
        #---------------------------------------------------------------
        if ( !defined($ws) ) {
            $ws = $binary_ws_rules{$last_type}{$type};
        }
        my $ws_3 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 4:
        # some special cases
        #---------------------------------------------------------------
        if ( $token eq '(' ) {

            # This will have to be tweaked as tokenization changes.
            # We want a space after certain block types:
            #     map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );
            #
            # But not others:
            #     &{ $_->[1] } ( delete $_[$#_]{ $_->[0] } );
            # At present, the & block is not marked as a code block, so
            # this works:
            if ( $last_type eq '}' ) {

                # /^(sort|map|grep)$/ 
                if ( $is_sort_map_grep{$last_block_type} ) {
                    $ws = WS_YES;
                }
                else {
                    $ws = WS_NO;
                }
            }

            # -----------------------------------------------------
            # 'w' and 'i' checks for something like:
            #   myfun(    &myfun(   ->myfun(
            # -----------------------------------------------------
            if (   ( $last_type =~ /^[wkU]$/ )
                || ( $last_type eq 'i' && $last_token =~ /^(\&|->)/ ) )
            {

                # Do not introduce new space between keyword or function
                # and ( except in special cases) because this can
                # introduce errors in some cases ( prnterr1.t )
                unless ( $space_before_paren{$last_token} ) {
                    $ws = WS_NO;
                }
            }

            # space between something like $i and ( in
            # for $i ( 0 .. 20 ) {
            # FIXME: eventually, type 'i' needs to be split into multiple
            # token types so this can be a hardwired rule.
            elsif ( $last_type eq 'i' && $last_token =~ /^[\$\%\@]/ ) {
                $ws = WS_YES;
            }

            # allow constant function followed by '()' to retain no space
            elsif ( $last_type eq 'C' && $$rtokens[ $j + 1 ] eq ')' ) {
                ;
                $ws = WS_NO;
            }
        }

        # keep space between 'sub' and '{' for anonymous sub definition
        if ( $type eq '{' ) {
            if ( $last_token eq 'sub' ) {
                $ws = WS_YES;
            }

            # this is needed to avoid no space in '){'
            if ( $last_token eq ')' && $token eq '{' ) { $ws = WS_YES }

            # avoid any space before the brace or bracket in something like
            #  @opts{'a','b',...}
            if ( $last_type eq 'i' && $last_token =~ /^\@/ ) {
                $ws = WS_NO;
            }
        }

        elsif ( $type eq 'i' ) {

            # never a space before ->
            if ( $token =~ /^\-\>/ ) {
                $ws = WS_NO;
            }
        }

        # retain any space between '-' and bare word
        elsif ( $type eq 'w' || $type eq 'C' ) {
            $ws = WS_OPTIONAL if $last_type eq '-';
        }

        # retain any space between '-' and bare word
        # example: avoid space between 'USER' and '-' here:
        #   $myhash{USER-NAME}='steve'; 
        elsif ( $type eq 'm' || $type eq '-' ) {
            $ws = WS_OPTIONAL if ( $last_type eq 'w' );
        }

        # always space before side comment
        elsif ( $type eq '#' ) { $ws = WS_YES if $j > 0 }

        # always preserver whatever space was used after a possible
        # filehandle or here doc operator
        if ( $type ne '#' && ( $last_type eq 'Z' || $last_type eq 'h' ) ) {
            $ws = WS_OPTIONAL;
        }

        my $ws_4 = $ws
          if FORMATTER_DEBUG_FLAG_WHITE;

        #---------------------------------------------------------------
        # section 5:
        # default rules not covered above
        #---------------------------------------------------------------
        # if we fall through to here,
        # look at the pre-defined hash tables for the two tokens, and
        # if (they are equal) use the common value
        # if (either is zero or undef) use the other
        # if (either is -1) use it
        # That is,
        # left  vs right
        #  1    vs    1     -->  1
        #  0    vs    0     -->  0
        # -1    vs   -1     --> -1
        #
        #  0    vs   -1     --> -1
        #  0    vs    1     -->  1
        #  1    vs    0     -->  1
        # -1    vs    0     --> -1
        #
        # -1    vs    1     --> -1
        #  1    vs   -1     --> -1
        if ( !defined($ws) ) {
            my $wl = $want_left_space{$type};
            my $wr = $want_right_space{$last_type};
            if ( !defined($wl) ) { $wl = 0 }
            if ( !defined($wr) ) { $wr = 0 }
            $ws = ( ( $wl == $wr ) || ( $wl == -1 ) || !$wr ) ? $wl : $wr;
        }

        if ( !defined($ws) ) {
            $ws = 0;
            write_diagnostics(
                "WS flag is undefined for tokens $last_token $token\n");
        }

        # Treat newline as a whitespace. Otherwise, we might combine
        # 'Send' and '-recipients' here according to the above rules:
        #    my $msg = new Fax::Send
        #      -recipients => $to,
        #      -data => $data;
        if ( $ws == 0 && $j == 0 ) { $ws = 1 }

        if (   ( $ws == 0 )
            && $j > 0
            && $j < $jmax
            && ( $last_type !~ /^[Zh]$/ ) )
        {

            # If this happens, we have a non-fatal but undesirable 
            # hole in the above rules which should be patched.
            write_diagnostics(
                "WS flag is zero for tokens $last_token $token\n");
        }
        $white_space_flag[$j] = $ws;

        FORMATTER_DEBUG_FLAG_WHITE && do {
            my $str = substr( $last_token, 0, 15 );
            $str .= ' ' x ( 16 - length($str) );
            if ( !defined($ws_1) ) { $ws_1 = "*" }
            if ( !defined($ws_2) ) { $ws_2 = "*" }
            if ( !defined($ws_3) ) { $ws_3 = "*" }
            if ( !defined($ws_4) ) { $ws_4 = "*" }
            print
"WHITE:  i=$j $str $last_type $type $ws_1 : $ws_2 : $ws_3 : $ws_4 : $ws \n";
        };
    }
    return \@white_space_flag;
}

{    # begin print_line_of_tokens

    my $rtoken_type;
    my $rtokens;
    my $rlevels;
    my $rslevels;
    my $rblock_type;
    my $rcontainer_type;
    my $rcontainer_environment;
    my $rtype_sequence;
    my $input_line;
    my $rnesting_tokens;
    my $rci_levels;
    my $rnesting_blocks;

    my $in_quote;
    my $python_indentation_level;

    # These local token variables are stored by store_token_to_go:
    my $block_type;
    my $ci_level;
    my $container_environment;
    my $container_type;
    my $in_continued_quote;
    my $level;
    my $nesting_blocks;
    my $no_internal_newlines;
    my $slevel;
    my $token;
    my $type;
    my $type_sequence;

    # routine to pull the jth token from the line of tokens
    sub extract_token {
        my $j = shift;
        $token                 = $$rtokens[$j];
        $type                  = $$rtoken_type[$j];
        $block_type            = $$rblock_type[$j];
        $container_type        = $$rcontainer_type[$j];
        $container_environment = $$rcontainer_environment[$j];
        $type_sequence         = $$rtype_sequence[$j];
        $level                 = $$rlevels[$j];
        $slevel                = $$rslevels[$j];
        $nesting_blocks        = $$rnesting_blocks[$j];
        $ci_level              = $$rci_levels[$j];
    }

    {
        my @saved_token;

        sub save_current_token {

            @saved_token = (
                $block_type,            $ci_level,
                $container_environment, $container_type,
                $in_continued_quote,    $level,
                $nesting_blocks,        $no_internal_newlines,
                $slevel,                $token,
                $type,                  $type_sequence,
            );
        }

        sub restore_current_token {
            (
                $block_type,            $ci_level,
                $container_environment, $container_type,
                $in_continued_quote,    $level,
                $nesting_blocks,        $no_internal_newlines,
                $slevel,                $token,
                $type,                  $type_sequence,
              )
              = @saved_token;
        }
    }

    # Routine to place the current token into the output stream.
    # Called once per output token.
    sub store_token_to_go {

        my $flag = $no_internal_newlines;
        if ( $_[0] ) { $flag = 1 }

        $tokens_to_go[ ++$max_index_to_go ] = $token;
        $types_to_go[$max_index_to_go]                 = $type;
        $nobreak_to_go[$max_index_to_go]               = $flag;
        $old_breakpoint_to_go[$max_index_to_go]        = 0;
        $forced_breakpoint_to_go[$max_index_to_go]     = 0;
        $block_type_to_go[$max_index_to_go]            = $block_type;
        $type_sequence_to_go[$max_index_to_go]         = $type_sequence;
        $container_environment_to_go[$max_index_to_go] = $container_environment;
        $nesting_blocks_to_go[$max_index_to_go]        = $nesting_blocks;
        $ci_levels_to_go[$max_index_to_go]             = $ci_level;
        $mate_index_to_go[$max_index_to_go]            = -1;
        $matching_token_to_go[$max_index_to_go]        = '';

        $levels_to_go[$max_index_to_go] = $level;
        $nesting_depth_to_go[$max_index_to_go] = ( $slevel >= 0 ) ? $slevel : 0;
        $lengths_to_go[ $max_index_to_go + 1 ] =
          $lengths_to_go[$max_index_to_go] + length($token);

        # Define the indentation that this token would have if it started
        # a new line.  We have to do this now because we need to know this
        # when considering one-line blocks.
        set_leading_whitespace( $level, $ci_level, $in_continued_quote );

        if ( $type ne 'b' ) {
            $last_last_nonblank_index_to_go = $last_nonblank_index_to_go;
            $last_last_nonblank_type_to_go  = $last_nonblank_type_to_go;
            $last_last_nonblank_token_to_go = $last_nonblank_token_to_go;
            $last_nonblank_index_to_go      = $max_index_to_go;
            $last_nonblank_type_to_go       = $type;
            $last_nonblank_token_to_go      = $token;
            if ( $type eq ',' ) {
                $comma_count_in_batch++;
            }
        }

        FORMATTER_DEBUG_FLAG_STORE && do {
            my ( $a, $b, $c ) = caller();
            print
"STORE: from $a $c: storing token $token type $type lev=$level slev=$slevel at $max_index_to_go\n";
        };
    }

    sub insert_new_token_to_go {

        # insert a new token into the output stream.  use same level as
        # previous token; assumes a character at max_index_to_go.
        save_current_token();
        ( $token, $type, $slevel, $no_internal_newlines ) = @_;

        if ( $max_index_to_go == UNDEFINED_INDEX ) {
            warning("code bug: bad call to insert_new_token_to_go\n");
        }
        $level = $levels_to_go[$max_index_to_go];

        # FIXME: it seems to be necessary to use the next, rather than
        # previous, value of this variable when creating a new blank (align.t)
        #my $slevel         = $nesting_depth_to_go[$max_index_to_go];
        $nesting_blocks        = $nesting_blocks_to_go[$max_index_to_go];
        $ci_level              = $ci_levels_to_go[$max_index_to_go];
        $container_environment = $container_environment_to_go[$max_index_to_go];
        $in_continued_quote    = 0;
        $block_type            = "";
        $type_sequence         = "";
        store_token_to_go();
        restore_current_token();
        return;
    }

    my %is_until_while_for_if_elsif_else;

    BEGIN {

        # always break after a closing curly of these block types:
        @_ = qw(until while for if elsif else);
        @is_until_while_for_if_elsif_else{@_} = (1) x scalar(@_);

    }

    sub print_line_of_tokens {

        my $line_of_tokens = shift;

        # This routine is called once per input line to process all of
        # the tokens on that line.  This is the first stage of
        # beautification.
        #
        # Full-line comments and blank lines may be processed immediately.
        #
        # For normal lines of code, the tokens are stored one-by-one,
        # via calls to 'sub store_token_to_go', until a known line break
        # point is reached.  Then, the batch of collected tokens is
        # passed along to 'sub output_line_to_go' for further
        # processing.  This routine decides if there should be
        # whitespace between each pair of non-white tokens, so later
        # routines only need to decide on any additional line breaks.
        # Any whitespace is initally a single space character.  Later,
        # the vertical aligner may expand that to be multiple space
        # characters if necessary for alignment.

        # extract input line number for error messages
        $input_line_number = $line_of_tokens->{_line_number};

        $rtoken_type            = $line_of_tokens->{_rtoken_type};
        $rtokens                = $line_of_tokens->{_rtokens};
        $rlevels                = $line_of_tokens->{_rlevels};
        $rslevels               = $line_of_tokens->{_rslevels};
        $rblock_type            = $line_of_tokens->{_rblock_type};
        $rcontainer_type        = $line_of_tokens->{_rcontainer_type};
        $rcontainer_environment = $line_of_tokens->{_rcontainer_environment};
        $rtype_sequence         = $line_of_tokens->{_rtype_sequence};
        $input_line             = $line_of_tokens->{_line_text};
        $rnesting_tokens        = $line_of_tokens->{_rnesting_tokens};
        $rci_levels             = $line_of_tokens->{_rci_levels};
        $rnesting_blocks        = $line_of_tokens->{_rnesting_blocks};

        $in_continued_quote       = $line_of_tokens->{_starting_in_quote};
        $in_quote                 = $line_of_tokens->{_ending_in_quote};
        $python_indentation_level =
          $line_of_tokens->{_python_indentation_level};

        my $j;
        my $j_next;
        my $jmax;
        my $next_nonblank_token;
        my $next_nonblank_token_type;
        my $rwhite_space_flag;

        $jmax                  = @$rtokens - 1;
        $block_type            = "";
        $container_type        = "";
        $container_environment = "";
        $type_sequence         = "";
        $no_internal_newlines  = 1 - $rOpts_add_newlines;

        # Handle a continued quote..
        if ($in_continued_quote) {

            # A line which is entirely a quote or pattern must go out
            # verbatim.  Note: the \n is contained in $input_line.
            if ( $jmax <= 0 ) {
                if ( ( $input_line =~ "\t" ) ) {
                    note_embedded_tab();
                }
                write_unindented_line("$input_line");
                $last_line_had_side_comment = 0;
                return;
            }

            # prior to version 20010406, perltidy had a bug which placed
            # continuation indentation before the last line of some multiline 
            # quotes and patterns -- exactly the lines passing this way.
            # To help find affected lines in scripts run with these
            # versions, run with '-chk', and it will warn of any quotes or
            # patterns which might have been modified by these early
            # versions.
            if ( $rOpts->{'check-multiline-quotes'} && $input_line =~ /^ / ) {
                warning(
"-chk: please check this line for extra leading whitespace\n"
                );
            }
        }

        # delete trailing blank tokens
        if ( $jmax > 0 && $$rtoken_type[$jmax] eq 'b' ) { $jmax-- }

        # Handle a blank line..
        if ( $jmax < 0 ) {

            # For the 'swallow-optional-blank-lines' option, we delete all
            # old blank lines and let the blank line rules generate any
            # needed blanks.
            if ( !$rOpts_swallow_optional_blank_lines ) {
                flush();
                $file_writer_object->write_blank_code_line();
                $last_line_leading_type = 'b';
            }
            $last_line_had_side_comment = 0;
            return;
        }

        # see if this is a static block comment (starts with ##)
        my $is_static_block_comment                       = 0;
        my $is_static_block_comment_without_leading_space = 0;
        if (   $jmax == 0
            && $$rtoken_type[0] eq '#'
            && $rOpts->{'static-block-comments'}
            && $input_line =~ /$static_block_comment_pattern/o )
        {
            $is_static_block_comment                       = 1;
            $is_static_block_comment_without_leading_space =
              ( length($1) <= 0 );
        }

        # create a hanging side comment if appropriate
        if (
               $jmax == 0
            && $$rtoken_type[0] eq '#'    # only token is a comment
            && $last_line_had_side_comment    # last line had side comment
            && $input_line =~ /^\s/           # there is some leading space
            && !$is_static_block_comment    # do not make static comment hanging
            && $rOpts->{'hanging-side-comments'}    # user is allowing this
          )
        {

            # We will insert an empty qw string at the start of the token list
            # to force this comment to be a side comment. The vertical aligner
            # should then line it up with the previous side comment.
            unshift @$rtoken_type,            'q';
            unshift @$rtokens,                '';
            unshift @$rlevels,                $$rlevels[0];
            unshift @$rslevels,               $$rslevels[0];
            unshift @$rblock_type,            '';
            unshift @$rcontainer_type,        '';
            unshift @$rcontainer_environment, '';
            unshift @$rtype_sequence,         '';
            unshift @$rnesting_tokens,        $$rnesting_tokens[0];
            unshift @$rci_levels,             $$rci_levels[0];
            unshift @$rnesting_blocks,        $$rnesting_blocks[0];
            $jmax = 1;
        }

        # remember if this line has a side comment
        $last_line_had_side_comment =
          ( $jmax > 0 && $$rtoken_type[$jmax] eq '#' );

        # Handle a block (full-line) comment..
        if ( ( $jmax == 0 ) && ( $$rtoken_type[0] eq '#' ) ) {

            if ( $rOpts->{'delete-block-comments'} ) { return }

            if ( $rOpts->{'tee-block-comments'} ) {
                $file_writer_object->tee_on();
            }
            flush();

            # output a blank line before block comments
            if (
                   $last_line_leading_type !~ /^[#b]$/
                && $rOpts->{'blanks-before-comments'}    # only if allowed
                && !
                $is_static_block_comment    # never before static block comments
              )
            {
                $file_writer_object->write_blank_code_line();
                $last_line_leading_type = 'b';
            }

            if (
                $rOpts->{'indent-block-comments'}
                && ( !$rOpts->{'indent-spaced-block-comments'}
                    || $input_line =~ /^\s+/ )
                && !$is_static_block_comment_without_leading_space
              )
            {
                extract_token(0);
                store_token_to_go();
                flush();
            }
            else {
                $file_writer_object->write_code_line( $$rtokens[0] . "\n" );
                $last_line_leading_type = '#';
            }
            if ( $rOpts->{'tee-block-comments'} ) {
                $file_writer_object->tee_off();
            }
            return;
        }

        # compare input/output indentation except for continuation lines
        # (because they have an unknown amount of initial blank space)
        # and lines which are quotes (because they may have been outdented)
        # Note: this test is placed here because we know the continuation flag
        # at this point, which allows us to avoid non-meaningful checks.
        my $structural_indentation_level = $$rlevels[0];
        compare_indentation_levels( $python_indentation_level,
            $structural_indentation_level )
          unless ( $python_indentation_level < 0
            || ( $$rci_levels[0] > 0 )
            || ( ( $python_indentation_level == 0 ) && $$rtoken_type[0] eq 'Q' )
          );

        #   Patch needed for MakeMaker.  Do not break a statement
        #   in which $VERSION may be calculated.  See MakeMaker.pm;
        #   this is based on the coding in it.
        #   The first line of a file that matches this will be eval'd:
        #       /([\$*])(([\w\:\']*)\bVERSION)\b.*\=/
        #   Examples:
        #     *VERSION = \'1.01';
        #     ( $VERSION ) = '$Revision: 1.28 $ ' =~ /\$Revision:\s+([^\s]+)/;
        #   We will pass such a line straight through without breaking
        #   it unless -npvl is used

        my $is_VERSION_statement = 0;

        if (
            !$saw_VERSION_in_this_file
            && $input_line =~ /VERSION/    # quick check to reject most lines
            && $input_line =~ /([\$*])(([\w\:\']*)\bVERSION)\b.*\=/
          )
        {
            $saw_VERSION_in_this_file = 1;
            $is_VERSION_statement     = 1;
            write_logfile_entry("passing VERSION line; -npvl deactivates\n");
            $no_internal_newlines = 1;
        }

        # take care of indentation-only
        # also write a line which is entirely a 'qw' list
        if ( $rOpts->{'indent-only'}
            || ( ( $jmax == 0 ) && ( $$rtoken_type[0] eq 'q' ) ) )
        {
            flush();
            $input_line =~ s/^\s*//;    # trim left end
            $input_line =~ s/\s*$//;    # trim right end

            extract_token(0);
            $token                 = $input_line;
            $type                  = 'q';
            $block_type            = "";
            $container_type        = "";
            $container_environment = "";
            $type_sequence         = "";
            store_token_to_go();
            output_line_to_go();
            return;
        }

        push ( @$rtokens,     ' ', ' ' );  # making $j+2 valid simplifies coding
        push ( @$rtoken_type, 'b', 'b' );
        ($rwhite_space_flag) =
          set_white_space_flag( $jmax, $rtokens, $rtoken_type, $rblock_type );

        # find input tabbing to allow checks for tabbing disagreement
        ## not used for now
        ##$input_line_tabbing = "";
        ##if ( $input_line =~ /^(\s*)/ ) { $input_line_tabbing = $1; }

        # if the buffer hasn't been flushed, add a leading space if
        # necessary to keep essential whitespace. This is really only
        # necessary if we are squeezing out all ws.
        if ( $max_index_to_go >= 0 ) {

            $old_line_count_in_batch++;

            if (
                is_essential_whitespace(
                    $last_last_nonblank_token,
                    $last_last_nonblank_type,
                    $tokens_to_go[$max_index_to_go],
                    $types_to_go[$max_index_to_go],
                    $$rtokens[0],
                    $$rtoken_type[0]
                )
              )
            {
                my $slevel = $$rslevels[0];
                insert_new_token_to_go( ' ', 'b', $slevel,
                    $no_internal_newlines );
            }
        }

        # If we just saw the end of an elsif block, write nag message
        # if we do not see another elseif or an else.  
        if ($looking_for_else) {

            unless ( $$rtokens[0] =~ /^(elsif|else)$/ ) {
                write_logfile_entry("(No else block)\n");
            }
            $looking_for_else = 0;
        }

        # This is a good place to kill incomplete one-line blocks
        if (   ( $semicolons_before_block_self_destruct == 0 )
            && ( $max_index_to_go >= 0 )
            && ( $types_to_go[$max_index_to_go] eq ';' )
            && ( $$rtokens[0] ne '}' ) )
        {
            destroy_one_line_block();
            output_line_to_go();
        }

        # loop to process the tokens one-by-one
        $type  = 'b';
        $token = "";

        foreach $j ( 0 .. $jmax ) {

            # pull out the local values for this token
            extract_token($j);

            if ( $type eq '#' ) {

                # trim trailing whitespace
                # (there is no option at present to prevent this)
                $token =~ s/\s*$//;

                if (
                    $rOpts->{'delete-side-comments'}

                    # delete closing side comments if necessary
                    || (   $rOpts->{'delete-closing-side-comments'}
                        && $token =~ /$closing_side_comment_prefix_pattern/o
                        && $last_nonblank_block_type =~
                        /$closing_side_comment_list_pattern/o )
                  )
                {
                    if ( $types_to_go[$max_index_to_go] eq 'b' ) {
                        unstore_token_to_go();
                    }
                    last;
                }
            }

            # If we are continuing after seeing a right curly brace, flush
            # buffer unless we see what we are looking for, as in
            #   } else ...
            if ( $rbrace_follower && $type ne 'b' ) {

                unless ( $rbrace_follower->{$token} ) {
                    output_line_to_go();
                }
                $rbrace_follower = undef;
            }

            $j_next = ( $$rtoken_type[ $j + 1 ] eq 'b' ) ? $j + 2 : $j + 1;
            $next_nonblank_token      = $$rtokens[$j_next];
            $next_nonblank_token_type = $$rtoken_type[$j_next];

            #--------------------------------------------------------
            # Start of section to patch token text
            #--------------------------------------------------------

            # Modify certain tokens here for whitespace
            # The following is not yet done, but could be:
            #   sub (x x x)
            # These become type 'i', space and all.
            if ( $type eq 'i' or $type eq 't' ) {

                # change "$  var"  to "$var" etc
                if ( $token =~ /^([\$\&\%\*\@]|\-\>)\s/ ) {
                    $token =~ s/\s*//g;
                }

                if ( $token =~ /^sub/ ) { $token =~ s/\s+/ /g }
            }

            # change 'LABEL   :'   to 'LABEL:'
            elsif ( $type eq 'J' ) { $token =~ s/\s+//g }

            # patch to add space to something like "x10"
            # This avoids having to split this token in the pre-tokenizer
            elsif ( $type eq 'n' ) {
                if ( $token =~ /^x\d+/ ) { $token =~ s/x/x / }
            }

            elsif ( $type eq 'Q' ) {
                note_embedded_tab() if ( $token =~ "\t" );

                # make note of something like '$var = s/xxx/yyy/;'
                # in case it should have been '$var =~ s/xxx/yyy/;'
                if (
                       $token =~ /^(s|tr|y|m|\/)/
                    && $last_nonblank_token =~ /^(=|==|!=)$/

                    # precededed by simple scalar
                    && $last_last_nonblank_type eq 'i'
                    && $last_last_nonblank_token =~ /^\$/

                    # followed by some kind of termination
                    # (but give complaint if we can's see far enough ahead)
                    && $next_nonblank_token =~ /^[; \)\}]$/

                    # scalar is not decleared
                    && !(
                           $types_to_go[0] eq 'k'
                        && $tokens_to_go[0] =~ /^(my|our|local)$/
                    )
                  )
                {
                    my $guess = substr( $last_nonblank_token, 0, 1 ) . '~';
                    complain(
"Note: be sure you want '$last_nonblank_token' instead of '$guess' here\n"
                    );
                }
            }

            # trim blanks from right of qw quotes
            # (To avoid trimming qw quotes use -ntqw; the tokenizer handles this) 
            elsif ( $type eq 'q' ) {
                $token =~ s/\s*$//;
                note_embedded_tab() if ( $token =~ "\t" );
            }

            #--------------------------------------------------------
            # End of section to patch token text
            #--------------------------------------------------------

            # insert any needed whitespace
            if (   ( $type ne 'b' )
                && ( $max_index_to_go >= 0 )
                && ( $types_to_go[$max_index_to_go] ne 'b' )
                && $rOpts_add_whitespace )
            {
                my $ws = $$rwhite_space_flag[$j];

                if ( $ws == 1 ) {
                    insert_new_token_to_go( ' ', 'b', $slevel,
                        $no_internal_newlines );
                }
            }

            # Do not allow breaks which would promote a side comment to a
            # block comment.  In order to allow a break before an opening
            # or closing BLOCK, followed by a side comment, those sections 
            # of code will handle this flag separately.
            my $side_comment_follows = ( $next_nonblank_token_type eq '#' );
            my $is_opening_BLOCK =
              (      $type eq '{'
                  && $token eq '{'
                  && $block_type
                  && $block_type ne 't' );
            my $is_closing_BLOCK =
              (      $type eq '}'
                  && $token eq '}'
                  && $block_type
                  && $block_type ne 't' );

            if (   $side_comment_follows
                && !$is_opening_BLOCK
                && !$is_closing_BLOCK )
            {
                $no_internal_newlines = 1;
            }

            # We're only going to handle breaking for code BLOCKS at this
            # (top) level.  Other indentation breaks will be handled by
            # sub scan_list, which is better suited to dealing with them.
            if ($is_opening_BLOCK) {

                # Tentatively output this token.  This is required before
                # calling starting_one_line_block.  We may have to unstore
                # it, though, if we have to break before it.
                store_token_to_go($side_comment_follows);

                # Look ahead to see if we might form a one-line block
                my $too_long =
                  starting_one_line_block( $j, $jmax, $level, $slevel,
                    $ci_level, $rtokens, $rtoken_type, $rblock_type );
                clear_breakpoint_undo_stack();

                # to simplify the logic below, set a flag to indicate if 
                # this opening brace is far from the keyword which introduces it
                my $keyword_on_same_line = 1;
                if (   ( $max_index_to_go >= 0 )
                    && ( $last_nonblank_type eq ')' ) )
                {
                    if (   $block_type =~ /^(if|else|elsif)$/
                        && ( $tokens_to_go[0] eq '}' )
                        && $rOpts_cuddled_else )
                    {
                        $keyword_on_same_line = 1;
                    }
                    elsif ( ( $slevel < $nesting_depth_to_go[0] ) || $too_long )
                    {
                        $keyword_on_same_line = 0;
                    }
                }

                # decide if user requested break before '{'
                my $want_break =

                  # use -bl flag if not a sub block of any type
                  $block_type !~ /^sub/
                  ? $rOpts->{'opening-brace-on-new-line'}

                  # use -sbl flag unless this is an anonymous sub block
                  : $block_type !~ /^sub\W*$/
                  ? $rOpts->{'opening-sub-brace-on-new-line'}

                  # do not break for anonymous subs
                  : 0;

                # Break before an opening '{' ...
                if (

                    # if requested 
                    $want_break

                    # and we were unable to start looking for a block,
                    && $index_start_one_line_block == UNDEFINED_INDEX

                    # or if it will not be on same line as its keyword, so that
                    # it will be outdented (eval.t, overload.t), and the user
                    # has not insisted on keeping it on the right
                    || (   !$keyword_on_same_line
                        && !$rOpts->{'opening-brace-always-on-right'} )

                  )
                {

                    # but only if allowed
                    unless ($no_internal_newlines) {

                        # since we already stored this token, we must unstore it
                        unstore_token_to_go();

                        # then output the line
                        output_line_to_go();

                        # and now store this token at the start of a new line
                        store_token_to_go($side_comment_follows);
                    }
                }

                # Now update for side comment
                if ($side_comment_follows) { $no_internal_newlines = 1 }

                # now output this line
                unless ($no_internal_newlines) {
                    output_line_to_go();
                }
            }

            elsif ($is_closing_BLOCK) {

                # If there is a pending one-line block ..
                if ( $index_start_one_line_block != UNDEFINED_INDEX ) {

                    # we have to terminate it if..
                    if (

                        # it is too long (final length may be different from
                        # initial estimate). note: must allow 1 space for this token
                        excess_line_length( $index_start_one_line_block,
                            $max_index_to_go ) >= 0

                        # or if it has too many semicolons
                        || (   $semicolons_before_block_self_destruct == 0
                            && $last_nonblank_type ne ';' )
                      )
                    {
                        destroy_one_line_block();
                    }
                }

                # put a break before this closing curly brace if appropriate
                unless ( $no_internal_newlines
                    || $index_start_one_line_block != UNDEFINED_INDEX )
                {

                    # add missing semicolon if ...
                    # there are some tokens
                    if (
                        ( $max_index_to_go > 0 )

                        # and we don't have one
                        && ( $last_nonblank_type ne ';' )

                        # patch until some block type issues are fixed:
                        # Do not add semi-colon for block types '{',
                        # '}', and ';' because we cannot be sure yet
                        # that this is a block and not an anonomyous
                        # hash (blktype.t, blktype1.t)
                        && ( $block_type !~ /^[\{\};]$/ )

                        # it seems best not to add semicolons in these
                        # special block types:
                        #     /^(sort|map|grep)$/
                        && ( !$is_sort_map_grep{$block_type} )

                        # and we are allowed to do so.
                        && $rOpts->{'add-semicolons'}
                      )
                    {

                        save_current_token();
                        $token  = ';';
                        $type   = ';';
                        $level  = $levels_to_go[$max_index_to_go];
                        $slevel = $nesting_depth_to_go[$max_index_to_go];
                        $nesting_blocks =
                          $nesting_blocks_to_go[$max_index_to_go];
                        $ci_level       = $ci_levels_to_go[$max_index_to_go];
                        $block_type     = "";
                        $container_type = "";
                        $container_environment = "";
                        $type_sequence         = "";

                        # Note - we remove any blank AFTER extracting its
                        # parameters such as level, etc, above
                        if ( $types_to_go[$max_index_to_go] eq 'b' ) {
                            unstore_token_to_go();
                        }
                        store_token_to_go();

                        note_added_semicolon();
                        restore_current_token();
                    }

                    # then write out everything before this closing curly brace
                    output_line_to_go();

                }

                # Now update for side comment
                if ($side_comment_follows) { $no_internal_newlines = 1 }

                # store the closing curly brace
                store_token_to_go();

                # ok, we just stored a closing curly brace.  Often, but
                # not always, we want to end the line immediately.
                # So now we have to check for special cases.

                # if this '}' successfully ends a one-line block..
                my $is_one_line_block = 0;
                if ( $index_start_one_line_block != UNDEFINED_INDEX ) {

                    $is_one_line_block = 1;

                    # we have to actually make it by removing tentative
                    # breaks that were set within it
                    undo_forced_breakpoint_stack(0);
                    set_nobreaks( $index_start_one_line_block,
                        $max_index_to_go - 1 );

                    # then re-initialize for the next one-line block
                    destroy_one_line_block();

                    # then decide if we want to break after the '}' ..
                    # We will keep going to allow certain brace followers as in:
                    #   do { $ifclosed = 1; last } unless $losing;
                    #
                    # But make a line break if the curly ends a
                    # significant block:
                    #    /^(until|while|for|if|elsif|else)$/
                    if ( $is_until_while_for_if_elsif_else{$block_type} ) {
                        output_line_to_go() unless ($no_internal_newlines);
                    }
                }

                # set string indicating what we need to look for brace follower
                # tokens
                if ( $block_type eq 'do' ) {
                    $rbrace_follower = \%is_do_follower;
                }
                elsif ( $block_type =~ /^(if|elsif|unless)$/ ) {
                    $rbrace_follower = \%is_if_brace_follower;
                }
                elsif ( $block_type eq 'else' ) {
                    $rbrace_follower = \%is_else_brace_follower;
                }

                # added eval for borris.t
                # /^(sort|map|grep|eval)$/
                elsif ( $is_sort_map_grep_eval{$block_type} ) {
                    $rbrace_follower = undef;
                }

                # anonymous sub
                elsif ( $block_type =~ /^sub\W*$/ ) {

                    if ($is_one_line_block) {
                        $rbrace_follower = \%is_anon_sub_1_brace_follower;
                    }
                    else {
                        $rbrace_follower = \%is_anon_sub_brace_follower;
                    }
                }

                # None of the above: specify what can follow a closing
                # brace of a block which is not an
                # if/elsif/else/do/sort/map/grep/eval 
                # Testfiles:
                # 'Toolbar.pm', 'Menubar.pm', bless.t, '3rules.pl', 'break1.t
                else {
                    $rbrace_follower = \%is_other_brace_follower;
                }

                # See if an elsif block is followed by another elsif or else;
                # complain if not.
                if ( $block_type eq 'elsif' ) {

                    if ( $next_nonblank_token_type eq 'b' ) {    # end of line?
                        $looking_for_else = 1;    # ok, check on next line
                    }
                    else {

                        unless ( $next_nonblank_token =~ /^(elsif|else)$/ ) {
                            write_logfile_entry("No else block :(\n");
                        }
                    }
                }

                # keep going after these block types: map,sort,grep
                # added eval for borris.t
                #     /^(sort|grep|map|eval)$/
                if ( $is_sort_map_grep_eval{$block_type} ) {

                    # keep going
                }

                # if no more tokens, postpone decision until re-entring
                elsif ( ( $next_nonblank_token_type eq 'b' )
                    && $rOpts_add_newlines )
                {
                    unless ($rbrace_follower) {
                        output_line_to_go() unless ($no_internal_newlines);
                    }
                }

                elsif ($rbrace_follower) {

                    unless ( $rbrace_follower->{$next_nonblank_token} ) {
                        output_line_to_go() unless ($no_internal_newlines);
                    }
                    $rbrace_follower = undef;
                }

                else {
                    output_line_to_go() unless ($no_internal_newlines);
                }

            }    # end treatment of closing block token

            # handle semicolon
            elsif ( $type eq ';' ) {

                # kill one-line blocks with too many semicolons
                $semicolons_before_block_self_destruct--;
                if (
                    ( $semicolons_before_block_self_destruct < 0 )
                    || (   $semicolons_before_block_self_destruct == 0
                        && $next_nonblank_token_type !~ /^[b\}]$/ )
                  )
                {
                    destroy_one_line_block();
                }

                # Remove unnecessary semicolons, but not after bare
                # blocks, where it could be unsafe if the brace is
                # mistokenized.
                if (
                    (
                        $last_nonblank_token eq '}'
                        && (
                            $is_block_without_semicolon{
                                $last_nonblank_block_type
                            }
                            || $last_nonblank_block_type =~ /^sub\s+\w/
                            || $last_nonblank_block_type =~ /^\w+:$/ )
                    )
                    || $last_nonblank_type eq ';'
                  )
                {

                    if (
                        $rOpts->{'delete-semicolons'}

                        # don't delete ; before a # because it would promote it
                        # to a block comment
                        && ( $next_nonblank_token_type ne '#' )
                      )
                    {
                        note_deleted_semicolon();
                        output_line_to_go()
                          unless ( $no_internal_newlines
                            || $index_start_one_line_block != UNDEFINED_INDEX );
                        next;
                    }
                    else {
                        write_logfile_entry("Extra ';'\n");
                    }
                }
                store_token_to_go();

                output_line_to_go()
                  unless ( $no_internal_newlines
                    || ( $next_nonblank_token eq '}' ) );

            }

            # handle here_doc target string
            elsif ( $type eq 'h' ) {
                $no_internal_newlines =
                  1;    # no newlines after seeing here-target
                destroy_one_line_block();
                store_token_to_go();
            }

            # handle all other token types
            else {

                # if this is a blank...
                if ( $type eq 'b' ) {

                    # make it just one character
                    $token = ' ' if $rOpts_add_whitespace;

                    # delete it if unwanted by whitespace rules
                    # or we are deleting all whitespace
                    my $ws = $$rwhite_space_flag[ $j + 1 ];
                    if ( ( defined($ws) && $ws == -1 )
                        || $rOpts_delete_old_whitespace )
                    {

                        # unless it might make a syntax error
                        next
                          unless is_essential_whitespace(
                            $last_last_nonblank_token,
                            $last_last_nonblank_type,
                            $tokens_to_go[$max_index_to_go],
                            $types_to_go[$max_index_to_go],
                            $$rtokens[ $j + 1 ],
                            $$rtoken_type[ $j + 1 ]
                          );
                    }
                }
                store_token_to_go();
            }

            # remember two previous nonblank OUTPUT tokens
            if ( $type ne '#' && $type ne 'b' ) {
                $last_last_nonblank_token = $last_nonblank_token;
                $last_last_nonblank_type  = $last_nonblank_type;
                $last_nonblank_token      = $token;
                $last_nonblank_type       = $type;
                $last_nonblank_block_type = $block_type;
            }

            # unset the continued-quote flag since it only applies to the
            # first token, and we want to resume normal formatting if
            # there are additional tokens on the line
            $in_continued_quote = 0;

        }    # end of loop over all tokens in this 'line_of_tokens'

        # we have to flush ..
        if (

            # if there is a side comment
            ( ( $type eq '#' ) && !$rOpts->{'delete-side-comments'} )

            # if this line which ends in a quote
            || $in_quote

            # if this is a VERSION statement
            || $is_VERSION_statement

            # to keep a label on one line if that is how it is now
            || ( ( $type eq 'J' ) && ( $max_index_to_go == 0 ) )

            # if we are instructed to keep all old line breaks
            || !$rOpts->{'delete-old-newlines'}
          )
        {
            destroy_one_line_block();
            output_line_to_go();
        }

        # mark old line breakpoints in current output stream
        if ( $max_index_to_go >= 0 && !$rOpts_ignore_old_line_breaks ) {
            $old_breakpoint_to_go[$max_index_to_go] = 1;
        }
    }
}    # end print_line_of_tokens

sub note_added_semicolon {
    $last_added_semicolon_at = $input_line_number;
    if ( $added_semicolon_count == 0 ) {
        $first_added_semicolon_at = $last_added_semicolon_at;
    }
    $added_semicolon_count++;
    write_logfile_entry("Added ';' here\n");
}

sub note_deleted_semicolon {
    $last_deleted_semicolon_at = $input_line_number;
    if ( $deleted_semicolon_count == 0 ) {
        $first_deleted_semicolon_at = $last_deleted_semicolon_at;
    }
    $deleted_semicolon_count++;
    write_logfile_entry("Deleted unnecessary ';'\n");    # i hope ;)
}

sub note_embedded_tab {
    $embedded_tab_count++;
    $last_embedded_tab_at = $input_line_number;
    if ( !$first_embedded_tab_at ) {
        $first_embedded_tab_at = $last_embedded_tab_at;
    }

    if ( $embedded_tab_count <= MAX_NAG_MESSAGES ) {
        write_logfile_entry("Embedded tabs in quote or pattern\n");
    }
}

sub starting_one_line_block {

    # after seeing an opening curly brace, look for the closing brace
    # and see if the entire block will fit on a line.  This routine is
    # not always right because it uses the old whitespace, so a check
    # is made later (at the closing brace) to make sure we really 
    # have a one-line block.  We have to do this preliminary check,
    # though, because otherwise we would always break at a semicolon
    # within a one-line block if the block contains multiple statements.

    my ( $j, $jmax, $level, $slevel, $ci_level, $rtokens, $rtoken_type,
        $rblock_type )
      = @_;

    # kill any current block - we can only go 1 deep
    destroy_one_line_block();

    # return value:  
    #  1=distance from start of block to opening brace exceeds line length
    #  0=otherwise

    my $i_start = 0;

    # shouldn't happen: there must have been a prior call to 
    # store_token_to_go to put the opening brace in the output stream
    if ( $max_index_to_go < 0 ) {
        warning("program bug: store_token_to_go called incorrectly\n");
        report_definite_bug();
    }
    else {

        # cannot use one-line blocks with cuddled else else/elsif lines
        if ( ( $tokens_to_go[0] eq '}' ) && $rOpts_cuddled_else ) {
            return 0;
        }
    }

    my $block_type = $$rblock_type[$j];

    # find the starting keyword for this block (such as 'if', 'else', ...)

    if ( $block_type =~ /^[\{\}\;\:]$/ ) {
        $i_start = $max_index_to_go;
    }

    elsif ( $last_last_nonblank_token_to_go eq ')' ) {

        # For something like "if (xxx) {", the keyword "if" will be
        # just after the most recent break. This will be 0 unless
        # we have just killed a one-line block and are starting another.
        # (doif.t)
        $i_start = $index_max_forced_break + 1;
        if ( $types_to_go[$i_start] eq 'b' ) {
            $i_start++;
        }

        unless ( $tokens_to_go[$i_start] eq $block_type ) {
            return 0;
        }
    }

    # the previous nonblank token should start these block types
    elsif (
        ( $last_last_nonblank_token_to_go eq $block_type )
        || (   $block_type =~ /^sub/
            && $last_last_nonblank_token_to_go =~ /^sub/ )
      )
    {
        $i_start = $last_last_nonblank_index_to_go;
    }

    else {
        return 1;
    }

    my $pos = total_line_length( $i_start, $max_index_to_go ) - 1;

    my $i;

    # see if length is too long to even start
    if ( $pos > $rOpts_maximum_line_length ) {
        return 1;
    }

    for ( $i = $j + 1 ; $i <= $jmax ; $i++ ) {

        # old whitespace could be arbitrarily large, so don't use it
        if ( $$rtoken_type[$i] eq 'b' ) { $pos += 1 }
        else { $pos += length( $$rtokens[$i] ) }

        # Return false result if we exceed the maximum line length,
        if ( $pos > $rOpts_maximum_line_length ) {
            return 0;
        }

        # or encounter another opening brace before finding the closing brace.
        elsif ($$rtokens[$i] eq '{'
            && $$rtoken_type[$i] eq '{'
            && $$rblock_type[$i] )
        {
            return 0;
        }

        # if we find our closing brace..
        elsif ($$rtokens[$i] eq '}'
            && $$rtoken_type[$i] eq '}'
            && $$rblock_type[$i] )
        {

            # be sure any trailing comment also fits on the line
            my $i_nonblank =
              ( $$rtoken_type[ $i + 1 ] eq 'b' ) ? $i + 2 : $i + 1;

            if ( $$rtoken_type[$i_nonblank] eq '#' ) {
                $pos += length( $$rtokens[$i_nonblank] );

                if ( $i_nonblank > $i + 1 ) {
                    $pos += length( $$rtokens[ $i + 1 ] );
                }

                if ( $pos > $rOpts_maximum_line_length ) {
                    return 0;
                }
            }

            # ok, it's a one-line block
            create_one_line_block( $i_start, 20 );
            return 0;
        }

        # just keep going for other characters
        else {
        }
    }

    # Allow certain types of new one-line blocks to form by joining 
    # input lines.  These can be safely done, but for other block types,
    # we keep old one-line blocks but do not form new ones. It is not
    # always a good idea to make as many one-line blocks as possible,
    # so other types are not done.  The user can always use -mangle.
    #     /^(eval|map|grep|sort)$/ 
    if ( $is_sort_map_grep_eval{$block_type} ) {
        create_one_line_block( $i_start, 1 );
    }

    return 0;
}

sub unstore_token_to_go {

    # remove most recent token from output stream
    if ( $max_index_to_go > 0 ) {
        $max_index_to_go--;
    }
    else {
        $max_index_to_go = UNDEFINED_INDEX;
    }

}

sub want_blank_line {
    flush();
    $file_writer_object->want_blank_line();
}

sub write_unindented_line {
    flush();
    $file_writer_object->write_line( $_[0] );
}

sub undo_lp_ci {

    # If there is a single, long parameter within parens, like this:
    # 
    #  $self->command( "/msg "
    #        . $infoline->chan
    #        . " You said $1, but did you know that it's square was "
    #        . $1 * $1 . " ?" );
    # 
    # we can remove the continuation indentation of the 2nd and higher lines
    # to achieve this effect, which is more pleasing:
    # 
    #  $self->command("/msg "
    #                 . $infoline->chan
    #                 . " You said $1, but did you know that it's square was "
    #                 . $1 * $1 . " ?");

    my ( $line_open, $i_start, $closing_index, $ri_first, $ri_last ) = @_;
    my $max_line = @$ri_first - 1;

    # must be multiple lines
    return unless $max_line > $line_open;

    my $lev_start     = $levels_to_go[$i_start];
    my $ci_start_plus = 1 + $ci_levels_to_go[$i_start];

    # see if all additional lines in this container have continuation
    # indentation
    my $n;
    my $line_1 = 1 + $line_open;
    for ( $n = $line_1 ; $n <= $max_line ; ++$n ) {
        my $ibeg = $$ri_first[$n];
        my $iend = $$ri_last[$n];
        if ( $ibeg eq $closing_index ) { $n--; last }
        return if ( $lev_start != $levels_to_go[$ibeg] );
        return if ( $ci_start_plus != $ci_levels_to_go[$ibeg] );
        last if ( $closing_index <= $iend );
    }

    # we can reduce the indentation of all continuation lines
    my $continuation_line_count = $n - $line_open;
    @ci_levels_to_go[ @$ri_first[ $line_1 .. $n ] ] =
      (0) x ($continuation_line_count);
    @leading_spaces_to_go[ @$ri_first[ $line_1 .. $n ] ] =
      @reduced_spaces_to_go[ @$ri_first[ $line_1 .. $n ] ];
}

sub set_logical_padding {

    # Look at a batch of lines and see if extra padding can improve the
    # alignment when there are leading logical operators. Here is an
    # example, in which some extra space is introduced before 
    # '( $year' to make it line up with the subsequent lines:
    #
    #       if (   ( $Year < 1601 )
    #           || ( $Year > 2899 )
    #           || ( $EndYear < 1601 )
    #           || ( $EndYear > 2899 ) )
    #       {
    #           &Error_OutOfRange;
    #       }
    #
    my ( $ri_first, $ri_last ) = @_;
    my $max_line = @$ri_first - 1;

    my ( $ibeg, $ibeg_next, $ibegm, $iend, $iendm, $ipad, $line, $pad_spaces,
        $tok_next, $has_leading_op_next, $has_leading_op );

    # looking at each line of this batch..
    foreach $line ( 0 .. $max_line - 1 ) {

        # see if the next line begins with a logical operator
        $ibeg                = $$ri_first[$line];
        $iend                = $$ri_last[$line];
        $ibeg_next           = $$ri_first[ $line + 1 ];
        $tok_next            = $tokens_to_go[$ibeg_next];
        $has_leading_op_next = ( $tok_next =~ /^(\&\&|\|\||and|or)$/ );
        next unless ($has_leading_op_next);

        # next line must not be at lesser depth
        next
          if ( $nesting_depth_to_go[$ibeg] > $nesting_depth_to_go[$ibeg_next] );

        # identify the token in this line to be padded on the left
        $ipad = undef;

        # handle lines at same depth...
        if ( $nesting_depth_to_go[$ibeg] == $nesting_depth_to_go[$ibeg_next] ) {

            # previous line must be at lesser depth if this is not first line
            # of the batch
            if ( $line > 0 ) {
                next
                  if $nesting_depth_to_go[$ibegm] >=
                  $nesting_depth_to_go[$ibeg];

                # skip if we are at same depth as a previous line
                # with leading logical operator
                next if $has_leading_op;
                $ipad = $ibeg;
            }

            # for first line of the batch..
            else {

                # if this is text after closing '}'
                # then look for an interior token to pad
                if ( $types_to_go[$ibeg] eq '}' ) {

                }

                # otherwise, we might pad if it looks really good
                else {

                    # we might pad token $ibeg, so be sure that it
                    # is at the same depth as the next line.
                    next
                      if ( $nesting_depth_to_go[ $ibeg + 1 ] !=
                        $nesting_depth_to_go[$ibeg_next] );

                    # We can pad on line 1 of a statement if at least 3
                    # lines will be aligned. Otherwise, it
                    # can look very confusing.
                    if ( $max_line > 2 ) {
                        my $leading_token = $tokens_to_go[$ibeg_next];
                        my $count         = 1;
                        foreach my $l ( 2 .. 3 ) {
                            my $ibeg_next_next = $$ri_first[ $line + $l ];
                            next
                              unless $tokens_to_go[$ibeg_next_next] eq
                              $leading_token;
                            $count++;
                        }
                        next unless $count == 3;
                        $ipad = $ibeg;
                    }
                    else {
                        next;
                    }
                }
            }
        }

        # find interior token to pad if necessary
        if ( !defined($ipad) ) {

            for ( my $i = $ibeg ; ( $i < $iend ) && !$ipad ; $i++ ) {

                # find any unclosed container
                next
                  unless ( $type_sequence_to_go[$i]
                    && $mate_index_to_go[$i] > $iend );

                # find next nonblank token to pad
                $ipad = $i + 1;
                if ( $types_to_go[$ipad] eq 'b' ) {
                    $ipad++;
                    last if ( $ipad > $iend );
                }
            }
            last unless $ipad;
        }

        # lines must be somewhat similar to be padded..
        my $iend_next  = $$ri_last[ $line + 1 ];
        my $inext_next = $ibeg_next + 1;
        if ( $types_to_go[$inext_next] eq 'b' ) {
            $inext_next++;
        }
        my $type = $types_to_go[$ipad];

        # see if there are multiple continuation lines
        my $logical_continuation_lines = 1;
        if ( $line + 2 <= $max_line ) {
            my $leading_token  = $tokens_to_go[$ibeg_next];
            my $ibeg_next_next = $$ri_first[ $line + 2 ];
            if (   $tokens_to_go[$ibeg_next_next] eq $leading_token
                && $nesting_depth_to_go[$ibeg_next] eq
                $nesting_depth_to_go[$ibeg_next_next] )
            {
                $logical_continuation_lines++;
            }
        }
        if (

            # next line must not be at greater depth
            $nesting_depth_to_go[ $iend_next + 1 ] <=
            $nesting_depth_to_go[$ipad]

            # and ..
            && (

                # either we have multiple continuation lines to follow
                # and we are not padding the first token
                ( $logical_continuation_lines > 1 && $ipad > 0 )

                # or..
                || (

                    # types must match
                    $types_to_go[$inext_next] eq $type

                    # and keywords must match if keyword
                    && !(
                           $type eq 'k'
                        && $tokens_to_go[$ipad] ne $tokens_to_go[$inext_next]
                    )
                )
            )
          )
        {
            my $length_1 = total_line_length( $ibeg,      $ipad - 1 );
            my $length_2 = total_line_length( $ibeg_next, $inext_next - 1 );
            $pad_spaces = $length_2 - $length_1;

            # make sure this won't change if -lp is used
            my $indentation_1 = $leading_spaces_to_go[$ibeg];
            if ( ref($indentation_1) ) {
                if ( $indentation_1->get_RECOVERABLE_SPACES() == 0 ) {
                    my $indentation_2 = $leading_spaces_to_go[$ibeg_next];
                    unless ( $indentation_2->get_RECOVERABLE_SPACES() == 0 ) {
                        $pad_spaces = 0;
                    }
                }
            }

            # we might be able to handle a pad of -1 by removing a blank
            # token
            if ( $pad_spaces < 0 ) {
                if ( $pad_spaces == -1 ) {
                    if ( $ipad > $ibeg && $types_to_go[ $ipad - 1 ] eq 'b' ) {
                        $tokens_to_go[ $ipad - 1 ] = '';
                    }
                }
                $pad_spaces = 0;
            }

            # now apply any padding for alignment
            if ( $ipad >= 0 && $pad_spaces ) {
                my $length_t = total_line_length( $ibeg, $iend );
                if ( $pad_spaces + $length_t <= $rOpts_maximum_line_length ) {
                    $tokens_to_go[$ipad] =
                      ' ' x $pad_spaces . $tokens_to_go[$ipad];
                }
            }
        }
    }
    continue {
        $iendm          = $iend;
        $ibegm          = $ibeg;
        $has_leading_op = $has_leading_op_next;
    }    # end of loop over lines
    return;
}

sub correct_lp_indentation {

    # When the -lp option is used, we need to make a last pass through
    # each line to correct the indentation positions in case they differ
    # from the predictions.  This is necessary because perltidy uses a
    # predictor/corrector method for aligning with opening parens.  The
    # predictor is usually good, but sometimes stumbles.  The corrector
    # tries to patch things up once the actual opening paren locations
    # are known.
    my ( $ri_first, $ri_last ) = @_;
    my $do_not_pad = 0;

    #  Note on flag '$do_not_pad':
    #  We want to avoid a situation like this, where the aligner inserts
    #  whitespace before the '=' to align it with a previous '=', because
    #  otherwise the parens might become mis-aligned in a situation like
    #  this, where the '=' has become aligned with the previous line,
    #  pushing the opening '(' forward beyond where we want it.
    #  
    #  $mkFloor::currentRoom = '';
    #  $mkFloor::c_entry     = $c->Entry(
    #                                 -width        => '10',
    #                                 -relief       => 'sunken',
    #                                 ...
    #                                 );
    # 
    #  We leave it to the aligner to decide how to do this.

    # first remove continuation indentation if appropriate
    my $max_line = @$ri_first - 1;

    # looking at each line of this batch..
    my ( $ibeg, $iend );
    my $line;
    foreach $line ( 0 .. $max_line ) {
        $ibeg = $$ri_first[$line];
        $iend = $$ri_last[$line];

        # looking at each token in this output line..
        my $i;
        foreach $i ( $ibeg .. $iend ) {

            # How many space characters to place before this token
            # for special alignment.  Actual padding is done in the
            # continue block.

            # looking for next unvisited indentation item
            my $indentation = $leading_spaces_to_go[$i];
            if ( !$indentation->get_MARKED() ) {
                $indentation->set_MARKED(1);

                # looking for indentation item for which we are aligning 
                # with parens, braces, and brackets 
                next unless ( $indentation->get_ALIGN_PAREN() );

                # skip closed container on this line
                if ( $i > $ibeg ) {
                    my $im = $i - 1;
                    if ( $types_to_go[$im] eq 'b' && $im > $ibeg ) { $im-- }
                    if (   $type_sequence_to_go[$im]
                        && $mate_index_to_go[$im] <= $iend )
                    {
                        next;
                    }
                }

                if ( $line == 1 && $i == $ibeg ) {
                    $do_not_pad = 1;
                }

                # Ok, let's see what the error is and try to fix it
                my $actual_pos;
                my $predicted_pos = $indentation->get_SPACES();
                if ( $i > $ibeg ) {

                    # token is mid-line - use length to previous token
                    $actual_pos = total_line_length( $ibeg, $i - 1 );

                    # for mid-line token, we must check to see if all
                    # additional lines have continuation indentation,
                    # and remove it if so.  Otherwise, we do not get
                    # good alignment.
                    my $closing_index = $indentation->get_CLOSED();
                    if ( $closing_index > $iend ) {
                        my $ibeg_next = $$ri_first[ $line + 1 ];
                        if ( $ci_levels_to_go[$ibeg_next] > 0 ) {
                            undo_lp_ci( $line, $i, $closing_index, $ri_first,
                                $ri_last );
                        }
                    }
                }
                elsif ( $line > 0 ) {

                    # handle case where token starts a new line; 
                    # use length of previous line 
                    my $ibegm = $$ri_first[ $line - 1 ];
                    my $iendm = $$ri_last[ $line - 1 ];
                    $actual_pos = total_line_length( $ibegm, $iendm );

                    # follow -pt style
                    ++$actual_pos
                      if ( $types_to_go[ $iendm + 1 ] eq 'b' );
                }
                else {

                    # token is first character of first line of batch
                    $actual_pos = $predicted_pos;
                }

                my $move_right = $actual_pos - $predicted_pos;

                # done if no error to correct (gnu2.t)
                if ( $move_right == 0 ) {
                    $indentation->set_RECOVERABLE_SPACES($move_right);
                    next;
                }

                # if we have not seen closure for this indentation in
                # this batch, we can only pass on a request to the
                # vertical aligner
                my $closing_index = $indentation->get_CLOSED();

                if ( $closing_index < 0 ) {
                    $indentation->set_RECOVERABLE_SPACES($move_right);
                    next;
                }

                # If necessary, look ahead to see if there is really any
                # leading whitespace dependent on this whitespace, and
                # also find the longest line using this whitespace.  
                # Since it is always safe to move left if there are no
                # dependents, we only need to do this if we may have
                # dependent nodes or need to move right. 

                my $right_margin = 0;
                my $have_child   = $indentation->get_HAVE_CHILD();

                my %saw_indentation;
                my $line_count = 1;
                $saw_indentation{$indentation} = $indentation;

                if ( $have_child || $move_right > 0 ) {
                    $have_child = 0;
                    my $max_length = 0;
                    if ( $i == $ibeg ) {
                        $max_length = total_line_length( $ibeg, $iend );
                    }

                    # look ahead at the rest of the lines of this batch..
                    my $line_t;
                    foreach $line_t ( $line + 1 .. $max_line ) {
                        my $ibeg_t = $$ri_first[$line_t];
                        my $iend_t = $$ri_last[$line_t];
                        last if ( $closing_index <= $ibeg_t );

                        # remember all different indentation objects
                        my $indentation_t = $leading_spaces_to_go[$ibeg_t];
                        $saw_indentation{$indentation_t} = $indentation_t;
                        $line_count++;

                        # remember longest line in the group
                        my $length_t = total_line_length( $ibeg_t, $iend_t );
                        if ( $length_t > $max_length ) {
                            $max_length = $length_t;
                        }
                    }
                    $right_margin = $rOpts_maximum_line_length - $max_length;
                    if ( $right_margin < 0 ) { $right_margin = 0 }
                }

                my $first_line_comma_count =
                  grep { $_ eq ',' } @types_to_go[ $ibeg .. $iend ];
                my $comma_count = $indentation->get_COMMA_COUNT();
                my $arrow_count = $indentation->get_ARROW_COUNT();

                # This is a simple approximate test for vertical alignment: 
                # if we broke just after an opening paren, brace, bracket, 
                # and there are 2 or more commas in the first line, 
                # and there are no '=>'s,
                # then we are probably vertically aligned.  We could set
                # an exact flag in sub scan_list, but this is good
                # enough.
                my $indentation_count     = keys %saw_indentation;
                my $is_vertically_aligned =
                  (      $i == $ibeg
                      && $first_line_comma_count > 1
                      && $indentation_count == 1
                      && ( $arrow_count == 0 || $arrow_count == $line_count ) );

                # Make the move if possible ..
                if (

                    # we can always move left
                    $move_right < 0

                    # but we should only move right if we are sure it will
                    # not spoil vertical alignment
                    || ( $comma_count == 0 )
                    || ( $comma_count > 0 && !$is_vertically_aligned )
                  )
                {
                    my $move =
                      ( $move_right <= $right_margin )
                      ? $move_right
                      : $right_margin;

                    foreach ( keys %saw_indentation ) {
                        $saw_indentation{$_}
                          ->permanently_decrease_AVAILABLE_SPACES( -$move );
                    }
                }

                # Otherwise, record what we want and the vertical aligner 
                # will try to recover it.
                else {
                    $indentation->set_RECOVERABLE_SPACES($move_right);
                }
            }
        }
    }
    return $do_not_pad;
}

# flush is called to output any tokens in the pipeline, so that
# an alternate source of lines can be written in the correct order

sub flush {
    destroy_one_line_block();
    output_line_to_go();
    Perl::Tidy::VerticalAligner::flush();
}

# output_line_to_go sends one logical line of tokens on down the
# pipeline to the VerticalAligner package, breaking the line into continuation
# lines as necessary.  The line of tokens is ready to go in the "to_go"
# arrays.

sub output_line_to_go {

    # debug stuff; this routine can be called from many points
    FORMATTER_DEBUG_FLAG_OUTPUT && do {
        my ( $a, $b, $c ) = caller;
        write_diagnostics(
"OUTPUT: output_line_to_go called: $a $c $last_nonblank_type $last_nonblank_token, one_line=$index_start_one_line_block, tokens to write=$max_index_to_go\n"
        );
        my $output_str = join "", @tokens_to_go[ 0 .. $max_index_to_go ];
        write_diagnostics("$output_str\n");
    };

    # just set a tentative breakpoint if we might be in a one-line block
    if ( $index_start_one_line_block != UNDEFINED_INDEX ) {
        set_forced_breakpoint($max_index_to_go);
        return;
    }

    my $cscw_block_comment;
    $cscw_block_comment = add_closing_side_comment()
      if ( $rOpts->{'closing-side-comments'} && $max_index_to_go >= 0 );

    match_opening_and_closing_tokens();

    # tell the -lp option we are outputting a batch so it can close
    # any unfinished items in its stack
    finish_lp_batch();

    my $imin = 0;
    my $imax = $max_index_to_go;

    # trim any blank tokens
    if ( $max_index_to_go >= 0 ) {
        if ( $types_to_go[$imin] eq 'b' ) { $imin++ }
        if ( $types_to_go[$imax] eq 'b' ) { $imax-- }
    }

    # anything left to write?
    if ( $imin <= $imax ) {

        # add a blank line before certain key types
        if ( $last_line_leading_type !~ /^[#b]/ ) {
            my $want_blank    = 0;
            my $leading_token = $tokens_to_go[$imin];
            my $leading_type  = $types_to_go[$imin];

            # blank lines before subs except declarations and one-liners
            # MCONVERSION LOCATION - for sub tokenization change
            if ( $leading_token =~ /^(sub\s)/ && $leading_type eq 'i' ) {
                $want_blank = ( $rOpts->{'blanks-before-subs'} )
                  && (
                    terminal_type( \@types_to_go, \@block_type_to_go, $imin,
                        $imax ) !~ /^[\;\}]$/
                  );
            }

            # break before all package declarations
            # MCONVERSION LOCATION - for tokenizaton change
            elsif ( $leading_token =~ /^(package\s)/ && $leading_type eq 'i' ) {
                $want_blank = ( $rOpts->{'blanks-before-subs'} );
            }

            # break before certain key blocks except one-liners
            if ( $leading_token =~ /^(BEGIN|END)$/ && $leading_type eq 'k' ) {
                $want_blank = ( $rOpts->{'blanks-before-subs'} )
                  && (
                    terminal_type( \@types_to_go, \@block_type_to_go, $imin,
                        $imax ) ne '}'
                  );
            }

            # Break before certain block types if we haven't had a break at this
            # level for a while.  This is the difficult decision..
            elsif ($leading_token =~ /^(unless|if|while|until|for|foreach)$/
                && $leading_type eq 'k'
                && $last_line_leading_level >= 0 )
            {
                my $lc = $nonblank_lines_at_depth[$last_line_leading_level];
                if ( !defined($lc) ) { $lc = 0 }

                $want_blank = $rOpts->{'blanks-before-blocks'}
                  && $lc >= $rOpts->{'long-block-line-count'}
                  && $file_writer_object->get_consecutive_nonblank_lines() >=
                  $rOpts->{'long-block-line-count'}
                  && (
                    terminal_type( \@types_to_go, \@block_type_to_go, $imin,
                        $imax ) ne '}'
                  );
            }

            if ($want_blank) {

                # future: send blank line down normal path to VerticalAligner
                Perl::Tidy::VerticalAligner::flush();
                $file_writer_object->write_blank_code_line();
            }
        }

        # update blank line variables and count number of consecutive 
        # non-blank, non-comment lines at this level
        $last_last_line_leading_level = $last_line_leading_level;
        $last_line_leading_level      = $levels_to_go[$imin];
        $last_line_leading_type       = $types_to_go[$imin];
        if (   $last_line_leading_level == $last_last_line_leading_level
            && $last_line_leading_level >= 0
            && $last_line_leading_type ne 'b'
            && $last_line_leading_type ne '#'
            && defined( $nonblank_lines_at_depth[$last_line_leading_level] ) )
        {
            $nonblank_lines_at_depth[$last_line_leading_level]++;
        }
        else {
            $nonblank_lines_at_depth[$last_line_leading_level] = 1;
        }

        FORMATTER_DEBUG_FLAG_FLUSH && do {
            my ( $package, $file, $line ) = caller;
            print
"FLUSH: flushing from $package $file $line, types= $types_to_go[$imin] to $types_to_go[$imax]\n";
        };

        # add a couple of extra terminal blank tokens
        pad_array_to_go();

        # set all forced breakpoints for good list formatting
        my $saw_good_break = 0;
        my $is_long_line   = excess_line_length( $imin, $max_index_to_go ) > 0;

        if (
            $max_index_to_go > 0
            && (
                   $is_long_line
                || $old_line_count_in_batch > 1
                || is_unbalanced_batch()
                || (
                    $comma_count_in_batch
                    && (   $rOpts_maximum_fields_per_table > 0
                        || $rOpts_comma_arrow_breakpoints == 0 )
                )
            )
          )
        {
            $saw_good_break = scan_list();
        }

        # let $ri_first and $ri_last be references to lists of 
        # first and last tokens of line fragments to output..
        my ( $ri_first, $ri_last );

        # write a single line if..
        if (

            # we aren't allowed to add any newlines
            !$rOpts_add_newlines

            # or, we don't already have an interior breakpoint
            # and we didn't see a good breakpoint
            || (
                   !$forced_breakpoint_count
                && !$saw_good_break

                # and this line is 'short' 
                && !$is_long_line
            )
          )
        {
            @$ri_first = ($imin);
            @$ri_last  = ($imax);
        }

        # otherwise use multiple lines
        else {

            ( $ri_first, $ri_last ) = set_continuation_breaks($saw_good_break);

            # now we do a correction step to clean this up a bit
            # (The only time we would not do this is for debugging)
            if ( $rOpts->{'recombine'} ) {
                ( $ri_first, $ri_last ) =
                  recombine_breakpoints( $ri_first, $ri_last );
            }
        }

        # do corrector step if -lp option is used
        my $do_not_pad = 0;
        if ($rOpts_line_up_parentheses) {
            $do_not_pad = correct_lp_indentation( $ri_first, $ri_last );
        }
        send_lines_to_vertical_aligner( $ri_first, $ri_last, $do_not_pad );
    }
    prepare_for_new_input_lines();

    # output any new -cscw block comment
    if ($cscw_block_comment) {
        flush();
        $file_writer_object->write_code_line( $cscw_block_comment . "\n" );
    }
}

sub reset_block_text_accumulator {

    # save text after 'if' and 'elsif' to append after 'else'
    if ($accumulating_text_for_block) {

        if ( $accumulating_text_for_block =~ /^(if|elsif)$/ ) {
            push @{$rleading_block_if_elsif_text}, $leading_block_text;
        }
    }
    $accumulating_text_for_block        = "";
    $leading_block_text                 = "";
    $leading_block_text_level           = 0;
    $leading_block_text_length_exceeded = 0;
    $leading_block_text_line_number     = 0;
    $leading_block_text_line_length     = 0;
}

sub set_block_text_accumulator {
    my $i = shift;
    $accumulating_text_for_block = $tokens_to_go[$i];
    if ( $accumulating_text_for_block !~ /^els/ ) {
        $rleading_block_if_elsif_text = [];
    }
    $leading_block_text             = "";
    $leading_block_text_level       = $levels_to_go[$i];
    $leading_block_text_line_number =
      $vertical_aligner_object->get_output_line_number();
    $leading_block_text_length_exceeded = 0;

    # this will contain the column number of the last character
    # of the closing side comment
    $leading_block_text_line_length =
      length($accumulating_text_for_block) +
      length( $rOpts->{'closing-side-comment-prefix'} ) +
      $leading_block_text_level * $rOpts_indent_columns + 3;
}

sub accumulate_block_text {
    my $i = shift;

    # accumulate leading text for -csc, ignoring any side comments
    if (   $accumulating_text_for_block
        && !$leading_block_text_length_exceeded
        && $types_to_go[$i] ne '#' )
    {

        my $added_length = length( $tokens_to_go[$i] );
        $added_length += 1 if $i == 0;
        my $new_line_length = $leading_block_text_line_length + $added_length;

        # we can add this text if we don't exceed some limits..
        if (

            # we must not have already exceeded the text length limit
            length($leading_block_text) <
            $rOpts_closing_side_comment_maximum_text

            # and either:
            # the new total line length must be below the line length limit
            # or the new length must be below the text length limit
            # (ie, we may allow one token to exceed the text length limit)
            && ( $new_line_length < $rOpts_maximum_line_length
                || length($leading_block_text) + $added_length <
                $rOpts_closing_side_comment_maximum_text )

            # UNLESS: we are adding a closing paren before the brace we seek.
            # This is an attempt to avoid situations where the ... to be
            # added are longer than the omitted right paren, as in:

            #   foreach my $item (@a_rather_long_variable_name_here) {
            #      &whatever;
            #   } ## end foreach my $item (@a_rather_long_variable_name_here...

            || (
                $tokens_to_go[$i] eq ')'
                && (
                    (
                           $i + 1 <= $max_index_to_go
                        && $block_type_to_go[ $i + 1 ] eq
                        $accumulating_text_for_block
                    )
                    || (   $i + 2 <= $max_index_to_go
                        && $block_type_to_go[ $i + 2 ] eq
                        $accumulating_text_for_block )
                )
            )
          )
        {

            # add an extra space at each newline
            if ( $i == 0 ) { $leading_block_text .= ' ' }

            # add the token text
            $leading_block_text .= $tokens_to_go[$i];
            $leading_block_text_line_length = $new_line_length;
        }

        # show that text was truncated if necessary
        elsif ( $types_to_go[$i] ne 'b' ) {
            $leading_block_text_length_exceeded = 1;
            $leading_block_text .= '...';
        }
    }
}

{
    my %is_if_elsif_else_unless_while_until_for_foreach;

    BEGIN {

        # These block types may have text between the keyword and opening
        # curly.  Note: 'else' does not, but must be included to allow trailing
        # if/elsif text to be appended.
        @_ = qw(if elsif else unless while until for foreach);
        @is_if_elsif_else_unless_while_until_for_foreach{@_} = (1) x scalar(@_);
    }

    sub accumulate_csc_text {

        # called once per output buffer when -csc is used. Accumulates
        # the text placed after certain closing block braces.
        # Defines and returns the following for this buffer:

        my $block_leading_text = "";    # the leading text of the last '}'
        my $rblock_leading_if_elsif_text;
        my $i_block_leading_text =
          -1;    # index of token owning block_leading_text
        my $block_line_count    = 100;    # how many lines the block spans
        my $terminal_type       = 'b';    # type of last nonblank token
        my $i_terminal          = 0;      # index of last nonblank token
        my $terminal_block_type = "";

        for my $i ( 0 .. $max_index_to_go ) {
            my $type       = $types_to_go[$i];
            my $block_type = $block_type_to_go[$i];
            my $token      = $tokens_to_go[$i];

            # remember last nonblank token type
            if ( $type ne '#' && $type ne 'b' ) {
                $terminal_type       = $type;
                $terminal_block_type = $block_type;
                $i_terminal          = $i;
            }

            my $type_sequence = $type_sequence_to_go[$i];
            if ( $block_type && $type_sequence ) {

                if ( $token eq '}' ) {

                    # restore any leading text saved when we entered this block
                    if ( defined( $block_leading_text{$type_sequence} ) ) {
                        ( $block_leading_text, $rblock_leading_if_elsif_text ) =
                          @{ $block_leading_text{$type_sequence} };
                        $i_block_leading_text = $i;
                        delete $block_leading_text{$type_sequence};
                        $rleading_block_if_elsif_text =
                          $rblock_leading_if_elsif_text;
                    }

                    # if we run into a '}' then we probably started accumulating
                    # at something like a trailing 'if' clause..no harm done.
                    if (   $accumulating_text_for_block
                        && $levels_to_go[$i] <= $leading_block_text_level )
                    {
                        my $lev = $levels_to_go[$i];
                        reset_block_text_accumulator();
                    }

                    if ( defined( $block_opening_line_number{$type_sequence} ) )
                    {
                        my $output_line_number =
                          $vertical_aligner_object->get_output_line_number();
                        $block_line_count = $output_line_number -
                          $block_opening_line_number{$type_sequence} + 1;
                        delete $block_opening_line_number{$type_sequence};
                    }
                    else {

                        # Error: block opening line undefined for this line..
                        # This shouldn't be possible, but it is not a
                        # significant problem.  
                    }
                }

                elsif ( $token eq '{' ) {

                    my $line_number =
                      $vertical_aligner_object->get_output_line_number();
                    $block_opening_line_number{$type_sequence} = $line_number;

                    if (   $accumulating_text_for_block
                        && $levels_to_go[$i] == $leading_block_text_level )
                    {

                        if ( $accumulating_text_for_block eq $block_type ) {

                            # save any leading text before we enter this block
                            $block_leading_text{$type_sequence} = [
                                $leading_block_text,
                                $rleading_block_if_elsif_text
                            ];
                            $block_opening_line_number{$type_sequence} =
                              $leading_block_text_line_number;
                            reset_block_text_accumulator();
                        }
                        else {

                            # shouldn't happen, but not a serious error.
                            # We were accumulating -csc text for block type
                            # $accumulating_text_for_block and unexpectedly
                            # encountered a '{' for block type $block_type.
                        }
                    }
                }
            }

            if (
                   $type eq 'k'
                && $csc_new_statement_ok

                #  /^(else|if|elsif|unless|while|until|for|foreach)$/
                && $is_if_elsif_else_unless_while_until_for_foreach{$token}
                && $token =~ /$closing_side_comment_list_pattern/o
              )
            {
                set_block_text_accumulator($i);
            }
            else {

                # note: ignoring type 'q' because of tricks being played
                # with 'q' for hanging side comments 
                if ( $type ne 'b' && $type ne '#' && $type ne 'q' ) {
                    $csc_new_statement_ok =
                      ( $block_type || $type eq 'J' || $type eq ';' );
                }
                if (   $type eq ';'
                    && $accumulating_text_for_block
                    && $levels_to_go[$i] == $leading_block_text_level )
                {
                    reset_block_text_accumulator();
                }
                else {
                    accumulate_block_text($i);
                }
            }
        }

        # Treat an 'else' block specially by adding preceding 'if' and
        # 'elsif' text.  Otherwise, the 'end else' is not helpful,
        # especially for cuddled-else formatting.
        if ( $terminal_block_type =~ /^els/ && $rblock_leading_if_elsif_text ) {
            $block_leading_text =
              make_else_csc_text( $i_terminal, $terminal_block_type,
                $block_leading_text, $rblock_leading_if_elsif_text );
        }

        return ( $terminal_type, $i_terminal, $i_block_leading_text,
            $block_leading_text, $block_line_count );
    }
}

sub make_else_csc_text {

    # create additional -csc text for an 'else' and optionally 'elsif',
    # depending on the value of switch
    # $rOpts_closing_side_comment_else_flag: 
    # 
    #  = 0 add 'if' text to trailing else
    #  = 1 same as 0 plus:
    #      add 'if' to 'elsif's if can fit in line length
    #      add last 'elsif' to trailing else if can fit in one line
    #  = 2 same as 1 but do not check if exceed line length
    #
    # $rif_elsif_text = a reference to a list of all previous closing
    # side comments created for this if block
    #
    my ( $i_terminal, $block_type, $block_leading_text, $rif_elsif_text ) = @_;
    my $csc_text = $block_leading_text;

    if ( $block_type eq 'elsif' && $rOpts_closing_side_comment_else_flag == 0 )
    {
        return $csc_text;
    }

    my $count = @{$rif_elsif_text};
    return $csc_text unless ($count);

    my $if_text = '[ if' . $rif_elsif_text->[0];

    # always show the leading 'if' text on 'else'
    if ( $block_type eq 'else' ) {
        $csc_text .= $if_text;
    }

    # see if that's all
    if ( $rOpts_closing_side_comment_else_flag == 0 ) {
        return $csc_text;
    }

    my $last_elsif_text = "";
    if ( $count > 1 ) {
        $last_elsif_text = ' [elsif' . $rif_elsif_text->[ $count - 1 ];
        if ( $count > 2 ) { $last_elsif_text = ' [...' . $last_elsif_text; }
    }

    # tentatively append one more item
    my $saved_text = $csc_text;
    if ( $block_type eq 'else' ) {
        $csc_text .= $last_elsif_text;
    }
    else {
        $csc_text .= ' ' . $if_text;
    }

    # all done if no length checks requested
    if ( $rOpts_closing_side_comment_else_flag == 2 ) {
        return $csc_text;
    }

    # undo it if line length exceeded
    my $length =
      length($csc_text) + length($block_type) +
      length( $rOpts->{'closing-side-comment-prefix'} ) +
      $levels_to_go[$i_terminal] * $rOpts_indent_columns + 3;
    if ( $length > $rOpts_maximum_line_length ) {
        $csc_text = $saved_text;
    }
    return $csc_text;
}

sub add_closing_side_comment {

    # add closing side comments after closing block braces if -csc used
    my $cscw_block_comment;

    #---------------------------------------------------------------
    # Step 1: loop through all tokens of this line to accumulate 
    # the text needed to create the closing side comments. Also see
    # how the line ends.
    #---------------------------------------------------------------

    my ( $terminal_type, $i_terminal, $i_block_leading_text,
        $block_leading_text, $block_line_count )
      = accumulate_csc_text();

    #---------------------------------------------------------------
    # Step 2: make the closing side comment if this ends a block
    #---------------------------------------------------------------
    my $have_side_comment = $i_terminal != $max_index_to_go;

    # if this line might end in a block closure..
    if (
        $terminal_type eq '}'

        # ..and either 
        && (

            # the block is long enough 
            ( $block_line_count >= $rOpts->{'closing-side-comment-interval'} )

            # or there is an existing comment to check
            || (   $have_side_comment
                && $rOpts->{'closing-side-comment-warnings'} )
        )

        # .. and if this is one of the types of interest
        && $block_type_to_go[$i_terminal] =~
        /$closing_side_comment_list_pattern/o

        # ..and the corresponding opening brace must is not in this batch
        # (because we do not need to tag one-line blocks, although this
        # should also be caught with a positive -csci value)
        && $mate_index_to_go[$i_terminal] < 0

        # ..and either
        && (

            # this is the last token (line doesnt have a side comment) 
            !$have_side_comment

            # or the old side comment is a closing side comment
            || $tokens_to_go[$max_index_to_go] =~
            /$closing_side_comment_prefix_pattern/o
        )
      )
    {

        # then make the closing side comment text
        my $token =
"$rOpts->{'closing-side-comment-prefix'} $block_type_to_go[$i_terminal]";

        # append any extra descriptive text collected above
        if ( $i_block_leading_text == $i_terminal ) {
            $token .= $block_leading_text;
        }
        $token =~ s/\s*$//;    # trim any trailing whitespace

        # handle case of existing closing side comment
        if ($have_side_comment) {

            # warn if requested and tokens differ significantly
            if ( $rOpts->{'closing-side-comment-warnings'} ) {
                my $old_csc = $tokens_to_go[$max_index_to_go];
                my $new_csc = $token;
                $new_csc =~ s/(\.\.\.)\s*$//;    # trim trailing '...'
                my $new_trailing_dots = $1;
                $old_csc =~ s/\.\.\.\s*$//;
                $new_csc =~ s/\s+//g;            # trim all whitespace
                $old_csc =~ s/\s+//g;

                # Patch to handle multiple closing side comments at
                # else and elsif's.  These have become too complicated
                # to check, so if we see an indication of 
                # '[ if' or '[ # elsif', then assume they were made
                # by perltidy.
                if ( $block_type_to_go[$i_terminal] eq 'else' ) {
                    if ( $old_csc =~ /\[\s*elsif/ ) { $old_csc = $new_csc }
                }
                elsif ( $block_type_to_go[$i_terminal] eq 'elsif' ) {
                    if ( $old_csc =~ /\[\s*if/ ) { $old_csc = $new_csc }
                }

                # if old comment is contained in new comment,
                # only compare the common part.
                if ( length($new_csc) > length($old_csc) ) {
                    $new_csc = substr( $new_csc, 0, length($old_csc) );
                }

                # if the new comment is shorter and has been limited,
                # only compare the common part.
                if ( length($new_csc) < length($old_csc) && $new_trailing_dots )
                {
                    $old_csc = substr( $old_csc, 0, length($new_csc) );
                }

                # any remaining difference?
                if ( $new_csc ne $old_csc ) {

                    # just leave the old comment if we are below the threshold
                    # for creating side comments
                    if ( $block_line_count <
                        $rOpts->{'closing-side-comment-interval'} )
                    {
                        $token = undef;
                    }

                    # otherwise we'll make a note of it
                    else {

                        warning(
"perltidy -cscw replaced: $tokens_to_go[$max_index_to_go]\n"
                        );

                        # save the old side comment in a new trailing block comment
                        my ( $day, $month, $year ) = (localtime)[ 3, 4, 5 ];
                        $year  += 1900;
                        $month += 1;
                        $cscw_block_comment =
"## perltidy -cscw $year-$month-$day: $tokens_to_go[$max_index_to_go]";
                    }
                }
                else {

                    # No differences.. we can safely delete old comment if we
                    # are below the threshold
                    if ( $block_line_count <
                        $rOpts->{'closing-side-comment-interval'} )
                    {
                        $token = undef;
                        unstore_token_to_go()
                          if ( $types_to_go[$max_index_to_go] eq '#' );
                        unstore_token_to_go()
                          if ( $types_to_go[$max_index_to_go] eq 'b' );
                    }
                }
            }

            # switch to the new csc (unless we deleted it!)
            $tokens_to_go[$max_index_to_go] = $token if $token;
        }

        # handle case of NO existing closing side comment
        else {

            # insert the new side comment into the output token stream
            my $type                  = '#';
            my $block_type            = '';
            my $type_sequence         = '';
            my $container_environment =
              $container_environment_to_go[$max_index_to_go];
            my $level                = $levels_to_go[$max_index_to_go];
            my $slevel               = $nesting_depth_to_go[$max_index_to_go];
            my $no_internal_newlines = 0;

            my $nesting_blocks     = $nesting_blocks_to_go[$max_index_to_go];
            my $ci_level           = $ci_levels_to_go[$max_index_to_go];
            my $in_continued_quote = 0;

            # first insert a blank token
            insert_new_token_to_go( ' ', 'b', $slevel, $no_internal_newlines );

            # then the side comment
            insert_new_token_to_go( $token, $type, $slevel,
                $no_internal_newlines );
        }
    }
    return $cscw_block_comment;
}

sub send_lines_to_vertical_aligner {

    my ( $ri_first, $ri_last, $do_not_pad ) = @_;

    my $rindentation_list = [0];    # ref to indentations for each line

    set_vertical_alignment_markers( $ri_first, $ri_last );

    # flush if necessary to avoid unwanted alignment
    my $must_flush = 0;
    if ( @$ri_first > 1 ) {

        # flush before a long if statement
        if ( $types_to_go[0] eq 'k' && $tokens_to_go[0] =~ /^(if|unless)$/ ) {
            $must_flush = 1;
        }
    }
    if ($must_flush) {
        Perl::Tidy::VerticalAligner::flush();
    }

    set_logical_padding( $ri_first, $ri_last );

    # loop to prepare each line for shipment
    my $n_last_line = @$ri_first - 1;
    for my $n ( 0 .. $n_last_line ) {
        my $ibeg = $$ri_first[$n];
        my $iend = $$ri_last[$n];

        my @patterns = ();
        my @tokens   = ();
        my @fields   = ();
        my $i_start  = $ibeg;
        my $i;

        my $j = 0;    # field index

        $patterns[0] = "";
        for $i ( $ibeg .. $iend ) {

            # if we find a new synchronization token, we are done with 
            # a field

            if ( $i > $i_start && $matching_token_to_go[$i] ne '' ) {

                # make separators in different nesting depths unique
                # by appending the nesting depth digit.
                my $tok = $matching_token_to_go[$i];

                if ( $tok ne '#' ) {
                    $tok .= "$nesting_depth_to_go[$i]";
                }

                # concatenate the text of the consecutive tokens to form 
                # the field
                push ( @fields,
                    join ( '', @tokens_to_go[ $i_start .. $i - 1 ] ) );

                # store the alignment token for this field
                push ( @tokens, $tok );

                # get ready for the next batch
                $i_start = $i;
                $j++;
                $patterns[$j] = "";
            }

            # continue accumulating tokens
            # handle non-keywords..
            if ( $types_to_go[$i] ne 'k' ) {
                my $type = $types_to_go[$i];

                # Mark most things before arrows as a quote to
                # get them to line up. Testfile: mixed.pl.
                if ( ( $i < $iend - 1 ) && ( $type =~ /^[wnC]$/ ) ) {
                    my $next_type       = $types_to_go[ $i + 1 ];
                    my $i_next_nonblank =
                      ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );

                    if ( $types_to_go[$i_next_nonblank] eq '=>' ) {
                        $type = 'Q';
                    }
                }

                # minor patch to make numbers and quotes align
                if ( $type eq 'n' ) { $type = 'Q' }

                $patterns[$j] .= $type;
            }

            # for keywords we have to use the actual text
            else {

                # map certain keywords to the same 'if' class to align
                # long if/elsif sequences. my testfile: elsif.pl
                my $tok = $tokens_to_go[$i];
                if ( $n == 0 && $tok =~ /^(elsif|else|unless)$/ ) {
                    $tok = 'if';
                }
                $patterns[$j] .= $tok;
            }
        }

        # done with this line .. join text of tokens to make the last field
        push ( @fields, join ( '', @tokens_to_go[ $i_start .. $iend ] ) );

        my ( $indentation, $lev, $level_end, $is_semicolon_terminated,
            $is_outdented_line )
          = set_adjusted_indentation( $ibeg, $iend, \@fields, \@patterns,
            $ri_first, $ri_last, $rindentation_list );

        # we will allow outdenting of long lines..
        my $outdent_long_lines = (

            # which are long quotes, if allowed
            ( $types_to_go[$ibeg] eq 'Q' && $rOpts->{'outdent-long-quotes'} )

            # which are long block comments, if allowed
              || (
                   $types_to_go[$ibeg] eq '#'
                && $rOpts->{'outdent-long-comments'}

                # but not if this is a static block comment
                && !(
                       $rOpts->{'static-block-comments'}
                    && $tokens_to_go[$ibeg] =~ /$static_block_comment_pattern/o
                )
              )
        );

        my $level_jump =
          $nesting_depth_to_go[ $iend + 1 ] - $nesting_depth_to_go[$ibeg];

        my $rvertical_tightness_flags =
          set_vertical_tightness_flags( $n, $n_last_line, $ibeg, $iend,
            $ri_first, $ri_last );

        # flush an outdented line to avoid any unwanted vertical alignment
        Perl::Tidy::VerticalAligner::flush() if ($is_outdented_line);

        # send this new line down the pipe
        Perl::Tidy::VerticalAligner::append_line(
            $lev,                            $level_end,
            $indentation,                    \@fields,
            \@tokens,                        \@patterns,
            $forced_breakpoint_to_go[$iend], $outdent_long_lines,
            $is_semicolon_terminated,        $do_not_pad,
            $rvertical_tightness_flags,      $level_jump,
        );

        # flush an outdented line to avoid any unwanted vertical alignment
        Perl::Tidy::VerticalAligner::flush() if ($is_outdented_line);

        $do_not_pad = 0;

    }    # end of loop to output each line

    # remember indentation of lines containing opening containers for
    # later use by sub set_adjusted_indentation
    save_opening_indentation( $ri_first, $ri_last, $rindentation_list );
}

{        # begin unmatched_indexes

    # closure to keep track of unbalanced containers.
    # arrays shared by the routines in this block:
    my @unmatched_opening_indexes_in_this_batch;
    my @unmatched_closing_indexes_in_this_batch;

    sub is_unbalanced_batch {
        @unmatched_opening_indexes_in_this_batch +
          @unmatched_closing_indexes_in_this_batch;
    }

    sub match_opening_and_closing_tokens {

        # Match up indexes of opening and closing braces, etc, in this batch.
        # This has to be done after all tokens are stored because unstoring
        # of tokens would otherwise cause trouble.

        @unmatched_opening_indexes_in_this_batch = ();
        @unmatched_closing_indexes_in_this_batch = ();

        my ( $i, $i_mate, $token );
        foreach $i ( 0 .. $max_index_to_go ) {
            if ( $type_sequence_to_go[$i] ) {
                $token = $tokens_to_go[$i];
                if ( $token =~ /^[\(\[\{\?]$/ ) {
                    push @unmatched_opening_indexes_in_this_batch, $i;
                }
                elsif ( $token =~ /^[\)\]\}\:]$/ ) {

                    $i_mate = pop @unmatched_opening_indexes_in_this_batch;
                    if ( defined($i_mate) && $i_mate >= 0 ) {
                        if ( $type_sequence_to_go[$i_mate] ==
                            $type_sequence_to_go[$i] )
                        {
                            $mate_index_to_go[$i]      = $i_mate;
                            $mate_index_to_go[$i_mate] = $i;
                        }
                        else {
                            push @unmatched_opening_indexes_in_this_batch,
                              $i_mate;
                            push @unmatched_closing_indexes_in_this_batch, $i;
                        }
                    }
                    else {
                        push @unmatched_closing_indexes_in_this_batch, $i;
                    }
                }
            }
        }
    }

    sub save_opening_indentation {

        # This should be called after each batch of tokens is output. It
        # saves indentations of lines of all unmatched opening tokens.
        # These will be used by sub get_opening_indentation.

        my ( $ri_first, $ri_last, $rindentation_list ) = @_;

        # we no longer need indentations of any saved indentations which
        # are unmatched closing tokens in this batch, because we will
        # never encounter them again.  So we can delete them to keep
        # the hash size down.
        foreach (@unmatched_closing_indexes_in_this_batch) {
            my $seqno = $type_sequence_to_go[$_];
            delete $saved_opening_indentation{$seqno};
        }

        # we need to save indentations of any unmatched opening tokens
        # in this batch because we may need them in a subsequent batch.
        foreach (@unmatched_opening_indexes_in_this_batch) {
            my $seqno = $type_sequence_to_go[$_];
            $saved_opening_indentation{$seqno} = [
                lookup_opening_indentation(
                    $_, $ri_first, $ri_last, $rindentation_list
                )
            ];
        }
    }
}    # end unmatched_indexes

sub get_opening_indentation {

    # get the indentation of the line which output the opening token
    # corresponding to a given closing token in the current output batch.
    #
    # given:
    # $i_closing - index in this line of a closing token ')' '}' or ']'
    #
    # $ri_first - reference to list of the first index $i for each output
    #               line in this batch
    # $ri_last - reference to list of the last index $i for each output line
    #              in this batch
    # $rindentation_list - reference to a list containing the indentation
    #            used for each line.  
    #
    # return:
    #   -the indentation of the line which contained the opening token 
    #    which matches the token at index $i_opening
    #   -and its offset (number of columns) from the start of the line
    #
    my ( $i_closing, $ri_first, $ri_last, $rindentation_list ) = @_;

    # first, see if the opening token is in the current batch
    my $i_opening = $mate_index_to_go[$i_closing];
    my ( $indent, $offset );
    if ( $i_opening >= 0 ) {

        # it is..look up the indentation
        ( $indent, $offset ) =
          lookup_opening_indentation( $i_opening, $ri_first, $ri_last,
            $rindentation_list );
    }

    # if not, it should have been stored in the hash by a previous batch
    else {
        my $seqno = $type_sequence_to_go[$i_closing];
        if ($seqno) {
            if ( $saved_opening_indentation{$seqno} ) {
                ( $indent, $offset ) = @{ $saved_opening_indentation{$seqno} };
            }
        }
    }
    return ( $indent, $offset );
}

sub lookup_opening_indentation {

    # get the indentation of the line in the current output batch
    # which output a selected opening token 
    #
    # given: 
    #   $i_opening - index of an opening token in the current output batch
    #                whose line indentation we need
    #   $ri_first - reference to list of the first index $i for each output
    #               line in this batch
    #   $ri_last - reference to list of the last index $i for each output line
    #              in this batch
    #   $rindentation_list - reference to a list containing the indentation
    #            used for each line.  (NOTE: the first slot in
    #            this list is the last returned line number, and this is
    #            followed by the list of indentations).
    #
    # return
    #   -the indentation of the line which contained token $i_opening
    #   -and its offset (number of columns) from the start of the line

    my ( $i_opening, $ri_start, $ri_last, $rindentation_list ) = @_;

    my $nline = $rindentation_list->[0];    # line number of previous lookup

    # reset line location if necessary
    $nline = 0 if ( $i_opening < $ri_start->[$nline] );

    # find the correct line
    unless ( $i_opening > $ri_last->[-1] ) {
        while ( $i_opening > $ri_last->[$nline] ) { $nline++; }
    }

    # error - token index is out of bounds - shouldn't happen
    else {
        warning(
"non-fatal program bug in lookup_opening_indentation - index out of range\n"
        );
        report_definite_bug();
        $nline = $#{$ri_last};
    }

    $rindentation_list->[0] =
      $nline;    # save line number to start looking next call
    my $ibeg = $ri_start->[$nline];
    my $offset = token_sequence_length( $ibeg, $i_opening ) - 1;
    return ( $rindentation_list->[ $nline + 1 ], $offset );
}

sub set_adjusted_indentation {

    # This routine has the final say regarding the actual indentation of
    # a line.  It starts with the basic indentation which has been
    # defined for the leading token, and then takes into account any
    # options that the user has set regarding special indenting and
    # outdenting.

    my ( $ibeg, $iend, $rfields, $rpatterns, $ri_first, $ri_last,
        $rindentation_list )
      = @_;

    # we need to know the last token of this line
    my ( $terminal_type, $i_terminal ) =
      terminal_type( \@types_to_go, \@block_type_to_go, $ibeg, $iend );

    my $is_outdented_line = 0;

    my $is_semicolon_terminated = $terminal_type eq ';'
      && $nesting_depth_to_go[$iend] < $nesting_depth_to_go[$ibeg];

    # Most lines are indented according to the initial token.
    # But it is common to outdent to the level just after the
    # terminal token in certain cases...
    # adjust_indentation flag:
    #       0 - do not adjust
    #       1 - outdent
    #      -1 - indent
    my $adjust_indentation = 0;

    my ( $opening_indentation, $opening_offset );

    # if we are at a closing token of some type..
    if ( $types_to_go[$ibeg] =~ /^[\)\}\]]$/ ) {

        # get the indentation of the line containing the corresponding
        # opening token
        ( $opening_indentation, $opening_offset ) =
          get_opening_indentation( $ibeg, $ri_first, $ri_last,
            $rindentation_list );

        # First set the default behavior:
        # default behavior is to outdent closing lines 
        # of the form:   ");  };  ];  )->xxx;"
        if (
            $is_semicolon_terminated

            # and 'cuddled parens' of the form:   ")->pack("  
            || (
                   $terminal_type eq '('
                && $types_to_go[$ibeg] eq ')'
                && ( $nesting_depth_to_go[$iend] + 1 ==
                    $nesting_depth_to_go[$ibeg] )
            )
          )
        {
            $adjust_indentation = 1;
        }

        # TESTING: outdent something like '),'
        if (
            $terminal_type eq ','

            # allow just one character before the comma
            && $i_terminal == $ibeg + 1

            # requre LIST environment; otherwise, we may outdent too much --
            # this can happen in calls without parentheses (overload.t);
            && $container_environment_to_go[$i_terminal] eq 'LIST'
          )
        {
            $adjust_indentation = 1;
        }

        # undo continuation indentation of a terminal closing token if
        # it is the last token before a level decrease.  This will allow
        # a closing token to line up with its opening counterpart, and
        # avoids a indentation jump larger than 1 level.
        if (   $types_to_go[$i_terminal] =~ /^[\}\]\)R]$/
            && $i_terminal == $ibeg )
        {
            my $ci              = $ci_levels_to_go[$ibeg];
            my $lev             = $levels_to_go[$ibeg];
            my $next_type       = $types_to_go[ $ibeg + 1 ];
            my $i_next_nonblank =
              ( ( $next_type eq 'b' ) ? $ibeg + 2 : $ibeg + 1 );
            if (   $i_next_nonblank <= $max_index_to_go
                && $levels_to_go[$i_next_nonblank] < $lev )
            {
                $adjust_indentation = 1;
            }
        }

        # Now modify default behavior according to user request:
        # handle option to indent non-blocks of the form );  };  ];
        if ( !$block_type_to_go[$ibeg] ) {
            if (   $rOpts->{'indent-closing-paren'}
                && $is_semicolon_terminated
                && $i_terminal == $ibeg + 1 )
            {
                $adjust_indentation = -1;
            }
        }

        # handle option to indent blocks 
        else {
            if (
                $rOpts->{'indent-closing-brace'}
                && (
                    $i_terminal == $ibeg    #  isolated terminal '}'
                    || $is_semicolon_terminated
                )
              )                             #  } xxxx ;
            {
                $adjust_indentation = -1;
            }
        }

    }

    # if at ');', '};', '>;', and '];' of a terminal qw quote
    elsif ( $$rpatterns[0] =~ /^qb*;$/ && $$rfields[0] =~ /^[\)\}\]\>];$/ ) {
        if ( !$rOpts->{'indent-closing-paren'} ) {
            $adjust_indentation = 1;
        }
        else {
            $adjust_indentation = -1;
        }
    }

    # Handle variation in indentation styles...
    # Select the indentation object to define leading
    # whitespace.  If we are outdenting something like '} } );'
    # then we want to use one level below the last token
    # ($i_terminal) in order to get it to fully outdent through
    # all levels.
    my $indentation;
    my $lev;
    my $level_end = $levels_to_go[$iend];

    if ( $adjust_indentation == 0 ) {
        $indentation = $leading_spaces_to_go[$ibeg];
        $lev         = $levels_to_go[$ibeg];
    }
    elsif ( $adjust_indentation == 1 ) {
        $indentation = $reduced_spaces_to_go[$i_terminal];
        $lev         = $levels_to_go[$i_terminal];
    }
    else {

        # There are two ways to handle -icb and -icp...
        # One way is to use the indentation of the previous line:
        # $indentation = $last_indentation_written;

        # The other way is to use the indentation that the previous line
        # would have had if it hadn't been adjusted:
        $indentation = $last_unadjusted_indentation;

        # Current method: use the minimum of the two. This avoids inconsistent
        # indentation.
        if ( get_SPACES($last_indentation_written) < get_SPACES($indentation) )
        {
            $indentation = $last_indentation_written;
        }

        # use previous indentation but use own level 
        # to cause list to be flushed properly
        $lev = $levels_to_go[$ibeg];
    }

    # remember indentation except for multi-line quotes, which get
    # no indentation
    unless ( $types_to_go[$ibeg] eq 'Q' && $lev == 0 ) {
        $last_indentation_written    = $indentation;
        $last_unadjusted_indentation = $leading_spaces_to_go[$ibeg];
    }

    # be sure lines with leading closing tokens are not outdented more
    # than the line which contained the corresponding opening token.
    my $is_isolated_block_brace =
      ( $iend == $ibeg ) && $block_type_to_go[$ibeg];
    if ( !$is_isolated_block_brace && defined($opening_indentation) ) {
        if ( get_SPACES($opening_indentation) > get_SPACES($indentation) ) {
            $indentation = $opening_indentation;
        }
    }

    # remember the indentation of each line of this batch
    push @{$rindentation_list}, $indentation;

    # outdent lines with certain leading tokens...
    if (

        # must be first word of this batch
        $ibeg == 0

        # and ...
        && (

            # certain leading keywords if requested
            (
                   $rOpts->{'outdent-keywords'}
                && $types_to_go[$ibeg] eq 'k'
                && $outdent_keyword{ $tokens_to_go[$ibeg] }
            )

            # or labels if requested
            || ( $rOpts->{'outdent-labels'} && $types_to_go[$ibeg] eq 'J' )

            # or static block comments if requested
            || (   $types_to_go[$ibeg] eq '#'
                && $rOpts->{'outdent-static-block-comments'}
                && $tokens_to_go[$ibeg] =~ /$static_block_comment_pattern/o
                && $rOpts->{'static-block-comments'} )
        )
      )

    {
        my $space_count = leading_spaces_to_go($ibeg);
        if ( $space_count > 0 ) {
            $space_count -= $rOpts_continuation_indentation;
            $is_outdented_line = 1;
            if ( $space_count < 0 ) { $space_count = 0 }

            # do not promote a spaced static block comment to non-spaced;
            # this is not normally necessary but could be for some
            # unusual user inputs (such as -ci = -i)
            if ( $types_to_go[$ibeg] eq '#' && $space_count == 0 ) {
                $space_count = 1;
            }

            if ($rOpts_line_up_parentheses) {
                $indentation =
                  new_lp_indentation_item( $space_count, $lev, 0, 0, 0 );
            }
            else {
                $indentation = $space_count;
            }
        }
    }

    return ( $indentation, $lev, $level_end, $is_semicolon_terminated,
        $is_outdented_line );
}

sub set_vertical_tightness_flags {

    my ( $n, $n_last_line, $ibeg, $iend, $ri_first, $ri_last ) = @_;

    # Define vertical tightness controls for the nth line of a batch.
    # We create an array of parameters which tell the vertical aligner
    # if we should combine this line with the next line to achieve the
    # desired vertical tightness.  The array of parameters contains:
    #
    #   [0] type: 1=is opening tok 2=is closing tok  3=is opening block brace
    #   [1] flag: if opening: 1=no multiple steps, 2=multiple steps ok
    #             if closing: spaces of padding to use
    #   [2] sequence number of container 
    #   [3] valid flag: do not append if this flag is false. Will be
    #       true if appropriate -vt flag is set.  Otherwise, Will be
    #       made true only for 2 line container in parens with -lp
    #
    # These flags are used by sub set_leading_whitespace in
    # the vertical aligner

    my $rvertical_tightness_flags;

    # For non-BLOCK tokens, we will need to examine the next line
    # too, so we won't consider the last line.
    if ( $n < $n_last_line ) {

        # see if last token is an opening token...not a BLOCK...
        my $ibeg_next = $$ri_first[ $n + 1 ];
        my $token_end = $tokens_to_go[$iend];
        my $iend_next = $$ri_last[ $n + 1 ];
        if (
               $type_sequence_to_go[$iend]
            && !$block_type_to_go[$iend]
            && $is_opening_token{$token_end}
            && (
                $opening_vertical_tightness{$token_end} > 0

                # allow 2-line method call to be closed up
                || (   $rOpts_line_up_parentheses
                    && $token_end eq '('
                    && $iend > $ibeg
                    && $types_to_go[ $iend - 1 ] ne 'b' )
            )
          )
        {

            # avoid multiple jumps in nesting depth in one line if
            # requested
            my $ovt       = $opening_vertical_tightness{$token_end};
            my $iend_next = $$ri_last[ $n + 1 ];
            unless (
                $ovt < 2
                && ( $nesting_depth_to_go[ $iend_next + 1 ] !=
                    $nesting_depth_to_go[$ibeg_next] )
              )
            {

                # If -vt flag has not been set, mark this as invalid
                # and aligner will validate it if it sees the closing paren
                # within 2 lines.
                my $valid_flag = $ovt;
                @{$rvertical_tightness_flags} =
                  ( 1, $ovt, $type_sequence_to_go[$iend], $valid_flag );
            }
        }

        # see if first token of next line is a closing token...
        # ..and be sure this line does not have a side comment
        my $token_next = $tokens_to_go[$ibeg_next];
        if (   $type_sequence_to_go[$ibeg_next]
            && !$block_type_to_go[$ibeg_next]
            && $is_closing_token{$token_next}
            && $types_to_go[$iend] !~ '#' )    # for safety, shouldn't happen!
        {
            my $ovt = $opening_vertical_tightness{$token_next};
            my $cvt = $closing_vertical_tightness{$token_next};
            if (

                # never append a trailing line like   )->pack(
                # because it will throw off later alignment
                (
                    $nesting_depth_to_go[$ibeg_next] ==
                    $nesting_depth_to_go[ $iend_next + 1 ] + 1
                )
                && (
                    $cvt == 2
                    || (
                        $container_environment_to_go[$ibeg_next] ne 'LIST'
                        && (
                            $cvt == 1

                            # allow closing up 2-line method calls
                            || (   $rOpts_line_up_parentheses
                                && $token_next eq ')' )
                        )
                    )
                )
              )
            {

                # decide which trailing closing tokens to append..
                my $ok = 0;
                if ( $cvt == 2 || $iend_next == $ibeg_next ) { $ok = 1 }
                else {
                    my $str =
                      join ( '',
                        @types_to_go[ $ibeg_next + 1 .. $ibeg_next + 2 ] );

                    # append closing token if followed by comment or ';' 
                    if ( $str =~ /^b?[#;]/ ) { $ok = 1 }
                }

                if ($ok) {
                    my $valid_flag = $cvt;
                    @{$rvertical_tightness_flags} = (
                        2,
                        $tightness{$token_next} == 2 ? 0 : 1,
                        $type_sequence_to_go[$ibeg_next], $valid_flag,
                    );
                }
            }
        }
    }

    # Check for a last line with isolated opening BLOCK curly
    elsif ($rOpts_block_brace_vertical_tightness
        && $ibeg eq $iend
        && $types_to_go[$iend] eq '{'
        && $block_type_to_go[$iend] =~
        /$block_brace_vertical_tightness_pattern/o )
    {
        @{$rvertical_tightness_flags} =
          ( 3, $rOpts_block_brace_vertical_tightness, 0, 1 );
    }

    return $rvertical_tightness_flags;
}

{
    my %is_vertical_alignment_type;
    my %is_vertical_alignment_keyword;

    BEGIN {
        @_ = qw#{ ? : => = += -= =~ *= /= && || ||= #;
        @is_vertical_alignment_type{@_} = (1) x scalar(@_);

        @_ = qw(if unless and or eq ne);
        @is_vertical_alignment_keyword{@_} = (1) x scalar(@_);
    }

    sub set_vertical_alignment_markers {

        # Look at the tokens in this output batch and define the array
        # 'matching_token_to_go' which marks tokens at which we would
        # accept vertical alignment.

        # nothing to do if we aren't allowed to change whitespace
        if ( !$rOpts_add_whitespace ) {
            for my $i ( 0 .. $max_index_to_go ) {
                $matching_token_to_go[$i] = '';
            }
            return;
        }

        my ( $ri_first, $ri_last ) = @_;

        # look at each line of this batch..
        my $last_vertical_alignment_before_index;
        my $vert_last_nonblank_type;
        my $vert_last_nonblank_block_type;
        my $max_line = @$ri_first - 1;
        my ( $i, $type, $token, $block_type, $last_nonblank_token,
            $alignment_type );
        my ( $ibeg, $iend, $line );
        foreach $line ( 0 .. $max_line ) {
            $ibeg                                 = $$ri_first[$line];
            $iend                                 = $$ri_last[$line];
            $last_vertical_alignment_before_index = -1;
            $vert_last_nonblank_type              = '';
            $vert_last_nonblank_block_type        = '';

            # look at each token in this output line..
            foreach $i ( $ibeg .. $iend ) {
                $alignment_type = '';
                $type           = $types_to_go[$i];
                $block_type     = $block_type_to_go[$i];
                $token          = $tokens_to_go[$i];

                # check for flag indicating that we should not align
                # this token
                if ( $matching_token_to_go[$i] ) {
                    $matching_token_to_go[$i] = '';
                    next;
                }

                #--------------------------------------------------------
                # First see if we want to align BEFORE this token
                #--------------------------------------------------------

                # The first possible token that we can align before
                # is index 2 because: 1) it doesn't normally make sense to
                # align before the first token and 2) the second
                # token must be a blank if we are to align before
                # the third
                if ( $i < $ibeg + 2 ) {
                }

                # must follow a blank token
                elsif ( $types_to_go[ $i - 1 ] ne 'b' ) {
                }

                # align a side comment --
                elsif ( $type eq '#' ) {

                    unless (

                        # it is a static side comment
                        (
                               $rOpts->{'static-side-comments'}
                            && $token =~ /$static_side_comment_pattern/o
                        )

                        # or a closing side comment
                        || (   $vert_last_nonblank_block_type
                            && $token =~
                            /$closing_side_comment_prefix_pattern/o )
                      )
                    {
                        $alignment_type = $type;
                    }    ## Example of a static side comment
                }

                # otherwise, do not align two in a row to create a
                # blank field
                elsif ( $last_vertical_alignment_before_index == $i - 2 ) {
                }

                # align before one of these keywords 
                # (within a line, since $i>1)
                elsif ( $type eq 'k' ) {

                    #  /^(if|unless|and|or|eq|ne)$/
                    if ( $is_vertical_alignment_keyword{$token} ) {
                        $alignment_type = $token;
                    }
                }

                # We have to be very careful about alignment before opening parens.
                # It is ok to line up sequences like this:
                #    if    ( $something eq "simple" )  { &handle_simple }
                #    elsif ( $something eq "hard" )    { &handle_hard }
                elsif ( $type eq '(' ) {
                    if ( ( $i == $ibeg + 2 )
                        && $tokens_to_go[$ibeg] =~ /^(if|elsif)/ )
                    {
                        $alignment_type = $type;
                    }
                }

                # align before one of these types..
                # Note: add '.' after new vertical aligner is operational
                elsif ( $is_vertical_alignment_type{$type} ) {
                    $alignment_type = $token;

                    # be sure the alignment tokens are unique
                    # This didn't work well: reason not determined
                    # if ($token ne $type) {$alignment_type .= $type}  
                }

                # NOTE: This is deactivated until the new vertical aligner
                # is finished because it causes the previous if/elsif alignment 
                # to fail
                #elsif ( $type eq '}' && $token eq '}' && $block_type_to_go[$i]) {
                #    $alignment_type = $type;
                #}

                if ($alignment_type) {
                    $last_vertical_alignment_before_index = $i;
                }

                #--------------------------------------------------------
                # Next see if we want to align AFTER the previous nonblank
                #--------------------------------------------------------

                # We want to line up ',' and interior ';' tokens, with the added
                # space AFTER these tokens.  (Note: interior ';' is included
                # because it may occur in short blocks).
                if (

                    # we haven't already set it
                    !$alignment_type

                    # and its not the first token of the line
                    && ( $i > $ibeg )

                    # and it follows a blank
                    && $types_to_go[ $i - 1 ] eq 'b'

                    # and previous token IS one of these:
                    && ( $vert_last_nonblank_type =~ /^[\,\;]$/ )

                    # and it's NOT one of these
                    && ( $type !~ /^[b\#\)\]\}]$/ )

                    # then go ahead and align 
                  )

                {
                    $alignment_type = $vert_last_nonblank_type;
                }

                #--------------------------------------------------------
                # then store the value
                #--------------------------------------------------------
                $matching_token_to_go[$i] = $alignment_type;
                if ( $type ne 'b' ) {
                    $vert_last_nonblank_type       = $type;
                    $vert_last_nonblank_block_type = $block_type;
                }
            }
        }
    }
}

sub terminal_type {

    #    returns type of last token on this line (terminal token), as follows:
    #    returns # for a full-line comment
    #    returns ' ' for a blank line
    #    otherwise returns final token type

    my ( $rtype, $rblock_type, $ibeg, $iend ) = @_;

    # check for full-line comment..
    if ( $$rtype[$ibeg] eq '#' ) {
        return wantarray ? ( $$rtype[$ibeg], $ibeg ) : $$rtype[$ibeg];
    }
    else {

        # start at end and walk bakwards..
        for ( my $i = $iend ; $i >= $ibeg ; $i-- ) {

            # skip past any side comment and blanks
            next if ( $$rtype[$i] eq 'b' );
            next if ( $$rtype[$i] eq '#' );

            # found it..make sure it is a BLOCK termination,
            # but hide a terminal } after sort/grep/map because it is not
            # necessarily the end of the line.  (terminal.t)
            my $terminal_type = $$rtype[$i];
            if (
                $terminal_type eq '}'
                && ( !$$rblock_type[$i]
                    || ( $$rblock_type[$i] =~ /^(sort|grep|map|do|eval)$/ ) )
              )
            {
                $terminal_type = 'b';
            }
            return wantarray ? ( $terminal_type, $i ) : $terminal_type;
        }

        # empty line
        return wantarray ? ( ' ', $ibeg ) : ' ';
    }
}

sub set_bond_strengths {

    BEGIN {

        ###############################################################
        # NOTE: NO_BREAK's set here are HINTS which may not be honored; 
        # essential NO_BREAKS's must be enforced in section 2, below.
        ###############################################################

        # adding NEW_TOKENS: add a left and right bond strength by
        # mimmicking what is done for an existing token type.  You
        # can skip this step at first and take the default, then
        # tweak later to get desired results.

        # The bond strengths should roughly follow precenence order where
        # possible.  If you make changes, please check the results very
        # carefully on a variety of scripts.

        # no break around possible filehandle
        $left_bond_strength{'Z'}  = NO_BREAK;
        $right_bond_strength{'Z'} = NO_BREAK;

        # never put a bare word on a new line:
        # example print (STDERR, "bla"); will fail with break after (
        $left_bond_strength{'w'} = NO_BREAK;

        # blanks always have infinite strength to force breaks after real tokens
        $right_bond_strength{'b'} = NO_BREAK;

        # try not to break on exponentation
        @_ = qw" ** .. ... <=> ";
        @left_bond_strength{@_}  = (STRONG) x scalar(@_);
        @right_bond_strength{@_} = (STRONG) x scalar(@_);

        # The comma-arrow has very low precedence but not a good break point
        $left_bond_strength{'=>'}  = NO_BREAK;
        $right_bond_strength{'=>'} = NOMINAL;

        # ok to break after label
        $left_bond_strength{'J'}  = NO_BREAK;
        $right_bond_strength{'J'} = NOMINAL;
        $left_bond_strength{'j'}  = STRONG;
        $right_bond_strength{'j'} = STRONG;

        $left_bond_strength{'->'}  = STRONG;
        $right_bond_strength{'->'} = VERY_STRONG;

        # breaking AFTER these is just ok:
        @_ = qw" % + - * / x  ";
        @left_bond_strength{@_}  = (STRONG) x scalar(@_);
        @right_bond_strength{@_} = (NOMINAL) x scalar(@_);

        # breaking BEFORE these is just ok:
        @_ = qw" >> << ";
        @right_bond_strength{@_} = (STRONG) x scalar(@_);
        @left_bond_strength{@_}  = (NOMINAL) x scalar(@_);

        # I prefer breaking before the string concatenation operator
        # because it can be hard to see at the end of a line
        # swap these to break after a '.' 
        # this could be a future option
        $right_bond_strength{'.'} = STRONG;
        $left_bond_strength{'.'}  = 0.9 * NOMINAL + 0.1 * WEAK;

        @_ = qw"} ] ) ";
        @left_bond_strength{@_}  = (STRONG) x scalar(@_);
        @right_bond_strength{@_} = (NOMINAL) x scalar(@_);

        # make these a little weaker than nominal so that they get
        # favored for end-of-line characters
        @_ = qw"!= == =~ !~";
        @left_bond_strength{@_}  = (STRONG) x scalar(@_);
        @right_bond_strength{@_} = ( 0.9 * NOMINAL + 0.1 * WEAK ) x scalar(@_);

        # break AFTER these 
        @_ = qw" < >  | & >= <=";
        @left_bond_strength{@_}  = (VERY_STRONG) x scalar(@_);
        @right_bond_strength{@_} = ( 0.8 * NOMINAL + 0.2 * WEAK ) x scalar(@_);

        # breaking either before or after a quote is ok
        # but bias for breaking before a quote
        $left_bond_strength{'Q'}  = NOMINAL;
        $right_bond_strength{'Q'} = NOMINAL + 0.02;
        $left_bond_strength{'q'}  = NOMINAL;
        $right_bond_strength{'q'} = NOMINAL;

        # starting a line with a keyword is usually ok
        $left_bond_strength{'k'} = NOMINAL;

        # we usually want to bond a keyword strongly to what immediately
        # follows, rather than leaving it stranded at the end of a line
        $right_bond_strength{'k'} = STRONG;

        $left_bond_strength{'G'}  = NOMINAL;
        $right_bond_strength{'G'} = STRONG;

        # it is very good to break AFTER various assignment operators
        @_ = qw(
          = **= += *= &= <<= &&=
          -= /= |= >>= ||=
          .= %= ^=
          x=
        );
        @left_bond_strength{@_}  = (STRONG) x scalar(@_);
        @right_bond_strength{@_} =
          ( 0.4 * WEAK + 0.6 * VERY_WEAK ) x scalar(@_);

        # break BEFORE '&&' and '||' 
        # set strength of '||' to same as '=' so that chains like
        # $a = $b || $c || $d   will break before the first '||'
        $right_bond_strength{'||'} = NOMINAL;
        $left_bond_strength{'||'}  = $right_bond_strength{'='};

        # set strength of && a little higher than ||
        $right_bond_strength{'&&'} = NOMINAL;
        $left_bond_strength{'&&'}  = $left_bond_strength{'||'} + 0.1;

        $left_bond_strength{';'}  = VERY_STRONG;
        $right_bond_strength{';'} = VERY_WEAK;
        $left_bond_strength{'f'}  = VERY_STRONG;

        # make right strength of for ';' a little less than '='
        # to make for contents break after the ';' to avoid this:
        #   for ( $j = $number_of_fields - 1 ; $j < $item_count ; $j +=
        #     $number_of_fields )
        # and make it weaker than ',' and 'and' too
        $right_bond_strength{'f'} = VERY_WEAK - 0.03;

        # The strengths of ?/: should be somewhere between 
        # an '=' and a quote (NOMINAL),
        # make strength of ':' slightly less than '?' to help
        # break long chains of ? : after the colons
        $left_bond_strength{':'}  = 0.4 * WEAK + 0.6 * NOMINAL;
        $right_bond_strength{':'} = NO_BREAK;
        $left_bond_strength{'?'}  = $left_bond_strength{':'} + 0.01;
        $right_bond_strength{'?'} = NO_BREAK;

        $left_bond_strength{','}  = VERY_STRONG;
        $right_bond_strength{','} = VERY_WEAK;
    }

    # patch-its always ok to break at end of line
    $nobreak_to_go[$max_index_to_go] = 0;

    # adding a small 'bias' to strengths is a simple way to make a line
    # break at the first of a sequence of identical terms.  For example,
    # to force long string of conditional operators to break with 
    # each line ending in a ':', we can add a small number to the bond
    # strength of each ':'
    my $colon_bias = 0;
    my $amp_bias   = 0;
    my $bar_bias   = 0;
    my $and_bias   = 0;
    my $or_bias    = 0;
    my $dot_bias   = 0;
    my $f_bias     = 0;
    my $code_bias  = -.01;
    my $type       = 'b';
    my $token      = ' ';
    my $last_type;
    my $last_nonblank_type  = $type;
    my $last_nonblank_token = $token;
    my $delta_bias          = 0.0001;
    my $list_str            = $left_bond_strength{'?'};

    my ( $block_type, $i_next, $i_next_nonblank, $next_nonblank_token,
        $next_nonblank_type, $next_token, $next_type, $total_nesting_depth, );

    # preliminary loop to compute bond strengths
    for ( my $i = 0 ; $i <= $max_index_to_go ; $i++ ) {
        $last_type = $type;
        if ( $type ne 'b' ) {
            $last_nonblank_type  = $type;
            $last_nonblank_token = $token;
        }
        $type = $types_to_go[$i];

        # strength on both sides of a blank is the same
        if ( $type eq 'b' && $last_type ne 'b' ) {
            $bond_strength_to_go[$i] = $bond_strength_to_go[ $i - 1 ];
            next;
        }

        $token               = $tokens_to_go[$i];
        $block_type          = $block_type_to_go[$i];
        $i_next              = $i + 1;
        $next_type           = $types_to_go[$i_next];
        $next_token          = $tokens_to_go[$i_next];
        $total_nesting_depth = $nesting_depth_to_go[$i_next];
        $i_next_nonblank     = ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );
        $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        # Some token chemistry...  The decision about where to break a
        # line depends upon a "bond strength" between tokens.  The LOWER
        # the bond strength, the MORE likely a break.  The strength
        # values are based on trial-and-error, and need to be tweaked
        # occasionally to get desired results.  Things to keep in mind
        # are:
        #   1. relative strengths are important.  small differences
        #      in strengths can make big formatting differences.
        #   2. each indentation level adds one unit of bond strength
        #   3. a value of NO_BREAK makes an unbreakable bond
        #   4. a value of VERY_WEAK is the strength of a ','
        #   5. values below NOMINAL are considered ok break points
        #   6. values above NOMINAL are considered poor break points
        # We are computing the strength of the bond between the current
        # token and the NEXT token.
        my $bond_str = VERY_STRONG;    # a default, high strength

        #---------------------------------------------------------------
        # section 1:
        # use minimum of left and right bond strengths if defined;
        # digraphs and trigraphs like to break on their left
        #---------------------------------------------------------------
        my $bsr = $right_bond_strength{$type};

        if ( !defined($bsr) ) {

            if ( $is_digraph{$type} || $is_trigraph{$type} ) {
                $bsr = STRONG;
            }
            else {
                $bsr = VERY_STRONG;
            }
        }

        if ( $token eq 'and' or $token eq 'or' ) {
            $bsr = NOMINAL;
        }
        elsif ( $token eq 'ne' or $token eq 'eq' ) {
            $bsr = NOMINAL;
        }
        my $bsl = $left_bond_strength{$next_nonblank_type};

        # set terminal bond strength to the nominal value
        # this will cause good preceding breaks to be retained
        if ( $i_next_nonblank > $max_index_to_go ) {
            $bsl = NOMINAL;
        }

        if ( !defined($bsl) ) {

            if (   $is_digraph{$next_nonblank_type}
                || $is_trigraph{$next_nonblank_type} )
            {
                $bsl = WEAK;
            }
            else {
                $bsl = VERY_STRONG;
            }
        }

        # make or, and slightly weaker than a ','
        if ( $next_nonblank_token eq 'or' ) {
            $bsl = VERY_WEAK - 0.02;
        }
        if ( $next_nonblank_token eq 'and' ) {
            $bsl = VERY_WEAK - 0.01;
        }
        elsif ( $next_nonblank_token eq 'ne' or $next_nonblank_token eq 'eq' ) {
            $bsl = NOMINAL;
        }
        elsif ( $next_nonblank_token =~ /^(lt|gt|le|ge)$/ ) {
            $bsl = 0.9 * NOMINAL + 0.1 * STRONG;
        }

        # Note: it might seem that we would want to keep a NO_BREAK if
        # either token has this value.  This didn't work, because in an
        # arrow list, it prevents the comma from separating from the
        # following bare word (which is probably quoted by its arrow).
        # So necessary NO_BREAK's have to be handled as special cases
        # in the final section.
        $bond_str = ( $bsr < $bsl ) ? $bsr : $bsl;
        my $bond_str_1 = $bond_str;

        #---------------------------------------------------------------
        # section 2:
        # special cases
        #---------------------------------------------------------------

        # allow long lines before final { in an if statement, as in:
        #    if (..........
        #      ..........)
        #    {
        #
        # Otherwise, the line before the { tends to be too short.
        if ( $type eq ')' ) {
            if ( $next_nonblank_type eq '{' ) {
                $bond_str = VERY_WEAK + 0.03;
            }
        }

        elsif ( $type eq '(' ) {
            if ( $next_nonblank_type eq '{' ) {
                $bond_str = NOMINAL;
            }
        }

        # break on something like '} (', but keep this stronger than a ','
        # example is in 'howe.pl'
        elsif ( $type eq 'R' or $type eq '}' ) {
            if ( $next_nonblank_type eq '(' ) {
                $bond_str = 0.8 * VERY_WEAK + 0.2 * WEAK;
            }
        }

        #-----------------------------------------------------------------
        # adjust bond strength bias
        #-----------------------------------------------------------------

        elsif ( $type eq 'f' ) {
            $bond_str += $f_bias;
            $f_bias   += $delta_bias;
        }

        # in long ?: conditionals, bias toward just one set per line (colon.t)
        elsif ( $type eq ':' ) {
            if ( !$want_break_before{$type} ) {
                $bond_str   += $colon_bias;
                $colon_bias += $delta_bias;
            }
        }

        if (   $next_nonblank_type eq ':'
            && $want_break_before{$next_nonblank_type} )
        {
            $bond_str   += $colon_bias;
            $colon_bias += $delta_bias;
        }

        # if leading '.' is used, align all but 'short' quotes;
        # the idea is to not place something like "\n" on a single line.
        elsif ( $next_nonblank_type eq '.' ) {
            if ( $want_break_before{'.'} ) {
                unless (
                    $last_nonblank_type eq '.'
                    && (
                        length($token) <=
                        $rOpts_short_concatenation_item_length )
                    && ( $token !~ /^[\)\]\}]$/ )
                  )
                {
                    $dot_bias += $delta_bias;
                }
                $bond_str += $dot_bias;
            }
        }
        elsif ( $next_nonblank_type eq '&&' ) {
            $bond_str += $amp_bias;
            $amp_bias += $delta_bias;
        }
        elsif ( $next_nonblank_type eq '||' ) {
            $bond_str += $bar_bias;
            $bar_bias += $delta_bias;
        }
        elsif ( $next_nonblank_type eq 'k' ) {

            if ( $next_nonblank_token eq 'and' ) {
                $bond_str += $and_bias;
                $and_bias += $delta_bias;
            }
            elsif ( $next_nonblank_token eq 'or' ) {
                $bond_str += $or_bias;
                $or_bias  += $delta_bias;
            }

            # FIXME: needs more testing
            elsif ( $is_keyword_returning_list{$next_nonblank_token} ) {
                $bond_str = $list_str if ( $bond_str > $list_str );
            }
        }

        # keep matrix and hash indices together
        # but make them a little below STRONG to allow breaking open
        # something like {'some-word'}{'some-very-long-word'} at the }{
        # (bracebrk.t)
        if (   ( $type eq ']' or $type eq 'R' )
            && ( $next_nonblank_type eq '[' or $next_nonblank_type eq 'L' ) )
        {
            $bond_str = 0.9 * STRONG + 0.1 * NOMINAL;
        }

        if ( $next_nonblank_type eq 'i' && $next_nonblank_token =~ /^->/ ) {

            # increase strength to the point where a break in the following
            # will be after the opening paren rather than at the arrow:
            #    $a->$b($c);
            if ( $type eq 'i' ) {
                $bond_str = 1.45 * STRONG;
            }

            elsif ( $type =~ /^[\)\]\}R]$/ ) {
                $bond_str = 0.1 * STRONG + 0.9 * NOMINAL;
            }

            # otherwise make strength before an '->' a little over a '+'
            else {
                if ( $bond_str <= NOMINAL ) {
                    $bond_str = NOMINAL + 0.01;
                }
            }
        }

        if ( $token eq ')' && $next_nonblank_token eq '[' ) {
            $bond_str = 0.2 * STRONG + 0.8 * NOMINAL;
        }

        # map1.t -- correct for a quirk in perl
        if (   $token eq '('
            && $next_nonblank_type eq 'i'
            && $last_nonblank_type eq 'k'
            && $is_sort_map_grep{$last_nonblank_token} )

          #     /^(sort|map|grep)$/ )
        {
            $bond_str = NO_BREAK;
        }

        # extrude.t: do not break before paren at:
        #    -l pid_filename(
        if ( $last_nonblank_type eq 'F' && $next_nonblank_token eq '(' ) {
            $bond_str = NO_BREAK;
        }

        # good to break after end of code blocks
        if ( $type eq '}' && $block_type ) {

            $bond_str = 0.5 * WEAK + 0.5 * VERY_WEAK + $code_bias;
            $code_bias += $delta_bias;
        }

        if ( $type eq 'k' ) {

            # allow certain control keywords to stand out
            if (   ( $next_nonblank_type eq 'k' )
                && ( $token =~ /^(last|next|redo|return)$/ ) )
            {
                $bond_str = 0.45 * WEAK + 0.55 * VERY_WEAK;
            }

            # Don't break after keyword my.  This is a quick fix for a
            # rare problem with perl. An example is this line from file 
            # Container.pm:
            # foreach my $question( Debian::DebConf::ConfigDb::gettree( $this->{'question'} ) )

            if ( $token eq 'my' ) {
                $bond_str = NO_BREAK;
            }

        }

        # good to break before 'if', 'unless', etc
        if ( $is_if_brace_follower{$next_nonblank_token} ) {
            $bond_str = VERY_WEAK;
        }

        if ( $next_nonblank_type eq 'k' ) {

            # keywords like 'unless' 'if' make good breaks
            if ( $is_do_follower{$next_nonblank_token} ) {
                $bond_str = VERY_WEAK / 1.05;
            }

        }

        # try not to break before a comma-arrow
        elsif ( $next_nonblank_type eq '=>' ) {
            if ( $bond_str < STRONG ) { $bond_str = STRONG }
        }

        #----------------------------------------------------------------------
        # only set NO_BREAK's from here on
        #----------------------------------------------------------------------
        if ( $type eq 'C' or $type eq 'U' ) {

            # use strict requires that bare word and => not be separated
            if ( $next_nonblank_type eq '=>' ) {
                $bond_str = NO_BREAK;
            }

        }

        # use strict requires that bare word within braces not start new line
        elsif ( $type eq 'L' ) {

            if ( $next_nonblank_type eq 'w' ) {
                $bond_str = NO_BREAK;
            }
        }

        # in older version of perl, use strict can cause problems with 
        # breaks before bare words following opening parens.  For example,
        # this will fail under older versions if a break is made between
        # '(' and 'MAIL':
        #  use strict;
        #  open( MAIL, "a long filename or command");
        #  close MAIL;
        elsif ( $type eq '{' ) {

            if ( $token eq '(' && $next_nonblank_type eq 'w' ) {

                # but it's fine to break if the word is followed by a '=>'
                # or if it is obviously a sub call
                my $i_next_next_nonblank = $i_next_nonblank + 1;
                my $next_next_type       = $types_to_go[$i_next_next_nonblank];
                if (   $next_next_type eq 'b'
                    && $i_next_nonblank < $max_index_to_go )
                {
                    $i_next_next_nonblank++;
                    $next_next_type = $types_to_go[$i_next_next_nonblank];
                }

                ##if ( $next_next_type ne '=>' ) {
                # these are ok: '->xxx', '=>', '('

                # We'll check for an old breakpoint and keep a leading
                # bareword if it was that way in the input file.  Presumably
                # it was ok that way.  For example, the following would remain
                # unchanged:
                #
                # @months = (
                #   January,   February, March,    April,
                #   May,       June,     July,     August,
                #   September, October,  November, December,
                # );
                #
                # This should be sufficient:
                if ( !$old_breakpoint_to_go[$i]
                    && ( $next_next_type eq ',' || $next_next_type eq '}' ) )
                {
                    $bond_str = NO_BREAK;
                }
            }
        }

        elsif ( $type eq 'w' ) {

            if ( $next_nonblank_type eq 'R' ) {
                $bond_str = NO_BREAK;
            }

            # use strict requires that bare word and => not be separated
            if ( $next_nonblank_type eq '=>' ) {
                $bond_str = NO_BREAK;
            }
        }

        # in fact, use strict hates bare words on any new line.  For example,
        # a break before the underscore here provokes the wrath of use strict:
        #    if ( -r $fn && ( -s _ || $AllowZeroFilesize)) {
        elsif ( $type eq 'F' ) {
            $bond_str = NO_BREAK;
        }

        # use strict does not allow separating type info from trailing { }
        # testfile is readmail.pl
        elsif ( $type eq 't' or $type eq 'i' ) {

            if ( $next_nonblank_type eq 'L' ) {
                $bond_str = NO_BREAK;
            }
        }

        # Do not break between a possible filehandle and a ? or / 
        # and do not introduce a break after it if there is no blank (extrude.t)
        elsif ( $type eq 'Z' ) {

            # dont break..
            if (

                # if there is no blank and we do not want one. Examples:
                #    print $x++    # do not break after $x
                #    print HTML"HELLO"   # break ok after HTML
                (
                       $next_type ne 'b'
                    && defined( $want_left_space{$next_type} )
                    && $want_left_space{$next_type} == WS_NO
                )

                # or we might be followed by the start of a quote
                || $next_nonblank_type =~ /^[\/\?]$/
              )
            {
                $bond_str = NO_BREAK;
            }
        }

        # Do not break before a possible file handle
        #if ( ( $type eq 'Z' ) || ( $next_nonblank_type eq 'Z' ) ) {
        if ( $next_nonblank_type eq 'Z' ) {
            $bond_str = NO_BREAK;
        }

        # patch to put cuddled elses back together when on multiple
        # lines, as in: } \n else \n { \n
        if ($rOpts_cuddled_else) {

            if (   ( $token eq 'else' ) && ( $next_nonblank_type eq '{' )
                || ( $type eq '}' ) && ( $next_nonblank_token eq 'else' ) )
            {
                $bond_str = NO_BREAK;
            }
        }

        # keep '}' together with ';'
        if ( ( $token eq '}' ) && ( $next_nonblank_type eq ';' ) ) {
            $bond_str = NO_BREAK;
        }

        # never break between sub name and opening paren
        if ( ( $type eq 'w' ) && ( $next_nonblank_token eq '(' ) ) {
            $bond_str = NO_BREAK;
        }

        #---------------------------------------------------------------
        # section 3:
        # now take nesting depth into account
        #---------------------------------------------------------------
        # final strength incorporates the bond strength and nesting depth
        my $strength;

        if ( defined($bond_str) && !$nobreak_to_go[$i] ) {
            if ( $total_nesting_depth > 0 ) {
                $strength = $bond_str + $total_nesting_depth;
            }
            else {
                $strength = $bond_str;
            }
        }
        else {
            $strength = NO_BREAK;
        }

        # always break after side comment
        if ( $type eq '#' ) { $strength = 0 }

        $bond_strength_to_go[$i] = $strength;

        FORMATTER_DEBUG_FLAG_BOND && do {
            my $str = substr( $token, 0, 15 );
            $str .= ' ' x ( 16 - length($str) );
            print
"BOND:  i=$i $str $type $next_nonblank_type depth=$total_nesting_depth strength=$bond_str_1 -> $bond_str -> $strength \n";
        };
    }
}

sub pad_array_to_go {

    # to simplify coding in scan_list and set_bond_strengths, it helps
    # to create some extra blank tokens at the end of the arrays
    $tokens_to_go[ $max_index_to_go + 1 ]        = '';
    $tokens_to_go[ $max_index_to_go + 2 ]        = '';
    $types_to_go[ $max_index_to_go + 1 ]         = 'b';
    $types_to_go[ $max_index_to_go + 2 ]         = 'b';
    $nesting_depth_to_go[ $max_index_to_go + 1 ] =
      $nesting_depth_to_go[$max_index_to_go];

    #    /^[R\}\)\]]$/ 
    if ( $is_closing_type{ $types_to_go[$max_index_to_go] } ) {
        if ( $nesting_depth_to_go[$max_index_to_go] <= 0 ) {

            # shouldn't happen:
            unless ( get_saw_brace_error() ) {
                warning(
"Program bug in scan_list: hit nesting error which should have been caught\n"
                );
                report_definite_bug();
            }
        }
        else {
            $nesting_depth_to_go[ $max_index_to_go + 1 ] -= 1;
        }
    }

    #       /^[L\{\(\[]$/
    elsif ( $is_opening_type{ $types_to_go[$max_index_to_go] } ) {
        $nesting_depth_to_go[ $max_index_to_go + 1 ] += 1;
    }
}

{    # begin scan_list

    my (
        $block_type,                $current_depth,
        $depth,                     $i,
        $i_last_nonblank_token,     $last_colon_sequence_number,
        $last_nonblank_token,       $last_nonblank_type,
        $last_old_breakpoint_count, $minimum_depth,
        $next_nonblank_block_type,  $next_nonblank_token,
        $next_nonblank_type,        $old_breakpoint_count,
        $starting_breakpoint_count, $starting_depth,
        $token,                     $type,
        $type_sequence,
    );

    my (
        @breakpoint_stack,              @breakpoint_undo_stack,
        @comma_index,                   @container_type,
        @identifier_count_stack,        @index_before_arrow,
        @interrupted_list,              @item_count_stack,
        @last_comma_index,              @last_dot_index,
        @last_nonblank_type,            @old_breakpoint_count_stack,
        @opening_structure_index_stack, @rfor_semicolon_list,
        @has_old_logical_breakpoints,   @rand_or_list,
        @i_equals,
    );

    # routine to define essential variables when we go 'up' to
    # a new depth
    sub check_for_new_minimum_depth {
        my $depth = shift;
        if ( $depth < $minimum_depth ) {

            $minimum_depth = $depth;

            # these arrays need not retain values between calls
            $breakpoint_stack[$depth]              = $starting_breakpoint_count;
            $container_type[$depth]                = "";
            $identifier_count_stack[$depth]        = 0;
            $index_before_arrow[$depth]            = -1;
            $interrupted_list[$depth]              = 1;
            $item_count_stack[$depth]              = 0;
            $last_nonblank_type[$depth]            = "";
            $opening_structure_index_stack[$depth] = -1;

            $breakpoint_undo_stack[$depth]       = undef;
            $comma_index[$depth]                 = undef;
            $last_comma_index[$depth]            = undef;
            $last_dot_index[$depth]              = undef;
            $old_breakpoint_count_stack[$depth]  = undef;
            $has_old_logical_breakpoints[$depth] = 0;
            $rand_or_list[$depth]                = [];
            $rfor_semicolon_list[$depth]         = [];
            $i_equals[$depth]                    = -1;

            # these arrays must retain values between calls
            if ( !defined( $has_broken_sublist[$depth] ) ) {
                $dont_align[$depth]         = 0;
                $has_broken_sublist[$depth] = 0;
                $want_comma_break[$depth]   = 0;
            }
        }
    }

    # routine to decide which commas to break at within a container;
    # returns:
    #   $bp_count = number of comma breakpoints set
    #   $do_not_break_apart = a flag indicating if container need not
    #     be broken open
    sub set_comma_breakpoints {

        my $dd                 = shift;
        my $bp_count           = 0;
        my $do_not_break_apart = 0;
        if ( $item_count_stack[$dd] && !$dont_align[$dd] ) {

            my $fbc = $forced_breakpoint_count;

            # always open comma lists not preceded by keywords,
            # barewords, identifiers (that is, anything that doesn't
            # look like a function call)
            my $must_break_open = $last_nonblank_type[$dd] !~ /^[kwiU]$/;

            set_comma_breakpoints_do(
                $dd,
                $opening_structure_index_stack[$dd],
                $i,
                $item_count_stack[$dd],
                $identifier_count_stack[$dd],
                $comma_index[$dd],
                $next_nonblank_type,
                $container_type[$dd],
                $interrupted_list[$dd],
                \$do_not_break_apart,
                $must_break_open,
            );
            $bp_count = $forced_breakpoint_count - $fbc;
            $do_not_break_apart = 0 if $must_break_open;
        }
        return ( $bp_count, $do_not_break_apart );
    }

    my %is_logical_container;

    BEGIN {
        @_ = qw# if elsif unless while and or not && | || ? : ! #;
        @is_logical_container{@_} = (1) x scalar(@_);
    }

    sub set_for_semicolon_breakpoints {
        my $dd = shift;
        foreach ( @{ $rfor_semicolon_list[$dd] } ) {
            set_forced_breakpoint($_);
        }
    }

    sub set_logical_breakpoints {
        my $dd = shift;
        if (
               $item_count_stack[$dd] == 0
            && $is_logical_container{ $container_type[$dd] }

            # TESTING:
            || $has_old_logical_breakpoints[$dd]
          )
        {

            # Look for breaks in this order:
            # 0   1    2   3
            # or  and  ||  &&
            foreach my $i ( 0 .. 3 ) {
                if ( $rand_or_list[$dd][$i] ) {
                    foreach ( @{ $rand_or_list[$dd][$i] } ) {
                        set_forced_breakpoint($_);
                    }

                    # break at any 'if' and 'unless' too
                    foreach ( @{ $rand_or_list[$dd][4] } ) {
                        set_forced_breakpoint($_);
                    }
                    $rand_or_list[$dd] = [];
                    last;
                }
            }
        }
    }

    sub is_unbreakable_container {

        # never break a container of one of these types
        # because bad things can happen (map1.t)
        my $dd = shift;

        #/^(sort|map|grep)$/
        $is_sort_map_grep{ $container_type[$dd] };
    }

    sub scan_list {

        # This routine is responsible for setting line breaks for all lists,
        # so that hierarchical structure can be displayed and so that list
        # items can be vertically aligned.  The output of this routine is
        # stored in the array @forced_breakpoint_to_go, which is used to set
        # final breakpoints.

        $starting_depth = $nesting_depth_to_go[0];

        $block_type                 = ' ';
        $current_depth              = $starting_depth;
        $i                          = -1;
        $last_colon_sequence_number = -1;
        $last_nonblank_token        = ';';
        $last_nonblank_type         = ';';
        $last_old_breakpoint_count  = 0;
        $minimum_depth = $current_depth + 1;    # forces update in check below
        $old_breakpoint_count      = 0;
        $starting_breakpoint_count = $forced_breakpoint_count;
        $token                     = ';';
        $type                      = ';';
        $type_sequence             = '';

        check_for_new_minimum_depth($current_depth);

        my $is_long_line = excess_line_length( 0, $max_index_to_go ) > 0;
        my $want_previous_breakpoint = -1;

        my $saw_good_breakpoint;
        my $i_line_end   = -1;
        my $i_line_start = -1;

        # loop over all tokens in this batch
        while ( ++$i <= $max_index_to_go ) {
            if ( $type ne 'b' ) {
                $i_last_nonblank_token = $i - 1;
                $last_nonblank_type    = $type;
                $last_nonblank_token   = $token;
            }
            $type          = $types_to_go[$i];
            $block_type    = $block_type_to_go[$i];
            $token         = $tokens_to_go[$i];
            $type_sequence = $type_sequence_to_go[$i];
            my $next_type       = $types_to_go[ $i + 1 ];
            my $next_token      = $tokens_to_go[ $i + 1 ];
            my $i_next_nonblank = ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );
            $next_nonblank_type       = $types_to_go[$i_next_nonblank];
            $next_nonblank_token      = $tokens_to_go[$i_next_nonblank];
            $next_nonblank_block_type = $block_type_to_go[$i_next_nonblank];

            # set break if flag was set
            if ( $want_previous_breakpoint >= 0 ) {
                set_forced_breakpoint($want_previous_breakpoint);
                $want_previous_breakpoint = -1;
            }

            $last_old_breakpoint_count = $old_breakpoint_count;
            if ( $old_breakpoint_to_go[$i] ) {
                $i_line_end   = $i;
                $i_line_start = $i_next_nonblank;

                $old_breakpoint_count++;

                # Break before certain keywords if user broke there and
                # this is a 'safe' break point. The idea is to retain
                # any preferred breaks for sequential list operations,
                # like a schwartzian transform. 
                if ($rOpts_break_at_old_keyword_breakpoints) {
                    if (
                           $next_nonblank_type eq 'k'
                        && $is_keyword_returning_list{$next_nonblank_token}
                        && (   $type =~ /^[=\)\]\}Riw]$/
                            || $type eq 'k'
                            && $is_keyword_returning_list{$token} )
                      )
                    {

                        # we actually have to set this break next time through
                        # the loop because if we are at a closing token (such 
                        # as '}') which forms a one-line block, this break might 
                        # get undone.
                        $want_previous_breakpoint = $i;
                    }
                }
            }
            next if ( $type eq 'b' );
            $depth = $nesting_depth_to_go[ $i + 1 ];

            # safety check - be sure we always break after a comment
            # Shouldn't happen .. an error here probably means that the
            # nobreak flag did not get turned off correctly during
            # formatting.
            if ( $type eq '#' ) {
                if ( $i != $max_index_to_go ) {
                    warning(
"Non-fatal program bug: backup logic needed to break after a comment\n"
                    );
                    report_definite_bug();
                    $nobreak_to_go[$i] = 0;
                    set_forced_breakpoint($i);
                }
            }

            # Force breakpoints at certain tokens in long lines.
            # Note that such breakpoints will be undone later if these tokens
            # are fully contained within parens on a line.
            if (
                   $type eq 'k'
                && $i > 0
                && $token =~ /^(if|unless)$/
                && (
                    $is_long_line

                    # or container is broken (by side-comment, etc)
                    || (   $next_nonblank_token eq '('
                        && $mate_index_to_go[$i_next_nonblank] < $i )
                )
              )
            {
                set_forced_breakpoint( $i - 1 );
            }

            # remember locations of '||'  and '&&' for possible breaks if we
            # decide this is a long logical expression.
            if ( $type eq '||' ) {
                push @{ $rand_or_list[$depth][2] }, $i;
                ++$has_old_logical_breakpoints[$depth]
                  if ( ( $i == $i_line_start || $i == $i_line_end )
                    && $rOpts_break_at_old_logical_breakpoints );
            }
            elsif ( $type eq '&&' ) {
                push @{ $rand_or_list[$depth][3] }, $i;
                ++$has_old_logical_breakpoints[$depth]
                  if ( ( $i == $i_line_start || $i == $i_line_end )
                    && $rOpts_break_at_old_logical_breakpoints );
            }
            elsif ( $type eq 'f' ) {
                push @{ $rfor_semicolon_list[$depth] }, $i;
            }
            elsif ( $type eq 'k' ) {
                if ( $token eq 'and' ) {
                    push @{ $rand_or_list[$depth][1] }, $i;
                    ++$has_old_logical_breakpoints[$depth]
                      if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints );
                }

                # break immediately at 'or's which are probably not in a logical
                # block -- but we will break in logical breaks below so that
                # they do not add to the forced_breakpoint_count
                elsif ( $token eq 'or' ) {
                    push @{ $rand_or_list[$depth][0] }, $i;
                    ++$has_old_logical_breakpoints[$depth]
                      if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints );
                    if ( $is_logical_container{ $container_type[$depth] } ) {
                    }
                    else {
                        if ($is_long_line) { set_forced_breakpoint($i) }
                        elsif ( ( $i == $i_line_start || $i == $i_line_end )
                            && $rOpts_break_at_old_logical_breakpoints )
                        {
                            $saw_good_breakpoint = 1;
                        }
                    }
                }
                elsif ( $token eq 'if' || $token eq 'unless' ) {
                    push @{ $rand_or_list[$depth][4] }, $i;
                    if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints )
                    {
                        set_forced_breakpoint($i);
                    }
                }
            }
            elsif ( $type eq '=' ) {
                $i_equals[$depth] = $i;
            }

            if ($type_sequence) {

                # handle any postponed closing breakpoints
                if ( $token =~ /^[\)\]\}\:]$/ ) {
                    if ( $token eq ':' ) {
                        $last_colon_sequence_number = $type_sequence;

                        # TESTING: retain break at a ':' line break
                        if ( ( $i == $i_line_start || $i == $i_line_end )
                            && $rOpts_break_at_old_trinary_breakpoints )
                        {

                            # TESTING:
                            set_forced_breakpoint($i);

                            # break at previous '='
                            if ( $i_equals[$depth] > 0 ) {
                                set_forced_breakpoint( $i_equals[$depth] );
                                $i_equals[$depth] = -1;
                            }
                        }
                    }
                    if ( defined( $postponed_breakpoint{$type_sequence} ) ) {
                        my $inc = ( $token eq ':' ) ? 0 : 1;
                        set_forced_breakpoint( $i - $inc );
                        delete $postponed_breakpoint{$type_sequence};
                    }
                }

                # set breaks at ?/: if they will get separated (and are
                # not a ?/: chain), or if the '?' is at the end of the
                # line
                elsif ( $token eq '?' ) {
                    my $i_colon = $mate_index_to_go[$i];
                    if (
                        $i_colon <= 0  # the ':' is not in this batch
                        || $i == 0     # this '?' is the first token of the line
                        || $i ==
                        $max_index_to_go    # or this '?' is the last token
                      )
                    {

                        # don't break at a '?' if preceded by ':' on
                        # this line of previous ?/: pair on this line.
                        # This is an attempt to preserve a chain of ?/:
                        # expressions (elsif2.t).  And don't break if
                        # this has a side comment.
                        set_forced_breakpoint($i)
                          unless (
                            $type_sequence == (
                                $last_colon_sequence_number +
                                  TYPE_SEQUENCE_INCREMENT
                            )
                            || $tokens_to_go[$max_index_to_go] eq '#'
                          );
                        set_closing_breakpoint($i);
                    }
                }
            }

            #print "LISTX sees: i=$i type=$type  tok=$token  block=$block_type depth=$depth\n";

            #------------------------------------------------------------
            # Handle Increasing Depth..
            #
            # prepare for a new list when depth increases
            # token $i is a '(','{', or '['
            #------------------------------------------------------------
            if ( $depth > $current_depth ) {

                $breakpoint_stack[$depth]       = $forced_breakpoint_count;
                $breakpoint_undo_stack[$depth]  = $forced_breakpoint_undo_count;
                $has_broken_sublist[$depth]     = 0;
                $identifier_count_stack[$depth] = 0;
                $index_before_arrow[$depth]     = -1;
                $interrupted_list[$depth]       = 0;
                $item_count_stack[$depth]       = 0;
                $last_comma_index[$depth]       = undef;
                $last_dot_index[$depth]         = undef;
                $last_nonblank_type[$depth]     = $last_nonblank_type;
                $old_breakpoint_count_stack[$depth]    = $old_breakpoint_count;
                $opening_structure_index_stack[$depth] = $i;
                $rand_or_list[$depth]                  = [];
                $rfor_semicolon_list[$depth]           = [];
                $i_equals[$depth]                      = -1;
                $want_comma_break[$depth]              = 0;
                $container_type[$depth]                =
                  ( $last_nonblank_type =~ /^(k|=>|&&|\|\||\?|\:|\.)$/ )
                  ? $last_nonblank_token
                  : "";
                $has_old_logical_breakpoints[$depth] = 0;

                # if line ends here then signal closing token to break
                if ( $next_nonblank_type eq 'b' || $next_nonblank_type eq '#' )
                {
                    set_closing_breakpoint($i);
                }

                # Not all lists of values should be vertically aligned..
                $dont_align[$depth] =

                  # code BLOCKS are handled at a higher level
                  ( $block_type ne "" )

                  # certain paren lists
                  || ( $type eq '(' ) && (

                    # it does not usually look good to align a list of
                    # identifiers in a parameter list, as in:
                    #    my($var1, $var2, ...) 
                    # (This test should probably be refined, for now I'm just
                    # testing for any keyword)
                    ( $last_nonblank_type eq 'k' )

                    # a trailing '(' usually indicates a non-list
                    || ( $next_nonblank_type eq '(' )
                  );

                # patch to outdent opening brace of long if/for/..
                # statements (like this one).  See similar coding in
                # set_continuation breaks.  We have also catch it here for
                # short line fragments which otherwise will not go through
                # set_continuation_breaks.
                if (
                    $block_type

                    # if we have the ')' but not its '(' in this batch..
                    && ( $last_nonblank_token eq ')' )
                    && $mate_index_to_go[$i_last_nonblank_token] < 0

                    # and user wants brace to left
                    && !$rOpts->{'opening-brace-always-on-right'}

                    && ( $type  eq '{' )    # should be true
                    && ( $token eq '{' )    # should be true
                  )
                {
                    set_forced_breakpoint( $i - 1 );
                }
            }

            #------------------------------------------------------------
            # Handle Decreasing Depth..
            #
            # finish off any old list when depth decreases
            # token $i is a ')','}', or ']'
            #------------------------------------------------------------
            elsif ( $depth < $current_depth ) {

                check_for_new_minimum_depth($depth);

                # force all outer logical containers to break after we see on
                # old breakpoint
                $has_old_logical_breakpoints[$depth] ||=
                  $has_old_logical_breakpoints[$current_depth];

                # Patch to break between ') {' if the paren list is broken.
                # There is similar logic in set_continuation_breaks for
                # non-broken lists.
                if (   $token eq ')'
                    && $next_nonblank_block_type
                    && $interrupted_list[$current_depth]
                    && $next_nonblank_type eq '{'
                    && !$rOpts->{'opening-brace-always-on-right'} )
                {
                    set_forced_breakpoint($i);
                }

                #print "LISTY sees: i=$i type=$type  tok=$token  block=$block_type depth=$depth next=$next_nonblank_type next_block=$next_nonblank_block_type inter=$interrupted_list[$current_depth]\n";

                # set breaks at commas if necessary
                my ( $bp_count, $do_not_break_apart ) =
                  set_comma_breakpoints($current_depth);

                my $i_opening = $opening_structure_index_stack[$current_depth];
                my $saw_opening_structure = ( $i_opening >= 0 );

                # this term is long if we had to break at interior commas..
                my $is_long_term = $bp_count > 0;

                # ..or if the length between opening and closing parens exceeds
                # allowed line length
                if ( !$is_long_term && $saw_opening_structure ) {
                    my $i_opening_minus = find_token_starting_list($i_opening);

                    # Note: we have to allow for one extra space after a
                    # closing token so that we do not strand a comma or
                    # semicolon, hence the '>=' here (oneline.t)
                    $is_long_term =
                      excess_line_length( $i_opening_minus, $i ) >= 0;
                }

                # We've set breaks after all comma-arrows.  Now we have to
                # undo them if this can be a one-line block
                # (the only breakpoints set will be due to comma-arrows)
                if (

                    # user doesn't require breaking after all comma-arrows
                    ( $rOpts_comma_arrow_breakpoints != 0 )

                    # and if the opening structure is in this batch
                    && $saw_opening_structure

                    # and either on the same old line 
                    && (
                        $old_breakpoint_count_stack[$current_depth] ==
                        $last_old_breakpoint_count

                        # or user wants to form long blocks with arrows
                        || $rOpts_comma_arrow_breakpoints == 2
                    )

                    # and we made some breakpoints between the opening and closing
                    && ( $breakpoint_undo_stack[$current_depth] <
                        $forced_breakpoint_undo_count )

                    # and this block is short enough to fit on one line
                    # Note: use < because need 1 more space for possible comma
                    && !$is_long_term

                  )
                {
                    undo_forced_breakpoint_stack(
                        $breakpoint_undo_stack[$current_depth] );
                }

                # now see if we have any comma breakpoints left
                my $has_comma_breakpoints =
                  ( $breakpoint_stack[$current_depth] !=
                      $forced_breakpoint_count );

                # update broken-sublist flag of the outer container
                     $has_broken_sublist[$depth] = $has_broken_sublist[$depth]
                  || $has_broken_sublist[$current_depth]
                  || $is_long_term
                  || $has_comma_breakpoints;

# =pod
# 
# Having come to the closing ')', '}', or ']', now we have to decide if we
# should 'open up' the structure by placing breaks at the opening and
# closing containers.  This is a tricky decision.  Here are some of the 
# basic considerations:
# 
# -If this is a BLOCK container, then any breakpoints will have already
# been set (and according to user preferences), so we need do nothing here.
# 
# -If we have a comma-separated list for which we can align the list items,
# then we need to do so because otherwise the vertical aligner cannot
# currently do the alignment.
# 
# -If this container does itself contain a container which has been broken
# open, then it should be broken open to properly show the structure.
# 
# -If there is nothing to align, and no other reason to break apart,
# then do not do it.
# 
# We will not break open the parens of a long but 'simple' logical expression.
# For example:
# 
# This is an example of a simple logical expression and its formatting:
# 
#     if ( $bigwasteofspace1 && $bigwasteofspace2
#         || $bigwasteofspace3 && $bigwasteofspace4 )
# 
# Most people would prefer this than the 'spacey' version:
# 
#     if ( 
#         $bigwasteofspace1 && $bigwasteofspace2
#         || $bigwasteofspace3 && $bigwasteofspace4 
#     )
# 
# To illustrate the rules for breaking logical expressions, consider:
# 
#             FULLY DENSE:
#             if ( $opt_excl
#                 and ( exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc ))
# 
# This is on the verge of being difficult to read.  The current default is to
# open it up like this:
# 
#             DEFAULT:
#             if (
#                 $opt_excl
#                 and ( exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc )
#               )
# 
# This is a compromise which tries to avoid being too dense and to spacey.
# A more spaced version would be:
# 
#             SPACEY:
#             if (
#                 $opt_excl
#                 and ( 
#                     exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc 
#                 )
#               )
# 
# Some people might prefer the spacey version -- an option could be added.  The
# innermost expression contains a long block '( exists $ids_...  ')'.  
# 
# Here is how the logic goes: We will force a break at the 'or' that the
# innermost expression contains, but we will not break apart its opening and
# closing containers because (1) it contains no multi-line sub-containers itself,
# and (2) there is no alignment to be gained by breaking it open like this
# 
#             and ( 
#                 exists $ids_excl_uc{$id_uc}
#                 or grep $id_uc =~ /$_/, @ids_excl_uc 
#             )
# 
# (although this looks perfectly ok and might be good for long expressions).  The
# outer 'if' container, though, contains a broken sub-container, so it will be
# broken open to avoid too much density.  Also, since it contains no 'or's, there
# will be a forced break at its 'and'.
# 
# =cut

                # set some flags telling something about this container..
                my $is_simple_logical_expression = 0;
                if (   $item_count_stack[$current_depth] == 0
                    && $saw_opening_structure
                    && $tokens_to_go[$i_opening] eq '('
                    && $is_logical_container{ $container_type[$current_depth] }
                  )
                {

                    # This seems to be a simple logical expression with
                    # no existing breakpoints.  Set a flag to prevent
                    # opening it up.
                    if ( !$has_comma_breakpoints ) {
                        $is_simple_logical_expression = 1;
                    }

                    # This seems to be a simple logical expression with
                    # breakpoints (broken sublists, for example).  Break
                    # at all 'or's and '||'s.
                    else {
                        set_logical_breakpoints($current_depth);
                    }
                }

                if ( $is_long_term
                    && @{ $rfor_semicolon_list[$current_depth] } )
                {
                    set_for_semicolon_breakpoints($current_depth);

                    # open up a long 'for' or 'foreach' container to allow
                    # leading term alignment unless -lp is used.
                    $has_comma_breakpoints = 1
                      unless $rOpts_line_up_parentheses;
                }

                if (

                    # breaks for code BLOCKS are handled at a higher level
                    !$block_type

                    # we do not need to break at the top level of an 'if'
                    # type expression
                    && !$is_simple_logical_expression

                    ## modification to keep ': (' containers vertically tight;
                    ## but probably better to let user set -vt=1 to avoid
                    ## inconsistency with other paren types
                    ## && ($container_type[$current_depth] ne ':')

                    # otherwise, we require one of these reasons for breaking:
                    && (

                        # - this term has forced line breaks
                        $has_comma_breakpoints

                        # - the opening container is separated from this batch
                        #   for some reason (comment, blank line, code block)
                        # - this is a non-paren container spanning multiple lines
                        || !$saw_opening_structure

                        # - this is a long block contained in another breakable
                        #   container
                        || (   $is_long_term
                            && $container_environment_to_go[$i_opening] ne
                            'BLOCK' )
                    )
                  )
                {

                    # For -lp option, we must put a breakpoint before
                    # the token which has been identified as starting
                    # this indentation level.  This is necessary for
                    # proper alignment.
                    if ( $rOpts_line_up_parentheses && $saw_opening_structure )
                    {
                        my $item = $leading_spaces_to_go[ $i_opening + 1 ];
                        if ( defined($item) ) {
                            my $i_start_2 = $item->get_STARTING_INDEX();
                            if (
                                defined($i_start_2)

                                # we are breaking after an opening brace, paren,
                                # so don't break before it too
                                && $i_start_2 ne $i_opening
                              )
                            {

                                # Only break for breakpoints at the same
                                # indentation level as the opening paren
                                my $test1 = $nesting_depth_to_go[$i_opening];
                                my $test2 = $nesting_depth_to_go[$i_start_2];
                                if ( $test2 == $test1 ) {
                                    set_forced_breakpoint( $i_start_2 - 1 );
                                }
                            }
                        }
                    }

                    # break after opening structure.
                    # note: break before closing structure will be automatic
                    if ( $minimum_depth <= $current_depth ) {

                        set_forced_breakpoint($i_opening)
                          unless ( $do_not_break_apart
                            || is_unbreakable_container($current_depth) );

                        # break at '.' of lower depth level before opening token
                        if ( $last_dot_index[$depth] ) {
                            set_forced_breakpoint( $last_dot_index[$depth] );
                        }

                        # break before opening structure if preeced by another
                        # closing structure and a comma.  This is normally
                        # done by the previous closing brace, but not
                        # if it was a one-line block.
                        if ( $i_opening > 2 ) {
                            my $i_prev =
                              ( $types_to_go[ $i_opening - 1 ] eq 'b' )
                              ? $i_opening - 2
                              : $i_opening - 1;

                            if (   $types_to_go[$i_prev] eq ','
                                && $types_to_go[ $i_prev - 1 ] =~ /^[\)\}]$/ )
                            {
                                set_forced_breakpoint($i_prev);
                            }

                            # also break before something like ':('  or '?('
                            # if appropriate.
                            elsif (
                                $types_to_go[$i_prev] =~ /^([\:\?]|&&|\|\|)$/ )
                            {
                                my $token_prev = $tokens_to_go[$i_prev];
                                if ( $want_break_before{$token_prev} ) {
                                    set_forced_breakpoint($i_prev);
                                }
                            }
                        }
                    }

                    # break after comma following closing structure
                    if ( $next_type eq ',' ) {
                        set_forced_breakpoint( $i + 1 );
                    }

                    # break before an '=' following closing structure
                    if (
                        $next_nonblank_type eq '='
                        && ( $breakpoint_stack[$current_depth] !=
                            $forced_breakpoint_count )
                      )
                    {
                        set_forced_breakpoint($i);
                    }

                    # break at any comma before the opening structure Added
                    # for -lp, but seems to be good in general.  It isn't
                    # obvious how far back to look; the '5' below seems to
                    # work well and will catch the comma in something like
                    #  push @list, myfunc( $param, $param, ..

                    my $icomma = $last_comma_index[$depth];
                    if ( defined($icomma) && ( $i_opening - $icomma ) < 5 ) {
                        unless ( $forced_breakpoint_to_go[$icomma] ) {
                            set_forced_breakpoint($icomma);
                        }
                    }
                }    # end logic to open up a container

                # Break open a logical container open if it was already open
                elsif ($is_simple_logical_expression
                    && $has_old_logical_breakpoints[$current_depth] )
                {
                    set_logical_breakpoints($current_depth);
                }

                # Handle long container which does not get opened up
                elsif ($is_long_term) {

                    # must set fake breakpoint to alert outer containers that
                    # they are complex
                    set_fake_breakpoint();
                }
            }

            #------------------------------------------------------------
            # Handle this token
            #------------------------------------------------------------

            $current_depth = $depth;

            # handle comma-arrow
            if ( $type eq '=>' ) {
                next if ( $last_nonblank_type eq '=>' );
                next if $rOpts_break_at_old_comma_breakpoints;
                next if $rOpts_comma_arrow_breakpoints == 3;
                $want_comma_break[$depth]   = 1;
                $index_before_arrow[$depth] = $i_last_nonblank_token;
                next;
            }

            elsif ( $type eq '.' ) {
                $last_dot_index[$depth] = $i;
            }

            # Turn off alignment if we are sure that this is not a list
            # environment.  To be safe, we will do this if we see certain
            # non-list tokens, such as ';', and also the environment is
            # not a list.  Note that '=' could be in any of the = operators
            # (lextest.t). We can't just use the reported environment
            # because it can be incorrect in some cases.
            elsif ($type =~ /(^[\;\<\>\~]$)|[=]/
                && $container_environment_to_go[$i] ne 'LIST' )
            {
                $dont_align[$depth]         = 1;
                $want_comma_break[$depth]   = 0;
                $index_before_arrow[$depth] = -1;
            }

            # now just handle any commas
            next unless ( $type eq ',' );

            $last_dot_index[$depth]   = undef;
            $last_comma_index[$depth] = $i;

            # break here if this comma follows a '=>'
            # but not if there is a side comment after the comma
            if ( $want_comma_break[$depth] ) {

                if ( $next_nonblank_type =~ /^[\)\}\]R]$/ ) {
                    $want_comma_break[$depth]   = 0;
                    $index_before_arrow[$depth] = -1;
                    next;
                }

                set_forced_breakpoint($i) unless ( $next_nonblank_type eq '#' );

                # break before the previous token if it looks safe
                # Example of something that we will not try to break before:
                #   DBI::SQL_SMALLINT() => $ado_consts->{adSmallInt},
                my $ibreak = $index_before_arrow[$depth] - 1;
                if (   $ibreak > 0
                    && $tokens_to_go[ $ibreak + 1 ] !~ /^[\)\}\]]$/ )
                {
                    if ( $tokens_to_go[$ibreak] eq '-' ) { $ibreak-- }
                    if ( $types_to_go[$ibreak] =~ /^[,b\(\{\[]$/ ) {
                        set_forced_breakpoint($ibreak);
                    }
                }

                $want_comma_break[$depth]   = 0;
                $index_before_arrow[$depth] = -1;

                # handle list which mixes '=>'s and ','s:
                # treat any list items so far as an interrupted list
                $interrupted_list[$depth] = 1;
                next;
            }

            # skip past these commas if we are not supposed to format them
            next if ( $dont_align[$depth] );

            # break after all commas above starting depth
            if ( $depth < $starting_depth ) {
                set_forced_breakpoint($i) unless ( $next_nonblank_type eq '#' );
                next;
            }

            # add this comma to the list..
            my $item_count = $item_count_stack[$depth];
            if ( $item_count == 0 ) {

                # but do not form a list with no opening structure
                # for example:

                #            open INFILE_COPY, ">$input_file_copy"
                #              or die ("very long message");

                if ( ( $opening_structure_index_stack[$depth] < 0 )
                    && $container_environment_to_go[$i] eq 'BLOCK' )
                {
                    $dont_align[$depth] = 1;
                    next;
                }
            }

            $comma_index[$depth][$item_count] = $i;
            ++$item_count_stack[$depth];
            if ( $last_nonblank_type =~ /^[iR\]]$/ ) {
                $identifier_count_stack[$depth]++;
            }
        }

        #-------------------------------------------
        # end of loop over all tokens in this batch
        #-------------------------------------------

        # set breaks for any unfinished lists ..
        for ( my $dd = $current_depth ; $dd >= $minimum_depth ; $dd-- ) {

            $interrupted_list[$dd] = 1;
            $has_broken_sublist[$dd] = 1 if ( $dd < $current_depth );
            set_comma_breakpoints($dd);
            set_logical_breakpoints($dd)
              if ( $has_old_logical_breakpoints[$dd] );
            set_for_semicolon_breakpoints($dd);

            # break open container...
            my $i_opening = $opening_structure_index_stack[$dd];
            set_forced_breakpoint($i_opening)
              unless (
                is_unbreakable_container($dd)

                # Avoid a break which would place an isolated ' or " 
                # on a line
                || (   $type eq 'Q'
                    && $i_opening >= $max_index_to_go - 2
                    && $token =~ /^['"]$/ )
              );
        }

        # Return a flag indicating if the input file had some good breakpoints.
        # This flag will be used to force a break in a line shorter than the
        # allowed line length.
        if ( $has_old_logical_breakpoints[$current_depth] ) {
            $saw_good_breakpoint = 1;
        }
        return $saw_good_breakpoint;
    }
}    # end scan_list

sub find_token_starting_list {

    # When testing to see if a block will fit on one line, some 
    # previous token(s) may also need to be on the line; particularly
    # if this is a sub call.  So we will look back at least one
    # token. NOTE: This isn't perfect, but not critical, because
    # if we mis-identify a block, it will be wrapped and therefore
    # fixed the next time it is formatted.
    my $i_opening_paren = shift;
    my $i_opening_minus = $i_opening_paren;
    my $im1             = $i_opening_paren - 1;
    my $im2             = $i_opening_paren - 2;
    my $im3             = $i_opening_paren - 3;
    my $typem1          = $types_to_go[$im1];
    my $typem2          = $im2 >= 0 ? $types_to_go[$im2] : 'b';
    if ( $typem1 eq ',' || ( $typem1 eq 'b' && $typem2 eq ',' ) ) {
        $i_opening_minus = $i_opening_paren;
    }
    elsif ( $tokens_to_go[$i_opening_paren] eq '(' ) {
        $i_opening_minus = $im1 if $im1 >= 0;

        # walk back to improve length estimate
        for ( my $j = $im1 ; $j >= 0 ; $j-- ) {
            last if ( $types_to_go[$j] =~ /^[\(\[\{L\}\]\)Rb,]$/ );
            $i_opening_minus = $j;
        }
        if ( $types_to_go[$i_opening_minus] eq 'b' ) { $i_opening_minus++ }
    }
    elsif ( $typem1 eq 'k' ) { $i_opening_minus = $im1 }
    elsif ( $typem1 eq 'b' && $im2 >= 0 && $types_to_go[$im2] eq 'k' ) {
        $i_opening_minus = $im2;
    }
    return $i_opening_minus;
}

{    # begin set_comma_breakpoints_do

    my %is_keyword_with_special_leading_term;

    BEGIN {

        # These keywords have prototypes which allow a special leading item
        # followed by a list
        @_ =
          qw(formline grep kill map printf sprintf push chmod join pack unshift);
        @is_keyword_with_special_leading_term{@_} = (1) x scalar(@_);
    }

    sub set_comma_breakpoints_do {

        # Given a list with some commas, set breakpoints at some of the
        # commas, if necessary, to make it easy to read.  This list is
        # an example:
        my (
            $depth,               $i_opening_paren,  $i_closing_paren,
            $item_count,          $identifier_count, $rcomma_index,
            $next_nonblank_type,  $list_type,        $interrupted,
            $rdo_not_break_apart, $must_break_open,
          )
          = @_;

        # nothing to do if no commas seen
        return if ( $item_count < 1 );
        my $i_first_comma     = $$rcomma_index[0];
        my $i_true_last_comma = $$rcomma_index[ $item_count - 1 ];
        my $i_last_comma      = $i_true_last_comma;
        if ( $i_last_comma >= $max_index_to_go ) {
            $i_last_comma = $$rcomma_index[ --$item_count - 1 ];
            return if ( $item_count < 1 );
        }

        #---------------------------------------------------------------
        # find lengths of all items in the list to calculate page layout
        #---------------------------------------------------------------
        my $comma_count = $item_count;
        my @item_lengths;
        my @i_term_begin;
        my @i_term_end;
        my @i_term_comma;
        my $i_prev_plus;
        my @max_length = ( 0, 0 );
        my $first_term_length;
        my $i      = $i_opening_paren;
        my $is_odd = 1;

        for ( my $j = 0 ; $j < $comma_count ; $j++ ) {
            $is_odd      = 1 - $is_odd;
            $i_prev_plus = $i + 1;
            $i           = $$rcomma_index[$j];

            my $i_term_end =
              ( $types_to_go[ $i - 1 ] eq 'b' ) ? $i - 2 : $i - 1;
            my $i_term_begin =
              ( $types_to_go[$i_prev_plus] eq 'b' )
              ? $i_prev_plus + 1
              : $i_prev_plus;
            push @i_term_begin, $i_term_begin;
            push @i_term_end,   $i_term_end;
            push @i_term_comma, $i;

            # note: currently adding 2 to all lengths (for comma and space)
            my $length =
              2 + token_sequence_length( $i_term_begin, $i_term_end );
            push @item_lengths, $length;

            if ( $j == 0 ) {
                $first_term_length = $length;
            }
            else {

                if ( $length > $max_length[$is_odd] ) {
                    $max_length[$is_odd] = $length;
                }
            }
        }

        # now we have to make a distinction between the comma count and item
        # count, because the item count will be one greater than the comma
        # count if the last item is not terminated with a comma
        my $i_b =
          ( $types_to_go[ $i_last_comma + 1 ] eq 'b' )
          ? $i_last_comma + 1
          : $i_last_comma;
        my $i_e =
          ( $types_to_go[ $i_closing_paren - 1 ] eq 'b' )
          ? $i_closing_paren - 2
          : $i_closing_paren - 1;
        my $i_effective_last_comma = $i_last_comma;

        my $last_item_length = token_sequence_length( $i_b + 1, $i_e );

        if ( $last_item_length > 0 ) {

            # add 2 to length because other lengths include a comma and a blank
            $last_item_length += 2;
            push @item_lengths, $last_item_length;
            push @i_term_begin, $i_b + 1;
            push @i_term_end,   $i_e;
            push @i_term_comma, undef;

            my $i_odd = $item_count % 2;

            if ( $last_item_length > $max_length[$i_odd] ) {
                $max_length[$i_odd] = $last_item_length;
            }

            $item_count++;
            $i_effective_last_comma = $i_e + 1;

            if ( $types_to_go[ $i_b + 1 ] =~ /^[iR\]]$/ ) {
                $identifier_count++;
            }
        }

        #---------------------------------------------------------------
        # End of length calculations
        #---------------------------------------------------------------

        #---------------------------------------------------------------
        # Compound List Rule 1: 
        # Break at (almost) every comma for a list containing a broken
        # sublist.  This has higher priority than the Interrupted List
        # Rule.
        #---------------------------------------------------------------
        if ( $has_broken_sublist[$depth] ) {

            # Break at every comma except for a comma between two
            # simple, small terms.  This prevents long vertical
            # columns of, say, just 0's.
            my $small_length = 10;    # 2 + actual maximum length wanted

            # We'll insert a break in long runs of small terms to 
            # allow alignment in uniform tables. 
            my $skipped_count = 0;
            my $columns       = table_columns_available($i_first_comma);
            my $fields        = int( $columns / $small_length );
            if (   $rOpts_maximum_fields_per_table
                && $fields > $rOpts_maximum_fields_per_table )
            {
                $fields = $rOpts_maximum_fields_per_table;
            }
            my $max_skipped_count = $fields - 1;

            my $is_simple_last_term = 0;
            my $is_simple_next_term = 0;
            foreach my $j ( 0 .. $item_count ) {
                $is_simple_last_term = $is_simple_next_term;
                $is_simple_next_term = 0;
                if (   $j < $item_count
                    && $i_term_end[$j] == $i_term_begin[$j]
                    && $item_lengths[$j] <= $small_length )
                {
                    $is_simple_next_term = 1;
                }
                next if $j == 0;
                if (   $is_simple_last_term
                    && $is_simple_next_term
                    && $skipped_count < $max_skipped_count )
                {
                    $skipped_count++;
                }
                else {
                    $skipped_count = 0;
                    my $i = $i_term_comma[ $j - 1 ];
                    last unless defined $i;
                    set_forced_breakpoint($i);
                }
            }

            # always break at the last comma if this list is
            # interrupted; we wouldn't want to leave a terminal '{', for
            # example.
            if ($interrupted) { set_forced_breakpoint($i_true_last_comma) }
            return;
        }

        #my ( $a, $b, $c ) = caller();
        #print "LISTX: in set_list $a $c interupt=$interrupted count=$item_count
        #i_first = $i_first_comma  i_last=$i_last_comma max=$max_index_to_go\n";
        #print "depth=$depth has_broken=$has_broken_sublist[$depth] is_multi=$is_multiline opening_paren=($i_opening_paren) \n";

        #---------------------------------------------------------------
        # Interrupted List Rule:
        # A list is is forced to use old breakpoints if it was interrupted
        # by side comments or blank lines, or requested by user.
        #---------------------------------------------------------------
        if (   $rOpts_break_at_old_comma_breakpoints
            || $interrupted
            || $i_opening_paren < 0 )
        {
            copy_old_breakpoints( $i_first_comma, $i_true_last_comma );
            return;
        }

        #---------------------------------------------------------------
        # Looks like a list of items.  We have to look at it and size it up.
        #---------------------------------------------------------------

        my $opening_token       = $tokens_to_go[$i_opening_paren];
        my $opening_environment =
          $container_environment_to_go[$i_opening_paren];

        #-------------------------------------------------------------------
        # Return if this will fit on one line 
        #-------------------------------------------------------------------

        my $i_opening_minus = find_token_starting_list($i_opening_paren);
        return
          unless excess_line_length( $i_opening_minus, $i_closing_paren ) > 0;

        #-------------------------------------------------------------------
        # Now we know that this block spans multiple lines; we have to set
        # at least one breakpoint -- real or fake -- as a signal to break
        # open any outer containers.
        #-------------------------------------------------------------------
        set_fake_breakpoint();

        # be sure we do not extend beyond the current list length
        if ( $i_effective_last_comma >= $max_index_to_go ) {
            $i_effective_last_comma = $max_index_to_go - 1;
        }

        # Set a flag indicating if we need to break open to keep -lp
        # items aligned.  This is necessary if any of the list terms
        # exceeds the available space after the '('. 
        my $need_lp_break_open = $must_break_open;
        if ( $rOpts_line_up_parentheses && !$must_break_open ) {
            my $columns_if_unbroken = $rOpts_maximum_line_length -
              total_line_length( $i_opening_minus, $i_opening_paren );
            $need_lp_break_open = ( $max_length[0] > $columns_if_unbroken )
              || ( $max_length[1] > $columns_if_unbroken )
              || ( $first_term_length > $columns_if_unbroken );
        }

        # Specify if the list must have an even number of fields or not.
        # It is generally safest to assume an even number, because the
        # list items might be a hash list.  But if we can be sure that
        # it is not a hash, then we can allow an odd number for more
        # flexibility.
        my $odd_or_even = 2;    # 1 = odd field count ok, 2 = want even count

        if (   $identifier_count >= $item_count - 1
            || $next_nonblank_type eq '='
            || ( $list_type && $list_type ne '=>' && $list_type !~ /^[\:\?]$/ )
          )
        {
            $odd_or_even = 1;
        }

        # do we have a long first term which should be
        # left on a line by itself?
        my $use_separate_first_term = (
            $odd_or_even == 1       # only if we can use 1 field/line
              && $item_count > 3    # need several items
              && $first_term_length >
              2 * $max_length[0] - 2    # need long first term
              && $first_term_length >
              2 * $max_length[1] - 2    # need long first term
        );

        # or do we know from the type of list that the first term should
        # be placed alone? 
        if ( !$use_separate_first_term ) {
            if ( $is_keyword_with_special_leading_term{$list_type} ) {
                $use_separate_first_term = 1;

                # should the container be broken open?
                if ( $item_count < 3 ) {
                    if ( $i_first_comma - $i_opening_paren < 4 ) {
                        $$rdo_not_break_apart = 1;
                    }
                }
                elsif ($first_term_length < 20
                    && $i_first_comma - $i_opening_paren < 4 )
                {
                    my $columns = table_columns_available($i_first_comma);
                    if ( $first_term_length < $columns ) {
                        $$rdo_not_break_apart = 1;
                    }
                }
            }
        }

        # if so, 
        if ($use_separate_first_term) {

            # ..set a break and update starting values
            $use_separate_first_term = 1;
            set_forced_breakpoint($i_first_comma);
            $i_opening_paren = $i_first_comma;
            $i_first_comma   = $$rcomma_index[1];
            $item_count--;
            return if $comma_count == 1;
            shift @item_lengths;
            shift @i_term_begin;
            shift @i_term_end;
            shift @i_term_comma;
        }

        # if not, update the metrics to include the first term
        else {
            if ( $first_term_length > $max_length[0] ) {
                $max_length[0] = $first_term_length;
            }
        }

        # Field width parameters
        my $pair_width = ( $max_length[0] + $max_length[1] );
        my $max_width  =
          ( $max_length[0] > $max_length[1] ) ? $max_length[0] : $max_length[1];

        # Number of free columns across the page width for laying out tables
        my $columns = table_columns_available($i_first_comma);

        # Estimated maximum number of fields which fit this space
        # This will be our first guess
        my $number_of_fields_max =
          maximum_number_of_fields( $columns, $odd_or_even, $max_width,
            $pair_width );
        my $number_of_fields = $number_of_fields_max;

        # Find the best-looking number of fields
        # and make this our second guess if possible
        my ( $number_of_fields_best, $ri_ragged_break_list,
            $new_identifier_count )
          = study_list_complexity( \@i_term_begin, \@i_term_end, \@item_lengths,
            $max_width );

        if (   $number_of_fields_best != 0
            && $number_of_fields_best < $number_of_fields_max )
        {
            $number_of_fields = $number_of_fields_best;
        }

        # ----------------------------------------------------------------------
        # If we are crowded and the -lp option is being used, try to
        # undo some indentation 
        # ----------------------------------------------------------------------
        if (
            $rOpts_line_up_parentheses
            && (
                $number_of_fields == 0
                || (   $number_of_fields == 1
                    && $number_of_fields != $number_of_fields_best )
            )
          )
        {
            my $available_spaces = get_AVAILABLE_SPACES_to_go($i_first_comma);
            if ( $available_spaces > 0 ) {

                my $spaces_wanted = $max_width - $columns;    # for 1 field

                if ( $number_of_fields_best == 0 ) {
                    $number_of_fields_best =
                      get_maximum_fields_wanted( \@item_lengths );
                }

                if ( $number_of_fields_best != 1 ) {
                    my $spaces_wanted_2 =
                      1 + $pair_width - $columns;             # for 2 fields
                    if ( $available_spaces > $spaces_wanted_2 ) {
                        $spaces_wanted = $spaces_wanted_2;
                    }
                }

                if ( $spaces_wanted > 0 ) {
                    my $deleted_spaces =
                      reduce_lp_indentation( $i_first_comma, $spaces_wanted );

                    # redo the math
                    if ( $deleted_spaces > 0 ) {
                        $columns = table_columns_available($i_first_comma);
                        $number_of_fields_max =
                          maximum_number_of_fields( $columns, $odd_or_even,
                            $max_width, $pair_width );
                        $number_of_fields = $number_of_fields_max;

                        if (   $number_of_fields_best == 1
                            && $number_of_fields >= 1 )
                        {
                            $number_of_fields = $number_of_fields_best;
                        }
                    }
                }
            }
        }

        # try for one column if two won't work
        if ( $number_of_fields <= 0 ) {
            $number_of_fields = int( $columns / $max_width );
        }

        # The user can place an upper bound on the number of fields,
        # which can be useful for doing maintenance on tables
        if (   $rOpts_maximum_fields_per_table
            && $number_of_fields > $rOpts_maximum_fields_per_table )
        {
            $number_of_fields = $rOpts_maximum_fields_per_table;
        }

        # How many columns (characters) and lines would this container take 
        # if no additional whitespace were added?
        my $packed_columns = token_sequence_length( $i_opening_paren + 1,
            $i_effective_last_comma + 1 );
        if ( $columns <= 0 ) { $columns = 1 }    # avoid divide by zero
        my $packed_lines = 1 + int( $packed_columns / $columns );

        # are we an item contained in an outer list?
        my $in_hierarchical_list = $next_nonblank_type =~ /^[\}\,]$/;

        if ( $number_of_fields <= 0 ) {

# =pod
# 
#         #---------------------------------------------------------------
#         # We're in trouble.  We can't find a single field width that works.
#         # There is no simple answer here; we may have a single long list
#         # item, or many.  
#         #---------------------------------------------------------------
# 
#         In many cases, it may be best to not force a break if there is just one
#         comma, because the standard continuation break logic will do a better
#         job without it.  
# 
#         In the common case that all but one of the terms can fit
#         on a single line, it may look better not to break open the
#         containing parens.  Consider, for example
# 
#             $color =
#               join ( '/',
#                 sort { $color_value{$::a} <=> $color_value{$::b}; }
#                 keys %colors );
# 
#         which will look like this with the container broken:
# 
#             $color = join (
#                 '/',
#                 sort { $color_value{$::a} <=> $color_value{$::b}; } keys %colors
#             );
# 
#         Here is an example of this rule for a long last term:
# 
#             log_message( 0, 256, 128,
#                 "Number of routes in adj-RIB-in to be considered: $peercount" );
# 
#         And here is an example with a long first term:
# 
#         $s = sprintf(
# "%2d wallclock secs (%$f usr %$f sys + %$f cusr %$f csys = %$f CPU)",
#             $r, $pu, $ps, $cu, $cs, $tt
#           )
#           if $style eq 'all';
# 
# =cut

            my $i_last_comma    = $$rcomma_index[ $comma_count - 1 ];
            my $long_last_term  = excess_line_length( 0, $i_last_comma ) <= 0;
            my $long_first_term =
              excess_line_length( $i_first_comma + 1, $max_index_to_go ) <= 0;

            # break at every comma ...
            if (

                # if requested by user or is best looking
                $number_of_fields_best == 1

                # or if this is a sublist of a larger list
                || $in_hierarchical_list

                # or if multiple commas and we dont have a long first or last
                # term
                || ( $comma_count > 1
                    && !( $long_last_term || $long_first_term ) )
              )
            {
                foreach ( 0 .. $comma_count - 1 ) {
                    set_forced_breakpoint( $$rcomma_index[$_] );
                }
            }
            elsif ($long_last_term) {

                set_forced_breakpoint($i_last_comma);
                $$rdo_not_break_apart = 1 unless $must_break_open;
            }
            elsif ($long_first_term) {

                set_forced_breakpoint($i_first_comma);
            }
            else {

                # let breaks be defined by default bond strength logic
            }
            return;
        }

        # --------------------------------------------------------
        # We have a tentative field count that seems to work.
        # How many lines will this require?
        # --------------------------------------------------------
        my $formatted_lines = $item_count / ($number_of_fields);
        if ( $formatted_lines != int $formatted_lines ) {
            $formatted_lines = 1 + int $formatted_lines;
        }

        # So far we've been trying to fill out to the right margin.  But
        # compact tables are easier to read, so let's see if we can use fewer 
        # fields without increasing the number of lines.
        $number_of_fields =
          compactify_table( $item_count, $number_of_fields, $formatted_lines,
            $odd_or_even );

        # How many spaces across the page will we fill?
        my $columns_per_line =
          ( int $number_of_fields / 2 ) * $pair_width +
          ( $number_of_fields % 2 ) * $max_width;

        my $formatted_columns;

        if ( $number_of_fields > 1 ) {
            $formatted_columns =
              ( $pair_width * ( int( $item_count / 2 ) ) + ( $item_count % 2 ) *
                  $max_width );
        }
        else {
            $formatted_columns = $max_width * $item_count;
        }
        if ( $formatted_columns < $packed_columns ) {
            $formatted_columns = $packed_columns;
        }

        my $unused_columns = $formatted_columns - $packed_columns;

        # set some empirical parameters to help decide if we should try to
        # align; high sparsity does not look good, especially with few lines
        my $sparsity = ($unused_columns) / ($formatted_columns);
        my $max_allowed_sparsity =
          ( $item_count < 3 ) ? 0.1
          : ( $packed_lines == 1 ) ? 0.15
          : ( $packed_lines == 2 ) ? 0.4
          : 0.7;

        # Begin check for shortcut methods, which avoid treating a list
        # as a table for relatively small parenthesized lists.  These
        # are usually easier to read if not formatted as tables.
        if (
            $packed_lines <= 2    # probably can fit in 2 lines
            && $item_count < 9    # doesn't have too many items
            && $opening_environment eq 'BLOCK'    # not a sub-container
            && $opening_token       eq '('        # is paren list
          )
        {

            # Shortcut method 1: for -lp and just one comma:
            # This is a no-brainer, just break at the comma.
            if (
                $rOpts_line_up_parentheses        # -lp
                && $item_count == 2               # two items, one comma
                && !$must_break_open
              )
            {
                my $i_break = $$rcomma_index[0];
                set_forced_breakpoint($i_break);
                $$rdo_not_break_apart = 1;
                set_non_alignment_flags( $comma_count, $rcomma_index );
                return;

            }

            # method 2 is for most small ragged lists which might look
            # best if not displayed as a table.
            if (
                ( $number_of_fields == 2 && $item_count == 3 )
                || (
                    $new_identifier_count > 0    # isn't all quotes
                    && $sparsity > 0.15
                )    # would be fairly spaced gaps if aligned
              )
            {

                my $break_count =
                  set_ragged_breakpoints( \@i_term_comma,
                    $ri_ragged_break_list );
                ++$break_count if ($use_separate_first_term);

                # NOTE: we should really use the true break count here,
                # which can be greater if there are large terms and
                # little space, but usually this will work well enough.
                unless ($must_break_open) {

                    if ( $break_count <= 1 ) {
                        $$rdo_not_break_apart = 1;
                    }
                    elsif ( $rOpts_line_up_parentheses && !$need_lp_break_open )
                    {
                        $$rdo_not_break_apart = 1;
                    }
                }
                set_non_alignment_flags( $comma_count, $rcomma_index );
                return;
            }

        }    # end shortcut methods

        # debug stuff

        FORMATTER_DEBUG_FLAG_SPARSE && do {
            print
"SPARSE:cols=$columns commas=$comma_count items:$item_count ids=$identifier_count pairwidth=$pair_width fields=$number_of_fields lines packed: $packed_lines packed_cols=$packed_columns fmtd:$formatted_lines cols /line:$columns_per_line  unused:$unused_columns fmtd:$formatted_columns sparsity=$sparsity allow=$max_allowed_sparsity\n";

        };

        #---------------------------------------------------------------
        # Compound List Rule 2: 
        # If this list is too long for one line, and it is an item of a
        # larger list, then we must format it, regardless of sparsity
        # (ian.t).  One reason that we have to do this is to trigger
        # Compound List Rule 1, above, which causes breaks at all commas of
        # all outer lists.  In this way, the structure will be properly
        # displayed.
        #---------------------------------------------------------------

        # Decide if this list is too long for one line unless broken 
        my $total_columns = table_columns_available($i_opening_paren);
        my $too_long      = $packed_columns > $total_columns;

        # For a paren list, include the length of the token just before the
        # '(' because this is likely a sub call, and we would have to
        # include the sub name on the same line as the list.  This is still
        # imprecise, but not too bad.  (steve.t)
        if ( !$too_long && $i_opening_paren > 0 && $opening_token eq '(' ) {

            $too_long =
              excess_line_length( $i_opening_minus,
                $i_effective_last_comma + 1 ) > 0;
        }

        # FIXME: For an item after a '=>', try to include the length of the
        # thing before the '=>'.  This is crude and should be improved by
        # actually looking back token by token.
        if ( !$too_long && $i_opening_paren > 0 && $list_type eq '=>' ) {
            my $i_opening_minus = $i_opening_paren - 4;
            if ( $i_opening_minus >= 0 ) {
                $too_long =
                  excess_line_length( $i_opening_minus,
                    $i_effective_last_comma + 1 ) > 0;
            }
        }

        # Always break lists contained in '[' and '{' if too long for 1 line,
        # and always break lists which are too long and part of a more complex
        # structure.
        my $must_break_open_container = $must_break_open
          || ( $too_long
            && ( $in_hierarchical_list || $opening_token ne '(' ) );

        #print "LISTX: next=$next_nonblank_type  avail cols=$columns packed=$packed_columns must format = $must_break_open_container too-long=$too_long  opening=$opening_token list_type=$list_type formatted_lines=$formatted_lines  packed=$packed_lines max_sparsity= $max_allowed_sparsity sparsity=$sparsity \n";

        #---------------------------------------------------------------
        # The main decision:
        # Now decide if we will align the data into aligned columns.  Do not
        # attempt to align columns if this is a tiny table or it would be
        # too spaced.  It seems that the more packed lines we have, the
        # sparser the list that can be allowed and still look ok.
        #---------------------------------------------------------------

        if (   ( $formatted_lines < 3 && $packed_lines < $formatted_lines )
            || ( $formatted_lines < 2 )
            || ( $unused_columns > $max_allowed_sparsity * $formatted_columns )
          )
        {

            #---------------------------------------------------------------
            # too sparse: would look ugly if aligned in a table;
            #---------------------------------------------------------------

            # use old breakpoints if this is a 'big' list
            # FIXME: goal is to improve set_ragged_breakpoints so that
            # this is not necessary.
            if ( $packed_lines > 2 && $item_count > 10 ) {
                write_logfile_entry("List sparse: using old breakpoints\n");
                copy_old_breakpoints( $i_first_comma, $i_last_comma );
            }

            # let the continuation logic handle it if 2 lines
            else {

                my $break_count =
                  set_ragged_breakpoints( \@i_term_comma,
                    $ri_ragged_break_list );
                ++$break_count if ($use_separate_first_term);

                unless ($must_break_open_container) {
                    if ( $break_count <= 1 ) {
                        $$rdo_not_break_apart = 1;
                    }
                    elsif ( $rOpts_line_up_parentheses && !$need_lp_break_open )
                    {
                        $$rdo_not_break_apart = 1;
                    }
                }
                set_non_alignment_flags( $comma_count, $rcomma_index );
            }
            return;
        }

        #---------------------------------------------------------------
        # go ahead and format as a table
        #---------------------------------------------------------------
        write_logfile_entry(
            "List: auto formatting with $number_of_fields fields/row\n");

        my $j_first_break =
          $use_separate_first_term ? $number_of_fields : $number_of_fields - 1;

        for (
            my $j = $j_first_break ;
            $j < $comma_count ;
            $j += $number_of_fields
          )
        {
            my $i = $$rcomma_index[$j];
            set_forced_breakpoint($i);
        }
        return;
    }
}

sub set_non_alignment_flags {

    # set flag which indicates that these commas should not be
    # aligned
    my ( $comma_count, $rcomma_index ) = @_;
    foreach ( 0 .. $comma_count - 1 ) {
        $matching_token_to_go[ $$rcomma_index[$_] ] = 1;
    }
}

sub study_list_complexity {

    # Look for complex tables which should be formatted with one term per line.
    # Returns the following:
    #
    #  \@i_ragged_break_list = list of good breakpoints to avoid lines
    #    which are hard to read
    #  $number_of_fields_best = suggested number of fields based on
    #    complexity; = 0 if any number may be used.
    #
    my ( $ri_term_begin, $ri_term_end, $ritem_lengths, $max_width ) = @_;
    my $item_count            = @{$ri_term_begin};
    my $complex_item_count    = 0;
    my $number_of_fields_best = $rOpts_maximum_fields_per_table;
    my $i_max                 = @{$ritem_lengths} - 1;
    ##my @item_complexity;

    my $i_last_last_break = -3;
    my $i_last_break      = -2;
    my @i_ragged_break_list;

    my $definitely_complex = 30;
    my $definitely_simple  = 12;
    my $quote_count        = 0;

    for my $i ( 0 .. $i_max ) {
        my $ib = $ri_term_begin->[$i];
        my $ie = $ri_term_end->[$i];

        # define complexity: start with the actual term length 
        my $weighted_length = ( $ritem_lengths->[$i] - 2 );

        ##TBD: join types here and check for variations
        ##my $str=join "", @tokens_to_go[$ib..$ie];

        my $is_quote = 0;
        if ( $types_to_go[$ib] =~ /^[qQ]$/ ) {
            $is_quote = 1;
            $quote_count++;
        }
        elsif ( $types_to_go[$ib] =~ /^[w\-]$/ ) {
            $quote_count++;
        }

        if ( $ib eq $ie ) {
            if ( $is_quote && $tokens_to_go[$ib] =~ /\s/ ) {
                $complex_item_count++;
                $weighted_length *= 2;
            }
            else {
            }
        }
        else {
            if ( grep { $_ eq 'b' } @types_to_go[ $ib .. $ie ] ) {
                $complex_item_count++;
                $weighted_length *= 2;
            }
            if ( grep { $_ eq '..' } @types_to_go[ $ib .. $ie ] ) {
                $weighted_length += 4;
            }
        }

        # add weight for extra tokens.
        $weighted_length += 2 * ( $ie - $ib );

##        my $BUB = join '', @tokens_to_go[$ib..$ie];
##        print "# COMPLEXITY:$weighted_length   $BUB\n";

##push @item_complexity, $weighted_length;

        # now mark a ragged break after this item it if it is 'long and
        # complex':
        if ( $weighted_length >= $definitely_complex ) {

            # if we broke after the previous term
            # then break before it too
            if (   $i_last_break == $i - 1
                && $i > 1
                && $i_last_last_break != $i - 2 )
            {

                ## FIXME: don't strand a small term
                pop @i_ragged_break_list;
                push @i_ragged_break_list, $i - 2;
                push @i_ragged_break_list, $i - 1;
            }

            push @i_ragged_break_list, $i;
            $i_last_last_break = $i_last_break;
            $i_last_break      = $i;
        }

        # don't break before a small last term -- it will
        # not look good on a line by itself.
        elsif ($i == $i_max
            && $i_last_break == $i - 1
            && $weighted_length <= $definitely_simple )
        {
            pop @i_ragged_break_list;
        }
    }

    my $identifier_count = $i_max + 1 - $quote_count;

    # Need more tuning here..
    if (   $max_width > 12
        && $complex_item_count > $item_count / 2
        && $number_of_fields_best != 2 )
    {
        $number_of_fields_best = 1;
    }

    return ( $number_of_fields_best, \@i_ragged_break_list, $identifier_count );
}

sub get_maximum_fields_wanted {

    # Not all tables look good with more than one field of items.
    # This routine looks at a table and decides if it should be
    # formatted with just one field or not.  
    # This coding is still under development.
    my ($ritem_lengths) = @_;

    my $number_of_fields_best = 0;

    # For just a few items, we tentatively assume just 1 field.
    my $item_count = @{$ritem_lengths};
    if ( $item_count <= 5 ) {
        $number_of_fields_best = 1;
    }

    # For larger tables, look at it both ways and see what looks best
    else {

        my $is_odd            = 1;
        my @max_length        = ( 0, 0 );
        my @last_length_2     = ( undef, undef );
        my @first_length_2    = ( undef, undef );
        my $last_length       = undef;
        my $total_variation_1 = 0;
        my $total_variation_2 = 0;
        my @total_variation_2 = ( 0, 0 );
        for ( my $j = 0 ; $j < $item_count ; $j++ ) {

            $is_odd = 1 - $is_odd;
            my $length = $ritem_lengths->[$j];
            if ( $length > $max_length[$is_odd] ) {
                $max_length[$is_odd] = $length;
            }

            if ( defined($last_length) ) {
                my $dl = abs( $length - $last_length );
                $total_variation_1 += $dl;
            }
            $last_length = $length;

            my $ll = $last_length_2[$is_odd];
            if ( defined($ll) ) {
                my $dl = abs( $length - $ll );
                $total_variation_2[$is_odd] += $dl;
            }
            else {
                $first_length_2[$is_odd] = $length;
            }
            $last_length_2[$is_odd] = $length;
        }
        $total_variation_2 = $total_variation_2[0] + $total_variation_2[1];

        my $factor = ( $item_count > 10 ) ? 1 : ( $item_count > 5 ) ? 0.75 : 0;
        unless ( $total_variation_2 < $factor * $total_variation_1 ) {
            $number_of_fields_best = 1;
        }
    }
    return ($number_of_fields_best);
}

sub table_columns_available {
    my $i_first_comma = shift;
    my $columns       =
      $rOpts_maximum_line_length - leading_spaces_to_go($i_first_comma);

    # Patch: the vertical formatter does not line up lines whose lengths
    # exactly equal the available line length because of allowances
    # that must be made for side comments.  Therefore, the number of
    # available columns is reduced by 1 character.
    $columns -= 1;
    return $columns;
}

sub maximum_number_of_fields {

    # how many fields will fit in the available space?
    my ( $columns, $odd_or_even, $max_width, $pair_width ) = @_;
    my $max_pairs        = int( $columns / $pair_width );
    my $number_of_fields = $max_pairs * 2;
    if (   $odd_or_even == 1
        && $max_pairs * $pair_width + $max_width <= $columns )
    {
        $number_of_fields++;
    }
    return $number_of_fields;
}

sub compactify_table {

    # given a table with a certain number of fields and a certain number
    # of lines, see if reducing the number of fields will make it look
    # better.
    my ( $item_count, $number_of_fields, $formatted_lines, $odd_or_even ) = @_;
    if ( $number_of_fields >= $odd_or_even * 2 && $formatted_lines > 0 ) {
        my $min_fields;

        for (
            $min_fields = $number_of_fields ;
            $min_fields >= $odd_or_even
            && $min_fields * $formatted_lines >= $item_count ;
            $min_fields -= $odd_or_even
          )
        {
            $number_of_fields = $min_fields;
        }
    }
    return $number_of_fields;
}

sub set_ragged_breakpoints {

    # Set breakpoints in a list that cannot be formatted nicely as a
    # table.  
    my ( $ri_term_comma, $ri_ragged_break_list ) = @_;

    my $break_count = 0;
    foreach (@$ri_ragged_break_list) {
        my $j = $ri_term_comma->[$_];
        if ($j) {
            set_forced_breakpoint($j);
            $break_count++;
        }
    }
    return $break_count;
}

sub copy_old_breakpoints {
    my ( $i_first_comma, $i_last_comma ) = @_;
    for my $i ( $i_first_comma .. $i_last_comma ) {
        if ( $old_breakpoint_to_go[$i] ) {
            set_forced_breakpoint($i);
        }
    }
}

sub set_nobreaks {
    my ( $i, $j ) = @_;
    if ( $i >= 0 && $i <= $j && $j <= $max_index_to_go ) {

        FORMATTER_DEBUG_FLAG_NOBREAK && do {
            my ( $a, $b, $c ) = caller();
            print(
"NOBREAK: forced_breakpoint $forced_breakpoint_count from $a $c with i=$i max=$max_index_to_go type=$types_to_go[$i]\n"
            );
        };

        @nobreak_to_go[ $i .. $j ] = (1) x ( $j - $i + 1 );
    }

    # shouldn't happen; non-critical error
    else {
        FORMATTER_DEBUG_FLAG_NOBREAK && do {
            my ( $a, $b, $c ) = caller();
            print(
"NOBREAK ERROR: from $a $c with i=$i j=$j max=$max_index_to_go\n"
            );
        };
    }
}

sub set_fake_breakpoint {

    # Just bump up the breakpoint count as a signal that there are breaks.
    # This is useful if we have breaks but may want to postpone deciding where
    # to make them.
    $forced_breakpoint_count++;
}

sub set_forced_breakpoint {
    my $i = shift;

    return unless defined $i && $i >= 0;

    # when called with certain tokens, use bond strengths to decide
    # if we break before or after it
    my $token = $tokens_to_go[$i];
    if ( $token =~ /^([\.\,\:\?]|&&|\|\|)$/ ) {
        if ( $want_break_before{$token} && $i >= 0 ) { $i-- }
    }

    # breaks are forced before 'or' and 'and' for now:
    if ( $is_if_unless_and_or{$token} ) { $i-- }

    if ( $i >= 0 && $i <= $max_index_to_go ) {
        my $i_nonblank = ( $types_to_go[$i] ne 'b' ) ? $i : $i - 1;

        FORMATTER_DEBUG_FLAG_FORCE && do {
            my ( $a, $b, $c ) = caller();
            print
"FORCE forced_breakpoint $forced_breakpoint_count from $a $c with i=$i_nonblank max=$max_index_to_go tok=$tokens_to_go[$i_nonblank] type=$types_to_go[$i_nonblank] nobr=$nobreak_to_go[$i_nonblank]\n";
        };

        if ( $i_nonblank >= 0 && $nobreak_to_go[$i_nonblank] == 0 ) {
            $forced_breakpoint_to_go[$i_nonblank] = 1;

            if ( $i_nonblank > $index_max_forced_break ) {
                $index_max_forced_break = $i_nonblank;
            }
            $forced_breakpoint_count++;
            $forced_breakpoint_undo_stack[ $forced_breakpoint_undo_count++ ] =
              $i_nonblank;

            # if we break at an opening container..break at the closing
            if ( $tokens_to_go[$i_nonblank] =~ /^[\{\[\(\?]$/ ) {
                set_closing_breakpoint($i_nonblank);
            }
        }
    }
}

sub clear_breakpoint_undo_stack {
    $forced_breakpoint_undo_count = 0;
}

sub undo_forced_breakpoint_stack {

    my $i_start = shift;
    if ( $i_start < 0 ) {
        $i_start = 0;
        my ( $a, $b, $c ) = caller();
        warning(
"Program Bug: undo_forced_breakpoint_stack from $a $c has i=$i_start "
        );
    }

    while ( $forced_breakpoint_undo_count > $i_start ) {
        my $i =
          $forced_breakpoint_undo_stack[ --$forced_breakpoint_undo_count ];
        if ( $i >= 0 && $i <= $max_index_to_go ) {
            $forced_breakpoint_to_go[$i] = 0;
            $forced_breakpoint_count--;

            FORMATTER_DEBUG_FLAG_UNDOBP && do {
                my ( $a, $b, $c ) = caller();
                print(
"UNDOBP: undo forced_breakpoint i=$i $forced_breakpoint_undo_count from $a $c max=$max_index_to_go\n"
                );
            };
        }

        # shouldn't happen, but not a critical error
        else {
            FORMATTER_DEBUG_FLAG_UNDOBP && do {
                my ( $a, $b, $c ) = caller();
                print(
"Program Bug: undo_forced_breakpoint from $a $c has i=$i but max=$max_index_to_go"
                );
            };
        }
    }
}

{    # begin recombine_breakpoints
    my %is_last_next_redo_return;
    my %is_if_unless;
    my %is_and_or;

    BEGIN {
        @_ = qw(last next redo return);
        @is_last_next_redo_return{@_} = (1) x scalar(@_);

        @_ = qw(if unless);
        @is_if_unless{@_} = (1) x scalar(@_);

        @_ = qw(and or);
        @is_and_or{@_} = (1) x scalar(@_);
    }

    sub recombine_breakpoints {

        # sub set_continuation_breaks is very liberal in setting line breaks
        # for long lines, always setting breaks at good breakpoints, even
        # when that creates small lines.  Occasionally small line fragments
        # are produced which would look better if they were combined.
        # That's the task of this routine, recombine_breakpoints.
        my ( $ri_first, $ri_last ) = @_;
        my $more_to_do = 1;

        # Keep looping until there are no more possible recombinations
        my $nmax_last = @$ri_last;
        while ($more_to_do) {
            my $n_best = 0;
            my $bs_best;
            my $n;
            my $nmax = @$ri_last - 1;

            # safety check..
            unless ( $nmax < $nmax_last ) {

                # shouldn't happen because splice below decreases nmax on each pass:
                # but i get paranoid sometimes
                die "Program bug-infinite loop in recombine breakpoints\n";
            }
            $nmax_last  = $nmax;
            $more_to_do = 0;

            # loop over all remaining lines...
            for $n ( 1 .. $nmax ) {

                #----------------------------------------------------------
                # Indexes of the endpoints of the two lines are:
                #
                #  ---left---- | ---right---
                #  $if   $imid | $imidr   $il
                #
                # We want to decide if we should join tokens $imid to $imidr
                #----------------------------------------------------------
                my $if    = $$ri_first[ $n - 1 ];
                my $il    = $$ri_last[$n];
                my $imid  = $$ri_last[ $n - 1 ];
                my $imidr = $$ri_first[$n];

                #print "RECOMBINE: n=$n imid=$imid if=$if type=$types_to_go[$if] =$tokens_to_go[$if] next_type=$types_to_go[$imidr] next_tok=$tokens_to_go[$imidr]\n";

                #----------------------------------------------------------
                # Start of special recombination rules 
                # These are ad-hoc rules which have been found to work ok.
                # Skip to next pair to avoid re-combination.  
                #----------------------------------------------------------

                # a terminal '{' should stay where it is
                next if ( $n == $nmax && $types_to_go[$imidr] eq '{' );

                #----------------------------------------------------------
                # examine token at $imid  (right end of first line of pair)
                #----------------------------------------------------------

                # an isolated '}' may join with a ';' terminated segment
                if ( $types_to_go[$imid] eq '}' ) {
                    next
                      unless (

                        # join } and ;
                        ( ( $if == $imid ) && ( $types_to_go[$il] eq ';' ) )

                        # handle '.' and '?' below
                        || ( $types_to_go[$imidr] =~ /^[\.\?]$/ )
                      );
                }

                # for lines ending in a comma...
                elsif ( $types_to_go[$imid] eq ',' ) {

                    # an isolated '},' may join with an identifier + ';' 
                    # this is useful for the class of a 'bless' statement (bless.t)
                    if (   $types_to_go[$if] eq '}'
                        && $types_to_go[$imidr] eq 'i' )
                    {
                        next
                          unless ( ( $if == ( $imid - 1 ) )
                            && ( $il == ( $imidr + 1 ) )
                            && ( $types_to_go[$il] eq ';' ) );

                        # override breakpoint
                        $forced_breakpoint_to_go[$imid] = 0;
                    }

                    # but otherwise, do not recombine unless this will leave 
                    # just 1 more line
                    else {
                        next unless ( $n + 1 >= $nmax );
                    }
                }

                # opening paren..
                elsif ( $types_to_go[$imid] eq '(' ) {

                    # No longer doing this
                }

                elsif ( $types_to_go[$imid] eq ')' ) {

                    # No longer doing this
                }

                # keep a terminal colon
                elsif ( $types_to_go[$imid] eq ':' ) {
                    next;
                }

                # keep a terminal for-semicolon
                elsif ( $types_to_go[$imid] eq 'f' ) {
                    next;
                }

                # if '=' at end of line ...
                elsif ( $types_to_go[$imid] eq '=' ) {

                    # always ok to join isolated '='
                    unless ( $if == $imid ) {

                        my $is_math = (
                            ( $types_to_go[$il] =~ /^[+-\/\*\)]$/ )

                            # note no '$' in pattern because -> can start long identifier
                              && !grep { $_ =~ /^(->|=>|[\,])/ }
                              @types_to_go[ $imidr .. $il ]
                        );

                        # retain the break after the '=' unless ...
                        next
                          unless (

                            # '=' is followed by a number and looks like math
                            ( $types_to_go[$imidr] eq 'n' && $is_math )

                            # or followed by a scalar and looks like math
                            || (   ( $types_to_go[$imidr] eq 'i' )
                                && ( $tokens_to_go[$imidr] =~ /^\$/ )
                                && $is_math )

                            # or followed by a single "short" token 
                            # ('12' is arbitrary)
                            || ( $il == $imidr
                                && token_sequence_length( $imidr, $imidr ) <
                                12 )

                          );
                    }
                    unless ( $tokens_to_go[$imidr] =~ /^[\{\(\[]$/ ) {
                        $forced_breakpoint_to_go[$imid] = 0;
                    }
                }

                # for keywords..
                elsif ( $types_to_go[$imid] eq 'k' ) {

                    # make major control keywords stand out
                    # (recombine.t)
                    next
                      if (

                        #/^(last|next|redo|return)$/
                        $is_last_next_redo_return{ $tokens_to_go[$imid] }
                      );
                }

                #----------------------------------------------------------
                # examine token at $imidr (left end of second line of pair)
                #----------------------------------------------------------

                # do not recombine lines with leading &&, ||, or :
                if ( $types_to_go[$imidr] =~ /^(|:|\&\&|\|\|)$/ ) {
                    next;
                }

                # Identify and recombine a broken ?/: chain
                elsif ( $types_to_go[$imidr] eq '?' ) {

                    # indexes of line first tokens --
                    #  mm  - line before previous line
                    #  f   - previous line
                    #     <-- this line
                    #  ff  - next line
                    #  fff - line after next
                    my $iff = $n < $nmax ? $$ri_first[ $n + 1 ] : -1;
                    my $ifff = $n + 2 <= $nmax ? $$ri_first[ $n + 2 ] : -1;
                    my $imm = $n > 1 ? $$ri_first[ $n - 2 ] : -1;
                    my $seqno = $type_sequence_to_go[$imidr];
                    my $f_ok  =
                      (      $tokens_to_go[$if] eq ':'
                          && $type_sequence_to_go[$if] ==
                          $seqno - TYPE_SEQUENCE_INCREMENT );
                    my $mm_ok =
                      (      $imm >= 0
                          && $tokens_to_go[$imm] eq ':'
                          && $type_sequence_to_go[$imm] ==
                          $seqno - 2 * TYPE_SEQUENCE_INCREMENT );

                    my $ff_ok =
                      (      $iff > 0
                          && $tokens_to_go[$iff] eq ':'
                          && $type_sequence_to_go[$iff] == $seqno );
                    my $fff_ok =
                      (      $ifff > 0
                          && $tokens_to_go[$ifff] eq ':'
                          && $type_sequence_to_go[$ifff] ==
                          $seqno + TYPE_SEQUENCE_INCREMENT );

                    # we require that this '?' be part of a correct sequence
                    # of 3 in a row or else no recombination is done.
                    next
                      unless ( ( $ff_ok || $mm_ok ) && ( $f_ok || $fff_ok ) );
                    $forced_breakpoint_to_go[$imid] = 0;
                }

                # do not recombine lines with leading '.' 
                elsif ( $types_to_go[$imidr] =~ /^(\.)$/ ) {
                    my $i_next_nonblank = $imidr + 1;
                    if ( $types_to_go[$i_next_nonblank] eq 'b' ) {
                        $i_next_nonblank++;
                    }

                    next
                      unless (

                        #      ... unless there is just one and we can reduce this to
                        #      two lines if we do.  For example, this :
                        #      
                        #                $bodyA .=
                        #                  '($dummy, $pat) = &get_next_tex_cmd;' . '$args .= $pat;'
                        #
                        #      looks better than this:
                        #                $bodyA .= '($dummy, $pat) = &get_next_tex_cmd;' 
                        #                   . '$args .= $pat;'

                        (
                               $n == 2
                            && $n == $nmax
                            && $types_to_go[$if] ne $types_to_go[$imidr]
                        )

                        #
                        #      ... or this would strand a short quote , like this
                        #                . "some long qoute"
                        #                . "\n";
                        #

                        || (   $types_to_go[$i_next_nonblank] eq 'Q'
                            && $i_next_nonblank >= $il - 1
                            && length( $tokens_to_go[$i_next_nonblank] ) <
                            $rOpts_short_concatenation_item_length )
                      );
                }

                # handle leading keyword..
                elsif ( $types_to_go[$imidr] eq 'k' ) {

                    # handle leading "and" and "or" 
                    if ( $is_and_or{ $tokens_to_go[$imidr] } ) {

                        # Decide if we will combine a single terminal 'and' and
                        # 'or' after an 'if' or 'unless'.  We should consider the
                        # possible vertical alignment, and visual clutter.

# =pod
# 
#     This looks best with the 'and' on the same line as the 'if':
# 
#         $a = 1
#           if $seconds and $nu < 2;
# 
#     But this looks better as shown:
# 
#         $a = 1
#           if !$this->{Parents}{$_}
#           or $this->{Parents}{$_} eq $_;
# 
#     Eventually, it would be nice to look for similarities (such as 'this' or
#     'Parents'), but for now I'm using a simple rule that says that the 
#     resulting line length must not be more than half the maximum line length
#     (making it 80/2 = 40 characters by default).
# 
# =cut

                        next
                          unless (
                            $n == $nmax    # if this is the last line
                            && $types_to_go[$il] eq ';' # ending in ';'
                            && $types_to_go[$if] eq 'k' # after 'if' or 'unless'
                                                        #   /^(if|unless)$/
                            && $is_if_unless{ $tokens_to_go[$if] }

                            # and if this doesn't make a long last line
                            && total_line_length( $if, $il ) <=
                            $half_maximum_line_length
                          );

                        # override breakpoint
                        $forced_breakpoint_to_go[$imid] = 0;
                    }

                    # handle leading "if" and "unless" 
                    elsif ( $is_if_unless{ $tokens_to_go[$imidr] } ) {

                        # FIXME: This is still experimental..may not be too useful
                        next
                          unless (
                            $n == $nmax    # if this is the last line
                            && $types_to_go[$il] eq ';'    # ending in ';'
                            && $types_to_go[$if] eq 'k'

                            #   /^(and|or)$/
                            && $is_and_or{ $tokens_to_go[$if] }

                            # and if this doesn't make a long last line
                            && total_line_length( $if, $il ) <=
                            $half_maximum_line_length
                          );

                        # override breakpoint
                        $forced_breakpoint_to_go[$imid] = 0;
                    }

                    # handle all other leading keywords
                    else {

                        # keywords look best at start of lines,
                        # but combine things like "1 while" 

                        unless ( $types_to_go[$imid] eq '=' ) {
                            next
                              if ( ( $types_to_go[$imid] ne 'k' )
                                && ( $tokens_to_go[$imidr] !~ /^(while)$/ ) );
                        }
                    }
                }

                # similar treatment of && and || as above for 'and' and 'or':
                elsif ( $types_to_go[$imidr] =~ /^(&&|\|\|)$/ ) {

                    # maybe looking at something like:
                    #   unless $TEXTONLY || $item =~ m%</?(hr>|p>|a|img)%i;

                    next
                      unless (
                        $n == $nmax    # if this is the last line
                        && $types_to_go[$il] eq ';'  # ending in ';'
                        && $types_to_go[$if] eq 'k'  # after an 'if' or 'unless'
                                                     #   /^(if|unless)$/
                        && $is_if_unless{ $tokens_to_go[$if] }

                        # and if this doesn't make a long last line
                        && total_line_length( $if, $il ) <=
                        $half_maximum_line_length
                      );

                    # override breakpoint
                    $forced_breakpoint_to_go[$imid] = 0;
                }

                # honor hard breakpoints
                next if ( $forced_breakpoint_to_go[$imid] > 0 );

                #----------------------------------------------------------
                # end of special recombination rules 
                #----------------------------------------------------------

                my $bs = $bond_strength_to_go[$imid];

                # combined line cannot be too long
                next
                  if excess_line_length( $if, $il ) > 0;

                # do not recombine if we would skip in indentation levels
                if ( $n < $nmax ) {
                    my $if_next = $$ri_first[ $n + 1 ];
                    next
                      if (
                           $levels_to_go[$if] < $levels_to_go[$imidr]
                        && $levels_to_go[$imidr] < $levels_to_go[$if_next]

                        # but an isolated 'if (' is undesirable
                        && !(
                               $n == 1
                            && $imid - $if <= 2
                            && $types_to_go[$if]  eq 'k'
                            && $tokens_to_go[$if] eq 'if'
                            && $tokens_to_go[$imid] ne '('
                        )

                        #
                      );
                }

                # honor no-break's
                next if ( $bs == NO_BREAK );

                # remember the pair with the greatest bond strength
                if ( !$n_best ) {
                    $n_best  = $n;
                    $bs_best = $bs;
                }
                else {

                    if ( $bs > $bs_best ) {
                        $n_best  = $n;
                        $bs_best = $bs;
                    }

                    # we have 2 or more candidates, so need another pass
                    $more_to_do++;
                }
            }

            # recombine the pair with the greatest bond strength
            if ($n_best) {
                splice @$ri_first, $n_best, 1;
                splice @$ri_last, $n_best - 1, 1;
            }
        }
        return ( $ri_first, $ri_last );
    }
}

sub set_continuation_breaks {

    # Define an array of indexes for inserting newline characters to
    # keep the line lengths below the maximum desired length.  There is
    # an implied break after the last token, so it need not be included.
    # We'll break at points where the bond strength is lowest.

    my $saw_good_break = shift;
    my @i_first        = ();      # the first index to output
    my @i_last         = ();      # the last index to output
    my @i_colon_breaks = ();      # needed to decide if we have to break at ?'s
    if ( $tokens_to_go[0] eq ':' ) { push @i_colon_breaks, 0 }

    set_bond_strengths();

    my $imin = 0;
    my $imax = $max_index_to_go;
    if ( $types_to_go[$imin] eq 'b' ) { $imin++ }
    if ( $types_to_go[$imax] eq 'b' ) { $imax-- }
    my $i_begin = $imin;

    my $leading_spaces          = leading_spaces_to_go($imin);
    my $line_count              = 0;
    my $last_break_strength     = NO_BREAK;
    my $i_last_break            = -1;
    my $max_bias                = 0.001;
    my $tiny_bias               = 0.0001;
    my $leading_alignment_token = "";
    my $leading_alignment_type  = "";

    # see if any ?/:'s are in order
    my $colons_in_order = 1;
    my $last_tok        = "";
    my @colon_list = grep /^[\?\:]$/, @tokens_to_go[ 0 .. $max_index_to_go ];
    foreach (@colon_list) {
        if ( $_ eq $last_tok ) { $colons_in_order = 0; last }
        $last_tok = $_;
    }

    # This is a sufficient but not necessary condition for colon chain
    my $is_colon_chain = ( $colons_in_order && @colon_list > 2 );

    while ( $i_begin <= $imax ) {
        my $lowest_strength        = NO_BREAK;
        my $starting_sum           = $lengths_to_go[$i_begin];
        my $i_lowest               = -1;
        my $i_test                 = -1;
        my $lowest_next_token      = '';
        my $lowest_next_type       = 'b';
        my $i_lowest_next_nonblank = -1;

        # loop to find next break point
        for ( $i_test = $i_begin ; $i_test <= $imax ; $i_test++ ) {
            my $type            = $types_to_go[$i_test];
            my $token           = $tokens_to_go[$i_test];
            my $next_type       = $types_to_go[ $i_test + 1 ];
            my $next_token      = $tokens_to_go[ $i_test + 1 ];
            my $i_next_nonblank =
              ( ( $next_type eq 'b' ) ? $i_test + 2 : $i_test + 1 );
            my $next_nonblank_type       = $types_to_go[$i_next_nonblank];
            my $next_nonblank_token      = $tokens_to_go[$i_next_nonblank];
            my $next_nonblank_block_type = $block_type_to_go[$i_next_nonblank];
            my $strength                 = $bond_strength_to_go[$i_test];
            my $must_break               = 0;

            # FIXME: TESTING: Might want to be able to break after these
            # force an immediate break at certain operators 
            # with lower level than the start of the line
            if (
                (
                    $next_nonblank_type =~ /^(\.|\&\&|\|\|)$/
                    || (   $next_nonblank_type eq 'k'
                        && $next_nonblank_token =~ /^(and|or)$/ )
                )
                && ( $nesting_depth_to_go[$i_begin] >
                    $nesting_depth_to_go[$i_next_nonblank] )
              )
            {
                set_forced_breakpoint($i_next_nonblank);
            }

            if (

                # Try to put a break where requested by scan_list
                $forced_breakpoint_to_go[$i_test]

                # break between ) { in a continued line so that the '{' can
                # be outdented
                # See similar logic in scan_list which catches instances
                # where a line is just something like ') {'
                || (   $line_count
                    && ( $token eq ')' )
                    && ( $next_nonblank_type eq '{' )
                    && ($next_nonblank_block_type)
                    && !$rOpts->{'opening-brace-always-on-right'} )

                # There is an implied forced break at a terminal opening brace
                || ( ( $type eq '{' ) && ( $i_test == $imax ) )

              )
            {

                # Forced breakpoints must sometimes be overridden, for example
                # because of a side comment causing a NO_BREAK.  It is easier
                # to catch this here than when they are set.
                if ( $strength < NO_BREAK ) {
                    $strength   = $lowest_strength - $tiny_bias;
                    $must_break = 1;
                }
            }

            # quit if a break here would put a good terminal token on
            # the next line and we already have a possible break
            if (
                   !$must_break
                && ( $next_nonblank_type =~ /^[\;\,]$/ )
                && (
                    (
                        $leading_spaces + $lengths_to_go[ $i_next_nonblank + 1 ]
                        - $starting_sum
                    ) > $rOpts_maximum_line_length
                )
              )
            {
                last if ( $i_lowest >= 0 );
            }

            # Avoid a break which would strand a single punctuation
            # token.  For example, we do not want to strand a leading
            # '.' which is followed by a long quoted string.
            if (
                   !$must_break
                && ( $i_test == $i_begin )
                && ( $i_test < $imax )
                && ( $token eq $type )
                && (
                    (
                        $leading_spaces + $lengths_to_go[ $i_test + 1 ] -
                        $starting_sum
                    ) <= $rOpts_maximum_line_length
                )
              )
            {
                $i_test++;

                if ( ( $i_test < $imax ) && ( $next_type eq 'b' ) ) {
                    $i_test++;
                }
                redo;
            }

            if ( ( $strength <= $lowest_strength ) && ( $strength < NO_BREAK ) )
            {

                # break at previous best break if it would have produced
                # a leading alignment of certain common tokens, and it
                # is different from the latest candidate break
                last
                  if ($leading_alignment_type);

                # Force at least one breakpoint if old code had good
                # break It is only called if a breakpoint is required or
                # desired.  This will probably need some adjustments
                # over time.  A goal is to try to be sure that, if a new
                # side comment is introduced into formated text, then
                # the same breakpoints will occur.  scbreak.t
                last
                  if (
                    $i_test == $imax                # we are at the end
                    && !$forced_breakpoint_count    #
                    && $saw_good_break              # old line had good break
                    && $type =~ /^[#;\{]$/          # and this line ends in
                                                    # ';' or side comment
                    && $i_last_break < 0        # and we haven't made a break
                    && $i_lowest > 0            # and we saw a possible break
                    && $i_lowest < $imax - 1    # (but not just before this ;)
                    && $strength - $lowest_strength < 0.5 * WEAK # and it's good
                  );

                $lowest_strength        = $strength;
                $i_lowest               = $i_test;
                $lowest_next_token      = $next_nonblank_token;
                $lowest_next_type       = $next_nonblank_type;
                $i_lowest_next_nonblank = $i_next_nonblank;
                last if $must_break;

                # set flags to remember if a break here will produce a
                # leading alignment of certain common tokens
                if (
                       $line_count > 0
                    && $i_test < $imax
                    && ( $lowest_strength - $last_break_strength <= $max_bias )
                    && ( $nesting_depth_to_go[$i_begin] >=
                        $nesting_depth_to_go[$i_next_nonblank] )
                    && (
                        (
                               $types_to_go[$i_begin] =~ /^(\.|\&\&|\|\||:)$/
                            && $types_to_go[$i_begin] eq $next_nonblank_type
                        )
                        || (   $tokens_to_go[$i_begin] =~ /^(and|or)$/
                            && $tokens_to_go[$i_begin] eq $next_nonblank_token )
                    )
                  )
                {
                    $leading_alignment_token = $next_nonblank_token;
                    $leading_alignment_type  = $next_nonblank_type;
                }
            }

            my $too_long =
              ( $i_test >= $imax )
              ? 1
              : (
                (
                    $leading_spaces + $lengths_to_go[ $i_test + 2 ] -
                      $starting_sum
                ) > $rOpts_maximum_line_length
              );

            FORMATTER_DEBUG_FLAG_BREAK
              && print
"BREAK: testing i = $i_test imax=$imax $types_to_go[$i_test] $next_nonblank_type leading sp=($leading_spaces) next length = $lengths_to_go[$i_test+2] too_long=$too_long str=$strength\n";

            # allow one extra terminal token after exceeding line length
            # if it would strand this token.
            if (   $rOpts_fuzzy_line_length
                && $too_long
                && ( $i_lowest == $i_test )
                && ( length($token) > 1 )
                && ( $next_nonblank_type =~ /^[\;\,]$/ ) )
            {
                $too_long = 0;
            }

            last
              if (
                ( $i_test == $imax )    # we're done if no more tokens,
                || (
                    ( $i_lowest >= 0 )    # or no more space and we have a break
                    && $too_long
                )
              );
        }

        # it's always ok to break at imax if no other break was found
        if ( $i_lowest < 0 ) { $i_lowest = $imax }

        # semi-final index calculation
        my $i_next_nonblank = (
            ( $types_to_go[ $i_lowest + 1 ] eq 'b' )
            ? $i_lowest + 2
            : $i_lowest + 1
        );
        my $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        my $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        #-------------------------------------------------------
        # ?/: rule 1 : if a break here will separate a '?' on this
        # line from its closing ':', then break at the '?' instead.
        #-------------------------------------------------------
        my $i;
        foreach $i ( $i_begin + 1 .. $i_lowest - 1 ) {
            next unless ( $tokens_to_go[$i] eq '?' );

            # do not break if probable sequence of ?/: statements
            next if ($is_colon_chain);

            # do not break if statement is broken by side comment
            next
              if (
                $tokens_to_go[$max_index_to_go] eq '#'
                && terminal_type( \@types_to_go, \@block_type_to_go, 0,
                    $max_index_to_go ) !~ /^[\;\}]$/
              );

            # no break needed if matching : is also on the line
            next
              if ( $mate_index_to_go[$i] >= 0
                && $mate_index_to_go[$i] <= $i_next_nonblank );

            $i_lowest = $i;
            if ( $want_break_before{'?'} ) { $i_lowest-- }
            last;
        }

        # final index calculation
        $i_next_nonblank = (
            ( $types_to_go[ $i_lowest + 1 ] eq 'b' )
            ? $i_lowest + 2
            : $i_lowest + 1
        );
        $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        FORMATTER_DEBUG_FLAG_BREAK
          && print "BREAK: best is i = $i_lowest strength = $lowest_strength\n";

        #-------------------------------------------------------
        # ?/: rule 2 : if we break at a '?', then break at its ':'
        #
        # Note: this rule is also in sub scan_list to handle a break
        # at the start and end of a line (in case breaks are dictated
        # by side comments).
        #-------------------------------------------------------
        if ( $next_nonblank_type eq '?' ) {
            set_closing_breakpoint($i_next_nonblank);
        }
        elsif ( $types_to_go[$i_lowest] eq '?' ) {
            set_closing_breakpoint($i_lowest);
        }

        #-------------------------------------------------------
        # ?/: rule 3 : if we break at a ':' then we save
        # its location for further work below.  We may need to go 
        # back and break at its '?'.
        #-------------------------------------------------------
        if ( $next_nonblank_type eq ':' ) {
            push @i_colon_breaks, $i_next_nonblank;
        }
        elsif ( $types_to_go[$i_lowest] eq ':' ) {
            push @i_colon_breaks, $i_lowest;
        }

        # here we should set breaks for all '?'/':' pairs which are
        # separated by this line 

        $line_count++;

        # save this line segment, after trimming blanks at the ends
        push ( @i_first,
            ( $types_to_go[$i_begin] eq 'b' ) ? $i_begin + 1 : $i_begin );
        push ( @i_last,
            ( $types_to_go[$i_lowest] eq 'b' ) ? $i_lowest - 1 : $i_lowest );

        # set a forced breakpoint at a container opening, if necessary, to 
        # signal a break at a closing container.  Excepting '(' for now.
        if ( $tokens_to_go[$i_lowest] =~ /^[\{\[]$/
            && !$forced_breakpoint_to_go[$i_lowest] )
        {
            set_closing_breakpoint($i_lowest);
        }

        # get ready to go again
        $i_begin                 = $i_lowest + 1;
        $last_break_strength     = $lowest_strength;
        $i_last_break            = $i_lowest;
        $leading_alignment_token = "";
        $leading_alignment_type  = "";
        $lowest_next_token       = '';
        $lowest_next_type        = 'b';

        if ( ( $i_begin <= $imax ) && ( $types_to_go[$i_begin] eq 'b' ) ) {
            $i_begin++;
        }

        # update indentation size
        if ( $i_begin <= $imax ) {
            $leading_spaces = leading_spaces_to_go($i_begin);
        }
    }

    #-------------------------------------------------------
    # ?/: rule 4 -- if we broke at a ':', then break at 
    # corresponding '?' unless this is a chain of ?: expressions
    #-------------------------------------------------------
    if (@i_colon_breaks) {

        # using a simple method for deciding if we are in a ?/: chain --
        # this is a chain if it has multiple ?/: pairs all in order;
        # otherwise not.
        # Note that if line starts in a ':' we count that above as a break
        my $is_chain = ( $colons_in_order && @i_colon_breaks > 1 );

        unless ($is_chain) {
            my @insert_list = ();
            foreach (@i_colon_breaks) {
                my $i_question = $mate_index_to_go[$_];
                if ( $i_question >= 0 ) {
                    if ( $want_break_before{'?'} ) {
                        $i_question--;
                        if (   $i_question > 0
                            && $types_to_go[$i_question] eq 'b' )
                        {
                            $i_question--;
                        }
                    }

                    if ( $i_question >= 0 ) {
                        push @insert_list, $i_question;
                    }
                }
                insert_additional_breaks( \@insert_list, \@i_first, \@i_last );
            }
        }
    }
    return \@i_first, \@i_last;
}

sub insert_additional_breaks {

    # this routine will add line breaks at requested locations after
    # sub set_continuation_breaks has made preliminary breaks.

    my ( $ri_break_list, $ri_first, $ri_last ) = @_;
    my $i_f;
    my $i_l;
    my $line_number = 0;
    my $i_break_left;
    foreach $i_break_left ( sort @$ri_break_list ) {

        $i_f = $$ri_first[$line_number];
        $i_l = $$ri_last[$line_number];
        while ( $i_break_left >= $i_l ) {
            $line_number++;

            # shouldn't happen unless caller passes bad indexes
            if ( $line_number >= @$ri_last ) {
                warning(
"Non-fatal program bug: couldn't set break at $i_break_left\n"
                );
                report_definite_bug();
                return;
            }
            $i_f = $$ri_first[$line_number];
            $i_l = $$ri_last[$line_number];
        }

        my $i_break_right = $i_break_left + 1;
        if ( $types_to_go[$i_break_right] eq 'b' ) { $i_break_right++ }

        if (   $i_break_left >= $i_f
            && $i_break_left < $i_l
            && $i_break_right > $i_f
            && $i_break_right <= $i_l )
        {
            splice( @$ri_first, $line_number, 1, ( $i_f, $i_break_right ) );
            splice( @$ri_last, $line_number, 1, ( $i_break_left, $i_l ) );
        }
    }
}

sub set_closing_breakpoint {

    # set a breakpoint at a matching closing token
    # at present, this is only used to break at a ':' which matches a '?'
    my $i_break = shift;

    if ( $mate_index_to_go[$i_break] >= 0 ) {

        # watch out for break between something like '()' 
        # which can occur under certain error conditions.
        # -- infinte recursion will occur (attrib.t)
        if ( $mate_index_to_go[$i_break] > $i_break + 1 ) {

            # break before } ] and ), but sub set_forced_breakpoint will decide
            # to break before or after a ? and :
            my $inc = ( $tokens_to_go[$i_break] eq '?' ) ? 0 : 1;
            set_forced_breakpoint( $mate_index_to_go[$i_break] - $inc );
        }
    }
    else {
        my $type_sequence = $type_sequence_to_go[$i_break];
        if ($type_sequence) {
            my $closing_token = $matching_token{ $tokens_to_go[$i_break] };
            $postponed_breakpoint{$type_sequence} = 1;
        }
    }
}

# check to see if output line tabbing agrees with input line
# this can be very useful for debugging a script which has an extra
# or missing brace
sub compare_indentation_levels {

    my ( $python_indentation_level, $structural_indentation_level ) = @_;
    if ( ( $python_indentation_level ne $structural_indentation_level ) ) {
        $last_tabbing_disagreement = $input_line_number;

        if ($in_tabbing_disagreement) {
        }
        else {
            $tabbing_disagreement_count++;

            if ( $tabbing_disagreement_count <= MAX_NAG_MESSAGES ) {
                write_logfile_entry(
"Start indentation disagreement: input=$python_indentation_level; output=$structural_indentation_level\n"
                );
            }
            $in_tabbing_disagreement    = $input_line_number;
            $first_tabbing_disagreement = $in_tabbing_disagreement
              unless ($first_tabbing_disagreement);
        }
    }
    else {

        if ($in_tabbing_disagreement) {

            if ( $tabbing_disagreement_count <= MAX_NAG_MESSAGES ) {
                write_logfile_entry(
"End indentation disagreement from input line $in_tabbing_disagreement\n"
                );

                if ( $tabbing_disagreement_count == MAX_NAG_MESSAGES ) {
                    write_logfile_entry(
                        "No further tabbing disagreements will be noted\n");
                }
            }
            $in_tabbing_disagreement = 0;
        }
    }
}

#####################################################################
#
# the Perl::Tidy::IndentationItem class supplies items which contain
# how much whitespace should be used at the start of a line
#
#####################################################################

package Perl::Tidy::IndentationItem;

# Indexes for indentation items
use constant SPACES             => 0;     # total leading white spaces
use constant LEVEL              => 1;     # the indentation 'level'
use constant CI_LEVEL           => 2;     # the 'continuation level'
use constant AVAILABLE_SPACES   => 3;     # how many left spaces available
                                          # for this level
use constant CLOSED             => 4;     # index where we saw closing '}'
use constant COMMA_COUNT        => 5;     # how many commas at this level?
use constant SEQUENCE_NUMBER    => 6;     # output batch number
use constant INDEX              => 7;     # index in output batch list
use constant HAVE_CHILD         => 8;     # any dependents?
use constant RECOVERABLE_SPACES => 9;     # how many spaces to the right
                                          # we would like to move to get
                                          # alignment (negative if left)
use constant ALIGN_PAREN        => 10;    # do we want to try to align
                                          # with an opening structure?
use constant MARKED             => 11;    # if visited by corrector logic
use constant STACK_DEPTH        => 12;    # indentation nesting depth
use constant STARTING_INDEX     => 13;    # first token index of this level
use constant ARROW_COUNT        => 14;    # how many =>'s

sub new {

    # Create an 'indentation_item' which describes one level of leading
    # whitespace when the '-lp' indentation is used.  We return
    # a reference to an anonymous array of associated variables.
    # See above constants for storage scheme.
    my (
        $class,               $spaces,           $level,
        $ci_level,            $available_spaces, $index,
        $gnu_sequence_number, $align_paren,      $stack_depth,
        $starting_index,
      )
      = @_;
    my $closed            = -1;
    my $arrow_count       = 0;
    my $comma_count       = 0;
    my $have_child        = 0;
    my $want_right_spaces = 0;
    my $marked            = 0;
    bless [
        $spaces,              $level,          $ci_level,
        $available_spaces,    $closed,         $comma_count,
        $gnu_sequence_number, $index,          $have_child,
        $want_right_spaces,   $align_paren,    $marked,
        $stack_depth,         $starting_index, $arrow_count,
    ], $class;
}

sub permanently_decrease_AVAILABLE_SPACES {

    # make a permanent reduction in the available indentation spaces
    # at one indentation item.  NOTE: if there are child nodes, their
    # total SPACES must be reduced by the caller.

    my ( $item, $spaces_needed ) = @_;
    my $available_spaces = $item->get_AVAILABLE_SPACES();
    my $deleted_spaces   =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $item->decrease_AVAILABLE_SPACES($deleted_spaces);
    $item->decrease_SPACES($deleted_spaces);
    $item->set_RECOVERABLE_SPACES(0);

    return $deleted_spaces;
}

sub tentatively_decrease_AVAILABLE_SPACES {

    # We are asked to tentatively delete $spaces_needed of indentation
    # for a indentation item.  We may want to undo this later.  NOTE: if
    # there are child nodes, their total SPACES must be reduced by the
    # caller.
    my ( $item, $spaces_needed ) = @_;
    my $available_spaces = $item->get_AVAILABLE_SPACES();
    my $deleted_spaces   =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $item->decrease_AVAILABLE_SPACES($deleted_spaces);
    $item->decrease_SPACES($deleted_spaces);
    $item->increase_RECOVERABLE_SPACES($deleted_spaces);
    return $deleted_spaces;
}

sub get_STACK_DEPTH {
    my $self = shift;
    return $self->[STACK_DEPTH];
}

sub get_SPACES {
    my $self = shift;
    return $self->[SPACES];
}

sub get_MARKED {
    my $self = shift;
    return $self->[MARKED];
}

sub set_MARKED {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[MARKED] = $value;
    }
    return $self->[MARKED];
}

sub get_AVAILABLE_SPACES {
    my $self = shift;
    return $self->[AVAILABLE_SPACES];
}

sub decrease_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[SPACES] -= $value;
    }
    return $self->[SPACES];
}

sub decrease_AVAILABLE_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[AVAILABLE_SPACES] -= $value;
    }
    return $self->[AVAILABLE_SPACES];
}

sub get_ALIGN_PAREN {
    my $self = shift;
    return $self->[ALIGN_PAREN];
}

sub get_RECOVERABLE_SPACES {
    my $self = shift;
    return $self->[RECOVERABLE_SPACES];
}

sub set_RECOVERABLE_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[RECOVERABLE_SPACES] = $value;
    }
    return $self->[RECOVERABLE_SPACES];
}

sub increase_RECOVERABLE_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[RECOVERABLE_SPACES] += $value;
    }
    return $self->[RECOVERABLE_SPACES];
}

sub get_CI_LEVEL {
    my $self = shift;
    return $self->[CI_LEVEL];
}

sub get_LEVEL {
    my $self = shift;
    return $self->[LEVEL];
}

sub get_SEQUENCE_NUMBER {
    my $self = shift;
    return $self->[SEQUENCE_NUMBER];
}

sub get_INDEX {
    my $self = shift;
    return $self->[INDEX];
}

sub get_STARTING_INDEX {
    my $self = shift;
    return $self->[STARTING_INDEX];
}

sub set_HAVE_CHILD {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[HAVE_CHILD] = $value;
    }
    return $self->[HAVE_CHILD];
}

sub get_HAVE_CHILD {
    my $self = shift;
    return $self->[HAVE_CHILD];
}

sub set_ARROW_COUNT {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[ARROW_COUNT] = $value;
    }
    return $self->[ARROW_COUNT];
}

sub get_ARROW_COUNT {
    my $self = shift;
    return $self->[ARROW_COUNT];
}

sub set_COMMA_COUNT {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[COMMA_COUNT] = $value;
    }
    return $self->[COMMA_COUNT];
}

sub get_COMMA_COUNT {
    my $self = shift;
    return $self->[COMMA_COUNT];
}

sub set_CLOSED {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[CLOSED] = $value;
    }
    return $self->[CLOSED];
}

sub get_CLOSED {
    my $self = shift;
    return $self->[CLOSED];
}

#####################################################################
#
# the Perl::Tidy::VerticalAligner::Line class supplies an object to
# contain a single output line
#
#####################################################################

package Perl::Tidy::VerticalAligner::Line;

{

    use strict;
    use Carp;

    use constant JMAX                      => 0;
    use constant JMAX_ORIGINAL_LINE        => 1;
    use constant RTOKENS                   => 2;
    use constant RFIELDS                   => 3;
    use constant RPATTERNS                 => 4;
    use constant INDENTATION               => 5;
    use constant LEADING_SPACE_COUNT       => 6;
    use constant OUTDENT_LONG_LINES        => 7;
    use constant LIST_TYPE                 => 8;
    use constant IS_HANGING_SIDE_COMMENT   => 9;
    use constant RALIGNMENTS               => 10;
    use constant MAXIMUM_LINE_LENGTH       => 11;
    use constant RVERTICAL_TIGHTNESS_FLAGS => 12;

    my %_index_map;
    $_index_map{jmax}                      = JMAX;
    $_index_map{jmax_original_line}        = JMAX_ORIGINAL_LINE;
    $_index_map{rtokens}                   = RTOKENS;
    $_index_map{rfields}                   = RFIELDS;
    $_index_map{rpatterns}                 = RPATTERNS;
    $_index_map{indentation}               = INDENTATION;
    $_index_map{leading_space_count}       = LEADING_SPACE_COUNT;
    $_index_map{outdent_long_lines}        = OUTDENT_LONG_LINES;
    $_index_map{list_type}                 = LIST_TYPE;
    $_index_map{is_hanging_side_comment}   = IS_HANGING_SIDE_COMMENT;
    $_index_map{ralignments}               = RALIGNMENTS;
    $_index_map{maximum_line_length}       = MAXIMUM_LINE_LENGTH;
    $_index_map{rvertical_tightness_flags} = RVERTICAL_TIGHTNESS_FLAGS;

    my @_default_data = ();
    $_default_data[JMAX]                      = undef;
    $_default_data[JMAX_ORIGINAL_LINE]        = undef;
    $_default_data[RTOKENS]                   = undef;
    $_default_data[RFIELDS]                   = undef;
    $_default_data[RPATTERNS]                 = undef;
    $_default_data[INDENTATION]               = undef;
    $_default_data[LEADING_SPACE_COUNT]       = undef;
    $_default_data[OUTDENT_LONG_LINES]        = undef;
    $_default_data[LIST_TYPE]                 = undef;
    $_default_data[IS_HANGING_SIDE_COMMENT]   = undef;
    $_default_data[RALIGNMENTS]               = [];
    $_default_data[MAXIMUM_LINE_LENGTH]       = undef;
    $_default_data[RVERTICAL_TIGHTNESS_FLAGS] = undef;

    {

        # methods to count object population
        my $_count = 0;
        sub get_count        { $_count; }
        sub _increment_count { ++$_count }
        sub _decrement_count { --$_count }
    }

    # Constructor may be called as a class method
    sub new {
        my ( $caller, %arg ) = @_;
        my $caller_is_obj = ref($caller);
        my $class = $caller_is_obj || $caller;
        no strict "refs";
        my $self = bless [], $class;

        $self->[RALIGNMENTS] = [];

        my $index;
        foreach ( keys %_index_map ) {
            $index = $_index_map{$_};
            if ( exists $arg{$_} ) { $self->[$index] = $arg{$_} }
            elsif ($caller_is_obj) { $self->[$index] = $caller->[$index] }
            else { $self->[$index] = $_default_data[$index] }
        }

        $self->_increment_count();
        return $self;
    }

    sub DESTROY {
        $_[0]->_decrement_count();
    }

    sub get_jmax                      { $_[0]->[JMAX] }
    sub get_jmax_original_line        { $_[0]->[JMAX_ORIGINAL_LINE] }
    sub get_rtokens                   { $_[0]->[RTOKENS] }
    sub get_rfields                   { $_[0]->[RFIELDS] }
    sub get_rpatterns                 { $_[0]->[RPATTERNS] }
    sub get_indentation               { $_[0]->[INDENTATION] }
    sub get_leading_space_count       { $_[0]->[LEADING_SPACE_COUNT] }
    sub get_outdent_long_lines        { $_[0]->[OUTDENT_LONG_LINES] }
    sub get_list_type                 { $_[0]->[LIST_TYPE] }
    sub get_is_hanging_side_comment   { $_[0]->[IS_HANGING_SIDE_COMMENT] }
    sub get_rvertical_tightness_flags { $_[0]->[RVERTICAL_TIGHTNESS_FLAGS] }

    sub set_column     { $_[0]->[RALIGNMENTS]->[ $_[1] ]->set_column( $_[2] ) }
    sub get_alignment  { $_[0]->[RALIGNMENTS]->[ $_[1] ] }
    sub get_alignments { @{ $_[0]->[RALIGNMENTS] } }
    sub get_column     { $_[0]->[RALIGNMENTS]->[ $_[1] ]->get_column() }

    sub get_starting_column {
        $_[0]->[RALIGNMENTS]->[ $_[1] ]->get_starting_column();
    }

    sub increment_column {
        $_[0]->[RALIGNMENTS]->[ $_[1] ]->increment_column( $_[2] );
    }
    sub set_alignments { my $self = shift; @{ $self->[RALIGNMENTS] } = @_; }

    sub current_field_width {
        my $self = shift;
        my ($j) = @_;
        if ( $j == 0 ) {
            return $self->get_column($j);
        }
        else {
            return $self->get_column($j) - $self->get_column( $j - 1 );
        }
    }

    sub field_width_growth {
        my $self = shift;
        my $j    = shift;
        return $self->get_column($j) - $self->get_starting_column($j);
    }

    sub starting_field_width {
        my $self = shift;
        my $j    = shift;
        if ( $j == 0 ) {
            return $self->get_starting_column($j);
        }
        else {
            return $self->get_starting_column($j) -
              $self->get_starting_column( $j - 1 );
        }
    }

    sub increase_field_width {

        my $self = shift;
        my ( $j, $pad ) = @_;
        my $jmax = $self->get_jmax();
        for my $k ( $j .. $jmax ) {
            $self->increment_column( $k, $pad );
        }
    }

    sub get_available_space_on_right {
        my $self = shift;
        my $jmax = $self->get_jmax();
        return $self->[MAXIMUM_LINE_LENGTH] - $self->get_column($jmax);
    }

    sub set_jmax                    { $_[0]->[JMAX]                    = $_[1] }
    sub set_jmax_original_line      { $_[0]->[JMAX_ORIGINAL_LINE]      = $_[1] }
    sub set_rtokens                 { $_[0]->[RTOKENS]                 = $_[1] }
    sub set_rfields                 { $_[0]->[RFIELDS]                 = $_[1] }
    sub set_rpatterns               { $_[0]->[RPATTERNS]               = $_[1] }
    sub set_indentation             { $_[0]->[INDENTATION]             = $_[1] }
    sub set_leading_space_count     { $_[0]->[LEADING_SPACE_COUNT]     = $_[1] }
    sub set_outdent_long_lines      { $_[0]->[OUTDENT_LONG_LINES]      = $_[1] }
    sub set_list_type               { $_[0]->[LIST_TYPE]               = $_[1] }
    sub set_is_hanging_side_comment { $_[0]->[IS_HANGING_SIDE_COMMENT] = $_[1] }
    sub set_alignment { $_[0]->[RALIGNMENTS]->[ $_[1] ] = $_[2] }

}

#####################################################################
#
# the Perl::Tidy::VerticalAligner::Alignment class holds information
# on a single column being aligned
#
#####################################################################
package Perl::Tidy::VerticalAligner::Alignment;

{

    use strict;

    #use Carp;

    # Symbolic array indexes
    use constant COLUMN          => 0;    # the current column number
    use constant STARTING_COLUMN => 1;    # column number when created
    use constant MATCHING_TOKEN  => 2;    # what token we are matching
    use constant STARTING_LINE   => 3;    # the line index of creation
    use constant ENDING_LINE     => 4;    # the most recent line to use it
    use constant SAVED_COLUMN    => 5;    # the most recent line to use it
    use constant SERIAL_NUMBER   => 6;    # unique number for this alignment
                                          # (just its index in an array)

    # Correspondence between variables and array indexes
    my %_index_map;
    $_index_map{column}          = COLUMN;
    $_index_map{starting_column} = STARTING_COLUMN;
    $_index_map{matching_token}  = MATCHING_TOKEN;
    $_index_map{starting_line}   = STARTING_LINE;
    $_index_map{ending_line}     = ENDING_LINE;
    $_index_map{saved_column}    = SAVED_COLUMN;
    $_index_map{serial_number}   = SERIAL_NUMBER;

    my @_default_data = ();
    $_default_data[COLUMN]          = undef;
    $_default_data[STARTING_COLUMN] = undef;
    $_default_data[MATCHING_TOKEN]  = undef;
    $_default_data[STARTING_LINE]   = undef;
    $_default_data[ENDING_LINE]     = undef;
    $_default_data[SAVED_COLUMN]    = undef;
    $_default_data[SERIAL_NUMBER]   = undef;

    # class population count
    {
        my $_count = 0;
        sub get_count        { $_count; }
        sub _increment_count { ++$_count }
        sub _decrement_count { --$_count }
    }

    # constructor
    sub new {
        my ( $caller, %arg ) = @_;
        my $caller_is_obj = ref($caller);
        my $class = $caller_is_obj || $caller;
        no strict "refs";
        my $self = bless [], $class;

        foreach ( keys %_index_map ) {
            my $index = $_index_map{$_};
            if ( exists $arg{$_} ) { $self->[$index] = $arg{$_} }
            elsif ($caller_is_obj) { $self->[$index] = $caller->[$index] }
            else { $self->[$index] = $_default_data[$index] }
        }
        $self->_increment_count();
        return $self;
    }

    sub DESTROY {
        $_[0]->_decrement_count();
    }

    sub get_column          { return $_[0]->[COLUMN] }
    sub get_starting_column { return $_[0]->[STARTING_COLUMN] }
    sub get_matching_token  { return $_[0]->[MATCHING_TOKEN] }
    sub get_starting_line   { return $_[0]->[STARTING_LINE] }
    sub get_ending_line     { return $_[0]->[ENDING_LINE] }
    sub get_serial_number   { return $_[0]->[SERIAL_NUMBER] }

    sub set_column          { $_[0]->[COLUMN]          = $_[1] }
    sub set_starting_column { $_[0]->[STARTING_COLUMN] = $_[1] }
    sub set_matching_token  { $_[0]->[MATCHING_TOKEN]  = $_[1] }
    sub set_starting_line   { $_[0]->[STARTING_LINE]   = $_[1] }
    sub set_ending_line     { $_[0]->[ENDING_LINE]     = $_[1] }
    sub increment_column { $_[0]->[COLUMN] += $_[1] }

    sub save_column    { $_[0]->[SAVED_COLUMN] = $_[0]->[COLUMN] }
    sub restore_column { $_[0]->[COLUMN]       = $_[0]->[SAVED_COLUMN] }

}

package Perl::Tidy::VerticalAligner;

# The Perl::Tidy::VerticalAligner package collects output lines and
# attempts to line up certain common tokens, such as => and #, which are
# identified by the calling routine.
# 
# There are two main routines: append_line and flush.  Append acts as a
# storage buffer, collecting lines into a group which can be vertically
# aligned.  When alignment is no longer possible or desirable, it dumps
# the group to flush.
# 
#     append_line -----> flush
# 
#     collects          writes
#     vertical          one
#     groups            group

BEGIN {

    # Caution: these debug flags produce a lot of output
    # They should all be 0 except when debugging small scripts

    use constant VALIGN_DEBUG_FLAG_APPEND  => 0;
    use constant VALIGN_DEBUG_FLAG_APPEND0 => 0;

    my $debug_warning = sub {
        print "VALIGN_DEBUGGING with key $_[0]\n";
    };

    VALIGN_DEBUG_FLAG_APPEND  && $debug_warning->('APPEND');
    VALIGN_DEBUG_FLAG_APPEND0 && $debug_warning->('APPEND0');

}

use vars qw(
  $vertical_aligner_self
  $current_line
  $maximum_alignment_index
  $ralignment_list
  $maximum_jmax_seen
  $minimum_jmax_seen
  $previous_minimum_jmax_seen
  $previous_maximum_jmax_seen
  $maximum_line_index
  $group_level
  $group_maximum_gap
  $last_group_level_written
  $extra_indent_ok
  $zero_count
  @group_lines
  $last_comment_column
  $last_side_comment_line_number
  $last_side_comment_length
  $last_side_comment_level
  $outdented_line_count
  $first_outdented_line_at
  $last_outdented_line_at
  $diagnostics_object
  $logger_object
  $file_writer_object
  @side_comment_history

  $cached_line_text
  $cached_line_type
  $cached_line_flag
  $cached_seqno
  $cached_line_valid

  $rOpts

  $rOpts_maximum_line_length
  $rOpts_continuation_indentation
  $rOpts_indent_columns
  $rOpts_tabs
  $rOpts_entab_leading_whitespace

  $rOpts_minimum_space_to_comment

);

sub initialize {

    my $class;

    ( $class, $rOpts, $file_writer_object, $logger_object, $diagnostics_object )
      = @_;

    # variables describing the entire space group:

    $ralignment_list            = [];
    $group_level                = 0;
    $last_group_level_written   = -1;
    $extra_indent_ok            = 0;    # can we move all lines to the right?
    $last_side_comment_length   = 0;
    $maximum_jmax_seen          = 0;
    $minimum_jmax_seen          = 0;
    $previous_minimum_jmax_seen = 0;
    $previous_maximum_jmax_seen = 0;

    # variables describing each line of the group
    @group_lines = ();                  # list of all lines in group

    $outdented_line_count          = 0;
    $first_outdented_line_at       = 0;
    $last_outdented_line_at        = 0;
    $last_side_comment_line_number = 0;
    $last_side_comment_level       = -1;

    # most recent 3 side comments; [ line number, column ]
    $side_comment_history[0] = [ -300, 0 ];
    $side_comment_history[1] = [ -200, 0 ];
    $side_comment_history[2] = [ -100, 0 ];

    # write_leader_and_string cache:
    $cached_line_text  = "";
    $cached_line_type  = 0;
    $cached_line_flag  = 0;
    $cached_seqno      = 0;
    $cached_line_valid = 0;

    # frequently used parameters
    $rOpts_indent_columns           = $rOpts->{'indent-columns'};
    $rOpts_tabs                     = $rOpts->{'tabs'};
    $rOpts_entab_leading_whitespace = $rOpts->{'entab-leading-whitespace'};
    $rOpts_minimum_space_to_comment = $rOpts->{'minimum-space-to-comment'};
    $rOpts_maximum_line_length      = $rOpts->{'maximum-line-length'};

    forget_side_comment();

    initialize_for_new_group();

    $vertical_aligner_self = {};
    bless $vertical_aligner_self, $class;
    return $vertical_aligner_self;
}

sub initialize_for_new_group {
    $maximum_line_index      = -1;      # lines in the current group
    $maximum_alignment_index = -1;      # alignments in current group
    $zero_count              = 0;       # count consecutive lines without tokens
    $current_line            = undef;   # line being matched for alignment
    $group_maximum_gap       = 0;       # largest gap introduced
}

# interface to Perl::Tidy::Diagnostics routines
sub write_diagnostics {
    if ($diagnostics_object) {
        $diagnostics_object->write_diagnostics(@_);
    }
}

# interface to Perl::Tidy::Logger routines
sub warning {
    if ($logger_object) {
        $logger_object->warning(@_);
    }
}

sub write_logfile_entry {
    if ($logger_object) {
        $logger_object->write_logfile_entry(@_);
    }
}

sub report_definite_bug {
    if ($logger_object) {
        $logger_object->report_definite_bug();
    }
}

sub get_SPACES {

    # return the number of leading spaces associated with an indentation
    # variable $indentation is either a constant number of spaces or an
    # object with a get_SPACES method.
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_SPACES() : $indentation;
}

sub get_RECOVERABLE_SPACES {

    # return the number of spaces (+ means shift right, - means shift left)
    # that we would like to shift a group of lines with the same indentation
    # to get them to line up with their opening parens
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_RECOVERABLE_SPACES() : 0;
}

sub get_STACK_DEPTH {

    my $indentation = shift;
    return ref($indentation) ? $indentation->get_STACK_DEPTH() : 0;
}

sub make_alignment {
    my ( $col, $token ) = @_;

    # make one new alignment at column $col which aligns token $token
    ++$maximum_alignment_index;
    my $alignment = new Perl::Tidy::VerticalAligner::Alignment(
        column          => $col,
        starting_column => $col,
        matching_token  => $token,
        starting_line   => $maximum_line_index,
        ending_line     => $maximum_line_index,
        serial_number   => $maximum_alignment_index,
    );
    $ralignment_list->[$maximum_alignment_index] = $alignment;
    return $alignment;
}

sub dump_alignments {
    print
"Current Alignments:\ni\ttoken\tstarting_column\tcolumn\tstarting_line\tending_line\n";
    for my $i ( 0 .. $maximum_alignment_index ) {
        my $column          = $ralignment_list->[$i]->get_column();
        my $starting_column = $ralignment_list->[$i]->get_starting_column();
        my $matching_token  = $ralignment_list->[$i]->get_matching_token();
        my $starting_line   = $ralignment_list->[$i]->get_starting_line();
        my $ending_line     = $ralignment_list->[$i]->get_ending_line();
        print
"$i\t$matching_token\t$starting_column\t$column\t$starting_line\t$ending_line\n";
    }
}

sub save_alignment_columns {
    for my $i ( 0 .. $maximum_alignment_index ) {
        $ralignment_list->[$i]->save_column();
    }
}

sub restore_alignment_columns {
    for my $i ( 0 .. $maximum_alignment_index ) {
        $ralignment_list->[$i]->restore_column();
    }
}

sub forget_side_comment {
    $last_comment_column = 0;
}

sub append_line {

    # sub append is called to place one line in the current vertical group.
    # 
    # The input parameters are:
    #     $level = indentation level of this line
    #     $rfields = reference to array of fields
    #     $rpatterns = reference to array of patterns, one per field
    #     $rtokens   = reference to array of tokens starting fields 1,2,..
    # 
    # Here is an example of what this package does.  In this example,
    # we are trying to line up both the '=>' and the '#'.  
    # 
    #         '18' => 'grave',    #   \`
    #         '19' => 'acute',    #   `'
    #         '20' => 'caron',    #   \v
    # <-tabs-><f1-><--field 2 ---><-f3->
    # |            |              |    |
    # |            |              |    |
    # col1        col2         col3 col4 
    # 
    # The calling routine has already broken the entire line into 3 fields as
    # indicated.  (So the work of identifying promising common tokens has
    # already been done).
    # 
    # In this example, there will be 2 tokens being matched: '=>' and '#'.
    # They are the leading parts of fields 2 and 3, but we do need to know
    # what they are so that we can dump a group of lines when these tokens
    # change.
    # 
    # The fields contain the actual characters of each field.  The patterns
    # are like the fields, but they contain mainly token types instead
    # of tokens, so they have fewer characters.  They are used to be
    # sure we are matching fields of similar type.
    # 
    # In this example, there will be 4 column indexes being adjusted.  The
    # first one is always at zero.  The interior columns are at the start of
    # the matching tokens, and the last one tracks the maximum line length.
    # 
    # Basically, each time a new line comes in, it joins the current vertical
    # group if possible.  Otherwise it causes the current group to be dumped
    # and a new group is started.
    # 
    # For each new group member, the column locations are increased, as
    # necessary, to make room for the new fields.  When the group is finally
    # output, these column numbers are used to compute the amount of spaces of
    # padding needed for each field.
    # 
    # Programming note: the fields are assumed not to have any tab characters.
    # Tabs have been previously removed except for tabs in quoted strings and
    # side comments.  Tabs in these fields can mess up the column counting.
    # The log file warns the user if there are any such tabs.

    my (
        $level,                     $level_end,
        $indentation,               $rfields,
        $rtokens,                   $rpatterns,
        $is_forced_break,           $outdent_long_lines,
        $is_terminal_statement,     $do_not_pad,
        $rvertical_tightness_flags, $level_jump,
      )
      = @_;

    my $leading_space_count = get_SPACES($indentation);

    # number of fields is $jmax
    # number of tokens between fields is $jmax-1
    my $jmax = $#{$rfields};
    $previous_minimum_jmax_seen = $minimum_jmax_seen;
    $previous_maximum_jmax_seen = $maximum_jmax_seen;

    VALIGN_DEBUG_FLAG_APPEND0 && do {
        print
"APPEND0: entering lines=$maximum_line_index new #fields= $jmax, leading_count=$leading_space_count last_cmt=$last_comment_column force=$is_forced_break\n";
    };

    # Validate cached line if necessary: If we can produce a container
    # with just 2 lines total by combining an existing cached opening
    # token with the closing token to follow, then we will mark both
    # cached flags as valid.
    if ($rvertical_tightness_flags) {
        if (   $maximum_line_index <= 0
            && $cached_line_type
            && $rvertical_tightness_flags->[2] == $cached_seqno )
        {
            $rvertical_tightness_flags->[3] ||= 1;
            $cached_line_valid ||= 1;
        }
    }

    # do not join an opening block brace with an unbalanced line
    # unless requested with a flag value of 2 
    if (   $cached_line_type == 3
        && $maximum_line_index < 0
        && $cached_line_flag < 2
        && $level_jump != 0 )
    {
        $cached_line_valid = 0;
    }

    # patch until new aligner is finished
    if ($do_not_pad) { my_flush() }

    # shouldn't happen:
    if ( $level < 0 ) { $level = 0 }

    # do not align code across indentation level changes
    if ( $level != $group_level ) {

        # we are allowed to shift a group of lines to the right if its 
        # level is greater than the previous and next group
        $extra_indent_ok =
          ( $level < $group_level && $last_group_level_written < $group_level );

        my_flush();

        # If we know that this line will get flushed out by itself because
        # of level changes, we can leave the extra_indent_ok flag set.
        # That way, if we get an external flush call, we will still be
        # able to do some -lp alignment if necessary.
        $extra_indent_ok = ( $is_terminal_statement && $level > $group_level );

        $group_level = $level;

        # wait until after the above flush to get the leading space
        # count because it may have been changed if the -icp flag is in
        # effect
        $leading_space_count = get_SPACES($indentation);

    }

    # --------------------------------------------------------------------
    # Step 1. Handle simple line of code with no fields to match.
    # --------------------------------------------------------------------
    if ( $jmax <= 0 ) {
        $zero_count++;

        if ( $maximum_line_index >= 0
            && !get_RECOVERABLE_SPACES( $group_lines[0]->get_indentation() ) )
        {

            # flush the current group if it has some aligned columns..
            if ( $group_lines[0]->get_jmax() > 1 ) { my_flush() }

            # flush current group if we are just collecting side comments..
            elsif (

                # ...and we haven't seen a comment lately
                ( $zero_count > 3 )

                # ..or if this new line doesn't fit to the left of the comments
                || ( ( $leading_space_count + length( $$rfields[0] ) ) >
                    $group_lines[0]->get_column(0) )
              )
            {
                my_flush();
            }
        }

        # just write this line directly if no current group, no side comment,
        # and no space recovery is needed.
        if ( $maximum_line_index < 0 && !get_RECOVERABLE_SPACES($indentation) )
        {
            write_leader_and_string( $leading_space_count, $$rfields[0], 0,
                $outdent_long_lines, $rvertical_tightness_flags );
            return;
        }
    }
    else {
        $zero_count = 0;
    }

    # programming check: (shouldn't happen)
    # an error here implies an incorrect call was made
    if ( $jmax > 0 && ( $#{$rtokens} != ( $jmax - 1 ) ) ) {
        warning(
"Program bug in Perl::Tidy::VerticalAligner - number of tokens = $#{$rtokens} should be one less than number of fields: $#{$rfields})\n"
        );
        report_definite_bug();
    }

    # --------------------------------------------------------------------
    # create an object to hold this line
    # --------------------------------------------------------------------
    my $is_hanging_side_comment = 0;
    my $new_line                = new Perl::Tidy::VerticalAligner::Line(
        jmax                      => $jmax,
        jmax_original_line        => $jmax,
        rtokens                   => $rtokens,
        rfields                   => $rfields,
        rpatterns                 => $rpatterns,
        indentation               => $indentation,
        leading_space_count       => $leading_space_count,
        outdent_long_lines        => $outdent_long_lines,
        list_type                 => "",
        is_hanging_side_comment   => $is_hanging_side_comment,
        maximum_line_length       => $rOpts->{'maximum-line-length'},
        rvertical_tightness_flags => $rvertical_tightness_flags,
    );

    # --------------------------------------------------------------------
    # It simplifies things to create a zero length side comment
    # if none exists.
    # --------------------------------------------------------------------
    make_side_comment( $new_line, $level_end );

    # --------------------------------------------------------------------
    # Decide if this is a simple list of items.
    # There are 3 list types: none, comma, comma-arrow.
    # We use this below to be less restrictive in deciding what to align.
    # --------------------------------------------------------------------
    if ($is_forced_break) {
        decide_if_list($new_line);
    }

    if ($current_line) {

        # --------------------------------------------------------------------
        # Allow hanging side comment to join current group, if any
        # This will help keep side comments aligned, because otherwise we
        # will have to start a new group, making alignment less likely.
        # --------------------------------------------------------------------
        $is_hanging_side_comment =
          hanging_comment_check( $new_line, $current_line );

        # --------------------------------------------------------------------
        # If there is just one previous line, and it has more fields
        # than the new line, try to join fields together to get a match with
        # the new line.  At the present time, only a single leading '=' is
        # allowed to be compressed out.  This is useful in rare cases where
        # a table is forced to use old breakpoints because of side comments,
        # and the table starts out something like this:
        #   my %MonthChars = ('0', 'Jan',   # side comment
        #                     '1', 'Feb',
        #                     '2', 'Mar',
        # Eliminating the '=' field will allow the remaining fields to line up.
        # This situation does not occur if there are no side comments
        # because scan_list would put a break after the opening '('.
        # --------------------------------------------------------------------
        eliminate_old_fields( $new_line, $current_line );

        # --------------------------------------------------------------------
        # If the new line has more fields than the current group,
        # see if we can match the first fields and combine the remaining
        # fields of the new line.  
        # --------------------------------------------------------------------
        eliminate_new_fields( $new_line, $current_line );

        # --------------------------------------------------------------------
        # Flush previous group unless all common tokens and patterns match..
        # --------------------------------------------------------------------
        check_match( $new_line, $current_line );

        # --------------------------------------------------------------------
        # See if there is space for this line in the current group (if any)
        # --------------------------------------------------------------------
        if ($current_line) {
            check_fit( $new_line, $current_line );
        }
    }

    # --------------------------------------------------------------------
    # Append this line to the current group (or start new group)
    # --------------------------------------------------------------------
    accept_line($new_line);

    # Future update to allow this to vary:
    $current_line = $new_line if ( $maximum_line_index == 0 );

    # --------------------------------------------------------------------
    # Step 8. Some old debugging stuff
    # --------------------------------------------------------------------
    VALIGN_DEBUG_FLAG_APPEND && do {
        print "APPEND fields:";
        dump_array(@$rfields);
        print "APPEND tokens:";
        dump_array(@$rtokens);
        print "APPEND patterns:";
        dump_array(@$rpatterns);
        dump_alignments();
    };
}

sub hanging_comment_check {

    my $line = shift;
    my $jmax = $line->get_jmax();
    return 0 unless $jmax == 1;    # must be 2 fields
    my $rtokens = $line->get_rtokens();
    return 0 unless $$rtokens[0] eq '#';    # the second field is a comment..
    my $rfields = $line->get_rfields();
    return 0 unless $$rfields[0] =~ /^\s*$/;    # the first field is empty...
    my $old_line            = shift;
    my $maximum_field_index = $old_line->get_jmax();
    return 0
      unless $maximum_field_index > $jmax;    # the current line has more fields
    my $rpatterns = $line->get_rpatterns();

    $line->set_is_hanging_side_comment(1);
    $jmax = $maximum_field_index;
    $line->set_jmax($jmax);
    $$rfields[$jmax] = $$rfields[1];
    $$rtokens[ $jmax - 1 ]   = $$rtokens[0];
    $$rpatterns[ $jmax - 1 ] = $$rpatterns[0];
    for ( my $j = 1 ; $j < $jmax ; $j++ ) {
        $$rfields[$j] = " ";    # NOTE: caused glitch unless 1 blank, why?
        $$rtokens[ $j - 1 ]   = "";
        $$rpatterns[ $j - 1 ] = "";
    }
    return 1;
}

sub eliminate_old_fields {

    my $new_line = shift;
    my $jmax     = $new_line->get_jmax();
    if ( $jmax > $maximum_jmax_seen ) { $maximum_jmax_seen = $jmax }
    if ( $jmax < $minimum_jmax_seen ) { $minimum_jmax_seen = $jmax }

    # there must be one previous line
    return unless ( $maximum_line_index == 0 );

    my $old_line            = shift;
    my $maximum_field_index = $old_line->get_jmax();

    # this line must have fewer fields
    return unless $maximum_field_index > $jmax;

    # be reasonable, not too few
    return unless ( $maximum_field_index - 2 <= $jmax );

    # must have side comment
    my $old_rfields = $old_line->get_rfields();
    return unless ( length( $$old_rfields[$maximum_field_index] ) > 0 );

    my $rtokens   = $new_line->get_rtokens();
    my $rfields   = $new_line->get_rfields();
    my $rpatterns = $new_line->get_rpatterns();

    my $old_rtokens   = $old_line->get_rtokens();
    my $old_rpatterns = $old_line->get_rpatterns();

    my $hid_equals = 0;

    my @new_alignments        = ();
    my @new_fields            = ();
    my @new_matching_patterns = ();
    my @new_matching_tokens   = ();

    my $j = 0;
    my $k;
    my $current_field   = '';
    my $current_pattern = '';

    # loop over all old tokens
    my $in_match = 0;
    for ( $k = 0 ; $k < $maximum_field_index ; $k++ ) {
        $current_field .= $$old_rfields[$k];
        $current_pattern .= $$old_rpatterns[$k];
        last if ( $j > $jmax - 1 );

        if ( $$old_rtokens[$k] eq $$rtokens[$j] ) {
            $in_match = 1;
            $new_fields[$j]            = $current_field;
            $new_matching_patterns[$j] = $current_pattern;
            $current_field   = '';
            $current_pattern = '';
            $new_matching_tokens[$j] = $$old_rtokens[$k];
            $new_alignments[$j]      = $old_line->get_alignment($k);
            $j++;
        }
        else {

            if ( $$old_rtokens[$k] =~ /^\=\d*$/ ) {
                $hid_equals = 1;
            }
            last if $in_match;    # disallow gaps in matching field types
        }
    }

    # Modify the current state if we are successful.
    # We must exactly reach the ends of both lists for success.
    if ( ( $j == $jmax ) && ( $current_field eq '' ) && $hid_equals ) {
        $k = $maximum_field_index;
        $current_field .= $$old_rfields[$k];
        $current_pattern .= $$old_rpatterns[$k];
        $new_fields[$j]            = $current_field;
        $new_matching_patterns[$j] = $current_pattern;

        $new_alignments[$j] = $old_line->get_alignment($k);
        $maximum_field_index = $j;

        $old_line->set_alignments(@new_alignments);
        $old_line->set_jmax($jmax);
        $old_line->set_rtokens( \@new_matching_tokens );
        $old_line->set_rfields( \@new_fields );
        $old_line->set_rpatterns( \@$rpatterns );
    }
}

# create an empty side comment if none exists
sub make_side_comment {
    my $new_line  = shift;
    my $level_end = shift;
    my $jmax      = $new_line->get_jmax();
    my $rtokens   = $new_line->get_rtokens();

    # if line does not have a side comment...
    if ( ( $jmax == 0 ) || ( $$rtokens[ $jmax - 1 ] ne '#' ) ) {
        my $rfields   = $new_line->get_rfields();
        my $rpatterns = $new_line->get_rpatterns();
        $$rtokens[$jmax] = '#';
        $$rfields[ ++$jmax ] = '';
        $$rpatterns[$jmax] = '#';
        $new_line->set_jmax($jmax);
        $new_line->set_jmax_original_line($jmax);
    }

    # line has a side comment..
    else {

        # don't remember old side comment location for very long
        my $line_number = $vertical_aligner_self->get_output_line_number();
        my $rfields     = $new_line->get_rfields();
        if (
            $line_number - $last_side_comment_line_number > 12

            # and don't remember comment location across block level changes
            || ( $level_end < $last_side_comment_level && $$rfields[0] =~ /^}/ )
          )
        {
            forget_side_comment();
        }
        $last_side_comment_line_number = $line_number;
        $last_side_comment_level       = $level_end;
    }
}

sub decide_if_list {

    my $line = shift;

    # A list will be taken to be a line with a forced break in which all
    # of the field separators are commas or comma-arrows (except for the
    # trailing #)

    # List separator tokens are things like ',3'   or '=>2',
    # where the trailing digit is the nesting depth.  Allow braces
    # to allow nested list items.
    my $rtokens    = $line->get_rtokens();
    my $test_token = $$rtokens[0];
    if ( $test_token =~ /^(\,|=>)/ ) {
        my $list_type = $test_token;
        my $jmax      = $line->get_jmax();

        foreach ( 1 .. $jmax - 2 ) {
            if ( $$rtokens[$_] !~ /^(\,|=>|\{)/ ) {
                $list_type = "";
                last;
            }
        }
        $line->set_list_type($list_type);
    }
}

sub eliminate_new_fields {

    return unless ( $maximum_line_index >= 0 );
    my $new_line = shift;
    my $jmax     = $new_line->get_jmax();

    # must be monotonic variation
    return unless ( $previous_maximum_jmax_seen <= $jmax );

    # must be more fields in the new line
    my $old_line            = shift;
    my $maximum_field_index = $old_line->get_jmax();
    return unless ( $maximum_field_index < $jmax );

    return
      unless ( $old_line->get_jmax_original_line() == $minimum_jmax_seen )
      ;    # only if monotonic

    # never combine fields of a comma list
    return
      unless ( $maximum_field_index > 1 )
      && ( $new_line->get_list_type() !~ /^,/ );

    my $rtokens       = $new_line->get_rtokens();
    my $rfields       = $new_line->get_rfields();
    my $rpatterns     = $new_line->get_rpatterns();
    my $old_rpatterns = $old_line->get_rpatterns();
    my $old_rtokens   = $old_line->get_rtokens();

    # loop over all old tokens except comment
    my $match = 1;
    my $k;
    for ( $k = 0 ; $k < $maximum_field_index - 1 ; $k++ ) {
        if (   ( $$old_rtokens[$k] ne $$rtokens[$k] )
            || ( $$old_rpatterns[$k] ne $$rpatterns[$k] ) )
        {
            $match = 0;
            last;
        }
    }

    # first tokens agree, so combine new tokens
    if ($match) {
        for $k ( $maximum_field_index .. $jmax - 1 ) {

            $$rfields[ $maximum_field_index - 1 ] .= $$rfields[$k];
            $$rfields[$k] = "";
            $$rpatterns[ $maximum_field_index - 1 ] .= $$rpatterns[$k];
            $$rpatterns[$k] = "";
        }

        $$rtokens[ $maximum_field_index - 1 ] = '#';
        $$rfields[$maximum_field_index]   = $$rfields[$jmax];
        $$rpatterns[$maximum_field_index] = $$rpatterns[$jmax];
        $jmax = $maximum_field_index;
    }
    $new_line->set_jmax($jmax);
}

sub check_match {

    my $new_line = shift;
    my $old_line = shift;

    my $jmax                = $new_line->get_jmax();
    my $maximum_field_index = $old_line->get_jmax();

    # flush if this line has too many fields
    if ( $jmax > $maximum_field_index ) { my_flush(); return }

    # flush if adding this line would make a non-monotonic field count
    if (
        ( $maximum_field_index > $jmax )    # this has too few fields
        && (
            ( $previous_minimum_jmax_seen < $jmax )  # and wouldn't be monotonic
            || ( $old_line->get_jmax_original_line() != $maximum_jmax_seen )
        )
      )
    {
        my_flush();
        return;
    }

    # otherwise append this line if everything matches
    my $jmax_original_line      = $new_line->get_jmax_original_line();
    my $is_hanging_side_comment = $new_line->get_is_hanging_side_comment();
    my $rtokens                 = $new_line->get_rtokens();
    my $rfields                 = $new_line->get_rfields();
    my $rpatterns               = $new_line->get_rpatterns();
    my $list_type               = $new_line->get_list_type();

    my $group_list_type = $old_line->get_list_type();
    my $old_rpatterns   = $old_line->get_rpatterns();
    my $old_rtokens     = $old_line->get_rtokens();

    my $jlimit = $jmax - 1;
    if ( $maximum_field_index > $jmax ) {
        $jlimit = $jmax_original_line;
        --$jlimit unless ( length( $new_line->get_rfields()->[$jmax] ) );
    }

    my $everything_matches = 1;

    # common list types always match
    unless ( ( $group_list_type && ( $list_type eq $group_list_type ) )
        || $is_hanging_side_comment )
    {

        my $leading_space_count = $new_line->get_leading_space_count();
        for my $j ( 0 .. $jlimit ) {
            my $match = 1;
            if (
                ( $j < $jlimit )
                && (   ( $$old_rtokens[$j] ne $$rtokens[$j] )
                    || ( $$old_rpatterns[$j] ne $$rpatterns[$j] ) )
              )
            {
                $match = 0;
            }

            # Don't let line with fewer fields increase column widths
            # ( align3.t )
            if ( $maximum_field_index > $jmax ) {
                my $pad =
                  length( $$rfields[$j] ) - $old_line->current_field_width($j);

                if ( $j == 0 ) {
                    $pad += $leading_space_count;
                }

                # TESTING: suspend this rule to allow last lines to join
                if ( $pad > 0 ) { $match = 0; }
            }

            unless ($match) {
                $everything_matches = 0;
                last;
            }
        }
    }

    if ( $maximum_field_index > $jmax ) {

        if ($everything_matches) {

            my $comment = $$rfields[$jmax];
            for $jmax ( $jlimit .. $maximum_field_index ) {
                $$rtokens[$jmax] = $$old_rtokens[$jmax];
                $$rfields[ ++$jmax ] = '';
                $$rpatterns[$jmax] = $$old_rpatterns[$jmax];
            }
            $$rfields[$jmax] = $comment;
            $new_line->set_jmax($jmax);
        }
    }

    my_flush() unless ($everything_matches);
}

sub check_fit {

    return unless ( $maximum_line_index >= 0 );
    my $new_line = shift;
    my $old_line = shift;

    my $jmax                    = $new_line->get_jmax();
    my $leading_space_count     = $new_line->get_leading_space_count();
    my $is_hanging_side_comment = $new_line->get_is_hanging_side_comment();
    my $rtokens                 = $new_line->get_rtokens();
    my $rfields                 = $new_line->get_rfields();
    my $rpatterns               = $new_line->get_rpatterns();

    my $group_list_type = $group_lines[0]->get_list_type();

    my $padding_so_far    = 0;
    my $padding_available = $old_line->get_available_space_on_right();

    # save current columns in case this doesn't work
    save_alignment_columns();

    my ( $j, $pad, $eight );
    for $j ( 0 .. $jmax ) {

        $pad = length( $$rfields[$j] ) - $old_line->current_field_width($j);

        if ( $j == 0 ) {
            $pad += $leading_space_count;
        }

        # remember largest gap of the group, excluding gap to side comment
        if (   $pad < 0
            && $group_maximum_gap < -$pad
            && $j > 0
            && $j < $jmax - 1 )
        {
            $group_maximum_gap = -$pad;
        }

        next if $pad < 0;

        # This line will need space; lets see if we want to accept it..
        if (

            # not if this won't fit
            ( $pad > $padding_available )

            # previously, there were upper bounds placed on padding here
            # (maximum_whitespace_columns), but they were not really helpful

          )
        {

            # revert to starting state then flush; things didn't work out
            restore_alignment_columns();
            my_flush();
            last;
        }

        # looks ok, squeeze this field in
        $old_line->increase_field_width( $j, $pad );
        $padding_available -= $pad;

        # remember largest gap of the group, excluding gap to side comment
        if ( $pad > $group_maximum_gap && $j > 0 && $j < $jmax - 1 ) {
            $group_maximum_gap = $pad;
        }
    }
}

sub accept_line {

    my $new_line = shift;
    $group_lines[ ++$maximum_line_index ] = $new_line;

    # initialize field lengths if starting new group
    if ( $maximum_line_index == 0 ) {

        my $jmax    = $new_line->get_jmax();
        my $rfields = $new_line->get_rfields();
        my $rtokens = $new_line->get_rtokens();
        my $j;
        my $col = $new_line->get_leading_space_count();

        for $j ( 0 .. $jmax ) {
            $col += length( $$rfields[$j] );

            # create initial alignments for the new group
            my $token = "";
            if ( $j < $jmax ) { $token = $$rtokens[$j] }
            my $alignment = make_alignment( $col, $token );
            $new_line->set_alignment( $j, $alignment );
        }

        $maximum_jmax_seen = $jmax;
        $minimum_jmax_seen = $jmax;
    }

    # use previous alignments otherwise
    else {
        my @new_alignments =
          $group_lines[ $maximum_line_index - 1 ]->get_alignments();
        $new_line->set_alignments(@new_alignments);
    }
}

sub dump_array {

    # debug routine to dump array contents
    local $" = ')(';
    print "(@_)\n";
}

# flush() sends the current Perl::Tidy::VerticalAligner group down the
# pipeline to Perl::Tidy::FileWriter.

# This is the external flush, which also empties the cache
sub flush {

    if ( $maximum_line_index < 0 ) {
        if ($cached_line_type) {
            $file_writer_object->write_code_line( $cached_line_text . "\n" );
            $cached_line_type = 0;
            $cached_line_text = "";
        }
    }
    else {
        my_flush();
    }
}

# This is the internal flush, which leaves the cache intact
sub my_flush {

    return if ( $maximum_line_index < 0 );

    VALIGN_DEBUG_FLAG_APPEND0 && do {
        my $group_list_type = $group_lines[0]->get_list_type();
        my ( $a, $b, $c ) = caller();
        my $maximum_field_index = $group_lines[0]->get_jmax();
        print
"APPEND0: Flush called from $a $b $c fields=$maximum_field_index list=$group_list_type lines=$maximum_line_index extra=$extra_indent_ok\n";

    };

    # some small groups are best left unaligned
    my $do_not_align = decide_if_aligned();

    # optimize side comment location 
    $do_not_align = adjust_side_comment($do_not_align);

    # recover spaces for -lp option if possible
    my $extra_leading_spaces = get_extra_leading_spaces();

    # all lines of this group have the same basic leading spacing
    my $group_leader_length = $group_lines[0]->get_leading_space_count();

    # add extra leading spaces if helpful
    my $min_ci_gap =
      improve_continuation_indentation( $do_not_align, $group_leader_length );

    # loop to output all lines
    for my $i ( 0 .. $maximum_line_index ) {
        my $line = $group_lines[$i];
        write_vertically_aligned_line( $line, $min_ci_gap, $do_not_align,
            $group_leader_length, $extra_leading_spaces );
    }

    initialize_for_new_group();
}

sub decide_if_aligned {

    # Do not try to align two lines which are not really similar
    return unless $maximum_line_index == 1;

    my $group_list_type = $group_lines[0]->get_list_type();

    my $do_not_align = (

        # always align lists
        !$group_list_type

          && (

            # don't align two lines with big gap
            $group_maximum_gap > 12

            # or lines with differing number of alignment tokens
            || $previous_maximum_jmax_seen != $previous_minimum_jmax_seen
          )
    );

    # But try to convert them into a simple comment group if the first line 
    # a has side comment
    my $rfields             = $group_lines[0]->get_rfields();
    my $maximum_field_index = $group_lines[0]->get_jmax();
    if (   $do_not_align
        && ( $maximum_line_index > 0 )
        && ( length( $$rfields[$maximum_field_index] ) > 0 ) )
    {
        combine_fields();
        $do_not_align = 0;
    }
    return $do_not_align;
}

sub adjust_side_comment {

    my $do_not_align = shift;

    # let's see if we can move the side comment field out a little
    # to improve readability (the last field is always a side comment field)
    my $have_side_comment       = 0;
    my $first_side_comment_line = -1;
    my $maximum_field_index     = $group_lines[0]->get_jmax();
    for my $i ( 0 .. $maximum_line_index ) {
        my $line = $group_lines[$i];

        if ( length( $line->get_rfields()->[$maximum_field_index] ) ) {
            $have_side_comment       = 1;
            $first_side_comment_line = $i;
            last;
        }
    }

    my $kmax = $maximum_field_index + 1;

    if ($have_side_comment) {

        my $line = $group_lines[0];

        # the maximum space without exceeding the line length:
        my $avail = $line->get_available_space_on_right();

        # try to use the previous comment column
        my $side_comment_column = $line->get_column( $kmax - 2 );
        my $move                = $last_comment_column - $side_comment_column;

##        my $sc_line0 = $side_comment_history[0]->[0];
##        my $sc_col0  = $side_comment_history[0]->[1];
##        my $sc_line1 = $side_comment_history[1]->[0];
##        my $sc_col1  = $side_comment_history[1]->[1];
##        my $sc_line2 = $side_comment_history[2]->[0];
##        my $sc_col2  = $side_comment_history[2]->[1];
##
##        # FUTURE UPDATES:
##        # Be sure to ignore 'do not align' and  '} # end comments'
##        # Find first $move > 0 and $move <= $avail as follows:
##        # 1. try sc_col1 if sc_col1 == sc_col0 && (line-sc_line0) < 12
##        # 2. try sc_col2 if (line-sc_line2) < 12
##        # 3. try min possible space, plus up to 8, 
##        # 4. try min possible space

        if ( $kmax > 0 && !$do_not_align ) {

            # but if this doesn't work, give up and use the minimum space
            if ( $move > $avail ) {
                $move = $rOpts_minimum_space_to_comment - 1;
            }

            # but we want some minimum space to the comment
            my $min_move = $rOpts_minimum_space_to_comment - 1;
            if (   $move >= 0
                && $last_side_comment_length > 0
                && ( $first_side_comment_line == 0 )
                && $group_level == $last_group_level_written )
            {
                $min_move = 0;
            }

            if ( $move < $min_move ) {
                $move = $min_move;
            }

            # prevously, an upper bound was placed on $move here, 
            # (maximum_space_to_comment), but it was not helpful

            # don't exceed the available space
            if ( $move > $avail ) { $move = $avail }

            # we can only increase space, never decrease
            if ( $move > 0 ) {
                $line->increase_field_width( $maximum_field_index - 1, $move );
            }

            # remember this column for the next group
            $last_comment_column = $line->get_column( $kmax - 2 );
        }
        else {

            # try to at least line up the existing side comment location
            if ( $kmax > 0 && $move > 0 && $move < $avail ) {
                $line->increase_field_width( $maximum_field_index - 1, $move );
                $do_not_align = 0;
            }

            # reset side comment column if we can't align
            else {
                forget_side_comment();
            }
        }
    }
    return $do_not_align;
}

sub improve_continuation_indentation {
    my ( $do_not_align, $group_leader_length ) = @_;

    # See if we can increase the continuation indentation
    # to move all continuation lines closer to the next field
    # (unless it is a comment).
    # 
    # '$min_ci_gap'is the extra indentation that we may need to introduce.
    # We will only introduce this to fields which already have some ci.
    # Without this variable, we would occasionally get something like this
    # (Complex.pm):
    # 
    # use overload '+' => \&plus,
    #   '-'            => \&minus,
    #   '*'            => \&multiply,
    #   ...
    #   'tan'          => \&tan,
    #   'atan2'        => \&atan2,
    # 
    # Whereas with this variable, we can shift variables over to get this:
    # 
    # use overload '+' => \&plus,
    #          '-'     => \&minus,
    #          '*'     => \&multiply,
    #          ...
    #          'tan'   => \&tan,
    #          'atan2' => \&atan2,

    my $maximum_field_index = $group_lines[0]->get_jmax();

    my $min_ci_gap = $rOpts_maximum_line_length;
    if ( $maximum_field_index > 1 && !$do_not_align ) {

        for my $i ( 0 .. $maximum_line_index ) {
            my $line                = $group_lines[$i];
            my $leading_space_count = $line->get_leading_space_count();
            my $rfields             = $line->get_rfields();

            my $gap = $line->get_column(0) - $leading_space_count -
              length( $$rfields[0] );

            if ( $leading_space_count > $group_leader_length ) {
                if ( $gap < $min_ci_gap ) { $min_ci_gap = $gap }
            }
        }

        if ( $min_ci_gap >= $rOpts_maximum_line_length ) {
            $min_ci_gap = 0;
        }
    }
    else {
        $min_ci_gap = 0;
    }
    return $min_ci_gap;
}

sub write_vertically_aligned_line {

    my ( $line, $min_ci_gap, $do_not_align, $group_leader_length,
        $extra_leading_spaces )
      = @_;
    my $rfields                   = $line->get_rfields();
    my $leading_space_count       = $line->get_leading_space_count();
    my $outdent_long_lines        = $line->get_outdent_long_lines();
    my $maximum_field_index       = $line->get_jmax();
    my $rvertical_tightness_flags = $line->get_rvertical_tightness_flags();

    # add any extra spaces
    if ( $leading_space_count > $group_leader_length ) {
        $leading_space_count += $min_ci_gap;
    }

    my $str = $$rfields[0];

    # loop to concatenate all fields of this line and needed padding
    my $total_pad_count = 0;
    my ( $j, $pad );
    for $j ( 1 .. $maximum_field_index ) {

        # skip zero-length side comments
        last
          if ( ( $j == $maximum_field_index )
            && ( !defined( $$rfields[$j] ) || ( length( $$rfields[$j] ) == 0 ) )
          );

        # compute spaces of padding before this field
        my $col = $line->get_column( $j - 1 );
        $pad = $col - ( length($str) + $leading_space_count );

        if ($do_not_align) {
            $pad =
              ( $j < $maximum_field_index )
              ? 0
              : $rOpts_minimum_space_to_comment - 1;
        }

        # accumulate the padding
        if ( $pad > 0 ) { $total_pad_count += $pad; }

        # add this field
        if ( !defined $$rfields[$j] ) {
            write_diagnostics("UNDEFined field at j=$j\n");
        }

        # only add padding when we have a finite field;
        # this avoids extra terminal spaces if we have empty fields
        if ( length( $$rfields[$j] ) > 0 ) {
            $str .= ' ' x $total_pad_count;
            $total_pad_count = 0;
            $str .= $$rfields[$j];
        }

        # update side comment history buffer
        if ( $j == $maximum_field_index ) {
            my $lineno = $file_writer_object->get_output_line_number();
            shift @side_comment_history;
            push @side_comment_history, [ $lineno, $col ];
        }
    }

    my $side_comment_length = ( length( $$rfields[$maximum_field_index] ) );

    # ship this line off
    write_leader_and_string( $leading_space_count + $extra_leading_spaces,
        $str, $side_comment_length, $outdent_long_lines,
        $rvertical_tightness_flags );
}

sub get_extra_leading_spaces {

    #----------------------------------------------------------
    # Define any extra indentation space (for the -lp option).  
    # Here is why:
    # If a list has side comments, sub scan_list must dump the
    # list before it sees everything.  When this happens, it sets
    # the indentation to the standard scheme, but notes how
    # many spaces it would have liked to use.  We may be able
    # to recover that space here in the event that that all of the
    # lines of a list are back together again.
    #----------------------------------------------------------

    my $extra_leading_spaces = 0;
    if ($extra_indent_ok) {
        my $object = $group_lines[0]->get_indentation();
        if ( ref($object) ) {
            my $extra_indentation_spaces_wanted =
              get_RECOVERABLE_SPACES($object);

            # all indentation objects must be the same
            my $i;
            for $i ( 1 .. $maximum_line_index ) {
                if ( $object != $group_lines[$i]->get_indentation() ) {
                    $extra_indentation_spaces_wanted = 0;
                    last;
                }
            }

            if ($extra_indentation_spaces_wanted) {

                # the maximum space without exceeding the line length:
                my $avail = $group_lines[0]->get_available_space_on_right();
                $extra_leading_spaces =
                  ( $avail > $extra_indentation_spaces_wanted )
                  ? $extra_indentation_spaces_wanted
                  : $avail;

                # update the indentation object because with -icp the terminal
                # ');' will use the same adjustment.
                $object->permanently_decrease_AVAILABLE_SPACES(
                    -$extra_leading_spaces );
            }
        }
    }
    return $extra_leading_spaces;
}

sub combine_fields {

    # combine all fields except for the comment field  ( sidecmt.t )
    my ( $j, $k );
    my $maximum_field_index = $group_lines[0]->get_jmax();
    for ( $j = 0 ; $j <= $maximum_line_index ; $j++ ) {
        my $line    = $group_lines[$j];
        my $rfields = $line->get_rfields();
        foreach ( 1 .. $maximum_field_index - 1 ) {
            $$rfields[0] .= $$rfields[$_];
        }
        $$rfields[1] = $$rfields[$maximum_field_index];

        $line->set_jmax(1);
        $line->set_column( 0, 0 );
        $line->set_column( 1, 0 );

    }
    $maximum_field_index = 1;

    for $j ( 0 .. $maximum_line_index ) {
        my $line    = $group_lines[$j];
        my $rfields = $line->get_rfields();
        for $k ( 0 .. $maximum_field_index ) {
            my $pad = length( $$rfields[$k] ) - $line->current_field_width($k);
            if ( $k == 0 ) {
                $pad += $group_lines[$j]->get_leading_space_count();
            }

            if ( $pad > 0 ) { $line->increase_field_width( $k, $pad ) }

        }
    }
}

sub get_output_line_number {

    # the output line number reported to a caller is the number of items 
    # written plus the number of items in the buffer
    my $self = shift;
    1 + $maximum_line_index + $file_writer_object->get_output_line_number();
}

sub write_leader_and_string {

    my ( $leading_space_count, $str, $side_comment_length, $outdent_long_lines,
        $rvertical_tightness_flags )
      = @_;

    my $leading_string = get_leading_string($leading_space_count);

    # handle outdenting of long lines:
    if ($outdent_long_lines) {
        my $excess =
          length($str) - $side_comment_length + $leading_space_count -
          $rOpts_maximum_line_length;
        if ( $excess > 0 ) {
            $leading_string         = "";
            $last_outdented_line_at =
              $file_writer_object->get_output_line_number();

            unless ($outdented_line_count) {
                $first_outdented_line_at = $last_outdented_line_at;
            }
            $outdented_line_count++;
        }
    }

    # Unpack any recombination data; it was packed by
    # sub send_lines_to_vertical_aligner. Contents:
    #
    #   [0] type: 1=opening  2=closing  3=opening block brace
    #   [1] flag: if opening: 1=no multiple steps, 2=multiple steps ok
    #             if closing: spaces of padding to use
    #   [2] sequence number of container 
    #   [3] valid flag: do not append if this flag is false
    #
    my ( $open_or_close, $tightness_flag, $seqno, $valid );
    if ($rvertical_tightness_flags) {
        ( $open_or_close, $tightness_flag, $seqno, $valid ) =
          @{$rvertical_tightness_flags};
    }

    # handle any cached line ..
    # either append this line to it or write it out
    if ($cached_line_text) {

        if ( !$cached_line_valid ) {
            $file_writer_object->write_code_line( $cached_line_text . "\n" );
        }

        # handle cached line with opening container token
        elsif ( $cached_line_type == 1 || $cached_line_type == 3 ) {

            my $gap = $leading_space_count - length($cached_line_text);

            # handle option of just one tight opening per line:
            if ( $cached_line_flag == 1 ) {
                if ( defined($open_or_close) && $open_or_close == 1 ) {
                    $gap = -1;
                }
            }

            if ( $gap >= 0 ) {
                $leading_string = $cached_line_text . ' ' x $gap;
            }
            else {
                $file_writer_object->write_code_line(
                    $cached_line_text . "\n" );
            }
        }

        # handle cached line to place before this closing container token
        else {
            my $test_line = $cached_line_text . ' ' x $cached_line_flag . $str;

            if ( length($test_line) <= $rOpts_maximum_line_length ) {
                $str            = $test_line;
                $leading_string = "";
            }
            else {
                $file_writer_object->write_code_line(
                    $cached_line_text . "\n" );
            }
        }
    }
    $cached_line_type = 0;
    $cached_line_text = "";

    my $line = $leading_string . $str;

    # write or cache this line
    if ( !$rvertical_tightness_flags || $side_comment_length > 0 ) {
        $file_writer_object->write_code_line( $line . "\n" );
    }
    else {
        $cached_line_text  = $line;
        $cached_line_type  = $open_or_close;
        $cached_line_flag  = $tightness_flag;
        $cached_seqno      = $seqno;
        $cached_line_valid = $valid;
    }

    $last_group_level_written = $group_level;
    $last_side_comment_length = $side_comment_length;
    $extra_indent_ok          = 0;
}

{    # begin get_leading_string

    my @leading_string_cache;

    sub get_leading_string {

        # define the leading whitespace string for this line..
        my $leading_whitespace_count = shift;

        # Handle case of zero whitespace, which includes multi-line quotes
        # (which may have a finite level; this prevents tab problems)
        if ( $leading_whitespace_count <= 0 ) {
            return "";
        }

        # look for previous result
        elsif ( $leading_string_cache[$leading_whitespace_count] ) {
            return $leading_string_cache[$leading_whitespace_count];
        }

        # must compute a string for this number of spaces
        my $leading_string;

        # Handle simple case of no tabs
        if ( !( $rOpts_tabs || $rOpts_entab_leading_whitespace )
            || $rOpts_indent_columns <= 0 )
        {
            $leading_string = ( ' ' x $leading_whitespace_count );
        }

        # Handle entab option
        elsif ($rOpts_entab_leading_whitespace) {
            my $space_count =
              $leading_whitespace_count % $rOpts_entab_leading_whitespace;
            my $tab_count =
              int(
                $leading_whitespace_count / $rOpts_entab_leading_whitespace );
            $leading_string = "\t" x $tab_count . ' ' x $space_count;
        }

        # Handle option of one tab per level
        else {
            $leading_string = ( "\t" x $group_level );
            my $space_count =
              $leading_whitespace_count - $group_level * $rOpts_indent_columns;

            # shouldn't happen:
            if ( $space_count < 0 ) {
                warning(
"Error in append_line: for level=$group_level count=$leading_whitespace_count\n"
                );
                $leading_string = ( ' ' x $leading_whitespace_count );
            }
            else {
                $leading_string .= ( ' ' x $space_count );
            }
        }
        $leading_string_cache[$leading_whitespace_count] = $leading_string;
        return $leading_string;
    }
}    # end get_leading_string

sub report_anything_unusual {
    my $self = shift;
    if ( $outdented_line_count > 0 ) {
        write_logfile_entry(
            "$outdented_line_count long lines were outdented:\n");
        write_logfile_entry(
            "  First at output line $first_outdented_line_at\n");

        if ( $outdented_line_count > 1 ) {
            write_logfile_entry(
                "   Last at output line $last_outdented_line_at\n");
        }
        write_logfile_entry(
            "  use -noll to prevent outdenting, -l=n to increase line length\n"
        );
        write_logfile_entry("\n");
    }
}

#####################################################################
#
# the Perl::Tidy::FileWriter class writes the output file
#
#####################################################################

package Perl::Tidy::FileWriter;

# Maximum number of little messages; probably need not be changed.
use constant MAX_NAG_MESSAGES => 6;

sub write_logfile_entry {
    my $self          = shift;
    my $logger_object = $self->{_logger_object};
    if ($logger_object) {
        $logger_object->write_logfile_entry(@_);
    }
}

sub new {
    my $class = shift;
    my ( $line_sink_object, $rOpts, $logger_object ) = @_;

    bless {
        _line_sink_object           => $line_sink_object,
        _logger_object              => $logger_object,
        _rOpts                      => $rOpts,
        _output_line_number         => 1,
        _consecutive_blank_lines    => 0,
        _consecutive_nonblank_lines => 0,
        _first_line_length_error    => 0,
        _max_line_length_error      => 0,
        _last_line_length_error     => 0,
        _first_line_length_error_at => 0,
        _max_line_length_error_at   => 0,
        _last_line_length_error_at  => 0,
        _line_length_error_count    => 0,
        _max_output_line_length     => 0,
        _max_output_line_length_at  => 0,
    }, $class;
}

sub tee_on {
    my $self = shift;
    $self->{_line_sink_object}->tee_on();
}

sub tee_off {
    my $self = shift;
    $self->{_line_sink_object}->tee_off();
}

sub get_output_line_number {
    my $self = shift;
    return $self->{_output_line_number};
}

sub decrement_output_line_number {
    my $self = shift;
    $self->{_output_line_number}--;
}

sub get_consecutive_nonblank_lines {
    my $self = shift;
    return $self->{_consecutive_nonblank_lines};
}

sub reset_consecutive_blank_lines {
    my $self = shift;
    $self->{_consecutive_blank_lines} = 0;
}

sub want_blank_line {
    my $self = shift;
    unless ( $self->{_consecutive_blank_lines} ) {
        $self->write_blank_code_line();
    }
}

sub write_blank_code_line {
    my $self  = shift;
    my $rOpts = $self->{_rOpts};
    return
      if ( $self->{_consecutive_blank_lines} >=
        $rOpts->{'maximum-consecutive-blank-lines'} );
    $self->{_consecutive_blank_lines}++;
    $self->{_consecutive_nonblank_lines} = 0;
    $self->write_line("\n");
}

sub write_code_line {
    my $self = shift;
    my $a    = shift;

    if ( $a =~ /^\s*$/ ) {
        my $rOpts = $self->{_rOpts};
        return
          if ( $self->{_consecutive_blank_lines} >=
            $rOpts->{'maximum-consecutive-blank-lines'} );
        $self->{_consecutive_blank_lines}++;
        $self->{_consecutive_nonblank_lines} = 0;
    }
    else {
        $self->{_consecutive_blank_lines} = 0;
        $self->{_consecutive_nonblank_lines}++;
    }
    $self->write_line($a);
}

sub write_line {
    my $self = shift;
    my $a    = shift;
    $self->{_line_sink_object}->write_line($a);
    if ( $a =~ /\n$/ ) { $self->{_output_line_number}++; }

    # This calculation of excess line length ignores any internal tabs
    my $rOpts  = $self->{_rOpts};
    my $exceed = length($a) - $rOpts->{'maximum-line-length'} - 1;
    if ( $a =~ /^\t+/g ) {
        $exceed += pos($a) * ( $rOpts->{'indent-columns'} - 1 );
    }

    # Note that we just incremented output line number to future value
    # so we must subtract 1 for current line number
    if ( length($a) > 1 + $self->{_max_output_line_length} ) {
        $self->{_max_output_line_length}    = length($a) - 1;
        $self->{_max_output_line_length_at} = $self->{_output_line_number} - 1;
    }

    if ( $exceed > 0 ) {
        my $output_line_number = $self->{_output_line_number};
        $self->{_last_line_length_error}    = $exceed;
        $self->{_last_line_length_error_at} = $output_line_number - 1;
        if ( $self->{_line_length_error_count} == 0 ) {
            $self->{_first_line_length_error}    = $exceed;
            $self->{_first_line_length_error_at} = $output_line_number - 1;
        }

        if (
            $self->{_last_line_length_error} > $self->{_max_line_length_error} )
        {
            $self->{_max_line_length_error}    = $exceed;
            $self->{_max_line_length_error_at} = $output_line_number - 1;
        }

        if ( $self->{_line_length_error_count} < MAX_NAG_MESSAGES ) {
            $self->write_logfile_entry(
                "Line length exceeded by $exceed characters\n");
        }
        $self->{_line_length_error_count}++;
    }

}

sub report_line_length_errors {
    my $self                    = shift;
    my $rOpts                   = $self->{_rOpts};
    my $line_length_error_count = $self->{_line_length_error_count};
    if ( $line_length_error_count == 0 ) {
        $self->write_logfile_entry(
            "No lines exceeded $rOpts->{'maximum-line-length'} characters\n");
        my $max_output_line_length    = $self->{_max_output_line_length};
        my $max_output_line_length_at = $self->{_max_output_line_length_at};
        $self->write_logfile_entry(
"  Maximum output line length was $max_output_line_length at line $max_output_line_length_at\n"
        );

    }
    else {

        my $word = ( $line_length_error_count > 1 ) ? "s" : "";
        $self->write_logfile_entry(
"$line_length_error_count output line$word exceeded $rOpts->{'maximum-line-length'} characters:\n"
        );

        $word = ( $line_length_error_count > 1 ) ? "First" : "";
        my $first_line_length_error    = $self->{_first_line_length_error};
        my $first_line_length_error_at = $self->{_first_line_length_error_at};
        $self->write_logfile_entry(
" $word at line $first_line_length_error_at by $first_line_length_error characters\n"
        );

        if ( $line_length_error_count > 1 ) {
            my $max_line_length_error     = $self->{_max_line_length_error};
            my $max_line_length_error_at  = $self->{_max_line_length_error_at};
            my $last_line_length_error    = $self->{_last_line_length_error};
            my $last_line_length_error_at = $self->{_last_line_length_error_at};
            $self->write_logfile_entry(
" Maximum at line $max_line_length_error_at by $max_line_length_error characters\n"
            );
            $self->write_logfile_entry(
" Last at line $last_line_length_error_at by $last_line_length_error characters\n"
            );
        }
    }
}

#####################################################################
#
# The Perl::Tidy::Debugger class shows line tokenization
#
#####################################################################

package Perl::Tidy::Debugger;

sub new {

    my ( $class, $filename ) = @_;

    bless {
        _debug_file        => $filename,
        _debug_file_opened => 0,
        _fh                => undef,
    }, $class;
}

sub really_open_debug_file {

    my $self       = shift;
    my $debug_file = $self->{_debug_file};
    my $fh;
    unless ( $fh = IO::File->new("> $debug_file") ) {
        warn("can't open $debug_file: $!\n");
    }
    $self->{_debug_file_opened} = 1;
    $self->{_fh}                = $fh;
    print $fh
      "Use -dump-token-types (-dtt) to get a list of token type codes\n";
}

sub close_debug_file {

    my $self = shift;
    my $fh   = $self->{_fh};
    if ( $self->{_debug_file_opened} ) {

        eval { $self->{_fh}->close() };
    }
}

sub write_debug_entry {

    # This is a debug dump routine which may be modified as necessary
    # to dump tokens on a line-by-line basis.  The output will be written
    # to the .DEBUG file when the -D flag is entered.
    my $self           = shift;
    my $line_of_tokens = shift;

    my $input_line        = $line_of_tokens->{_line_text};
    my $rtoken_type       = $line_of_tokens->{_rtoken_type};
    my $rtokens           = $line_of_tokens->{_rtokens};
    my $rlevels           = $line_of_tokens->{_rlevels};
    my $rslevels          = $line_of_tokens->{_rslevels};
    my $rblock_type       = $line_of_tokens->{_rblock_type};
    my $input_line_number = $line_of_tokens->{_line_number};
    my $line_type         = $line_of_tokens->{_line_type};

    my ( $j, $num );

    my $token_str              = "$input_line_number: ";
    my $reconstructed_original = "$input_line_number: ";
    my $block_str              = "$input_line_number: ";

    #$token_str .= "$line_type: ";
    #$reconstructed_original .= "$line_type: ";

    my $pattern   = "";
    my @next_char = ( '"', '"' );
    my $i_next    = 0;
    unless ( $self->{_debug_file_opened} ) { $self->really_open_debug_file() }
    my $fh = $self->{_fh};

    for ( $j = 0 ; $j < @$rtoken_type ; $j++ ) {

        # testing patterns
        if ( $$rtoken_type[$j] eq 'k' ) {
            $pattern .= $$rtokens[$j];
        }
        else {
            $pattern .= $$rtoken_type[$j];
        }
        $reconstructed_original .= $$rtokens[$j];
        $block_str .= "($$rblock_type[$j])";
        $num = length( $$rtokens[$j] );
        my $type_str = $$rtoken_type[$j];

        # be sure there are no blank tokens (shouldn't happen)
        # This can only happen if a programming error has been made
        # because all valid tokens are non-blank
        if ( $type_str eq ' ' ) {
            print $fh "BLANK TOKEN on the next line\n";
            $type_str = $next_char[$i_next];
            $i_next   = 1 - $i_next;
        }

        if ( length($type_str) == 1 ) {
            $type_str = $type_str x $num;
        }
        $token_str .= $type_str;
    }

    # Write what you want here ...
    # print $fh "$input_line\n";
    # print $fh "$pattern\n";
    print $fh "$reconstructed_original\n";
    print $fh "$token_str\n";

    #print $fh "$block_str\n";
}

#####################################################################
#
# The Perl::Tidy::LineBuffer class supplies a 'get_line()'
# method for returning the next line to be parsed, as well as a
# 'peek_ahead()' method
#
# The input parameter is an object with a 'get_line()' method
# which returns the next line to be parsed
#
#####################################################################

package Perl::Tidy::LineBuffer;

sub new {

    my $class              = shift;
    my $line_source_object = shift;

    return bless {
        _line_source_object => $line_source_object,
        _rlookahead_buffer  => [],
    }, $class;
}

sub peek_ahead {
    my $self               = shift;
    my $buffer_index       = shift;
    my $line               = undef;
    my $line_source_object = $self->{_line_source_object};
    my $rlookahead_buffer  = $self->{_rlookahead_buffer};
    if ( $buffer_index < scalar(@$rlookahead_buffer) ) {
        $line = $$rlookahead_buffer[$buffer_index];
    }
    else {
        $line = $line_source_object->get_line();
        push ( @$rlookahead_buffer, $line );
    }
    return $line;
}

sub get_line {
    my $self               = shift;
    my $line               = undef;
    my $line_source_object = $self->{_line_source_object};
    my $rlookahead_buffer  = $self->{_rlookahead_buffer};

    if ( scalar(@$rlookahead_buffer) ) {
        $line = shift @$rlookahead_buffer;
    }
    else {
        $line = $line_source_object->get_line();
    }
    return $line;
}

########################################################################
#
# the Perl::Tidy::Tokenizer package is essentially a filter which
# reads lines of perl source code from a source object and provides
# corresponding tokenized lines through its get_line() method.  Lines
# flow from the source_object to the caller like this:
#
# source_object --> LineBuffer_object --> Tokenizer -->  calling routine
#   get_line()         get_line()           get_line()     line_of_tokens
#
# The source object can be any object with a get_line() method which
# supplies one line (a character string) perl call.
# The LineBuffer object is created by the Tokenizer.
# The Tokenizer returns a reference to a data structure 'line_of_tokens'
# containing one tokenized line for each call to its get_line() method.
#
# WARNING: This is not a real class yet.  Only one tokenizer my be used.
#
########################################################################

package Perl::Tidy::Tokenizer;

BEGIN {

    # Caution: these debug flags produce a lot of output
    # They should all be 0 except when debugging small scripts

    use constant TOKENIZER_DEBUG_FLAG_EXPECT   => 0;
    use constant TOKENIZER_DEBUG_FLAG_NSCAN    => 0;
    use constant TOKENIZER_DEBUG_FLAG_QUOTE    => 0;
    use constant TOKENIZER_DEBUG_FLAG_SCAN_ID  => 0;
    use constant TOKENIZER_DEBUG_FLAG_TOKENIZE => 0;

    my $debug_warning = sub {
        print "TOKENIZER_DEBUGGING with key $_[0]\n";
    };

    TOKENIZER_DEBUG_FLAG_EXPECT   && $debug_warning->('EXPECT');
    TOKENIZER_DEBUG_FLAG_NSCAN    && $debug_warning->('NSCAN');
    TOKENIZER_DEBUG_FLAG_QUOTE    && $debug_warning->('QUOTE');
    TOKENIZER_DEBUG_FLAG_SCAN_ID  && $debug_warning->('SCAN_ID');
    TOKENIZER_DEBUG_FLAG_TOKENIZE && $debug_warning->('TOKENIZE');

}

use Carp;
use vars qw{
  $tokenizer_self
  $level_in_tokenizer
  $slevel_in_tokenizer
  $nesting_token_string
  $nesting_type_string
  $nesting_block_string
  $nesting_block_flag
  $nesting_list_string
  $nesting_list_flag
  $saw_negative_indentation
  $id_scan_state
  $last_nonblank_token
  $last_nonblank_type
  $last_nonblank_block_type
  $last_nonblank_container_type
  $last_nonblank_type_sequence
  $last_last_nonblank_token
  $last_last_nonblank_type
  $last_last_nonblank_block_type
  $last_last_nonblank_container_type
  $last_last_nonblank_type_sequence
  $last_nonblank_prototype
  $statement_type
  $identifier
  $in_quote
  $quote_type
  $quote_character
  $quote_pos
  $quote_depth
  $allowed_quote_modifiers
  $paren_depth
  @paren_type
  @paren_semicolon_count
  @paren_structural_type
  $brace_depth
  @brace_type
  @brace_structural_type
  @brace_context
  @brace_package
  $square_bracket_depth
  @square_bracket_type
  @square_bracket_structural_type
  @depth_array
  @starting_line_of_current_depth
  @current_depth
  @current_sequence_number
  @nesting_sequence_number
  @lower_case_labels_at
  $saw_v_string
  %is_constant
  %is_user_function
  %user_function_prototype
  %saw_function_definition
  $max_token_index
  $peeked_ahead
  $current_package
  $unexpected_error_count
  $input_line
  $input_line_number
  $rpretokens
  $rpretoken_map
  $rpretoken_type
  $want_paren
  $context
  @slevel_stack
  $ci_string_in_tokenizer
  $continuation_string_in_tokenizer
  $in_statement_continuation
  $started_looking_for_here_target_at
  $nearly_matched_here_target_at

  %is_indirect_object_taker
  %is_block_operator
  %expecting_operator_token
  %expecting_operator_types
  %expecting_term_types
  %expecting_term_token
  %is_block_function
  %is_block_list_function
  %is_digraph
  %is_file_test_operator
  %is_trigraph
  %is_valid_token_type
  %is_keyword
  %is_code_block_token
  %really_want_term
  @opening_brace_names
  @closing_brace_names
  %is_keyword_taking_list
};

# possible values of operator_expected()
use constant TERM     => -1;
use constant UNKNOWN  => 0;
use constant OPERATOR => 1;

# possible values of context 
use constant SCALAR_CONTEXT  => -1;
use constant UNKNOWN_CONTEXT => 0;
use constant LIST_CONTEXT    => 1;

# Maximum number of little messages; probably need not be changed.
use constant MAX_NAG_MESSAGES => 6;

{

    # methods to count instances
    my $_count = 0;
    sub get_count        { $_count; }
    sub _increment_count { ++$_count }
    sub _decrement_count { --$_count }
}

sub DESTROY {
    $_[0]->_decrement_count();
}

sub new {

    my $class = shift;

    # Note: 'tabs' and 'indent_columns' are temporary and should be
    # removed asap
    my %defaults = (
        source_object       => undef,
        debugger_object     => undef,
        diagnostics_object  => undef,
        logger_object       => undef,
        starting_level      => undef,
        indent_columns      => 4,
        tabs                => 0,
        look_for_hash_bang  => 0,
        trim_qw             => 1,
        look_for_autoloader => 1,
        look_for_selfloader => 1,
    );
    my %args = ( %defaults, @_ );

    # we are given an object with a get_line() method to supply source lines
    my $source_object = $args{source_object};

    # we create another object with a get_line() and peek_ahead() method
    my $line_buffer_object = Perl::Tidy::LineBuffer->new($source_object);

    # Tokenizer state data is as follows:
    # _rhere_target_list    reference to list of here-doc targets
    # _here_doc_target      the target string for a here document
    # _here_quote_character the type of here-doc quoting (" ' ` or none)
    #                       to determine if interpolation is done
    # _quote_target         character we seek if chasing a quote
    # _line_start_quote     line where we started looking for a long quote
    # _in_here_doc          flag indicating if we are in a here-doc
    # _in_pod               flag set if we are in pod documentation
    # _in_error             flag set if we saw severe error (binary in script)
    # _in_data              flag set if we are in __DATA__ section
    # _in_end               flag set if we are in __END__ section
    # _in_format            flag set if we are in a format description
    # _in_quote             flag telling if we are chasing a quote
    # _starting_level       indentation level of first line
    # _input_tabstr         string denoting one indentation level of input file
    # _know_input_tabstr    flag indicating if we know _input_tabstr
    # _line_buffer_object   object with get_line() method to supply source code
    # _diagnostics_object   place to write debugging information
    $tokenizer_self = {
        _rhere_target_list    => undef,
        _in_here_doc          => 0,
        _here_doc_target      => "",
        _here_quote_character => "",
        _in_data              => 0,
        _in_end               => 0,
        _in_format            => 0,
        _in_error             => 0,
        _in_pod               => 0,
        _in_quote             => 0,
        _quote_target         => "",
        _line_start_quote     => -1,
        _starting_level       => $args{starting_level},
        _know_starting_level  => defined( $args{starting_level} ),
        _tabs                 => $args{tabs},
        _indent_columns       => $args{indent_columns},
        _look_for_hash_bang   => $args{look_for_hash_bang},
        _trim_qw              => $args{trim_qw},
        _input_tabstr         => "",
        _know_input_tabstr    => -1,
        _last_line_number     => 0,
        _saw_perl_dash_P      => 0,
        _saw_perl_dash_w      => 0,
        _saw_use_strict       => 0,
        _look_for_autoloader  => $args{look_for_autoloader},
        _look_for_selfloader  => $args{look_for_selfloader},
        _saw_autoloader       => 0,
        _saw_selfloader       => 0,
        _saw_hash_bang        => 0,
        _saw_end              => 0,
        _saw_data             => 0,
        _saw_lc_filehandle    => 0,
        _started_tokenizing   => 0,
        _line_buffer_object   => $line_buffer_object,
        _debugger_object      => $args{debugger_object},
        _diagnostics_object   => $args{diagnostics_object},
        _logger_object        => $args{logger_object},
    };

    prepare_for_a_new_file();
    find_starting_indentation_level();

    bless $tokenizer_self, $class;

    # This is not a full class yet, so die if an attempt is made to
    # create more than one object.

    if ( _increment_count() > 1 ) {
        confess
"Attempt to create more than 1 object in $class, which is not a true class yet\n";
    }

    return $tokenizer_self;

}

# interface to Perl::Tidy::Logger routines
sub warning {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->warning(@_);
    }
}

sub complain {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->complain(@_);
    }
}

sub write_logfile_entry {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->write_logfile_entry(@_);
    }
}

sub interrupt_logfile {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->interrupt_logfile();
    }
}

sub resume_logfile {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->resume_logfile();
    }
}

sub increment_brace_error {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->increment_brace_error();
    }
}

sub report_definite_bug {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->report_definite_bug();
    }
}

sub brace_warning {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->brace_warning(@_);
    }
}

sub get_saw_brace_error {
    my $logger_object = $tokenizer_self->{_logger_object};
    if ($logger_object) {
        $logger_object->get_saw_brace_error();
    }
    else {
        0;
    }
}

# interface to Perl::Tidy::Diagnostics routines
sub write_diagnostics {
    if ( $tokenizer_self->{_diagnostics_object} ) {
        $tokenizer_self->{_diagnostics_object}->write_diagnostics(@_);
    }
}

sub report_tokenization_errors {

    my $self = shift;

    my $level = get_indentation_level();
    if ( $level != $tokenizer_self->{_starting_level} ) {
        warning("final indentation level: $level\n");
    }

    check_final_nesting_depths();

    if ( $tokenizer_self->{_look_for_hash_bang}
        && !$tokenizer_self->{_saw_hash_bang} )
    {
        warning(
            "hit EOF without seeing hash-bang line; maybe don't need -x?\n");
    }

    if ( $tokenizer_self->{_in_format} ) {
        warning("hit EOF while in format description\n");
    }

    # this check may be removed after a year or so
    if ( $tokenizer_self->{_saw_lc_filehandle} ) {

        warning( <<'EOM' );
------------------------------------------------------------------------
PLEASE NOTE: If you get this message, it is because perltidy noticed
possible ambiguous syntax at one or more places in your script, as
noted above.  The problem is with statements accepting indirect objects,
such as print and printf statements of the form

    print bareword ( $etc

Perltidy needs your help in deciding if 'bareword' is a filehandle or a
function call.  The problem is the space between 'bareword' and '('.  If
'bareword' is a function call, you should remove the trailing space.  If
'bareword' is a filehandle, you should avoid the opening paren or else
globally capitalize 'bareword' to be BAREWORD.  So the above line
would be: 

    print bareword( $etc    # function
or
    print bareword @list    # filehandle
or
    print BAREWORD ( $etc   # filehandle

If you want to keep the line as it is, and are sure it is correct,
you can use -w=0 to prevent this message.
------------------------------------------------------------------------
EOM

    }

    if ( $tokenizer_self->{_in_pod} ) {

        # Just write log entry if this is after __END__ or __DATA__
        # because this happens to often, and it is not likely to be
        # a parsing error.
        if ( $tokenizer_self->{_saw_data} || $tokenizer_self->{_saw_end} ) {
            write_logfile_entry(
"hit eof while in pod documentation (no =cut seen)\n\tthis can cause trouble with some pod utilities\n"
            );
        }

        else {
            complain(
"hit eof while in pod documentation (no =cut seen)\n\tthis can cause trouble with some pod utilities\n"
            );
        }

    }

    if ( $tokenizer_self->{_in_here_doc} ) {
        my $here_doc_target = $tokenizer_self->{_here_doc_target};
        if ($here_doc_target) {
            warning(
"hit EOF in here document starting at line $started_looking_for_here_target_at with target: $here_doc_target\n"
            );
        }
        else {
            warning(
"hit EOF in here document starting at line $started_looking_for_here_target_at with empty target string\n"
            );
        }
        if ($nearly_matched_here_target_at) {
            warning(
"NOTE: almost matched at input line $nearly_matched_here_target_at except for whitespace\n"
            );
        }
    }

    if ( $tokenizer_self->{_in_quote} ) {
        my $line_start_quote = $tokenizer_self->{_line_start_quote};
        my $quote_target     = $tokenizer_self->{_quote_target};
        warning(
"hit EOF seeking end of quote/pattern starting at line $line_start_quote ending in $quote_target\n"
        );
    }

    unless ( $tokenizer_self->{_saw_perl_dash_w} ) {
        if ( $] < 5.006 ) {
            write_logfile_entry("Suggest including '-w parameter'\n");
        }
        else {
            write_logfile_entry("Suggest including 'use warnings;'\n");
        }
    }

    if ( $tokenizer_self->{_saw_perl_dash_P} ) {
        write_logfile_entry("Use of -P parameter for defines is discouraged\n");
    }

    unless ( $tokenizer_self->{_saw_use_strict} ) {
        write_logfile_entry("Suggest including 'use strict;'\n");
    }

    # it is suggested that lables have at least one upper case character
    # for legibility and to avoid code breakage as new keywords are introduced
    if (@lower_case_labels_at) {
        my $num = @lower_case_labels_at;
        write_logfile_entry(
            "Suggest using upper case characters in label(s)\n");
        local $" = ')(';
        write_logfile_entry("  defined at line(s): (@lower_case_labels_at)\n");
    }
}

sub report_v_string {

    # warn if this version can't handle v-strings
    my $tok = shift;
    $saw_v_string = $input_line_number;
    if ( $] < 5.006 ) {
        warning(
"Found v-string '$tok' but v-strings are not implemented in your version of perl; see Camel 3 book ch 2\n"
        );
    }
}

sub get_input_line_number {
    return $tokenizer_self->{_last_line_number};
}

# returns the next tokenized line
sub get_line {

    my $self = shift;

    my $input_line = $tokenizer_self->{_line_buffer_object}->get_line();

    return undef unless ($input_line);

    $tokenizer_self->{_last_line_number}++;

    # remove any control m; otherwise here-target's may not match;
    # trimming trailing white space would work too, but that would
    # change the original line
    $input_line =~ s/(\r|\035)*$//gi;

    my $input_line_number = $tokenizer_self->{_last_line_number};

    # create a data structure describing this line which will be
    # returned to the caller.

    # _line_type codes are: 
    #   SYSTEM         - system-specific code before hash-bang line
    #   CODE           - line of perl code (including comments)
    #   POD_START      - line starting pod, such as '=head'
    #   POD            - pod documentation text 
    #   POD_END        - last line of pod section, '=cut'
    #   HERE           - text of here-document 
    #   HERE_END       - last line of here-doc (target word)
    #   FORMAT         - format section
    #   FORMAT_END     - last line of format section, '.'
    #   DATA_START     - __DATA__ line
    #   DATA           - unidentified text following __DATA__ 
    #   END_START      - __END__ line
    #   END            - unidentified text following __END__ 
    #   ERROR          - we are in big trouble, probably not a perl script

    # Other variables:
    #   _curly_brace_depth     - depth of curly braces at start of line
    #   _square_bracket_depth  - depth of square brackets at start of line
    #   _paren_depth           - depth of parens at start of line
    #   _starting_in_quote     - this line continues a multi-line quote
    #                            (so don't trim leading blanks!)
    #   _ending_in_quote       - this line ends in a multi-line quote
    #                            (so don't trim trailing blanks!)

    my $line_of_tokens = {
        _line_type                => 'EOF',
        _line_text                => $input_line,
        _line_number              => $input_line_number,
        _rtoken_type              => undef,
        _rtokens                  => undef,
        _rlevels                  => undef,
        _rslevels                 => undef,
        _rblock_type              => undef,
        _rcontainer_type          => undef,
        _rcontainer_environment   => undef,
        _rtype_sequence           => undef,
        _rnesting_tokens          => undef,
        _rci_levels               => undef,
        _rnesting_blocks          => undef,
        _python_indentation_level => -1,                   ## 0,
        _starting_in_quote        =>
          ( $tokenizer_self->{_in_quote} && ( $quote_type eq 'Q' ) ),
        _ending_in_quote      => 0,
        _curly_brace_depth    => $brace_depth,
        _square_bracket_depth => $square_bracket_depth,
        _paren_depth          => $paren_depth,
        _quote_character      => '',
    };

    # must print line unchanged if we are in a here document
    if ( $tokenizer_self->{_in_here_doc} ) {

        $line_of_tokens->{_line_type} = 'HERE';
        my $here_doc_target      = $tokenizer_self->{_here_doc_target};
        my $here_quote_character = $tokenizer_self->{_here_quote_character};
        my $candidate_target     = $input_line;
        chomp $candidate_target;
        if ( $candidate_target eq $here_doc_target ) {
            $nearly_matched_here_target_at = undef;
            $line_of_tokens->{_line_type} = 'HERE_END';
            write_logfile_entry("Exiting HERE document $here_doc_target\n");

            my $rhere_target_list = $tokenizer_self->{_rhere_target_list};
            if (@$rhere_target_list) {    # there can be multiple here targets
                ( $here_doc_target, $here_quote_character ) =
                  @{ shift @$rhere_target_list };
                $tokenizer_self->{_here_doc_target}      = $here_doc_target;
                $tokenizer_self->{_here_quote_character} =
                  $here_quote_character;
                write_logfile_entry(
                    "Entering HERE document $here_doc_target\n");
                $nearly_matched_here_target_at      = undef;
                $started_looking_for_here_target_at = $input_line_number;
            }
            else {
                $tokenizer_self->{_in_here_doc}          = 0;
                $tokenizer_self->{_here_doc_target}      = "";
                $tokenizer_self->{_here_quote_character} = "";
            }
        }

        # check for error of extra whitespace
        else {
            $candidate_target =~ s/\s*$//;
            $candidate_target =~ s/^\s*//;
            if ( $candidate_target eq $here_doc_target ) {
                $nearly_matched_here_target_at = $input_line_number;
            }
        }
        return $line_of_tokens;
    }

    # must print line unchanged if we are in a format section
    elsif ( $tokenizer_self->{_in_format} ) {

        if ( $input_line =~ /^\.[\s#]*$/ ) {
            write_logfile_entry("Exiting format section\n");
            $tokenizer_self->{_in_format} = 0;
            $line_of_tokens->{_line_type} = 'FORMAT_END';
        }
        else {
            $line_of_tokens->{_line_type} = 'FORMAT';
        }
        return $line_of_tokens;
    }

    # must print line unchanged if we are in pod documentation
    elsif ( $tokenizer_self->{_in_pod} ) {

        $line_of_tokens->{_line_type} = 'POD';
        if ( $input_line =~ /^=cut/ ) {
            $line_of_tokens->{_line_type} = 'POD_END';
            write_logfile_entry("Exiting POD section\n");
            $tokenizer_self->{_in_pod} = 0;
        }
        if ( $input_line =~ /^\#\!.*perl\b/ ) {
            warning("Hash-bang in pod can cause perl to fail! \n");
        }

        return $line_of_tokens;
    }

    # must print line unchanged if we have seen a severe error (i.e., we
    # are seeing illegal tokens and connot continue.  Syntax errors do
    # not pass this route).  Calling routine can decide what to do, but
    # the default can be to just pass all lines as if they were after __END__ 
    elsif ( $tokenizer_self->{_in_error} ) {
        $line_of_tokens->{_line_type} = 'ERROR';
        return $line_of_tokens;
    }

    # print line unchanged if we are __DATA__ section
    elsif ( $tokenizer_self->{_in_data} ) {

        # ...but look for POD 
        # Note that the _in_data and _in_end flags remain set 
        # so that we return to that state after seeing the 
        # end of a pod section
        if ( $input_line =~ /^=(?!cut)/ ) {
            $line_of_tokens->{_line_type} = 'POD_START';
            write_logfile_entry("Entering POD section\n");
            $tokenizer_self->{_in_pod} = 1;
            return $line_of_tokens;
        }
        else {
            $line_of_tokens->{_line_type} = 'DATA';
            return $line_of_tokens;
        }
    }

    # print line unchanged if we are in __END__ section
    elsif ( $tokenizer_self->{_in_end} ) {

        # ...but look for POD 
        # Note that the _in_data and _in_end flags remain set 
        # so that we return to that state after seeing the 
        # end of a pod section
        if ( $input_line =~ /^=(?!cut)/ ) {
            $line_of_tokens->{_line_type} = 'POD_START';
            write_logfile_entry("Entering POD section\n");
            $tokenizer_self->{_in_pod} = 1;
            return $line_of_tokens;
        }
        else {
            $line_of_tokens->{_line_type} = 'END';
            return $line_of_tokens;
        }
    }

    # check for a hash-bang line if we haven't seen one
    if ( !$tokenizer_self->{_saw_hash_bang} ) {
        if ( $input_line =~ /^\#\!.*perl\b/ ) {
            $tokenizer_self->{_saw_hash_bang} = $input_line_number;

            # check for -w and -P flags
            if ( $input_line =~ /^\#\!.*perl\s.*-.*P/ ) {
                $tokenizer_self->{_saw_perl_dash_P} = 1;
            }

            if ( $input_line =~ /^\#\!.*perl\s.*-.*w/ ) {
                $tokenizer_self->{_saw_perl_dash_w} = 1;
            }

            if (   ( $input_line_number > 1 )
                && ( !$tokenizer_self->{_look_for_hash_bang} ) )
            {

                # this is helpful for VMS systems; we may have accidentally
                # tokenized some DCL commands
                if ( $tokenizer_self->{_started_tokenizing} ) {
                    warning(
"There seems to be a hash-bang after line 1; do you need to run with -x ?\n"
                    );
                }
                else {
                    complain("Useless hash-bang after line 1\n");
                }
            }

            # Report the leading hash-bang as a system line
            # This will prevent -dac from deleting it
            else {
                $line_of_tokens->{_line_type} = 'SYSTEM';
                return $line_of_tokens;
            }
        }
    }

    # wait for a hash-bang before parsing if the user invoked us with -x
    if ( $tokenizer_self->{_look_for_hash_bang}
        && !$tokenizer_self->{_saw_hash_bang} )
    {
        $line_of_tokens->{_line_type} = 'SYSTEM';
        return $line_of_tokens;
    }

    # a first line of the form ': #' will be marked as SYSTEM 
    # since lines of this form may be used by tcsh
    if ( $input_line_number == 1 && $input_line =~ /^\s*\:\s*\#/ ) {
        $line_of_tokens->{_line_type} = 'SYSTEM';
        return $line_of_tokens;
    }

    # now we know that it is ok to tokenize the line...
    # the line tokenizer will modify any of these private variables:
    #        _rhere_target_list
    #        _in_data 
    #        _in_end
    #        _in_format
    #        _in_error
    #        _in_pod 
    #        _in_quote
    my $ending_in_quote_last = $tokenizer_self->{_in_quote};
    tokenize_this_line($line_of_tokens);

    # Now finish defining the return structure and return it
    $line_of_tokens->{_ending_in_quote} = $tokenizer_self->{_in_quote};

    # handle severe error (binary data in script)
    if ( $tokenizer_self->{_in_error} ) {
        $tokenizer_self->{_in_quote} = 0;    # to avoid any more messages
        warning("Giving up after error\n");
        $line_of_tokens->{_line_type} = 'ERROR';
        reset_indentation_level(0);          # avoid error messages
        return $line_of_tokens;
    }

    # handle start of pod documentation
    if ( $tokenizer_self->{_in_pod} ) {

        # This gets tricky..above a __DATA__ or __END__ section, perl
        # accepts '=cut' as the start of pod section. But afterwards,
        # only pod utilities see it and they may ignore an =cut without
        # leading =head.  In any case, this isn't good.
        if ( $input_line =~ /^=cut\b/ ) {
            if ( $tokenizer_self->{_saw_data} || $tokenizer_self->{_saw_end} ) {
                complain("=cut while not in pod ignored\n");
                $tokenizer_self->{_in_pod}    = 0;
                $line_of_tokens->{_line_type} = 'POD_STOP';
            }
            else {
                $line_of_tokens->{_line_type} = 'POD_END';
                complain(
"=cut starts a pod section .. this can fool pod utilities.\n"
                );
                write_logfile_entry("Entering POD section\n");
            }
        }

        else {
            $line_of_tokens->{_line_type} = 'POD_START';
            write_logfile_entry("Entering POD section\n");
        }

        return $line_of_tokens;
    }

    # update indentation levels for log messages
    if ( $input_line !~ /^\s*$/ ) {
        my $rlevels                      = $line_of_tokens->{_rlevels};
        my $structural_indentation_level = $$rlevels[0];
        my ( $python_indentation_level, $msg ) =
          find_indentation_level( $input_line, $structural_indentation_level );
        if ($msg) { write_logfile_entry("$msg") }
        if ( $tokenizer_self->{_know_input_tabstr} == 1 ) {
            $line_of_tokens->{_python_indentation_level} =
              $python_indentation_level;
        }
    }

    # see if this line contains here doc targets
    my $rhere_target_list = $tokenizer_self->{_rhere_target_list};
    if (@$rhere_target_list) {

        #my $here_doc_target = shift @$rhere_target_list;
        my ( $here_doc_target, $here_quote_character ) =
          @{ shift @$rhere_target_list };
        $tokenizer_self->{_in_here_doc}          = 1;
        $tokenizer_self->{_here_doc_target}      = $here_doc_target;
        $tokenizer_self->{_here_quote_character} = $here_quote_character;
        write_logfile_entry("Entering HERE document $here_doc_target\n");
        $started_looking_for_here_target_at = $input_line_number;
    }

    # NOTE: __END__ and __DATA__ statements are written unformatted
    # because they can theoretically contain additional characters
    # which are not tokenized (and cannot be read with <DATA> either!).
    if ( $tokenizer_self->{_in_data} ) {
        $line_of_tokens->{_line_type} = 'DATA_START';
        write_logfile_entry("Starting __DATA__ section\n");
        $tokenizer_self->{_saw_data} = 1;

        # keep parsing after __DATA__ if use SelfLoader was seen
        if ( $tokenizer_self->{_saw_selfloader} ) {
            $tokenizer_self->{_in_data} = 0;
            write_logfile_entry(
                "SelfLoader seen, continuing; -nlsl deactivates\n");
        }

        return $line_of_tokens;
    }

    elsif ( $tokenizer_self->{_in_end} ) {
        $line_of_tokens->{_line_type} = 'END_START';
        write_logfile_entry("Starting __END__ section\n");
        $tokenizer_self->{_saw_end} = 1;

        # keep parsing after __END__ if use AutoLoader was seen
        if ( $tokenizer_self->{_saw_autoloader} ) {
            $tokenizer_self->{_in_end} = 0;
            write_logfile_entry(
                "AutoLoader seen, continuing; -nlal deactivates\n");
        }
        return $line_of_tokens;
    }

    # now, finally, we know that this line is type 'CODE'
    $line_of_tokens->{_line_type} = 'CODE';

    # remember if we have seen any real code
    if (   !$tokenizer_self->{_started_tokenizing}
        && $input_line !~ /^\s*$/
        && $input_line !~ /^\s*#/ )
    {
        $tokenizer_self->{_started_tokenizing} = 1;
    }

    if ( $tokenizer_self->{_debugger_object} ) {
        $tokenizer_self->{_debugger_object}->write_debug_entry($line_of_tokens);
    }

    # Note: if keyword 'format' occurs in this line code, it is still CODE
    # (keyword 'format' need not start a line)
    if ( $tokenizer_self->{_in_format} ) {
        write_logfile_entry("Entering format section\n");
    }

    if ( $tokenizer_self->{_in_quote}
        and ( $tokenizer_self->{_line_start_quote} < 0 ) )
    {

        if ( ( my $quote_target = get_quote_target() ) !~ /^\s*$/ ) {
            $tokenizer_self->{_line_start_quote} = $input_line_number;
            $tokenizer_self->{_quote_target}     = $quote_target;
            write_logfile_entry(
                "Start multi-line quote or pattern ending in $quote_target\n");
        }
    }
    elsif ( ( $tokenizer_self->{_line_start_quote} >= 0 )
        and !$tokenizer_self->{_in_quote} )
    {
        $tokenizer_self->{_line_start_quote} = -1;
        write_logfile_entry("End of multi-line quote or pattern\n");
    }

    # we are returning a line of CODE
    return $line_of_tokens;
}

sub find_starting_indentation_level {

    my $starting_level    = 0;
    my $know_input_tabstr = -1;    # flag for find_indentation_level

    # use value if given as parameter
    if ( $tokenizer_self->{_know_starting_level} ) {
        $starting_level = $tokenizer_self->{_starting_level};
    }

    # if we know there is a hash_bang line, the level must be zero
    elsif ( $tokenizer_self->{_look_for_hash_bang} ) {
        $tokenizer_self->{_know_starting_level} = 1;
    }

    # otherwise figure it out from the input file
    else {
        my $line;
        my $i                            = 0;
        my $structural_indentation_level = -1; # flag for find_indentation_level

        my $msg = "";
        while ( $line =
            $tokenizer_self->{_line_buffer_object}->peek_ahead( $i++ ) )
        {

            # if first line is #! then assume starting level is zero
            if ( $i == 1 && $line =~ /^\#\!/ ) {
                $starting_level = 0;
                last;
            }
            next if ( $line =~ /^\s*#/ );      # must not be comment
            next if ( $line =~ /^\s*$/ );      # must not be blank
            ( $starting_level, $msg ) =
              find_indentation_level( $line, $structural_indentation_level );
            if ($msg) { write_logfile_entry("$msg") }
            last;
        }
        $msg = "Line $i implies starting-indentation-level = $starting_level\n";

        if ( $starting_level > 0 ) {

            my $input_tabstr = $tokenizer_self->{_input_tabstr};
            if ( $input_tabstr eq "\t" ) {
                $msg .= "by guessing input tabbing uses 1 tab per level\n";
            }
            else {
                my $cols = length($input_tabstr);
                $msg .=
                  "by guessing input tabbing uses $cols blanks per level\n";
            }
        }
        write_logfile_entry("$msg");
    }
    $tokenizer_self->{_starting_level} = $starting_level;
    reset_indentation_level($starting_level);
}

# Find indentation level given a input line.  At the same time, try to
# figure out the input tabbing scheme.
# 
# There are two types of calls:
# 
# Type 1: $structural_indentation_level < 0 
#  In this case we have to guess $input_tabstr to figure out the level.
# 
# Type 2: $structural_indentation_level >= 0
#  In this case the level of this line is known, and this routine can
#  update the tabbing string, if still unknown, to make the level correct.

sub find_indentation_level {
    my ( $line, $structural_indentation_level ) = @_;
    my $level = 0;
    my $msg   = "";

    my $know_input_tabstr = $tokenizer_self->{_know_input_tabstr};
    my $input_tabstr      = $tokenizer_self->{_input_tabstr};

    # find leading whitespace
    my $leading_whitespace = ( $line =~ /^(\s*)/ ) ? $1 : "";

    # make first guess at input tabbing scheme if necessary
    if ( $know_input_tabstr < 0 ) {

        $know_input_tabstr = 0;

        if ( $tokenizer_self->{_tabs} ) {
            $input_tabstr = "\t";
            if ( length($leading_whitespace) > 0 ) {
                if ( $leading_whitespace !~ /\t/ ) {

                    my $cols = $tokenizer_self->{_indent_columns};

                    if ( length($leading_whitespace) < $cols ) {
                        $cols = length($leading_whitespace);
                    }
                    $input_tabstr = " " x $cols;
                }
            }
        }
        else {
            $input_tabstr = " " x $tokenizer_self->{_indent_columns};

            if ( length($leading_whitespace) > 0 ) {
                if ( $leading_whitespace =~ /^\t/ ) {
                    $input_tabstr = "\t";
                }
            }
        }
        $tokenizer_self->{_know_input_tabstr} = $know_input_tabstr;
        $tokenizer_self->{_input_tabstr}      = $input_tabstr;
    }

    # determine the input tabbing scheme if possible
    if (   ( $know_input_tabstr == 0 )
        && ( length($leading_whitespace) > 0 )
        && ( $structural_indentation_level > 0 ) )
    {
        my $saved_input_tabstr = $input_tabstr;

        # check for common case of one tab per indentation level
        if ( $leading_whitespace eq "\t" x $structural_indentation_level ) {
            if ( $leading_whitespace eq "\t" x $structural_indentation_level ) {
                $input_tabstr = "\t";
                $msg          = "Guessing old indentation was tab character\n";
            }
        }

        else {

            # detab any tabs based on 8 blanks per tab
            my $entabbed = "";
            if ( $leading_whitespace =~ s/^\t+/        /g ) {
                $entabbed = "entabbed";
            }

            # now compute tabbing from number of spaces
            my $columns =
              length($leading_whitespace) / $structural_indentation_level;
            if ( $columns == int $columns ) {
                $msg =
                  "Guessing old indentation was $columns $entabbed spaces\n";
            }
            else {
                $columns = int $columns;
                $msg     =
"old indentation is unclear, using $columns $entabbed spaces\n";
            }
            $input_tabstr = " " x $columns;
        }
        $know_input_tabstr = 1;
        $tokenizer_self->{_know_input_tabstr} = $know_input_tabstr;
        $tokenizer_self->{_input_tabstr}      = $input_tabstr;

        # see if mistakes were made
        if ( ( $tokenizer_self->{_starting_level} > 0 )
            && !$tokenizer_self->{_know_starting_level} )
        {

            if ( $input_tabstr ne $saved_input_tabstr ) {
                complain(
"I made a bad starting level guess; rerun with a value for -sil \n"
                );
            }
        }
    }

    # use current guess at input tabbing to get input indentation level
    #
    # Patch to handle a common case of entabbed leading whitespace
    # If the leading whitespace equals 4 spaces and we also have
    # tabs, detab the input whitespace assuming 8 spaces per tab.  
    if ( length($input_tabstr) == 4 ) {
        $leading_whitespace =~ s/^\t+/        /g;
    }

    if ( ( my $len_tab = length($input_tabstr) ) > 0 ) {
        my $pos = 0;

        while ( substr( $leading_whitespace, $pos, $len_tab ) eq $input_tabstr )
        {
            $pos += $len_tab;
            $level++;
        }
    }
    return ( $level, $msg );
}

sub dump_token_types {
    my $class = shift;
    my $fh    = shift;

    # This should be the latest list of token types in use
    # adding NEW_TOKENS: add a comment here
    print $fh <<'END_OF_LIST';

Here is a list of the token types currently used.  
For the following tokens, the "type" of a token is just the token itself.  

.. :: << >> ** && .. ||  -> => += -= .= %= &= |= ^= *= <>
( ) <= >= == =~ !~ != ++ -- /= x=
... **= <<= >>= &&= ||= <=> 
, + - / * | % ! x ~ = \ ? : . < > ^ &

The following additional token types are defined:

 type    meaning
    b    blank (white space) 
    {    indent: opening structural curly brace or square bracket or paren
         (code block, anonymous hash reference, or anonymous array reference)
    }    outdent: right structural curly brace or square bracket or paren
    [    left non-structural square bracket (enclosing an array index)
    ]    right non-structural square bracket
    (    left non-structural paren (all but a list right of an =)
    )    right non-structural parena
    L    left non-structural curly brace (enclosing a key)
    R    right non-structural curly brace 
    ;    terminal semicolon
    f    indicates a semicolon in a "for" statement
    h    here_doc operator <<
    #    a comment
    Q    indicates a quote or pattern
    q    indicates a qw quote block
    k    a perl keyword
    C    user-defined constant or constant function (with void prototype = ())
    U    user-defined function taking parameters
    G    user-defined function taking block parameter (like grep/map/eval)
    M    (unused, but reserved for subroutine definition name)
    P    (unused, but -html uses it to label pod text)
    t    type indicater such as %,$,@,*,&,sub
    w    bare word (perhaps a subroutine call)
    i    identifier of some type (with leading %, $, @, *, &, sub )
    n    a number
    v    a v-string
    F    a file test operator (like -e)
    Y    File handle
    Z    identifier in indirect object slot: may be file handle, object
    J    LABEL:  code block label
    j    LABEL after next, last, redo, goto
    p    unary +
    m    unary -
    pp   pre-increment operator ++
    mm   pre-decrement operator -- 
END_OF_LIST
}

# This is a currently unused debug routine
sub dump_functions {

    my $fh = *STDOUT;
    my ( $pkg, $sub );
    foreach $pkg ( keys %is_user_function ) {
        print $fh "\nnon-constant subs in package $pkg\n";

        foreach $sub ( keys %{ $is_user_function{$pkg} } ) {
            my $msg = "";
            if ( $is_block_list_function{$pkg}{$sub} ) {
                $msg = 'block_list';
            }

            if ( $is_block_function{$pkg}{$sub} ) {
                $msg = 'block';
            }
            print $fh "$sub $msg\n";
        }
    }

    foreach $pkg ( keys %is_constant ) {
        print $fh "\nconstants and constant subs in package $pkg\n";

        foreach $sub ( keys %{ $is_constant{$pkg} } ) {
            print $fh "$sub\n";
        }
    }
}

sub prepare_for_a_new_file {
    $saw_negative_indentation = 0;
    $id_scan_state            = '';
    $statement_type           = '';     # currently either '' or 'use'
    $last_nonblank_token      = ';';    # the only possible starting state which
    $last_nonblank_type       = ';';    # will make a leading brace a code block
    $last_nonblank_block_type = '';
    $last_nonblank_container_type      = '';
    $last_nonblank_type_sequence       = '';
    $last_last_nonblank_token          = ';';
    $last_last_nonblank_type           = ';';
    $last_last_nonblank_block_type     = '';
    $last_last_nonblank_container_type = '';
    $last_last_nonblank_type_sequence  = '';
    $last_nonblank_prototype           = "";
    $identifier                        = '';
    $in_quote   = 0;     # flag telling if we are chasing a quote, and what kind
    $quote_type = 'Q';
    $quote_character = "";    # character we seek if chasing a quote
    $quote_pos   = 0;  # next character index to check for case of alphanum char
    $quote_depth = 0;
    $allowed_quote_modifiers = "";
    $paren_depth             = 0;
    $brace_depth             = 0;
    $square_bracket_depth    = 0;
    $current_package         = "main";
    @current_depth[ 0 .. $#closing_brace_names ] =
      (0) x scalar @closing_brace_names;
    @nesting_sequence_number[ 0 .. $#closing_brace_names ] =
      ( 0 .. $#closing_brace_names );
    @current_sequence_number = ();

    $paren_type[$paren_depth]            = '';
    $paren_semicolon_count[$paren_depth] = 0;
    $brace_type[$brace_depth] = ';';    # identify opening brace as code block
    $brace_structural_type[$brace_depth]                   = '';
    $brace_context[$brace_depth]                           = UNKNOWN_CONTEXT;
    $paren_structural_type[$brace_depth]                   = '';
    $square_bracket_type[$square_bracket_depth]            = '';
    $square_bracket_structural_type[$square_bracket_depth] = '';
    $brace_package[$paren_depth]                           = $current_package;
    %is_constant                      = ();             # user-defined constants
    %is_user_function                 = ();             # user-defined functions
    %user_function_prototype          = ();             # their prototypes
    %is_block_function                = ();
    %is_block_list_function           = ();
    %saw_function_definition          = ();
    $unexpected_error_count           = 0;
    $want_paren                       = "";
    $context                          = UNKNOWN_CONTEXT;
    @slevel_stack                     = ();
    $ci_string_in_tokenizer           = "";
    $continuation_string_in_tokenizer = "0";
    $in_statement_continuation        = 0;
    @lower_case_labels_at             = ();
    $saw_v_string         = 0;      # for warning of v-strings on older perl
    $nesting_token_string = "";
    $nesting_type_string  = "";
    $nesting_block_string = '1';    # initially in a block
    $nesting_block_flag   = 1;
    $nesting_list_string  = '0';    # initially not in a list
    $nesting_list_flag    = 0;      # initially not in a list
    $nearly_matched_here_target_at = undef;
}

sub get_quote_target {
    return matching_end_token($quote_character);
}

sub get_indentation_level {
    return $level_in_tokenizer;
}

sub reset_indentation_level {
    $level_in_tokenizer  = $_[0];
    $slevel_in_tokenizer = $_[0];
    push @slevel_stack, $slevel_in_tokenizer;
}

{    # begin tokenize_this_line

    use constant BRACE          => 0;
    use constant SQUARE_BRACKET => 1;
    use constant PAREN          => 2;
    use constant QUESTION_COLON => 3;

    my (
        $block_type,      $container_type,       $expecting,
        $here_doc_target, $here_quote_character, $i,
        $i_tok,           $last_nonblank_i,      $next_tok,
        $next_type,       $prototype,            $rtoken_map,
        $rtoken_type,     $rtokens,              $tok,
        $type,            $type_sequence,
    );

    my @here_target_list = ();    # list of here-doc target strings

    # ------------------------------------------------------------
    # beginning of various scanner interfaces to simplify coding
    # ------------------------------------------------------------
    sub scan_bare_identifier {
        ( $i, $tok, $type, $prototype ) =
          scan_bare_identifier_do( $input_line, $i, $tok, $type, $prototype,
            $rtoken_map );
    }

    sub scan_identifier {
        ( $i, $tok, $type, $id_scan_state, $identifier ) =
          scan_identifier_do( $i, $id_scan_state, $identifier, $rtokens );
    }

    sub scan_id {
        ( $i, $tok, $type, $id_scan_state ) =
          scan_id_do( $input_line, $i, $tok, $rtokens, $rtoken_map,
            $id_scan_state );
    }

    my $number;

    sub scan_number {
        ( $i, $type, $number ) =
          scan_number_do( $input_line, $i, $rtoken_map, $type );
    }

    # a sub to warn if token found where term expected
    sub error_if_expecting_TERM {
        if ( $expecting == TERM ) {
            if ( $really_want_term{$last_nonblank_type} ) {
                unexpected( $tok, "term", $i_tok, $last_nonblank_i );
                1;
            }
        }
    }

    # a sub to warn if token found where operator expected
    sub error_if_expecting_OPERATOR {
        if ( $expecting == OPERATOR ) {
            my $thing = defined $_[0] ? $_[0] : $tok;
            unexpected( $thing, "operator", $i_tok, $last_nonblank_i );
            if ( $i_tok == 0 ) {
                interrupt_logfile();
                warning("Missing ';' above?\n");
                resume_logfile();
            }
            1;
        }
    }

    # ------------------------------------------------------------
    # end scanner interfaces
    # ------------------------------------------------------------

    my %is_for_foreach;
    @_ = qw(for foreach);
    @is_for_foreach{@_} = (1) x scalar(@_);

    # ------------------------------------------------------------
    # begin hash of code for handling most token types
    # ------------------------------------------------------------
    my $tokenization_code = {

        # no special code for these types yet, but syntax checks
        # could be added

##      '!'   => undef,
##      '!='  => undef,
##      '!~'  => undef,
##      '%='  => undef,
##      '&&=' => undef,
##      '&='  => undef,
##      '+='  => undef,
##      '-='  => undef,
##      '..'  => undef,
##      '..'  => undef,
##      '...' => undef,
##      '.='  => undef,
##      '<<=' => undef,
##      '<='  => undef,
##      '<=>' => undef,
##      '<>'  => undef,
##      '='   => undef,
##      '=='  => undef,
##      '=~'  => undef,
##      '>='  => undef,
##      '>>'  => undef,
##      '>>=' => undef,
##      '\\'  => undef,
##      '^='  => undef,
##      '|='  => undef,
##      '||=' => undef,
##      '~'   => undef,

        '>' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },
        '|' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },
        '$' => sub {

            # start looking for a scalar
            error_if_expecting_OPERATOR("Scalar")
              if ( $expecting == OPERATOR );
            scan_identifier();

            if ( $identifier eq '$^W' ) {
                $tokenizer_self->{_saw_perl_dash_w} = 1;
            }

            # Check for indentifier in indirect object slot
            # (vorboard.pl, sort.t).  Something like:
            #   /^(print|printf|sort|exec|system)$/ 
            if (
                $is_indirect_object_taker{$last_nonblank_token}

                || ( ( $last_nonblank_token eq '(' )
                    && $is_indirect_object_taker{ $paren_type[$paren_depth] } )
                || ( $last_nonblank_type =~ /^[Uw]$/ )    # possible object
              )
            {
                $type = 'Z';
            }
        },
        '(' => sub {

            ++$paren_depth;
            $paren_semicolon_count[$paren_depth] = 0;
            if ($want_paren) {
                $container_type = $want_paren;
                $want_paren     = "";
            }
            else {
                $container_type = $last_nonblank_token;

                # We can check for a syntax error here of unexpected '(',
                # but this is going to get messy...
                if (
                    $expecting == OPERATOR

                    # be sure this is not a method call of the form
                    # &method(...), $method->(..), &{method}(...)
                    # NOTE: at present, braces in something like &{ xxx }
                    # are not marked as a block, we might have a method call
                    && $last_nonblank_token !~ /^([\}\&]|\-\>)/

                  )
                {

                    # ref: camel 3 p 703.
                    if ( $last_last_nonblank_token eq 'do' ) {
                        complain(
"do SUBROUTINE is deprecated; consider & or -> notation\n"
                        );
                    }
                    else {

                        # if this is an empty list, (), then it is not an
                        # error; for example, we might have a constant pi and
                        # invoke it with pi() or just pi;
                        my ( $next_nonblank_token, $i_next ) =
                          find_next_nonblank_token( $i, $rtokens );
                        if ( $next_nonblank_token ne ')' ) {
                            my $hint;
                            error_if_expecting_OPERATOR('(');

                            if ( $last_nonblank_type eq 'C' ) {
                                $hint =
                                  "$last_nonblank_token has a void prototype\n";
                            }
                            elsif ( $last_nonblank_type eq 'i' ) {
                                if (   $i_tok > 0
                                    && $last_nonblank_token =~ /^\$/ )
                                {
                                    $hint =
"Do you mean '$last_nonblank_token->(' ?\n";
                                }
                            }
                            if ($hint) {
                                interrupt_logfile();
                                warning($hint);
                                resume_logfile();
                            }
                        } ## end if ( $next_nonblank_token...
                    } ## end else [ if ( $last_last_nonblank_token...
                } ## end if ( $expecting == OPERATOR...
            }
            $paren_type[$paren_depth] = $container_type;
            $type_sequence = increase_nesting_depth( PAREN, $i_tok );

            # propagate types down through nested parens
            # for example: the second paren in 'if ((' would be structural
            # since the first is.

            if ( $last_nonblank_token eq '(' ) {
                $type = $last_nonblank_type;
            }

            #     We exclude parens as structural after a ',' because it
            #     causes subtle problems with continuation indentation for
            #     something like this, where the first 'or' will not get
            #     indented.
            # 
            #         assert(
            #             __LINE__,
            #             ( not defined $check )
            #               or ref $check
            #               or $check eq "new"
            #               or $check eq "old",
            #         );
            # 
            #     Likewise, we exclude parens where a statement can start
            #     because of problems with continuation indentation, like
            #     these:
            # 
            #         ($firstline =~ /^#\!.*perl/)
            #         and (print $File::Find::name, "\n")
            #           and (return 1);
            # 
            #         (ref($usage_fref) =~ /CODE/) 
            #         ? &$usage_fref
            #           : (&blast_usage, &blast_params, &blast_general_params);

            else {
                $type = '{';
            }

            if ( $last_nonblank_type eq ')' ) {
                warning(
                    "Syntax error? found token '$last_nonblank_type' then '('\n"
                );
            }
            $paren_structural_type[$paren_depth] = $type;

        },
        ')' => sub {
            $type_sequence = decrease_nesting_depth( PAREN, $i_tok );

            if ( $paren_structural_type[$paren_depth] eq '{' ) {
                $type = '}';
            }

            $container_type = $paren_type[$paren_depth];

            #    /^(for|foreach)$/
            if ( $is_for_foreach{ $paren_type[$paren_depth] } ) {
                my $num_sc = $paren_semicolon_count[$paren_depth];
                if ( $num_sc > 0 && $num_sc != 2 ) {
                    warning("Expected 2 ';' in 'for(;;)' but saw $num_sc\n");
                }
            }

            if ( $paren_depth > 0 ) { $paren_depth-- }
        },
        ',' => sub {
            if ( $last_nonblank_type eq ',' ) {
                complain("Repeated ','s \n");
            }
##                FIXME: need to move this elsewhere, perhaps check after a '('
##                elsif ($last_nonblank_token eq '(') {
##                    warning("Leading ','s illegal in some versions of perl\n");
##                }
        },
        ';' => sub {
            $context        = UNKNOWN_CONTEXT;
            $statement_type = '';

            #    /^(for|foreach)$/ 
            if ( $is_for_foreach{ $paren_type[$paren_depth] } )
            {    # mark ; in for loop

                # Be careful: we do not want a semicolon such as the
                # following to be included: 
                #
                #    for (sort {strcoll($a,$b);} keys %investments) {

                if (   $brace_depth == $depth_array[PAREN][BRACE][$paren_depth]
                    && $square_bracket_depth ==
                    $depth_array[PAREN][SQUARE_BRACKET][$paren_depth] )
                {

                    $type = 'f';
                    $paren_semicolon_count[$paren_depth]++;
                }
            }

        },
        '"' => sub {
            error_if_expecting_OPERATOR("String")
              if ( $expecting == OPERATOR );
            $in_quote                = 1;
            $type                    = 'Q';
            $allowed_quote_modifiers = "";
        },
        "'" => sub {
            error_if_expecting_OPERATOR("String")
              if ( $expecting == OPERATOR );
            $in_quote                = 1;
            $type                    = 'Q';
            $allowed_quote_modifiers = "";
        },
        '`' => sub {
            error_if_expecting_OPERATOR("String")
              if ( $expecting == OPERATOR );
            $in_quote                = 1;
            $type                    = 'Q';
            $allowed_quote_modifiers = "";
        },
        '/' => sub {
            my $is_pattern;

            if ( $expecting == UNKNOWN ) {    # indeterminte, must guess..
                my $msg;
                ( $is_pattern, $msg ) =
                  guess_if_pattern_or_division( $i, $rtokens, $rtoken_map );

                if ($msg) {
                    write_diagnostics("DIVIDE:$msg\n");
                    write_logfile_entry($msg);
                }
            }
            else { $is_pattern = ( $expecting == TERM ) }

            if ($is_pattern) {
                $in_quote                = 1;
                $type                    = 'Q';
                $allowed_quote_modifiers = '[cgimosx]';
            }
            else {    # not a pattern; check for a /= token

                if ( $$rtokens[ $i + 1 ] eq '=' ) {    # form token /=
                    $i++;
                    $tok  = '/=';
                    $type = $tok;
                }

                #DEBUG - collecting info on what tokens follow a divide
                # for development of guessing algorithm
                #if ( numerator_expected( $i, $rtokens ) < 0 ) {
                #    #write_diagnostics( "DIVIDE? $input_line\n" );
                #}
            }
        },
        '{' => sub {

            # if we just saw a ')', we will label this block with
            # its type.  We need to do this to allow sub
            # code_block_type to determine if this brace starts a
            # code block or anonymous hash.  (The type of a paren
            # pair is the preceding token, such as 'if', 'else',
            # etc).
            $container_type = "";
            if ( $last_nonblank_token eq ')' ) {
                $last_nonblank_token = $paren_type[ $paren_depth + 1 ];

                # defensive move in case of a nesting error (pbug.t)
                # in which this ')' had no previous '('
                # this nesting error will have been caught
                if ( !defined($last_nonblank_token) ) {
                    $last_nonblank_token = 'if';
                }

                # check for syntax error here;
                # expecting: (if|elsif|while|until|for|foreach) 
                # Delete this if it is too redundant
                #unless ( $is_keyword{$last_nonblank_token} ) {
                unless ( $last_nonblank_token =~
                    /^(if|elsif|unless|while|until|for|foreach)$/ )
                {
                    warning(
"syntax error at ') {', didn't see (if|elsif|unless|while|until|for|foreach)\n"
                    );
                }
            }

            # patch for paren-less for/foreach glitch, part 2.
            # see note below under 'qw'
            elsif ($last_nonblank_token eq 'qw'
                && $is_for_foreach{$want_paren} )
            {
                $last_nonblank_token = $want_paren;
                if ( $last_last_nonblank_token eq $want_paren ) {
                    warning(
"syntax error at '$want_paren .. {' -- missing \$ loop variable\n"
                    );

                }
                $want_paren = "";
            }

            # now identify which of the three possible types of
            # curly braces we have: hash index container, anonymous
            # hash reference, or code block.

            # non-structural (hash index) curly brace pair 
            # get marked 'L' and 'R'
            if ( is_non_structural_brace() ) {
                $type = 'L';
            }

            # code and anonymous hash have the same type, '{', but are 
            # distinguished by 'block_type', 
            # which will be blank for an anonymous hash
            else {
                $block_type = code_block_type( $i_tok, $rtokens );
            }
            $brace_type[ ++$brace_depth ] = $block_type;
            $brace_package[$brace_depth] = $current_package;
            $type_sequence = increase_nesting_depth( BRACE, $i_tok );
            $brace_structural_type[$brace_depth] = $type;
            $brace_context[$brace_depth]         = $context;
        },
        '}' => sub {
            $block_type = $brace_type[$brace_depth];
            if ($block_type) { $statement_type = '' }

            if ( defined( $brace_package[$brace_depth] ) ) {
                $current_package = $brace_package[$brace_depth];
            }

            # can happen on brace error (caught elsewhere)
            else {
            }
            $type_sequence = decrease_nesting_depth( BRACE, $i_tok );

            if ( $brace_structural_type[$brace_depth] eq 'L' ) {
                $type = 'R';
            }

            # propagate type information for 'do' and 'eval' blocks.  
            # This is necessary to enable us to know if an operator 
            # or term is expected next
            if ( $is_block_operator{ $brace_type[$brace_depth] } ) {
                $tok = $brace_type[$brace_depth];
            }

            $context = $brace_context[$brace_depth];
            if ( $brace_depth > 0 ) { $brace_depth--; }
        },
        '&' => sub {    # maybe sub call? start looking

            # We have to check for sub call unless we are sure we
            # are expecting an operator.  This example from s2p
            # got mistaken as a q operator in an early version:
            #   print BODY &q(<<'EOT');
            if ( $expecting != OPERATOR ) {
                scan_identifier();
            }
            else {
            }
        },
        '<' => sub {    # angle operator or less than?

            if ( $expecting != OPERATOR ) {
                ( $i, $type ) =
                  find_angle_operator_termination( $input_line, $i, $rtoken_map,
                    $expecting );

            }
            else {
            }
        },
        '?' => sub {    # ?: conditional or starting pattern?

            my $is_pattern;

            if ( $expecting == UNKNOWN ) {

                my $msg;
                ( $is_pattern, $msg ) =
                  guess_if_pattern_or_conditional( $i, $rtokens, $rtoken_map );

                if ($msg) { write_logfile_entry($msg) }
            }
            else { $is_pattern = ( $expecting == TERM ) }

            if ($is_pattern) {
                $in_quote                = 1;
                $type                    = 'Q';
                $allowed_quote_modifiers = '[cgimosx]';    # TBD:check this
            }
            else {

                $type_sequence =
                  increase_nesting_depth( QUESTION_COLON, $i_tok );
            }
        },
        '*' => sub {    # typeglob, or multiply?

            if ( $expecting == TERM ) {
                scan_identifier();
            }
            else {

                if ( $$rtokens[ $i + 1 ] eq '=' ) {
                    $tok  = '*=';
                    $type = $tok;
                    $i++;
                }
                elsif ( $$rtokens[ $i + 1 ] eq '*' ) {
                    $tok  = '**';
                    $type = $tok;
                    $i++;
                    if ( $$rtokens[ $i + 1 ] eq '=' ) {
                        $tok  = '**=';
                        $type = $tok;
                        $i++;
                    }
                }
            }
        },
        '.' => sub {    # what kind of . ?

            if ( $expecting != OPERATOR ) {
                scan_number();
                if ( $type eq '.' ) {
                    error_if_expecting_TERM()
                      if ( $expecting == TERM );
                }
            }
            else {
            }
        },
        ':' => sub {

            # if this is the first nonblank character, call it a label
            # since perl seems to just swallow it
            if ( $input_line_number == 1 && $last_nonblank_i == -1 ) {
                $type = 'J';
            }

            # otherwise, it should be part of a ?/: operator
            else {
                $type_sequence =
                  decrease_nesting_depth( QUESTION_COLON, $i_tok );
                if ( $last_nonblank_token eq '?' ) {
                    warning("Syntax error near ? :\n");
                }
            }
        },
        '+' => sub {    # what kind of plus?

            if ( $expecting == TERM ) {
                scan_number();

                # unary plus is safest assumption if not a number
                if ( !defined($number) ) { $type = 'p'; }
            }
            elsif ( $expecting == OPERATOR ) {
            }
            else {
                if ( $next_type eq 'w' ) { $type = 'p' }
            }
        },
        '@' => sub {

            error_if_expecting_OPERATOR("Array")
              if ( $expecting == OPERATOR );
            scan_identifier();
        },
        '%' => sub {    # hash or modulo?

            # first guess is hash if no following blank
            if ( $expecting == UNKNOWN ) {
                if ( $next_type ne 'b' ) { $expecting = TERM }
            }
            if ( $expecting == TERM ) {
                scan_identifier();
            }
        },
        '[' => sub {
            $square_bracket_type[ ++$square_bracket_depth ] =
              $last_nonblank_token;
            $type_sequence = increase_nesting_depth( SQUARE_BRACKET, $i_tok );

            # It may seem odd, but structural square brackets have
            # type '{' and '}'.  This simplifies the indentation logic.
            if ( !is_non_structural_brace() ) {
                $type = '{';
            }
            $square_bracket_structural_type[$square_bracket_depth] = $type;
        },
        ']' => sub {
            $type_sequence = decrease_nesting_depth( SQUARE_BRACKET, $i_tok );

            if ( $square_bracket_structural_type[$square_bracket_depth] eq '{' )
            {
                $type = '}';
            }
            if ( $square_bracket_depth > 0 ) { $square_bracket_depth--; }
        },
        '-' => sub {    # what kind of minus?

            if ( ( $expecting != OPERATOR )
                && $is_file_test_operator{$next_tok} )
            {
                $i++;
                $tok .= $next_tok;
                $type = 'F';
            }
            elsif ( $expecting == TERM ) {
                scan_number();

                # maybe part of bareword token? unary is safest
                if ( !defined($number) ) { $type = 'm'; }

            }
            elsif ( $expecting == OPERATOR ) {
            }
            else {

                if ( $next_type eq 'w' ) {
                    $type = 'm';
                }
            }
        },

        '^' => sub {

            # check for special variables like ${^WARNING_BITS}
            if ( $expecting == TERM ) {

                # FIXME: this should work but will not catch errors
                # because we also have to be sure that previous token is
                # a type character ($,@,%).  
                if ( $last_nonblank_token eq '{'
                    && ( $next_tok =~ /^[A-Za-z_]/ ) )
                {

                    if ( $next_tok eq 'W' ) {
                        $tokenizer_self->{_saw_perl_dash_w} = 1;
                    }
                    $tok  = $tok . $next_tok;
                    $i    = $i + 1;
                    $type = 'w';
                }

                else {
                    unless ( error_if_expecting_TERM() ) {

                        # Something like this is valid but strange: 
                        # undef ^I;
                        complain("The '^' seems unusual here\n");
                    }
                }
            }
        },

        '::' => sub {    # probably a sub call
            scan_bare_identifier();
        },
        '<<' => sub {    # maybe a here-doc?
            return
              unless ( $i < $max_token_index )
              ;          # here-doc not possible if end of line

            if ( $expecting != OPERATOR ) {
                my ($found_target);
                ( $found_target, $here_doc_target, $here_quote_character, $i ) =
                  find_here_doc( $expecting, $i, $rtokens, $rtoken_map );

                if ($found_target) {
                    push @here_target_list,
                      [ $here_doc_target, $here_quote_character ];
                    $type = 'h';
                    if ( length($here_doc_target) > 80 ) {
                        my $truncated = substr( $here_doc_target, 0, 80 );
                        complain("Long here-target: '$truncated' ...\n");
                    }
                    elsif ( $here_doc_target !~ /^[A-Z_]\w+$/ ) {
                        complain(
                            "Unconventional here-target: '$here_doc_target'\n"
                        );
                    }
                }
                elsif ( $expecting == TERM ) {

                    # shouldn't happen..
                    warning("Program bug; didn't find here doc target\n");
                    report_definite_bug();
                }
            }
            else {
            }
        },
        '->' => sub {

            # if -> points to a bare word, we must scan for an identifier,
            # otherwise something like ->y would look like the y operator
            scan_identifier();
        },

        # type = 'pp' for pre-increment, '++' for post-increment 
        '++' => sub {
            if ( $expecting == TERM ) { $type = 'pp' }
        },

        '=>' => sub {
            if ( $last_nonblank_type eq $tok ) {
                complain("Repeated '=>'s \n");
            }
        },

        # type = 'mm' for pre-decrement, '--' for post-decrement
        '--' => sub {

            if ( $expecting == TERM ) { $type = 'mm' }
        },

        '&&' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },

        '||' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },
    };

    # ------------------------------------------------------------
    # end hash of code for handling individual token types
    # ------------------------------------------------------------

    my %matching_start_token = ( '}' => '{', ']' => '[', ')' => '(' );

    my %is_zero_continuation_block_type;
    @_ = qw( } { BEGIN END CHECK INIT AUTOLOAD DESTROY continue ;
      if elsif else unless while until for foreach );
    @is_zero_continuation_block_type{@_} = (1) x scalar(@_);

    my %is_not_zero_continuation_block_type;
    @_ = qw(sort grep map do eval);
    @is_not_zero_continuation_block_type{@_} = (1) x scalar(@_);

    my %is_logical_container;
    @_ = qw(if elsif unless while and or not && !  || for foreach);
    @is_logical_container{@_} = (1) x scalar(@_);

    my %is_binary_type;
    @_ = qw(|| &&);
    @is_binary_type{@_} = (1) x scalar(@_);

    # 'L' is token for opening { at hash key
    my %is_opening_type;
    @_ = qw" L { ( [ ";
    @is_opening_type{@_} = (1) x scalar(@_);

    # 'R' is token for closing } at hash key
    my %is_closing_type;
    @_ = qw" R } ) ] ";
    @is_closing_type{@_} = (1) x scalar(@_);

    my %is_q_qq_qw_qx_qr_s_y_tr_m;
    @_ = qw(q qq qw qx qr s y tr m);
    @is_q_qq_qw_qx_qr_s_y_tr_m{@_} = (1) x scalar(@_);

    my %is_redo_last_next_goto;
    @_ = qw(redo last next goto);
    @is_redo_last_next_goto{@_} = (1) x scalar(@_);

    my %is_use_require;
    @_ = qw(use require);
    @is_use_require{@_} = (1) x scalar(@_);

    my %is_sub_package;
    @_ = qw(sub package);
    @is_sub_package{@_} = (1) x scalar(@_);

    # This hash holds the hash key in $tokenizer_self for these keywords:
    my %is_format_END_DATA = (
        'format'   => '_in_format',
        '__END__'  => '_in_end',
        '__DATA__' => '_in_data',
    );

    # ref: camel 3 p 147, 
    # but perl may accept undocumented flags
    my %quote_modifiers = (
        's'  => '[cegimosx]',
        'y'  => '[cds]',
        'tr' => '[cds]',
        'm'  => '[cgimosx]',
        'qr' => '[imosx]',
        'q'  => "",
        'qq' => "",
        'qw' => "",
        'qx' => "",
    );

    # table showing how many quoted things to look for after quote operator..
    # s, y, tr have 2 (pattern and replacement)
    # others have 1 (pattern only)
    my %quote_items = (
        's'  => 2,
        'y'  => 2,
        'tr' => 2,
        'm'  => 1,
        'qr' => 1,
        'q'  => 1,
        'qq' => 1,
        'qw' => 1,
        'qx' => 1,
    );

    sub tokenize_this_line {

# =pod 
# 
# This routine breaks a line of perl code into tokens which are of use in
# indentation and reformatting.  One of my goals has been to define tokens
# such that a newline may be inserted between any pair of tokens without
# changing or invalidating the program. This version comes close to this,
# although there are necessarily a few exceptions which must be caught by
# the formatter.  Many of these involve the treatment of bare words.
# 
# The tokens and their types are returned in arrays.  See previous
# routine for their names.
# 
# See also the array "valid_token_types" in the BEGIN section for an
# up-to-date list.
# 
# To simplify things, token types are either a single character, or they
# are identical to the tokens themselves.
# 
# As a debugging aid, the -D flag creates a file containing a side-by-side
# comparison of the input string and its tokenization for each line of a file.
# This is an invaluable debugging aid.
# 
# In addition to tokens, and some associated quantities, the tokenizer
# also returns flags indication any special line types.  These include
# quotes, here_docs, formats.
# 
# -----------------------------------------------------------------------
# 
# How to add NEW_TOKENS:
# 
# New token types will undoubtedly be needed in the future both to keep up
# with changes in perl and to help adapt the tokenizer to other applications.
# 
# Here are some notes on the minimal steps.  I wrote these notes while
# adding the 'v' token type for v-strings, which are things like version
# numbers 5.6.0, and ip addresses, and will use that as an example.  ( You
# can use your editor to search for the string "NEW_TOKENS" to find the
# appropriate sections to change):
# 
# *. Try to talk somebody else into doing it!  If not, ..
# 
# *. Make a backup of your current version in case things don't work out!
# 
# *. Think of a new, unused character for the token type, and add to
# the array @valid_token_types in the BEGIN section of this package.
# For example, I used 'v' for v-strings.
# 
# *. Implement coding to recognize the $type of the token in this routine.
# This is the hardest part, and is best done by immitating or modifying
# some of the existing coding.  For example, to recognize v-strings, I
# patched 'sub scan_bare_identifier' to recognize v-strings beginning with
# 'v' and 'sub scan_number' to recognize v-strings without the leading 'v'.
# 
# *. Update sub operator_expected.  This update is critically important but
# the coding is trivial.  Look at the comments in that routine for help.
# For v-strings, which should behave like numbers, I just added 'v' to the
# regex used to handle numbers and strings (types 'n' and 'Q').
# 
# *. Implement a 'bond strength' rule in sub set_bond_strengths in
# Perl::Tidy::Formatter for breaking lines around this token type.  You can
# skip this step and take the default at first, then adjust later to get
# desired results.  For adding type 'v', I looked at sub bond_strength and
# saw that number type 'n' was using default strengths, so I didn't do
# anything.  I may tune it up someday if I don't like the way line
# breaks with v-strings look.
# 
# *. Implement a 'whitespace' rule in sub set_white_space_flag in
# Perl::Tidy::Formatter.  For adding type 'v', I looked at this routine
# and saw that type 'n' used spaces on both sides, so I just added 'v'
# to the array @spaces_both_sides. 
# 
# *. Update HtmlWriter package so that users can colorize the token as
# desired.  This is quite easy; see comments identified by 'NEW_TOKENS' in
# that package.  For v-strings, I initially chose to use a default color
# equal to the default for numbers, but it might be nice to change that
# eventually.
# 
# *. Update comments in Perl::Tidy::Tokenizer::dump_token_types.  
# 
# *. Run lots and lots of debug tests.  Start with special files designed
# to test the new token type.  Run with the -D flag to create a .DEBUG
# file which shows the tokenization.  When these work ok, test as many old
# scripts as possible.  Start with all of the '.t' files in the 'test'
# directory of the distribution file.  Compare .tdy output with previous
# version and updated version to see the differences.  Then include as
# many more files as possible. My own technique has been to collect a huge
# number of perl scripts (thousands!) into one directory and run perltidy
# *, then run diff between the output of the previous version and the
# current version.
# 
# -----------------------------------------------------------------------
# 
# =cut

        my $line_of_tokens = shift;
        my ($untrimmed_input_line) = $line_of_tokens->{_line_text};

        # patch while coding change is underway
        # make callers private data to allow access
        # $tokenizer_self = $caller_tokenizer_self;

        # extract line number for use in error messages
        $input_line_number = $line_of_tokens->{_line_number};

        # check for pod documentation
        if ( ( $untrimmed_input_line =~ /^=[A-Za-z_]/ ) ) {

            # must not be in multi-line quote
            # and must not be in an eqn
            if ( !$in_quote and ( operator_expected( 'b', '=', 'b' ) == TERM ) )
            {
                $tokenizer_self->{_in_pod} = 1;
                return;
            }
        }

        $input_line = $untrimmed_input_line;

        chomp $input_line;

        # trim start of this line unless we are continuing a quoted line
        # do not trim end because we might end in a quote (test: deken4.pl)
        # Perl::Tidy::Formatter will delete needless trailing blanks
        unless ( $in_quote && ( $quote_type eq 'Q' ) ) {
            $input_line =~ s/^\s*//;    # trim left end
        }

        # initialize for the main loop
        my @output_token_list     = ();    # stack of output token indexes
        my @output_token_type     = ();    # token types
        my @output_block_type     = ();    # types of code block
        my @output_container_type = ();    # paren types, such as if, elsif, ..
        my @output_type_sequence  = ();    # nesting sequential number

        $tok             = $last_nonblank_token;
        $type            = $last_nonblank_type;
        $prototype       = $last_nonblank_prototype;
        $last_nonblank_i = -1;
        $block_type      = $last_nonblank_block_type;
        $container_type  = $last_nonblank_container_type;
        $type_sequence   = $last_nonblank_type_sequence;
        @here_target_list = ();            # list of here-doc target strings

        $peeked_ahead = 0;

        # tokenization is done in two stages..
        # stage 1 is a very simple pre-tokenization
        my $max_tokens_wanted = 0; # this signals pre_tokenize to get all tokens

        # a little optimization for a full-line comment
        if ( !$in_quote && ( $input_line =~ /^#/ ) ) {
            $max_tokens_wanted = 1    # no use tokenizing a comment
        }

        # start by breaking the line into pre-tokens
        ( $rpretokens, $rpretoken_map, $rpretoken_type ) =
          pre_tokenize( $input_line, $max_tokens_wanted );

        $max_token_index = scalar(@$rpretokens) - 1;
        push ( @$rpretokens, ' ', ' ', ' ' )
          ;                           # extra whitespace simplifies logic
        push ( @$rpretoken_map,  0,   0,   0 );     # shouldn't be referenced
        push ( @$rpretoken_type, 'b', 'b', 'b' );

        # temporary copies while coding change is underway
        ( $rtokens, $rtoken_map, $rtoken_type ) =
          ( $rpretokens, $rpretoken_map, $rpretoken_type );

        # initialize for main loop
        for $i ( 0 .. $max_token_index + 3 ) {
            $output_token_type[$i]     = "";
            $output_block_type[$i]     = "";
            $output_container_type[$i] = "";
            $output_type_sequence[$i]  = "";
        }
        $i     = -1;
        $i_tok = -1;

        # ------------------------------------------------------------
        # begin main tokenization loop
        # ------------------------------------------------------------

        # we are looking at each pre-token of one line and combining them
        # into tokens
        while ( ++$i <= $max_token_index ) {

            if ($in_quote) {    # continue looking for end of a quote
                $type = $quote_type;

                unless (@output_token_list) {  # initialize if continuation line
                    push ( @output_token_list, $i );
                    $output_token_type[$i] = $type;

                }
                $tok = $quote_character unless ( $quote_character =~ /^\s*$/ );

                # scan for the end of the quote or pattern
                ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth ) =
                  do_quote( $i, $in_quote, $quote_character, $quote_pos,
                    $quote_depth, $rtokens, $rtoken_map );

                # all done if we didn't find it
                last if ($in_quote);

                # re-initialize for next search
                $quote_character = '';
                $quote_pos       = 0;
                $quote_type      = 'Q';
                last if ( ++$i > $max_token_index );

                # look for any modifiers
                if ($allowed_quote_modifiers) {

                    # check for exact quote modifiers
                    if ( $$rtokens[$i] =~ /^[A-Za-z_]/ ) {
                        my $str = $$rtokens[$i];
                        while ( $str =~ /\G$allowed_quote_modifiers/gc ) { }

                        if ( defined( pos($str) ) ) {

                            # matched
                            if ( pos($str) == length($str) ) {
                                last if ( ++$i > $max_token_index );
                            }

                            # Looks like a joined quote modifier
                            # and keyword, maybe something like
                            # s/xxx/yyy/gefor @k=...  
                            # Example is "galgen.pl".  Would have to split
                            # the word and insert a new token in the
                            # pre-token list.  This is so rare that I haven't
                            # done it.  Will just issue a warning citation.

                            # This error might also be triggered if my quote
                            # modifier characters are incomplete
                            else {
                                warning(<<EOM);

Partial match to quote modifier $allowed_quote_modifiers at word: '$str'
Please put a space between quote modifiers and trailing keywords.
EOM

                                # print "token $$rtokens[$i]\n";
                                # my $num = length($str) - pos($str);
                                # $$rtokens[$i]=substr($$rtokens[$i],pos($str),$num);
                                # print "continuing with new token $$rtokens[$i]\n";

                                # skipping past this token does least damage
                                last if ( ++$i > $max_token_index );
                            }
                        }
                        else {

                            # example file: rokicki4.pl
                            # This error might also be triggered if my quote
                            # modifier characters are incomplete
                            write_logfile_entry(
"Note: found word $str at quote modifier location\n"
                            );
                        }
                    }

                    # re-initialize
                    $allowed_quote_modifiers = "";
                }
            }

            unless ( $tok =~ /^\s*$/ ) {

                # try to catch some common errors
                if ( ( $type eq 'n' ) && ( $tok ne '0' ) ) {

                    if ( $last_nonblank_token eq 'eq' ) {
                        complain("Should 'eq' be '==' here ?\n");
                    }
                    elsif ( $last_nonblank_token eq 'ne' ) {
                        complain("Should 'ne' be '!=' here ?\n");
                    }
                }

                $last_last_nonblank_token          = $last_nonblank_token;
                $last_last_nonblank_type           = $last_nonblank_type;
                $last_last_nonblank_block_type     = $last_nonblank_block_type;
                $last_last_nonblank_container_type =
                  $last_nonblank_container_type;
                $last_last_nonblank_type_sequence =
                  $last_nonblank_type_sequence;
                $last_nonblank_token          = $tok;
                $last_nonblank_type           = $type;
                $last_nonblank_prototype      = $prototype;
                $last_nonblank_block_type     = $block_type;
                $last_nonblank_container_type = $container_type;
                $last_nonblank_type_sequence  = $type_sequence;
                $last_nonblank_i              = $i_tok;
            }

            # store previous token type
            if ( $i_tok >= 0 ) {
                $output_token_type[$i_tok]     = $type;
                $output_block_type[$i_tok]     = $block_type;
                $output_container_type[$i_tok] = $container_type;
                $output_type_sequence[$i_tok]  = $type_sequence;
            }
            my $pre_tok  = $$rtokens[$i];        # get the next pre-token
            my $pre_type = $$rtoken_type[$i];    # and type
            $tok  = $pre_tok;
            $type = $pre_type;                   # to be modified as necessary
            $block_type = "";    # blank for all tokens except code block braces
            $container_type = "";    # blank for all tokens except some parens
            $type_sequence  = "";    # blank for all tokens except ?/:
            $prototype = "";    # blank for all tokens except user defined subs
            $i_tok     = $i;

            # this pre-token will start an output token
            push ( @output_token_list, $i_tok );

            # continue gathering identifier if necessary
            # but do not start on blanks and comments
            if ( $id_scan_state && $pre_type !~ /[b#]/ ) {

                if ( $id_scan_state =~ /^(sub|package)/ ) {
                    scan_id();
                }
                else {
                    scan_identifier();
                }

                last if ($id_scan_state);
                next if ( ( $i > 0 ) || $type );

                # didn't find any token; start over
                $type = $pre_type;
                $tok  = $pre_tok;
            }

            # handle whitespace tokens..
            next if ( $type eq 'b' );
            my $prev_tok  = $i > 0 ? $$rtokens[ $i - 1 ]     : ' ';
            my $prev_type = $i > 0 ? $$rtoken_type[ $i - 1 ] : 'b';

            # Build larger tokens where possible, since we are not in a quote.
            #
            # First try to assemble digraphs.  The following tokens are
            # excluded and handled specially: 
            # '/=' is excluded because the / might start a pattern.
            # 'x=' is excluded since it might be $x=, with $ on previous line  
            # '**' and *= might be typeglobs of punctuation variables
            # I have allowed tokens starting with <, such as <=,
            # because I don't think these could be valid angle operators.
            # test file: storrs4.pl
            my $test_tok = $tok . $$rtokens[ $i + 1 ];

            if (
                $is_digraph{$test_tok}
                && ( $test_tok ne '/=' )    # might be pattern
                && ( $test_tok ne 'x=' )    # might be $x
                && ( $test_tok ne '**' )    # typeglob?
                && ( $test_tok ne '*=' )    # typeglob?
              )
            {
                $tok = $test_tok;
                $i++;

                # Now try to assemble trigraphs.  Note that all possible
                # perl trigraphs can be constructed by appending a character
                # to a digraph.
                $test_tok = $tok . $$rtokens[ $i + 1 ];

                if ( $is_trigraph{$test_tok} ) {
                    $tok = $test_tok;
                    $i++;
                }
            }
            $type      = $tok;
            $next_tok  = $$rtokens[ $i + 1 ];
            $next_type = $$rtoken_type[ $i + 1 ];

            TOKENIZER_DEBUG_FLAG_TOKENIZE && do {
                local $" = ')(';
                my @debug_list = (
                    $last_nonblank_token,      $tok,
                    $next_tok,                 $brace_depth,
                    $brace_type[$brace_depth], $paren_depth,
                    $paren_type[$paren_depth]
                );
                print "TOKENIZE:(@debug_list)\n";
            };

            ###############################################################
            # We have the next token, $tok.
            # Now we have to examine this token and decide what it is
            # and define its $type
            #
            # section 1: bare words
            ###############################################################

            if ( $pre_type eq 'w' ) {
                $expecting = operator_expected( $prev_type, $tok, $next_type );
                my ( $next_nonblank_token, $i_next ) =
                  find_next_nonblank_token( $i, $rtokens );

                # quote a word followed by => operator
                if ( $next_nonblank_token eq '=' ) {

                    if ( $$rtokens[ $i_next + 1 ] eq '>' ) {
                        if ( $is_constant{$current_package}{$tok} ) {
                            $type = 'C';
                        }
                        elsif ( $is_user_function{$current_package}{$tok} ) {
                            $type      = 'U';
                            $prototype =
                              $user_function_prototype{$current_package}{$tok};
                        }
                        elsif ( $tok =~ /^v\d+$/ ) {
                            $type = 'v';
                            unless ($saw_v_string) { report_v_string($tok) }
                        }
                        else { $type = 'w' }

                        next;
                    }
                }

                # quote a bare word within braces..like xxx->{s}; note that we
                # must be sure this is not a structural brace, to avoid
                # mistaking {s} in the following for a quoted bare word:
                #     for(@[){s}bla}BLA}
                if (   ( $last_nonblank_type eq 'L' )
                    && ( $next_nonblank_token eq '}' ) )
                {
                    $type = 'w';
                    next;
                }

                # a bare word immediately followed by :: is not a keyword;
                # use $tok_kw when testing for keywords to avoid a mistake
                my $tok_kw = $tok;
                if ( $$rtokens[ $i + 1 ] eq ':' && $$rtokens[ $i + 2 ] eq ':' )
                {
                    $tok_kw .= '::';
                }

                # handle operator x (now we know it isn't $x=)
                if ( ( $tok =~ /^x\d*$/ ) && ( $expecting == OPERATOR ) ) {
                    if ( $tok eq 'x' ) {

                        if ( $$rtokens[ $i + 1 ] eq '=' ) {    # x=
                            $tok  = 'x=';
                            $type = $tok;
                            $i++;
                        }
                        else {
                            $type = 'x';
                        }
                    }

                    # FIXME: Patch: mark something like x4 as an integer for now
                    # It gets fixed downstream.  This is easier than
                    # splitting the pretoken.
                    else {
                        $type = 'n';
                    }
                }

                elsif ( ( $tok eq 'strict' )
                    and ( $last_nonblank_token eq 'use' ) )
                {
                    $tokenizer_self->{_saw_use_strict} = 1;
                    scan_bare_identifier();
                }

                elsif ( ( $tok eq 'warnings' )
                    and ( $last_nonblank_token eq 'use' ) )
                {
                    $tokenizer_self->{_saw_perl_dash_w} = 1;

                    # scan as identifier, so that we pick up something like:
                    # use warnings::register
                    scan_bare_identifier();
                }

                elsif (
                       $tok eq 'AutoLoader'
                    && $tokenizer_self->{_look_for_autoloader}
                    && (
                        $last_nonblank_token eq 'use'

                        # these regexes are from AutoSplit.pm, which we want
                        # to mimic
                        || $input_line =~ /^\s*(use|require)\s+AutoLoader\b/
                        || $input_line =~ /\bISA\s*=.*\bAutoLoader\b/
                    )
                  )
                {
                    write_logfile_entry("AutoLoader seen, -nlal deactivates\n");
                    $tokenizer_self->{_saw_autoloader}      = 1;
                    $tokenizer_self->{_look_for_autoloader} = 0;
                    scan_bare_identifier();
                }

                elsif (
                       $tok eq 'SelfLoader'
                    && $tokenizer_self->{_look_for_selfloader}
                    && (   $last_nonblank_token eq 'use'
                        || $input_line =~ /^\s*(use|require)\s+SelfLoader\b/
                        || $input_line =~ /\bISA\s*=.*\bSelfLoader\b/ )
                  )
                {
                    write_logfile_entry("SelfLoader seen, -nlsl deactivates\n");
                    $tokenizer_self->{_saw_selfloader}      = 1;
                    $tokenizer_self->{_look_for_selfloader} = 0;
                    scan_bare_identifier();
                }

                elsif ( ( $tok eq 'constant' )
                    and ( $last_nonblank_token eq 'use' ) )
                {
                    scan_bare_identifier();
                    my ( $next_nonblank_token, $i_next ) =
                      find_next_nonblank_token( $i, $rtokens );

                    if ($next_nonblank_token) {

                        if ( $is_keyword{$next_nonblank_token} ) {
                            warning(
"Attempting to define constant '$next_nonblank_token' which is a perl keyword\n"
                            );
                        }

                        # FIXME: could check for error in which next token is
                        # not a word (number, punctuation, ..)
                        else {
                            $is_constant{$current_package}
                              {$next_nonblank_token} = 1;
                        }
                    }
                }

                # various quote operators
                elsif ( $is_q_qq_qw_qx_qr_s_y_tr_m{$tok} ) {
                    if ( $expecting == OPERATOR ) {

                        # patch for paren-less for/foreach glitch, part 1
                        # perl will accept this construct as valid:
                        #
                        #    foreach my $key qw\Uno Due Tres Quadro\ {
                        #        print "Set $key\n";
                        #    }
                        unless ( $tok eq 'qw' && $is_for_foreach{$want_paren} )
                        {
                            error_if_expecting_OPERATOR();
                        }
                    }
                    $in_quote                = $quote_items{$tok};
                    $allowed_quote_modifiers = $quote_modifiers{$tok};

                    # All quote types are 'Q' except possibly qw quotes.
                    # qw quotes are special in that they may generally be trimmed
                    # of leading and trailing whitespace.  So they are given a 
                    # separate type, 'q', unless requested otherwise.  
                    $type =
                      ( $tok eq 'qw' && $tokenizer_self->{_trim_qw} )
                      ? 'q'
                      : 'Q';
                    $quote_type = $type;
                }

                # check for a statement label
                elsif (( $next_nonblank_token eq ':' )
                    && ( $$rtokens[ $i_next + 1 ] ne ':' )
                    && label_ok() )
                {
                    if ( $tok !~ /A-Z/ ) {
                        push @lower_case_labels_at, $input_line_number;
                    }
                    $type = 'J';
                    $tok .= ':';
                    $i = $i_next;
                    next;
                }

                #      'sub' || 'package' 
                elsif ( $is_sub_package{$tok_kw} ) {
                    error_if_expecting_OPERATOR()
                      if ( $expecting == OPERATOR );
                    scan_id();
                }

                # Note on token types for format, __DATA__, __END__:
                # It simplifies things to give these type ';', so that when we
                # start rescanning we will be expecting a token of type TERM.
                # We will switch to type 'k' before outputting the tokens.
                elsif ( $is_format_END_DATA{$tok_kw} ) {
                    $type = ';';    # make tokenizer look for TERM next
                    $tokenizer_self->{ $is_format_END_DATA{$tok_kw} } = 1;
                    last;
                }

                elsif ( $is_keyword{$tok_kw} ) {
                    $type = 'k';

                    # Since for and foreach may not be followed immediately
                    # by an opening paren, we have to remember which keyword
                    # is associated with the next '('
                    if ( $is_for_foreach{$tok} ) {
                        if ( new_statement_ok() ) {
                            $want_paren = $tok;
                        }
                    }

                    # recognize 'use' statements, which are special
                    elsif ( $is_use_require{$tok} ) {
                        $statement_type = $tok;
                        error_if_expecting_OPERATOR()
                          if ( $expecting == OPERATOR );
                    }

                    # Check for misplaced 'elsif' and 'else', but allow isolated
                    # else or elsif blocks to be formatted.  This is indicated
                    # by a last noblank token of ';'
                    elsif ( $tok eq 'elsif' || $tok eq 'else' ) {
                        if (   $last_nonblank_token ne ';'
                            && $last_nonblank_block_type !~
                            /^(if|elsif|unless)$/ )
                        {
                            warning(
"'$tok' should follow an 'if', 'elsif', or 'unless' block\n"
                            );
                        }
                    }
                    elsif ( $tok eq 'continue' ) {
                        if (   $last_nonblank_token ne ';'
                            && $last_nonblank_block_type !~
                            /^(\{|\}|;|while|until|for|foreach)$/ )
                        {

                            # note: ';' '{' and '}' in list above
                            # because continues can follow bare blocks
                            warning("'$tok' should follow a block\n");
                        }
                    }
                }

                # check for inline label following 
                #         /^(redo|last|next|goto)$/ 
                elsif (( $last_nonblank_type eq 'k' )
                    && ( $is_redo_last_next_goto{$last_nonblank_token} ) )
                {
                    $type = 'j';
                    next;
                }

                # something else -- 
                else {

                    scan_bare_identifier();
                    if ( $type eq 'w' ) {
                        error_if_expecting_OPERATOR("bareword")
                          if ( $expecting == OPERATOR );

                        # mark bare words immediately followed by a paren as
                        # functions
                        $next_tok = $$rtokens[ $i + 1 ];
                        if ( $next_tok eq '(' ) {
                            $type = 'U';
                        }

                        # mark bare words following a file test operator as
                        # something that will expect an operator next.
                        # patch 072901: unless followed immediately by a paren,
                        # in which case it must be a function call (pid.t)
                        if ( $last_nonblank_type eq 'F' && $next_tok ne '(' ) {
                            $type = 'C';
                        }
                    }
                }
            }

            ###############################################################
            # section 2: strings of digits
            ###############################################################
            elsif ( $pre_type eq 'd' ) {
                $expecting = operator_expected( $prev_type, $tok, $next_type );
                error_if_expecting_OPERATOR("Number")
                  if ( $expecting == OPERATOR );
                scan_number();
                if ( !defined($number) ) {

                    # shouldn't happen - we should always get a number
                    warning("non-number beginning with digit--program bug\n");
                    report_definite_bug();
                }
            }

            ###############################################################
            # section 3: all other tokens
            ###############################################################

            else {
                last if ( $tok eq '#' );
                my $code = $tokenization_code->{$tok};
                if ($code) {
                    $expecting =
                      operator_expected( $prev_type, $tok, $next_type );
                    $code->();
                    redo if $in_quote;
                }
            }
        }

        # -----------------------------
        # end of main tokenization loop 
        # -----------------------------

        if ( $i_tok >= 0 ) {
            $output_token_type[$i_tok]     = $type;
            $output_block_type[$i_tok]     = $block_type;
            $output_container_type[$i_tok] = $container_type;
            $output_type_sequence[$i_tok]  = $type_sequence;
        }

        unless ( ( $type eq 'b' ) || ( $type eq '#' ) ) {
            $last_last_nonblank_token          = $last_nonblank_token;
            $last_last_nonblank_type           = $last_nonblank_type;
            $last_last_nonblank_block_type     = $last_nonblank_block_type;
            $last_last_nonblank_container_type = $last_nonblank_container_type;
            $last_last_nonblank_type_sequence  = $last_nonblank_type_sequence;
            $last_nonblank_token               = $tok;
            $last_nonblank_type                = $type;
            $last_nonblank_block_type          = $block_type;
            $last_nonblank_container_type      = $container_type;
            $last_nonblank_type_sequence       = $type_sequence;
            $last_nonblank_prototype           = $prototype;
        }

        # reset indentation level if necessary at a sub or package
        # in an attempt to recover from a nesting error
        if ( $level_in_tokenizer < 0 ) {
            if ( $input_line =~ /^\s*(sub|package)\s+(\w+)/ ) {
                reset_indentation_level(0);
                brace_warning("resetting level to 0 at $1 $2\n");
            }
        }

        # all done tokenizing this line ... 
        # now prepare the final list of tokens and types

        my @token_type     = ();   # stack of output token types
        my @block_type     = ();   # stack of output code block types
        my @container_type = ();   # stack of output code container types
        my @type_sequence  = ();   # stack of output type sequence numbers
        my @tokens         = ();   # output tokens
        my @levels         = ();   # structural brace levels of output tokens
        my @slevels        = ();   # secondary nesting levels of output tokens
        my @nesting_tokens = ();   # string of tokens leading to this depth
        my @nesting_types  = ();   # string of token types leading to this depth
        my @nesting_blocks = ();   # string of block types leading to this depth
        my @nesting_lists  = ();   # string of list types leading to this depth
        my @ci_string = ();  # string needed to compute continuation indentation
        my @container_environment = ();    # BLOCK or LIST
        my $container_environment = '';
        my $im                    = -1;    # previous $i value
        my $num;
        my $ci_string_sum = ( $_ = $ci_string_in_tokenizer ) =~ tr/1/0/;

# =head1 Computing Token Indentation
# 
#     The final section of the tokenizer forms tokens and also computes
#     parameters needed to find indentation.  It is much easier to do it
#     in the tokenizer than elsewhere.  Here is a brief description of how
#     indentation is computed.  Perl::Tidy computes indentation as the sum
#     of 2 terms:
# 
#     (1) structural indentation, such as if/else/elsif blocks
#     (2) continuation indentation, such as long parameter call lists.
# 
#     These are occasionally called primary and secondary indentation.
# 
#     Structural indentation is introduced by tokens of type '{', although
#     the actual tokens might be '{', '(', or '['.  Structural indentation
#     is of two types: BLOCK and non-BLOCK.  Default structural indentation
#     is 4 characters if the standard indentation scheme is used.
# 
#     Continuation indentation is introduced whenever a line at BLOCK level
#     is broken before its termination.  Default continuation indentation
#     is 2 characters in the standard indentation scheme.
# 
#     Both types of indentation may be nested arbitrarily deep and
#     interlaced.  The distinction between the two is somewhat arbitrary.  
# 
#     For each token, we will define two variables which would apply if
#     the current statement were broken just before that token, so that
#     that token started a new line:
# 
#     $level = the structural indentation level,
#     $ci_level = the continuation indentation level 
# 
#     The total indentation will be $level * (4 spaces) + $ci_level * (2 spaces),
#     assuming defaults.  However, in some special cases it is customary
#     to modify $ci_level from this strict value.
# 
#     The total structural indentation is easy to compute by adding and
#     subtracting 1 from a saved value as types '{' and '}' are seen.  The
#     running value of this variable is $level_in_tokenizer.
# 
#     The total continuation is much more difficult to compute, and requires
#     several variables.  These veriables are:
# 
#     $ci_string_in_tokenizer = a string of 1's and 0's indicating, for
#       each indentation level, if there are intervening open secondary
#       structures just prior to that level.
#     $continuation_string_in_tokenizer = a string of 1's and 0's indicating
#       if the last token at that level is "continued", meaning that it
#       is not the first token of an expression.
#     $nesting_block_string = a string of 1's and 0's indicating, for each
#       indentation level, if the level is of type BLOCK or not.
#     $nesting_block_flag = the most recent 1 or 0 of $nesting_block_string
#     $nesting_list_string = a string of 1's and 0's indicating, for each
#       indentation level, if it is is appropriate for list formatting.
#       If so, continuation indentation is used to indent long list items.
#     $nesting_list_flag = the most recent 1 or 0 of $nesting_list_string
#     @slevel_stack = a stack of total nesting depths at each 
#       structural indentation level, where "total nesting depth" means
#       the nesting depth that would occur if every nesting token -- '{', '[',
#       and '(' -- , regardless of context, is used to compute a nesting
#       depth.
# 
# =cut

        #my $nesting_block_flag = ($nesting_block_string =~ /1$/); 
        #my $nesting_list_flag = ($nesting_list_string =~ /1$/); 

        my ( $ci_string_i, $level_i, $nesting_block_string_i,
            $nesting_list_string_i, $nesting_token_string_i,
            $nesting_type_string_i, );

        foreach $i (@output_token_list) {  # scan the list of pre-tokens indexes

            # self-checking for valid token types
            my $type = $output_token_type[$i];
            my $tok = $$rtokens[$i];   # the token, but ONLY if same as pretoken
            $level_i = $level_in_tokenizer;

            # This can happen by running perltidy on non-scripts
            # although it could also be bug introduced by programming change.
            # Perl silently accepts a 032 (^Z) and takes it as the end
            if ( !$is_valid_token_type{$type} ) {
                my $val = ord($type);
                warning(
                    "unexpected character decimal $val ($type) in script\n");
                $tokenizer_self->{_in_error} = 1;
            }

            # ----------------------------------------------------------------
            # TOKEN TYPE PATCHES
            #  output __END__, __DATA__, and format as type 'k' instead of ';'
            # to make html colors correct, etc.
            my $fix_type = $type;
            if ( $type eq ';' && $tok =~ /\w/ ) { $fix_type = 'k' }

            # output anonymous 'sub' as keyword
            if ( $type eq 't' && $tok eq 'sub' ) { $fix_type = 'k' }

            # -----------------------------------------------------------------

            $nesting_token_string_i = $nesting_token_string;
            $nesting_type_string_i  = $nesting_type_string;
            $nesting_block_string_i = $nesting_block_string;
            $nesting_list_string_i  = $nesting_list_string;

            # set primary indentation levels based on structural braces
            # Note: these are set so that the leading braces have a HIGHER
            # level than their CONTENTS, which is convenient for indentation
            # Also, define continuation indentation for each token.
            if ( $type eq '{' || $type eq 'L' ) {

                # use environment before updating
                $container_environment =
                  $nesting_block_flag ? 'BLOCK'
                  : $nesting_list_flag ? 'LIST'
                  : "";

                # if the difference between total nesting levels is not 1,
                # there are intervening non-structural nesting types between
                # this '{' and the previous unclosed '{'
                my $intervening_secondary_structure = 0;
                if (@slevel_stack) {
                    $intervening_secondary_structure =
                      $slevel_in_tokenizer - $slevel_stack[-1];
                }

# =head1 Continuation Indentation
# 
# Having tried setting continuation indentation both in the formatter and
# in the tokenizer, I can say that setting it in the tokenizer is much,
# much easier.  The formatter already has too much to do, and can't
# make decisions on line breaks without knowing what 'ci' will be at
# arbitrary locations.
# 
# But a problem with setting the continuation indentation (ci) here
# in the tokenizer is that we do not know where line breaks will actually
# be.  As a result, we don't know if we should propagate continuation
# indentation to higher levels of structure.  
# 
# For nesting of only structural indentation, we never need to do this.
# For example, in a long if statement, like this
# 
#   if ( !$output_block_type[$i] 
#     && ($in_statement_continuation) )
#   {           <--outdented
#       do_something();
#   }
# 
# the second line has ci but we do normally give the lines within the BLOCK
# any ci.  This would be true if we had blocks nested arbitrarily deeply.
# 
# But consider something like this, where we have created a break after
# an opening paren on line 1, and the paren is not (currently) a
# structural indentation token:
# 
# my $file = $menubar->Menubutton(
#   qw/-text File -underline 0 -menuitems/ => [
#       [
#           Cascade    => '~View',
#           -menuitems => [
#           ...
# 
# The second line has ci, so it would seem reasonable to propagate it
# down, giving the third line 1 ci + 1 indentation.  This suggests the
# following rule, which is currently used to propagating ci down: if there
# are any non-structural opening parens (or brackets, or braces), before
# an opening structural brace, then ci is propagated down, and otherwise
# not.  The variable $intervening_secondary_structure contains this
# information for the current token, and the string
# "$ci_string_in_tokenizer" is a stack of previous values of this
# variable.
# 
# =cut

                # save the current states
                push ( @slevel_stack, 1 + $slevel_in_tokenizer );
                $level_in_tokenizer++;

                if ( $output_block_type[$i] ) {
                    $nesting_block_flag = 1;
                    $nesting_block_string .= '1';
                }
                else {
                    $nesting_block_flag = 0;
                    $nesting_block_string .= '0';
                }

                # we will use continuation indentation within containers
                # which are not blocks and not logical expressions
                my $bit = 0;
                if ( !$output_block_type[$i] ) {

                    # propagate flag down at nested open parens
                    if ( $output_container_type[$i] eq '(' ) {
                        $bit = 1 if $nesting_list_flag;
                    }

                    # use list continuation if not a logical grouping
                    # /^(if|elsif|unless|while|and|or|not|&&|!|\|\||for|foreach)$/
                    else {
                        $bit = 1
                          unless
                          $is_logical_container{ $output_container_type[$i] };
                    }
                }
                $nesting_list_string .= $bit;
                $nesting_list_flag = $bit;

                $ci_string_in_tokenizer .=
                  ( $intervening_secondary_structure != 0 ) ? '1' : '0';
                $ci_string_sum = ( $_ = $ci_string_in_tokenizer ) =~ tr/1/0/;
                $continuation_string_in_tokenizer .=
                  ( $in_statement_continuation > 0 ) ? '1' : '0';

# =pod
# 
#  Sometimes we want to give an opening brace continuation indentation,
#  and sometimes not.  For code blocks, we don't do it, so that the leading
#  '{' gets outdented, like this:
# 
#   if ( !$output_block_type[$i] 
#     && ($in_statement_continuation) )
#   {           <--outdented
# 
#  For other types, we will give them continuation indentation.  For example,
#  here is how a list looks with the opening paren indented:
# 
#     @LoL =
#       ( [ "fred", "barney" ], [ "george", "jane", "elroy" ],
#         [ "homer", "marge", "bart" ], );
# 
#  This looks best when 'ci' is one-half of the indentation  (i.e., 2 and 4)
# 
# =cut

                my $total_ci = $ci_string_sum;
                if (
                    !$output_block_type[$i]    # patch: skip for BLOCK
                    && ($in_statement_continuation)
                  )
                {
                    $total_ci += $in_statement_continuation
                      unless ( $ci_string_in_tokenizer =~ /1$/ );
                }

                $ci_string_i               = $total_ci;
                $in_statement_continuation = 0;
            }

            elsif ( $type eq '}' || $type eq 'R' ) {

                # only a nesting error in the script would prevent popping here
                if ( @slevel_stack > 1 ) { pop (@slevel_stack); }

                $level_i = --$level_in_tokenizer;

                # restore previous level values
                if ( length($nesting_block_string) > 1 )
                {    # true for valid script
                    chop $nesting_block_string;
                    $nesting_block_flag = ( $nesting_block_string =~ /1$/ );
                    chop $nesting_list_string;
                    $nesting_list_flag = ( $nesting_list_string =~ /1$/ );

                    chop $ci_string_in_tokenizer;
                    $ci_string_sum =
                      ( $_ = $ci_string_in_tokenizer ) =~ tr/1/0/;

                    $in_statement_continuation =
                      chop $continuation_string_in_tokenizer;

                    # zero continuation flag at terminal BLOCK '}' which
                    # ends a statement.  
                    if ( $output_block_type[$i] ) {

                        # ...These include non-anonymous subs 
                        # note: could be sub ::abc { or sub 'abc
                        if ( $output_block_type[$i] =~ m/^sub\s*/gc ) {

                            # note: older versions of perl require the /gc modifier 
                            # here or else the \G does not work.
                            if ( $output_block_type[$i] =~ /\G('|::|\w)/gc ) {
                                $in_statement_continuation = 0;
                            }
                        }

                        # ...and include all block types except user subs with 
                        # block prototypes and these: (sort|grep|map|do|eval)
                        # /^(\}|\{|BEGIN|END|CHECK|INIT|AUTOLOAD|DESTROY|continue|;|if|elsif|else|unless|while|until|for|foreach)$/
                        elsif (
                            $is_zero_continuation_block_type{ $output_block_type
                                  [$i] } )
                        {
                            $in_statement_continuation = 0;
                        }

                        # ..but these are not terminal types:
                        #     /^(sort|grep|map|do|eval)$/ )
                        elsif (
                            $is_not_zero_continuation_block_type{
                                $output_block_type[$i] } )
                        {
                        }

                        # ..and a block introduced by a label
                        # /^\w+\s*:$/gc ) {
                        elsif ( $output_block_type[$i] =~ /:$/ ) {
                            $in_statement_continuation = 0;
                        }

                        # ..nor user function with block prototype
                        else {
                        }
                    }

                    # If we are in a list, then
                    # we must set continuatoin indentation at the closing
                    # paren of something like this (paren after $check):
                    #     assert(
                    #         __LINE__,
                    #         ( not defined $check )
                    #           or ref $check
                    #           or $check eq "new"
                    #           or $check eq "old",
                    #     );
                    elsif ( $tok eq ')' ) {
                        $in_statement_continuation = 1
                          if $output_container_type[$i] =~ /^[;,\{\}]$/;
                    }
                }

                # use environment after updating
                $container_environment =
                  $nesting_block_flag ? 'BLOCK'
                  : $nesting_list_flag ? 'LIST'
                  : "";
                $ci_string_i = $ci_string_sum + $in_statement_continuation;
                $nesting_block_string_i = $nesting_block_string;
                $nesting_list_string_i  = $nesting_list_string;
            }

            # not a structural indentation type..
            else {

                $container_environment =
                  $nesting_block_flag ? 'BLOCK'
                  : $nesting_list_flag ? 'LIST'
                  : "";

                # zero the continuation indentation at certain tokens so
                # that they will be at the same level as its container.  For
                # commas, this simplifies the -lp indentation logic, which
                # counts commas.  For ?: it makes them stand out.
                if ($nesting_list_flag) {
                    if ( $type =~ /^[,\?\:]$/ ) {
                        $in_statement_continuation = 0;
                    }
                }

                # be sure binary operators get continuation indentation
                if ( $is_binary_type{$type} && $container_environment ) {
                    $in_statement_continuation = 1;
                }

                # continuation indentation is sum of any open ci from previous
                # levels plus the current level
                $ci_string_i = $ci_string_sum + $in_statement_continuation;

                # update continuation flag ...
                # if this isn't a blank or comment..
                if ( $type ne 'b' && $type ne '#' ) {

                    # and we are in a BLOCK
                    if ($nesting_block_flag) {

                        # the next token after a ';' and label starts a new stmt
                        if ( $type eq ';' || $type eq 'J' ) {
                            $in_statement_continuation = 0;
                        }

                        # otherwise, we are continuing the current statement
                        else {
                            $in_statement_continuation = 1;
                        }
                    }

                    # if we are not in a BLOCK..
                    else {

                        # do not use continuation indentation if not list
                        # environment (could be within if/elsif clause)
                        if ( !$nesting_list_flag ) {
                            $in_statement_continuation = 0;
                        }

                        # otherwise, the next token after a ',' starts a new term
                        elsif ( $type eq ',' ) {
                            $in_statement_continuation = 0;
                        }

                        # otherwise, we are continuing the current term
                        else {
                            $in_statement_continuation = 1;
                        }
                    }
                }
            }

            if ( $level_in_tokenizer < 0 ) {
                unless ($saw_negative_indentation) {
                    $saw_negative_indentation = 1;
                    warning("Starting negative indentation\n");
                }
            }

            # set secondary nesting levels based on all continment token types
            # Note: these are set so that the nesting depth is the depth
            # of the PREVIOUS TOKEN, which is convenient for setting
            # the stength of token bonds
            my $slevel_i = $slevel_in_tokenizer;

            #    /^[L\{\(\[]$/ 
            if ( $is_opening_type{$type} ) {
                $slevel_in_tokenizer++;
                $nesting_token_string .= $tok;
                $nesting_type_string .= $type;
            }

            #       /^[R\}\)\]]$/ 
            elsif ( $is_closing_type{$type} ) {
                $slevel_in_tokenizer--;
                my $char = chop $nesting_token_string;

                if ( $char ne $matching_start_token{$tok} ) {
                    $nesting_token_string .= $char . $tok;
                    $nesting_type_string .= $type;
                }
                else {
                    chop $nesting_type_string;
                }
            }

            push ( @block_type,            $output_block_type[$i] );
            push ( @ci_string,             $ci_string_i );
            push ( @container_environment, $container_environment );
            push ( @container_type,        $output_container_type[$i] );
            push ( @levels,                $level_i );
            push ( @nesting_tokens,        $nesting_token_string_i );
            push ( @nesting_types,         $nesting_type_string_i );
            push ( @slevels,               $slevel_i );
            push ( @token_type,            $fix_type );
            push ( @type_sequence,         $output_type_sequence[$i] );
            push ( @nesting_blocks,        $nesting_block_string );
            push ( @nesting_lists,         $nesting_list_string );

            # now form the previous token
            if ( $im >= 0 ) {
                $num =
                  $$rtoken_map[$i] - $$rtoken_map[$im];    # how many characters

                if ( $num > 0 ) {
                    push ( @tokens,
                        substr( $input_line, $$rtoken_map[$im], $num ) );
                }
            }
            $im = $i;
        }

        $num = length($input_line) - $$rtoken_map[$im];    # make the last token
        if ( $num > 0 ) {
            push ( @tokens, substr( $input_line, $$rtoken_map[$im], $num ) );
        }

        $tokenizer_self->{_in_quote}          = $in_quote;
        $tokenizer_self->{_rhere_target_list} = \@here_target_list;

        $line_of_tokens->{_rtoken_type}            = \@token_type;
        $line_of_tokens->{_rtokens}                = \@tokens;
        $line_of_tokens->{_rblock_type}            = \@block_type;
        $line_of_tokens->{_rcontainer_type}        = \@container_type;
        $line_of_tokens->{_rcontainer_environment} = \@container_environment;
        $line_of_tokens->{_rtype_sequence}         = \@type_sequence;
        $line_of_tokens->{_rlevels}                = \@levels;
        $line_of_tokens->{_rslevels}               = \@slevels;
        $line_of_tokens->{_rnesting_tokens}        = \@nesting_tokens;
        $line_of_tokens->{_rci_levels}             = \@ci_string;
        $line_of_tokens->{_rnesting_blocks}        = \@nesting_blocks;

        return;
    }
}    # end tokenize_this_line

sub new_statement_ok {

    # return true if the current token can start a new statement

    return label_ok()    # a label would be ok here

      || $last_nonblank_type eq 'J';    # or we follow a label

}

sub label_ok {

    # Decide if a bare word followed by a colon here is a label

    # if it follows an opening or closing code block curly brace..
    if ( ( $last_nonblank_token eq '{' || $last_nonblank_token eq '}' )
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # it is a label if and only if the curly encloses a code block
        return $brace_type[$brace_depth];
    }

    # otherwise, it is a label if and only if it follows a ';' 
    else {
        return ( $last_nonblank_token eq ';' );
    }
}

sub code_block_type {

    # Decide if this is a block of code, and its type.
    # Must be called only when $type = $token = '{'
    # The problem is to distinguish between the start of a block of code
    # and the start of an anonymous hash reference
    # Returns "" if not code block, otherwise returns 'last_nonblank_token'
    # to indicate the type of code block.  (For example, 'last_nonblank_token'
    # might be 'if' for an if block, 'else' for an else block, etc).

    # handle case of multiple '{'s

    # print "BLOCK_TYPE EXAMINING: type=$last_nonblank_type tok=$last_nonblank_token\n"; 

    my ( $i, $rtokens ) = @_;
    if (   $last_nonblank_token eq '{'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # opening brace where a statement may appear is probably
        # a code block but might be and anonymous hash reference
        if ( $brace_type[$brace_depth] ) {
            return decide_if_code_block( $i, $rtokens );
        }

        # cannot start a code block within an anonymous hash
        else {
            return "";
        }
    }

    elsif ( $last_nonblank_token eq ';' ) {

        # an opening brace where a statement may appear is probably
        # a code block but might be and anonymous hash reference
        return decide_if_code_block( $i, $rtokens );
    }

    # handle case of '}{'
    elsif ($last_nonblank_token eq '}'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # a } { situation ...
        # could be hash reference after code block..(blktype1.t)
        if ($last_nonblank_block_type) {
            return decide_if_code_block( $i, $rtokens );
        }

        # must be a block if it follows a closing hash reference
        else {
            return $last_nonblank_token;
        }
    }

    # NOTE: braces after type characters start code blocks, but for
    # simplicity these are not identified as such.  See also 
    # sub is_non_structural_brace. 
    # elsif ( $last_nonblank_type eq 't' ) {
    #    return $last_nonblank_token;
    # }

    # brace after label:
    elsif ( $last_nonblank_type eq 'J' ) {
        return $last_nonblank_token;
    }

    # otherwise, look at previous token.  This must be a code block if
    # it follows any of these:
    # /^(BEGIN|END|CHECK|INIT|AUTOLOAD|DESTROY|continue|if|elsif|else|unless|do|while|until|eval|for|foreach|map|grep|sort)$/
    elsif ( $is_code_block_token{$last_nonblank_token} ) {
        return $last_nonblank_token;
    }

    # or a sub definition
    elsif ( ( $last_nonblank_type eq 'i' || $last_nonblank_type eq 't' )
        && $last_nonblank_token =~ /^sub\b/ )
    {
        return $last_nonblank_token;
    }

    # user-defined subs with block parameters (like grep/map/eval)
    elsif ( $last_nonblank_type eq 'G' ) {
        return $last_nonblank_token;
    }

    # anything else must be anonymous hash reference
    else {
        return "";
    }
}

sub decide_if_code_block {

    my ( $i, $rtokens ) = @_;
    my ( $next_nonblank_token, $i_next ) =
      find_next_nonblank_token( $i, $rtokens );

    # we are at a '{' where a statement may appear.  
    # We must decide if this brace starts an anonymous hash or a code
    # block.
    # return "" if anonymous hash, and $last_nonblank_token otherwise

    # initialize to be code BLOCK
    my $code_block_type = $last_nonblank_token;

    # Check for an empty anonymous hash reference:
    # Maybe something like sub { { } }
    if ( $next_nonblank_token eq '}' ) {
        $code_block_type = "";
    }

    # FIXME: coding incomplete

    return $code_block_type;
}

sub unexpected {

    # report unexpected token type and show where it is
    my ( $found, $expecting, $i_tok, $last_nonblank_i ) = @_;
    $unexpected_error_count++;
    if ( $unexpected_error_count <= MAX_NAG_MESSAGES ) {
        my $msg = "found $found where $expecting expected";
        my $pos = $$rpretoken_map[$i_tok];
        interrupt_logfile();
        my ( $offset, $numbered_line, $underline ) =
          make_numbered_line( $input_line_number, $input_line, $pos );
        $underline = write_on_underline( $underline, $pos - $offset, '^' );

        my $trailer = "";
        if ( ( $i_tok > 0 ) && ( $last_nonblank_i >= 0 ) ) {
            my $pos_prev = $$rpretoken_map[$last_nonblank_i];
            my $num;
            if ( $$rpretoken_type[ $i_tok - 1 ] eq 'b' ) {
                $num = $$rpretoken_map[ $i_tok - 1 ] - $pos_prev;
            }
            else {
                $num = $pos - $pos_prev;
            }
            if ( $num > 40 ) { $num = 40; $pos_prev = $pos - 40; }

            $underline =
              write_on_underline( $underline, $pos_prev - $offset, '-' x $num );
            $trailer = " (previous token underlined)";
        }
        warning( $numbered_line . "\n" );
        warning( $underline . "\n" );
        warning( $msg . $trailer . "\n" );
        resume_logfile();
    }
}

sub indicate_error {
    my ( $msg, $line_number, $input_line, $pos, $carrat ) = @_;
    interrupt_logfile();
    warning($msg);
    write_error_indicator_pair( $line_number, $input_line, $pos, $carrat );
    resume_logfile();
}

sub write_error_indicator_pair {
    my ( $line_number, $input_line, $pos, $carrat ) = @_;
    my ( $offset, $numbered_line, $underline ) =
      make_numbered_line( $line_number, $input_line, $pos );
    $underline = write_on_underline( $underline, $pos - $offset, $carrat );
    warning( $numbered_line . "\n" );
    $underline =~ s/\s*$//;
    warning( $underline . "\n" );
}

sub make_numbered_line {

    #  Given an input line, its line number, and a character position of
    #  interest, create a string not longer than 80 characters of the form
    #     $lineno: sub_string
    #  such that the sub_string of $str contains the position of interest
    # 
    #  Here is an example of what we want, in this case we add trailing
    #  '...' because the line is long.
    # 
    # 2: (One of QAML 2.0's authors is a member of the World Wide Web Con ...
    # 
    #  Here is another example, this time in which we used leading '...'
    #  because of excessive length:
    # 
    # 2: ... er of the World Wide Web Consortium's
    # 
    #  input parameters are:
    #   $lineno = line number
    #   $str = the text of the line
    #   $pos = position of interest (the error) : 0 = first character
    # 
    #   We return : 
    #     - $offset = an offset which corrects the position in case we only
    #       display part of a line, such that $pos-$offset is the effective
    #       position from the start of the displayed line.
    #     - $numbered_line = the numbered line as above, 
    #     - $underline = a blank 'underline' which is all spaces with the same
    #       number of characters as the numbered line.

    my ( $lineno, $str, $pos ) = @_;
    my $offset = ( $pos < 60 ) ? 0 : $pos - 40;
    my $excess = length($str) - $offset - 68;
    my $numc   = ( $excess > 0 ) ? 68 : undef;

    if ( defined($numc) ) {
        if ( $offset == 0 ) {
            $str = substr( $str, $offset, $numc - 4 ) . " ...";
        }
        else {
            $str = "... " . substr( $str, $offset + 4, $numc - 4 ) . " ...";
        }
    }
    else {

        if ( $offset == 0 ) {
        }
        else {
            $str = "... " . substr( $str, $offset + 4 );
        }
    }

    my $numbered_line = sprintf( "%d: ", $lineno );
    $offset -= length($numbered_line);
    $numbered_line .= $str;
    my $underline = " " x length($numbered_line);
    return ( $offset, $numbered_line, $underline );
}

sub write_on_underline {

    # The "underline" is a string that shows where an error is; it starts
    # out as a string of blanks with the same length as the numbered line of 
    # code above it, and we have to add marking to show where an error is.
    # In the example below, we want to write the string '--^' just below
    # the line of bad code:
    # 
    # 2: (One of QAML 2.0's authors is a member of the World Wide Web Con ...
    #                 ---^                                                   
    # We are given the current underline string, plus a position and a
    # string to write on it.
    # 
    # In the above example, there will be 2 calls to do this:
    # First call:  $pos=19, pos_chr=^
    # Second call: $pos=16, pos_chr=---
    # 
    # This is a trivial thing to do with substr, but there is some
    # checking to do.

    my ( $underline, $pos, $pos_chr ) = @_;

    # check for error..shouldn't happen
    unless ( ( $pos >= 0 ) && ( $pos <= length($underline) ) ) {
        return $underline;
    }
    my $excess = length($pos_chr) + $pos - length($underline);
    if ( $excess > 0 ) {
        $pos_chr = substr( $pos_chr, 0, length($pos_chr) - $excess );
    }
    substr( $underline, $pos, length($pos_chr) ) = $pos_chr;
    return ($underline);
}

sub is_non_structural_brace {

    # Decide if a brace or bracket is structural or non-structural
    # by looking at the previous token and type

    # EXPERIMENTAL: Mark slices as structural; idea was to improve formatting.
    # Tentatively deactivated because it caused the wrong operator expectation
    # for this code:
    #      $user = @vars[1] / 100;
    # Must update sub operator_expected before re-implementing.
    # if ( $last_nonblank_type eq 'i' && $last_nonblank_token =~ /^@/ ) {
    #    return 0;
    # }

    # NOTE: braces after type characters start code blocks, but for
    # simplicity these are not identified as such.  See also 
    # sub code_block_type
    # if ($last_nonblank_type eq 't') {return 0} 

    # otherwise, it is non-structural if it is decorated 
    # by type information.
    # For example, the '{' here is non-structural:   ${xxx}
    (
        $last_nonblank_token =~ /^([\$\@\*\&\%\)]|->|::)/

          # or if we follow a hash or array closing curly brace or bracket
          # For example, the second '{' in this is non-structural: $a{'x'}{'y'}
          # because the first '}' would have been given type 'R'
          || $last_nonblank_type =~ /^([R\]])$/
    );
}

sub operator_expected {

    # Many perl symbols have two or more meanings.  For example, '<<'
    # can be a shift operator or a here-doc operator.  The
    # interpretation of these symbols depends on the current state of
    # the tokenizer, which may either be expecting a term or an
    # operator.  For this example, a << would be a shift if an operator
    # is expected, and a here-doc if a term is expected.  This routine
    # is called to make this decision for any current token.  It returns
    # one of three possible values:
    # 
    #     OPERATOR - operator expected (or at least, not a term)
    #     UNKNOWN  - can't tell  
    #     TERM     - a term is expected (or at least, not an operator)
    # 
    # The decision is based on what has been seen so far.  This
    # information is stored in the "$last_nonblank_type" and
    # "$last_nonblank_token" variables.  For example, if the
    # $last_nonblank_type is '=~', then we are expecting a TERM, whereas
    # if $last_nonblank_type is 'n' (numeric), we are expecting an
    # OPERATOR. 
    # 
    # If a UNKNOWN is returned, the calling routine must guess. A major
    # goal of this tokenizer is to minimize the possiblity of returning
    # UNKNOWN, because a wrong guess can spoil the formatting of a
    # script.
    # 
    # adding NEW_TOKENS: it is critically important that this routine be
    # updated to allow it to determine if an operator or term is to be
    # expected after the new token.  Doing this simply involves adding
    # the new token character to one of the regexes in this routine or
    # to one of the hash lists
    # that it uses, which are initialized in the BEGIN section.

    my ( $prev_type, $tok, $next_type ) = @_;
    my $op_expected = UNKNOWN;

    # Note: function prototype is available for token type 'U' for future 
    # program development.  It contains the leading and trailing parens,
    # and no blanks.  It might be used to eliminate token type 'C', for
    # example (prototype = '()'). Thus:
    # if ($last_nonblank_type eq 'U') {
    #     print "previous token=$last_nonblank_token  type=$last_nonblank_type prototype=$last_nonblank_prototype\n";
    # }

    # A possible filehandle (or object) requires some care...
    if ( $last_nonblank_type eq 'Z' ) {

        # angle.t
        if ( $last_nonblank_token =~ /^[A-Za-z_]/ ) {
            $op_expected = UNKNOWN;
        }

        # For possible file handle like "$a", Perl uses weird parsing rules.
        # For example:
        # print $a/2,"/hi";   - division
        # print $a / 2,"/hi"; - division
        # print $a/ 2,"/hi";  - division
        # print $a /2,"/hi";  - pattern (and error)!
        elsif ( ( $prev_type eq 'b' ) && ( $next_type ne 'b' ) ) {
            $op_expected = TERM;
        }

        # Note when an operation is being done where a
        # filehandle might be expected, since a change in whitespace
        # could change the interpretation of the statement.
        else {
            if ( $tok =~ /^([x\/\+\-\*\%\&\.\?\<]|\>\>)$/ ) {
                complain("operator in print statement not recommended\n");
                $op_expected = OPERATOR;
            }
        }
    }

    # handle something after 'do' and 'eval'
    elsif ( $is_block_operator{$last_nonblank_token} ) {

        # something like $a = eval "expression";
        #                          ^             
        if ( $last_nonblank_type eq 'k' ) {
            $op_expected = TERM;    # expression or list mode following keyword
        }

        # something like $a = do { BLOCK } / 2;
        #                                  ^             
        else {
            $op_expected = OPERATOR;    # block mode following }
        }
    }

    # handle bare word..
    elsif ( $last_nonblank_type eq 'w' ) {

        # unfortunately, we can't tell what type of token to expect next
        # after most bare words
        $op_expected = UNKNOWN;
    }

    # operator, but not term possible after these types
    # Note: moved ')' from type to token because parens in list context
    # get marked as '{' '}' now.  This is a minor glitch in the following:
    #    my %opts = (ref $_[0] eq 'HASH') ? %{shift()} : ();
    #
    elsif (( $last_nonblank_type =~ /^[\]RnviQh]$/ )
        || ( $last_nonblank_token =~ /^(\)|\$|\-\>)/ ) )
    {
        $op_expected = OPERATOR;

        # in a 'use' statement, numbers and v-strings are not really
        # numbers, so to avoid incorrect error messages, we will
        # mark them as unknown for now (use.t)
        if (   ( $statement_type eq 'use' )
            && ( $last_nonblank_type =~ /^[nv]$/ ) )
        {
            $op_expected = UNKNOWN;
        }
    }

    # no operator after many keywords, such as "die", "warn", etc
    elsif ( $expecting_term_token{$last_nonblank_token} ) {
        $op_expected = TERM;
    }

    # no operator after things like + - **  (i.e., other operators)
    elsif ( $expecting_term_types{$last_nonblank_type} ) {
        $op_expected = TERM;
    }

    # a few operators, like "time", have an empty prototype () and so
    # take no parameters but produce a value to operate on
    elsif ( $expecting_operator_token{$last_nonblank_token} ) {
        $op_expected = OPERATOR;
    }

    # post-increment and decrement produce values to be operated on
    elsif ( $expecting_operator_types{$last_nonblank_type} ) {
        $op_expected = OPERATOR;
    }

    # no value to operate on after sub block
    elsif ( $last_nonblank_token =~ /^sub\s/ ) { $op_expected = TERM; }

    # a right brace here indicates the end of a simple block.
    # all non-structural right braces have type 'R'
    # all braces associated with block operator keywords have been given those
    # keywords as "last_nonblank_token" and caught above.
    # (This statement is order dependent, and must come after checking
    # $last_nonblank_token).
    elsif ( $last_nonblank_type eq '}' ) {
        $op_expected = TERM;
    }

    # something else..what did I forget?
    else {

        # collecting diagnostics on unknown operator types..see what was missed
        $op_expected = UNKNOWN;
        write_diagnostics(
"OP: unknown after type=$last_nonblank_type  token=$last_nonblank_token\n"
        );
    }

    TOKENIZER_DEBUG_FLAG_EXPECT && do {
        print
"EXPECT: returns $op_expected for last type $last_nonblank_type token $last_nonblank_token\n";
    };
    return $op_expected;
}

# The following routines keep track of nesting depths of the nesting
# types, ( [ { and ?.  This is necessary for determining the indentation
# level, and also for debugging programs.  Not only do they keep track of
# nesting depths of the individual brace types, but they check that each
# of the other brace types is balanced within matching pairs.  For
# example, if the program sees this sequence:
# 
#         {  ( ( ) }
# 
# then it can determine that there is an extra left paren somewhere
# between the { and the }.  And so on with every other possible
# combination of outer and inner brace types.  For another
# example:
# 
#         ( [ ..... ]  ] )
# 
# which has an extra ] within the parens.  
# 
# The brace types have indexes 0 .. 3 which are indexes into
# the matrices.
# 
# The pair ? : are treated as just another nesting type, with ? acting
# as the opening brace and : acting as the closing brace.
# 
# The matrix 
# 
#         $depth_array[$a][$b][ $current_depth[$a] ] = $current_depth[$b];
# 
# saves the nesting depth of brace type $b (where $b is either of the other
# nesting types) when brace type $a enters a new depth.  When this depth
# decreases, a check is made that the current depth of brace types $b is
# unchanged, or otherwise there must have been an error.  This can
# be very useful for localizing errors, particularly when perl runs to
# the end of a large file (such as this one) and announces that there
# is a problem somewhere.
# 
# A numerical sequence number is maintained for every nesting type,
# so that each matching pair can be uniquely identified in a simple
# way.

sub increase_nesting_depth {
    my ( $a, $i_tok ) = @_;
    my $b;
    $current_depth[$a]++;

    # Sequence numbers increment by number of items.  This keeps
    # a unique set of numbers but still allows the relative location
    # of any type to be determined.
    $nesting_sequence_number[$a] += scalar(@closing_brace_names);
    my $seqno = $nesting_sequence_number[$a];
    $current_sequence_number[$a][ $current_depth[$a] ] = $seqno;

    my $pos = $$rpretoken_map[$i_tok];
    $starting_line_of_current_depth[$a][ $current_depth[$a] ] =
      [ $input_line_number, $input_line, $pos ];

    for $b ( 0 .. $#closing_brace_names ) {
        next if ( $b == $a );
        $depth_array[$a][$b][ $current_depth[$a] ] = $current_depth[$b];
    }
    return $seqno;
}

sub decrease_nesting_depth {

    my ( $a, $i_tok ) = @_;
    my $pos = $$rpretoken_map[$i_tok];
    my $b;
    my $seqno = 0;

    if ( $current_depth[$a] > 0 ) {

        $seqno = $current_sequence_number[$a][ $current_depth[$a] ];

        # check that any brace types $b contained within are balanced
        for $b ( 0 .. $#closing_brace_names ) {
            next if ( $b == $a );

            unless ( $depth_array[$a][$b][ $current_depth[$a] ] ==
                $current_depth[$b] )
            {
                my $diff = $current_depth[$b] -
                  $depth_array[$a][$b][ $current_depth[$a] ];

                # don't whine too many times
                my $saw_brace_error = get_saw_brace_error();
                if (
                    $saw_brace_error <= MAX_NAG_MESSAGES

                    # if too many closing types have occured, we probably
                    # already caught this error
                    && ( ( $diff > 0 ) || ( $saw_brace_error <= 0 ) )
                  )
                {
                    interrupt_logfile();
                    my $rsl =
                      $starting_line_of_current_depth[$a][ $current_depth[$a] ];
                    my $sl  = $$rsl[0];
                    my $rel = [ $input_line_number, $input_line, $pos ];
                    my $el  = $$rel[0];
                    my ($ess);

                    if ( $diff == 1 || $diff == -1 ) {
                        $ess = '';
                    }
                    else {
                        $ess = 's';
                    }
                    my $bname =
                      ( $diff > 0 )
                      ? $opening_brace_names[$b]
                      : $closing_brace_names[$b];
                    write_error_indicator_pair( @$rsl, '^' );
                    my $msg = <<"EOM";
Found $diff extra $bname$ess between $opening_brace_names[$a] on line $sl and $closing_brace_names[$a] on line $el
EOM

                    if ( $diff > 0 ) {
                        my $rml =
                          $starting_line_of_current_depth[$b]
                          [ $current_depth[$b] ];
                        my $ml = $$rml[0];
                        $msg .=
"    The most recent un-matched $bname is on line $ml\n";
                        write_error_indicator_pair( @$rml, '^' );
                    }
                    write_error_indicator_pair( @$rel, '^' );
                    warning($msg);
                    resume_logfile();
                }
                increment_brace_error();
            }
        }
        $current_depth[$a]--;
    }
    else {

        my $saw_brace_error = get_saw_brace_error();
        if ( $saw_brace_error <= MAX_NAG_MESSAGES ) {
            my $msg = <<"EOM";
There is no previous $opening_brace_names[$a] to match a $closing_brace_names[$a] on line $input_line_number
EOM
            indicate_error( $msg, $input_line_number, $input_line, $pos, '^' );
        }
        increment_brace_error();
    }
    return $seqno;
}

sub check_final_nesting_depths {
    my ($a);

    for $a ( 0 .. $#closing_brace_names ) {

        if ( $current_depth[$a] ) {
            my $rsl = $starting_line_of_current_depth[$a][ $current_depth[$a] ];
            my $sl  = $$rsl[0];
            my $msg = <<"EOM";
Final nesting depth of $opening_brace_names[$a]s is $current_depth[$a]
The most recent un-matched $opening_brace_names[$a] is on line $sl
EOM
            indicate_error( $msg, @$rsl, '^' );
            increment_brace_error();
        }
    }
}

sub numerator_expected {

    # this is a filter for a possible numerator, in support of guessing
    # for the / pattern delimiter token.
    # returns -
    #   1 - yes
    #   0 - can't tell
    #  -1 - no
    # Note: I am using the convention that variables ending in
    # _expected have these 3 possible values.
    my ( $i, $rtokens ) = @_;
    my $next_token = $$rtokens[ $i + 1 ];
    if ( $next_token eq '=' ) { $i++; }    # handle /=
    my ( $next_nonblank_token, $i_next ) =
      find_next_nonblank_token( $i, $rtokens );

    if ( $next_nonblank_token =~ /(\(|\$|\w|\.|\@)/ ) {
        1;
    }
    else {

        if ( $next_nonblank_token =~ /^\s*$/ ) {
            0;
        }
        else {
            -1;
        }
    }
}

sub pattern_expected {

    # This is the start of a filter for a possible pattern.
    # It looks at the token after a possbible pattern and tries to
    # determine if that token could end a pattern.
    # returns -
    #   1 - yes
    #   0 - can't tell
    #  -1 - no
    my ( $i, $rtokens ) = @_;
    my $next_token = $$rtokens[ $i + 1 ];
    if ( $next_token =~ /^[cgimosx]/ ) { $i++; }    # skip possible modifier
    my ( $next_nonblank_token, $i_next ) =
      find_next_nonblank_token( $i, $rtokens );

    # list of tokens which may follow a pattern
    # (can probably be expanded)
    if ( $next_nonblank_token =~ /(\)|\}|\;|\&\&|\|\||and|or|while|if|unless)/ )
    {
        1;
    }
    else {

        if ( $next_nonblank_token =~ /^\s*$/ ) {
            0;
        }
        else {
            -1;
        }
    }
}

sub find_next_nonblank_token_on_this_line {
    my ( $i, $rtokens ) = @_;
    my $next_nonblank_token;

    if ( $i < $max_token_index ) {
        $next_nonblank_token = $$rtokens[ ++$i ];

        if ( $next_nonblank_token =~ /^\s*$/ ) {

            if ( $i < $max_token_index ) {
                $next_nonblank_token = $$rtokens[ ++$i ];
            }
        }
    }
    else {
        $next_nonblank_token = "";
    }
    return ( $next_nonblank_token, $i );
}

sub find_next_nonblank_token {
    my ( $i, $rtokens ) = @_;

    if ( $i >= $max_token_index ) {

        if ( !$peeked_ahead ) {
            $peeked_ahead = 1;
            $rtokens      = peek_ahead_for_nonblank_token($rtokens);
        }
    }
    my $next_nonblank_token = $$rtokens[ ++$i ];

    if ( $next_nonblank_token =~ /^\s*$/ ) {
        $next_nonblank_token = $$rtokens[ ++$i ];
    }
    return ( $next_nonblank_token, $i );
}

sub peek_ahead_for_n_nonblank_pre_tokens {

    # returns next n pretokens if they exist
    # returns undef's if hits eof without seeing any pretokens
    my $max_pretokens = shift;
    my $line;
    my $i = 0;
    my ( $rpre_tokens, $rmap, $rpre_types );

    while ( $line = $tokenizer_self->{_line_buffer_object}->peek_ahead( $i++ ) )
    {
        $line =~ s/^\s*//;    # trim leading blanks
        next if ( length($line) <= 0 );    # skip blank
        next if ( $line =~ /^#/ );         # skip comment
        ( $rpre_tokens, $rmap, $rpre_types ) =
          pre_tokenize( $line, $max_pretokens );
        last;
    }
    return ( $rpre_tokens, $rpre_types );
}

# look ahead for next non-blank, non-comment line of code
sub peek_ahead_for_nonblank_token {
    my $rtokens = shift;
    my $line;
    my $i = 0;

    while ( $line = $tokenizer_self->{_line_buffer_object}->peek_ahead( $i++ ) )
    {
        $line =~ s/^\s*//;    # trim leading blanks
        next if ( length($line) <= 0 );    # skip blank
        next if ( $line =~ /^#/ );         # skip comment
        my ( $rtok, $rmap, $rtype ) =
          pre_tokenize( $line, 2 );        # only need 2 pre-tokens
        my $j = $max_token_index + 1;
        my $tok;

        foreach $tok (@$rtok) {
            last if ( $tok =~ "\n" );
            $$rtokens[ ++$j ] = $tok;
        }
        last;
    }
    return $rtokens;
}

sub pre_tokenize {

    # Break a string, $str, into a sequence of preliminary tokens.  We
    # are interested in these types of tokens: 
    #   words       (type='w'),            example: 'max_tokens_wanted'
    #   digits      (type = 'd'),          example: '0755'
    #   whitespace  (type = 'b'),          example: '   '
    #   any other single character (i.e. punct; type = the character itself).  
    # We cannot do better than this yet because we might be in a quoted
    # string or pattern.  Caller sets $max_tokens_wanted to 0 to get all
    # tokens.
    my ( $str, $max_tokens_wanted ) = @_;

    # we return references to these 3 arrays:
    my @tokens    = ();     # array of the tokens themselves
    my @token_map = (0);    # string position of start of each token
    my @type      = ();     # 'b'=whitespace, 'd'=digits, 'w'=alpha, or punct

    do {

        # whitespace
        if ( $str =~ /\G(\s+)/gc ) { push @type, 'b'; }

        # numbers
        # note that this must come before words!
        elsif ( $str =~ /\G(\d+)/gc ) { push @type, 'd'; }

        # words
        elsif ( $str =~ /\G(\w+)/gc ) { push @type, 'w'; }

        # single-character punctuation
        elsif ( $str =~ /\G(\W)/gc ) { push @type, $1; }

        # that's all..
        else {
            return ( \@tokens, \@token_map, \@type );
        }

        push @tokens,    $1;
        push @token_map, pos($str);

    } while ( --$max_tokens_wanted != 0 );

    return ( \@tokens, \@token_map, \@type );
}

sub show_tokens {

    # this is an old debug routine
    my ( $rtokens, $rtoken_map ) = @_;
    my $num = scalar(@$rtokens);
    my $i;

    for ( $i = 0 ; $i < $num ; $i++ ) {
        my $len = length( $$rtokens[$i] );
        print "$i:$len:$$rtoken_map[$i]:$$rtokens[$i]:\n";
    }
}

sub find_angle_operator_termination {

    # We are looking at a '<' and want to know if it is an angle operator.
    # We are to return:
    #   $i = pretoken index of ending '>' if found, current $i otherwise
    #   $type = 'Q' if found, '>' otherwise
    my ( $input_line, $i_beg, $rtoken_map, $expecting ) = @_;
    my $i    = $i_beg;
    my $type = '<';
    pos($input_line) = 1 + $$rtoken_map[$i];

    my $filter;

    # we just have to find the next '>' if a term is expected
    if ( $expecting == TERM ) { $filter = '[\>]' }

    # we have to guess if we don't know what is expected
    elsif ( $expecting == UNKNOWN ) { $filter = '[\>\;\=\#\|\<]' }

    # shouldn't happen - we shouldn't be here if operator is expected
    else { warning("Program Bug in find_angle_operator_termination\n") }

    # To illustrate what we might be looking at, in case we are
    # guessing, here are some examples of valid angle operators
    # (or file globs):
    #  <tmp_imp/*>
    #  <FH>
    #  <$fh>
    #  <*.c *.h>
    #  <_>
    #  <jskdfjskdfj* op/* jskdjfjkosvk*> ( glob.t)
    #  <${PREFIX}*img*.$IMAGE_TYPE>
    #  <img*.$IMAGE_TYPE>
    #  <Timg*.$IMAGE_TYPE>
    #  <$LATEX2HTMLVERSIONS${dd}html[1-9].[0-9].pl>
    #
    # Here are some examples of lines which do not have angle operators:
    #  return undef unless $self->[2]++ < $#{$self->[1]};
    #  < 2  || @$t >
    #
    # the following line from dlister.pl caused trouble:
    #  print'~'x79,"\n",$D<1024?"0.$D":$D>>10,"K, $C files\n\n\n";
    #
    # If the '<' starts an angle operator, it must end on this line and
    # it must not have certain characters like ';' and '=' in it.  I use
    # this to limit the testing.  This filter should be improved if
    # possible.

    if ( $input_line =~ /($filter)/g ) {

        if ( $1 eq '>' ) {

            # We MAY have found an angle operator termination if we get
            # here, but we need to do more to be sure we haven't been
            # fooled.
            my $pos = pos($input_line);

            my $pos_beg = $$rtoken_map[$i];
            my $str     = substr( $input_line, $pos_beg, ( $pos - $pos_beg ) );

            ######################################debug#####
            #write_diagnostics( "ANGLE? :$str\n");
            #print "ANGLE: found $1 at pos=$pos\n";
            ######################################debug#####
            $type = 'Q';
            my $error;
            ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map );

            # It may be possible that a quote ends midway in a pretoken.
            # If this happens, it may be necessary to split the pretoken.
            if ($error) {
                warning(
                    "Possible tokinization error..please check this line\n");
                report_possible_bug();
            }

            # Now let's see where we stand....
            # OK if math op not possible
            if ( $expecting == TERM ) {
            }

            # OK if there are no more than 2 pre-tokens inside
            # (not possible to write 2 token math between < and >)
            # This catches most common cases
            elsif ( $i <= $i_beg + 3 ) {
                write_diagnostics("ANGLE(1 or 2 tokens): $str\n");
            }

            # Not sure..
            else {

                # Let's try a Brace Test: any braces inside must balance
                my $br = 0;
                while ( $str =~ /\{/g ) { $br++ }
                while ( $str =~ /\}/g ) { $br-- }
                my $sb = 0;
                while ( $str =~ /\[/g ) { $sb++ }
                while ( $str =~ /\]/g ) { $sb-- }
                my $pr = 0;
                while ( $str =~ /\(/g ) { $pr++ }
                while ( $str =~ /\)/g ) { $pr-- }

                # if braces do not balance - not angle operator
                if ( $br || $sb || $pr ) {
                    $i    = $i_beg;
                    $type = '<';
                    write_diagnostics(
                        "NOT ANGLE (BRACE={$br ($pr [$sb ):$str\n");
                }

                # we should keep doing more checks here...to be continued
                # Tentatively accepting this as a valid angle operator.
                # There are lots more things that can be checked.
                else {
                    write_diagnostics(
                        "ANGLE-Guessing yes: $str expecting=$expecting\n");
                    write_logfile_entry("Guessing angle operator here: $str\n");
                }
            }
        }

        # didn't find ending >
        else {
            if ( $expecting == TERM ) {
                warning("No ending > for angle operator\n");
            }
        }
    }
    return ( $i, $type );
}

sub inverse_pretoken_map {

    # Starting with the current pre_token index $i, scan forward until
    # finding the index of the next pre_token whose position is $pos.
    my ( $i, $pos, $rtoken_map ) = @_;
    my $error = 0;

    while ( ++$i <= $max_token_index ) {

        if ( $pos <= $$rtoken_map[$i] ) {

            # Let the calling routine handle errors in which we do not
            # land on a pre-token boundary.  It can happen by running
            # perltidy on some non-perl scripts, for example.
            if ( $pos < $$rtoken_map[$i] ) { $error = 1 }
            $i--;
            last;
        }
    }
    return ( $i, $error );
}

sub guess_if_pattern_or_conditional {

    # this routine is called when we have encountered a ? following an
    # unknown bareword, and we must decide if it starts a pattern or not
    # input parameters:
    #   $i - token index of the ? starting possible pattern
    # output parameters:
    #   $is_pattern = 0 if probably not pattern,  =1 if probably a pattern
    #   msg = a warning or diagnostic message
    my ( $i, $rtokens, $rtoken_map ) = @_;
    my $is_pattern = 0;
    my $msg        = "guessing that ? after $last_nonblank_token starts a ";

    if ( $i >= $max_token_index ) {
        $msg .= "conditional (no end to pattern found on the line)\n";
    }
    else {
        my $ibeg = $i;
        $i = $ibeg + 1;
        my $next_token = $$rtokens[$i];    # first token after ?

        # look for a possible ending ? on this line..
        my $in_quote        = 1;
        my $quote_depth     = 0;
        my $quote_character = '';
        my $quote_pos       = 0;
        ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth ) =
          follow_quoted_string( $ibeg, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth );

        if ($in_quote) {

            # we didn't find an ending ? on this line, 
            # so we bias towards conditional
            $is_pattern = 0;
            $msg .= "conditional (no ending ? on this line)\n";

            # we found an ending ?, so we bias towards a pattern
        }
        else {

            if ( pattern_expected( $i, $rtokens ) >= 0 ) {
                $is_pattern = 1;
                $msg .= "pattern (found ending ? and pattern expected)\n";
            }
            else {
                $msg .= "pattern (uncertain, but found ending ?)\n";
            }
        }
    }
    return ( $is_pattern, $msg );
}

sub guess_if_pattern_or_division {

    # this routine is called when we have encountered a / following an
    # unknown bareword, and we must decide if it starts a pattern or is a
    # division
    # input parameters:
    #   $i - token index of the / starting possible pattern
    # output parameters:
    #   $is_pattern = 0 if probably division,  =1 if probably a pattern
    #   msg = a warning or diagnostic message
    my ( $i, $rtokens, $rtoken_map ) = @_;
    my $is_pattern = 0;
    my $msg        = "guessing that / after $last_nonblank_token starts a ";

    if ( $i >= $max_token_index ) {
        "division (no end to pattern found on the line)\n";
    }
    else {
        my $ibeg = $i;
        my $divide_expected = numerator_expected( $i, $rtokens );
        $i = $ibeg + 1;
        my $next_token = $$rtokens[$i];    # first token after slash

        # look for a possible ending / on this line..
        my $in_quote        = 1;
        my $quote_depth     = 0;
        my $quote_character = '';
        my $quote_pos       = 0;
        ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth ) =
          follow_quoted_string( $ibeg, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth );

        if ($in_quote) {

            # we didn't find an ending / on this line, 
            # so we bias towards division
            if ( $divide_expected >= 0 ) {
                $is_pattern = 0;
                $msg .= "division (no ending / on this line)\n";
            }
            else {
                $msg        = "multi-line pattern (division not possible)\n";
                $is_pattern = 1;
            }

        }

        # we found an ending /, so we bias towards a pattern
        else {

            if ( pattern_expected( $i, $rtokens ) >= 0 ) {

                if ( $divide_expected >= 0 ) {

                    if ( $i - $ibeg > 60 ) {
                        $msg .= "division (matching / too distant)\n";
                        $is_pattern = 0;
                    }
                    else {
                        $msg .= "pattern (but division possible too)\n";
                        $is_pattern = 1;
                    }
                }
                else {
                    $is_pattern = 1;
                    $msg .= "pattern (division not possible)\n";
                }
            }
            else {

                if ( $divide_expected >= 0 ) {
                    $is_pattern = 0;
                    $msg .= "division (pattern not possible)\n";
                }
                else {
                    $is_pattern = 1;
                    $msg .=
                      "pattern (uncertain, but division would not work here)\n";
                }
            }
        }
    }
    return ( $is_pattern, $msg );
}

sub find_here_doc {

    # find the target of a here document, if any
    # input parameters:
    #   $i - token index of the second < of <<
    #   ($i must be less than the last token index if this is called)
    # output parameters:
    #   $found_target = 0 didn't find target; =1 found target
    #   HERE_TARGET - the target string (may be empty string)
    #   $i - unchanged if not here doc, 
    #    or index of the last token of the here target
    my ( $expecting, $i, $rtokens, $rtoken_map ) = @_;
    my $ibeg                 = $i;
    my $found_target         = 0;
    my $here_doc_target      = '';
    my $here_quote_character = '';
    my ( $next_nonblank_token, $i_next_nonblank, $next_token );
    $next_token = $$rtokens[ $i + 1 ];

    # perl allows a backslash before the target string (heredoc.t)
    my $backslash = 0;
    if ( $next_token eq '\\' ) {
        $backslash  = 1;
        $next_token = $$rtokens[ $i + 2 ];
    }

    ( $next_nonblank_token, $i_next_nonblank ) =
      find_next_nonblank_token_on_this_line( $i, $rtokens );

    if ( $next_nonblank_token =~ /[\'\"\`]/ ) {

        my $in_quote    = 1;
        my $quote_depth = 0;
        my $quote_pos   = 0;

        ( $i, $in_quote, $here_quote_character, $quote_pos, $quote_depth ) =
          follow_quoted_string( $i_next_nonblank, $in_quote, $rtokens,
            $here_quote_character, $quote_pos, $quote_depth );

        if ($in_quote) {    # didn't find end of quote, so no target found
            $i = $ibeg;
        }
        else {              # found ending quote
            my $j;
            $found_target = 1;

            my $tokj;
            for ( $j = $i_next_nonblank + 1 ; $j < $i ; $j++ ) {
                $tokj = $$rtokens[$j];

                # we have to remove any backslash before the quote character
                # so that the here-doc-target exactly matches this string
                next
                  if ( $tokj eq "\\"
                    && $j < $i - 1
                    && $$rtokens[ $j + 1 ] eq $here_quote_character );
                $here_doc_target .= $tokj;
            }
        }
    }

    elsif ( ( $next_token =~ /^\s*$/ ) and ( $expecting == TERM ) ) {
        $found_target = 1;
        write_logfile_entry(
            "found blank here-target after <<; suggest using \"\"\n");
        $i = $ibeg;
    }
    elsif ( $next_token =~ /^\w/ ) {    # simple bareword or integer after <<

        my $here_doc_expected;
        if ( $expecting == UNKNOWN ) {
            $here_doc_expected = guess_if_here_doc($next_token);
        }
        else {
            $here_doc_expected = 1;
        }

        if ($here_doc_expected) {
            $found_target    = 1;
            $here_doc_target = $next_token;
            $i               = $ibeg + 1;
        }

    }
    else {

        if ( $expecting == TERM ) {
            $found_target = 1;
            write_logfile_entry("Note: bare here-doc operator <<\n");
        }
        else {
            $i = $ibeg;
        }
    }

    # patch to neglect any prepended backslash
    if ( $found_target && $backslash ) { $i++ }

    return ( $found_target, $here_doc_target, $here_quote_character, $i );
}

# try to resolve here-doc vs. shift by looking ahead for
# non-code or the end token (currently only looks for end token)
# returns 1 if it is probably a here doc, 0 if not
sub guess_if_here_doc {

    # This is how many lines we will search for a target as part of the
    # guessing strategy.  It is a constant because there is probably
    # little reason to change it.
    use constant HERE_DOC_WINDOW => 40;

    my $next_token        = shift;
    my $here_doc_expected = 0;
    my $line;
    my $k   = 0;
    my $msg = "checking <<";

    while ( $line = $tokenizer_self->{_line_buffer_object}->peek_ahead( $k++ ) )
    {
        chomp $line;

        if ( $line =~ /^$next_token$/ ) {
            $msg .= " -- found target $next_token ahead $k lines\n";
            $here_doc_expected = 1;    # got it
            last;
        }
        last if ( $k >= HERE_DOC_WINDOW );
    }

    unless ($here_doc_expected) {

        if ( !defined($line) ) {
            $here_doc_expected = -1;    # hit eof without seeing target
            $msg .= " -- must be shift; target $next_token not in file\n";

        }
        else {                          # still unsure..taking a wild guess

            if ( !$is_constant{$current_package}{$next_token} ) {
                $here_doc_expected = 1;
                $msg .=
                  " -- guessing it's a here-doc ($next_token not a constant)\n";
            }
            else {
                $msg .=
                  " -- guessing it's a shift ($next_token is a constant)\n";
            }
        }
    }
    write_logfile_entry($msg);
    return $here_doc_expected;
}

sub do_quote {

    # follow (or continue following) quoted string or pattern
    # $in_quote return code:
    #   0 - ok, found end
    #   1 - still must find end of quote whose target is $quote_character
    #   2 - still looking for end of first of two quotes
    my ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth, $rtokens,
        $rtoken_map )
      = @_;

    if ( $in_quote == 2 ) {    # two quotes/patterns to follow
        my $ibeg = $i;
        ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth ) =
          follow_quoted_string( $i, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth );

        if ( $in_quote == 1 ) {
            if ( $quote_character =~ /[\{\[\<\(]/ ) { $i++; }
            $quote_character = '';
        }
    }

    if ( $in_quote == 1 ) {    # one (more) quote to follow
        my $ibeg = $i;
        ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth ) =
          follow_quoted_string( $ibeg, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth );
    }
    return ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth );
}

sub scan_number_do {

    #  scan a number in any of the formats that Perl accepts
    #  Underbars (_) are allowed in decimal numbers.  
    #  input parameters -
    #      $input_line  - the string to scan
    #      $i           - pre_token index to start scanning
    #    $rtoken_map    - reference to the pre_token map giving starting
    #                    character position in $input_line of token $i
    #  output parameters -
    #    $i            - last pre_token index of the number just scanned
    #    number        - the number (characters); or undef if not a number

    my ( $input_line, $i, $rtoken_map, $input_type ) = @_;
    my $pos_beg = $$rtoken_map[$i];
    my $pos;
    my $i_begin = $i;
    my $number  = undef;
    my $type    = $input_type;

    my $first_char = substr( $input_line, $pos_beg, 1 );

    # Look for bad starting characters; Shouldn't happen..
    if ( $first_char !~ /[\d\.\+\-Ee]/ ) {
        warning("Program bug - scan_number given character $first_char\n");
        report_definite_bug();
        return ( $i, $type, $number );
    }

    # handle v-string without leading 'v' character ('Two Dot' rule)
    # (vstring.t)
    pos($input_line) = $pos_beg;
    if ( $input_line =~ /\G((\d+)?\.\d+(\.\d+)+)/g ) {
        $pos = pos($input_line);
        my $numc = $pos - $pos_beg;
        $number = substr( $input_line, $pos_beg, $numc );
        $type = 'v';
        unless ($saw_v_string) { report_v_string($number) }
    }

    # handle octal, hex, binary
    if ( !defined($number) ) {
        pos($input_line) = $pos_beg;
        if ( $input_line =~ /\G[+-]?0((x[0-9a-fA-F_]+)|([0-7_]+)|(b[01_]+))/g )
        {
            $pos = pos($input_line);
            my $numc = $pos - $pos_beg;
            $number = substr( $input_line, $pos_beg, $numc );
            $type = 'n';
        }
    }

    # handle decimal 
    if ( !defined($number) ) {
        pos($input_line) = $pos_beg;

        if ( $input_line =~ /\G([+-]?[\d_]*(\.[\d_]*)?([Ee][+-]?(\d+))?)/g ) {
            $pos = pos($input_line);

            # watch out for things like 0..40 which would give 0. by this;
            if (   ( substr( $input_line, $pos - 1, 1 ) eq '.' )
                && ( substr( $input_line, $pos, 1 ) eq '.' ) )
            {
                $pos--;
            }
            my $numc = $pos - $pos_beg;
            $number = substr( $input_line, $pos_beg, $numc );
            $type = 'n';
        }
    }

    # filter out non-numbers like e + - . e2  .e3 +e6
    # the rule: at least one digit, and any 'e' must be preceded by a digit
    if (
        $number !~ /\d/    # no digits
        || (   $number =~ /^(.*)[eE]/
            && $1 !~ /\d/ )    # or no digits before the 'e'
      )
    {
        $number = undef;
        $type   = $input_type;
        return ( $i, $type, $number );
    }

    # Found a number; now we must convert back from character position
    # to pre_token index. An error here implies user syntax error.
    # An example would be an invalid octal number like '009'.
    my $error;
    ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map );
    if ($error) { warning("Possibly invalid number\n") }

    return ( $i, $type, $number );
}

sub scan_bare_identifier_do {

    # this routine is called to scan a token starting with an alphanumeric
    # variable or package separator, :: or '.

    my ( $input_line, $i, $tok, $type, $prototype, $rtoken_map ) = @_;
    my $i_begin = $i;
    my $package = undef;

    my $i_beg = $i;

    # we have to back up one pretoken at a :: since each : is one pretoken
    if ( $tok eq '::' ) { $i_beg-- }
    if ( $tok eq '->' ) { $i_beg-- }
    my $pos_beg = $$rtoken_map[$i_beg];
    pos($input_line) = $pos_beg;

    if ( $input_line =~ m/\G\s*((?:\w*(?:'|::)))*(?:->)?(\w+)/gc ) {

        my $pos  = pos($input_line);
        my $numc = $pos - $pos_beg;
        $tok = substr( $input_line, $pos_beg, $numc );

        # type 'w' includes anything without leading type info
        # ($,%,@,*) including something like abc::def::ghi
        $type = 'w';

        if ( defined($1) ) {
            $package = $1;
            $package =~ s/\'/::/g;
            if ( $package =~ /^\:/ ) { $package = 'main' . $package }
            $package =~ s/::$//;
        }
        else {
            $package = $current_package;

            if ( $is_keyword{$tok} ) {
                $type = 'k';
            }
        }
        my $sub_name = $2;

        # if it is a bareword..
        if ( $type eq 'w' ) {

            # check for v-string with leading 'v' type character
            # (This seems to have presidence over filehandle, type 'Y')
            if ( $tok =~ /^v\d+$/ ) {

                # we only have the first part - something like 'v101' - 
                # look for more
                if ( $input_line =~ m/\G(\.\d+)+/gc ) {
                    $pos  = pos($input_line);
                    $numc = $pos - $pos_beg;
                    $tok  = substr( $input_line, $pos_beg, $numc );
                }
                $type = 'v';

                # warn if this version can't handle v-strings
                unless ($saw_v_string) { report_v_string($tok) }
            }

            elsif ( $is_constant{$package}{$sub_name} ) {
                $type = 'C';
            }

            # bareword after sort has implied empty prototype; for example:
            # @sorted = sort numerically ( 53, 29, 11, 32, 7 );
            # This has priority over whatever the user has specified.
            elsif ($last_nonblank_token eq 'sort'
                && $last_nonblank_type eq 'k' )
            {
                $type = 'Z';
            }

            # Note: strangely, perl does not seem to really let you create
            # functions which act like eval and do, in the sense that eval
            # and do may have operators following the final }, but any operators
            # that you create with prototype (&) apparently do not allow
            # trailing operators, only terms.  This seems strange.
            # If this ever changes, here is the update
            # to make perltidy behave accordingly:

            # elsif ( $is_block_function{$package}{$tok} ) {
            #    $tok='eval'; # patch to do braces like eval  - doesn't work
            #    $type = 'k';
            #}
            # FIXME: This could become a separate type to allow for different
            # future behavior:
            elsif ( $is_block_function{$package}{$sub_name} ) {
                $type = 'G';
            }

            elsif ( $is_block_list_function{$package}{$sub_name} ) {
                $type = 'G';
            }
            elsif ( $is_user_function{$package}{$sub_name} ) {
                $type      = 'U';
                $prototype = $user_function_prototype{$package}{$sub_name};
            }

            # check for indirect object
            elsif (

                # added 2001-03-27: must not be followed immediately by '('
                # see fhandle.t
                ( $input_line !~ m/\G\(/gc )

                # and
                && (

                    # preceded by keyword like 'print', 'printf' and friends
                    $is_indirect_object_taker{$last_nonblank_token}

                    # or preceded by something like 'print(' or 'printf('
                    || (
                        ( $last_nonblank_token eq '(' )
                        && $is_indirect_object_taker{ $paren_type[$paren_depth]
                        }

                    )
                )
              )
            {

                # may not be indirect object unless followed by a space
                if ( $input_line =~ m/\G\s+/gc ) {
                    $type = 'Y';

                    # Abandon Hope ...
                    # Perl's indirect object notation is a very bad
                    # thing and can cause subtle bugs, especially for
                    # beginning programmers.  And I haven't even been
                    # able to figure out a sane warning scheme which
                    # doesn't get in the way of good scripts.

                    # Complain if a filehandle has any lower case
                    # letters.  This is suggested good practice, but the
                    # main reason for this warning is that prior to
                    # release 20010328, perltidy incorrectly parsed a
                    # function call after a print/printf, with the
                    # result that a space got added before the opening
                    # paren, thereby converting the function name to a
                    # filehandle according to perl's weird rules.  This
                    # will not usually generate a syntax error, so this
                    # is a potentially serious bug.  By warning 
                    # of filehandles with any lower case letters,
                    # followed by opening parens, we will help the user
                    # find almost all of these older errors.  
                    # use 'sub_name' because something like
                    # main::MYHANDLE is ok for filehandle
                    if ( $sub_name =~ /[a-z]/ ) {

                        # could be bug caused by older perltidy if 
                        # followed by '('
                        if ( $input_line =~ m/\G\s*\(/gc ) {
                            complain(
"Caution: unknown word '$tok' in indirect object slot\n"
                            );
                        }
                    }
                }

                # bareword not followed by a space -- may not be filehandle
                # (may be function call defined in a 'use' statement)
                else {
                    $type = 'Z';
                }
            }
        }

        # Now we must convert back from character position
        # to pre_token index. 
        # I don't think an error flag can occur here ..but who knows
        my $error;
        ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map );
        if ($error) {
            warning("scan_bare_identifier: Possibly invalid tokenization\n");
        }
    }

    # no match but line not blank - could be syntax error
    # perl will take '::' alone without complaint
    else {
        $type = 'w';

        # change this warning to log message if it becomes annoying
        warning("didn't find identifier after leading ::\n");
    }
    return ( $i, $tok, $type, $prototype );
}

sub scan_id_do {

    # This is the new scanner and will eventually replace scan_identifier.
    # Only type 'sub' and 'package' are implemented.
    # Token types $ * % @ & -> are not yet implemented.
    # 
    # Scan identifier following a type token.
    # The type of call depends on $id_scan_state: $id_scan_state = ''
    # for starting call, in which case $tok must be the token defining
    # the type.  
    # 
    # If the type token is the last nonblank token on the line, a value
    # of $id_scan_state = $tok is returned, indicating that further
    # calls must be made to get the identifier.  If the type token is
    # not the last nonblank token on the line, the identifier is
    # scanned and handled and a value of '' is returned.

    my ( $input_line, $i, $tok, $rtokens, $rtoken_map, $id_scan_state ) = @_;
    my $type = '';
    my ( $i_beg, $pos_beg );

    #print "NSCAN:entering i=$i, tok=$tok, type=$type, state=$id_scan_state\n";
    #my ($a,$b,$c) = caller;
    #print "NSCAN: scan_id called with tok=$tok $a $b $c\n";

    # on re-entry, start scanning at first token on the line
    if ($id_scan_state) {
        $i_beg = $i;
        $type  = '';
    }

    # on initial entry, start scanning just after type token
    else {
        $i_beg         = $i + 1;
        $id_scan_state = $tok;
        $type          = 't';
    }

    # find $i_beg = index of next nonblank token,
    # and handle empty lines
    my $blank_line          = 0;
    my $next_nonblank_token = $$rtokens[$i_beg];
    if ( $i_beg > $max_token_index ) {
        $blank_line = 1;
    }
    else {

        # only a '#' immediately after a '$' is not a comment
        if ( $next_nonblank_token eq '#' ) {
            unless ( $tok eq '$' ) {
                $blank_line = 1;
            }
        }

        if ( $next_nonblank_token =~ /^\s/ ) {
            ( $next_nonblank_token, $i_beg ) =
              find_next_nonblank_token_on_this_line( $i_beg, $rtokens );
            if ( $next_nonblank_token =~ /(^#|^\s*$)/ ) {
                $blank_line = 1;
            }
        }
    }

    # handle non-blank line; identifier, if any, must follow
    unless ($blank_line) {

        if ( $id_scan_state eq 'sub' ) {
            ( $i, $tok, $type ) =
              do_scan_sub( $input_line, $i, $i_beg, $tok, $type, $rtokens,
                $rtoken_map );
        }

        elsif ( $id_scan_state eq 'package' ) {
            ( $i, $tok, $type ) =
              do_scan_package( $input_line, $i, $i_beg, $tok, $type, $rtokens,
                $rtoken_map );
        }

        else {
            warning("invalid token in scan_id: $tok\n");
        }
        $id_scan_state = '';
    }

    if ( $id_scan_state && ( !defined($type) || !$type ) ) {

        # shouldn't happen:
        warning(
"Program bug in scan_id: undefined type but scan_state=$id_scan_state\n"
        );
        report_definite_bug();
    }

    TOKENIZER_DEBUG_FLAG_NSCAN && do {
        print
          "NSCAN: returns i=$i, tok=$tok, type=$type, state=$id_scan_state\n";
    };
    return ( $i, $tok, $type, $id_scan_state );
}

sub do_scan_sub {

    # do_scan_sub parses a sub name and prototype
    # it is called with $i_beg equal to the index of the first nonblank
    # token following a 'sub' token.

    # TODO: add future error checks to be sure we have a valid 
    # sub name.  For example, 'sub &doit' is wrong.  Also, be sure
    # a name is given if and only if a non-anonymous sub is 
    # appropriate.

    my ( $input_line, $i, $i_beg, $tok, $type, $rtokens, $rtoken_map ) = @_;
    my $subname = undef;
    my $package = undef;
    my $proto   = undef;
    my $attrs   = undef;

    my $pos_beg = $$rtoken_map[$i_beg];
    pos($input_line) = $pos_beg;

    # sub NAME PROTO ATTRS BLOCK
    #if ( $input_line =~ m/\G\s*((?:\w*(?:'|::))*)(\w+)(\s*\([^){]*\))?/gc ) {
    if (
        $input_line =~ m/\G\s*
        ((?:\w*(?:'|::))*)  # package - something that ends in :: or '
        (\w+)               # NAME    - required
        (\s*\([^){]*\))?    # PROTO   - something in parens
        (\s*:(\s*(\w+))+)?  # ATTRS   - leading : followed by one or more words
        /gcx
      )
    {
        $subname = $2;
        $proto   = $3;
        $attrs   = $4;

        if ($attrs) {

            # unused for now

        }
        $package = ( defined($1) && $1 ) ? $1 : $current_package;
        $package =~ s/\'/::/g;
        if ( $package =~ /^\:/ ) { $package = 'main' . $package }
        $package =~ s/::$//;
        my $pos  = pos($input_line);
        my $numc = $pos - $pos_beg;
        $tok = 'sub ' . substr( $input_line, $pos_beg, $numc );
        $type = 'i';

        # We must convert back from character position
        # to pre_token index. 
        # I don't think an error flag can occur here ..but ?
        my $error;
        ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map );
        if ($error) { warning("Possibly invalid sub\n") }

        # check for multiple definitions of a sub
        my ( $next_nonblank_token, $i_next ) =
          find_next_nonblank_token_on_this_line( $i, $rtokens );

        if ( $next_nonblank_token =~ /^(\s*|#)$/ )
        {    # skip blank or side comment
            my ( $rpre_tokens, $rpre_types ) =
              peek_ahead_for_n_nonblank_pre_tokens(1);
            if ( defined($rpre_tokens) && @$rpre_tokens ) {
                $next_nonblank_token = $rpre_tokens->[0];
            }
            else {
                $next_nonblank_token = '}';
            }
        }

        if ( $next_nonblank_token eq '{' ) {
            if ( $saw_function_definition{$package}{$subname} ) {
                my $lno = $saw_function_definition{$package}{$subname};
                warning(
"already saw definition of 'sub $subname' in package '$package' at line $lno\n"
                );
            }
            $saw_function_definition{$package}{$subname} = $input_line_number;
        }
        elsif ( $next_nonblank_token eq ';' ) {
        }
        elsif ( $next_nonblank_token eq '}' ) {
        }
        elsif ($next_nonblank_token) {    # EOF technically ok
            warning(
"expecting ';' or '{' after definition or declaration of sub $subname but saw ($next_nonblank_token)\n"
            );

        }

        if ( defined($proto) ) {
            $proto =~ s/^\s*\(\s*//;
            $proto =~ s/\s*\)$//;
            if ($proto) {
                $is_user_function{$package}{$subname}        = 1;
                $user_function_prototype{$package}{$subname} = "($proto)";

                # prototypes containing '&' must be treated specially..
                if ( $proto =~ /\&/ ) {

                    # right curly braces of prototypes ending in 
                    # '&' may be followed by an operator
                    if ( $proto =~ /\&$/ ) {
                        $is_block_function{$package}{$subname} = 1;
                    }

                    # right curly braces of prototypes NOT ending in 
                    # '&' may NOT be followed by an operator
                    elsif ( $proto !~ /\&$/ ) {
                        $is_block_list_function{$package}{$subname} = 1;
                    }
                }
            }
            else {
                $is_constant{$package}{$subname} = 1;
            }
        }
        else {
            $is_user_function{$package}{$subname} = 1;
        }

    }

    # look for prototype following an anonymous sub so they don't get
    # stranded.  ( sub.t )  
    #elsif ( $input_line =~ m/\G\s*\([^){]*\)/gc ) 
    # sub PROTO ATTRS BLOCK
    elsif (
        $input_line =~ m/\G(\s*\([^){]*\))?  # PROTO
      (\s*:(\s*(\w+))+)?    # ATTRS
      /gcx
        && ( $1 || $2 )
      )
    {

        # remove this after testing
        if ($2) { write_diagnostics("Found anonymous sub ATTRS $2 \n"); }
        my $pos = pos($input_line);
        my $error;
        ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map );
        if ($error) { warning("Possibly invalid sub\n") }
    }

    # no match but line not blank
    else {
    }
    return ( $i, $tok, $type );
}

sub do_scan_package {

    # do_scan_package parses a package name 
    # it is called with $i_beg equal to the index of the first nonblank
    # token following a 'package' token.

    my ( $input_line, $i, $i_beg, $tok, $type, $rtokens, $rtoken_map ) = @_;
    my $package = undef;
    my $pos_beg = $$rtoken_map[$i_beg];
    pos($input_line) = $pos_beg;

    # handle non-blank line; package name, if any, must follow
    if ( $input_line =~ m/\G\s*((?:\w*(?:'|::))*\w+)/gc ) {
        $package = $1;
        $package = ( defined($1) && $1 ) ? $1 : 'main';
        $package =~ s/\'/::/g;
        if ( $package =~ /^\:/ ) { $package = 'main' . $package }
        $package =~ s/::$//;
        my $pos  = pos($input_line);
        my $numc = $pos - $pos_beg;
        $tok = 'package ' . substr( $input_line, $pos_beg, $numc );
        $type = 'i';

        # Now we must convert back from character position
        # to pre_token index. 
        # I don't think an error flag can occur here ..but ?
        my $error;
        ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map );
        if ($error) { warning("Possibly invalid package\n") }
        $current_package = $package;

        # check for error
        my ( $next_nonblank_token, $i_next ) =
          find_next_nonblank_token( $i, $rtokens );
        if ( $next_nonblank_token !~ /^[;\}]$/ ) {
            warning(
                "Unexpected '$next_nonblank_token' after package name '$tok'\n"
            );
        }
    }

    # no match but line not blank --
    # could be a label with name package, like package:  , for example.
    else {
        $type = 'k';
    }

    return ( $i, $tok, $type );
}

sub scan_identifier_do {

    # This routine assembles tokens into identifiers.  It maintains a
    # scan state, id_scan_state.  It updates id_scan_state based upon
    # current id_scan_state and token, and returns an updated
    # id_scan_state and the next index after the identifier.  

    my ( $i, $id_scan_state, $identifier, $rtokens ) = @_;
    my $i_begin   = $i;
    my $type      = '';
    my $tok_begin = $$rtokens[$i_begin];
    if ( $tok_begin eq ':' ) { $tok_begin = '::' }
    my $id_scan_state_begin = $id_scan_state;
    my $identifier_begin    = $identifier;
    my $tok                 = $tok_begin;
    my $message             = "";

    # these flags will be used to help figure out the type:
    my $saw_alpha = ( $tok =~ /^[A-Za-z_]/ );
    my $saw_type;

    # allow old package separator (') except in 'use' statement
    my $allow_tick = ( $last_nonblank_token ne 'use' );

    # get started by defining a type and a state if necessary
    unless ($id_scan_state) {
        $context = UNKNOWN_CONTEXT;

        # fixup for digraph
        if ( $tok eq '>' ) {
            $tok       = '->';
            $tok_begin = $tok;
        }
        $identifier = $tok;

        if ( $tok eq '$' || $tok eq '*' ) {
            $id_scan_state = '$';
            $context       = SCALAR_CONTEXT;
        }
        elsif ( $tok eq '%' || $tok eq '@' ) {
            $id_scan_state = '$';
            $context       = LIST_CONTEXT;
        }
        elsif ( $tok eq '&' ) {
            $id_scan_state = '&';
        }
        elsif ( $tok eq 'sub' or $tok eq 'package' ) {
            $saw_alpha     = 0;     # 'sub' is considered type info here
            $id_scan_state = '$';
            $identifier .= ' ';     # need a space to separate sub from sub name
        }
        elsif ( $tok eq '::' ) {
            $id_scan_state = 'A';
        }
        elsif ( $tok =~ /^[A-Za-z_]/ ) {
            $id_scan_state = ':';
        }
        elsif ( $tok eq '->' ) {
            $id_scan_state = '$';
        }
        else {

            # shouldn't happen
            my ( $a, $b, $c ) = caller;
            warning("Program Bug: scan_identifier given bad token = $tok \n");
            warning("   called from sub $a  line: $c\n");
            report_definite_bug();
        }
        $saw_type = !$saw_alpha;
    }
    else {
        $i--;
        $saw_type = ( $tok =~ /([\$\%\@\*\&])/ );
    }

    # now loop to gather the identifier
    my $i_save = $i;

    while ( $i < $max_token_index ) {
        $i_save = $i unless ( $tok =~ /^\s*$/ );
        $tok = $$rtokens[ ++$i ];

        if ( ( $tok eq ':' ) && ( $$rtokens[ $i + 1 ] eq ':' ) ) {
            $tok = '::';
            $i++;
        }

        if ( $id_scan_state eq '$' ) {    # starting variable name

            if ( $tok eq '$' ) {

                $identifier .= $tok;

                # we've got a punctuation variable if end of line (punct.t)
                if ( $i == $max_token_index ) {
                    $type          = 'i';
                    $id_scan_state = '';
                    last;
                }
            }
            elsif ( $tok =~ /^[A-Za-z_]/ ) {    # alphanumeric ..
                $saw_alpha     = 1;
                $id_scan_state = ':';           # now need ::
                $identifier .= $tok;
            }
            elsif ( $tok eq "'" && $allow_tick ) {    # alphanumeric ..
                $saw_alpha     = 1;
                $id_scan_state = ':';                 # now need ::
                $identifier .= $tok;

                # Perl will accept leading digits in identifiers,
                # although they may not always produce useful results.  
                # Something like $main::0 is ok.  But this also works:
                #
                #  sub howdy::123::bubba{ print "bubba $54321!\n" }
                #  howdy::123::bubba();
                #
            }
            elsif ( $tok =~ /^[0-9]/ ) {              # numeric
                $saw_alpha     = 1;
                $id_scan_state = ':';                 # now need ::
                $identifier .= $tok;
            }
            elsif ( $tok eq '::' ) {
                $id_scan_state = 'A';
                $identifier .= $tok;
            }
            elsif ( ( $tok eq '#' ) && ( $identifier eq '$' ) ) {    # $#array
                $identifier .= $tok;    # keep same state, a $ could follow
            }
            elsif ( $tok eq '{' ) {     # skip something like ${xxx} or ->{
                $id_scan_state = '';

                # if this is the first token of a line, any tokens for this
                # identifier have already been accumulated
                if ( $identifier eq '$' || $i == 0 ) { $identifier = ''; }
                $i = $i_save;
                last;
            }

            # space ok after leading $ % * & @
            elsif ( $tok =~ /^\s*$/ ) {

                if ( $identifier =~ /^[\$\%\*\&\@]/ ) {

                    if ( length($identifier) > 1 ) {
                        $id_scan_state = '';
                        $i             = $i_save;
                        $type          = 'i';    # probably punctuation variable
                        last;
                    }
                    else {

                        # spaces after $'s are common, and space after @
                        # is harmless, so only complain about space
                        # after other type characters. Space after $ and
                        # @ will be removed in formatting.  Report space
                        # after % and * because they might indicate a
                        # parsing error.  In other words '% ' might be a
                        # modulo operator.  Delete this warning if it
                        # gets annoying.
                        if ( $identifier !~ /^[\@\$]$/ ) {
                            $message =
                              "Space in identifier, following $identifier\n";
                        }
                    }
                }

                # else:
                # space after '->' is ok
            }
            elsif ( $tok eq '^' ) {

                # check for some special variables like $^W
                if ( $identifier =~ /^[\$\*\@\%]$/ ) {
                    $identifier .= $tok;
                    $id_scan_state = 'A';
                }
                else {
                    $id_scan_state = '';
                }
            }
            else {    # something else

                # check for various punctuation variables
                if ( $identifier =~ /^[\$\*\@\%]$/ ) {
                    $identifier .= $tok;
                }

                elsif ( $identifier eq '$#' ) {

                    if ( $tok eq '{' ) { $type = 'i'; $i = $i_save }

                    # perl seems to allow just these: $#: $#- $#+
                    elsif ( $tok =~ /^[\:\-\+]$/ ) {
                        $type = 'i';
                        $identifier .= $tok;
                    }
                    else {
                        $i = $i_save;
                        write_logfile_entry( 'Use of $# is deprecated' . "\n" );
                    }
                }
                elsif ( $identifier eq '$$' ) {

                    # perl does not allow references to punctuation
                    # variables without braces.  For example, this 
                    # won't work:
                    #  $:=\4;
                    #  $a = $$:;
                    # You would have to use
                    #  $a = ${$:};

                    $i = $i_save;
                    if ( $tok eq '{' ) { $type = 't' }
                    else { $type = 'i' }
                }
                elsif ( $identifier eq '->' ) {
                    $i = $i_save;
                }
                else {
                    $i = $i_save;
                    if ( length($identifier) == 1 ) { $identifier = ''; }
                }
                $id_scan_state = '';
                last;
            }
        }
        elsif ( $id_scan_state eq '&' ) {    # starting sub call?

            if ( $tok =~ /^[\$A-Za-z_]/ ) {    # alphanumeric ..
                $id_scan_state = ':';          # now need ::
                $saw_alpha     = 1;
                $identifier .= $tok;
            }
            elsif ( $tok eq "'" && $allow_tick ) {    # alphanumeric ..
                $id_scan_state = ':';                 # now need ::
                $saw_alpha     = 1;
                $identifier .= $tok;
            }
            elsif ( $tok =~ /^[0-9]/ ) {    # numeric..see comments above
                $id_scan_state = ':';       # now need ::
                $saw_alpha     = 1;
                $identifier .= $tok;
            }
            elsif ( $tok =~ /^\s*$/ ) {     # allow space
            }
            elsif ( $tok eq '::' ) {        # leading ::
                $id_scan_state = 'A';       # accept alpha next
                $identifier .= $tok;
            }
            elsif ( $tok eq '{' ) {
                if ( $identifier eq '&' || $i == 0 ) { $identifier = ''; }
                $i             = $i_save;
                $id_scan_state = '';
                last;
            }
            else {

                # punctuation variable?
                # testfile: cunningham4.pl
                if ( $identifier eq '&' ) {
                    $identifier .= $tok;
                }
                else {
                    $identifier = '';
                    $i          = $i_save;
                    $type       = '&';
                }
                $id_scan_state = '';
                last;
            }
        }
        elsif ( $id_scan_state eq 'A' ) {    # looking for alpha (after ::)

            if ( $tok =~ /^[A-Za-z_]/ ) {    # found it
                $identifier .= $tok;
                $id_scan_state = ':';        # now need ::
                $saw_alpha     = 1;
            }
            elsif ( $tok eq "'" && $allow_tick ) {
                $identifier .= $tok;
                $id_scan_state = ':';        # now need ::
                $saw_alpha     = 1;
            }
            elsif ( $tok =~ /^[0-9]/ ) {     # numeric..see comments above
                $identifier .= $tok;
                $id_scan_state = ':';        # now need ::
                $saw_alpha     = 1;
            }
            elsif ( ( $identifier =~ /^sub / ) && ( $tok =~ /^\s*$/ ) ) {
                $id_scan_state = '(';
                $identifier .= $tok;
            }
            elsif ( ( $identifier =~ /^sub / ) && ( $tok eq '(' ) ) {
                $id_scan_state = ')';
                $identifier .= $tok;
            }
            else {
                $id_scan_state = '';
                $i             = $i_save;
                last;
            }
        }
        elsif ( $id_scan_state eq ':' ) {    # looking for :: after alpha

            if ( $tok eq '::' ) {            # got it
                $identifier .= $tok;
                $id_scan_state = 'A';        # now require alpha
            }
            elsif ( $tok =~ /^[A-Za-z_]/ ) {    # more alphanumeric is ok here
                $identifier .= $tok;
                $id_scan_state = ':';           # now need ::
                $saw_alpha     = 1;
            }
            elsif ( $tok =~ /^[0-9]/ ) {        # numeric..see comments above
                $identifier .= $tok;
                $id_scan_state = ':';           # now need ::
                $saw_alpha     = 1;
            }
            elsif ( $tok eq "'" && $allow_tick ) {    # tick

                if ( $is_keyword{$identifier} ) {
                    $id_scan_state = '';              # that's all
                    $i             = $i_save;
                }
                else {
                    $identifier .= $tok;
                }
            }
            elsif ( ( $identifier =~ /^sub / ) && ( $tok =~ /^\s*$/ ) ) {
                $id_scan_state = '(';
                $identifier .= $tok;
            }
            elsif ( ( $identifier =~ /^sub / ) && ( $tok eq '(' ) ) {
                $id_scan_state = ')';
                $identifier .= $tok;
            }
            else {
                $id_scan_state = '';        # that's all
                $i             = $i_save;
                last;
            }
        }
        elsif ( $id_scan_state eq '(' ) {    # looking for ( of prototype

            if ( $tok eq '(' ) {             # got it
                $identifier .= $tok;
                $id_scan_state = ')';        # now find the end of it
            }
            elsif ( $tok =~ /^\s*$/ ) {      # blank - keep going
                $identifier .= $tok;
            }
            else {
                $id_scan_state = '';         # that's all - no prototype
                $i             = $i_save;
                last;
            }
        }
        elsif ( $id_scan_state eq ')' ) {    # looking for ) to end

            if ( $tok eq ')' ) {             # got it
                $identifier .= $tok;
                $id_scan_state = '';         # all done
                last;
            }
            elsif ( $tok =~ /^[\s\$\%\\\*\@\&\;]/ ) {
                $identifier .= $tok;
            }
            else {    # probable error in script, but keep going
                warning("Unexpected '$tok' while seeking end of prototype\n");
                $identifier .= $tok;
            }
        }
        else {        # can get here due to error in initialization
            $id_scan_state = '';
            $i             = $i_save;
            last;
        }
    }

    if ( $id_scan_state eq ')' ) {
        warning("Hit end of line while seeking ) to end prototype\n");
    }

    # once we enter the actual identifier, it may not extend beyond
    # the end of the current line
    if ( $id_scan_state =~ /^[A\:\(\)]/ ) {
        $id_scan_state = '';
    }
    if ( $i < 0 ) { $i = 0 }

    unless ($type) {

        if ($saw_type) {

            if ($saw_alpha) {
                $type = 'i';
            }
            elsif ( $identifier eq '->' ) {
                $type = '->';
            }
            elsif (
                ( length($identifier) > 1 )

                # In something like '@$=' we have an identifier '@$'
                # In something like '$${' we have type '$$' (and only
                # part of an identifier)
                && !( $identifier =~ /\$$/ && $tok eq '{' )
                && ( $identifier !~ /^(sub |package )$/ )
              )
            {
                $type = 'i';
            }
            else { $type = 't' }
        }
        elsif ($saw_alpha) {

            # type 'w' includes anything without leading type info
            # ($,%,@,*) including something like abc::def::ghi
            $type = 'w';
        }
        else {
            $type = '';
        }    # this can happen on a restart
    }

    if ($identifier) {
        $tok = $identifier;
        if ($message) { write_logfile_entry($message) }
    }
    else {
        $tok = $tok_begin;
        $i   = $i_begin;
    }

    TOKENIZER_DEBUG_FLAG_SCAN_ID && do {
        my ( $a, $b, $c ) = caller;
        print
"SCANID: called from $a $b $c with tok, i, state, identifier =$tok_begin, $i_begin, $id_scan_state_begin, $identifier_begin\n";
        print
"SCANID: returned with tok, i, state, identifier =$tok, $i, $id_scan_state, $identifier\n";
    };
    return ( $i, $tok, $type, $id_scan_state, $identifier );
}

sub follow_quoted_string {

    # scan for a specific token, skipping escaped characters
    # if the quote character is blank, use the first non-blank character
    # input parameters:
    #   $rtokens = reference to the array of tokens
    #   $i = the token index of the first character to search
    #   $in_quote = number of quoted strings being followed
    #   $beginning_tok = the starting quote character
    #   $quote_pos = index to check next for alphanumeric delimiter
    # output parameters:
    #   $i = the token index of the ending quote character
    #   $in_quote = decremented if found end, unchanged if not
    #   $beginning_tok = the starting quote character
    #   $quote_pos = index to check next for alphanumeric delimiter
    #   $quote_depth = nesting depth, since delimiters '{ ( [ <' can be nested.
    my ( $i_beg, $in_quote, $rtokens, $beginning_tok, $quote_pos, $quote_depth )
      = @_;
    my ( $tok, $end_tok );
    my $i = $i_beg - 1;

    TOKENIZER_DEBUG_FLAG_QUOTE && do {
        print
"QUOTE entering with quote_pos = $quote_pos i=$i beginning_tok =$beginning_tok\n";
    };

    # get the corresponding end token
    if ( $beginning_tok !~ /^\s*$/ ) {
        $end_tok = matching_end_token($beginning_tok);
    }

    # a blank token means we must find and use the first non-blank one
    else {
        my $allow_quote_comments = ( $i < 0 ) ? 1 : 0; # i<0 means we saw a <cr>

        while ( $i < $max_token_index ) {
            $tok = $$rtokens[ ++$i ];

            if ( $tok !~ /^\s*$/ ) {

                if ( ( $tok eq '#' ) && ($allow_quote_comments) ) {
                    $i = $max_token_index;
                }
                else {

                    if ( length($tok) > 1 ) {
                        if ( $quote_pos <= 0 ) { $quote_pos = 1 }
                        $beginning_tok = substr( $tok, $quote_pos - 1, 1 );
                    }
                    else {
                        $beginning_tok = $tok;
                        $quote_pos     = 0;
                    }
                    $end_tok     = matching_end_token($beginning_tok);
                    $quote_depth = 1;
                    last;
                }
            }
            else {
                $allow_quote_comments = 1;
            }
        }
    }

    # There are two different loops which search for the ending quote
    # character.  In the rare case of an alphanumeric quote delimiter, we
    # have to look through alphanumeric tokens character-by-character, since
    # the pre-tokenization process combines multiple alphanumeric
    # characters, whereas for a non-alphanumeric delimiter, only tokens of
    # length 1 can match.

    # loop for case of alphanumeric quote delimiter..
    # "quote_pos" is the position the current word to begin searching
    if ( $beginning_tok =~ /\w/ ) {

        # Note this because it is not recommended practice except
        # for obfuscated perl contests
        if ( $in_quote == 1 ) {
            write_logfile_entry(
                "Note: alphanumeric quote delimiter ($beginning_tok) \n");
        }

        while ( $i < $max_token_index ) {

            if ( $quote_pos == 0 || ( $i < 0 ) ) {
                $tok = $$rtokens[ ++$i ];

                if ( $tok eq '\\' ) {

                    $quote_pos++;
                    last if ( $i >= $max_token_index );
                    $tok = $$rtokens[ ++$i ];

                }
            }
            my $old_pos = $quote_pos;

            unless ( defined($tok) && defined($end_tok) && defined($quote_pos) )
            {

            }
            $quote_pos = 1 + index( $tok, $end_tok, $quote_pos );

            if ( $quote_pos > 0 ) {

                $quote_depth--;

                if ( $quote_depth == 0 ) {
                    $in_quote--;
                    last;
                }
            }
        }
    }

    # loop for case of a non-alphanumeric quote delimiter..
    else {

        while ( $i < $max_token_index ) {
            $tok = $$rtokens[ ++$i ];

            if ( $tok eq $end_tok ) {
                $quote_depth--;

                if ( $quote_depth == 0 ) {
                    $in_quote--;
                    last;
                }
            }
            elsif ( $tok eq $beginning_tok ) {
                $quote_depth++;
            }
            elsif ( $tok eq '\\' ) {
                $i++;
            }
        }
    }
    if ( $i > $max_token_index ) { $i = $max_token_index }
    return ( $i, $in_quote, $beginning_tok, $quote_pos, $quote_depth );
}

sub matching_end_token {

    # find closing character for a pattern
    my $beginning_token = shift;

    if ( $beginning_token eq '{' ) {
        '}';
    }
    elsif ( $beginning_token eq '[' ) {
        ']';
    }
    elsif ( $beginning_token eq '<' ) {
        '>';
    }
    elsif ( $beginning_token eq '(' ) {
        ')';
    }
    else {
        $beginning_token;
    }
}

BEGIN {

    # These names are used in error messages
    @opening_brace_names = qw# '{' '[' '(' '?' #;
    @closing_brace_names = qw# '}' ']' ')' ':' #;

    my @digraphs = qw(
      .. :: << >> ** && .. ||  -> => += -= .= %= &= |= ^= *= <>
      <= >= == =~ !~ != ++ -- /= x=
    );
    @is_digraph{@digraphs} = (1) x scalar(@digraphs);

    my @trigraphs = qw( ... **= <<= >>= &&= ||= <=> );
    @is_trigraph{@trigraphs} = (1) x scalar(@trigraphs);

    # make a hash of all valid token types for self-checking the tokenizer
    # (adding NEW_TOKENS : select a new character and add to this list)
    my @valid_token_types = qw#
      b C G L R f h Q k t w i q n p m F pp mm U j J Y Z v
      { } ( ) [ ] ; + - / * | % ! x ~ = \ ? : . < > ^ &
      #;
    push ( @valid_token_types, @digraphs );
    push ( @valid_token_types, @trigraphs );
    push ( @valid_token_types, '#' );
    push ( @valid_token_types, ',' );
    @is_valid_token_type{@valid_token_types} = (1) x scalar(@valid_token_types);

    # a list of file test letters, as in -e (Table 3-4 of 'camel 3')
    my @file_test_operators =
      qw( A B C M O R S T W X b c d e f g k l o p r s t u w x z);
    @is_file_test_operator{@file_test_operators} =
      (1) x scalar(@file_test_operators);

    # these functions have prototypes of the form (&), so when they are
    # followed by a block, that block MAY BE followed by an operator.
    @_ = qw( do eval );
    @is_block_operator{@_} = (1) x scalar(@_);

    # these functions allow an identifier in the indirect object slot
    @_ = qw( print printf sort exec system );
    @is_indirect_object_taker{@_} = (1) x scalar(@_);

    # These tokens may precede a code block
    @_ = qw( BEGIN END CHECK INIT AUTOLOAD DESTROY continue if elsif else
      unless do while until eval for foreach map grep sort );
    @is_code_block_token{@_} = (1) x scalar(@_);

    # I'll build the list of keywords incrementally
    my @Keywords = ();

    # keywords and tokens after which a value or pattern is expected,
    # but not an operator.  In other words, these should consume terms
    # to their right, or at least they are not expected to be followed
    # immediately by operators.
    # --added srand 20-mar-01
    my @value_requestor = qw(
      AUTOLOAD
      BEGIN
      CHECK
      DESTROY
      END
      EQ
      GE
      GT
      INIT
      LE
      LT
      NE
      abs
      accept
      alarm
      and
      atan2
      bind
      binmode
      bless
      caller
      chdir
      chmod
      chomp
      chop
      chown
      chr
      chroot
      close
      closedir
      cmp
      connect
      continue
      cos
      crypt
      dbmclose
      dbmopen
      defined
      delete
      die
      dump
      each
      else
      elsif
      eof
      eq
      exec
      exists
      exit
      exp
      fcntl
      fileno
      flock
      for
      foreach
      formline
      ge
      getc
      getgrgid
      getgrnam
      gethostbyaddr
      gethostbyname
      getnetbyaddr
      getnetbyname
      getpeername
      getpgrp
      getpriority
      getprotobyname
      getprotobynumber
      getpwnam
      getpwuid
      getservbyname
      getservbyport
      getsockname
      getsockopt
      glob
      gmtime
      goto
      grep
      gt
      hex
      if
      index
      int
      ioctl
      join
      keys
      kill
      last
      lc
      lcfirst
      le
      length
      link
      listen
      local
      localtime
      lock
      log
      lstat
      lt
      map
      mkdir
      msgctl
      msgget
      msgrcv
      msgsnd
      my
      ne
      next
      no
      not
      oct
      open
      opendir
      or
      ord
      our
      pack
      pipe
      pop
      pos
      print
      printf
      prototype
      push
      quotemeta
      rand
      read
      readdir
      readlink
      readline
      readpipe
      recv
      redo
      ref
      rename
      require
      reset
      return
      reverse
      rewinddir
      rindex
      rmdir
      scalar
      seek
      seekdir
      select
      semctl
      semget
      semop
      send
      sethostent
      setnetent
      setpgrp
      setpriority
      setprotoent
      setservent
      setsockopt
      shift
      shmctl
      shmget
      shmread
      shmwrite
      shutdown
      sin
      sleep
      socket
      socketpair
      sort
      splice
      split
      sprintf
      sqrt
      srand
      stat
      study
      substr
      symlink
      syscall
      sysopen
      sysread
      sysseek
      system
      syswrite
      tell
      telldir
      tie
      tied
      truncate
      uc
      ucfirst
      umask
      undef
      unless
      unlink
      unpack
      unshift
      untie
      until
      use
      utime
      values
      vec
      waitpid
      warn
      while
      write
      xor
    );

    push ( @Keywords, @value_requestor );

    # These are treated the same but are not keywords:
    my @extra_vr = qw(
      constant
      switch
      vars
    );
    push ( @value_requestor, @extra_vr );

    @expecting_term_token{@value_requestor} = (1) x scalar(@value_requestor);

    # this list contains keywords which do not look for arguments,
    # so that they might be followed by an operator, or at least
    # not a term.
    my @operator_requestor = qw(
      endgrent
      endhostent
      endnetent
      endprotoent
      endpwent
      endservent
      fork
      getgrent
      gethostent
      getlogin
      getnetent
      getppid
      getprotoent
      getpwent
      getservent
      setgrent
      setpwent
      time
      times
      wait
      wantarray
    );

    push ( @Keywords, @operator_requestor );

    # These are treated the same but are not considered keywords:
    my @extra_or = qw(
      STDERR
      STDIN
      STDOUT
    );

    push ( @operator_requestor, @extra_or );

    @expecting_operator_token{@operator_requestor} =
      (1) x scalar(@operator_requestor);

    # these token TYPES expect trailing operator but not a term
    # note: ++ and -- are post-increment and decrement, 'C' = constant
    my @operator_requestor_types = qw( ++ -- C );
    @expecting_operator_types{@operator_requestor_types} =
      (1) x scalar(@operator_requestor_types);

    # these token TYPES consume values (terms)
    # note: pp and mm are pre-increment and decrement
    # f=semicolon in for,  F=file test operator
    my @value_requestor_type = qw#
      L { ( [ ~ !~ =~ ; . .. ...  : && ! || = + - x
      **= += -= .= /= *= %= x= &= |= ^= <<= >>= &&= ||=
      <= >= == != => \ > < % * / ? & | ** <=>
      f F pp mm Y p m U J G
      #;
    push ( @value_requestor_type, ',' )
      ;    # (perl doesn't like a ',' in a qw block)
    @expecting_term_types{@value_requestor_type} =
      (1) x scalar(@value_requestor_type);

    # For simple syntax checking, it is nice to have a list of operators which
    # will really be unhappy if not followed by a term.  This includes most
    # of the above... 
    %really_want_term = %expecting_term_types;

    # with these exceptions...
    delete $really_want_term{'U'}; # user sub, depends on prototype
    delete $really_want_term{'F'}; # file test works on $_ if no following term
    delete $really_want_term{'Y'}; # indirect object, too risky to check syntax;
                                   # let perl do it

    # These keywords are handled specially in the tokenizer code:
    my @special_keywords = qw(
      do
      eval
      format
      m
      package
      q
      qq
      qr
      qw
      qx
      s
      sub
      tr
      y
    );
    push ( @Keywords, @special_keywords );

    # Keywords after which list formatting may be used
    # WARNING: do not include |map|grep|eval or perl may die on
    # syntax errors (map1.t). 
    my @keyword_taking_list = qw(
      and
      chmod
      chomp
      chop
      chown
      dbmopen
      die
      elsif
      exec
      fcntl
      for
      foreach
      formline
      getsockopt
      if
      index
      ioctl
      join
      kill
      local
      msgctl
      msgrcv
      msgsnd
      my
      open
      or
      our
      pack
      print
      printf
      push
      read
      readpipe
      recv
      return
      reverse
      rindex
      seek
      select
      semctl
      semget
      send
      setpriority
      setsockopt
      shmctl
      shmget
      shmread
      shmwrite
      socket
      socketpair
      sort
      splice
      split
      sprintf
      substr
      syscall
      sysopen
      sysread
      sysseek
      system
      syswrite
      tie
      unless
      unlink
      unpack
      unshift
      until
      vec
      warn
      while
    );
    @is_keyword_taking_list{@keyword_taking_list} =
      (1) x scalar(@keyword_taking_list);

    # These are not used in any way yet
    #    my @unused_keywords = qw(
    #      CORE
    #     __FILE__
    #     __LINE__
    #     __PACKAGE__
    #     );

    #  The list of keywords was extracted from function 'keyword' in
    #  perl file toke.c version 5.005.03, using this utility, plus a
    #  little editing: (file getkwd.pl):
    #  while (<>) { while (/\"(.*)\"/g) { print "$1\n"; } }
    #  Add 'get' prefix where necessary, then split into the above lists.
    #  This list should be updated as necessary.
    #  The list should not contain these special variables:
    #  ARGV DATA ENV SIG STDERR STDIN STDOUT
    #  __DATA__ __END__

    @is_keyword{@Keywords} = (1) x scalar(@Keywords);
}
1;
__END__





=head1 NAME

Perl::Tidy - main module for the perltidy utility

=head1 SYNOPSIS

    use Perl::Tidy;

    Perl::Tidy::perltidy(
        source      => $source,
        destination => $destination,
        stderr      => $stderr,
        argv        => $argv,
        perltidyrc  => $perltidyrc,
        logfile     => $logfile,
        errorfile   => $errorfile,
    );

=head1 DESCRIPTION

This module makes the functionality of the perltidy utility available to perl
scripts.  Any or all of the input parameters may be omitted, in which case the
@ARGV array will be used to provide input parameters as described
in the perltidy(1) man page.

For example, the perltidy script is basically just this:

    use Perl::Tidy;
    Perl::Tidy::perltidy();

The module accepts input and output streams by a variety of methods.
The following list of parameters may be any of a the following: a
filename, an ARRAY reference, a SCALAR reference, or an object with
either a B<getline> or B<print> method, as appropriate.

        source          - the source of the script to be formatted
        destination     - the destination of the formatted output
        stderr          - standard error output
        perltidyrc      - the .perltidyrc file
        logfile         - the .LOG file stream, if any 
        errorfile       - the .ERR file stream, if any

The following chart illustrates the logic used to decide how to
treat a parameter.

   ref($param)  $param is assumed to be:
   -----------  ---------------------
   undef        a filename
   SCALAR       ref to string; IO::Scalar must be installed
   ARRAY        ref to array; IO::ScalarArray must be installed
   (other)      object with getline (if source) or print method

If the parameter is an object, and the object has a B<close> method, that
close method will be called at the end of the stream.

=over 4

=item source

If the B<source> parameter is given, it defines the source of the
input stream.

=item destination

If the B<destination> parameter is given, it will be used to define the
file or memory location to receive output of perltidy.  

=item stderr

The B<stderr> parameter allows the calling program to capture the output
to what would otherwise go to the standard error output device.

=item perltidyrc

If the B<perltidyrc> file is given, it will be used instead of any
F<.perltidyrc> configuration file that would otherwise be used. 

=item argv

If the B<argv> parameter is given, it will be used instead of the
B<@ARGV> array.  The B<argv> parameter may be a string, a reference to a
string, or a reference to an array.  If it is a string or reference to a
string, it will be parsed into an array of items just as if it were a
command line string.

=back

=head1 EXAMPLE

The following example passes perltidy a snippet as a reference
to a string and receives the result back in a reference to
an array.  It requires that both IO::Scalar and IO::ScalarArray 
be installed to run.

 use Perl::Tidy;
 
 # some messy source code to format
 my $source = <<'EOM';
 use strict;
 my @editors=('Emacs', 'Vi   '); my $rand = rand();
 print "A poll of 10 random programmers gave these results:\n";
 foreach(0..10) {
 my $i=int ($rand+rand());
 print " $editors[$i] users are from Venus" . ", " . 
 "$editors[1-$i] users are from Mars" . 
 "\n";
 }
 EOM
 
 # We'll pass it as ref to SCALAR and receive it in a ref to ARRAY
 my @dest;
 perltidy( source => \$source, destination => \@dest );
 foreach (@dest) {print}

=head1 EXPORT

  &perltidy

=head1 CREDITS

Thanks to Hugh Myers who developed the initial modular interface 
to perltidy.

=head1 VERSION

This man page documents Perl::Tidy version 20020826.

=head1 AUTHOR

 Steve Hancock
 perltidy at users.sourceforge.net

=head1 SEE ALSO

The perltidy(1) man page describes all of the features of perltidy.  It
can be found at http://perltidy.sourceforge.net.

=cut






