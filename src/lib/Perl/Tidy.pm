############################################################
#
#    perltidy - a perl script indenter and formatter
#
#    Copyright (c) 2000-2007 by Steve Hancock
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
#      perltidy Tidy.pm
#
#    Code Contributions:
#      Michael Cartmell supplied code for adaptation to VMS and helped with
#        v-strings.
#      Hugh S. Myers supplied sub streamhandle and the supporting code to
#        create a Perl::Tidy module which can operate on strings, arrays, etc.
#      Yves Orton supplied coding to help detect Windows versions.
#      Axel Rose supplied a patch for MacPerl.
#      Sebastien Aperghis-Tramoni supplied a patch for the defined or operator.
#      Dan Tyrell contributed a patch for binary I/O.
#      Ueli Hugenschmidt contributed a patch for -fpsc
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
$|++;

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
    ( $VERSION = q($Id: Tidy.pm,v 1.73 2007/12/05 17:51:17 perltidy Exp $) ) =~ s/^.*\s+(\d+)\/(\d+)\/(\d+).*$/$1$2$3/; # all one line for MakeMaker
}

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
    # ARRAY  ref           Perl::Tidy::IOScalarArray (formerly IO::ScalarArray)
    # STRING ref           Perl::Tidy::IOScalar      (formerly IO::Scalar)
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
            $New = sub { Perl::Tidy::IOScalarArray->new(@_) };
        }
        elsif ( $ref eq 'SCALAR' ) {
            $New = sub { Perl::Tidy::IOScalar->new(@_) };
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

sub find_input_line_ending {

    # Peek at a file and return first line ending character.
    # Quietly return undef in case of any trouble.
    my ($input_file) = @_;
    my $ending;

    # silently ignore input from object or stdin
    if ( ref($input_file) || $input_file eq '-' ) {
        return $ending;
    }
    open( INFILE, $input_file ) || return $ending;

    binmode INFILE;
    my $buf;
    read( INFILE, $buf, 1024 );
    close INFILE;
    if ( $buf && $buf =~ /([\012\015]+)/ ) {
        my $test = $1;

        # dos
        if ( $test =~ /^(\015\012)+$/ ) { $ending = "\015\012" }

        # mac
        elsif ( $test =~ /^\015+$/ ) { $ending = "\015" }

        # unix
        elsif ( $test =~ /^\012+$/ ) { $ending = "\012" }

        # unknown
        else { }
    }

    # no ending seen
    else { }

    return $ending;
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

sub make_temporary_filename {

    # Make a temporary filename.
    #
    # The POSIX tmpnam() function tends to be unreliable for non-unix
    # systems (at least for the win32 systems that I've tested), so use
    # a pre-defined name.  A slight disadvantage of this is that two
    # perltidy runs in the same working directory may conflict.
    # However, the chance of that is small and managable by the user.
    # An alternative would be to check for the file's existance and use,
    # say .TMP0, .TMP1, etc, but that scheme has its own problems.  So,
    # keep it simple.
    my $name = "perltidy.TMP";
    if ( $^O =~ /win32|dos/i || $^O eq 'VMS' || $^O eq 'MacOs' ) {
        return $name;
    }
    eval "use POSIX qw(tmpnam)";
    if ($@) { return $name }
    use IO::File;

    # just make a couple of tries before giving up and using the default
    for ( 0 .. 1 ) {
        my $tmpname = tmpnam();
        my $fh = IO::File->new( $tmpname, O_RDWR | O_CREAT | O_EXCL );
        if ($fh) {
            $fh->close();
            return ($tmpname);
            last;
        }
    }
    return ($name);
}

# Here is a map of the flow of data from the input source to the output
# line sink:
#
# LineSource-->Tokenizer-->Formatter-->VerticalAligner-->FileWriter-->
#       input                         groups                 output
#       lines   tokens      lines       of          lines    lines
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

            if   ( ref $input_file ) { print STDERR " of reference to:" }
            else                     { print STDERR " of file:" }
            print STDERR " $input_file";
        }
        print STDERR "\n";
        exit $exit_flag if defined($exit_flag);
    }

    sub perltidy {

        my %defaults = (
            argv                  => undef,
            destination           => undef,
            formatter             => undef,
            logfile               => undef,
            errorfile             => undef,
            perltidyrc            => undef,
            source                => undef,
            stderr                => undef,
            dump_options          => undef,
            dump_options_type     => undef,
            dump_getopt_flags     => undef,
            dump_options_category => undef,
            dump_options_range    => undef,
            dump_abbreviations    => undef,
        );

        # don't overwrite callers ARGV
        local @ARGV = @ARGV;

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

        my $get_hash_ref = sub {
            my ($key) = @_;
            my $hash_ref = $input_hash{$key};
            if ( defined($hash_ref) ) {
                unless ( ref($hash_ref) eq 'HASH' ) {
                    my $what = ref($hash_ref);
                    my $but_is =
                      $what ? "but is ref to $what" : "but is not a reference";
                    croak <<EOM;
------------------------------------------------------------------------
error in call to perltidy:
-$key must be reference to HASH $but_is
------------------------------------------------------------------------
EOM
                }
            }
            return $hash_ref;
        };

        %input_hash = ( %defaults, %input_hash );
        my $argv               = $input_hash{'argv'};
        my $destination_stream = $input_hash{'destination'};
        my $errorfile_stream   = $input_hash{'errorfile'};
        my $logfile_stream     = $input_hash{'logfile'};
        my $perltidyrc_stream  = $input_hash{'perltidyrc'};
        my $source_stream      = $input_hash{'source'};
        my $stderr_stream      = $input_hash{'stderr'};
        my $user_formatter     = $input_hash{'formatter'};

        # various dump parameters
        my $dump_options_type     = $input_hash{'dump_options_type'};
        my $dump_options          = $get_hash_ref->('dump_options');
        my $dump_getopt_flags     = $get_hash_ref->('dump_getopt_flags');
        my $dump_options_category = $get_hash_ref->('dump_options_category');
        my $dump_abbreviations    = $get_hash_ref->('dump_abbreviations');
        my $dump_options_range    = $get_hash_ref->('dump_options_range');

        # validate dump_options_type
        if ( defined($dump_options) ) {
            unless ( defined($dump_options_type) ) {
                $dump_options_type = 'perltidyrc';
            }
            unless ( $dump_options_type =~ /^(perltidyrc|full)$/ ) {
                croak <<EOM;
------------------------------------------------------------------------
Please check value of -dump_options_type in call to perltidy;
saw: '$dump_options_type' 
expecting: 'perltidyrc' or 'full'
------------------------------------------------------------------------
EOM

            }
        }
        else {
            $dump_options_type = "";
        }

        if ($user_formatter) {

            # if the user defines a formatter, there is no output stream,
            # but we need a null stream to keep coding simple
            $destination_stream = Perl::Tidy::DevNull->new();
        }

        # see if ARGV is overridden
        if ( defined($argv) ) {

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
        my ( $rOpts, $config_file, $rraw_options, $saw_extrude, $roption_string,
            $rexpansion, $roption_category, $roption_range )
          = process_command_line(
            $perltidyrc_stream,  $is_Windows, $Windows_type,
            $rpending_complaint, $dump_options_type,
          );

        # return or exit immediately after all dumps
        my $quit_now = 0;

        # Getopt parameters and their flags
        if ( defined($dump_getopt_flags) ) {
            $quit_now = 1;
            foreach my $op ( @{$roption_string} ) {
                my $opt  = $op;
                my $flag = "";

                # Examples:
                #  some-option=s
                #  some-option=i
                #  some-option:i
                #  some-option!
                if ( $opt =~ /(.*)(!|=.*|:.*)$/ ) {
                    $opt  = $1;
                    $flag = $2;
                }
                $dump_getopt_flags->{$opt} = $flag;
            }
        }

        if ( defined($dump_options_category) ) {
            $quit_now = 1;
            %{$dump_options_category} = %{$roption_category};
        }

        if ( defined($dump_options_range) ) {
            $quit_now = 1;
            %{$dump_options_range} = %{$roption_range};
        }

        if ( defined($dump_abbreviations) ) {
            $quit_now = 1;
            %{$dump_abbreviations} = %{$rexpansion};
        }

        if ( defined($dump_options) ) {
            $quit_now = 1;
            %{$dump_options} = %{$rOpts};
        }

        return if ($quit_now);

        # make printable string of options for this run as possible diagnostic
        my $readable_options = readable_options( $rOpts, $roption_string );

        # dump from command line
        if ( $rOpts->{'dump-options'} ) {
            print STDOUT $readable_options;
            exit 1;
        }

        check_options( $rOpts, $is_Windows, $Windows_type,
            $rpending_complaint );

        if ($user_formatter) {
            $rOpts->{'format'} = 'user';
        }

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

        my $output_extension =
          make_extension( $rOpts->{'output-file-extension'},
            $default_file_extension{ $rOpts->{'format'} }, $dot );

        my $backup_extension =
          make_extension( $rOpts->{'backup-file-extension'}, 'bak', $dot );

        my $html_toc_extension =
          make_extension( $rOpts->{'html-toc-extension'}, 'toc', $dot );

        my $html_src_extension =
          make_extension( $rOpts->{'html-src-extension'}, 'src', $dot );

        # check for -b option;
        my $in_place_modify = $rOpts->{'backup-and-modify-in-place'}
          && $rOpts->{'format'} eq 'tidy' # silently ignore unless beautify mode
          && @ARGV > 0;    # silently ignore if standard input;
                           # this allows -b to be in a .perltidyrc file
                           # without error messages when running from an editor

        # turn off -b with warnings in case of conflicts with other options
        if ($in_place_modify) {
            if ( $rOpts->{'standard-output'} ) {
                warn "Ignoring -b; you may not use -b and -st together\n";
                $in_place_modify = 0;
            }
            if ($destination_stream) {
                warn
"Ignoring -b; you may not specify a destination array and -b together\n";
                $in_place_modify = 0;
            }
            if ($source_stream) {
                warn
"Ignoring -b; you may not specify a source array and -b together\n";
                $in_place_modify = 0;
            }
            if ( $rOpts->{'outfile'} ) {
                warn "Ignoring -b; you may not use -b and -o together\n";
                $in_place_modify = 0;
            }
            if ( defined( $rOpts->{'output-path'} ) ) {
                warn "Ignoring -b; you may not use -b and -opath together\n";
                $in_place_modify = 0;
            }
        }

        Perl::Tidy::Formatter::check_options($rOpts);
        if ( $rOpts->{'format'} eq 'html' ) {
            Perl::Tidy::HtmlWriter->check_options($rOpts);
        }

        # make the pattern of file extensions that we shouldn't touch
        my $forbidden_file_extensions = "(($dot_pattern)(LOG|DEBUG|ERR|TEE)";
        if ($output_extension) {
            my $ext = quotemeta($output_extension);
            $forbidden_file_extensions .= "|$ext";
        }
        if ( $in_place_modify && $backup_extension ) {
            my $ext = quotemeta($backup_extension);
            $forbidden_file_extensions .= "|$ext";
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
            unshift( @ARGV, $source_stream );

            # No special treatment for source stream which is a filename.
            # This will enable checks for binary files and other bad stuff.
            $source_stream = undef unless ref($source_stream);
        }

        # use stdin by default if no source array and no args
        else {
            unshift( @ARGV, '-' ) unless @ARGV;
        }

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

                    # file doesn't exist - check for a file glob
                    if ( $input_file =~ /([\?\*\[\{])/ ) {

                        # Windows shell may not remove quotes, so do it
                        my $input_file = $input_file;
                        if ( $input_file =~ /^\'(.+)\'$/ ) { $input_file = $1 }
                        if ( $input_file =~ /^\"(.+)\"$/ ) { $input_file = $1 }
                        my $pattern = fileglob_to_re($input_file);
                        eval "/$pattern/";
                        if ( !$@ && opendir( DIR, './' ) ) {
                            my @files =
                              grep { /$pattern/ && !-d $_ } readdir(DIR);
                            closedir(DIR);
                            if (@files) {
                                unshift @ARGV, @files;
                                next;
                            }
                        }
                    }
                    print "skipping file: '$input_file': no matches found\n";
                    next;
                }

                unless ( -f $input_file ) {
                    print "skipping file: $input_file: not a regular file\n";
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
                        unless ( mkdir $new_path, 0777 ) {
                            die "unable to create directory $new_path: $!\n";
                        }
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
                && (   $input_file =~ /$forbidden_file_extensions/o
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
            my $actual_output_extension;

            if ( $rOpts->{'outfile'} ) {

                if ( $number_of_files <= 1 ) {

                    if ( $rOpts->{'standard-output'} ) {
                        die "You may not use -o and -st together\n";
                    }
                    elsif ($destination_stream) {
                        die
"You may not specify a destination array and -o together\n";
                    }
                    elsif ( defined( $rOpts->{'output-path'} ) ) {
                        die "You may not specify -o and -opath together\n";
                    }
                    elsif ( defined( $rOpts->{'output-file-extension'} ) ) {
                        die "You may not specify -o and -oext together\n";
                    }
                    $output_file = $rOpts->{outfile};

                    # make sure user gives a file name after -o
                    if ( $output_file =~ /^-/ ) {
                        die "You must specify a valid filename after -o\n";
                    }

                    # do not overwrite input file with -o
                    if ( defined($input_file_permissions)
                        && ( $output_file eq $input_file ) )
                    {
                        die
                          "Use 'perltidy -b $input_file' to modify in-place\n";
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
                    $actual_output_extension = $output_extension;
                    $output_file             = $fileroot . $output_extension;
                }
            }

            # the 'sink_object' knows how to write the output file
            my $tee_file = $fileroot . $dot . "TEE";

            my $line_separator = $rOpts->{'output-line-ending'};
            if ( $rOpts->{'preserve-line-endings'} ) {
                $line_separator = find_input_line_ending($input_file);
            }

            # Eventually all I/O may be done with binmode, but for now it is
            # only done when a user requests a particular line separator
            # through the -ple or -ole flags
            my $binmode = 0;
            if   ( defined($line_separator) ) { $binmode        = 1 }
            else                              { $line_separator = "\n" }

            my $sink_object =
              Perl::Tidy::LineSink->new( $output_file, $tee_file,
                $line_separator, $rOpts, $rpending_logfile_message, $binmode );

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
                $rraw_options, $Windows_type,  $readable_options,
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
                  Perl::Tidy::HtmlWriter->new( $fileroot, $output_file,
                    $actual_output_extension, $html_toc_extension,
                    $html_src_extension );
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
                binmode $fout;
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

sub fileglob_to_re {

    # modified (corrected) from version in find2perl
    my $x = shift;
    $x =~ s#([./^\$()])#\\$1#g;    # escape special characters
    $x =~ s#\*#.*#g;               # '*' -> '.*'
    $x =~ s#\?#.#g;                # '?' -> '.'
    "^$x\\z";                      # match whole word
}

sub make_extension {

    # Make a file extension, including any leading '.' if necessary
    # The '.' may actually be an '_' under VMS
    my ( $extension, $default, $dot ) = @_;

    # Use the default if none specified
    $extension = $default unless ($extension);

    # Only extensions with these leading characters get a '.'
    # This rule gives the user some freedom
    if ( $extension =~ /^[a-zA-Z0-9]/ ) {
        $extension = $dot . $extension;
    }
    return $extension;
}

sub write_logfile_header {
    my (
        $rOpts,        $logger_object, $config_file,
        $rraw_options, $Windows_type,  $readable_options
    ) = @_;
    $logger_object->write_logfile_entry(
"perltidy version $VERSION log file on a $^O system, OLD_PERL_VERSION=$]\n"
    );
    if ($Windows_type) {
        $logger_object->write_logfile_entry("Windows type is $Windows_type\n");
    }
    my $options_string = join( ' ', @$rraw_options );

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

        $logger_object->write_logfile_entry($readable_options);

        $logger_object->write_logfile_entry(
            "------------------------------------\n");
    }
    $logger_object->write_logfile_entry(
        "To find error messages search for 'WARNING' with your editor\n");
}

sub generate_options {

    ######################################################################
    # Generate and return references to:
    #  @option_string - the list of options to be passed to Getopt::Long
    #  @defaults - the list of default options
    #  %expansion - a hash showing how all abbreviations are expanded
    #  %category - a hash giving the general category of each option
    #  %option_range - a hash giving the valid ranges of certain options

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
    # valign                              # for debugging vertical alignment
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

    my @option_string   = ();
    my %expansion       = ();
    my %option_category = ();
    my %option_range    = ();
    my $rexpansion      = \%expansion;

    # names of categories in manual
    # leading integers will allow sorting
    my @category_name = (
        '0. I/O control',
        '1. Basic formatting options',
        '2. Code indentation control',
        '3. Whitespace control',
        '4. Comment controls',
        '5. Linebreak controls',
        '6. Controlling list formatting',
        '7. Retaining or ignoring existing line breaks',
        '8. Blank line control',
        '9. Other controls',
        '10. HTML options',
        '11. pod2html options',
        '12. Controlling HTML properties',
        '13. Debugging',
    );

    #  These options are parsed directly by perltidy:
    #    help h
    #    version v
    #  However, they are included in the option set so that they will
    #  be seen in the options dump.

    # These long option names have no abbreviations or are treated specially
    @option_string = qw(
      html!
      noprofile
      no-profile
      npro
      recombine!
      valign!
    );

    my $category = 13;    # Debugging
    foreach (@option_string) {
        my $opt = $_;     # must avoid changing the actual flag
        $opt =~ s/!$//;
        $option_category{$opt} = $category_name[$category];
    }

    $category = 11;                                       # HTML
    $option_category{html} = $category_name[$category];

    # routine to install and check options
    my $add_option = sub {
        my ( $long_name, $short_name, $flag ) = @_;
        push @option_string, $long_name . $flag;
        $option_category{$long_name} = $category_name[$category];
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
    # 'n' for abbreviations).  Categories follow the manual.

    ###########################
    $category = 0;    # I/O_Control
    ###########################
    $add_option->( 'backup-and-modify-in-place', 'b',     '!' );
    $add_option->( 'backup-file-extension',      'bext',  '=s' );
    $add_option->( 'force-read-binary',          'f',     '!' );
    $add_option->( 'format',                     'fmt',   '=s' );
    $add_option->( 'logfile',                    'log',   '!' );
    $add_option->( 'logfile-gap',                'g',     ':i' );
    $add_option->( 'outfile',                    'o',     '=s' );
    $add_option->( 'output-file-extension',      'oext',  '=s' );
    $add_option->( 'output-path',                'opath', '=s' );
    $add_option->( 'profile',                    'pro',   '=s' );
    $add_option->( 'quiet',                      'q',     '!' );
    $add_option->( 'standard-error-output',      'se',    '!' );
    $add_option->( 'standard-output',            'st',    '!' );
    $add_option->( 'warning-output',             'w',     '!' );

    # options which are both toggle switches and values moved here
    # to hide from tidyview (which does not show category 0 flags):
    # -ole moved here from category 1
    # -sil moved here from category 2
    $add_option->( 'output-line-ending',         'ole', '=s' );
    $add_option->( 'starting-indentation-level', 'sil', '=i' );

    ########################################
    $category = 1;    # Basic formatting options
    ########################################
    $add_option->( 'check-syntax',             'syn',  '!' );
    $add_option->( 'entab-leading-whitespace', 'et',   '=i' );
    $add_option->( 'indent-columns',           'i',    '=i' );
    $add_option->( 'maximum-line-length',      'l',    '=i' );
    $add_option->( 'perl-syntax-check-flags',  'pscf', '=s' );
    $add_option->( 'preserve-line-endings',    'ple',  '!' );
    $add_option->( 'tabs',                     't',    '!' );

    ########################################
    $category = 2;    # Code indentation control
    ########################################
    $add_option->( 'continuation-indentation',           'ci',   '=i' );
    $add_option->( 'line-up-parentheses',                'lp',   '!' );
    $add_option->( 'outdent-keyword-list',               'okwl', '=s' );
    $add_option->( 'outdent-keywords',                   'okw',  '!' );
    $add_option->( 'outdent-labels',                     'ola',  '!' );
    $add_option->( 'outdent-long-quotes',                'olq',  '!' );
    $add_option->( 'indent-closing-brace',               'icb',  '!' );
    $add_option->( 'closing-token-indentation',          'cti',  '=i' );
    $add_option->( 'closing-paren-indentation',          'cpi',  '=i' );
    $add_option->( 'closing-brace-indentation',          'cbi',  '=i' );
    $add_option->( 'closing-square-bracket-indentation', 'csbi', '=i' );
    $add_option->( 'brace-left-and-indent',              'bli',  '!' );
    $add_option->( 'brace-left-and-indent-list',         'blil', '=s' );

    ########################################
    $category = 3;    # Whitespace control
    ########################################
    $add_option->( 'add-semicolons',                            'asc',   '!' );
    $add_option->( 'add-whitespace',                            'aws',   '!' );
    $add_option->( 'block-brace-tightness',                     'bbt',   '=i' );
    $add_option->( 'brace-tightness',                           'bt',    '=i' );
    $add_option->( 'delete-old-whitespace',                     'dws',   '!' );
    $add_option->( 'delete-semicolons',                         'dsm',   '!' );
    $add_option->( 'nospace-after-keyword',                     'nsak',  '=s' );
    $add_option->( 'nowant-left-space',                         'nwls',  '=s' );
    $add_option->( 'nowant-right-space',                        'nwrs',  '=s' );
    $add_option->( 'paren-tightness',                           'pt',    '=i' );
    $add_option->( 'space-after-keyword',                       'sak',   '=s' );
    $add_option->( 'space-for-semicolon',                       'sfs',   '!' );
    $add_option->( 'space-function-paren',                      'sfp',   '!' );
    $add_option->( 'space-keyword-paren',                       'skp',   '!' );
    $add_option->( 'space-terminal-semicolon',                  'sts',   '!' );
    $add_option->( 'square-bracket-tightness',                  'sbt',   '=i' );
    $add_option->( 'square-bracket-vertical-tightness',         'sbvt',  '=i' );
    $add_option->( 'square-bracket-vertical-tightness-closing', 'sbvtc', '=i' );
    $add_option->( 'trim-qw',                                   'tqw',   '!' );
    $add_option->( 'want-left-space',                           'wls',   '=s' );
    $add_option->( 'want-right-space',                          'wrs',   '=s' );

    ########################################
    $category = 4;    # Comment controls
    ########################################
    $add_option->( 'closing-side-comment-else-flag',    'csce', '=i' );
    $add_option->( 'closing-side-comment-interval',     'csci', '=i' );
    $add_option->( 'closing-side-comment-list',         'cscl', '=s' );
    $add_option->( 'closing-side-comment-maximum-text', 'csct', '=i' );
    $add_option->( 'closing-side-comment-prefix',       'cscp', '=s' );
    $add_option->( 'closing-side-comment-warnings',     'cscw', '!' );
    $add_option->( 'closing-side-comments',             'csc',  '!' );
    $add_option->( 'format-skipping',                   'fs',   '!' );
    $add_option->( 'format-skipping-begin',             'fsb',  '=s' );
    $add_option->( 'format-skipping-end',               'fse',  '=s' );
    $add_option->( 'hanging-side-comments',             'hsc',  '!' );
    $add_option->( 'indent-block-comments',             'ibc',  '!' );
    $add_option->( 'indent-spaced-block-comments',      'isbc', '!' );
    $add_option->( 'fixed-position-side-comment',       'fpsc', '=i' );
    $add_option->( 'minimum-space-to-comment',          'msc',  '=i' );
    $add_option->( 'outdent-long-comments',             'olc',  '!' );
    $add_option->( 'outdent-static-block-comments',     'osbc', '!' );
    $add_option->( 'static-block-comment-prefix',       'sbcp', '=s' );
    $add_option->( 'static-block-comments',             'sbc',  '!' );
    $add_option->( 'static-side-comment-prefix',        'sscp', '=s' );
    $add_option->( 'static-side-comments',              'ssc',  '!' );

    ########################################
    $category = 5;    # Linebreak controls
    ########################################
    $add_option->( 'add-newlines',                        'anl',   '!' );
    $add_option->( 'block-brace-vertical-tightness',      'bbvt',  '=i' );
    $add_option->( 'block-brace-vertical-tightness-list', 'bbvtl', '=s' );
    $add_option->( 'brace-vertical-tightness',            'bvt',   '=i' );
    $add_option->( 'brace-vertical-tightness-closing',    'bvtc',  '=i' );
    $add_option->( 'cuddled-else',                        'ce',    '!' );
    $add_option->( 'delete-old-newlines',                 'dnl',   '!' );
    $add_option->( 'opening-brace-always-on-right',       'bar',   '!' );
    $add_option->( 'opening-brace-on-new-line',           'bl',    '!' );
    $add_option->( 'opening-hash-brace-right',            'ohbr',  '!' );
    $add_option->( 'opening-paren-right',                 'opr',   '!' );
    $add_option->( 'opening-square-bracket-right',        'osbr',  '!' );
    $add_option->( 'opening-sub-brace-on-new-line',       'sbl',   '!' );
    $add_option->( 'paren-vertical-tightness',            'pvt',   '=i' );
    $add_option->( 'paren-vertical-tightness-closing',    'pvtc',  '=i' );
    $add_option->( 'stack-closing-hash-brace',            'schb',  '!' );
    $add_option->( 'stack-closing-paren',                 'scp',   '!' );
    $add_option->( 'stack-closing-square-bracket',        'scsb',  '!' );
    $add_option->( 'stack-opening-hash-brace',            'sohb',  '!' );
    $add_option->( 'stack-opening-paren',                 'sop',   '!' );
    $add_option->( 'stack-opening-square-bracket',        'sosb',  '!' );
    $add_option->( 'vertical-tightness',                  'vt',    '=i' );
    $add_option->( 'vertical-tightness-closing',          'vtc',   '=i' );
    $add_option->( 'want-break-after',                    'wba',   '=s' );
    $add_option->( 'want-break-before',                   'wbb',   '=s' );
    $add_option->( 'break-after-all-operators',           'baao',  '!' );
    $add_option->( 'break-before-all-operators',          'bbao',  '!' );
    $add_option->( 'keep-interior-semicolons',            'kis',   '!' );

    ########################################
    $category = 6;    # Controlling list formatting
    ########################################
    $add_option->( 'break-at-old-comma-breakpoints', 'boc', '!' );
    $add_option->( 'comma-arrow-breakpoints',        'cab', '=i' );
    $add_option->( 'maximum-fields-per-table',       'mft', '=i' );

    ########################################
    $category = 7;    # Retaining or ignoring existing line breaks
    ########################################
    $add_option->( 'break-at-old-keyword-breakpoints', 'bok', '!' );
    $add_option->( 'break-at-old-logical-breakpoints', 'bol', '!' );
    $add_option->( 'break-at-old-ternary-breakpoints', 'bot', '!' );
    $add_option->( 'ignore-old-breakpoints',           'iob', '!' );

    ########################################
    $category = 8;    # Blank line control
    ########################################
    $add_option->( 'blanks-before-blocks',            'bbb', '!' );
    $add_option->( 'blanks-before-comments',          'bbc', '!' );
    $add_option->( 'blanks-before-subs',              'bbs', '!' );
    $add_option->( 'long-block-line-count',           'lbl', '=i' );
    $add_option->( 'maximum-consecutive-blank-lines', 'mbl', '=i' );
    $add_option->( 'swallow-optional-blank-lines',    'sob', '!' );

    ########################################
    $category = 9;    # Other controls
    ########################################
    $add_option->( 'delete-block-comments',        'dbc',  '!' );
    $add_option->( 'delete-closing-side-comments', 'dcsc', '!' );
    $add_option->( 'delete-pod',                   'dp',   '!' );
    $add_option->( 'delete-side-comments',         'dsc',  '!' );
    $add_option->( 'tee-block-comments',           'tbc',  '!' );
    $add_option->( 'tee-pod',                      'tp',   '!' );
    $add_option->( 'tee-side-comments',            'tsc',  '!' );
    $add_option->( 'look-for-autoloader',          'lal',  '!' );
    $add_option->( 'look-for-hash-bang',           'x',    '!' );
    $add_option->( 'look-for-selfloader',          'lsl',  '!' );
    $add_option->( 'pass-version-line',            'pvl',  '!' );

    ########################################
    $category = 13;    # Debugging
    ########################################
    $add_option->( 'DEBUG',                           'D',    '!' );
    $add_option->( 'DIAGNOSTICS',                     'I',    '!' );
    $add_option->( 'check-multiline-quotes',          'chk',  '!' );
    $add_option->( 'dump-defaults',                   'ddf',  '!' );
    $add_option->( 'dump-long-names',                 'dln',  '!' );
    $add_option->( 'dump-options',                    'dop',  '!' );
    $add_option->( 'dump-profile',                    'dpro', '!' );
    $add_option->( 'dump-short-names',                'dsn',  '!' );
    $add_option->( 'dump-token-types',                'dtt',  '!' );
    $add_option->( 'dump-want-left-space',            'dwls', '!' );
    $add_option->( 'dump-want-right-space',           'dwrs', '!' );
    $add_option->( 'fuzzy-line-length',               'fll',  '!' );
    $add_option->( 'help',                            'h',    '' );
    $add_option->( 'short-concatenation-item-length', 'scl',  '=i' );
    $add_option->( 'show-options',                    'opt',  '!' );
    $add_option->( 'version',                         'v',    '' );

    #---------------------------------------------------------------------

    # The Perl::Tidy::HtmlWriter will add its own options to the string
    Perl::Tidy::HtmlWriter->make_getopt_long_names( \@option_string );

    ########################################
    # Set categories 10, 11, 12
    ########################################
    # Based on their known order
    $category = 12;    # HTML properties
    foreach my $opt (@option_string) {
        my $long_name = $opt;
        $long_name =~ s/(!|=.*|:.*)$//;
        unless ( defined( $option_category{$long_name} ) ) {
            if ( $long_name =~ /^html-linked/ ) {
                $category = 10;    # HTML options
            }
            elsif ( $long_name =~ /^pod2html/ ) {
                $category = 11;    # Pod2html
            }
            $option_category{$long_name} = $category_name[$category];
        }
    }

    #---------------------------------------------------------------
    # Assign valid ranges to certain options
    #---------------------------------------------------------------
    # In the future, these may be used to make preliminary checks
    # hash keys are long names
    # If key or value is undefined:
    #   strings may have any value
    #   integer ranges are >=0
    # If value is defined:
    #   value is [qw(any valid words)] for strings
    #   value is [min, max] for integers
    #   if min is undefined, there is no lower limit
    #   if max is undefined, there is no upper limit
    # Parameters not listed here have defaults
    %option_range = (
        'format'             => [ 'tidy', 'html', 'user' ],
        'output-line-ending' => [ 'dos',  'win',  'mac', 'unix' ],

        'block-brace-tightness'    => [ 0, 2 ],
        'brace-tightness'          => [ 0, 2 ],
        'paren-tightness'          => [ 0, 2 ],
        'square-bracket-tightness' => [ 0, 2 ],

        'block-brace-vertical-tightness'            => [ 0, 2 ],
        'brace-vertical-tightness'                  => [ 0, 2 ],
        'brace-vertical-tightness-closing'          => [ 0, 2 ],
        'paren-vertical-tightness'                  => [ 0, 2 ],
        'paren-vertical-tightness-closing'          => [ 0, 2 ],
        'square-bracket-vertical-tightness'         => [ 0, 2 ],
        'square-bracket-vertical-tightness-closing' => [ 0, 2 ],
        'vertical-tightness'                        => [ 0, 2 ],
        'vertical-tightness-closing'                => [ 0, 2 ],

        'closing-brace-indentation'          => [ 0, 3 ],
        'closing-paren-indentation'          => [ 0, 3 ],
        'closing-square-bracket-indentation' => [ 0, 3 ],
        'closing-token-indentation'          => [ 0, 3 ],

        'closing-side-comment-else-flag' => [ 0, 2 ],
        'comma-arrow-breakpoints'        => [ 0, 3 ],
    );

    # Note: we could actually allow negative ci if someone really wants it:
    # $option_range{'continuation-indentation'} = [ undef, undef ];

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
      break-at-old-ternary-breakpoints
      break-at-old-keyword-breakpoints
      comma-arrow-breakpoints=1
      nocheck-syntax
      closing-side-comment-interval=6
      closing-side-comment-maximum-text=20
      closing-side-comment-else-flag=0
      closing-paren-indentation=0
      closing-brace-indentation=0
      closing-square-bracket-indentation=0
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
      nologfile
      noquiet
      noshow-options
      nostatic-side-comments
      noswallow-optional-blank-lines
      notabs
      nowarning-output
      outdent-labels
      outdent-long-quotes
      outdent-long-comments
      paren-tightness=1
      paren-vertical-tightness-closing=0
      paren-vertical-tightness=0
      pass-version-line
      recombine
      valign
      short-concatenation-item-length=8
      space-for-semicolon
      square-bracket-tightness=1
      square-bracket-vertical-tightness-closing=0
      square-bracket-vertical-tightness=0
      static-block-comments
      trim-qw
      format=tidy
      backup-file-extension=bak
      format-skipping

      pod2html
      html-table-of-contents
      html-entities
    );

    push @defaults, "perl-syntax-check-flags=-c -T";

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
        'noll' => [qw(nooutdent-long-lines)],
        'io'   => [qw(indent-only)],
        'delete-all-comments' =>
          [qw(delete-block-comments delete-side-comments delete-pod)],
        'nodelete-all-comments' =>
          [qw(nodelete-block-comments nodelete-side-comments nodelete-pod)],
        'dac'  => [qw(delete-all-comments)],
        'ndac' => [qw(nodelete-all-comments)],
        'gnu'  => [qw(gnu-style)],
        'pbp'  => [qw(perl-best-practices)],
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

        'break-at-old-trinary-breakpoints' => [qw(bot)],

        'cti=0' => [qw(cpi=0 cbi=0 csbi=0)],
        'cti=1' => [qw(cpi=1 cbi=1 csbi=1)],
        'cti=2' => [qw(cpi=2 cbi=2 csbi=2)],
        'icp'   => [qw(cpi=2 cbi=2 csbi=2)],
        'nicp'  => [qw(cpi=0 cbi=0 csbi=0)],

        'closing-token-indentation=0' => [qw(cpi=0 cbi=0 csbi=0)],
        'closing-token-indentation=1' => [qw(cpi=1 cbi=1 csbi=1)],
        'closing-token-indentation=2' => [qw(cpi=2 cbi=2 csbi=2)],
        'indent-closing-paren'        => [qw(cpi=2 cbi=2 csbi=2)],
        'noindent-closing-paren'      => [qw(cpi=0 cbi=0 csbi=0)],

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

        'otr'                   => [qw(opr ohbr osbr)],
        'opening-token-right'   => [qw(opr ohbr osbr)],
        'notr'                  => [qw(nopr nohbr nosbr)],
        'noopening-token-right' => [qw(nopr nohbr nosbr)],

        'sot'                    => [qw(sop sohb sosb)],
        'nsot'                   => [qw(nsop nsohb nsosb)],
        'stack-opening-tokens'   => [qw(sop sohb sosb)],
        'nostack-opening-tokens' => [qw(nsop nsohb nsosb)],

        'sct'                    => [qw(scp schb scsb)],
        'stack-closing-tokens'   => => [qw(scp schb scsb)],
        'nsct'                   => [qw(nscp nschb nscsb)],
        'nostack-opening-tokens' => [qw(nscp nschb nscsb)],

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
              norecombine
              )
        ],

        # this style tries to follow the GNU Coding Standards (which do
        # not really apply to perl but which are followed by some perl
        # programmers).
        'gnu-style' => [
            qw(
              lp bl noll pt=2 bt=2 sbt=2 cpi=1 csbi=1 cbi=1
              )
        ],

        # Style suggested in Damian Conway's Perl Best Practices
        'perl-best-practices' => [
            qw(l=78 i=4 ci=4 st se vt=2 cti=0 pt=1 bt=1 sbt=1 bbt=1 nsfs nolq),
q(wbb=% + - * / x != == >= <= =~ !~ < > | & = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=)
        ],

        # Additional styles can be added here
    );

    Perl::Tidy::HtmlWriter->make_abbreviated_names( \%expansion );

    # Uncomment next line to dump all expansions for debugging:
    # dump_short_names(\%expansion);
    return (
        \@option_string,   \@defaults, \%expansion,
        \%option_category, \%option_range
    );

}    # end of generate_options

sub process_command_line {

    my (
        $perltidyrc_stream,  $is_Windows, $Windows_type,
        $rpending_complaint, $dump_options_type
    ) = @_;

    use Getopt::Long;

    my (
        $roption_string,   $rdefaults, $rexpansion,
        $roption_category, $roption_range
    ) = generate_options();

    #---------------------------------------------------------------
    # set the defaults by passing the above list through GetOptions
    #---------------------------------------------------------------
    my %Opts = ();
    {
        local @ARGV;
        my $i;

        # do not load the defaults if we are just dumping perltidyrc
        unless ( $dump_options_type eq 'perltidyrc' ) {
            for $i (@$rdefaults) { push @ARGV, "--" . $i }
        }

        # Patch to save users Getopt::Long configuration
        # and set to Getopt::Long defaults.  Use eval to avoid
        # breaking old versions of Perl without these routines.
        my $glc;
        eval { $glc = Getopt::Long::Configure() };
        unless ($@) {
            eval { Getopt::Long::ConfigDefaults() };
        }
        else { $glc = undef }

        if ( !GetOptions( \%Opts, @$roption_string ) ) {
            die "Programming Bug: error in setting default options";
        }

        # Patch to put the previous Getopt::Long configuration back
        eval { Getopt::Long::Configure($glc) } if defined $glc;
    }

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

        $i =~ s/^--/-/;
        if ( $i =~ /^-(npro|noprofile|no-profile)$/ ) {
            $saw_ignore_profile = 1;
        }

        # note: this must come before -pro and -profile, below:
        elsif ( $i =~ /^-(dump-profile|dpro)$/ ) {
            $saw_dump_profile = 1;
        }
        elsif ( $i =~ /^-(pro|profile)=(.+)/ ) {
            if ($config_file) {
                warn
"Only one -pro=filename allowed, using '$2' instead of '$config_file'\n";
            }
            $config_file = $2;
            unless ( -e $config_file ) {
                warn "cannot find file given with -pro=$config_file: $!\n";
                $config_file = "";
            }
        }
        elsif ( $i =~ /^-(pro|profile)=?$/ ) {
            die "usage: -pro=filename or --profile=filename, no spaces\n";
        }
        elsif ( $i =~ /^-extrude$/ ) {
            $saw_extrude = 1;
        }
        elsif ( $i =~ /^-(help|h|HELP|H)$/ ) {
            usage();
            exit 1;
        }
        elsif ( $i =~ /^-(version|v)$/ ) {
            show_version();
            exit 1;
        }
        elsif ( $i =~ /^-(dump-defaults|ddf)$/ ) {
            dump_defaults(@$rdefaults);
            exit 1;
        }
        elsif ( $i =~ /^-(dump-long-names|dln)$/ ) {
            dump_long_names(@$roption_string);
            exit 1;
        }
        elsif ( $i =~ /^-(dump-short-names|dsn)$/ ) {
            dump_short_names($rexpansion);
            exit 1;
        }
        elsif ( $i =~ /^-(dump-token-types|dtt)$/ ) {
            Perl::Tidy::Tokenizer->dump_token_types(*STDOUT);
            exit 1;
        }
    }

    if ( $saw_dump_profile && $saw_ignore_profile ) {
        warn "No profile to dump because of -npro\n";
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
                warn <<EOM;
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
        $config_file =
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

            my ( $rconfig_list, $death_message ) =
              read_config_file( $fh_config, $config_file, $rexpansion );
            die $death_message if ($death_message);

            # process any .perltidyrc parameters right now so we can
            # localize errors
            if (@$rconfig_list) {
                local @ARGV = @$rconfig_list;

                expand_command_abbreviations( $rexpansion, \@raw_options,
                    $config_file );

                if ( !GetOptions( \%Opts, @$roption_string ) ) {
                    die
"Error in this config file: $config_file  \nUse -npro to ignore this file, -h for help'\n";
                }

                # Anything left in this local @ARGV is an error and must be
                # invalid bare words from the configuration file.  We cannot
                # check this earlier because bare words may have been valid
                # values for parameters.  We had to wait for GetOptions to have
                # a look at @ARGV.
                if (@ARGV) {
                    my $count = @ARGV;
                    my $str   = "\'" . pop(@ARGV) . "\'";
                    while ( my $param = pop(@ARGV) ) {
                        if ( length($str) < 70 ) {
                            $str .= ", '$param'";
                        }
                        else {
                            $str .= ", ...";
                            last;
                        }
                    }
                    die <<EOM;
There are $count unrecognized values in the configuration file '$config_file':
$str
Use leading dashes for parameters.  Use -npro to ignore this file.
EOM
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
                        warn "ignoring --$_ in config file: $config_file\n";
                    }
                }
            }
        }
    }

    #---------------------------------------------------------------
    # now process the command line parameters
    #---------------------------------------------------------------
    expand_command_abbreviations( $rexpansion, \@raw_options, $config_file );

    if ( !GetOptions( \%Opts, @$roption_string ) ) {
        die "Error on command line; for help try 'perltidy -h'\n";
    }

    return ( \%Opts, $config_file, \@raw_options, $saw_extrude, $roption_string,
        $rexpansion, $roption_category, $roption_range );
}    # end of process_command_line

sub check_options {

    my ( $rOpts, $is_Windows, $Windows_type, $rpending_complaint ) = @_;

    #---------------------------------------------------------------
    # check and handle any interactions among the basic options..
    #---------------------------------------------------------------

    # Since -vt, -vtc, and -cti are abbreviations, but under
    # msdos, an unquoted input parameter like vtc=1 will be
    # seen as 2 parameters, vtc and 1, so the abbreviations
    # won't be seen.  Therefore, we will catch them here if
    # they get through.

    if ( defined $rOpts->{'vertical-tightness'} ) {
        my $vt = $rOpts->{'vertical-tightness'};
        $rOpts->{'paren-vertical-tightness'}          = $vt;
        $rOpts->{'square-bracket-vertical-tightness'} = $vt;
        $rOpts->{'brace-vertical-tightness'}          = $vt;
    }

    if ( defined $rOpts->{'vertical-tightness-closing'} ) {
        my $vtc = $rOpts->{'vertical-tightness-closing'};
        $rOpts->{'paren-vertical-tightness-closing'}          = $vtc;
        $rOpts->{'square-bracket-vertical-tightness-closing'} = $vtc;
        $rOpts->{'brace-vertical-tightness-closing'}          = $vtc;
    }

    if ( defined $rOpts->{'closing-token-indentation'} ) {
        my $cti = $rOpts->{'closing-token-indentation'};
        $rOpts->{'closing-square-bracket-indentation'} = $cti;
        $rOpts->{'closing-brace-indentation'}          = $cti;
        $rOpts->{'closing-paren-indentation'}          = $cti;
    }

    # In quiet mode, there is no log file and hence no way to report
    # results of syntax check, so don't do it.
    if ( $rOpts->{'quiet'} ) {
        $rOpts->{'check-syntax'} = 0;
    }

    # can't check syntax if no output
    if ( $rOpts->{'format'} ne 'tidy' ) {
        $rOpts->{'check-syntax'} = 0;
    }

    # Never let Windows 9x/Me systems run syntax check -- this will prevent a
    # wide variety of nasty problems on these systems, because they cannot
    # reliably run backticks.  Don't even think about changing this!
    if (   $rOpts->{'check-syntax'}
        && $is_Windows
        && ( !$Windows_type || $Windows_type =~ /^(9|Me)/ ) )
    {
        $rOpts->{'check-syntax'} = 0;
    }

    # It's really a bad idea to check syntax as root unless you wrote
    # the script yourself.  FIXME: not sure if this works with VMS
    unless ($is_Windows) {

        if ( $< == 0 && $rOpts->{'check-syntax'} ) {
            $rOpts->{'check-syntax'} = 0;
            $$rpending_complaint .=
"Syntax check deactivated for safety; you shouldn't run this as root\n";
        }
    }

    # see if user set a non-negative logfile-gap
    if ( defined( $rOpts->{'logfile-gap'} ) && $rOpts->{'logfile-gap'} >= 0 ) {

        # a zero gap will be taken as a 1
        if ( $rOpts->{'logfile-gap'} == 0 ) {
            $rOpts->{'logfile-gap'} = 1;
        }

        # setting a non-negative logfile gap causes logfile to be saved
        $rOpts->{'logfile'} = 1;
    }

    # not setting logfile gap, or setting it negative, causes default of 50
    else {
        $rOpts->{'logfile-gap'} = 50;
    }

    # set short-cut flag when only indentation is to be done.
    # Note that the user may or may not have already set the
    # indent-only flag.
    if (   !$rOpts->{'add-whitespace'}
        && !$rOpts->{'delete-old-whitespace'}
        && !$rOpts->{'add-newlines'}
        && !$rOpts->{'delete-old-newlines'} )
    {
        $rOpts->{'indent-only'} = 1;
    }

    # -isbc implies -ibc
    if ( $rOpts->{'indent-spaced-block-comments'} ) {
        $rOpts->{'indent-block-comments'} = 1;
    }

    # -bli flag implies -bl
    if ( $rOpts->{'brace-left-and-indent'} ) {
        $rOpts->{'opening-brace-on-new-line'} = 1;
    }

    if (   $rOpts->{'opening-brace-always-on-right'}
        && $rOpts->{'opening-brace-on-new-line'} )
    {
        warn <<EOM;
 Conflict: you specified both 'opening-brace-always-on-right' (-bar) and 
  'opening-brace-on-new-line' (-bl).  Ignoring -bl. 
EOM
        $rOpts->{'opening-brace-on-new-line'} = 0;
    }

    # it simplifies things if -bl is 0 rather than undefined
    if ( !defined( $rOpts->{'opening-brace-on-new-line'} ) ) {
        $rOpts->{'opening-brace-on-new-line'} = 0;
    }

    # -sbl defaults to -bl if not defined
    if ( !defined( $rOpts->{'opening-sub-brace-on-new-line'} ) ) {
        $rOpts->{'opening-sub-brace-on-new-line'} =
          $rOpts->{'opening-brace-on-new-line'};
    }

    # set shortcut flag if no blanks to be written
    unless ( $rOpts->{'maximum-consecutive-blank-lines'} ) {
        $rOpts->{'swallow-optional-blank-lines'} = 1;
    }

    if ( $rOpts->{'entab-leading-whitespace'} ) {
        if ( $rOpts->{'entab-leading-whitespace'} < 0 ) {
            warn "-et=n must use a positive integer; ignoring -et\n";
            $rOpts->{'entab-leading-whitespace'} = undef;
        }

        # entab leading whitespace has priority over the older 'tabs' option
        if ( $rOpts->{'tabs'} ) { $rOpts->{'tabs'} = 0; }
    }
}

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

            # convert any leading 'no-' to just 'no'
            if ( $word =~ /^(-[-]?no)-(.*)/ ) { $word = $1 . $2 }

            # if it is a dash flag (instead of a file name)..
            if ( $word =~ /^-[-]?([\w\-]+)(.*)/ ) {

                my $abr   = $1;
                my $flags = $2;

                # save the raw input for debug output in case of circular refs
                if ( $pass_count == 0 ) {
                    push( @$rraw_options, $word );
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
                        push( @new_argv, '--' . $abbrev . $flags );
                    }
                }

                # not in expansion hash, must be actual long name
                else {
                    push( @new_argv, $word );
                }
            }

            # not a dash item, so just save it for the next pass
            else {
                push( @new_argv, $word );
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

    # TODO: are these more standard names?
    # Win32s Win95 Win98 WinMe WinNT3.51 WinNT4 Win2000 WinXP/.Net Win2003

    # Returns a string that determines what MS OS we are on.
    # Returns win32s,95,98,Me,NT3.51,NT4,2000,XP/.Net,Win2003
    # Returns blank string if not an MS system.
    # Original code contributed by: Yves Orton
    # We need to know this to decide where to look for config files

    my $rpending_complaint = shift;
    my $os                 = "";
    return $os unless $^O =~ /win32|dos/i;    # is it a MS box?

    # Systems built from Perl source may not have Win32.pm
    # But probably have Win32::GetOSVersion() anyway so the
    # following line is not 'required':
    # return $os unless eval('require Win32');

    # Use the standard API call to determine the version
    my ( $undef, $major, $minor, $build, $id );
    eval { ( $undef, $major, $minor, $build, $id ) = Win32::GetOSVersion() };

    #
    #    NAME                   ID   MAJOR  MINOR
    #    Windows NT 4           2      4       0
    #    Windows 2000           2      5       0
    #    Windows XP             2      5       1
    #    Windows Server 2003    2      5       2

    return "win32s" unless $id;    # If id==0 then its a win32s box.
    $os = {                        # Magic numbers from MSDN
                                   # documentation of GetOSVersion
        1 => {
            0  => "95",
            10 => "98",
            90 => "Me"
        },
        2 => {
            0  => "2000",          # or NT 4, see below
            1  => "XP/.Net",
            2  => "Win2003",
            51 => "NT3.51"
        }
    }->{$id}->{$minor};

    # If $os is undefined, the above code is out of date.  Suggested updates
    # are welcome.
    unless ( defined $os ) {
        $os = "";
        $$rpending_complaint .= <<EOS;
Error trying to discover Win_OS_Type: $id:$major:$minor Has no name of record!
We won't be able to look for a system-wide config file.
EOS
    }

    # Unfortunately the logic used for the various versions isnt so clever..
    # so we have to handle an outside case.
    return ( $os eq "2000" && $major != 5 ) ? "NT4" : $os;
}

sub is_unix {
    return
         ( $^O !~ /win32|dos/i )
      && ( $^O ne 'VMS' )
      && ( $^O ne 'OS2' )
      && ( $^O ne 'MacOS' );
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
    my $os = (@_) ? shift : Win_OS_Type();
    return unless $os;

    my $system   = "";
    my $allusers = "";

    if ( $os =~ /9[58]|Me/ ) {
        $system = "C:/Windows";
    }
    elsif ( $os =~ /NT|XP|200?/ ) {
        $system = ( $os =~ /XP/ ) ? "C:/Windows/" : "C:/WinNT/";
        $allusers =
          ( $os =~ /NT/ )
          ? "C:/WinNT/profiles/All Users/"
          : "C:/Documents and Settings/All Users/";
    }
    else {

        # This currently would only happen on a win32s computer.  I dont have
        # one to test, so I am unsure how to proceed.  Suggestions welcome!
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
        while ( my $line = $fh->getline() ) { print STDOUT $line }
        eval { $fh->close() };
    }
    else {
        print STDOUT "# ...no config file found\n";
    }
}

sub read_config_file {

    my ( $fh, $config_file, $rexpansion ) = @_;
    my @config_list = ();

    # file is bad if non-empty $death_message is returned
    my $death_message = "";

    my $name = undef;
    my $line_no;
    while ( my $line = $fh->getline() ) {
        $line_no++;
        chomp $line;
        next if $line =~ /^\s*#/;    # skip full-line comment
        ( $line, $death_message ) =
          strip_comment( $line, $config_file, $line_no );
        last if ($death_message);
        $line =~ s/^\s*(.*?)\s*$/$1/;    # trim both ends
        next unless $line;

        # look for something of the general form
        #    newname { body }
        # or just
        #    body

        if ( $line =~ /^((\w+)\s*\{)?([^}]*)(\})?$/ ) {
            my ( $newname, $body, $curly ) = ( $2, $3, $4 );

            # handle a new alias definition
            if ($newname) {
                if ($name) {
                    $death_message =
"No '}' seen after $name and before $newname in config file $config_file line $.\n";
                    last;
                }
                $name = $newname;

                if ( ${$rexpansion}{$name} ) {
                    local $" = ')(';
                    my @names = sort keys %$rexpansion;
                    $death_message =
                        "Here is a list of all installed aliases\n(@names)\n"
                      . "Attempting to redefine alias ($name) in config file $config_file line $.\n";
                    last;
                }
                ${$rexpansion}{$name} = [];
            }

            # now do the body
            if ($body) {

                my ( $rbody_parts, $msg ) = parse_args($body);
                if ($msg) {
                    $death_message = <<EOM;
Error reading file '$config_file' at line number $line_no.
$msg
Please fix this line or use -npro to avoid reading this file
EOM
                    last;
                }

                if ($name) {

                    # remove leading dashes if this is an alias
                    foreach (@$rbody_parts) { s/^\-+//; }
                    push @{ ${$rexpansion}{$name} }, @$rbody_parts;
                }
                else {
                    push( @config_list, @$rbody_parts );
                }
            }

            if ($curly) {
                unless ($name) {
                    $death_message =
"Unexpected '}' seen in config file $config_file line $.\n";
                    last;
                }
                $name = undef;
            }
        }
    }
    eval { $fh->close() };
    return ( \@config_list, $death_message );
}

sub strip_comment {

    my ( $instr, $config_file, $line_no ) = @_;
    my $msg = "";

    # nothing to do if no comments
    if ( $instr !~ /#/ ) {
        return ( $instr, $msg );
    }

    # use simple method of no quotes
    elsif ( $instr !~ /['"]/ ) {
        $instr =~ s/\s*\#.*$//;    # simple trim
        return ( $instr, $msg );
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
                $msg = <<EOM;
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
    return ( $outstr, $msg );
}

sub parse_args {

    # Parse a command string containing multiple string with possible
    # quotes, into individual commands.  It might look like this, for example:
    #
    #    -wba=" + - "  -some-thing -wbb='. && ||'
    #
    # There is no need, at present, to handle escaped quote characters.
    # (They are not perltidy tokens, so needn't be in strings).

    my ($body)     = @_;
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
                if ( length($part) ) { push @body_parts, $part; }
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
                if ( length($part) ) { push @body_parts, $part; }
                $part = "";
            }
            elsif ( $body =~ /\G(.)/gc ) {
                $part .= $1;
            }
            else {
                if ( length($part) ) { push @body_parts, $part; }
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

sub readable_options {

    # return options for this run as a string which could be
    # put in a perltidyrc file
    my ( $rOpts, $roption_string ) = @_;
    my %Getopt_flags;
    my $rGetopt_flags    = \%Getopt_flags;
    my $readable_options = "# Final parameter set for this run.\n";
    $readable_options .=
      "# See utility 'perltidyrc_dump.pl' for nicer formatting.\n";
    foreach my $opt ( @{$roption_string} ) {
        my $flag = "";
        if ( $opt =~ /(.*)(!|=.*)$/ ) {
            $opt  = $1;
            $flag = $2;
        }
        if ( defined( $rOpts->{$opt} ) ) {
            $rGetopt_flags->{$opt} = $flag;
        }
    }
    foreach my $key ( sort keys %{$rOpts} ) {
        my $flag   = $rGetopt_flags->{$key};
        my $value  = $rOpts->{$key};
        my $prefix = '--';
        my $suffix = "";
        if ($flag) {
            if ( $flag =~ /^=/ ) {
                if ( $value !~ /^\d+$/ ) { $value = '"' . $value . '"' }
                $suffix = "=" . $value;
            }
            elsif ( $flag =~ /^!/ ) {
                $prefix .= "no" unless ($value);
            }
            else {

                # shouldn't happen
                $readable_options .=
                  "# ERROR in dump_options: unrecognized flag $flag for $key\n";
            }
        }
        $readable_options .= $prefix . $key . $suffix . "\n";
    }
    return $readable_options;
}

sub show_version {
    print <<"EOM";
This is perltidy, v$VERSION 

Copyright 2000-2007, Steve Hancock

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
 -ole=s  specify output line ending (s=dos or win, mac, unix)
 -ple    keep output line endings same as input (input must be filename)

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
 -cti=n  closing indentation of paren, square bracket, or non-block brace: 
         n=0 none, =1 align with opening, =2 one full indentation level
 -icp    equivalent to -cti=2
 -wls=s  want space left of tokens in string; i.e. -nwls='+ - * /'
 -wrs=s  want space right of tokens in string;
 -sts    put space before terminal semicolon of a statement
 -sak=s  put space between keywords given in s and '(';
 -nsak=s no space between keywords in s and '('; i.e. -nsak='my our local'

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
 -kis    keep interior semicolons.  Allows multiple statements per line.
 -boc    break at old comma breaks: turns off all automatic list formatting
 -bol    break at old logical breakpoints: or, and, ||, && (default)
 -bok    break at old list keyword breakpoints such as map, sort (default)
 -bot    break at old conditional (ternary ?:) operator breakpoints (default)
 -cab=n  break at commas after a comma-arrow (=>):
         n=0 break at all commas after =>
         n=1 stable: break unless this breaks an existing one-line container
         n=2 break only if a one-line container cannot be formed
         n=3 do not treat commas after => specially at all

Comment controls
 -ibc    indent block comments (default)
 -isbc   indent spaced block comments; may indent unless no leading space
 -msc=n  minimum desired spaces to side comment, default 4
 -fpsc=n fix position for side comments; default 0;
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
 -html write an html file (see 'man perl2web' for many options)
       Note: when -html is used, no indentation or formatting are done.
       Hint: try perltidy -html -css=mystyle.css filename.pl
       and edit mystyle.css to change the appearance of filename.html.
       -nnn gives line numbers
       -pre only writes out <pre>..</pre> code section
       -toc places a table of contents to subs at the top (default)
       -pod passes pod text through pod2html (default)
       -frm write html as a frame (3 files)
       -text=s extra extension for table of contents if -frm, default='toc'
       -sext=s extra extension for file content if -frm, default='src'

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
    eval { $beauty->finish_formatting() };
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
        push( @$rlookahead_buffer, $line );
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

# PACKAGE VARIABLES for for processing an entire FILE.
use vars qw{
  $tokenizer_self

  $last_nonblank_token
  $last_nonblank_type
  $last_nonblank_block_type
  $statement_type
  $in_attribute_list
  $current_package
  $context

  %is_constant
  %is_user_function
  %user_function_prototype
  %is_block_function
  %is_block_list_function
  %saw_function_definition

  $brace_depth
  $paren_depth
  $square_bracket_depth

  @current_depth
  @total_depth
  $total_depth
  @nesting_sequence_number
  @current_sequence_number
  @paren_type
  @paren_semicolon_count
  @paren_structural_type
  @brace_type
  @brace_structural_type
  @brace_statement_type
  @brace_context
  @brace_package
  @square_bracket_type
  @square_bracket_structural_type
  @depth_array
  @nested_ternary_flag
  @starting_line_of_current_depth
};

# GLOBAL CONSTANTS for routines in this package
use vars qw{
  %is_indirect_object_taker
  %is_block_operator
  %expecting_operator_token
  %expecting_operator_types
  %expecting_term_types
  %expecting_term_token
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
  %is_q_qq_qw_qx_qr_s_y_tr_m
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
        source_object        => undef,
        debugger_object      => undef,
        diagnostics_object   => undef,
        logger_object        => undef,
        starting_level       => undef,
        indent_columns       => 4,
        tabs                 => 0,
        look_for_hash_bang   => 0,
        trim_qw              => 1,
        look_for_autoloader  => 1,
        look_for_selfloader  => 1,
        starting_line_number => 1,
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
    # _in_attribute_list    flag telling if we are looking for attributes
    # _in_quote             flag telling if we are chasing a quote
    # _starting_level       indentation level of first line
    # _input_tabstr         string denoting one indentation level of input file
    # _know_input_tabstr    flag indicating if we know _input_tabstr
    # _line_buffer_object   object with get_line() method to supply source code
    # _diagnostics_object   place to write debugging information
    # _unexpected_error_count  error count used to limit output
    # _lower_case_labels_at  line numbers where lower case labels seen
    $tokenizer_self = {
        _rhere_target_list                  => [],
        _in_here_doc                        => 0,
        _here_doc_target                    => "",
        _here_quote_character               => "",
        _in_data                            => 0,
        _in_end                             => 0,
        _in_format                          => 0,
        _in_error                           => 0,
        _in_pod                             => 0,
        _in_attribute_list                  => 0,
        _in_quote                           => 0,
        _quote_target                       => "",
        _line_start_quote                   => -1,
        _starting_level                     => $args{starting_level},
        _know_starting_level                => defined( $args{starting_level} ),
        _tabs                               => $args{tabs},
        _indent_columns                     => $args{indent_columns},
        _look_for_hash_bang                 => $args{look_for_hash_bang},
        _trim_qw                            => $args{trim_qw},
        _input_tabstr                       => "",
        _know_input_tabstr                  => -1,
        _last_line_number                   => $args{starting_line_number} - 1,
        _saw_perl_dash_P                    => 0,
        _saw_perl_dash_w                    => 0,
        _saw_use_strict                     => 0,
        _saw_v_string                       => 0,
        _look_for_autoloader                => $args{look_for_autoloader},
        _look_for_selfloader                => $args{look_for_selfloader},
        _saw_autoloader                     => 0,
        _saw_selfloader                     => 0,
        _saw_hash_bang                      => 0,
        _saw_end                            => 0,
        _saw_data                           => 0,
        _saw_negative_indentation           => 0,
        _started_tokenizing                 => 0,
        _line_buffer_object                 => $line_buffer_object,
        _debugger_object                    => $args{debugger_object},
        _diagnostics_object                 => $args{diagnostics_object},
        _logger_object                      => $args{logger_object},
        _unexpected_error_count             => 0,
        _started_looking_for_here_target_at => 0,
        _nearly_matched_here_target_at      => undef,
        _line_text                          => "",
        _rlower_case_labels_at              => undef,
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
        my $started_looking_for_here_target_at =
          $tokenizer_self->{_started_looking_for_here_target_at};
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
        my $nearly_matched_here_target_at =
          $tokenizer_self->{_nearly_matched_here_target_at};
        if ($nearly_matched_here_target_at) {
            warning(
"NOTE: almost matched at input line $nearly_matched_here_target_at except for whitespace\n"
            );
        }
    }

    if ( $tokenizer_self->{_in_quote} ) {
        my $line_start_quote = $tokenizer_self->{_line_start_quote};
        my $quote_target     = $tokenizer_self->{_quote_target};
        my $what =
          ( $tokenizer_self->{_in_attribute_list} )
          ? "attribute list"
          : "quote/pattern";
        warning(
"hit EOF seeking end of $what starting at line $line_start_quote ending in $quote_target\n"
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
    if ( $tokenizer_self->{_rlower_case_labels_at} ) {
        my @lower_case_labels_at =
          @{ $tokenizer_self->{_rlower_case_labels_at} };
        write_logfile_entry(
            "Suggest using upper case characters in label(s)\n");
        local $" = ')(';
        write_logfile_entry("  defined at line(s): (@lower_case_labels_at)\n");
    }
}

sub report_v_string {

    # warn if this version can't handle v-strings
    my $tok = shift;
    unless ( $tokenizer_self->{_saw_v_string} ) {
        $tokenizer_self->{_saw_v_string} = $tokenizer_self->{_last_line_number};
    }
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

    # USES GLOBAL VARIABLES: $tokenizer_self, $brace_depth,
    # $square_bracket_depth, $paren_depth

    my $input_line = $tokenizer_self->{_line_buffer_object}->get_line();
    $tokenizer_self->{_line_text} = $input_line;

    return undef unless ($input_line);

    my $input_line_number = ++$tokenizer_self->{_last_line_number};

    # Find and remove what characters terminate this line, including any
    # control r
    my $input_line_separator = "";
    if ( chomp($input_line) ) { $input_line_separator = $/ }

    # TODO: what other characters should be included here?
    if ( $input_line =~ s/((\r|\035|\032)+)$// ) {
        $input_line_separator = $2 . $input_line_separator;
    }

    # for backwards compatability we keep the line text terminated with
    # a newline character
    $input_line .= "\n";
    $tokenizer_self->{_line_text} = $input_line;    # update

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
        _starting_in_quote    => 0,                    # to be set by subroutine
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
            $tokenizer_self->{_nearly_matched_here_target_at} = undef;
            $line_of_tokens->{_line_type}                     = 'HERE_END';
            write_logfile_entry("Exiting HERE document $here_doc_target\n");

            my $rhere_target_list = $tokenizer_self->{_rhere_target_list};
            if (@$rhere_target_list) {    # there can be multiple here targets
                ( $here_doc_target, $here_quote_character ) =
                  @{ shift @$rhere_target_list };
                $tokenizer_self->{_here_doc_target} = $here_doc_target;
                $tokenizer_self->{_here_quote_character} =
                  $here_quote_character;
                write_logfile_entry(
                    "Entering HERE document $here_doc_target\n");
                $tokenizer_self->{_nearly_matched_here_target_at} = undef;
                $tokenizer_self->{_started_looking_for_here_target_at} =
                  $input_line_number;
            }
            else {
                $tokenizer_self->{_in_here_doc}          = 0;
                $tokenizer_self->{_here_doc_target}      = "";
                $tokenizer_self->{_here_quote_character} = "";
            }
        }

        # check for error of extra whitespace
        # note for PERL6: leading whitespace is allowed
        else {
            $candidate_target =~ s/\s*$//;
            $candidate_target =~ s/^\s*//;
            if ( $candidate_target eq $here_doc_target ) {
                $tokenizer_self->{_nearly_matched_here_target_at} =
                  $input_line_number;
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
            warning(
                "Hash-bang in pod can cause older versions of perl to fail! \n"
            );
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
                $line_of_tokens->{_line_type} = 'POD_END';
            }
            else {
                $line_of_tokens->{_line_type} = 'POD_START';
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

        my ( $here_doc_target, $here_quote_character ) =
          @{ shift @$rhere_target_list };
        $tokenizer_self->{_in_here_doc}          = 1;
        $tokenizer_self->{_here_doc_target}      = $here_doc_target;
        $tokenizer_self->{_here_quote_character} = $here_quote_character;
        write_logfile_entry("Entering HERE document $here_doc_target\n");
        $tokenizer_self->{_started_looking_for_here_target_at} =
          $input_line_number;
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
    if (  !$tokenizer_self->{_started_tokenizing}
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

        #if ( ( my $quote_target = get_quote_target() ) !~ /^\s*$/ ) {
        if (
            ( my $quote_target = $tokenizer_self->{_quote_target} ) !~ /^\s*$/ )
        {
            $tokenizer_self->{_line_start_quote} = $input_line_number;
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

    # USES GLOBAL VARIABLES: $tokenizer_self
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

    # USES GLOBAL VARIABLES: $tokenizer_self
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
                $msg =
"old indentation is unclear, using $columns $entabbed spaces\n";
            }
            $input_tabstr = " " x $columns;
        }
        $know_input_tabstr                    = 1;
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

sub ones_count {

    # count number of 1's in a string of 1's and 0's
    # example: ones_count("010101010101") gives 6
    return ( my $cis = $_[0] ) =~ tr/1/0/;
}

sub prepare_for_a_new_file {

    # previous tokens needed to determine what to expect next
    $last_nonblank_token      = ';';    # the only possible starting state which
    $last_nonblank_type       = ';';    # will make a leading brace a code block
    $last_nonblank_block_type = '';

    # scalars for remembering statement types across multiple lines
    $statement_type    = '';            # '' or 'use' or 'sub..' or 'case..'
    $in_attribute_list = 0;

    # scalars for remembering where we are in the file
    $current_package = "main";
    $context         = UNKNOWN_CONTEXT;

    # hashes used to remember function information
    %is_constant             = ();      # user-defined constants
    %is_user_function        = ();      # user-defined functions
    %user_function_prototype = ();      # their prototypes
    %is_block_function       = ();
    %is_block_list_function  = ();
    %saw_function_definition = ();

    # variables used to track depths of various containers
    # and report nesting errors
    $paren_depth          = 0;
    $brace_depth          = 0;
    $square_bracket_depth = 0;
    @current_depth[ 0 .. $#closing_brace_names ] =
      (0) x scalar @closing_brace_names;
    $total_depth = 0;
    @total_depth = ();
    @nesting_sequence_number[ 0 .. $#closing_brace_names ] =
      ( 0 .. $#closing_brace_names );
    @current_sequence_number             = ();
    $paren_type[$paren_depth]            = '';
    $paren_semicolon_count[$paren_depth] = 0;
    $paren_structural_type[$brace_depth] = '';
    $brace_type[$brace_depth] = ';';    # identify opening brace as code block
    $brace_structural_type[$brace_depth]                   = '';
    $brace_statement_type[$brace_depth]                    = "";
    $brace_context[$brace_depth]                           = UNKNOWN_CONTEXT;
    $brace_package[$paren_depth]                           = $current_package;
    $square_bracket_type[$square_bracket_depth]            = '';
    $square_bracket_structural_type[$square_bracket_depth] = '';

    initialize_tokenizer_state();
}

{                                       # begin tokenize_this_line

    use constant BRACE          => 0;
    use constant SQUARE_BRACKET => 1;
    use constant PAREN          => 2;
    use constant QUESTION_COLON => 3;

    # TV1: scalars for processing one LINE.
    # Re-initialized on each entry to sub tokenize_this_line.
    my (
        $block_type,        $container_type,    $expecting,
        $i,                 $i_tok,             $input_line,
        $input_line_number, $last_nonblank_i,   $max_token_index,
        $next_tok,          $next_type,         $peeked_ahead,
        $prototype,         $rhere_target_list, $rtoken_map,
        $rtoken_type,       $rtokens,           $tok,
        $type,              $type_sequence,     $indent_flag,
    );

    # TV2: refs to ARRAYS for processing one LINE
    # Re-initialized on each call.
    my $routput_token_list     = [];    # stack of output token indexes
    my $routput_token_type     = [];    # token types
    my $routput_block_type     = [];    # types of code block
    my $routput_container_type = [];    # paren types, such as if, elsif, ..
    my $routput_type_sequence  = [];    # nesting sequential number
    my $routput_indent_flag    = [];    #

    # TV3: SCALARS for quote variables.  These are initialized with a
    # subroutine call and continually updated as lines are processed.
    my ( $in_quote, $quote_type, $quote_character, $quote_pos, $quote_depth,
        $quoted_string_1, $quoted_string_2, $allowed_quote_modifiers, );

    # TV4: SCALARS for multi-line identifiers and
    # statements. These are initialized with a subroutine call
    # and continually updated as lines are processed.
    my ( $id_scan_state, $identifier, $want_paren, $indented_if_level );

    # TV5: SCALARS for tracking indentation level.
    # Initialized once and continually updated as lines are
    # processed.
    my (
        $nesting_token_string,      $nesting_type_string,
        $nesting_block_string,      $nesting_block_flag,
        $nesting_list_string,       $nesting_list_flag,
        $ci_string_in_tokenizer,    $continuation_string_in_tokenizer,
        $in_statement_continuation, $level_in_tokenizer,
        $slevel_in_tokenizer,       $rslevel_stack,
    );

    # TV6: SCALARS for remembering several previous
    # tokens. Initialized once and continually updated as
    # lines are processed.
    my (
        $last_nonblank_container_type,     $last_nonblank_type_sequence,
        $last_last_nonblank_token,         $last_last_nonblank_type,
        $last_last_nonblank_block_type,    $last_last_nonblank_container_type,
        $last_last_nonblank_type_sequence, $last_nonblank_prototype,
    );

    # ----------------------------------------------------------------
    # beginning of tokenizer variable access and manipulation routines
    # ----------------------------------------------------------------

    sub initialize_tokenizer_state {

        # TV1: initialized on each call
        # TV2: initialized on each call
        # TV3:
        $in_quote                = 0;
        $quote_type              = 'Q';
        $quote_character         = "";
        $quote_pos               = 0;
        $quote_depth             = 0;
        $quoted_string_1         = "";
        $quoted_string_2         = "";
        $allowed_quote_modifiers = "";

        # TV4:
        $id_scan_state     = '';
        $identifier        = '';
        $want_paren        = "";
        $indented_if_level = 0;

        # TV5:
        $nesting_token_string             = "";
        $nesting_type_string              = "";
        $nesting_block_string             = '1';    # initially in a block
        $nesting_block_flag               = 1;
        $nesting_list_string              = '0';    # initially not in a list
        $nesting_list_flag                = 0;      # initially not in a list
        $ci_string_in_tokenizer           = "";
        $continuation_string_in_tokenizer = "0";
        $in_statement_continuation        = 0;
        $level_in_tokenizer               = 0;
        $slevel_in_tokenizer              = 0;
        $rslevel_stack                    = [];

        # TV6:
        $last_nonblank_container_type      = '';
        $last_nonblank_type_sequence       = '';
        $last_last_nonblank_token          = ';';
        $last_last_nonblank_type           = ';';
        $last_last_nonblank_block_type     = '';
        $last_last_nonblank_container_type = '';
        $last_last_nonblank_type_sequence  = '';
        $last_nonblank_prototype           = "";
    }

    sub save_tokenizer_state {

        my $rTV1 = [
            $block_type,        $container_type,    $expecting,
            $i,                 $i_tok,             $input_line,
            $input_line_number, $last_nonblank_i,   $max_token_index,
            $next_tok,          $next_type,         $peeked_ahead,
            $prototype,         $rhere_target_list, $rtoken_map,
            $rtoken_type,       $rtokens,           $tok,
            $type,              $type_sequence,     $indent_flag,
        ];

        my $rTV2 = [
            $routput_token_list,    $routput_token_type,
            $routput_block_type,    $routput_container_type,
            $routput_type_sequence, $routput_indent_flag,
        ];

        my $rTV3 = [
            $in_quote,        $quote_type,
            $quote_character, $quote_pos,
            $quote_depth,     $quoted_string_1,
            $quoted_string_2, $allowed_quote_modifiers,
        ];

        my $rTV4 =
          [ $id_scan_state, $identifier, $want_paren, $indented_if_level ];

        my $rTV5 = [
            $nesting_token_string,      $nesting_type_string,
            $nesting_block_string,      $nesting_block_flag,
            $nesting_list_string,       $nesting_list_flag,
            $ci_string_in_tokenizer,    $continuation_string_in_tokenizer,
            $in_statement_continuation, $level_in_tokenizer,
            $slevel_in_tokenizer,       $rslevel_stack,
        ];

        my $rTV6 = [
            $last_nonblank_container_type,
            $last_nonblank_type_sequence,
            $last_last_nonblank_token,
            $last_last_nonblank_type,
            $last_last_nonblank_block_type,
            $last_last_nonblank_container_type,
            $last_last_nonblank_type_sequence,
            $last_nonblank_prototype,
        ];
        return [ $rTV1, $rTV2, $rTV3, $rTV4, $rTV5, $rTV6 ];
    }

    sub restore_tokenizer_state {
        my ($rstate) = @_;
        my ( $rTV1, $rTV2, $rTV3, $rTV4, $rTV5, $rTV6 ) = @{$rstate};
        (
            $block_type,        $container_type,    $expecting,
            $i,                 $i_tok,             $input_line,
            $input_line_number, $last_nonblank_i,   $max_token_index,
            $next_tok,          $next_type,         $peeked_ahead,
            $prototype,         $rhere_target_list, $rtoken_map,
            $rtoken_type,       $rtokens,           $tok,
            $type,              $type_sequence,     $indent_flag,
        ) = @{$rTV1};

        (
            $routput_token_list,    $routput_token_type,
            $routput_block_type,    $routput_container_type,
            $routput_type_sequence, $routput_type_sequence,
        ) = @{$rTV2};

        (
            $in_quote, $quote_type, $quote_character, $quote_pos, $quote_depth,
            $quoted_string_1, $quoted_string_2, $allowed_quote_modifiers,
        ) = @{$rTV3};

        ( $id_scan_state, $identifier, $want_paren, $indented_if_level ) =
          @{$rTV4};

        (
            $nesting_token_string,      $nesting_type_string,
            $nesting_block_string,      $nesting_block_flag,
            $nesting_list_string,       $nesting_list_flag,
            $ci_string_in_tokenizer,    $continuation_string_in_tokenizer,
            $in_statement_continuation, $level_in_tokenizer,
            $slevel_in_tokenizer,       $rslevel_stack,
        ) = @{$rTV5};

        (
            $last_nonblank_container_type,
            $last_nonblank_type_sequence,
            $last_last_nonblank_token,
            $last_last_nonblank_type,
            $last_last_nonblank_block_type,
            $last_last_nonblank_container_type,
            $last_last_nonblank_type_sequence,
            $last_nonblank_prototype,
        ) = @{$rTV6};
    }

    sub get_indentation_level {

        # patch to avoid reporting error if indented if is not terminated
        if ($indented_if_level) { return $level_in_tokenizer - 1 }
        return $level_in_tokenizer;
    }

    sub reset_indentation_level {
        $level_in_tokenizer  = $_[0];
        $slevel_in_tokenizer = $_[0];
        push @{$rslevel_stack}, $slevel_in_tokenizer;
    }

    sub peeked_ahead {
        $peeked_ahead = defined( $_[0] ) ? $_[0] : $peeked_ahead;
    }

    # ------------------------------------------------------------
    # end of tokenizer variable access and manipulation routines
    # ------------------------------------------------------------

    # ------------------------------------------------------------
    # beginning of various scanner interface routines
    # ------------------------------------------------------------
    sub scan_replacement_text {

        # check for here-docs in replacement text invoked by
        # a substitution operator with executable modifier 'e'.
        #
        # given:
        #  $replacement_text
        # return:
        #  $rht = reference to any here-doc targets
        my ($replacement_text) = @_;

        # quick check
        return undef unless ( $replacement_text =~ /<</ );

        write_logfile_entry("scanning replacement text for here-doc targets\n");

        # save the logger object for error messages
        my $logger_object = $tokenizer_self->{_logger_object};

        # localize all package variables
        local (
            $tokenizer_self,          $last_nonblank_token,
            $last_nonblank_type,      $last_nonblank_block_type,
            $statement_type,          $in_attribute_list,
            $current_package,         $context,
            %is_constant,             %is_user_function,
            %user_function_prototype, %is_block_function,
            %is_block_list_function,  %saw_function_definition,
            $brace_depth,             $paren_depth,
            $square_bracket_depth,    @current_depth,
            @total_depth,             $total_depth,
            @nesting_sequence_number, @current_sequence_number,
            @paren_type,              @paren_semicolon_count,
            @paren_structural_type,   @brace_type,
            @brace_structural_type,   @brace_statement_type,
            @brace_context,           @brace_package,
            @square_bracket_type,     @square_bracket_structural_type,
            @depth_array,             @starting_line_of_current_depth,
            @nested_ternary_flag,
        );

        # save all lexical variables
        my $rstate = save_tokenizer_state();
        _decrement_count();    # avoid error check for multiple tokenizers

        # make a new tokenizer
        my $rOpts = {};
        my $rpending_logfile_message;
        my $source_object =
          Perl::Tidy::LineSource->new( \$replacement_text, $rOpts,
            $rpending_logfile_message );
        my $tokenizer = Perl::Tidy::Tokenizer->new(
            source_object        => $source_object,
            logger_object        => $logger_object,
            starting_line_number => $input_line_number,
        );

        # scan the replacement text
        1 while ( $tokenizer->get_line() );

        # remove any here doc targets
        my $rht = undef;
        if ( $tokenizer_self->{_in_here_doc} ) {
            $rht = [];
            push @{$rht},
              [
                $tokenizer_self->{_here_doc_target},
                $tokenizer_self->{_here_quote_character}
              ];
            if ( $tokenizer_self->{_rhere_target_list} ) {
                push @{$rht}, @{ $tokenizer_self->{_rhere_target_list} };
                $tokenizer_self->{_rhere_target_list} = undef;
            }
            $tokenizer_self->{_in_here_doc} = undef;
        }

        # now its safe to report errors
        $tokenizer->report_tokenization_errors();

        # restore all tokenizer lexical variables
        restore_tokenizer_state($rstate);

        # return the here doc targets
        return $rht;
    }

    sub scan_bare_identifier {
        ( $i, $tok, $type, $prototype ) =
          scan_bare_identifier_do( $input_line, $i, $tok, $type, $prototype,
            $rtoken_map, $max_token_index );
    }

    sub scan_identifier {
        ( $i, $tok, $type, $id_scan_state, $identifier ) =
          scan_identifier_do( $i, $id_scan_state, $identifier, $rtokens,
            $max_token_index, $expecting );
    }

    sub scan_id {
        ( $i, $tok, $type, $id_scan_state ) =
          scan_id_do( $input_line, $i, $tok, $rtokens, $rtoken_map,
            $id_scan_state, $max_token_index );
    }

    sub scan_number {
        my $number;
        ( $i, $type, $number ) =
          scan_number_do( $input_line, $i, $rtoken_map, $type,
            $max_token_index );
        return $number;
    }

    # a sub to warn if token found where term expected
    sub error_if_expecting_TERM {
        if ( $expecting == TERM ) {
            if ( $really_want_term{$last_nonblank_type} ) {
                unexpected( $tok, "term", $i_tok, $last_nonblank_i, $rtoken_map,
                    $rtoken_type, $input_line );
                1;
            }
        }
    }

    # a sub to warn if token found where operator expected
    sub error_if_expecting_OPERATOR {
        if ( $expecting == OPERATOR ) {
            my $thing = defined $_[0] ? $_[0] : $tok;
            unexpected( $thing, "operator", $i_tok, $last_nonblank_i,
                $rtoken_map, $rtoken_type, $input_line );
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

    my %is_my_our;
    @_ = qw(my our);
    @is_my_our{@_} = (1) x scalar(@_);

    # These keywords may introduce blocks after parenthesized expressions,
    # in the form:
    # keyword ( .... ) { BLOCK }
    # patch for SWITCH/CASE: added 'switch' 'case' 'given' 'when'
    my %is_blocktype_with_paren;
    @_ = qw(if elsif unless while until for foreach switch case given when);
    @is_blocktype_with_paren{@_} = (1) x scalar(@_);

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
##      '//=' => undef,
##      '~'   => undef,
##      '~~'  => undef,
##      '!~~'  => undef,

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
                    # &method(...), $method->(..), &{method}(...),
                    # $ref[2](list) is ok & short for $ref[2]->(list)
                    # NOTE: at present, braces in something like &{ xxx }
                    # are not marked as a block, we might have a method call
                    && $last_nonblank_token !~ /^([\]\}\&]|\-\>)/

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
                          find_next_nonblank_token( $i, $rtokens,
                            $max_token_index );
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
            ( $type_sequence, $indent_flag ) =
              increase_nesting_depth( PAREN, $$rtoken_map[$i_tok] );

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
            ( $type_sequence, $indent_flag ) =
              decrease_nesting_depth( PAREN, $$rtoken_map[$i_tok] );

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

            # patch for operator_expected: note if we are in the list (use.t)
            if ( $statement_type eq 'use' ) { $statement_type = '_use' }
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
                  guess_if_pattern_or_division( $i, $rtokens, $rtoken_map,
                    $max_token_index );

                if ($msg) {
                    write_diagnostics("DIVIDE:$msg\n");
                    write_logfile_entry($msg);
                }
            }
            else { $is_pattern = ( $expecting == TERM ) }

            if ($is_pattern) {
                $in_quote                = 1;
                $type                    = 'Q';
                $allowed_quote_modifiers = '[cgimosxp]';
            }
            else {    # not a pattern; check for a /= token

                if ( $$rtokens[ $i + 1 ] eq '=' ) {    # form token /=
                    $i++;
                    $tok  = '/=';
                    $type = $tok;
                }

              #DEBUG - collecting info on what tokens follow a divide
              # for development of guessing algorithm
              #if ( numerator_expected( $i, $rtokens, $max_token_index ) < 0 ) {
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

            # ATTRS: for a '{' following an attribute list, reset
            # things to look like we just saw the sub name
            if ( $statement_type =~ /^sub/ ) {
                $last_nonblank_token = $statement_type;
                $last_nonblank_type  = 'i';
                $statement_type      = "";
            }

            # patch for SWITCH/CASE: hide these keywords from an immediately
            # following opening brace
            elsif ( ( $statement_type eq 'case' || $statement_type eq 'when' )
                && $statement_type eq $last_nonblank_token )
            {
                $last_nonblank_token = ";";
            }

            elsif ( $last_nonblank_token eq ')' ) {
                $last_nonblank_token = $paren_type[ $paren_depth + 1 ];

                # defensive move in case of a nesting error (pbug.t)
                # in which this ')' had no previous '('
                # this nesting error will have been caught
                if ( !defined($last_nonblank_token) ) {
                    $last_nonblank_token = 'if';
                }

                # check for syntax error here;
                unless ( $is_blocktype_with_paren{$last_nonblank_token} ) {
                    my $list = join( ' ', sort keys %is_blocktype_with_paren );
                    warning(
                        "syntax error at ') {', didn't see one of: $list\n");
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

                # patch for SWITCH/CASE:
                # allow paren-less identifier after 'when'
                # if the brace is preceded by a space
                if (   $statement_type eq 'when'
                    && $last_nonblank_type      eq 'i'
                    && $last_last_nonblank_type eq 'k'
                    && ( $i_tok == 0 || $rtoken_type->[ $i_tok - 1 ] eq 'b' ) )
                {
                    $type       = '{';
                    $block_type = $statement_type;
                }
            }

            # code and anonymous hash have the same type, '{', but are
            # distinguished by 'block_type',
            # which will be blank for an anonymous hash
            else {

                $block_type = code_block_type( $i_tok, $rtokens, $rtoken_type,
                    $max_token_index );

                # patch to promote bareword type to function taking block
                if (   $block_type
                    && $last_nonblank_type eq 'w'
                    && $last_nonblank_i >= 0 )
                {
                    if ( $routput_token_type->[$last_nonblank_i] eq 'w' ) {
                        $routput_token_type->[$last_nonblank_i] = 'G';
                    }
                }

                # patch for SWITCH/CASE: if we find a stray opening block brace
                # where we might accept a 'case' or 'when' block, then take it
                if (   $statement_type eq 'case'
                    || $statement_type eq 'when' )
                {
                    if ( !$block_type || $block_type eq '}' ) {
                        $block_type = $statement_type;
                    }
                }
            }
            $brace_type[ ++$brace_depth ] = $block_type;
            $brace_package[$brace_depth] = $current_package;
            ( $type_sequence, $indent_flag ) =
              increase_nesting_depth( BRACE, $$rtoken_map[$i_tok] );
            $brace_structural_type[$brace_depth] = $type;
            $brace_context[$brace_depth]         = $context;
            $brace_statement_type[$brace_depth]  = $statement_type;
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
            ( $type_sequence, $indent_flag ) =
              decrease_nesting_depth( BRACE, $$rtoken_map[$i_tok] );

            if ( $brace_structural_type[$brace_depth] eq 'L' ) {
                $type = 'R';
            }

            # propagate type information for 'do' and 'eval' blocks.
            # This is necessary to enable us to know if an operator
            # or term is expected next
            if ( $is_block_operator{ $brace_type[$brace_depth] } ) {
                $tok = $brace_type[$brace_depth];
            }

            $context        = $brace_context[$brace_depth];
            $statement_type = $brace_statement_type[$brace_depth];
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
                    $expecting, $max_token_index );

            }
            else {
            }
        },
        '?' => sub {    # ?: conditional or starting pattern?

            my $is_pattern;

            if ( $expecting == UNKNOWN ) {

                my $msg;
                ( $is_pattern, $msg ) =
                  guess_if_pattern_or_conditional( $i, $rtokens, $rtoken_map,
                    $max_token_index );

                if ($msg) { write_logfile_entry($msg) }
            }
            else { $is_pattern = ( $expecting == TERM ) }

            if ($is_pattern) {
                $in_quote                = 1;
                $type                    = 'Q';
                $allowed_quote_modifiers = '[cgimosxp]';
            }
            else {
                ( $type_sequence, $indent_flag ) =
                  increase_nesting_depth( QUESTION_COLON,
                    $$rtoken_map[$i_tok] );
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

            # ATTRS: check for a ':' which introduces an attribute list
            # (this might eventually get its own token type)
            elsif ( $statement_type =~ /^sub/ ) {
                $type              = 'A';
                $in_attribute_list = 1;
            }

            # check for scalar attribute, such as
            # my $foo : shared = 1;
            elsif ($is_my_our{$statement_type}
                && $current_depth[QUESTION_COLON] == 0 )
            {
                $type              = 'A';
                $in_attribute_list = 1;
            }

            # otherwise, it should be part of a ?/: operator
            else {
                ( $type_sequence, $indent_flag ) =
                  decrease_nesting_depth( QUESTION_COLON,
                    $$rtoken_map[$i_tok] );
                if ( $last_nonblank_token eq '?' ) {
                    warning("Syntax error near ? :\n");
                }
            }
        },
        '+' => sub {    # what kind of plus?

            if ( $expecting == TERM ) {
                my $number = scan_number();

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
            ( $type_sequence, $indent_flag ) =
              increase_nesting_depth( SQUARE_BRACKET, $$rtoken_map[$i_tok] );

            # It may seem odd, but structural square brackets have
            # type '{' and '}'.  This simplifies the indentation logic.
            if ( !is_non_structural_brace() ) {
                $type = '{';
            }
            $square_bracket_structural_type[$square_bracket_depth] = $type;
        },
        ']' => sub {
            ( $type_sequence, $indent_flag ) =
              decrease_nesting_depth( SQUARE_BRACKET, $$rtoken_map[$i_tok] );

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
                my ( $next_nonblank_token, $i_next ) =
                  find_next_nonblank_token( $i + 1, $rtokens,
                    $max_token_index );

                # check for a quoted word like "-w=>xx";
                # it is sufficient to just check for a following '='
                if ( $next_nonblank_token eq '=' ) {
                    $type = 'm';
                }
                else {
                    $i++;
                    $tok .= $next_tok;
                    $type = 'F';
                }
            }
            elsif ( $expecting == TERM ) {
                my $number = scan_number();

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
                my ( $found_target, $here_doc_target, $here_quote_character,
                    $saw_error );
                (
                    $found_target, $here_doc_target, $here_quote_character, $i,
                    $saw_error
                  )
                  = find_here_doc( $expecting, $i, $rtokens, $rtoken_map,
                    $max_token_index );

                if ($found_target) {
                    push @{$rhere_target_list},
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
                    unless ($saw_error) {

                        # shouldn't happen..
                        warning("Program bug; didn't find here doc target\n");
                        report_definite_bug();
                    }
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
            elsif ( $expecting == UNKNOWN ) {
                my ( $next_nonblank_token, $i_next ) =
                  find_next_nonblank_token( $i, $rtokens, $max_token_index );
                if ( $next_nonblank_token eq '$' ) { $type = 'pp' }
            }
        },

        '=>' => sub {
            if ( $last_nonblank_type eq $tok ) {
                complain("Repeated '=>'s \n");
            }

            # patch for operator_expected: note if we are in the list (use.t)
            # TODO: make version numbers a new token type
            if ( $statement_type eq 'use' ) { $statement_type = '_use' }
        },

        # type = 'mm' for pre-decrement, '--' for post-decrement
        '--' => sub {

            if ( $expecting == TERM ) { $type = 'mm' }
            elsif ( $expecting == UNKNOWN ) {
                my ( $next_nonblank_token, $i_next ) =
                  find_next_nonblank_token( $i, $rtokens, $max_token_index );
                if ( $next_nonblank_token eq '$' ) { $type = 'mm' }
            }
        },

        '&&' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },

        '||' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },

        '//' => sub {
            error_if_expecting_TERM()
              if ( $expecting == TERM );
        },
    };

    # ------------------------------------------------------------
    # end hash of code for handling individual token types
    # ------------------------------------------------------------

    my %matching_start_token = ( '}' => '{', ']' => '[', ')' => '(' );

    # These block types terminate statements and do not need a trailing
    # semicolon
    # patched for SWITCH/CASE:
    my %is_zero_continuation_block_type;
    @_ = qw( } { BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue ;
      if elsif else unless while until for foreach switch case given when);
    @is_zero_continuation_block_type{@_} = (1) x scalar(@_);

    my %is_not_zero_continuation_block_type;
    @_ = qw(sort grep map do eval);
    @is_not_zero_continuation_block_type{@_} = (1) x scalar(@_);

    my %is_logical_container;
    @_ = qw(if elsif unless while and or err not && !  || for foreach);
    @is_logical_container{@_} = (1) x scalar(@_);

    my %is_binary_type;
    @_ = qw(|| &&);
    @is_binary_type{@_} = (1) x scalar(@_);

    my %is_binary_keyword;
    @_ = qw(and or err eq ne cmp);
    @is_binary_keyword{@_} = (1) x scalar(@_);

    # 'L' is token for opening { at hash key
    my %is_opening_type;
    @_ = qw" L { ( [ ";
    @is_opening_type{@_} = (1) x scalar(@_);

    # 'R' is token for closing } at hash key
    my %is_closing_type;
    @_ = qw" R } ) ] ";
    @is_closing_type{@_} = (1) x scalar(@_);

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
    # perl 5.10 adds 'p' (preserve)
    my %quote_modifiers = (
        's'  => '[cegimosxp]',
        'y'  => '[cds]',
        'tr' => '[cds]',
        'm'  => '[cgimosxp]',
        'qr' => '[imosxp]',
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
  # *. For another example, search for the smartmatch operator '~~'
  # with your editor to see where updates were made for it.
  #
  # -----------------------------------------------------------------------

        my $line_of_tokens = shift;
        my ($untrimmed_input_line) = $line_of_tokens->{_line_text};

        # patch while coding change is underway
        # make callers private data to allow access
        # $tokenizer_self = $caller_tokenizer_self;

        # extract line number for use in error messages
        $input_line_number = $line_of_tokens->{_line_number};

        # reinitialize for multi-line quote
        $line_of_tokens->{_starting_in_quote} = $in_quote && $quote_type eq 'Q';

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

        # update the copy of the line for use in error messages
        # This must be exactly what we give the pre_tokenizer
        $tokenizer_self->{_line_text} = $input_line;

        # re-initialize for the main loop
        $routput_token_list     = [];    # stack of output token indexes
        $routput_token_type     = [];    # token types
        $routput_block_type     = [];    # types of code block
        $routput_container_type = [];    # paren types, such as if, elsif, ..
        $routput_type_sequence  = [];    # nesting sequential number

        $rhere_target_list = [];

        $tok             = $last_nonblank_token;
        $type            = $last_nonblank_type;
        $prototype       = $last_nonblank_prototype;
        $last_nonblank_i = -1;
        $block_type      = $last_nonblank_block_type;
        $container_type  = $last_nonblank_container_type;
        $type_sequence   = $last_nonblank_type_sequence;
        $indent_flag     = 0;
        $peeked_ahead    = 0;

        # tokenization is done in two stages..
        # stage 1 is a very simple pre-tokenization
        my $max_tokens_wanted = 0; # this signals pre_tokenize to get all tokens

        # a little optimization for a full-line comment
        if ( !$in_quote && ( $input_line =~ /^#/ ) ) {
            $max_tokens_wanted = 1    # no use tokenizing a comment
        }

        # start by breaking the line into pre-tokens
        ( $rtokens, $rtoken_map, $rtoken_type ) =
          pre_tokenize( $input_line, $max_tokens_wanted );

        $max_token_index = scalar(@$rtokens) - 1;
        push( @$rtokens,    ' ', ' ', ' ' ); # extra whitespace simplifies logic
        push( @$rtoken_map, 0,   0,   0 );   # shouldn't be referenced
        push( @$rtoken_type, 'b', 'b', 'b' );

        # initialize for main loop
        for $i ( 0 .. $max_token_index + 3 ) {
            $routput_token_type->[$i]     = "";
            $routput_block_type->[$i]     = "";
            $routput_container_type->[$i] = "";
            $routput_type_sequence->[$i]  = "";
            $routput_indent_flag->[$i]    = 0;
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

                unless ( @{$routput_token_list} )
                {               # initialize if continuation line
                    push( @{$routput_token_list}, $i );
                    $routput_token_type->[$i] = $type;

                }
                $tok = $quote_character unless ( $quote_character =~ /^\s*$/ );

                # scan for the end of the quote or pattern
                (
                    $i, $in_quote, $quote_character, $quote_pos, $quote_depth,
                    $quoted_string_1, $quoted_string_2
                  )
                  = do_quote(
                    $i,               $in_quote,    $quote_character,
                    $quote_pos,       $quote_depth, $quoted_string_1,
                    $quoted_string_2, $rtokens,     $rtoken_map,
                    $max_token_index
                  );

                # all done if we didn't find it
                last if ($in_quote);

                # save pattern and replacement text for rescanning
                my $qs1 = $quoted_string_1;
                my $qs2 = $quoted_string_2;

                # re-initialize for next search
                $quote_character = '';
                $quote_pos       = 0;
                $quote_type      = 'Q';
                $quoted_string_1 = "";
                $quoted_string_2 = "";
                last if ( ++$i > $max_token_index );

                # look for any modifiers
                if ($allowed_quote_modifiers) {

                    # check for exact quote modifiers
                    if ( $$rtokens[$i] =~ /^[A-Za-z_]/ ) {
                        my $str = $$rtokens[$i];
                        my $saw_modifier_e;
                        while ( $str =~ /\G$allowed_quote_modifiers/gc ) {
                            my $pos = pos($str);
                            my $char = substr( $str, $pos - 1, 1 );
                            $saw_modifier_e ||= ( $char eq 'e' );
                        }

                        # For an 'e' quote modifier we must scan the replacement
                        # text for here-doc targets.
                        if ($saw_modifier_e) {

                            my $rht = scan_replacement_text($qs1);

                            # Change type from 'Q' to 'h' for quotes with
                            # here-doc targets so that the formatter (see sub
                            # print_line_of_tokens) will not make any line
                            # breaks after this point.
                            if ($rht) {
                                push @{$rhere_target_list}, @{$rht};
                                $type = 'h';
                                if ( $i_tok < 0 ) {
                                    my $ilast = $routput_token_list->[-1];
                                    $routput_token_type->[$ilast] = $type;
                                }
                            }
                        }

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

                $last_last_nonblank_token      = $last_nonblank_token;
                $last_last_nonblank_type       = $last_nonblank_type;
                $last_last_nonblank_block_type = $last_nonblank_block_type;
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
                $routput_token_type->[$i_tok]     = $type;
                $routput_block_type->[$i_tok]     = $block_type;
                $routput_container_type->[$i_tok] = $container_type;
                $routput_type_sequence->[$i_tok]  = $type_sequence;
                $routput_indent_flag->[$i_tok]    = $indent_flag;
            }
            my $pre_tok  = $$rtokens[$i];        # get the next pre-token
            my $pre_type = $$rtoken_type[$i];    # and type
            $tok  = $pre_tok;
            $type = $pre_type;                   # to be modified as necessary
            $block_type = "";    # blank for all tokens except code block braces
            $container_type = "";    # blank for all tokens except some parens
            $type_sequence  = "";    # blank for all tokens except ?/:
            $indent_flag    = 0;
            $prototype = "";    # blank for all tokens except user defined subs
            $i_tok     = $i;

            # this pre-token will start an output token
            push( @{$routput_token_list}, $i_tok );

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
            my $test_tok   = $tok . $$rtokens[ $i + 1 ];
            my $combine_ok = $is_digraph{$test_tok};

            # check for special cases which cannot be combined
            if ($combine_ok) {

                # '//' must be defined_or operator if an operator is expected.
                # TODO: Code for other ambiguous digraphs (/=, x=, **, *=)
                # could be migrated here for clarity
                if ( $test_tok eq '//' ) {
                    my $next_type = $$rtokens[ $i + 1 ];
                    my $expecting =
                      operator_expected( $prev_type, $tok, $next_type );
                    $combine_ok = 0 unless ( $expecting == OPERATOR );
                }
            }

            if (
                $combine_ok
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

            # turn off attribute list on first non-blank, non-bareword
            if ( $pre_type ne 'w' ) { $in_attribute_list = 0 }

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
                  find_next_nonblank_token( $i, $rtokens, $max_token_index );

                # ATTRS: handle sub and variable attributes
                if ($in_attribute_list) {

                    # treat bare word followed by open paren like qw(
                    if ( $next_nonblank_token eq '(' ) {
                        $in_quote                = $quote_items{'q'};
                        $allowed_quote_modifiers = $quote_modifiers{'q'};
                        $type                    = 'q';
                        $quote_type              = 'q';
                        next;
                    }

                    # handle bareword not followed by open paren
                    else {
                        $type = 'w';
                        next;
                    }
                }

                # quote a word followed by => operator
                if ( $next_nonblank_token eq '=' ) {

                    if ( $$rtokens[ $i_next + 1 ] eq '>' ) {
                        if ( $is_constant{$current_package}{$tok} ) {
                            $type = 'C';
                        }
                        elsif ( $is_user_function{$current_package}{$tok} ) {
                            $type = 'U';
                            $prototype =
                              $user_function_prototype{$current_package}{$tok};
                        }
                        elsif ( $tok =~ /^v\d+$/ ) {
                            $type = 'v';
                            report_v_string($tok);
                        }
                        else { $type = 'w' }

                        next;
                    }
                }

     # quote a bare word within braces..like xxx->{s}; note that we
     # must be sure this is not a structural brace, to avoid
     # mistaking {s} in the following for a quoted bare word:
     #     for(@[){s}bla}BLA}
     # Also treat q in something like var{-q} as a bare word, not qoute operator
                ##if (   ( $last_nonblank_type eq 'L' )
                ##    && ( $next_nonblank_token eq '}' ) )
                if (
                    $next_nonblank_token eq '}'
                    && (
                        $last_nonblank_type eq 'L'
                        || (   $last_nonblank_type eq 'm'
                            && $last_last_nonblank_type eq 'L' )
                    )
                  )
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
                      find_next_nonblank_token( $i, $rtokens,
                        $max_token_index );

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
                elsif (
                       ( $next_nonblank_token eq ':' )
                    && ( $$rtokens[ $i_next + 1 ] ne ':' )
                    && ( $i_next <= $max_token_index )    # colon on same line
                    && label_ok()
                  )
                {
                    if ( $tok !~ /[A-Z]/ ) {
                        push @{ $tokenizer_self->{_rlower_case_labels_at} },
                          $input_line_number;
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

                    # remember my and our to check for trailing ": shared"
                    elsif ( $is_my_our{$tok} ) {
                        $statement_type = $tok;
                    }

                    # Check for misplaced 'elsif' and 'else', but allow isolated
                    # else or elsif blocks to be formatted.  This is indicated
                    # by a last noblank token of ';'
                    elsif ( $tok eq 'elsif' ) {
                        if (   $last_nonblank_token ne ';'
                            && $last_nonblank_block_type !~
                            /^(if|elsif|unless)$/ )
                        {
                            warning(
"expecting '$tok' to follow one of 'if|elsif|unless'\n"
                            );
                        }
                    }
                    elsif ( $tok eq 'else' ) {

                        # patched for SWITCH/CASE
                        if (   $last_nonblank_token ne ';'
                            && $last_nonblank_block_type !~
                            /^(if|elsif|unless|case|when)$/ )
                        {
                            warning(
"expecting '$tok' to follow one of 'if|elsif|unless|case|when'\n"
                            );
                        }
                    }
                    elsif ( $tok eq 'continue' ) {
                        if (   $last_nonblank_token ne ';'
                            && $last_nonblank_block_type !~
                            /(^(\{|\}|;|while|until|for|foreach)|:$)/ )
                        {

                            # note: ';' '{' and '}' in list above
                            # because continues can follow bare blocks;
                            # ':' is labeled block
                            #
                            ############################################
                            # NOTE: This check has been deactivated because
                            # continue has an alternative usage for given/when
                            # blocks in perl 5.10
                            ## warning("'$tok' should follow a block\n");
                            ############################################
                        }
                    }

                    # patch for SWITCH/CASE if 'case' and 'when are
                    # treated as keywords.
                    elsif ( $tok eq 'when' || $tok eq 'case' ) {
                        $statement_type = $tok;    # next '{' is block
                    }

                    # indent trailing if/unless/while/until
                    # outdenting will be handled by later indentation loop
                    if (   $tok =~ /^(if|unless|while|until)$/
                        && $next_nonblank_token ne '(' )
                    {
                        $indent_flag = 1;
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

                        if ( $expecting == OPERATOR ) {

                            # don't complain about possible indirect object
                            # notation.
                            # For example:
                            #   package main;
                            #   sub new($) { ... }
                            #   $b = new A::;  # calls A::new
                            #   $c = new A;    # same thing but suspicious
                            # This will call A::new but we have a 'new' in
                            # main:: which looks like a constant.
                            #
                            if ( $last_nonblank_type eq 'C' ) {
                                if ( $tok !~ /::$/ ) {
                                    complain(<<EOM);
Expecting operator after '$last_nonblank_token' but found bare word '$tok'
       Maybe indirectet object notation?
EOM
                                }
                            }
                            else {
                                error_if_expecting_OPERATOR("bareword");
                            }
                        }

                        # mark bare words immediately followed by a paren as
                        # functions
                        $next_tok = $$rtokens[ $i + 1 ];
                        if ( $next_tok eq '(' ) {
                            $type = 'U';
                        }

                        # underscore after file test operator is file handle
                        if ( $tok eq '_' && $last_nonblank_type eq 'F' ) {
                            $type = 'Z';
                        }

                        # patch for SWITCH/CASE if 'case' and 'when are
                        # not treated as keywords:
                        if (
                            (
                                   $tok eq 'case'
                                && $brace_type[$brace_depth] eq 'switch'
                            )
                            || (   $tok eq 'when'
                                && $brace_type[$brace_depth] eq 'given' )
                          )
                        {
                            $statement_type = $tok;    # next '{' is block
                            $type = 'k';    # for keyword syntax coloring
                        }

                        # patch for SWITCH/CASE if switch and given not keywords
                        # Switch is not a perl 5 keyword, but we will gamble
                        # and mark switch followed by paren as a keyword.  This
                        # is only necessary to get html syntax coloring nice,
                        # and does not commit this as being a switch/case.
                        if ( $next_nonblank_token eq '('
                            && ( $tok eq 'switch' || $tok eq 'given' ) )
                        {
                            $type = 'k';    # for keyword syntax coloring
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
                my $number = scan_number();
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
            $routput_token_type->[$i_tok]     = $type;
            $routput_block_type->[$i_tok]     = $block_type;
            $routput_container_type->[$i_tok] = $container_type;
            $routput_type_sequence->[$i_tok]  = $type_sequence;
            $routput_indent_flag->[$i_tok]    = $indent_flag;
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
        my $ci_string_sum = ones_count($ci_string_in_tokenizer);

# Computing Token Indentation
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
#     @{$rslevel_stack} = a stack of total nesting depths at each
#       structural indentation level, where "total nesting depth" means
#       the nesting depth that would occur if every nesting token -- '{', '[',
#       and '(' -- , regardless of context, is used to compute a nesting
#       depth.

        #my $nesting_block_flag = ($nesting_block_string =~ /1$/);
        #my $nesting_list_flag = ($nesting_list_string =~ /1$/);

        my ( $ci_string_i, $level_i, $nesting_block_string_i,
            $nesting_list_string_i, $nesting_token_string_i,
            $nesting_type_string_i, );

        foreach $i ( @{$routput_token_list} )
        {    # scan the list of pre-tokens indexes

            # self-checking for valid token types
            my $type                    = $routput_token_type->[$i];
            my $forced_indentation_flag = $routput_indent_flag->[$i];

            # See if we should undo the $forced_indentation_flag.
            # Forced indentation after 'if', 'unless', 'while' and 'until'
            # expressions without trailing parens is optional and doesn't
            # always look good.  It is usually okay for a trailing logical
            # expression, but if the expression is a function call, code block,
            # or some kind of list it puts in an unwanted extra indentation
            # level which is hard to remove.
            #
            # Example where extra indentation looks ok:
            # return 1
            #   if $det_a < 0 and $det_b > 0
            #       or $det_a > 0 and $det_b < 0;
            #
            # Example where extra indentation is not needed because
            # the eval brace also provides indentation:
            # print "not " if defined eval {
            #     reduce { die if $b > 2; $a + $b } 0, 1, 2, 3, 4;
            # };
            #
            # The following rule works fairly well:
            #   Undo the flag if the end of this line, or start of the next
            #   line, is an opening container token or a comma.
            # This almost always works, but if not after another pass it will
            # be stable.
            if ( $forced_indentation_flag && $type eq 'k' ) {
                my $ixlast  = -1;
                my $ilast   = $routput_token_list->[$ixlast];
                my $toklast = $routput_token_type->[$ilast];
                if ( $toklast eq '#' ) {
                    $ixlast--;
                    $ilast   = $routput_token_list->[$ixlast];
                    $toklast = $routput_token_type->[$ilast];
                }
                if ( $toklast eq 'b' ) {
                    $ixlast--;
                    $ilast   = $routput_token_list->[$ixlast];
                    $toklast = $routput_token_type->[$ilast];
                }
                if ( $toklast =~ /^[\{,]$/ ) {
                    $forced_indentation_flag = 0;
                }
                else {
                    ( $toklast, my $i_next ) =
                      find_next_nonblank_token( $max_token_index, $rtokens,
                        $max_token_index );
                    if ( $toklast =~ /^[\{,]$/ ) {
                        $forced_indentation_flag = 0;
                    }
                }
            }

            # if we are already in an indented if, see if we should outdent
            if ($indented_if_level) {

                # don't try to nest trailing if's - shouldn't happen
                if ( $type eq 'k' ) {
                    $forced_indentation_flag = 0;
                }

                # check for the normal case - outdenting at next ';'
                elsif ( $type eq ';' ) {
                    if ( $level_in_tokenizer == $indented_if_level ) {
                        $forced_indentation_flag = -1;
                        $indented_if_level       = 0;
                    }
                }

                # handle case of missing semicolon
                elsif ( $type eq '}' ) {
                    if ( $level_in_tokenizer == $indented_if_level ) {
                        $indented_if_level = 0;

                        # TBD: This could be a subroutine call
                        $level_in_tokenizer--;
                        if ( @{$rslevel_stack} > 1 ) {
                            pop( @{$rslevel_stack} );
                        }
                        if ( length($nesting_block_string) > 1 )
                        {    # true for valid script
                            chop $nesting_block_string;
                            chop $nesting_list_string;
                        }

                    }
                }
            }

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
            if ( $type eq '{' || $type eq 'L' || $forced_indentation_flag > 0 )
            {

                # use environment before updating
                $container_environment =
                    $nesting_block_flag ? 'BLOCK'
                  : $nesting_list_flag  ? 'LIST'
                  :                       "";

                # if the difference between total nesting levels is not 1,
                # there are intervening non-structural nesting types between
                # this '{' and the previous unclosed '{'
                my $intervening_secondary_structure = 0;
                if ( @{$rslevel_stack} ) {
                    $intervening_secondary_structure =
                      $slevel_in_tokenizer - $rslevel_stack->[-1];
                }

     # Continuation Indentation
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

                # save the current states
                push( @{$rslevel_stack}, 1 + $slevel_in_tokenizer );
                $level_in_tokenizer++;

                if ($forced_indentation_flag) {

                    # break BEFORE '?' when there is forced indentation
                    if ( $type eq '?' ) { $level_i = $level_in_tokenizer; }
                    if ( $type eq 'k' ) {
                        $indented_if_level = $level_in_tokenizer;
                    }
                }

                if ( $routput_block_type->[$i] ) {
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
                if ( !$routput_block_type->[$i] ) {

                    # propagate flag down at nested open parens
                    if ( $routput_container_type->[$i] eq '(' ) {
                        $bit = 1 if $nesting_list_flag;
                    }

                  # use list continuation if not a logical grouping
                  # /^(if|elsif|unless|while|and|or|not|&&|!|\|\||for|foreach)$/
                    else {
                        $bit = 1
                          unless
                            $is_logical_container{ $routput_container_type->[$i]
                              };
                    }
                }
                $nesting_list_string .= $bit;
                $nesting_list_flag = $bit;

                $ci_string_in_tokenizer .=
                  ( $intervening_secondary_structure != 0 ) ? '1' : '0';
                $ci_string_sum = ones_count($ci_string_in_tokenizer);
                $continuation_string_in_tokenizer .=
                  ( $in_statement_continuation > 0 ) ? '1' : '0';

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

                my $total_ci = $ci_string_sum;
                if (
                    !$routput_block_type->[$i]    # patch: skip for BLOCK
                    && ($in_statement_continuation)
                    && !( $forced_indentation_flag && $type eq ':' )
                  )
                {
                    $total_ci += $in_statement_continuation
                      unless ( $ci_string_in_tokenizer =~ /1$/ );
                }

                $ci_string_i               = $total_ci;
                $in_statement_continuation = 0;
            }

            elsif ($type eq '}'
                || $type eq 'R'
                || $forced_indentation_flag < 0 )
            {

                # only a nesting error in the script would prevent popping here
                if ( @{$rslevel_stack} > 1 ) { pop( @{$rslevel_stack} ); }

                $level_i = --$level_in_tokenizer;

                # restore previous level values
                if ( length($nesting_block_string) > 1 )
                {    # true for valid script
                    chop $nesting_block_string;
                    $nesting_block_flag = ( $nesting_block_string =~ /1$/ );
                    chop $nesting_list_string;
                    $nesting_list_flag = ( $nesting_list_string =~ /1$/ );

                    chop $ci_string_in_tokenizer;
                    $ci_string_sum = ones_count($ci_string_in_tokenizer);

                    $in_statement_continuation =
                      chop $continuation_string_in_tokenizer;

                    # zero continuation flag at terminal BLOCK '}' which
                    # ends a statement.
                    if ( $routput_block_type->[$i] ) {

                        # ...These include non-anonymous subs
                        # note: could be sub ::abc { or sub 'abc
                        if ( $routput_block_type->[$i] =~ m/^sub\s*/gc ) {

                         # note: older versions of perl require the /gc modifier
                         # here or else the \G does not work.
                            if ( $routput_block_type->[$i] =~ /\G('|::|\w)/gc )
                            {
                                $in_statement_continuation = 0;
                            }
                        }

# ...and include all block types except user subs with
# block prototypes and these: (sort|grep|map|do|eval)
# /^(\}|\{|BEGIN|END|CHECK|INIT|AUTOLOAD|DESTROY|UNITCHECK|continue|;|if|elsif|else|unless|while|until|for|foreach)$/
                        elsif (
                            $is_zero_continuation_block_type{
                                $routput_block_type->[$i] } )
                        {
                            $in_statement_continuation = 0;
                        }

                        # ..but these are not terminal types:
                        #     /^(sort|grep|map|do|eval)$/ )
                        elsif (
                            $is_not_zero_continuation_block_type{
                                $routput_block_type->[$i] } )
                        {
                        }

                        # ..and a block introduced by a label
                        # /^\w+\s*:$/gc ) {
                        elsif ( $routput_block_type->[$i] =~ /:$/ ) {
                            $in_statement_continuation = 0;
                        }

                        # user function with block prototype
                        else {
                            $in_statement_continuation = 0;
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
                          if $routput_container_type->[$i] =~ /^[;,\{\}]$/;
                    }

                    elsif ( $tok eq ';' ) { $in_statement_continuation = 0 }
                }

                # use environment after updating
                $container_environment =
                    $nesting_block_flag ? 'BLOCK'
                  : $nesting_list_flag  ? 'LIST'
                  :                       "";
                $ci_string_i = $ci_string_sum + $in_statement_continuation;
                $nesting_block_string_i = $nesting_block_string;
                $nesting_list_string_i  = $nesting_list_string;
            }

            # not a structural indentation type..
            else {

                $container_environment =
                    $nesting_block_flag ? 'BLOCK'
                  : $nesting_list_flag  ? 'LIST'
                  :                       "";

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
                if (
                    $container_environment
                    && (   $type eq 'k' && $is_binary_keyword{$tok}
                        || $is_binary_type{$type} )
                  )
                {
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
                unless ( $tokenizer_self->{_saw_negative_indentation} ) {
                    $tokenizer_self->{_saw_negative_indentation} = 1;
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
                $nesting_type_string  .= $type;
            }

            #       /^[R\}\)\]]$/
            elsif ( $is_closing_type{$type} ) {
                $slevel_in_tokenizer--;
                my $char = chop $nesting_token_string;

                if ( $char ne $matching_start_token{$tok} ) {
                    $nesting_token_string .= $char . $tok;
                    $nesting_type_string  .= $type;
                }
                else {
                    chop $nesting_type_string;
                }
            }

            push( @block_type,            $routput_block_type->[$i] );
            push( @ci_string,             $ci_string_i );
            push( @container_environment, $container_environment );
            push( @container_type,        $routput_container_type->[$i] );
            push( @levels,                $level_i );
            push( @nesting_tokens,        $nesting_token_string_i );
            push( @nesting_types,         $nesting_type_string_i );
            push( @slevels,               $slevel_i );
            push( @token_type,            $fix_type );
            push( @type_sequence,         $routput_type_sequence->[$i] );
            push( @nesting_blocks,        $nesting_block_string );
            push( @nesting_lists,         $nesting_list_string );

            # now form the previous token
            if ( $im >= 0 ) {
                $num =
                  $$rtoken_map[$i] - $$rtoken_map[$im];    # how many characters

                if ( $num > 0 ) {
                    push( @tokens,
                        substr( $input_line, $$rtoken_map[$im], $num ) );
                }
            }
            $im = $i;
        }

        $num = length($input_line) - $$rtoken_map[$im];    # make the last token
        if ( $num > 0 ) {
            push( @tokens, substr( $input_line, $$rtoken_map[$im], $num ) );
        }

        $tokenizer_self->{_in_attribute_list} = $in_attribute_list;
        $tokenizer_self->{_in_quote}          = $in_quote;
        $tokenizer_self->{_quote_target} =
          $in_quote ? matching_end_token($quote_character) : "";
        $tokenizer_self->{_rhere_target_list} = $rhere_target_list;

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

#########i#############################################################
# Tokenizer routines which assist in identifying token types
#######################################################################

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
    # USES GLOBAL VARIABLES: $last_nonblank_type, $last_nonblank_token,
    # $statement_type

    my ( $prev_type, $tok, $next_type ) = @_;

    my $op_expected = UNKNOWN;

#print "tok=$tok last type=$last_nonblank_type last tok=$last_nonblank_token\n";

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

        # in a 'use' statement, numbers and v-strings are not true
        # numbers, so to avoid incorrect error messages, we will
        # mark them as unknown for now (use.t)
        # TODO: it would be much nicer to create a new token V for VERSION
        # number in a use statement.  Then this could be a check on type V
        # and related patches which change $statement_type for '=>'
        # and ',' could be removed.  Further, it would clean things up to
        # scan the 'use' statement with a separate subroutine.
        if (   ( $statement_type eq 'use' )
            && ( $last_nonblank_type =~ /^[nv]$/ ) )
        {
            $op_expected = UNKNOWN;
        }
    }

    # no operator after many keywords, such as "die", "warn", etc
    elsif ( $expecting_term_token{$last_nonblank_token} ) {

        # patch for dor.t (defined or).
        # perl functions which may be unary operators
        # TODO: This list is incomplete, and these should be put
        # into a hash.
        if (   $tok eq '/'
            && $next_type          eq '/'
            && $last_nonblank_type eq 'k'
            && $last_nonblank_token =~ /^eof|undef|shift|pop$/ )
        {
            $op_expected = OPERATOR;
        }
        else {
            $op_expected = TERM;
        }
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

        # patch for dor.t (defined or).
        if (   $tok eq '/'
            && $next_type eq '/'
            && $last_nonblank_token eq ']' )
        {
            $op_expected = OPERATOR;
        }
        else {
            $op_expected = TERM;
        }
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

sub new_statement_ok {

    # return true if the current token can start a new statement
    # USES GLOBAL VARIABLES: $last_nonblank_type

    return label_ok()    # a label would be ok here

      || $last_nonblank_type eq 'J';    # or we follow a label

}

sub label_ok {

    # Decide if a bare word followed by a colon here is a label
    # USES GLOBAL VARIABLES: $last_nonblank_token, $last_nonblank_type,
    # $brace_depth, @brace_type

    # if it follows an opening or closing code block curly brace..
    if ( ( $last_nonblank_token eq '{' || $last_nonblank_token eq '}' )
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # it is a label if and only if the curly encloses a code block
        return $brace_type[$brace_depth];
    }

    # otherwise, it is a label if and only if it follows a ';'
    # (real or fake)
    else {
        return ( $last_nonblank_type eq ';' );
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
    # USES GLOBAL VARIABLES: $last_nonblank_token, $last_nonblank_type,
    # $last_nonblank_block_type, $brace_depth, @brace_type

    # handle case of multiple '{'s

# print "BLOCK_TYPE EXAMINING: type=$last_nonblank_type tok=$last_nonblank_token\n";

    my ( $i, $rtokens, $rtoken_type, $max_token_index ) = @_;
    if (   $last_nonblank_token eq '{'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # opening brace where a statement may appear is probably
        # a code block but might be and anonymous hash reference
        if ( $brace_type[$brace_depth] ) {
            return decide_if_code_block( $i, $rtokens, $rtoken_type,
                $max_token_index );
        }

        # cannot start a code block within an anonymous hash
        else {
            return "";
        }
    }

    elsif ( $last_nonblank_token eq ';' ) {

        # an opening brace where a statement may appear is probably
        # a code block but might be and anonymous hash reference
        return decide_if_code_block( $i, $rtokens, $rtoken_type,
            $max_token_index );
    }

    # handle case of '}{'
    elsif ($last_nonblank_token eq '}'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # a } { situation ...
        # could be hash reference after code block..(blktype1.t)
        if ($last_nonblank_block_type) {
            return decide_if_code_block( $i, $rtokens, $rtoken_type,
                $max_token_index );
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
# /^(BEGIN|END|CHECK|INIT|AUTOLOAD|DESTROY|UNITCHECK|continue|if|elsif|else|unless|do|while|until|eval|for|foreach|map|grep|sort)$/
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

    # check bareword
    elsif ( $last_nonblank_type eq 'w' ) {
        return decide_if_code_block( $i, $rtokens, $rtoken_type,
            $max_token_index );
    }

    # anything else must be anonymous hash reference
    else {
        return "";
    }
}

sub decide_if_code_block {

    # USES GLOBAL VARIABLES: $last_nonblank_token
    my ( $i, $rtokens, $rtoken_type, $max_token_index ) = @_;
    my ( $next_nonblank_token, $i_next ) =
      find_next_nonblank_token( $i, $rtokens, $max_token_index );

    # we are at a '{' where a statement may appear.
    # We must decide if this brace starts an anonymous hash or a code
    # block.
    # return "" if anonymous hash, and $last_nonblank_token otherwise

    # initialize to be code BLOCK
    my $code_block_type = $last_nonblank_token;

    # Check for the common case of an empty anonymous hash reference:
    # Maybe something like sub { { } }
    if ( $next_nonblank_token eq '}' ) {
        $code_block_type = "";
    }

    else {

        # To guess if this '{' is an anonymous hash reference, look ahead
        # and test as follows:
        #
        # it is a hash reference if next come:
        #   - a string or digit followed by a comma or =>
        #   - bareword followed by =>
        # otherwise it is a code block
        #
        # Examples of anonymous hash ref:
        # {'aa',};
        # {1,2}
        #
        # Examples of code blocks:
        # {1; print "hello\n", 1;}
        # {$a,1};

        # We are only going to look ahead one more (nonblank/comment) line.
        # Strange formatting could cause a bad guess, but that's unlikely.
        my @pre_types  = @$rtoken_type[ $i + 1 .. $max_token_index ];
        my @pre_tokens = @$rtokens[ $i + 1 .. $max_token_index ];
        my ( $rpre_tokens, $rpre_types ) =
          peek_ahead_for_n_nonblank_pre_tokens(20);    # 20 is arbitrary but
                                                       # generous, and prevents
                                                       # wasting lots of
                                                       # time in mangled files
        if ( defined($rpre_types) && @$rpre_types ) {
            push @pre_types,  @$rpre_types;
            push @pre_tokens, @$rpre_tokens;
        }

        # put a sentinal token to simplify stopping the search
        push @pre_types, '}';

        my $jbeg = 0;
        $jbeg = 1 if $pre_types[0] eq 'b';

        # first look for one of these
        #  - bareword
        #  - bareword with leading -
        #  - digit
        #  - quoted string
        my $j = $jbeg;
        if ( $pre_types[$j] =~ /^[\'\"]/ ) {

            # find the closing quote; don't worry about escapes
            my $quote_mark = $pre_types[$j];
            for ( my $k = $j + 1 ; $k < $#pre_types ; $k++ ) {
                if ( $pre_types[$k] eq $quote_mark ) {
                    $j = $k + 1;
                    my $next = $pre_types[$j];
                    last;
                }
            }
        }
        elsif ( $pre_types[$j] eq 'd' ) {
            $j++;
        }
        elsif ( $pre_types[$j] eq 'w' ) {
            unless ( $is_keyword{ $pre_tokens[$j] } ) {
                $j++;
            }
        }
        elsif ( $pre_types[$j] eq '-' && $pre_types[ ++$j ] eq 'w' ) {
            $j++;
        }
        if ( $j > $jbeg ) {

            $j++ if $pre_types[$j] eq 'b';

            # it's a hash ref if a comma or => follow next
            if ( $pre_types[$j] eq ','
                || ( $pre_types[$j] eq '=' && $pre_types[ ++$j ] eq '>' ) )
            {
                $code_block_type = "";
            }
        }
    }

    return $code_block_type;
}

sub unexpected {

    # report unexpected token type and show where it is
    # USES GLOBAL VARIABLES: $tokenizer_self
    my ( $found, $expecting, $i_tok, $last_nonblank_i, $rpretoken_map,
        $rpretoken_type, $input_line )
      = @_;

    if ( ++$tokenizer_self->{_unexpected_error_count} <= MAX_NAG_MESSAGES ) {
        my $msg = "found $found where $expecting expected";
        my $pos = $$rpretoken_map[$i_tok];
        interrupt_logfile();
        my $input_line_number = $tokenizer_self->{_last_line_number};
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

sub is_non_structural_brace {

    # Decide if a brace or bracket is structural or non-structural
    # by looking at the previous token and type
    # USES GLOBAL VARIABLES: $last_nonblank_type, $last_nonblank_token

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

#########i#############################################################
# Tokenizer routines for tracking container nesting depths
#######################################################################

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
    my ( $aa, $pos ) = @_;

    # USES GLOBAL VARIABLES: $tokenizer_self, @current_depth,
    # @current_sequence_number, @depth_array, @starting_line_of_current_depth
    my $bb;
    $current_depth[$aa]++;
    $total_depth++;
    $total_depth[$aa][ $current_depth[$aa] ] = $total_depth;
    my $input_line_number = $tokenizer_self->{_last_line_number};
    my $input_line        = $tokenizer_self->{_line_text};

    # Sequence numbers increment by number of items.  This keeps
    # a unique set of numbers but still allows the relative location
    # of any type to be determined.
    $nesting_sequence_number[$aa] += scalar(@closing_brace_names);
    my $seqno = $nesting_sequence_number[$aa];
    $current_sequence_number[$aa][ $current_depth[$aa] ] = $seqno;

    $starting_line_of_current_depth[$aa][ $current_depth[$aa] ] =
      [ $input_line_number, $input_line, $pos ];

    for $bb ( 0 .. $#closing_brace_names ) {
        next if ( $bb == $aa );
        $depth_array[$aa][$bb][ $current_depth[$aa] ] = $current_depth[$bb];
    }

    # set a flag for indenting a nested ternary statement
    my $indent = 0;
    if ( $aa == QUESTION_COLON ) {
        $nested_ternary_flag[ $current_depth[$aa] ] = 0;
        if ( $current_depth[$aa] > 1 ) {
            if ( $nested_ternary_flag[ $current_depth[$aa] - 1 ] == 0 ) {
                my $pdepth = $total_depth[$aa][ $current_depth[$aa] - 1 ];
                if ( $pdepth == $total_depth - 1 ) {
                    $indent = 1;
                    $nested_ternary_flag[ $current_depth[$aa] - 1 ] = -1;
                }
            }
        }
    }
    return ( $seqno, $indent );
}

sub decrease_nesting_depth {

    my ( $aa, $pos ) = @_;

    # USES GLOBAL VARIABLES: $tokenizer_self, @current_depth,
    # @current_sequence_number, @depth_array, @starting_line_of_current_depth
    my $bb;
    my $seqno             = 0;
    my $input_line_number = $tokenizer_self->{_last_line_number};
    my $input_line        = $tokenizer_self->{_line_text};

    my $outdent = 0;
    $total_depth--;
    if ( $current_depth[$aa] > 0 ) {

        # set a flag for un-indenting after seeing a nested ternary statement
        $seqno = $current_sequence_number[$aa][ $current_depth[$aa] ];
        if ( $aa == QUESTION_COLON ) {
            $outdent = $nested_ternary_flag[ $current_depth[$aa] ];
        }

        # check that any brace types $bb contained within are balanced
        for $bb ( 0 .. $#closing_brace_names ) {
            next if ( $bb == $aa );

            unless ( $depth_array[$aa][$bb][ $current_depth[$aa] ] ==
                $current_depth[$bb] )
            {
                my $diff =
                  $current_depth[$bb] -
                  $depth_array[$aa][$bb][ $current_depth[$aa] ];

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
                      $starting_line_of_current_depth[$aa]
                      [ $current_depth[$aa] ];
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
                      ? $opening_brace_names[$bb]
                      : $closing_brace_names[$bb];
                    write_error_indicator_pair( @$rsl, '^' );
                    my $msg = <<"EOM";
Found $diff extra $bname$ess between $opening_brace_names[$aa] on line $sl and $closing_brace_names[$aa] on line $el
EOM

                    if ( $diff > 0 ) {
                        my $rml =
                          $starting_line_of_current_depth[$bb]
                          [ $current_depth[$bb] ];
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
        $current_depth[$aa]--;
    }
    else {

        my $saw_brace_error = get_saw_brace_error();
        if ( $saw_brace_error <= MAX_NAG_MESSAGES ) {
            my $msg = <<"EOM";
There is no previous $opening_brace_names[$aa] to match a $closing_brace_names[$aa] on line $input_line_number
EOM
            indicate_error( $msg, $input_line_number, $input_line, $pos, '^' );
        }
        increment_brace_error();
    }
    return ( $seqno, $outdent );
}

sub check_final_nesting_depths {
    my ($aa);

    # USES GLOBAL VARIABLES: @current_depth, @starting_line_of_current_depth

    for $aa ( 0 .. $#closing_brace_names ) {

        if ( $current_depth[$aa] ) {
            my $rsl =
              $starting_line_of_current_depth[$aa][ $current_depth[$aa] ];
            my $sl  = $$rsl[0];
            my $msg = <<"EOM";
Final nesting depth of $opening_brace_names[$aa]s is $current_depth[$aa]
The most recent un-matched $opening_brace_names[$aa] is on line $sl
EOM
            indicate_error( $msg, @$rsl, '^' );
            increment_brace_error();
        }
    }
}

#########i#############################################################
# Tokenizer routines for looking ahead in input stream
#######################################################################

sub peek_ahead_for_n_nonblank_pre_tokens {

    # returns next n pretokens if they exist
    # returns undef's if hits eof without seeing any pretokens
    # USES GLOBAL VARIABLES: $tokenizer_self
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

    # USES GLOBAL VARIABLES: $tokenizer_self
    my ( $rtokens, $max_token_index ) = @_;
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

#########i#############################################################
# Tokenizer guessing routines for ambiguous situations
#######################################################################

sub guess_if_pattern_or_conditional {

    # this routine is called when we have encountered a ? following an
    # unknown bareword, and we must decide if it starts a pattern or not
    # input parameters:
    #   $i - token index of the ? starting possible pattern
    # output parameters:
    #   $is_pattern = 0 if probably not pattern,  =1 if probably a pattern
    #   msg = a warning or diagnostic message
    # USES GLOBAL VARIABLES: $last_nonblank_token
    my ( $i, $rtokens, $rtoken_map, $max_token_index ) = @_;
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
        my $quoted_string;
        (
            $i, $in_quote, $quote_character, $quote_pos, $quote_depth,
            $quoted_string
          )
          = follow_quoted_string( $ibeg, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth, $max_token_index );

        if ($in_quote) {

            # we didn't find an ending ? on this line,
            # so we bias towards conditional
            $is_pattern = 0;
            $msg .= "conditional (no ending ? on this line)\n";

            # we found an ending ?, so we bias towards a pattern
        }
        else {

            if ( pattern_expected( $i, $rtokens, $max_token_index ) >= 0 ) {
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
    # USES GLOBAL VARIABLES: $last_nonblank_token
    my ( $i, $rtokens, $rtoken_map, $max_token_index ) = @_;
    my $is_pattern = 0;
    my $msg        = "guessing that / after $last_nonblank_token starts a ";

    if ( $i >= $max_token_index ) {
        "division (no end to pattern found on the line)\n";
    }
    else {
        my $ibeg = $i;
        my $divide_expected =
          numerator_expected( $i, $rtokens, $max_token_index );
        $i = $ibeg + 1;
        my $next_token = $$rtokens[$i];    # first token after slash

        # look for a possible ending / on this line..
        my $in_quote        = 1;
        my $quote_depth     = 0;
        my $quote_character = '';
        my $quote_pos       = 0;
        my $quoted_string;
        (
            $i, $in_quote, $quote_character, $quote_pos, $quote_depth,
            $quoted_string
          )
          = follow_quoted_string( $ibeg, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth, $max_token_index );

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

            if ( pattern_expected( $i, $rtokens, $max_token_index ) >= 0 ) {

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

# try to resolve here-doc vs. shift by looking ahead for
# non-code or the end token (currently only looks for end token)
# returns 1 if it is probably a here doc, 0 if not
sub guess_if_here_doc {

    # This is how many lines we will search for a target as part of the
    # guessing strategy.  It is a constant because there is probably
    # little reason to change it.
    # USES GLOBAL VARIABLES: $tokenizer_self, $current_package
    # %is_constant,
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

#########i#############################################################
# Tokenizer Routines for scanning identifiers and related items
#######################################################################

sub scan_bare_identifier_do {

    # this routine is called to scan a token starting with an alphanumeric
    # variable or package separator, :: or '.
    # USES GLOBAL VARIABLES: $current_package, $last_nonblank_token,
    # $last_nonblank_type,@paren_type, $paren_depth

    my ( $input_line, $i, $tok, $type, $prototype, $rtoken_map,
        $max_token_index )
      = @_;
    my $i_begin = $i;
    my $package = undef;

    my $i_beg = $i;

    # we have to back up one pretoken at a :: since each : is one pretoken
    if ( $tok eq '::' ) { $i_beg-- }
    if ( $tok eq '->' ) { $i_beg-- }
    my $pos_beg = $$rtoken_map[$i_beg];
    pos($input_line) = $pos_beg;

    #  Examples:
    #   A::B::C
    #   A::
    #   ::A
    #   A'B
    if ( $input_line =~ m/\G\s*((?:\w*(?:'|::)))*(?:(?:->)?(\w+))?/gc ) {

        my $pos  = pos($input_line);
        my $numc = $pos - $pos_beg;
        $tok = substr( $input_line, $pos_beg, $numc );

        # type 'w' includes anything without leading type info
        # ($,%,@,*) including something like abc::def::ghi
        $type = 'w';

        my $sub_name = "";
        if ( defined($2) ) { $sub_name = $2; }
        if ( defined($1) ) {
            $package = $1;

            # patch: don't allow isolated package name which just ends
            # in the old style package separator (single quote).  Example:
            #   use CGI':all';
            if ( !($sub_name) && substr( $package, -1, 1 ) eq '\'' ) {
                $pos--;
            }

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

        # if it is a bareword..
        if ( $type eq 'w' ) {

            # check for v-string with leading 'v' type character
            # (This seems to have presidence over filehandle, type 'Y')
            if ( $tok =~ /^v\d[_\d]*$/ ) {

                # we only have the first part - something like 'v101' -
                # look for more
                if ( $input_line =~ m/\G(\.\d[_\d]*)+/gc ) {
                    $pos  = pos($input_line);
                    $numc = $pos - $pos_beg;
                    $tok  = substr( $input_line, $pos_beg, $numc );
                }
                $type = 'v';

                # warn if this version can't handle v-strings
                report_v_string($tok);
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
                    # letters.  This is suggested good practice.
                    # Use 'sub_name' because something like
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
        ( $i, $error ) =
          inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );
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
# USES GLOBAL VARIABLES: $current_package, $last_nonblank_token, $in_attribute_list,
# $statement_type, $tokenizer_self

    my ( $input_line, $i, $tok, $rtokens, $rtoken_map, $id_scan_state,
        $max_token_index )
      = @_;
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
              find_next_nonblank_token_on_this_line( $i_beg, $rtokens,
                $max_token_index );
            if ( $next_nonblank_token =~ /(^#|^\s*$)/ ) {
                $blank_line = 1;
            }
        }
    }

    # handle non-blank line; identifier, if any, must follow
    unless ($blank_line) {

        if ( $id_scan_state eq 'sub' ) {
            ( $i, $tok, $type, $id_scan_state ) = do_scan_sub(
                $input_line, $i,             $i_beg,
                $tok,        $type,          $rtokens,
                $rtoken_map, $id_scan_state, $max_token_index
            );
        }

        elsif ( $id_scan_state eq 'package' ) {
            ( $i, $tok, $type ) =
              do_scan_package( $input_line, $i, $i_beg, $tok, $type, $rtokens,
                $rtoken_map, $max_token_index );
            $id_scan_state = '';
        }

        else {
            warning("invalid token in scan_id: $tok\n");
            $id_scan_state = '';
        }
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

sub check_prototype {
    my ( $proto, $package, $subname ) = @_;
    return unless ( defined($package) && defined($subname) );
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

sub do_scan_package {

    # do_scan_package parses a package name
    # it is called with $i_beg equal to the index of the first nonblank
    # token following a 'package' token.
    # USES GLOBAL VARIABLES: $current_package,

    my ( $input_line, $i, $i_beg, $tok, $type, $rtokens, $rtoken_map,
        $max_token_index )
      = @_;
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
        ( $i, $error ) =
          inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );
        if ($error) { warning("Possibly invalid package\n") }
        $current_package = $package;

        # check for error
        my ( $next_nonblank_token, $i_next ) =
          find_next_nonblank_token( $i, $rtokens, $max_token_index );
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
    # USES GLOBAL VARIABLES: $context, $last_nonblank_token,
    # $last_nonblank_type

    my ( $i, $id_scan_state, $identifier, $rtokens, $max_token_index,
        $expecting )
      = @_;
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
            elsif ( $tok eq '{' ) {

                # check for something like ${#} or ${©}
                if (   $identifier eq '$'
                    && $i + 2 <= $max_token_index
                    && $$rtokens[ $i + 2 ] eq '}'
                    && $$rtokens[ $i + 1 ] !~ /[\s\w]/ )
                {
                    my $next2 = $$rtokens[ $i + 2 ];
                    my $next1 = $$rtokens[ $i + 1 ];
                    $identifier .= $tok . $next1 . $next2;
                    $i += 2;
                    $id_scan_state = '';
                    last;
                }

                # skip something like ${xxx} or ->{
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

                    # Perl accepts '$^]' or '@^]', but
                    # there must not be a space before the ']'.
                    my $next1 = $$rtokens[ $i + 1 ];
                    if ( $next1 eq ']' ) {
                        $i++;
                        $identifier .= $next1;
                        $id_scan_state = "";
                        last;
                    }
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
                    if   ( $tok eq '{' ) { $type = 't' }
                    else                 { $type = 'i' }
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
                #
                # We have to be careful here.  If we are in an unknown state,
                # we will reject the punctuation variable.  In the following
                # example the '&' is a binary opeator but we are in an unknown
                # state because there is no sigil on 'Prima', so we don't
                # know what it is.  But it is a bad guess that
                # '&~' is a punction variable.
                # $self->{text}->{colorMap}->[
                #   Prima::PodView::COLOR_CODE_FOREGROUND
                #   & ~tb::COLOR_INDEX ] =
                #   $sec->{ColorCode}
                if ( $identifier eq '&' && $expecting ) {
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
                if ( $identifier =~ /^->/ && $last_nonblank_type eq 'w' ) {
                    $type = 'w';
                }
                else { $type = 'i' }
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

{

    # saved package and subnames in case prototype is on separate line
    my ( $package_saved, $subname_saved );

    sub do_scan_sub {

        # do_scan_sub parses a sub name and prototype
        # it is called with $i_beg equal to the index of the first nonblank
        # token following a 'sub' token.

        # TODO: add future error checks to be sure we have a valid
        # sub name.  For example, 'sub &doit' is wrong.  Also, be sure
        # a name is given if and only if a non-anonymous sub is
        # appropriate.
        # USES GLOBAL VARS: $current_package, $last_nonblank_token,
        # $in_attribute_list, %saw_function_definition,
        # $statement_type

        my (
            $input_line, $i,             $i_beg,
            $tok,        $type,          $rtokens,
            $rtoken_map, $id_scan_state, $max_token_index
        ) = @_;
        $id_scan_state = "";    # normally we get everything in one call
        my $subname = undef;
        my $package = undef;
        my $proto   = undef;
        my $attrs   = undef;
        my $match;

        my $pos_beg = $$rtoken_map[$i_beg];
        pos($input_line) = $pos_beg;

        # sub NAME PROTO ATTRS
        if (
            $input_line =~ m/\G\s*
        ((?:\w*(?:'|::))*)  # package - something that ends in :: or '
        (\w+)               # NAME    - required
        (\s*\([^){]*\))?    # PROTO   - something in parens
        (\s*:)?             # ATTRS   - leading : of attribute list
        /gcx
          )
        {
            $match   = 1;
            $subname = $2;
            $proto   = $3;
            $attrs   = $4;

            $package = ( defined($1) && $1 ) ? $1 : $current_package;
            $package =~ s/\'/::/g;
            if ( $package =~ /^\:/ ) { $package = 'main' . $package }
            $package =~ s/::$//;
            my $pos  = pos($input_line);
            my $numc = $pos - $pos_beg;
            $tok = 'sub ' . substr( $input_line, $pos_beg, $numc );
            $type = 'i';
        }

        # Look for prototype/attributes not preceded on this line by subname;
        # This might be an anonymous sub with attributes,
        # or a prototype on a separate line from its sub name
        elsif (
            $input_line =~ m/\G(\s*\([^){]*\))?  # PROTO
            (\s*:)?                              # ATTRS leading ':'
            /gcx
            && ( $1 || $2 )
          )
        {
            $match = 1;
            $proto = $1;
            $attrs = $2;

            # Handle prototype on separate line from subname
            if ($subname_saved) {
                $package = $package_saved;
                $subname = $subname_saved;
                $tok     = $last_nonblank_token;
            }
            $type = 'i';
        }

        if ($match) {

            # ATTRS: if there are attributes, back up and let the ':' be
            # found later by the scanner.
            my $pos = pos($input_line);
            if ($attrs) {
                $pos -= length($attrs);
            }

            my $next_nonblank_token = $tok;

            # catch case of line with leading ATTR ':' after anonymous sub
            if ( $pos == $pos_beg && $tok eq ':' ) {
                $type              = 'A';
                $in_attribute_list = 1;
            }

            # We must convert back from character position
            # to pre_token index.
            else {

                # I don't think an error flag can occur here ..but ?
                my $error;
                ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map,
                    $max_token_index );
                if ($error) { warning("Possibly invalid sub\n") }

                # check for multiple definitions of a sub
                ( $next_nonblank_token, my $i_next ) =
                  find_next_nonblank_token_on_this_line( $i, $rtokens,
                    $max_token_index );
            }

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
            $package_saved = "";
            $subname_saved = "";
            if ( $next_nonblank_token eq '{' ) {
                if ($subname) {

                    # Check for multiple definitions of a sub, but
                    # it is ok to have multiple sub BEGIN, etc,
                    # so we do not complain if name is all caps
                    if (   $saw_function_definition{$package}{$subname}
                        && $subname !~ /^[A-Z]+$/ )
                    {
                        my $lno = $saw_function_definition{$package}{$subname};
                        warning(
"already saw definition of 'sub $subname' in package '$package' at line $lno\n"
                        );
                    }
                    $saw_function_definition{$package}{$subname} =
                      $tokenizer_self->{_last_line_number};
                }
            }
            elsif ( $next_nonblank_token eq ';' ) {
            }
            elsif ( $next_nonblank_token eq '}' ) {
            }

            # ATTRS - if an attribute list follows, remember the name
            # of the sub so the next opening brace can be labeled.
            # Setting 'statement_type' causes any ':'s to introduce
            # attributes.
            elsif ( $next_nonblank_token eq ':' ) {
                $statement_type = $tok;
            }

            # see if PROTO follows on another line:
            elsif ( $next_nonblank_token eq '(' ) {
                if ( $attrs || $proto ) {
                    warning(
"unexpected '(' after definition or declaration of sub '$subname'\n"
                    );
                }
                else {
                    $id_scan_state  = 'sub';    # we must come back to get proto
                    $statement_type = $tok;
                    $package_saved  = $package;
                    $subname_saved  = $subname;
                }
            }
            elsif ($next_nonblank_token) {      # EOF technically ok
                warning(
"expecting ':' or ';' or '{' after definition or declaration of sub '$subname' but saw '$next_nonblank_token'\n"
                );
            }
            check_prototype( $proto, $package, $subname );
        }

        # no match but line not blank
        else {
        }
        return ( $i, $tok, $type, $id_scan_state );
    }
}

#########i###############################################################
# Tokenizer utility routines which may use CONSTANTS but no other GLOBALS
#########################################################################

sub find_next_nonblank_token {
    my ( $i, $rtokens, $max_token_index ) = @_;

    if ( $i >= $max_token_index ) {
        if ( !peeked_ahead() ) {
            peeked_ahead(1);
            $rtokens =
              peek_ahead_for_nonblank_token( $rtokens, $max_token_index );
        }
    }
    my $next_nonblank_token = $$rtokens[ ++$i ];

    if ( $next_nonblank_token =~ /^\s*$/ ) {
        $next_nonblank_token = $$rtokens[ ++$i ];
    }
    return ( $next_nonblank_token, $i );
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
    my ( $i, $rtokens, $max_token_index ) = @_;
    my $next_token = $$rtokens[ $i + 1 ];
    if ( $next_token eq '=' ) { $i++; }    # handle /=
    my ( $next_nonblank_token, $i_next ) =
      find_next_nonblank_token( $i, $rtokens, $max_token_index );

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
    my ( $i, $rtokens, $max_token_index ) = @_;
    my $next_token = $$rtokens[ $i + 1 ];
    if ( $next_token =~ /^[cgimosxp]/ ) { $i++; }    # skip possible modifier
    my ( $next_nonblank_token, $i_next ) =
      find_next_nonblank_token( $i, $rtokens, $max_token_index );

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
    my ( $i, $rtokens, $max_token_index ) = @_;
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

sub find_angle_operator_termination {

    # We are looking at a '<' and want to know if it is an angle operator.
    # We are to return:
    #   $i = pretoken index of ending '>' if found, current $i otherwise
    #   $type = 'Q' if found, '>' otherwise
    my ( $input_line, $i_beg, $rtoken_map, $expecting, $max_token_index ) = @_;
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
            my $str = substr( $input_line, $pos_beg, ( $pos - $pos_beg ) );

            # Reject if the closing '>' follows a '-' as in:
            # if ( VERSION < 5.009 && $op-> name eq 'aassign' ) { }
            if ( $expecting eq UNKNOWN ) {
                my $check = substr( $input_line, $pos - 2, 1 );
                if ( $check eq '-' ) {
                    return ( $i, $type );
                }
            }

            ######################################debug#####
            #write_diagnostics( "ANGLE? :$str\n");
            #print "ANGLE: found $1 at pos=$pos str=$str check=$check\n";
            ######################################debug#####
            $type = 'Q';
            my $error;
            ( $i, $error ) =
              inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );

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

    my ( $input_line, $i, $rtoken_map, $input_type, $max_token_index ) = @_;
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
    # TODO: v-strings may contain underscores
    pos($input_line) = $pos_beg;
    if ( $input_line =~ /\G((\d+)?\.\d+(\.\d+)+)/g ) {
        $pos = pos($input_line);
        my $numc = $pos - $pos_beg;
        $number = substr( $input_line, $pos_beg, $numc );
        $type = 'v';
        report_v_string($number);
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
    ( $i, $error ) =
      inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );
    if ($error) { warning("Possibly invalid number\n") }

    return ( $i, $type, $number );
}

sub inverse_pretoken_map {

    # Starting with the current pre_token index $i, scan forward until
    # finding the index of the next pre_token whose position is $pos.
    my ( $i, $pos, $rtoken_map, $max_token_index ) = @_;
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
    #   $saw_error - flag noting unbalanced quote on here target
    my ( $expecting, $i, $rtokens, $rtoken_map, $max_token_index ) = @_;
    my $ibeg                 = $i;
    my $found_target         = 0;
    my $here_doc_target      = '';
    my $here_quote_character = '';
    my $saw_error            = 0;
    my ( $next_nonblank_token, $i_next_nonblank, $next_token );
    $next_token = $$rtokens[ $i + 1 ];

    # perl allows a backslash before the target string (heredoc.t)
    my $backslash = 0;
    if ( $next_token eq '\\' ) {
        $backslash  = 1;
        $next_token = $$rtokens[ $i + 2 ];
    }

    ( $next_nonblank_token, $i_next_nonblank ) =
      find_next_nonblank_token_on_this_line( $i, $rtokens, $max_token_index );

    if ( $next_nonblank_token =~ /[\'\"\`]/ ) {

        my $in_quote    = 1;
        my $quote_depth = 0;
        my $quote_pos   = 0;
        my $quoted_string;

        (
            $i, $in_quote, $here_quote_character, $quote_pos, $quote_depth,
            $quoted_string
          )
          = follow_quoted_string( $i_next_nonblank, $in_quote, $rtokens,
            $here_quote_character, $quote_pos, $quote_depth, $max_token_index );

        if ($in_quote) {    # didn't find end of quote, so no target found
            $i = $ibeg;
            if ( $expecting == TERM ) {
                warning(
"Did not find here-doc string terminator ($here_quote_character) before end of line \n"
                );
                $saw_error = 1;
            }
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

    return ( $found_target, $here_doc_target, $here_quote_character, $i,
        $saw_error );
}

sub do_quote {

    # follow (or continue following) quoted string(s)
    # $in_quote return code:
    #   0 - ok, found end
    #   1 - still must find end of quote whose target is $quote_character
    #   2 - still looking for end of first of two quotes
    #
    # Returns updated strings:
    #  $quoted_string_1 = quoted string seen while in_quote=1
    #  $quoted_string_2 = quoted string seen while in_quote=2
    my (
        $i,               $in_quote,    $quote_character,
        $quote_pos,       $quote_depth, $quoted_string_1,
        $quoted_string_2, $rtokens,     $rtoken_map,
        $max_token_index
    ) = @_;

    my $in_quote_starting = $in_quote;

    my $quoted_string;
    if ( $in_quote == 2 ) {    # two quotes/quoted_string_1s to follow
        my $ibeg = $i;
        (
            $i, $in_quote, $quote_character, $quote_pos, $quote_depth,
            $quoted_string
          )
          = follow_quoted_string( $i, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth, $max_token_index );
        $quoted_string_2 .= $quoted_string;
        if ( $in_quote == 1 ) {
            if ( $quote_character =~ /[\{\[\<\(]/ ) { $i++; }
            $quote_character = '';
        }
        else {
            $quoted_string_2 .= "\n";
        }
    }

    if ( $in_quote == 1 ) {    # one (more) quote to follow
        my $ibeg = $i;
        (
            $i, $in_quote, $quote_character, $quote_pos, $quote_depth,
            $quoted_string
          )
          = follow_quoted_string( $ibeg, $in_quote, $rtokens, $quote_character,
            $quote_pos, $quote_depth, $max_token_index );
        $quoted_string_1 .= $quoted_string;
        if ( $in_quote == 1 ) {
            $quoted_string_1 .= "\n";
        }
    }
    return ( $i, $in_quote, $quote_character, $quote_pos, $quote_depth,
        $quoted_string_1, $quoted_string_2 );
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
    #   $quoted_string = the text of the quote (without quotation tokens)
    my ( $i_beg, $in_quote, $rtokens, $beginning_tok, $quote_pos, $quote_depth,
        $max_token_index )
      = @_;
    my ( $tok, $end_tok );
    my $i             = $i_beg - 1;
    my $quoted_string = "";

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

    ###################################################################
    # Case 1 (rare): loop for case of alphanumeric quote delimiter..
    # "quote_pos" is the position the current word to begin searching
    ###################################################################
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

                    # retain backslash unless it hides the end token
                    $quoted_string .= $tok
                      unless $$rtokens[ $i + 1 ] eq $end_tok;
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

                $quoted_string .=
                  substr( $tok, $old_pos, $quote_pos - $old_pos - 1 );

                $quote_depth--;

                if ( $quote_depth == 0 ) {
                    $in_quote--;
                    last;
                }
            }
            else {
                $quoted_string .= substr( $tok, $old_pos );
            }
        }
    }

    ########################################################################
    # Case 2 (normal): loop for case of a non-alphanumeric quote delimiter..
    ########################################################################
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

                # retain backslash unless it hides the beginning or end token
                $tok = $$rtokens[ ++$i ];
                $quoted_string .= '\\'
                  unless ( $tok eq $end_tok || $tok eq $beginning_tok );
            }
            $quoted_string .= $tok;
        }
    }
    if ( $i > $max_token_index ) { $i = $max_token_index }
    return ( $i, $in_quote, $beginning_tok, $quote_pos, $quote_depth,
        $quoted_string );
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

sub dump_token_types {
    my $class = shift;
    my $fh    = shift;

    # This should be the latest list of token types in use
    # adding NEW_TOKENS: add a comment here
    print $fh <<'END_OF_LIST';

Here is a list of the token types currently used for lines of type 'CODE'.  
For the following tokens, the "type" of a token is just the token itself.  

.. :: << >> ** && .. || // -> => += -= .= %= &= |= ^= *= <>
( ) <= >= == =~ !~ != ++ -- /= x=
... **= <<= >>= &&= ||= //= <=> 
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
    i    identifier of some type (with leading %, $, @, *, &, sub, -> )
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
    A    : used as attribute separator
    
    Here are the '_line_type' codes used internally:
    SYSTEM         - system-specific code before hash-bang line
    CODE           - line of perl code (including comments)
    POD_START      - line starting pod, such as '=head'
    POD            - pod documentation text
    POD_END        - last line of pod section, '=cut'
    HERE           - text of here-document
    HERE_END       - last line of here-doc (target word)
    FORMAT         - format section
    FORMAT_END     - last line of format section, '.'
    DATA_START     - __DATA__ line
    DATA           - unidentified text following __DATA__
    END_START      - __END__ line
    END            - unidentified text following __END__
    ERROR          - we are in big trouble, probably not a perl script
END_OF_LIST
}

BEGIN {

    # These names are used in error messages
    @opening_brace_names = qw# '{' '[' '(' '?' #;
    @closing_brace_names = qw# '}' ']' ')' ':' #;

    my @digraphs = qw(
      .. :: << >> ** && .. || // -> => += -= .= %= &= |= ^= *= <>
      <= >= == =~ !~ != ++ -- /= x= ~~
    );
    @is_digraph{@digraphs} = (1) x scalar(@digraphs);

    my @trigraphs = qw( ... **= <<= >>= &&= ||= //= <=> !~~ );
    @is_trigraph{@trigraphs} = (1) x scalar(@trigraphs);

    # make a hash of all valid token types for self-checking the tokenizer
    # (adding NEW_TOKENS : select a new character and add to this list)
    my @valid_token_types = qw#
      A b C G L R f h Q k t w i q n p m F pp mm U j J Y Z v
      { } ( ) [ ] ; + - / * | % ! x ~ = \ ? : . < > ^ &
      #;
    push( @valid_token_types, @digraphs );
    push( @valid_token_types, @trigraphs );
    push( @valid_token_types, '#' );
    push( @valid_token_types, ',' );
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
    @_ = qw( print printf sort exec system say);
    @is_indirect_object_taker{@_} = (1) x scalar(@_);

    # These tokens may precede a code block
    # patched for SWITCH/CASE
    @_ =
      qw( BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue if elsif else
      unless do while until eval for foreach map grep sort
      switch case given when);
    @is_code_block_token{@_} = (1) x scalar(@_);

    # I'll build the list of keywords incrementally
    my @Keywords = ();

    # keywords and tokens after which a value or pattern is expected,
    # but not an operator.  In other words, these should consume terms
    # to their right, or at least they are not expected to be followed
    # immediately by operators.
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
      UNITCHECK
      abs
      accept
      alarm
      and
      atan2
      bind
      binmode
      bless
      break
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

      switch
      case
      given
      when
      err
      say
    );

    # patched above for SWITCH/CASE given/when err say
    # 'err' is a fairly safe addition.
    # TODO: 'default' still needed if appropriate
    # 'use feature' seen, but perltidy works ok without it.
    # Concerned that 'default' could break code.
    push( @Keywords, @value_requestor );

    # These are treated the same but are not keywords:
    my @extra_vr = qw(
      constant
      vars
    );
    push( @value_requestor, @extra_vr );

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

    push( @Keywords, @operator_requestor );

    # These are treated the same but are not considered keywords:
    my @extra_or = qw(
      STDERR
      STDIN
      STDOUT
    );

    push( @operator_requestor, @extra_or );

    @expecting_operator_token{@operator_requestor} =
      (1) x scalar(@operator_requestor);

    # these token TYPES expect trailing operator but not a term
    # note: ++ and -- are post-increment and decrement, 'C' = constant
    my @operator_requestor_types = qw( ++ -- C <> q );
    @expecting_operator_types{@operator_requestor_types} =
      (1) x scalar(@operator_requestor_types);

    # these token TYPES consume values (terms)
    # note: pp and mm are pre-increment and decrement
    # f=semicolon in for,  F=file test operator
    my @value_requestor_type = qw#
      L { ( [ ~ !~ =~ ; . .. ... A : && ! || // = + - x
      **= += -= .= /= *= %= x= &= |= ^= <<= >>= &&= ||= //=
      <= >= == != => \ > < % * / ? & | ** <=> ~~ !~~
      f F pp mm Y p m U J G j >> << ^ t
      #;
    push( @value_requestor_type, ',' )
      ;    # (perl doesn't like a ',' in a qw block)
    @expecting_term_types{@value_requestor_type} =
      (1) x scalar(@value_requestor_type);

    # Note: the following valid token types are not assigned here to
    # hashes requesting to be followed by values or terms, but are
    # instead currently hard-coded into sub operator_expected:
    # ) -> :: Q R Z ] b h i k n v w } #

    # For simple syntax checking, it is nice to have a list of operators which
    # will really be unhappy if not followed by a term.  This includes most
    # of the above...
    %really_want_term = %expecting_term_types;

    # with these exceptions...
    delete $really_want_term{'U'}; # user sub, depends on prototype
    delete $really_want_term{'F'}; # file test works on $_ if no following term
    delete $really_want_term{'Y'}; # indirect object, too risky to check syntax;
                                   # let perl do it

    @_ = qw(q qq qw qx qr s y tr m);
    @is_q_qq_qw_qx_qr_s_y_tr_m{@_} = (1) x scalar(@_);

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
    push( @Keywords, @special_keywords );

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

Perl::Tidy - Parses and beautifies perl source

=head1 SYNOPSIS

    use Perl::Tidy;

    Perl::Tidy::perltidy(
        source            => $source,
        destination       => $destination,
        stderr            => $stderr,
        argv              => $argv,
        perltidyrc        => $perltidyrc,
        logfile           => $logfile,
        errorfile         => $errorfile,
        formatter         => $formatter,           # callback object (see below)
        dump_options      => $dump_options,
        dump_options_type => $dump_options_type,
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

        source            - the source of the script to be formatted
        destination       - the destination of the formatted output
        stderr            - standard error output
        perltidyrc        - the .perltidyrc file
        logfile           - the .LOG file stream, if any 
        errorfile         - the .ERR file stream, if any
        dump_options      - ref to a hash to receive parameters (see below), 
        dump_options_type - controls contents of dump_options
        dump_getopt_flags - ref to a hash to receive Getopt flags
        dump_options_category - ref to a hash giving category of options
        dump_abbreviations    - ref to a hash giving all abbreviations

The following chart illustrates the logic used to decide how to
treat a parameter.

   ref($param)  $param is assumed to be:
   -----------  ---------------------
   undef        a filename
   SCALAR       ref to string
   ARRAY        ref to array
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

=item dump_options

If the B<dump_options> parameter is given, it must be the reference to a hash.
In this case, the parameters contained in any perltidyrc configuration file
will be placed in this hash and perltidy will return immediately.  This is
equivalent to running perltidy with --dump-options, except that the perameters
are returned in a hash rather than dumped to standard output.  Also, by default
only the parameters in the perltidyrc file are returned, but this can be
changed (see the next parameter).  This parameter provides a convenient method
for external programs to read a perltidyrc file.  An example program using
this feature, F<perltidyrc_dump.pl>, is included in the distribution.

Any combination of the B<dump_> parameters may be used together.

=item dump_options_type

This parameter is a string which can be used to control the parameters placed
in the hash reference supplied by B<dump_options>.  The possible values are
'perltidyrc' (default) and 'full'.  The 'full' parameter causes both the
default options plus any options found in a perltidyrc file to be returned.

=item dump_getopt_flags

If the B<dump_getopt_flags> parameter is given, it must be the reference to a
hash.  This hash will receive all of the parameters that perltidy understands
and flags that are passed to Getopt::Long.  This parameter may be
used alone or with the B<dump_options> flag.  Perltidy will
exit immediately after filling this hash.  See the demo program
F<perltidyrc_dump.pl> for example usage.

=item dump_options_category

If the B<dump_options_category> parameter is given, it must be the reference to a
hash.  This hash will receive a hash with keys equal to all long parameter names
and values equal to the title of the corresponding section of the perltidy manual.
See the demo program F<perltidyrc_dump.pl> for example usage.

=item dump_abbreviations

If the B<dump_abbreviations> parameter is given, it must be the reference to a
hash.  This hash will receive all abbreviations used by Perl::Tidy.  See the
demo program F<perltidyrc_dump.pl> for example usage.

=back

=head1 EXAMPLE

The following example passes perltidy a snippet as a reference
to a string and receives the result back in a reference to
an array.  

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

=head1 Using the B<formatter> Callback Object

The B<formatter> parameter is an optional callback object which allows
the calling program to receive tokenized lines directly from perltidy for
further specialized processing.  When this parameter is used, the two
formatting options which are built into perltidy (beautification or
html) are ignored.  The following diagram illustrates the logical flow:

                    |-- (normal route)   -> code beautification
  caller->perltidy->|-- (-html flag )    -> create html 
                    |-- (formatter given)-> callback to write_line

This can be useful for processing perl scripts in some way.  The 
parameter C<$formatter> in the perltidy call,

        formatter   => $formatter,  

is an object created by the caller with a C<write_line> method which
will accept and process tokenized lines, one line per call.  Here is
a simple example of a C<write_line> which merely prints the line number,
the line type (as determined by perltidy), and the text of the line:

 sub write_line {
 
     # This is called from perltidy line-by-line
     my $self              = shift;
     my $line_of_tokens    = shift;
     my $line_type         = $line_of_tokens->{_line_type};
     my $input_line_number = $line_of_tokens->{_line_number};
     my $input_line        = $line_of_tokens->{_line_text};
     print "$input_line_number:$line_type:$input_line";
 }

The complete program, B<perllinetype>, is contained in the examples section of
the source distribution.  As this example shows, the callback method
receives a parameter B<$line_of_tokens>, which is a reference to a hash
of other useful information.  This example uses these hash entries:

 $line_of_tokens->{_line_number} - the line number (1,2,...)
 $line_of_tokens->{_line_text}   - the text of the line
 $line_of_tokens->{_line_type}   - the type of the line, one of:

    SYSTEM         - system-specific code before hash-bang line
    CODE           - line of perl code (including comments)
    POD_START      - line starting pod, such as '=head'
    POD            - pod documentation text
    POD_END        - last line of pod section, '=cut'
    HERE           - text of here-document
    HERE_END       - last line of here-doc (target word)
    FORMAT         - format section
    FORMAT_END     - last line of format section, '.'
    DATA_START     - __DATA__ line
    DATA           - unidentified text following __DATA__
    END_START      - __END__ line
    END            - unidentified text following __END__
    ERROR          - we are in big trouble, probably not a perl script

Most applications will be only interested in lines of type B<CODE>.  For
another example, let's write a program which checks for one of the
so-called I<naughty matching variables> C<&`>, C<$&>, and C<$'>, which
can slow down processing.  Here is a B<write_line>, from the example
program B<find_naughty.pl>, which does that:

 sub write_line {
 
     # This is called back from perltidy line-by-line
     # We're looking for $`, $&, and $'
     my ( $self, $line_of_tokens ) = @_;
 
     # pull out some stuff we might need
     my $line_type         = $line_of_tokens->{_line_type};
     my $input_line_number = $line_of_tokens->{_line_number};
     my $input_line        = $line_of_tokens->{_line_text};
     my $rtoken_type       = $line_of_tokens->{_rtoken_type};
     my $rtokens           = $line_of_tokens->{_rtokens};
     chomp $input_line;
 
     # skip comments, pod, etc
     return if ( $line_type ne 'CODE' );
 
     # loop over tokens looking for $`, $&, and $'
     for ( my $j = 0 ; $j < @$rtoken_type ; $j++ ) {
 
         # we only want to examine token types 'i' (identifier)
         next unless $$rtoken_type[$j] eq 'i';
 
         # pull out the actual token text
         my $token = $$rtokens[$j];
 
         # and check it
         if ( $token =~ /^\$[\`\&\']$/ ) {
             print STDERR
               "$input_line_number: $token\n";
         }
     }
 }

This example pulls out these tokenization variables from the $line_of_tokens
hash reference:

     $rtoken_type = $line_of_tokens->{_rtoken_type};
     $rtokens     = $line_of_tokens->{_rtokens};

The variable C<$rtoken_type> is a reference to an array of token type codes,
and C<$rtokens> is a reference to a corresponding array of token text.
These are obviously only defined for lines of type B<CODE>.
Perltidy classifies tokens into types, and has a brief code for each type.
You can get a complete list at any time by running perltidy from the
command line with

     perltidy --dump-token-types

In the present example, we are only looking for tokens of type B<i>
(identifiers), so the for loop skips past all other types.  When an
identifier is found, its actual text is checked to see if it is one
being sought.  If so, the above write_line prints the token and its
line number.

The B<formatter> feature is relatively new in perltidy, and further
documentation needs to be written to complete its description.  However,
several example programs have been written and can be found in the
B<examples> section of the source distribution.  Probably the best way
to get started is to find one of the examples which most closely matches
your application and start modifying it.

For help with perltidy's pecular way of breaking lines into tokens, you
might run, from the command line, 

 perltidy -D filename

where F<filename> is a short script of interest.  This will produce
F<filename.DEBUG> with interleaved lines of text and their token types.
The B<-D> flag has been in perltidy from the beginning for this purpose.
If you want to see the code which creates this file, it is
C<write_debug_entry> in Tidy.pm.

=head1 EXPORT

  &perltidy

=head1 CREDITS

Thanks to Hugh Myers who developed the initial modular interface 
to perltidy.

=head1 VERSION

This man page documents Perl::Tidy version 20070801.

=head1 AUTHOR

 Steve Hancock
 perltidy at users.sourceforge.net

=head1 SEE ALSO

The perltidy(1) man page describes all of the features of perltidy.  It
can be found at http://perltidy.sourceforge.net.

=cut
