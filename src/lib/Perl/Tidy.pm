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
