# anonomyous sub with prototype
*$AUTOLOAD = sub () { $val };

# some one-line subs exactly 80 characters long when formatted
sub proxy_authorization_basic { shift->_basic_auth("Proxy-Authorization", @_) }
sub get_pdls {my($this) = @_; return ($this->{ParNames},$this->{ParObjs});}
sub d { $CPAN::Frontend->myprint(shift->format_result('Distribution',@_));}
sub PDL::initialize { bless &PDL::pdl_null(), (ref $_[0] ? ref $_[0]: $_[0]) }

# The extra space between the sub name and prototypes will be removed;
# prototypes do not get vertically aligned because they are part of the
# sub token
sub O_PRIORITY		() { 0 }
sub O_SECTION		() { 1 }
sub O_MAINT_FROM	() { 2 } # undef for non-specific, else listref
sub O_MAINT_TO		() { 3 } # undef if there's no maint override
sub message (@) { print STDERR (@_) unless $opt_quiet; }
sub verbose (@) { print STDERR (@_) if $opt_verbose; }
sub debug   (@) { print STDERR (@_) if $opt_debug;   }
sub trace   (@) { print STDERR (@_) if $opt_trace;   }

    # examples of formatting anonymous subs
    # continuation indentation continues after the closing curly brace
    $convert = sub { byte( $_[0] ) }
      if $BITPIX == 8;

      my $esub =
              $cnd1 ? sub { $_[0]; }
              : $cnd2 ? \&extract_email_address : \&extract_email_name;

# simple example, showing why we can't outdent the '}' of an anonymous sub
$SIG{__DIE__} = sub {
    require Carp;
    Carp::confess;
  }

  if DEBUG;

    # It would be nice to undo the continuation indentation of the terminal '}' 
    # of an anonymous sub.  It would be necessary to look ahead for a
    # BLOCK termination; this isn't done yet.
    if (@options) {
        return sub {
            my $objname;

            foreach $objname (@options) {
                $objname = "main::$objname" unless $objname =~ /::/;
                eval "showlex_obj('&$objname', \\&$objname)";
            }
          }
    }

# Note that the anonymous sub here can outdent
# (not in a BLOCK) 
%do_on_control = (
    %do_on_control, %destination_ctrl, 'plain' => sub {
        reset_char_props();
    },    # <- outdent ok here; not in BLOCK
);

# had unwanted break at one time
$a = [ [CopyName], [], sub { "__copy" }, [] ];
