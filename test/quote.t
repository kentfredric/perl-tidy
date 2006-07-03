# This line has been nicely formatted by the user.  It should
# retain the whitespace and remain nice.
$src_line =~ s{ _ref \s* (?= [\{[(] ) }  # If _ref precedes opening bracket...
              {_ref->}gxms;    # ...insert an arrow

# same line with long comment will no longer be aligned after a break:
{
$src_line =~ s{ _ref \s* (?= [\{[(] ) }  # If _ref precedes opening bracket...
              {_ref->}gxms;    # ...insert an arrow
}

# this file illustrates how the q* operators allow comments
# a space after qw causes the # to become a comment!
my @str = qw # Hi!
  (
  # this is a comment
  a b c d e
  # this is another comment
  );
print "@str\n";

# without a space, the # becomes the quote symbol!
my @str = qw# Hi!
  (
  a b c d e
  )#;
print "@str\n";

# a space after q causes the # to become a comment!
my $str = q # Hi!
(
a b c d e
);
print "$str\n";

# no space after q causes the # to become the quote symbol
my $str = q# Hi!
(
a b c d e
); #;
print "$str\n";

my $str = q/#abc#/;
print "$str\n";

my $str = q
# this is a comment within a quote!
(
a b c d e
);
print "$str\n";

#Here is an example from Mastering Regular Expressions 
$Cnested = qq<
   $OpenParen                            #  (
      $ctext*                            #     normal*
      (?: $quoted_pair $ctext* )*        #     (special normal*)*
   $CloseParen                           #                       )
>;

print "$Cnested\n";

{

    # multiline quote for testing behavior of terminal ');'
    open INFILE_COPY, ">$input_file_copy"
      or die (
        "Couldn't open $input_file_copy: $!\n
                	It is needed to check syntax; deactivate with -nsyn"
    );
}

# x is operator
($x,$y,$text)= $pdf->textParagraph(10,800,250,800, join(' ',q|Liber vel Legis. inforom summa cum laude. | x (1000/$f)));
