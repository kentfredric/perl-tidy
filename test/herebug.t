# This is a current perltidy BUG: here doc in replacement string.
# When the tokenizer sees the modifier 'e', it should look through the
# replacement string for here docs and process them. 
# This will require a patch to the tokenizer to save the
# replacement string for reparsing.
my $text="Hello World!\n";
$text =~ s@Hello@<<'END'@e;
Goodbye 
Cruel
END
print "$text\n";

# On multiple lines
my $text="Hello World!\n";
$text =~ s@
Hello     # this is the word Hello\@
@<<'END'  # This will be replaced with a here doc
@ex;      # End of regex; here comes the here-doc text
Goodbye 
Cruel
END
print "$text\n";

#  Another multi-line example; the here-doc follows the 'e'
my $html="Hello World!\n";
my $q = "Crazy";
$html =~ s@World@"$q".<<'END_SCRIPT'.
"Mad" .
"Mad"
@e;
Well Well
END_SCRIPT
print "$html\n";

# A substitution within a replacement string
$text="Hello World";
$text1="Hello World";
$text =~ s@Hello@
$text1=~
s|Hello|<<'END'|ex
@ex;
Goodbye 
Cruel
END
print "$text\n";
print "$text1\n";
