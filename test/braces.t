# Some one-line block test cases

# Here the inner block is small enough to be one-line block in 80 columns
{{{{{{${msg}="Hello World!"; print "My message: ${msg}\n";}}}}}}

# Here the inner block is small enough to be one-line block in 80 columns
# Using the old whitespace would have caused it to be broken in two.
{{{{{{${msg}=                      "Hello World!"; print "My message: ${msg}\n";}}}}}}

# Here the inner block is too large to be one-line block in 80 columns
{{{{{{${msg}="Hello World!"; print "The good message is: ${msg}\n";}}}}}}

# This illustrates a former problem which has been solved.
# Here the formatted inner block is too large to be a one-line
# block in 80 columns, but the formatter used to use old whitespace
# when checking for one-line-block lengths (which lack spaces around the
# '=' and within the inner braces), and it makes a one-line block.  The
# result was that the line is too long and gets outdented.  Running the
# script through a second time fixed the problem.  This is now fixed,
# so this line should be broken into two lines:
{{{{{{${msg}="Hello World!"; print "The message is: ${msg}\n";}}}}}}

# Here the inner block is too large to be one-line block in 80 columns
# but it is followed by a here doc, so the line cannot be broken after
# the here doc target string (unless the here-doc were to be moved);
{{{{{{${msg}=<<END; print "The really, really, important message of the day is: ${msg}\n";}}}}}}
Hello World!
END
