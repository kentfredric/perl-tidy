# Sample Makefile to install perltidy and its man pages on a Unix system

# Step 1: edit BINDIR to reflect where you want to put perltidy
# This is fairly standard:
BINDIR	= /usr/local/bin

# Step 2: edit MANDIR to reflect where you want to put the man page, perltidy.1
# or comment out MANDIR to skip installing a man page.
# (Man pages can be found on the web at http://perltidy.sourceforge.net)
#
# Note: The value of MANDIR varies a lot, so be sure to check this.
MANDIR	= /usr/local/man/man1

# Step 3: Then become superuser (if necessary) and issue the command
# make install

PROGRAM	= perltidy
PROG2	= perl2web

install: $(PROGRAM)
	cp $(PROGRAM) $(BINDIR)/$(PROGRAM)
	chmod 755 $(BINDIR)/$(PROGRAM)
	if test -d $(MANDIR); then cp $(PROGRAM).1 $(MANDIR)/$(PROGRAM).1; \
	chmod 644 $(MANDIR)/$(PROGRAM).1; \
	fi
	if test -d $(MANDIR); then cp $(PROG2).1 $(MANDIR)/$(PROG2).1; \
	chmod 644 $(MANDIR)/$(PROG2).1; \
	fi

uninstall: 
	if test -d $(BINDIR); then rm -f $(BINDIR)/$(PROGRAM); fi
	if test -d $(MANDIR); then rm -f $(MANDIR)/$(PROGRAM).1; fi
	if test -d $(MANDIR); then rm -f $(MANDIR)/$(PROG2).1; fi
