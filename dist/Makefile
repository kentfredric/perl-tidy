# Sample Makefile to install perltidy and its man page on a Unix system

# Step 1: edit BINDIR to reflect where you want to put perltidy
# Debian Linux standard is:
# BINDIR = /usr/bin
# This is fairly standard:
BINDIR	= /usr/local/bin

# Step 2: edit MANDIR to reflect where you want to put the man page, perltidy.1
# or comment out MANDIR to skip installing a man page.
# Note: The value of MANDIR varies a lot, so be sure to check this.
# Hint: if directory usr/local/man/man1 does not exist,
# you might find the man path by entering the command
#   perl '-V:install.*' and looking for the value of 'installman1dir'
# Debian Linux standard:
# MANDIR = /usr/share/man/man1
MANDIR	= /usr/local/man/man1

# Step 3: Then become superuser (if necessary) and issue the command
# make install

PROGRAM	= perltidy
PROG2 = perl2web

install: $(PROGRAM)
	cp $(PROGRAM) $(BINDIR)/$(PROGRAM)
	chmod 755 $(BINDIR)/$(PROGRAM)
	if test -d $(MANDIR); then cp $(PROGRAM).1 $(MANDIR)/$(PROGRAM).1; \
	chmod 644 $(MANDIR)/$(PROGRAM).1; \
	if test -d $(MANDIR); then cp $(PROG2).1 $(MANDIR)/$(PROG2).1; \
	chmod 644 $(MANDIR)/$(PROG2).1; \
	fi

uninstall: 
	if test -d $(BINDIR); then rm -f $(BINDIR)/$(PROGRAM); fi
	if test -d $(MANDIR); then rm -f $(MANDIR)/$(PROGRAM).1; fi
	if test -d $(MANDIR); then rm -f $(MANDIR)/$(PROG2).1; fi

###############################################################
# The rest of this is just to build the distribution file
# I've removed pods because they waste space and can be obtained from CVS
###############################################################

VERSION=20011117
PROG=$(PROGRAM)-$(VERSION)
PROGTEST=$(PROGRAM)-$(VERSION)-test
TMPDIR=/tmp/$(PROG)
dist:
	(cd ../docs; make)
	# cp ../perltidy.pl perltidy
	mkdir $(TMPDIR)
	#mkdir $(TMPDIR)/test
	mkdir $(TMPDIR)/docs
	cp ../src/perltidy $(TMPDIR)
	cp lextest.pl $(TMPDIR)
	cp perltidy.bat $(TMPDIR)
	cp Makefile.install $(TMPDIR)/Makefile
	cp ../docs/README $(TMPDIR)
	cp ../docs/COPYING $(TMPDIR)
	cp ../docs/perl2web.1 $(TMPDIR)
	cp ../docs/perltidy.1 $(TMPDIR)
	#cp ../test/*.t $(TMPDIR)/test
	cp ../docs/perltidy.pod $(TMPDIR)/docs
	cp ../docs/perl2web.pod $(TMPDIR)/docs
	cp ../docs/tutorial.pod $(TMPDIR)/docs
	cp ../docs/testfile.pl $(TMPDIR)/docs
#	cp ../docs/ChangeLog.pod $(TMPDIR)/docs
#	cp ../docs/BUGS.pod $(TMPDIR)/docs
	cp ../docs/perltidy.html $(TMPDIR)/docs
	cp ../docs/perl2web.html $(TMPDIR)/docs
	cp ../docs/tutorial.html $(TMPDIR)/docs
	cp ../docs/ChangeLog.html $(TMPDIR)/docs
	cp ../docs/BUGS.html $(TMPDIR)/docs
	cp ../docs/perltidy.txt $(TMPDIR)/docs
	cp ../docs/perl2web.txt $(TMPDIR)/docs
	cp ../docs/tutorial.txt $(TMPDIR)/docs
	cp ../docs/ChangeLog.txt $(TMPDIR)/docs
	cp ../docs/BUGS.txt $(TMPDIR)/docs
	cp ../docs/TODO.txt $(TMPDIR)/docs
	(cd $(TMPDIR)/..; \
	    tar cfz /tmp/$(PROG).tgz $(PROG))
	mv /tmp/$(PROG).tgz ../archive
	flipall $(TMPDIR) -m
	#flipall $(TMPDIR)/test -m
	flipall $(TMPDIR)/docs -m
	(cd $(TMPDIR)/..; \
	    zip $(PROG).zip $(PROG)/*  $(PROG)/*/*)
	mv /tmp/$(PROG).zip ../archive
	rm -rf $(TMPDIR)
	mkdir $(TMPDIR)
	mkdir $(TMPDIR)/test
	cp ../test/README $(TMPDIR)/test
	cp ../test/*.t $(TMPDIR)/test
	(cd $(TMPDIR)/..; \
	    tar cfz /tmp/$(PROGTEST).tgz $(PROG))
	mv /tmp/$(PROGTEST).tgz ../archive
	flipall $(TMPDIR)/test -m
	(cd $(TMPDIR)/..; \
	    zip $(PROGTEST).zip $(PROG)/test/*  )
	mv /tmp/$(PROGTEST).zip ../archive
	rm -rf $(TMPDIR)

.PHONY: docs

docs:
	(cd ../docs; make)
