.phony : docs

DOCS=perltidy.1 \
INSTALL \
INSTALL.html \
README \
README.html \
perltidy.txt \
perltidy.html \
tutorial.html \
tutorial.txt \
stylekey.html \
BUGS.html \
BUGS.txt \
FAQ.html \
FAQ.txt \
TODO.html \
TODO.txt \
ChangeLog.txt \
ChangeLog.html

docs: $(DOCS)

README: README.pod
	pod2text README.pod >README

INSTALL: INSTALL.pod
	pod2text INSTALL.pod >INSTALL

perltidy.1: ../src/perltidy
	pod2man ../src/perltidy >perltidy.1

README.html: README.pod
	pod2html README.pod >README.html

INSTALL.html: INSTALL.pod
	pod2html INSTALL.pod >INSTALL.html

perltidy.txt: ../src/perltidy
	pod2text ../src/perltidy >perltidy.txt

perltidy.html: ../src/perltidy
	pod2html ../src/perltidy >perltidy.html

stylekey.html: stylekey.pod
	pod2html stylekey.pod >stylekey.html

tutorial.html: tutorial.pod
	pod2html tutorial.pod >tutorial.html

tutorial.txt: tutorial.pod
	pod2text tutorial.pod >tutorial.txt

FAQ.html: FAQ.pod
	pod2html FAQ.pod >FAQ.html

FAQ.txt: FAQ.pod
	pod2text FAQ.pod >FAQ.txt

BUGS.html: BUGS.pod
	pod2html BUGS.pod >BUGS.html

BUGS.txt: BUGS.pod
	pod2text BUGS.pod >BUGS.txt

ChangeLog.txt: ChangeLog.pod
	pod2text ChangeLog.pod >ChangeLog.txt

ChangeLog.html: ChangeLog.pod
	pod2html ChangeLog.pod >ChangeLog.html

TODO.html: TODO.pod
	pod2html TODO.pod >TODO.html

TODO.txt: TODO.pod
	pod2text TODO.pod >TODO.txt
