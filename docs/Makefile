.phony : docs

DOCS=perltidy.1 \
perl2web.1 \
README \
README.html \
perltidy.txt \
perltidy.html \
tutorial.html \
tutorial.txt \
BUGS.html \
BUGS.txt \
FAQ.html \
FAQ.txt \
TODO.html \
TODO.txt \
ChangeLog.txt \
ChangeLog.html \
perl2web.txt \
perl2web.html 

docs: $(DOCS)

README: README.pod
	pod2text README.pod >README

perltidy.1: perltidy.pod
	pod2man perltidy.pod >perltidy.1

perl2web.1: perl2web.pod
	pod2man perl2web.pod >perl2web.1

README.html: README.pod
	pod2html README.pod >README.html

perltidy.txt: perltidy.pod
	pod2text perltidy.pod >perltidy.txt

perltidy.html: perltidy.pod
	pod2html perltidy.pod >perltidy.html

perl2web.txt: perl2web.pod
	pod2text perl2web.pod >perl2web.txt

perl2web.html: perl2web.pod
	pod2html perl2web.pod >perl2web.html

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

