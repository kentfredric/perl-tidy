.PHONY : all
all :
	(cd ../docs; make)
	cp ../docs/*.html .
	alconv -le alconv.txt
	cp node000.html index.html
	chmod a+r *.html
	chmod a+r *.gif
	(cd ..; tar cvfz htdocs.tgz htdocs/*.html htdocs/*.gif htdocs/COPYING)
