CLASS=class-system-01-13.plt
CLASS_LATEST=class-system-latest.plt
CCS=login.ccs.neu.edu
CCS_PATH=/course/cs2510h/.www/

scrbls := $(wildcard *.scrbl) $(wildcard assignments/*.scrbl)

all:   clean class web
push:  clean class web-scp

clean: clean-class clean-web

#
# class
#

.PHONY: clean-class
class: $(CLASS) clean-class

$(CLASS): 
	raco pack -v ++setup class --collect --at-plt --replace $(CLASS) class

clean-class:
	find ../class* -name compiled -type d | xargs rm -fr
	rm -fr ../class-system-*.plt

#
# web
#

web: 2510H 2510H/dpc.pdf

2510H/dpc.pdf: dpc.scrbl	
	raco scribble --latex --dest 2510H/ dpc.scrbl
	cd 2510H; xelatex dpc.tex

2510H: $(CLASS) $(scrbls)
	ln -sf $(CLASS) $(CLASS_LATEST)
	env CLASS=$(CLASS) raco make 2510H.scrbl
	raco scribble --htmls \
		++extra fonts/kulminoituva-webfont.eot \
		++extra fonts/kulminoituva-webfont.svg \
		++extra fonts/kulminoituva-webfont.ttf \
		++extra fonts/kulminoituva-webfont.woff \
		++extra $(CLASS) \
		++extra $(CLASS_LATEST) \
		++extra misc/Invader01.pdf \
		++xref-in setup/xref load-collections-xref \
		--redirect http://www.ccs.neu.edu/course/cs2510h/ \
		--style 2510H.css \
		2510H.scrbl

web-students:
	env STUDENTS=1 make web

clean-web:
	find . -name compiled -type d | xargs rm -fr
	rm -rf 2510H/ class-system-*.plt

#
# web-scp
#

web-scp: clean-web class web-students
	scp -r 2510H/* $(CCS):$(CCS_PATH)
	make web-perms

web-perms:
	ssh $(CCS) chmod -Rf g+w $(CCS_PATH) || true

clean: clean-web
	-rm *log pict*png *~ *out scribble-common.js scribble.css scribble-style.css
