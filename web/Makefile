CLASS=class-system-01-08.plt
CLASS_LATEST=class-system-latest.plt
CCS=login.ccs.neu.edu
CCS_PATH=/course/cs2510h/.www/

all:   clean class web
push:  clean class web-scp

clean: clean-class clean-web

#
# class
#

.PHONY: class
class: clean-class copy-doc
	raco pack -v ++setup class --collect --at-plt --replace $(CLASS) class

clean-class:
	find ../class* -name compiled -type d | xargs rm -fr
	rm -fr ../class-system-*.plt

copy-doc:
	# rm -fr ../class/5/scribblings/
	# rm -fr ../class/4/scribblings/
	# rm -fr ../class/3/scribblings/
	# rm -fr ../class/2/scribblings/
	# rm -fr ../class/1/scribblings/
	# rm -fr ../class/0/scribblings/
	# cp -r ./class/ ../class/5/scribblings/
	# cp -r ./class/ ../class/4/scribblings/
	# cp -r ./class/ ../class/3/scribblings/
	# cp -r ./class/ ../class/2/scribblings/
	# cp -r ./class/ ../class/1/scribblings/
	# cp -r ./class/ ../class/0/scribblings/


#
# web
#

web:
	ln -sf $(CLASS) $(CLASS_LATEST)
	env CLASS=$(CLASS) raco make 2510H.scrbl
	raco scribble --htmls \
		++extra fonts/kulminoituva-webfont.eot \
		++extra fonts/kulminoituva-webfont.svg \
		++extra fonts/kulminoituva-webfont.ttf \
		++extra fonts/kulminoituva-webfont.woff \
		++extra $(CLASS) \
		++extra $(CLASS_LATEST) \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		--redirect http://www.ccs.neu.edu/course/cs2510h/ \
		--style 2510H.css \
		2510H.scrbl

#++extra ../notes/notes.pdf \
#++extra ../notes/notes-2up.pdf \
#++extra labs/9/class3-html.plt \
#++extra labs/11/class4-blobs.plt \
#++extra labs/12/source-sink.rkt \
#++extra labs/13/state-diagram.png \

web-students:
	env STUDENTS=1 make web

clean-web:
	find . -name compiled -type d | xargs rm -fr
	rm -rf 2510H/ class-system-*.plt

#
# web-scp
#

web-scp: clean-web web-students	
	scp -r 2510H/* $(CCS):$(CCS_PATH)
	make web-perms

web-perms:
	ssh $(CCS) chmod -Rf g+w $(CCS_PATH) || true

clean: clean-web
	-rm *log pict*png *~ *out scribble-common.js scribble.css scribble-style.css