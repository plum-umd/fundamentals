CLASS=class-system-01-04.plt
CLASS_LATEST=class-system-latest.plt
CCS=login.ccs.neu.edu
CCS_PATH=/course/cs2510h/.www/
RACO=raco

scrbls := $(wildcard *.scrbl) $(wildcard assignments/*.scrbl) $(wildcard labs/*.scrbl)

all:   clean class web
push:  clean class web-scp

clean: clean-class clean-web

#
# class
#

.PHONY: clean-class
class: $(CLASS) clean-class

$(CLASS):
	$(RACO) pack -v ++setup class --collect --at-plt --replace $(CLASS) class

clean-class:
	find ../class* -name compiled -type d | xargs rm -fr
	rm -fr ../class-system-*.plt

#
# web
#

web: 2510H 2510H/dpc.pdf

2510H/dpc.pdf: dpc.scrbl
	$(RACO) scribble --latex --dest 2510H/ dpc.scrbl
	cd 2510H; xelatex dpc.tex

2510H: $(CLASS) $(scrbls)
	ln -sf $(CLASS) $(CLASS_LATEST)
	env CLASS=$(CLASS) $(RACO) make 2510H.scrbl
	$(RACO) scribble --htmls \
		++extra fonts/kulminoituva-webfont.eot \
		++extra fonts/kulminoituva-webfont.svg \
		++extra fonts/kulminoituva-webfont.ttf \
		++extra fonts/kulminoituva-webfont.woff \
		++extra $(CLASS) \
		++extra $(CLASS_LATEST) \
		++extra misc/Invader01.pdf \
		++extra misc/exam1-s11.pdf \
		++extra misc/exam1-s12.pdf \
		++extra misc/exam2-s11.pdf \
		++extra misc/exam2-s12.pdf \
		++extra misc/old-exam2.pdf \
		++extra misc/marathon.rkt \
		++extra misc/gender.rkt \
		++extra misc/soln1.pdf \
		++extra misc/First.java \
		++extra misc/Trees.java \
		++extra misc/Book.java \
		++extra misc/Traverse.java \
		++extra misc/Today1.java \
		++extra misc/Today2.java \
		++extra misc/Today3.java \
		++extra misc/class-impl1.rkt \
		++extra misc/class-impl2.rkt \
		++extra misc/Life.java \
		++extra assignments/Employees.pdf \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
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
	rsync -avz 2510H/* $(CCS):$(CCS_PATH)
	make web-perms

web-perms:
	ssh $(CCS) chmod -Rf g+w $(CCS_PATH) || true

clean: clean-web
	-rm *log pict*png *~ *out scribble-common.js scribble.css scribble-style.css
