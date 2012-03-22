CCS=login.ccs.neu.edu
CCS_PATH=/course/cs2510/.www/

scrbls := $(wildcard *.scrbl)

all:   clean web
push:  clean web-scp web-perms

clean: clean-web

#
# web
#

web: 2510

2510: $(scrbls)
	raco make 2510.scrbl
	raco scribble --htmls \
		++extra ../fonts/kulminoituva-webfont.eot \
		++extra ../fonts/kulminoituva-webfont.svg \
		++extra ../fonts/kulminoituva-webfont.ttf \
		++extra ../fonts/kulminoituva-webfont.woff \
		++extra CCISFlyer.jpg \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		--style 2510.css \
		2510.scrbl
	cp -r assignments/*.html 2510/
	cp -r assignments/*.java 2510/
	cp -r assignments/*.rtf 2510/
	cp -r assignments/*.zip 2510/
	cp -r labs/*.html 2510/
	cp -r labs/Lab10/ 2510/Lab10/
	cp -r labs/*.zip 2510/
	cp -r labs/*.java 2510/
	cp -r labs/*.jar 2510/
	cp -r labs/*.txt 2510/
	cp -r labs/*.png 2510/
	cp -r blog/*.java 2510/
	cp -r blog/*.html 2510/
	cp -r blog/*.pdf 2510/
	cp -r blog/*.zip 2510/
	cp -r exams/*.pdf 2510/

web-students:
	env STUDENTS=1 make 2510

clean-web:
	find . -name compiled -type d | xargs rm -fr
	rm -rf 2510/ 

#
# web-scp
#

web-scp: clean-web web-students	
	rsync -avz 2510/* $(CCS):$(CCS_PATH)
	make web-perms

web-perms:
	ssh $(CCS) chmod -Rf g+w $(CCS_PATH) || true
