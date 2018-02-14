course = cmsc132A
course_group = cmsc131afa17
course_term = spring2018

.PHONY : all clean test

all: clean cmsc132A

test: clean
	raco test .

cmsc132A:
	scribble --htmls ++style extra.css \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl
	cp code/* $(course)/
	cd ../java/ ; zip Assign3 -r Assign3 ; cd ../www
	cp ../java/Assign3.zip $(course)/

clean:
	rm -rf $(course)



