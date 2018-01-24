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

clean:
	rm -rf $(course)



