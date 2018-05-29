course = cmsc131A
course_group = cmsc131afa18
course_term = fall2018
site_loc = /fs/www/class/$(course_term)

.PHONY : all clean push test

all: clean cmsc131A

test: clean
	raco test .

cmsc131A:
	scribble --htmls ++style extra.css \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl
	cp code/* cmsc131A/
	cp past-exams/* cmsc131A/
	cp demo/* cmsc131A/

clean:
	rm -rf $(course)



