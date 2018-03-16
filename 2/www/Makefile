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
	cp exams/* $(course)/
	cd ../java/ ; \
		zip Assign3 -r Assign3 ; \
		zip Assign4 -r Assign4 ; \
		zip Assign5 -r Assign5 ; \
		zip Lab6 -r Lab6 ; \
		zip Lab7 -r Lab7 ; \
		zip Lab8 -r Lab8 ; \
		zip Lab9 -r Lab9 ; \
		zip Lab10 -r Lab10 ; \
		zip Lab11 -r Lab11 ; \
		zip Lab12 -r Lab12 ; \
		zip classcode1 -r classcode1
	cp ../java/Assign3.zip \
		../java/Assign4.zip \
		../java/Assign5.zip \
		../java/Lab6.zip \
		../java/Lab7.zip \
		../java/Lab8.zip \
		../java/Lab9.zip \
		../java/Lab10.zip \
		../java/Lab11.zip \
		../java/Lab12.zip \
		../java/classcode1.zip \
		$(course)/

clean:
	rm -rf $(course)



