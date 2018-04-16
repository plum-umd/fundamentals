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
		zip Lab13 -r Lab13 ; \
		zip Lab14 -r Lab14 ; \
		zip Lab15 -r Lab15 ; \
		zip Lab16 -r Lab16 ; \
		zip Lab17 -r Lab17 ; \
		zip Lab18 -r Lab18 ; \
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
		../java/Lab13.zip \
		../java/Lab14.zip \
		../java/Lab15.zip \
		../java/Lab16.zip \
		../java/Lab17.zip \
		../java/Lab18.zip \
		../java/classcode1.zip \
		$(course)/

clean:
	rm -rf $(course)



