course = cmsc131A
course_group = cmsc131afa17
course_term = fall2017
site_loc = /fs/www/class/$(course_term)

.PHONY : all clean push skeletons

all: clean assign6.zip assign7.zip cmsc131A

cmsc131A:
	scribble --htmls ++style extra.css \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl
	cp eclipse.rkt cmsc131A/
	cp distance.rkt cmsc131A/
	cp fancy-eclipse.rkt cmsc131A/
	cp days.rkt cmsc131A/
	cp traffic.rkt cmsc131A/
	cp render-string.rkt cmsc131A/
	cp chip0.rkt cmsc131A/
	cp chip1.rkt cmsc131A/
	cp chip0.png cmsc131A/
	cp chip1.png cmsc131A/
	cp chip2.png cmsc131A/
	cp chip3.png cmsc131A/
	cp invaders.rkt cmsc131A/
	cp invader-shots-dvanhorn-abourg.rkt cmsc131A/
	cp invaders-shoot-dvanhorn-abourg.rkt cmsc131A/
	cp shots.rkt cmsc131A/
	cp shots-draw-on.rkt cmsc131A/
	cp lists-1.rkt cmsc131A/
	cp lists-2.rkt cmsc131A/
	cp feedback-assign4.pdf cmsc131A/
	cp m1-practice.pdf cmsc131A/
	cp m1-practice-soln.pdf cmsc131A/
	cp sort.rkt cmsc131A/
	cp snake-complete.rkt cmsc131A/
	cp snake-in-progress.rkt cmsc131A/
	cp f-all-abstract.rkt cmsc131A/
	cp addn-abstract.rkt cmsc131A/
	cp even-odd-abstract.rkt cmsc131A/
	cp prod-sum-abstract.rkt cmsc131A/
	cp using-abstractions.rkt cmsc131A/
	cp adventure.rkt cmsc131A/
	cp adventure2.rkt cmsc131A/
	cp m2-practice.pdf cmsc131A/
	cp m2-practice-soln.pdf cmsc131A/
	cp multi-inputs.rkt cmsc131A/
	cp merge.rkt cmsc131A/
	cp bundle.rkt cmsc131A/
	cp qsort.rkt cmsc131A/
	cp drill-soln.rkt cmsc131A/
	cp graph.rkt cmsc131A/
	cp graph2.rkt cmsc131A/
	cp typed.rkt cmsc131A/
	cp typed2.rkt cmsc131A/
	cp ml.rkt cmsc131A/

clean:
	rm -rf $(course)



