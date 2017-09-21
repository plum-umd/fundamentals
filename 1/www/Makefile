course = cmsc131A
course_group = cmsc131Afa17
course_term = fall2017
site_loc = /fs/www/class/$(course_term)

.PHONY : all push

all:
	rm -rf $(course)
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
	cp chip{0,1}.rkt cmsc131A/
	cp chip{0,1,2,3}.png cmsc131A/
	cp -r A{2,3} cmsc131A/

push:
	find $(course) -type d -exec chmod 775 {} \;
	find $(course) -type f -exec chmod 664 {} \;
	chmod 771 $(course)/A{2,3}
	rsync -rvzp cmsc131A umd:$(site_loc)
	ssh umd "chown -R :$(course_group) $(site_loc)/$(course)/*"
