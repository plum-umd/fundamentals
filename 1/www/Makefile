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

push:
	find $(course) -type d -exec chmod 775 {} \;
	find $(course) -type f -exec chmod 664 {} \;
	rsync -rvzp cmsc131A umd:$(site_loc)
	ssh umd "chown -R :$(course_group) $(site_loc)/$(course)/*"
