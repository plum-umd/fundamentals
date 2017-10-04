course = cmsc131A
course_group = cmsc131Afa17
course_term = fall2017
site_loc = /fs/www/class/$(course_term)
git_local = $(shell git rev-parse @)
git_remote = $(shell git rev-parse @{u})
git_base = $(shell git merge-base @ @{u})

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
	cp invaders.rkt cmsc131A/
	cp invader-shots-dvanhorn-abourg.rkt cmsc131A/
	cp shots.rkt cmsc131A/
	cp shots-draw-on.rkt cmsc131A/
	cp lists-{1,2}.rkt cmsc131A/
	cp feedback-assign4.pdf cmsc131A/
	cp m1-practice{,-soln}.pdf cmsc131A/
	cp sort.rkt cmsc131A/
	cp -r A{2,3} cmsc131A/

push: all
	@git fetch
ifeq ($(git_local),$(git_remote))
	find $(course) -type d -exec chmod 775 {} \;
	find $(course) -type f -exec chmod 664 {} \;
	chmod 771 $(course)/A{2,3}
	rsync -rvzp cmsc131A umd:$(site_loc)
	ssh umd "chown -R :$(course_group) $(site_loc)/$(course)/*"
else ifeq ($(git_local),$(git_base))
	@echo 'You need to `git pull` before modifying the website'
else ifeq ($(git_remote),$(git_base))
	@echo 'You need to `git push` before modifying the website'
endif

