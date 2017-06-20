course = cmsc198
course_group = cmsc198fa17
course_term = fall2017
site_loc = /fs/www/class/$(course_term)

.PHONY : all push

all:
	rm -rf $(course)
	scribble --htmls ++style extra.css $(course).scrbl

push:
	find $(course) -type d -exec chmod 775 {} \;
	find $(course) -type f -exec chmod 664 {} \;
	rsync -rvzp cmsc198 umd:$(site_loc)
	ssh umd "chown -R :$(course_group) $(site_loc)/$(course)/*"
