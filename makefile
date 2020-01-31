default: .tested

.tested: web/*.scm test/*.scm
	hdt
	touch $@

clean:
	rm -rf .tested
	
GUILE_CONFIG ?= guile-config
SITE_DIR ?= $(shell $(GUILE_CONFIG) info sitedir)

install:
	install -D -t $(SITE_DIR)/web web/driver.scm
	install -D -t $(SITE_DIR)/web/driver web/driver/*

uninstall:
	rm -rf $(SITE_DIR)/web/driver
	rm -rf $(SITE_DIR)/web/driver.scm
