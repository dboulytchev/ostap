SUBDIRS = src $(P5_SUBDIR) util

if CAMLP4
P4_SUBDIR = camlp4
endif

if CAMLP5
P5_SUBDIR = camlp5
endif

.PHONY: doc
doc:
	(cd doc; make doc)
