# description
DESCRIPTION = "Ostap --- parser combinators library and syntax extension"

# type in sources here
SOURCES = $(top_srcdir)/src/Pretty.ml $(top_srcdir)/src/PrioReorderer.ml $(top_srcdir)/src/BNF3.ml $(top_srcdir)/src/Types.ml $(top_srcdir)/src/Msg.ml $(top_srcdir)/src/Reason.ml $(top_srcdir)/src/Matcher.ml $(top_srcdir)/src/Combinators.ml $(top_srcdir)/src/Trap.ml Util.ml

include $(top_srcdir)/m4/Makefile.am.vars

MLI := $(SOURCES:.ml=.mli)

# overwrite some variables
OFLAGS += -w x
CFLAGS += -w x

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(top_srcdir)/camlp5/pa_ostap.cmo $(PARSER_INCLUDES) pa_log.cmo $(LOG) pr_o.cmo pa_gt.cmo -L `camlp5o -where`"
AM_OFLAGS += -for-pack Ostap -rectypes
AM_BFLAGS += -rectypes

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I ../src

# packed CMO, add to TARGET
CMOPK := $(addsuffix .cmo, $(PACKAGE))
# packed CMX, add to TARGET
CMXPK := $(addsuffix .cmx, $(PACKAGE))
# interface for packed object
CMIPK := $(addsuffix .cmi, $(PACKAGE))
# object for linking
OPK := $(addsuffix .o, $(PACKAGE))

TARGET = $(CMOPK) $(CMXPK) $(CMIPK) $(OPK) $(MLI) META

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets

# META description for package
META: $(top_srcdir)/VERSION
	@echo "Generating META description"; \
	revision=`grep "\\$$Revision:" $< | sed -e 's,^\\$$,,' -e 's, \\$$$$,,'`; \
	date=`LC_TIME=C date`; \
	echo "version = \"$$revision\"" > $@; \
	echo "description = \"$(DESCRIPTION)\"" >> $@; \
	echo "requires = \"\"" >> $@

install-data-am: $(TARGET)
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" != ""; then \
	    $(mkinstalldirs) $(SITELIBDIR); \
	    $(OCAMLFIND) install -destdir $(SITELIBDIR) $(PACKAGE) $(TARGET); \
	else \
	    $(OCAMLFIND) install $(PACKAGE) $(TARGET); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    installdir=$(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    installdir=$(SITELIBDIR)/$(PACKAGE); \
	fi; \
	$(mkinstalldirs) $$installdir; \
	for i in $(TARGET); do \
	    $(INSTALL_DATA) $$i $$installdir/$$i; \
	done
endif

#install-exec-am: $(BYTE) $(NATIVE)
#	$(mkinstalldirs) ${bindir}
#	$(INSTALL_PROGRAM) $(BYTE) ${bindir}/$(BYTE)
#	$(INSTALL_PROGRAM) $(NATIVE) ${bindir}/$(PACKAGE)$(EXEEXT)

uninstall-am:
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" = ""; then \
	    $(OCAMLFIND) remove $(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    rm -fR $(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
endif
#	rm -f ${bindir}/$(BYTE) ${bindir}/$(PACKAGE)$(EXEEXT)

include $(top_srcdir)/m4/Makefile.am.rules
