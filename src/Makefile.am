
# type in sources here
SOURCES = Pretty.ml PrioReorderer.ml BNF3.ml Types.ml Matcher.ml Combinators.ml Msg.ml Reason.ml Trap.ml
MLI = Pretty.mli PrioReorderer.mli BNF3.mli Types.mli Matcher.mli Combinators.mli Msg.mli Reason.mli Trap.mli

include $(top_srcdir)/m4/Makefile.am.vars

# CMO - all compiled objects
CMO := $(SORTED:.ml=.cmo)

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pr_o.cmo pa_log.cmo $(LOG) pa_gt.cmo -L `camlp5o -where`"
AM_BFLAGS += -w x -rectypes -for-pack Ostap
AM_OFLAGS += -w x -rectypes -for-pack Ostap

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
CMOPK =
CMXPK =
TARGET = $(MLI) $(CMI) $(CMO)

include $(top_srcdir)/m4/Makefile.am.targets

install:

uninstall:

# overwrite some targets
install-data-am: $(TARGET)
	@true
uninstall-am: $(TARGET)
	@true

include $(top_srcdir)/m4/Makefile.am.rules
