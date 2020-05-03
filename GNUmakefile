US_COMMON_TOP = .


.PHONY: help help-intro help-us-common register-version-in-header              \
		register-us-common  info info-local


MODULES_DIRS = src doc conf test


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true



# Default target:
help: help-intro help-us-common


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-us-common:
	@cd $(TRACES_TOP) && $(MAKE) -s help-traces


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 52 ; else \
	$(MAKE) register-us-common ; fi


register-us-common:
	@echo "-define( us_common_version, \"$(US_COMMON_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(US_COMMON_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


stats:
	@$(MAKE_CODE_STATS) $(US_COMMON_TOP)


info: info-local


info-local:
	@echo "REBAR3_EXEC = $(REBAR3_EXEC)"
	@echo "TRACES_TOP = $(TRACES_TOP)"
	@echo "WOOPER_TOP = $(WOOPER_TOP)"
	@echo "MYRIAD_TOP = $(MYRIAD_TOP)"


include $(US_COMMON_TOP)/GNUmakesettings.inc
