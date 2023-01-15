US_COMMON_TOP = .


.PHONY: help help-intro help-us-common register-version-in-header              \
		register-us-common info info-local info-conditionals info-deps


MODULES_DIRS = src doc conf test

# To override the 'all' default target with a parallel version:
BASE_MAKEFILE := true



# Default target:
help: help-intro help-us-common


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-us-common:
	@cd $(TRACES_TOP) && $(MAKE) -s help-traces


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ]; then \
	echo "Error, no version file defined." 1>&2; exit 52; else \
	$(MAKE) -s register-us-common; fi


register-us-common:
	@echo "-define( us_common_version, \"$(US_COMMON_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(US_COMMON_BEAM_DIRS); do echo $$(readlink -f $$d); done


stats:
	@$(MAKE_CODE_STATS) $(US_COMMON_TOP)


info: info-local


info-local:
	@echo "REBAR3_EXEC = $(REBAR3_EXEC)"


info-conditionals:
	@echo "US_COMMON_DEBUG_FLAGS = $(US_COMMON_DEBUG_FLAGS)"
	@echo "US_COMMON_CHECK_FLAGS = $(US_COMMON_CHECK_FLAGS)"


# Typically useful to know the software context for continuous integration:
info-context: info-platform info-versions info-source-layout


info-versions:
	@echo "MYRIAD_VERSION  = $(MYRIAD_VERSION)"
	@echo "WOOPER_VERSION  = $(WOOPER_VERSION)"
	@echo "TRACES_VERSION  = $(TRACES_VERSION)"
	@echo "SEAPLUS_VERSION = $(SEAPLUS_VERSION)"
	@echo "MOBILE_VERSION  = $(MOBILE_VERSION)"


info-deps:
	@echo "MYRIAD_TOP = $(MYRIAD_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"
	@echo "WOOPER_TOP = $(WOOPER_TOP)) (i.e. $$(realpath $(WOOPER_TOP)))"
	@echo "TRACES_TOP = $(TRACES_TOP)) (i.e. $$(realpath $(TRACES_TOP)))"
	@echo "SEAPLUS_TOP = $(SEAPLUS_TOP)) (i.e. $$(realpath $(SEAPLUS_TOP)))"
	@echo "MOBILE_TOP = $(MOBILE_TOP)) (i.e. $$(realpath $(MOBILE_TOP)))"


include $(US_COMMON_TOP)/GNUmakesettings.inc
