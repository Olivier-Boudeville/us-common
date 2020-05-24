# Prerequisite: US_COMMON_TOP must be set.


# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(US_COMMON_TOP)
endif


# Project section.


# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := US-Common
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := us_common
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
US_COMMON_VERSION := 0.0.1


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(US_COMMON_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := $(PROJECT_NAME)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(US_COMMON_TOP)
endif


US_COMMON_RELEASE_BASE := us-common-$(PROJECT_VERSION)

US_COMMON_RELEASE_ARCHIVE_ZIP := $(US_COMMON_RELEASE_BASE).zip
US_COMMON_RELEASE_ARCHIVE_BZ2 := $(US_COMMON_RELEASE_BASE).tar.bz2
US_COMMON_RELEASE_ARCHIVE_XZ  := $(US_COMMON_RELEASE_BASE).tar.xz


# Source section.
US_COMMON_SRC := $(US_COMMON_TOP)/src

# BEAM path section.
US_COMMON_BEAM := $(US_COMMON_TOP)/src


# We rely on Myriad, WOOPER and Traces as well:
BEAM_DIRS += $(US_COMMON_BEAM_DIRS)


ifndef VM_NAME
	VM_NAME := us_common_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := us_common_test
endif


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
US_COMMON_BEAM_DIRS := $(US_COMMON_BEAM)


# For later reuse in the next layer of the software stack:
US_COMMON_PLT_FILE := $(US_COMMON_TOP)/us_common.plt


# This is the merged PLT of the level just below in the software stack.
# For us-common, it is the PLT of 'Traces':
#
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(TRACES_PLT_FILE)
endif


# We define the path to the Traces layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, dependencies may be found
# either in a local _checkouts child directory (if checkout directories are used
# and if the top-level package of interest is the current one), or in sibling
# directories (i.e. whose build root is at the same level as this one).
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
ifneq ($(wildcard $(US_COMMON_TOP)/../traces),)

	# By default, expect sibling directories:
	TRACES_TOP = $(US_COMMON_TOP)/../traces

else

	# Otherwise assume a checkout one:
	ifneq ($(wildcard $(US_COMMON_TOP)/_checkouts/traces),)

		TRACES_TOP = $(US_COMMON_TOP)/_checkouts/traces

   else

		FORCE_EVAL := $(error Unable to find the build root of Ceylan-Traces)

	endif

endif
