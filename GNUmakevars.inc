# Prerequisite: US_COMMON_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid US-Common execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - registration: regarding naming registering
#  - scheduling: regarding task scheduling
#  - actions: about the actions that may be externally triggered on server
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#US_COMMON_DEBUG_FLAGS += -Dus_common_debug_registration
	#US_COMMON_DEBUG_FLAGS += -Dus_common_debug_scheduling
	US_COMMON_DEBUG_FLAGS += -Dus_common_debug_actions
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: US_COMMON_CHECK_FLAGS += -Dus_common_check_scheduling
    # Does not exist: US_COMMON_CHECK_FLAGS += -Dus_common_check_actions
endif

ERLANG_COMPILER_OPT_BASE += -Dus_common_version=\"$(US_COMMON_VERSION)\" \
							$(US_COMMON_DEBUG_FLAGS)                     \
							$(US_COMMON_CHECK_FLAGS)



# Useful to target for example the root of the current layer (e.g. to locate the
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


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := us_common
endif


# PROJECT_NAME_FOR_OTP should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_OTP
	PROJECT_NAME_FOR_OTP := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar, hex
# and the code itself, see class_USServer:get_us_common_version{_string,}/0):
#
US_COMMON_VERSION := 0.1.8


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



# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
US_COMMON_BEAM_DIRS := $(US_COMMON_BEAM) $(US_COMMON_TOP)/..


# We rely on Myriad, WOOPER and Traces as well:
BEAM_DIRS += $(US_COMMON_BEAM_DIRS)


ifndef VM_NAME
	VM_NAME := us_common_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := us_common_test
endif


# To enforce specific settings such as registration scopes:
#
# (not to be mixed up with our priv/conf/us.config)
#
INTERNAL_OPTIONS += --config $(US_COMMON_TOP)/conf/us-common.config


# No $(LAYER_TOP) in includes, it would be misleading as edoc is run from the
# clone root, not from doc:
#
ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {macros,[{$(PROJECT_EDOC_VERSION_KEY),"$(PROJECT_VERSION)"}]}, {includes,["..", "$(TRACES_TOP)/..", "$(WOOPER_TOP)/..", "$(MYRIAD_TOP)/.."]}, {overview, "doc/overview.edoc"}]'
endif


# For later reuse in the next layer of the software stack:
US_COMMON_PLT_FILE := $(US_COMMON_TOP)/US-Common.plt


# This is the merged PLT of the level just below in the software stack.
# For us-common, it is the PLT of 'Traces':
#
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(TRACES_PLT_FILE)
endif


# We define the path to the Traces layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (e.g. myriad) as opposed to
# project/repository names (e.g. Ceylan-Myriad):
#

# First priority was the expected environment variable; now disabled as this led
# to unfortunate user-based overriding due to .bashrc-related elements (prefer
# symbolic links whenever needed):

#ifneq ($(CEYLAN_TRACES),)
#	TRACES_TOP = $(CEYLAN_TRACES)
#else
	ifneq ($(wildcard $(US_COMMON_TOP)/_checkouts/traces/.),)
		TRACES_TOP = $(US_COMMON_TOP)/_checkouts/traces
	else
		# Second, a sibling one is searched for:
		ifneq ($(wildcard $(US_COMMON_TOP)/../traces/.),)
			TRACES_TOP = $(US_COMMON_TOP)/../traces
		else
			# Otherwise most probably in a 'rebar3 compile' context:
			ifneq ($(wildcard $(US_COMMON_TOP)/_build/default/lib/traces/.),)
				TRACES_TOP = $(US_COMMON_TOP)/_build/default/lib/traces
			else
				FORCE_EVAL := $(error Unable to find the build root of Ceylan-Traces from '$(realpath .)')
			endif
		endif
	endif
#endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(TRACES_TOP)
endif
