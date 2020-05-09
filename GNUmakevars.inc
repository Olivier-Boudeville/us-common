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
	PROJECT_NAME = US-common
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := us_common
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
US_COMMON_VERSION := 0.0.1


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(US_COMMON_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = $(PROJECT_NAME)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(US_COMMON_TOP)
endif



# For any quick, local, non-packaged update thereof:
WOOPER_SIBLING_BUILD = $(LAYER_TOP)/../Ceylan-WOOPER


# The OTP tree of the WOOPER application within a local OTP _build tree:
WOOPER_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/wooper

# Where WOOPER BEAMs are to lie when used as an OTP application:
WOOPER_LOCAL_EBIN := $(WOOPER_LOCAL_APP)/ebin/


US_COMMON_RELEASE_BASE := us-common-$(PROJECT_VERSION)

US_COMMON_RELEASE_ARCHIVE_ZIP := $(US_COMMON_RELEASE_BASE).zip
US_COMMON_RELEASE_ARCHIVE_BZ2 := $(US_COMMON_RELEASE_BASE).tar.bz2
US_COMMON_RELEASE_ARCHIVE_XZ  := $(US_COMMON_RELEASE_BASE).tar.xz


# Source section.
US_COMMON_SRC := $(US_COMMON_TOP)/src

# BEAM path section.
US_COMMON_BEAM := $(US_COMMON_TOP)

# Include path section (also for include_lib).
US_COMMON_INC = -I$(US_COMMON_SRC) -I$(US_COMMON_TOP)/..


# In an OTP/rebar3-style application layout, at compilation time, libraries
# making use of WOOPER will expect its includes to be located in:
#
# (Myriad already taken care of, at the WOOPER level)
#
WOOPER_OTP_INC := -I$(US_COMMON_TOP)/../wooper/include

INC += $(US_COMMON_INC) $(WOOPER_OTP_INC)


# When building a layer in an OTP context from its usual, GIT root (ex: with
# 'make rebar3-application'), the BEAMs of WOOPER are to be found in the OTP
# build tree - rather than in a supposedly fully-built usual root for WOOPER:
#
WOOPER_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(WOOPER_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout, at compilation time, modules
# compiled (directly or not) by the WOOPER parse transform will expect its
# module to be located in:
#
WOOPER_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../wooper/ebin


DOC_ROOT = $(US_COMMON_TOP)/../../../doc


ifndef VM_NAME

  VM_NAME := us_common_debug

endif


ifndef VM_TEST_NAME

  VM_TEST_NAME := us_common_test

endif


# We rely on Myriad and WOOPER as well:
BEAM_DIRS += $(US_COMMON_BEAM_DIRS)


# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
US_COMMON_BEAM_DIRS = $(US_COMMON_BEAM)/src


# For later reuse in the next layer of the software stack:
US_COMMON_PLT_FILE := $(US_COMMON_TOP)/us_common.plt


# This is the merged PLT of the level just below in the software stack.
# For us-common, it is the PLT of 'Traces':
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(TRACES_PLT_FILE)
endif


# We define the path to the Traces layer:
#
# (it may be a symbolic link pointing to the actual Traces package to be used,
# which itself may be either a 'Ceylan-Traces' directory or a 'traces' one)
#
ifneq ($(wildcard $(US_COMMON_TOP)/../traces),)

	TRACES_TOP = $(US_COMMON_TOP)/../traces

else

	# Default:
	TRACES_TOP = $(US_COMMON_TOP)/../Ceylan-Traces

endif