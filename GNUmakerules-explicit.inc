# Here are gathered explicit (immediate, static) rules for US-common.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: US_COMMON_TOP must be set.

.PHONY: default-us-common-rule


# The first non-generic, therefore default, rule:
default-us-common-rule: all
