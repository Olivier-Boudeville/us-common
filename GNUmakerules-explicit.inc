# Here are gathered explicit (immediate, static) rules for US-Common.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: US_COMMON_TOP must be set.

.PHONY: default-us-common-rule create-us-common-checkout



# The first non-generic, therefore default, rule:
default-us-common-rule: all


# Create a _checkouts directory in which all Ceylan dependencies for the current
# layer are referenced through a symlink.
#
create-us-common-checkout:
	@echo "  Creating a checkout directory for US-Common"
	@$(MAKE) -s create-traces-checkout
	@cd _checkouts && ln -sf ../$(TRACES_TOP)
