US_COMMON_TOP = ..


DOC_BASE_NAME := us-common-technical-manual-english


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_US_COMMON_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_US_COMMON_HTML)


CURRENT_US_COMMON_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_US_COMMON_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



PROJECT_CSS := pygments-default.css,us-common.css

PROJECT_ICON := us-common-icon.png


# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(US_COMMON_WEB_MIRROR_BRANCH)")



.PHONY: all doc html pdf create-links clone-mirror                     \
		export-doc export-doc-to-official export-doc-to-mirror         \
		info info-web



# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the documentation of the 'US-Common' layer."


doc: html pdf api-doc


html: local-html

pdf: local-pdf


# This link does not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_US_COMMON_HTML) index.html



# Creates a separate repository for the GitHub pages branch:
clone-mirror:
	@echo "  Creating a separate repository for website in $(GITHUB_PAGES_BASE)"
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/us-common $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages
	@echo "Please remove all initial content in that selected 'gh-pages' branch."


export-doc: clean-doc doc export-doc-to-official export-doc-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/WOOPER/
#
export-doc-to-official: clean-doc create-links doc
	@echo "   Exporting US-Common documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) -r $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/US-Common/


export-doc-to-mirror: clean-doc create-links doc
	@echo "   Exporting US-Common documentation to mirror website in $(US_COMMON_WEB_MIRROR_BRANCH)"
	@/bin/cp -rf $(WEB_CONTENT) $(US_COMMON_WEB_MIRROR_BRANCH) && cd $(US_COMMON_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "US-Common doc updated." && git push && git pull --ff-only



info: info-doc info-web


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "US_COMMON_WEB_MIRROR_BRANCH = $(US_COMMON_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For WOOPER_TOP and MYRIAD_TOP:
include $(US_COMMON_TOP)/GNUmakesettings.inc
