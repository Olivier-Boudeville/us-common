US_COMMON_TOP = ..


DOC_BASE_NAME := us-common-technical-manual-english

DOC_PDF_NAME := $(DOC_BASE_NAME).pdf

OVERALL_DOCUMENT_TARGET = $(DOC_BASE_NAME).rst

CURRENT_US_COMMON_HTML = $(DOC_BASE_NAME).html

HTML_TARGET = $(CURRENT_US_COMMON_HTML)

DOC_GENERATED_FILES = $(DOC_PDF_NAME) $(HTML_TARGET)

DOC_FILES = $(DOC_GENERATED_FILES)


CURRENT_US_COMMON_DOC := $(DOC_PDF_NAME)-$(US_COMMON_VERSION).pdf

PROJECT_CSS := pygments-default.css,us-common.css

PROJECT_ICON := us-common-icon.png

WEB_CONTENT = *.html *.css *.png *.pdf

# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(US_COMMON_WEB_MIRROR_BRANCH)")


OVERALL_DOCUMENT_SOURCE = $(DOC_BASE_NAME).rst


.PHONY: all doc full-doc clone-mirror                    \
		export-doc export-to-official export-to-mirror   \
		info-web clean clean-doc test uml-diagram-test



# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the manual of the 'us-common' layer."


doc:


full-doc: $(DOC_BASE_NAME).pdf $(DOC_BASE_NAME).html


html: clean-doc $(HTML_TARGET)


# Creates a separate repository for the GitHub pages branch:
# (please then remove all initial content of that branch)
#
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/us-common $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


export-to-official: $(DOC_GENERATED_FILES)
	@echo "   Exporting us-common documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/US-Common/


export-to-mirror: $(DOC_GENERATED_FILES)
	@echo "   Exporting us-common documentation to mirror website in $(US_COMMON_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(US_COMMON_WEB_MIRROR_BRANCH) && cd $(US_COMMON_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "us-common doc updated." && git push && git pull --ff-only


clean: clean-doc

clean-doc:
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc \
	$(CURRENT_US_COMMON_DOC) $(DOC_GENERATED_FILES)


info-web:
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "US_COMMON_WEB_MIRROR_BRANCH = $(US_COMMON_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

include $(US_COMMON_TOP)/GNUmakesettings.inc
