# Configuration
ALL_FILES := $(sort $(shell find -type f -not -wholename "./.*" -not -name Makefile -not -wholename "*/__pycache__/*"))
SERVICE := $(notdir $(CURDIR))
SERVICE_LC := $(shell echo $(SERVICE) | tr '[[:upper:]]' '[[:lower:]]')
SERVICE_ID_FULL := $(shell sha256sum $(ALL_FILES) | sha256sum | cut -d' ' -f1)
SERVICE_ID_SHORT := $(shell echo $(SERVICE_ID_FULL) | head -c 6)

# Docker Tags
DOCKER_TEST_TAG = $(SERVICE_LC)-$(SERVICE_ID_SHORT).test

# Preparation of dependencies
CACHEBASE ?= ~/.cache/$(SERVICE).make-cache/
CACHEDIR = $(CACHEBASE)/$(SERVICE_ID_FULL)

.SECONDARY:  # Do not remove intermediate files. We need them for caching!

.PHONY: all
all: check-format check-linters run-tests
.PHONY: apply-format
apply-format: $(CACHEDIR)/apply-format
.PHONY: check-format
check-format: | $(CACHEDIR)/check-format
.PHONY: check-linters
check-linters: | $(CACHEDIR)/check-linters
.PHONY: run-tests
run-tests: run-docker-tests | run-undockered-tests
.PHONY: run-docker-tests
run-docker-tests: | $(addprefix $(CACHEDIR)/,run-arch-tests run-debian-tests run-python3.8-tests run-python3.9-tests run-python3.10-tests)
.PHONY: run-undockered-tests
run-undockered-tests: | $(CACHEDIR)/run-undockered-tests
.PHONY: get-service-id
get-service-id:
	@echo $(SERVICE_ID_FULL)

$(CACHEDIR):
	mkdir -p $@


$(CACHEDIR)/run-undockered-tests: | $(CACHEDIR)
	poetry run pytest
	touch $@

$(CACHEDIR)/run-%-tests: | $(CACHEDIR)/%-test-image
	platform=$(subst -test-image,,$(notdir $|)) ; \
	docker run --privileged -v "$(CURDIR)/tests/:/project/tests" -t $(DOCKER_TEST_TAG).$$platform
	touch $@

$(CACHEDIR)/%-test-image: | $(CACHEDIR)
	platform=$(subst -test-image,,$(notdir $@)) ; \
	DOCKER_BUILDKIT=1 docker build . -t $(DOCKER_TEST_TAG).$$platform -f docker-images/$$platform
	touch $@


# STATIC ANALYSIS OF SOURCE CODE
$(CACHEDIR)/check-linters: | $(CACHEDIR)/check-flake8 $(CACHEDIR)/check-mypy
	touch $@

$(CACHEDIR)/check-flake8: | $(CACHEDIR)/check-format
	poetry run flake8
	touch $@

$(CACHEDIR)/check-mypy: | $(CACHEDIR)
	poetry run mypy .
	touch $@


# CHECKING FORMAT AND REFORMATTING
$(CACHEDIR)/apply-format: $(ALL_FILES) | $(CACHEDIR)
	poetry run isort .
	poetry run black .
	touch $@

$(CACHEDIR)/check-format: | $(CACHEDIR)/check-black $(CACHEDIR)/check-isort
	touch $@

$(CACHEDIR)/check-black: | $(CACHEDIR)
	poetry run black --check .
	touch $@

$(CACHEDIR)/check-isort: | $(CACHEDIR)
	poetry run isort --check .
	touch $@
