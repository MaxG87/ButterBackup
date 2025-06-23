# Configuration
ALL_FILES := $(sort $(shell find -type f -not -wholename "./.*" -not -name Makefile -not -wholename "*/__pycache__/*"))
ALL_DOCKER_FILES := $(shell ls dockerfiles)
SERVICE := $(notdir $(CURDIR))
SERVICE_LC := $(shell echo $(SERVICE) | tr '[[:upper:]]' '[[:lower:]]')
SERVICE_ID_FULL := $(shell sha256sum $(ALL_FILES) | sha256sum | cut -d' ' -f1)
SERVICE_ID_SHORT := $(shell echo $(SERVICE_ID_FULL) | head -c 6)

# Docker Tags
DOCKER_TEST_TAG = $(SERVICE_LC)-$(SERVICE_ID_SHORT).test

# Preparation of dependencies
CACHEBASE ?= ~/.cache/$(SERVICE).make-cache
CACHEDIR = $(CACHEBASE)/$(SERVICE_ID_FULL)

# run-arch-tests, run-python3.8-tests, ...
DOCKER_TESTS := $(addsuffix -tests, $(addprefix run-,$(ALL_DOCKER_FILES)))

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
run-docker-tests: | $(DOCKER_TESTS)
.PHONY: run-undockered-tests
run-undockered-tests: | $(CACHEDIR)/run-undockered-tests
.PHONY: get-service-id
get-service-id:
	@echo $(SERVICE_ID_FULL)

# This is a pseudo phony target. Unfortunately, Make does not support
# phony pattern rules.
run-%-tests: | $(CACHEDIR)/run-%-tests
	@touch $|

$(CACHEDIR):
	mkdir -p $@


$(CACHEDIR)/run-undockered-tests: | $(CACHEDIR)
	uv run pytest -n $$(nproc)
	touch $@

$(CACHEDIR)/run-%-tests: | $(CACHEDIR)/%-test-image
	platform=$(subst -test-image,,$(notdir $|)) ; \
	docker run --rm --privileged -v "$(CURDIR)/tests/:/project/tests" -t $(DOCKER_TEST_TAG).$$platform
	touch $@

$(CACHEDIR)/%-test-image: | $(CACHEDIR)
	platform=$(subst -test-image,,$(notdir $@)) ; \
	DOCKER_BUILDKIT=1 docker build . -t $(DOCKER_TEST_TAG).$$platform -f dockerfiles/$$platform
	touch $@


# STATIC ANALYSIS OF SOURCE CODE
$(CACHEDIR)/check-linters: | $(CACHEDIR)/check-ruff $(CACHEDIR)/check-mypy
	touch $@

$(CACHEDIR)/check-ruff: | $(CACHEDIR)/check-format
	uv run ruff check .
	touch $@

$(CACHEDIR)/check-mypy: | $(CACHEDIR)
	uv run mypy .
	touch $@


# CHECKING FORMAT AND REFORMATTING
$(CACHEDIR)/apply-format: $(ALL_FILES) | $(CACHEDIR)
	uv run ruff check --select I --fix
	uv run ruff format
	touch $@

$(CACHEDIR)/check-format: | $(CACHEDIR)/check-black $(CACHEDIR)/check-import-ordering
	touch $@

$(CACHEDIR)/check-black: | $(CACHEDIR)
	uv run ruff format --check
	touch $@

$(CACHEDIR)/check-import-ordering: | $(CACHEDIR)
	uv run ruff check --select I .
	touch $@
