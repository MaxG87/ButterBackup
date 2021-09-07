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

.PHONY: all
all: check-format check-linters run-tests
.PHONY: apply-format
apply-format: | $(CACHEDIR)/apply-format
.PHONY: check-format
check-format: | $(CACHEDIR)/check-format
.PHONY: check-linters
check-linters: | $(CACHEDIR)/check-linters
.PHONY: run-tests
run-tests: run-docker-tests | $(CACHEDIR)/run-tests
.PHONY: run-docker-tests
run-docker-tests: | $(CACHEDIR)/run-debian-tests

$(CACHEDIR):
	mkdir -p $@


# EXECUTION OF TEST SUITE
$(CACHEDIR)/run-tests: | $(CACHEDIR)/run-undockered-tests $(CACHEDIR)/run-debian-tests
	touch $@

$(CACHEDIR)/run-undockered-tests: | $(CACHEDIR)
	poetry run pytest --cov src --cov-branch --cov-fail-under 70
	touch $@

$(CACHEDIR)/run-debian-tests: | $(CACHEDIR)/debian-test-image
	docker run --privileged -t $(DOCKER_TEST_TAG).debian
	touch $@

$(CACHEDIR)/debian-test-image: | $(CACHEDIR)
	DOCKER_BUILDKIT=1 docker build . -t $(DOCKER_TEST_TAG).debian -f docker-images/debian
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
$(CACHEDIR)/apply-format: | $(CACHEDIR)
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
