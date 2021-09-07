# Configuration
ALL_FILES := $(sort $(shell find -type f -not -wholename "./.*" -not -name Makefile -not -wholename "*/__pycache__/*"))
SERVICE := $(notdir $(CURDIR))
SERVICE_ID_FULL := $(shell sha256sum $(ALL_FILES) | sha256sum | cut -d' ' -f1)
SERVICE_ID_SHORT := $(shell echo $(SERVICE_ID_FULL) | head -c 6)

# Docker Tags
DOCKER_BASE_TAG = $(SERVICE)-$(SERVICE_ID_SHORT)
DOCKER_FORMAT_TAG = $(DOCKER_BASE_TAG).format
DOCKER_LINT_TAG = $(DOCKER_BASE_TAG).lint
DOCKER_TEST_TAG = $(DOCKER_BASE_TAG).test

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
run-tests: | $(CACHEDIR)/run-tests

$(CACHEDIR):
	mkdir -p $@

$(CACHEDIR)/run-tests: | $(CACHEDIR)/run-undockered-tests
	touch $@

$(CACHEDIR)/run-undockered-tests: | $(CACHEDIR)
	poetry run pytest --cov src --cov-branch --cov-fail-under 70
	touch $@

$(CACHEDIR)/check-linters: | $(CACHEDIR)/check-flake8 $(CACHEDIR)/check-mypy
	touch $@

$(CACHEDIR)/check-flake8: | $(CACHEDIR)/check-format
	poetry run flake8
	touch $@

$(CACHEDIR)/check-mypy: | $(CACHEDIR)
	poetry run mypy .
	touch $@

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
