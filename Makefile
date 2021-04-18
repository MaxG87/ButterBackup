.PHONY: all
all: apply-format run-lint run-tests


.PHONY: apply-format
apply-format:
	poetry run black .
	poetry run isort .


.PHONY: run-lint
run-lint:
	poetry run flake8
	poetry run mypy .


.PHONY: run-tests
run-tests:
	poetry run pytest --cov src --cov-branch --cov-fail-under 47
