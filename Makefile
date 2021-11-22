.DEFAULT_GOAL := build

.PHONY: build
build: ## Compile the binary (Default)
	@stack build

.PHONY: help
help: ## Show this help
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[1m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.PHONY: lint
lint: ## Check the source code
	@hlint src

.PHONY: format
format: ## Check the source code
	@fourmolu -m inplace src/*

.PHONY: install
install: ## Clean generated files
	@stack install

.PHONY: clean
clean: ## Clean generated files
	@stack clean
