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
format: ## Auto-format the source code
	@fourmolu -m inplace src/*.hs

.PHONY: install
install: ## Install the compiled executable
	@stack install --ghc-options="-O2"

.PHONY: clean
clean: ## Clean generated files
	@stack clean

.PHONY: rebuild
rebuild: ## Clean and then build
	@stack clean
	@stack build

.PHONY: run
run: ## Run gitja on this repository into ./output/
	rm -rf output
	stack run -- -q

.PHONY: test
test: ## Run the "tests"
	bash ./test/test.sh

.PHONY: prod
prod: ## Build with optimise flags
	@stack build --ghc-options="-O2"

.PHONY: prof
prof: ## Build with profiling enabled
	@stack build --profile
	@rm -r output
	@stack exec --profile -- gitja -q +RTS -p
	@head -n 6 gitja.prof

.PHONY: all
all: format lint rebuild run test ## Lint, format, rebuild, run, test
