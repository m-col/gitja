.DEFAULT_GOAL := build

.PHONY: build
build: ## Compile the binary (Default)
	@stack build --keep-going

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

.PHONY: all
all: format lint rebuild run test ## Lint, format, rebuild, run, test

.PHONY: prod
prod: ## Build with optimise flags
	@stack build --ghc-options="-O2"

.PHONY: dot
dot: ## Visualise dependencies
	stack dot --external --depth 1 --prune base,text,bytestring,transformers \
		|  twopi -Goverlap=false -Tpng -o deps.png

PROF ?= fprof-auto
# other options: fno-prof-auto fprof-auto-top

.PHONY: prof
prof: _prof_build ## Build with profiling enabled
	stack exec --profile -- gitja -q +RTS -p -po${PROF}
	ghc-prof-flamegraph ${PROF}.prof -o ${PROF}.svg

.PHONY: prof_speedscope
prof_speedscope: _prof_build ## Build with profiling enabled (JSON output with speedscope)
	stack exec --profile -- gitja -q +RTS -p -po${PROF} -pj
	speedscope ${PROF}.prof

_prof_build:
	stack build --profile --ghc-options="-${PROF}"
	rm -fr output
