##############################################################################
# Project configuration

PACKAGE    := redact
BINARY     := $(PACKAGE)
CABAL_FILE := $(PACKAGE).cabal
PROJECT    := $(PACKAGE)-haskell

##############################################################################
# Make configuration

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error GNU Make 4.0 or later required)
endif
.RECIPEPREFIX := >

SHELL := bash
.SHELLFLAGS := -o nounset -o errexit -o pipefail -c

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --warn-undefined-variables

.DEFAULT_GOAL := build

NIX_PATH_ARGS :=
ifneq ($(origin STACK_NIX_PATH), undefined)
  NIX_PATH_ARGS := "--nix-path=$(STACK_NIX_PATH)"
endif

RESOLVER_ARGS :=
ifneq ($(origin RESOLVER), undefined)
  RESOLVER_ARGS := "--resolver" "$(RESOLVER)"
endif

STACK_YAML_ARGS :=
ifneq ($(origin CONFIG), undefined)
  STACK_YAML_ARGS := "--stack-yaml" "$(CONFIG)"
endif

##############################################################################
# Functions

define all_files
  find . -not -path '*/\.*' -type f
endef

define die
  (echo "error: $(1)" ; false)
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

##############################################################################
# Rules

build: # build package *
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
.PHONY: build

clean: # clean package
> @stack clean
.PHONY: clean

clean-all: clean # clean package and remove artifacts
> @rm -rf .stack-work
> @rm -rf build
> @rm -f *.yaml.lock
.PHONY: clean-all

grep: # grep all non-hidden files for expression E
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help: # show this help
> @grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>   | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>   | column -t -s $$'\t'
> @echo
> @echo "* Use STACK_NIX_PATH to specify a Nix path."
> @echo "* Use RESOLVER to specify a resolver."
> @echo "* Use CONFIG to specify a Stack configuration file."
.PHONY: help

hlint: # run hlint on all Haskell source
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hsgrep: # grep all Haskell source for expression E
> $(eval E := "")
> @test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
> @$(call hs_files) | xargs grep -Hn '$(E)' || true
.PHONY: hsgrep

hsrecent: # show N most recently modified Haskell files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: hsrecent

hssloc: # count lines of Haskell source
> @$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'
.PHONY: hssloc

man: # build man page
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> $(eval DATE := $(shell date --rfc-3339=date))
> @mkdir -p build
> @pandoc -s -t man -o build/$(BINARY).1 \
>   --variable header="$(BINARY) Manual" \
>   --variable footer="$(PROJECT) $(VERSION) ($(DATE))" \
>   doc/$(BINARY).1.md
.PHONY: man

recent: # show N most recently modified files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

repl: # enter a REPL *
> @stack exec ghci $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
.PHONY: repl

source-git: # create source tarball of git TREE
> $(eval TREE := "HEAD")
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref $(TREE)))
> @test "${BRANCH}" = "main" || echo "WARNING: Not in main branch!" >&2
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ $(TREE) \
>   | xz \
>   > build/$(PROJECT)-$(VERSION).tar.xz
.PHONY: source-git

source-tar: # create source tarball using tar
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @sed -e 's,^/,./,' -e 's,/$$,,' .gitignore > build/.gitignore
> @tar \
>   --exclude-vcs \
>   --exclude-ignore-recursive=build/.gitignore \
>   --transform "s,^\.,$(PROJECT)-$(VERSION)," \
>   -Jcf build/$(PROJECT)-$(VERSION).tar.xz \
>   .
> @rm -f build/.gitignore
.PHONY: source-tar

test: # run tests, optionally for pattern P *
> $(eval P := "")
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @test -z "$(P)" \
>   && stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   || stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>       --test-arguments '--pattern $(P)'
.PHONY: test

test-all: # run tests for all versions
> $(eval CONFIG := $(shell \
    test -f stack-nix-8.2.2.yaml \
    && echo stack-nix-8.2.2.yaml \
    || echo stack-8.2.2.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval CONFIG := $(shell \
    test -f stack-nix-8.4.4.yaml \
    && echo stack-nix-8.4.4.yaml \
    || echo stack-8.4.4.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval CONFIG := $(shell \
    test -f stack-nix-8.6.5.yaml \
    && echo stack-nix-8.6.5.yaml \
    || echo stack-8.6.5.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval CONFIG := $(shell \
    test -f stack-nix.yaml \
    && echo stack-nix.yaml \
    || echo stack.yaml))
> @command -v hr >/dev/null 2>&1 && hr $(CONFIG) || true
> @make test CONFIG=$(CONFIG)
> $(eval STACK_NIX_PATH := $(shell \
    test -f stack-nix-nightly.path \
    && cat stack-nix-nightly.path \
    || true))
> @command -v hr >/dev/null 2>&1 && hr nightly || true
> @test -f stack-nix-nightly.path \
>   && make test RESOLVER=nightly STACK_NIX_PATH="$(STACK_NIX_PATH)" \
>   || make test RESOLVER=nightly
.PHONY: test-all

todo: # search for TODO items
> @find . -type f \
>   -not -path '*/\.*' \
>   -not -path './build/*' \
>   -not -path './project/*' \
>   -not -path ./Makefile \
>   | xargs grep -Hn TODO \
>   | grep -v '^Binary file ' \
>   || true
.PHONY: todo

version: # show current version
> @grep '^version:' $(CABAL_FILE) | sed 's/^version: */$(PROJECT) /'
.PHONY: version
