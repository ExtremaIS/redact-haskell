DESTDIR ?=
prefix  ?= /usr

bindir      := $(DESTDIR)$(prefix)/bin
datarootdir := $(DESTDIR)$(prefix)/share
docdir      := $(datarootdir)/doc/redact-haskell
man1dir     := $(datarootdir)/man/man1

SHELL := bash

build:
	@stack build
.PHONY: build

install:
	$(eval LIROOT := $(shell stack path --local-install-root))
	@mkdir -p "$(bindir)"
	@install -m 0755 "$(LIROOT)/bin/redact" "$(bindir)/redact"
	@mkdir -p "$(man1dir)"
	@install -m 0644 doc/redact.1 "$(man1dir)"
	@gzip "$(man1dir)/redact.1"
	@mkdir -p "$(docdir)"
	@install -m 0644 README.md "$(docdir)"
	@gzip "$(docdir)/README.md"
	@install -m 0644 -T CHANGELOG.md "$(docdir)/changelog"
	@gzip "$(docdir)/changelog"
	@install -m 0644 LICENSE "$(docdir)"
	@gzip "$(docdir)/LICENSE"
.PHONY: install

test:
	@stack test
.PHONY: test
