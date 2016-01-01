STACK_BASE=linux-x86_64-gmp4
STACK_URL=https://www.stackage.org/stack/$(STACK_BASE)

export PATH := $(wildcard stack-*-$(STACK_BASE)):$(PATH)

vype: stack
	stack setup
	stack build
	cp .stack-work/install/*/*/*/bin/VYPe15 vype

stack: $(STACK_BASE).tar.gz
	tar xzf $<

$(STACK_BASE).tar.gz:
	wget $(STACK_URL) -O $@

pack:
	zip xkidon00.zip -r src Makefile dokumentace.pdf rozdeleni stack.yaml LICENSE VYPe15.cabal

.PHONY:
	stack
