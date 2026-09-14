# NOTE: this assumes that eai-tool-library is checked out in ..

EMACS ?= emacs
EL_FILES := $(wildcard *.el)

.PHONY: all byte-compile clean

all: byte-compile

byte-compile: clean
	@$(EMACS) -Q -L . -L ../eai-tool-library --batch -f batch-byte-compile $(EL_FILES)

clean:
	@rm -f *.elc
