EMACS ?= emacs
BATCH := $(EMACS) -Q --batch -L .

.PHONY: test compile clean install lint

test:
	cask exec buttercup -L .

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile mojo-mode.el

clean:
	rm -rf .cask *.elc

install:
	cask install

# Byte-compile check that does not require Cask.
lint:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile mojo-mode.el
