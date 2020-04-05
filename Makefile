.PHONY: all clean

# non-versioned include
-include vars.mk

CARGO := $(shell which cargo)
CARGO_BUILD_ARGS :=

all: build

build: 
	$(CARGO) build $(CARGO_BUILD_ARGS)

test:
	$(CARGO) test $(TEST) $(CARGO_BUILD_ARGS) -- --nocapture

clean:
	rm -rf target/
	rm -f Cargo.lock
	cargo clean
