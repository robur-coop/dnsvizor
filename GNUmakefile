-include Makefile.config

UNIKERNELS = \
  dns-only \
  dns-and-dhcp

MIRAGE_FLAGS += --prng fortuna
MODE ?= "unix"

BUILD  = $(patsubst %, %-build, $(UNIKERNELS))
CI     = $(patsubst %, %-ci, $(UNIKERNELS))
CLEAN  = $(patsubst %, %-clean, $(UNIKERNELS))

build: $(BUILD)
ci: $(CI)
clean: $(CLEAN)

%-build:
	cd $* && \
	mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
	$(MAKE)

%-ci:
	cd $* && \
	mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
	make depend && \
	$(MAKE) && \
	find . -type f -perm +1 -exec sha256sum {} \;

%-clean:
	-cd $* && mirage clean
