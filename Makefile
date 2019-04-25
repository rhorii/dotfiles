UNAME := $(shell uname)

ifeq ($(UNAME),Darwin)
	include macos.mk
endif

ifeq ($(UNAME),Linux)
	include archlinux.mk
endif
