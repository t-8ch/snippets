.PHONY: all

all: contactquery

contactquery: PACKAGES=libebook-contacts-1.2
contactquery: CFLAGS=$(shell pkg-config --cflags $(PACKAGES)) -Wall -std=c99 -Werror
contactquery: LDFLAGS=$(shell pkg-config --libs $(PACKAGES))
