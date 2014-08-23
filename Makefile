.PHONY: all check clean test_contactquery clean_contactquery

all: contactquery
check: test_contactquery
clean: clean_contactquery

contactquery: PACKAGES=libebook-contacts-1.2
contactquery: CFLAGS=$(shell pkg-config --cflags $(PACKAGES)) -Wall -std=c99 -Werror
contactquery: LDFLAGS=$(shell pkg-config --libs $(PACKAGES))

test_contactquery: contactquery
	py.test test_contactquery.py

clean_contactquery:
	$(RM) contactquery
