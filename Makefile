.PHONY: all check clean test_contactquery clean_contactquery

# FIXME version detection logic
# boundary completely arbitrary
ifeq ($(shell pkg-config 'libebook-1.2 > 3.10' --print-errors 2>&1),)
EBOOK_CONTACTS=libebook-contacts-1.2
MODERN_PLATFORM=1
else
EBOOK_CONTACTS=libebook-1.2
MODERN_PLATFORM=0
endif

all: contactquery
check: test_contactquery
clean: clean_contactquery

contactquery: PACKAGES=$(EBOOK_CONTACTS)
contactquery: CFLAGS=$(shell pkg-config --cflags $(PACKAGES)) -Wall -std=c99 -Werror -DMODERN_PLATFORM=$(MODERN_PLATFORM)
contactquery: LDLIBS=$(shell pkg-config --libs $(PACKAGES))

test_contactquery: contactquery
	py.test test_contactquery.py

clean_contactquery:
	$(RM) contactquery
