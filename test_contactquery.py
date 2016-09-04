from __future__ import unicode_literals

import copy
import io
import os
import subprocess
import tempfile

import pytest
import py.path


here = py.path.local(__file__).dirpath()


FOO = ('BEGIN:VCARD\n'
       'VERSION:3.0\n'
       'EMAIL:foo@example.com\n'
       'FN:Foo\n'
       'END:VCARD')

BAR = ('BEGIN:VCARD\n'
       'VERSION:3.0\n'
       'EMAIL:bar@example.com\n'
       'FN:Bar\n'
       'END:VCARD')

BAZ = ('BEGIN:VCARD\n'
       'VERSION:3.0\n'
       'EMAIL:baz@example.com\n'
       'EMAIL:baz@example.org\n'
       'FN:Baz\n'
       'END:VCARD')

QUUX = ('BEGIN:VCARD\n'
        'VERSION:3.0\n'
        'EMAIL;PREF=15:quux+15@example.com\n'
        'EMAIL:quux+100@example.com\n'
        'EMAIL;PREF=1:quux+1@example.com\n'
        'FN:Quux\n'
        'END:VCARD')


class ContactQuery(object):
    def __init__(self, tmpdir):
        self.tmpdir = tmpdir
        self.executable = str(here / 'contactquery')

    def add_contact(self, contents):
        handle, name = tempfile.mkstemp(dir=str(self.tmpdir), suffix='.vcf')
        with io.open(handle, 'w') as f:
            f.write(contents)

    def _parse_line(self, line):
        return tuple(line.decode('utf-8').split('\t', 2))

    def search(self, query):
        output = self.check_output([str(self.tmpdir), query])
        lines = output.splitlines()
        assert lines[0] == b'Searching...'
        return [self._parse_line(line) for line in lines[1:]]

    def check_call(self, args):
        args = copy.copy(args)
        args.insert(0, self.executable)
        return subprocess.check_call(args)

    def check_output(self, args):
        args = copy.copy(args)
        args.insert(0, self.executable)
        return subprocess.check_output(args)

    def __repr__(self):
        return '<ContactQuery {}>'.format(self.tmpdir)


@pytest.fixture(scope='function')
def contactquery(request, tmpdir):
    return ContactQuery(tmpdir)


def test_without_dir(contactquery):
    with pytest.raises(subprocess.CalledProcessError):
        contactquery.check_call([])


def test_invalid_dir(contactquery):
    with pytest.raises(subprocess.CalledProcessError):
        contactquery.check_call([os.path.devnull, ''])


def test_status_message(contactquery):
    tmpdir = str(contactquery.tmpdir)
    output = contactquery.check_output([tmpdir, ''])
    assert output == b'Searching...\n'


def test_empty_dir(contactquery):
    assert contactquery.search('') == []


def test_basic(contactquery):
    contactquery.add_contact(FOO)
    assert contactquery.search('') == [('foo@example.com', 'Foo')]

    contactquery.add_contact(BAR)
    assert sorted(contactquery.search('')) == [
        ('bar@example.com', 'Bar'),
        ('foo@example.com', 'Foo'),
    ]

    assert sorted(contactquery.search('bar')) == [
        ('bar@example.com', 'Bar'),
    ]


def test_broken_files(contactquery):
    contactquery.add_contact('''some crap''')

    assert contactquery.search('') == []

    contactquery.add_contact(FOO)
    assert contactquery.search('') == [('foo@example.com', 'Foo')]


def test_multiple_email_addresses(contactquery):
    contactquery.add_contact(BAZ)
    assert sorted(contactquery.search('')) == [
        ('baz@example.com', 'Baz'),
        ('baz@example.org', 'Baz'),
    ]


def test_mail_preference_parameter(contactquery):
    contactquery.add_contact(QUUX)
    assert contactquery.search('') == [
        ('quux+1@example.com', 'Quux'),
        ('quux+15@example.com', 'Quux'),
        ('quux+100@example.com', 'Quux'),
    ]


WITH_TYPES = ('BEGIN:VCARD\n'
              'VERSION:3.0\n'
              'EMAIL;TYPE=SOMETYPE:bar@example.com\n'
              'FN:Bar\n'
              'END:VCARD')


def test_mail_type_parameter(contactquery):
    contactquery.add_contact(WITH_TYPES)
    assert contactquery.search('') == [
        ('bar@example.com', 'Bar', 'sometype'),
    ]


def test_mail_type_internet_parameter(contactquery):
    contactquery.add_contact(
        'BEGIN:VCARD\n'
        'VERSION:3.0\n'
        'EMAIL;TYPE=INTERNET:bar@example.com\n'
        'FN:Bar\n'
        'END:VCARD'
    )
    assert contactquery.search('') == [
        ('bar@example.com', 'Bar'),
    ]

def test_search_by_email_component(contactquery):
    contactquery.add_contact(
        'BEGIN:VCARD\n'
        'VERSION:3.0\n'
        'EMAIL;TYPE=INTERNET:bar@example.com\n'
        'FN:Foo\n'
        'END:VCARD'
    )
    contactquery.add_contact(
        'BEGIN:VCARD\n'
        'VERSION:3.0\n'
        'EMAIL;TYPE=INTERNET:baz@example.com\n'
        'FN:Baz\n'
        'END:VCARD'
    )
    assert contactquery.search('foo') == [
        ('bar@example.com', 'Foo'),
    ]
    assert contactquery.search('bar') == [
        ('bar@example.com', 'Foo'),
    ]
