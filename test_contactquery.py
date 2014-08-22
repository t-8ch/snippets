import os
import subprocess
import tempfile

import pytest
import py.path


here = py.path.local(__file__).dirpath()


FOO = ('BEGIN:VCARD\n'
       'VERSION:3.0\n'
       'EMAIL;TYPE=INTERNET:foo@example.com\n'
       'FN:Foo\n'
       'END:VCARD')

BAR = ('BEGIN:VCARD\n'
       'VERSION:3.0\n'
       'EMAIL;TYPE=INTERNET:bar@example.com\n'
       'FN:Bar\n'
       'END:VCARD')

BAZ = ('BEGIN:VCARD\n'
       'VERSION:3.0\n'
       'EMAIL;TYPE=INTERNET:baz@example.com\n'
       'EMAIL:baz@example.org\n'
       'FN:Baz\n'
       'END:VCARD')


class ContactQuery(object):
    def __init__(self):
        self.tmpdir = py.path.local(tempfile.mkdtemp())
        self.executable = str(here / 'contactquery')

    def add_contact(self, contents):
        handle, name = tempfile.mkstemp(dir=str(self.tmpdir), suffix='.vcf')
        with open(handle, 'w') as f:
            f.write(contents)

    def cleanup(self):
        return
        if self.tmpdir:
            self.tmpdir.remove()

    def _parse_line(self, line):
        return tuple(line.decode('utf-8').rstrip().split('\t', 2))

    def search(self, query):
        popen = subprocess.Popen([self.executable, str(self.tmpdir), query],
                                 stdout=subprocess.PIPE)
        lines = popen.stdout.readlines()
        assert lines[0] == b'Searching...\n'
        return [self._parse_line(line) for line in lines[1:]]

    def check_call(self, args):
        args = args.copy()
        args.insert(0, self.executable)
        return subprocess.check_call(args)

    def check_output(self, args):
        args = args.copy()
        args.insert(0, self.executable)
        return subprocess.check_output(args)

    def __repr__(self):
        return '<ContactQuery {}>'.format(self.tmpdir)


@pytest.fixture(scope='function')
def contactquery(request):
    cq = ContactQuery()
    request.addfinalizer(cq.cleanup)
    return cq


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
