#!/usr/bin/env python3

import bs4
import requests
import textwrap


def parse_float_line(line):
    v, u = line.split(':', 1)[1].strip().split(' ', 1)
    if u != 'GB':
        raise ValueError()

    a, b = v.split(',')
    return float(a) + (float(b) / 10.0)


def parse_timeframe_line(line):
    return line.split(' ', 2)[2]


def main():
    r = requests.get('https://center.vodafone.de/vfcenter/verbrauch.html')
    s = bs4.BeautifulSoup(r.text, "html.parser")
    c = s.find('div', class_='teaserHandsetContent')
    t = c.text.strip()
    used, included, timeframe = t.splitlines()
    used = parse_float_line(used)
    included = parse_float_line(included)
    timeframe = parse_timeframe_line(timeframe)

    # FIXME percent
    print(textwrap.dedent('''
    {} GB of {} GB ({}%) used from {}
    ''').strip().format(used, included, used/included * 100, timeframe))


if __name__ == '__main__':
    main()
