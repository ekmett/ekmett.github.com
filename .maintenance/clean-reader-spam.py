"""Remove reviewed spam only; preserve article bytes and other discussions.
Run from the repository root. The original snapshot remains in Git history.
"""
from pathlib import Path
import re
import zipfile
import gzip

REMOVALS = {
    '4167': 'Random-token comment, no discussion content.',
    '104982': 'Generic promotional praise under the commercial name Garden Seed.',
    '106754': 'Unrelated baby-products advertising trackback.',
    '100933': 'Generic plugin promotion linking to an unrelated camera marketing page.',
    '60990': 'Generic praise advertising an unrelated adult site.',
    '91959': 'Boilerplate criticism linking to an unrelated promotional profile.',
}
# The question is relevant and received a substantive answer. Keep it, but remove
# its unrelated antivirus advertising link.
UNLINK = '21159'
COUNTS = {'parameterized-monads-in-haskell': 3, 'generatingfunctorology': 11, 'higher-order-abstract-syntax-a-la-carte': 1, 'the-cofree-comonad-and-the-expression-problem': 7, 'remodeling-precision': 8}


def clean(text):
    removed = 0
    def comment(match):
        nonlocal removed
        block = match.group()
        ident = re.search(r'\bid=[\"\']comment-(\d+)', block).group(1)
        if ident in REMOVALS:
            removed += 1
            return ''
        if ident == UNLINK:
            block = re.sub(r'(<cite>)<a\b[^>]*>(.*?)</a>', r'\1\2', block, flags=re.S)
        return block
    text = re.sub(r'<li\b[^>]*\bid=[\"\']comment-\d+[\"\'][^>]*>.*?</li>', comment, text, flags=re.S)
    if removed:
        def heading(m):
            count = int(m.group(2)) - removed
            return m.group(1) + str(count) + (' Response to' if count == 1 else ' Responses to')
        text = re.sub(r'(<h3 id="comments">)(\d+) Responses? to', heading, text)
    def item(m):
        ids = re.findall(r'#comment-(\d+)', m.group())
        if any(i in REMOVALS for i in ids):
            return ''
        block = m.group()
        for slug, count in COUNTS.items():
            if re.search(r'<link>[^<]*/' + re.escape(slug) + r'/</link>', block):
                block = re.sub(r'<slash:comments>\d+</slash:comments>', f'<slash:comments>{count}</slash:comments>', block)
        return block
    text = re.sub(r'<item>.*?</item>', item, text, flags=re.S)
    for slug, count in COUNTS.items():
        text = re.sub(r'(<a\b[^>]*href=[\"\'][^\"\']*/' + re.escape(slug) + r'/(?:index\.html)?#comments[\"\'][^>]*>)\[\d+\] Comments?(</a>)', lambda m: m.group(1) + f'[{count}] ' + ('Comment' if count == 1 else 'Comments') + m.group(2), text)
    return text


def clean_bytes(before):
    compressed = before.startswith(b"\x1f\x8b")
    payload = gzip.decompress(before) if compressed else before
    try:
        after = clean(payload.decode("utf-8")).encode("utf-8")
    except UnicodeDecodeError:
        return before
    if after == payload:
        return before
    return gzip.compress(after, mtime=int.from_bytes(before[4:8], "little")) if compressed else after


def main():
    changed = []
    for path in Path('reader').rglob('*'):
        if not path.is_file() or path.suffix not in {'.html', '.xml'}:
            continue
        before = path.read_bytes()
        after = clean_bytes(before)
        if after != before:
            path.write_bytes(after)
            changed.append(str(path))
    # Also clean the publicly downloadable HTTrack cache, retaining its entries,
    # compression settings, timestamps, and metadata.
    path = Path('hts-cache/new.zip')
    replacement = path.with_suffix('.clean.zip')
    cache_changes = 0
    with zipfile.ZipFile(path) as old, zipfile.ZipFile(replacement, 'w') as new:
        new.comment = old.comment
        for entry in old.infolist():
            before = old.read(entry)
            after = before
            if '/reader/' in entry.filename and b'comment-' in before:
                try:
                    after = clean_bytes(before)
                except UnicodeDecodeError:
                    pass
            cache_changes += after != before
            new.writestr(entry, after)
    if cache_changes:
        replacement.replace(path)
    else:
        replacement.unlink()
    print(f'{len(changed)} pages/feeds and {cache_changes} cached pages changed.')

if __name__ == '__main__':
    main()
