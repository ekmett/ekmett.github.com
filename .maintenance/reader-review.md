# Reader snapshot review — 2026-09-22

The HTTrack snapshot dates to 20 August 2013. It includes 73 of the 85
Comonad.Reader articles in the current website catalog. The 12 articles from
2014 onward are absent; this is not a complete archive of the later blog.

71 archived article bodies have identical whitespace-normalized text to the
later source snapshots in ekmett/comonad.com. Kan Extensions II predates a
correction of two lowercase `o` type operators to `O`. Deriving Strength from
Laziness predates the later corrections and qualification of the costrength
claim. These historical article bodies have not been changed.

All 73 article URLs under https://ekmett.github.io/reader/ returned HTTP 200.
The old ekmett.github.com hostname did not resolve during this check.

## Comment decisions

320 distinct HTML comments and the comment feeds were screened. Six clear spam
records were removed by explicit ID; reasons are in clean-reader-spam.py.
Copies in duplicate pages, RSS feeds, and the downloadable HTTrack cache were
also cleaned. Article/index/feed comment counts were updated.

Comment 21159 contains a relevant question about alternatives to monadic
control flow and a substantive reply from Edward Kmett. Its unrelated antivirus
advertising URL was removed; the question, attribution, and answer remain.

Generic compliments without clear advertising evidence were retained. The
questions in 43367 and 20353 received substantive historical replies and remain.
Technical comments and relevant trackbacks were not removed merely because the
new site selected a shorter discussion.

## Verification

- Compared all 182 article-body occurrences in changed HTML files against the
  preceding commit: unchanged.
- All 74 retained comment occurrences in changed HTML files have identical text;
  their markup is also identical except the advertising link in 21159.
- The six deleted IDs occurred in 12 HTML comment blocks; their RSS items and
  cached copies were removed too.
- No additional comment IDs were found only in feeds, including compressed feeds.
- The cleanup script is repeatable; a second run makes no further changes.

The preceding Git commit retains the complete original snapshot. Changes are
limited to reviewed comments and their counts; original article text is intact.
