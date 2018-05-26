Some time before 2008 I needed to write tests for Haskell, specifically for
what became <https://github.com/elaforge/karya>.  At the time, I think HUnit
existed, but it did stuff I didn't think I needed, such as manually arranging
tests into a hierarchy, and didn't have stuff I thought I did, such as
automatic test discovery, diffs, filename:linenumber for failed tests, etc.
So I wrote my own.

Over the years some features became obsolete (e.g. ghc now supports
filename:linenumber natively via call stacks), but I got used to this style of
tests, so I decided to try to extract it for use in cabal projects.
