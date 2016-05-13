
This document describes the changes in each version of chicken-sdl2-image.

This library follows "[semantic versioning](http://semver.org)".
Until version 1.0.0 is released, the API is not guaranteed to be "stable".
That means the maintainer reserves the right to change the API if needed,
possibly in ways that break backwards compatibility with previous versions.
**Large backwards-incompatible changes are unlikely**,
but there may be small tweaks and fixes to the API if problems are discovered.

After version 1.0.0 is released, the API is guaranteed to remain stable (no backwards-incompatible changes)
until the next new major version (e.g. going from version 1.x to 2.0.0, or 2.x to 3.0.0).


# 0.1.0 (2015-12-19)

Initial release. The following procedures were included:

```
init!               quit!
compiled-version    current-version
load                load*
load-rw             load-rw*
load-typed-rw       load-typed-rw*
```
