# Version 1.5

## Test environments

* Local:
  - Windows 11, R 4.4.3 (x86_64-w64-mingw32/x64 (64-bit))
* win-builder:
  - Windows Server 2022 (release 4.2.2 (2022-10-31 ucrt) and devel (unstable))
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 2 notes

- checking for future file timestamps ... NOTE
  unable to verify current time

  - This seems to be a known issue (<https://stackoverflow.com/questions/63613301/>)

- checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

  - Because I don't have GhostScript installed

## Reverse dependencies

* pmxpartab: OK
* ggquickeda: OK
* coveffectsplot: OK
* rUM: OK
* ttt: OK
* abmR: ERROR in an example --> package maintainer has been notified
* yamlet: OK

