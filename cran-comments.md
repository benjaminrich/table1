# Version 1.4.3

Note: second submission (first one failed  because I forgot to delete the 'revdep' directory).

## Test environments

* Local:
  - Windows 10, R 4.2.2 (x86_64-w64-mingw32/x64 (64-bit))
  - Linux (NixOS), R 4.2.2 (x86_64-pc-linux-gnu)
* win-builder:
  - Windows Server 2022 (release 4.2.2 (2022-10-31 ucrt) and devel (unstable))
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* pmxpartab: OK
* ggquickeda: OK
* coveffectsplot: OK
* rUM: OK
* ttt: OK
* abmR: ERROR in an example --> package maintainer has been notified
* yamlet: OK

