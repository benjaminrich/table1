# Version 1.5.1

Note: This minor release fixes a bug that was introduced in version 1.5.0.

## Test environments

* Local:
  - Windows 11 x64 (build 26100) (x86_64-w64-mingw32/x64), R version 4.5.1 (2025-06-13 ucrt)
* win-builder:
  - R Under development (unstable) (2025-09-10 r88809 ucrt)
  - R version 4.5.1 (2025-06-13 ucrt)
* With GitHub actions:
  - ubuntu-latest (devel)
    - Ubuntu 24.04.3 LTS (x86_64, linux-gnu), R Under development (unstable) (2025-09-08 r88798)
  - ubuntu-latest (release)
    - Ubuntu 24.04.3 LTS (x86_64, linux-gnu), R version 4.5.1 (2025-06-13)
  - ubuntu-latest (oldrel-1)
    - Ubuntu 24.04.3 LTS (x86_64, linux-gnu), R version 4.4.3 (2025-02-28)
  - windows-latest
    - Windows Server 2022 x64 (build 26100) (x86_64, mingw32), R version 4.5.1 (2025-06-13 ucrt)
  - macos-latest
    - macOS Sequoia 15.5 (aarch64, darwin20), R version 4.5.1 (2025-06-13)

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking for future file timestamps ... NOTE
  unable to verify current time

  - This seems to be a known issue (<https://stackoverflow.com/questions/63613301/>)

* checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

  - Because I don't have GhostScript installed

## Reverse dependencies

* BiVariAn: OK
* coveffectsplot: OK
* ggquickeda: OK
* lulab.utils:  ERROR --> package maintainer has been notified
* metalite.table1: OK
* pmxpartab: OK
* psborrow2: OK
* public.ctn0094data: OK
* rUM: OK
* SelectionBias: OK
* ttt: OK
* yamlet: OK
