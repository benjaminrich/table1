# Version 1.2.1

## Test environments

* Local:
  - Windows 10, R 4.0.1 (x86_64-w64-mingw32/x64)
* travis-ci:
  - Ubuntu Linux 16.04.6 LTS (release and devel)
* win-builder (release (4.0.3) and devel)
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reviewer comments

2020-11-25 Uwe Ligges:

```
URL: https://yihui.name/knitr (moved to https://yihui.org/knitr)
```

Fixed.

# Version 1.2

## Test environments

* Local:
  - Windows 10, R 3.6.2 (x86_64-w64-mingw32/x64)
* travis-ci:
  - Ubuntu Linux 16.04.6 LTS (release and devel)
* win-builder (release and devel)
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* ggquickeda: OK
* coveffectsplot: OK
* yamlet: OK

## Reviewer comments

2020-03-22 Uwe Ligges:

```
Thanks, we see:


Found the following (possibly) invalid file URI:
URI: rmarkdown.rstudio.com/
From: inst/doc/table1-examples.html

Please use fully specified URLs starting with the protocol, e.g. https://.....

Please fix and resubmit.
```

Fixed by adding `https://` prefix.

