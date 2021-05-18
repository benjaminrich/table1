# Version 1.4.1

* Local:
  - NixOS (Linux), R 4.0.4 (x86_64-pc-linux-gnu)
  - Windows 10, R 4.0.5 and unstable (2021-05-11 r80283) (x86_64-w64-mingw32/x64 (64-bit))
* travis-ci:
  - Ubuntu Linux 16.04.6 LTS (xenial) (release and devel)
    - This is failing, but I don't understand why. The process gets stuck and
      times out while building the vignettes. But the vignettes build fine
      locally, and I've checked all the links and they work fine. The problems
      seems to be unrelated to my package, but something to do with the
      travis-ci environment. I really wish I could fix it, but I'm not sure
      how.
* win-builder:
  - Windows Server 2008 (release (4.0.4) and devel (unstable))
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - This had the following WARNING (unrelated to my package):
      ```
      ! LaTeX Error: File `ulem.sty' not found.
      
      ! Emergency stop.
      <read *> 
      
      Error: processing vignette 'table1-latex.Rmd' failed with diagnostics:
      LaTeX failed to compile /home/docker/table1.Rcheck/vign_test/table1/vignettes/table1-latex.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See table1-latex.log for more info.
      --- failed re-building ‘table1-latex.Rmd’
      ```
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - This had the following NOTE (unrelated to my package):
      ```
      * checking sizes of PDF files under 'inst/doc' ... NOTE
      Unable to find GhostScript executable to run checks on size reduction
      ```
  - Fedora Linux, R-devel, clang, gfortran
    - This had the following problem, which is unrelated to my package:
      ```
      Error: Bioconductor version '3.13' requires R version '4.1'; R version is too new; see
      https://bioconductor.org/install
      ```

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* ggquickeda: OK
* coveffectsplot: OK
* yamlet: OK

