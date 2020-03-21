# table1 1.2

* Added `caption` argument to generate a caption for the table.

* Set `render.missing=NULL` to ignore missing values.

* Added `SUM` to `stats.default` for continuous variables.

* Added `PCTnoNA` to `stats.default` and `na.is.category` option to `render.categorical.default`.

* Changed (n=XX) to (N=XX) in default table header.

* Improved error handling in function `eqcut`.

* Function `signif_pad` can now use different decimal marks to support different locales.

* Added function `round_pad` (similar to `signif_pad` but for fixed rounding).

* Added function `t1read`.

# table1 1.1

* Added `quantile.type` argument to `stats.default` to allow selecting which quantile type is computed.

* Special handling for "0%" and "100%" in `stats.apply.rounding` so that no decimals are shown.

* `droplevels` now works properly with nested grouping.

* `table1` now returns a character string (containing HTML). A print method handles the printing. There is no longer a need to use `capture.output` to embed `table1` in a shiny app.

* Removed `standalone` option as it is no longer needed.

* Use `htmltools` functionality to print.

* Preview in RStudio notebook now works ([issue #7](https://github.com/benjaminrich/table1/issues/7)).

* Added `knit_print` method. No need to specify `results="asis"` chunk option anymore when knitting.

* Default CSS is included automatically. It can still be overridden by specifying a custom stylesheet in R Markdown.

* Changed notation from upper case "Q" to small case "q" for percentiles in `stats.default`.

* `Q1` and `Q3` are now aliases for `q25` and `q75` respectively in `stats.default`.

* Tertiles added to `stats.default`.

* New function `eqcut` for creating equal-sized categories from continuous variables.

* New `footnote` argument for adding a footnote to the table.

* The vignette has been expanded with new examples and the documentation improved.

# table1 1.0

* This is the initial release of table1 on CRAN.
