#' Round numbers with 0-padding.
#'
#' Utility functions to round numbers, similar the the base functions \code{signif}
#' and \code{round}, but resulting in character representations that keep zeros at
#' the right edge if they are significant.
#'
#' @param x A numeric vector.
#' @param digits An integer specifying the number of significant digits to keep
#' (for \code{signif_pad}) or the number of digits after the decimal point (for
#' \code{round_pad}).
#' @param round.integers Should rounding be limited to digits to the right of
#' the decimal point?
#' @param round5up Should numbers with 5 as the last digit always be rounded
#' up? The standard R approach is "go to the even digit" (IEC 60559 standard,
#' see \code{\link{round}}), while some other softwares (e.g. SAS, Excel)
#' always round up.
#' @param dec The character symbol to use as decimal mark (locale
#' specific). [Deprecated; use \code{decimal.mark} instead]
#' @param ... Further options, passed to \code{formatC} (which is used
#' internally). Not all options will work, but some might be useful (e.g.
#' \code{big.mark}, \code{decimal.mark}).
#'
#' @return A character vector containing the rounded numbers.
#'
#' @seealso
#' \code{\link{signif}}
#' \code{\link{round}}
#' \code{\link{formatC}}
#' \code{\link{prettyNum}}
#' \code{\link{format}}
#'
#' @examples
#' x <- c(0.9001, 12345, 1.2, 1., 0.1, 0.00001 , 1e5)
#' signif_pad(x, digits=3)
#' signif_pad(x, digits=3, round.integers=TRUE)
#' 
#' # Compare:
#' as.character(signif(x, digits=3))
#' format(x, digits=3, nsmall=3)
#' prettyNum(x, digits=3, drop0trailing=TRUE)
#' prettyNum(x, digits=3, drop0trailing=FALSE)
#' 
#' # This is very close.
#' formatC(x, format="fg", flag="#", digits=3) 
#' formatC(signif(x, 3), format="fg", flag="#", digits=3)
#' 
#' # Could always remove the trailing "."
#' sub("[.]$", "", formatC(x, format="fg", flag="#", digits=3))
#' 
#' @keywords utilities
#' @export
signif_pad <- function(x, digits=3, round.integers=TRUE, round5up=TRUE, dec, ...) {
    args <- list(...)
    if (!missing(dec)) {
        warning("argument dec is deprecated; please use decimal.mark instead.", call.=FALSE)
        args$decimal.mark <- dec
    }
    eps <- if (round5up) x*(10^(-(digits + 3))) else 0

    rx <- ifelse(!is.na(x) & x >= 10^digits & .isFALSE(round.integers),
        round(x),
        signif(x+eps, digits))

    args1 <- c(list(x=rx, digits=digits, format="fg", flag="#"),
        args[names(args) %in% names(formals(formatC))])
    args1 <- args1[!duplicated(names(args1))]
    cx <- do.call(formatC, args1)

    cx[is.na(x)] <- "0"                    # Put in a dummy value for missing x
    cx <- gsub("[^0-9]*$", "", cx)         # Remove any trailing non-digit characters
    ifelse(is.na(x), NA, cx)
}

#' @rdname signif_pad
#' @export
round_pad <- function (x, digits=2, round5up=TRUE, dec, ...) {
    args <- list(...)
    if (!missing(dec)) {
        warning("argument dec is deprecated; please use decimal.mark instead.", call.=FALSE)
        args$decimal.mark <- dec
    }
    eps <- if (round5up) 10^(-(digits + 3)) else 0

    rx <- round(x + eps, digits)

    args1 <- c(list(x=rx, digits=digits, format="f", flag="0"),
        args[names(args) %in% names(formals(formatC))])
    args1 <- args1[!duplicated(names(args1))]
    cx <- do.call(formatC, args1)
    ifelse(is.na(x), NA, cx)
}

# Internal function
format_n <- function (x, ...) {
    args <- list(...)
    args <- args[!(names(args) %in% c("format"))]

    cx <- do.call(formatC,
        c(list(x=x, format="d"),
          args[names(args) %in% names(formals(formatC))]))
    ifelse(is.na(x), NA, cx)
}

#' Compute some basic descriptive statistics.
#'
#' Values of type \code{factor}, \code{character} and \code{logical} are
#' treated as categorical. For logicals, the two categories are given the
#' labels `Yes` for \code{TRUE}, and `No` for \code{FALSE}.  Factor levels with
#' zero counts are retained.
#'
#' @param x A vector or numeric, factor, character or logical values.
#' @param quantile.type An integer from 1 to 9, passed as the \code{type}
#' argument to function \code{\link[stats]{quantile}}.
#' @param ... Further arguments (ignored).
#'
#' @return A list. For numeric \code{x}, the list contains the numeric elements:
#' \itemize{
#'   \item \code{N}: the number of non-missing values
#'   \item \code{NMISS}: the number of missing values
#'   \item \code{SUM}: the sum of the non-missing values
#'   \item \code{MEAN}: the mean of the non-missing values
#'   \item \code{SD}: the standard deviation of the non-missing values
#'   \item \code{MIN}: the minimum of the non-missing values
#'   \item \code{MEDIAN}: the median of the non-missing values
#'   \item \code{CV}: the percent coefficient of variation of the non-missing values
#'   \item \code{GMEAN}: the geometric mean of the non-missing values if non-negative, or \code{NA}
#'   \item \code{GSD}: the geometric standard devaition of the non-missing values if non-negative, or \code{NA}
#'   \item \code{GCV}: the percent geometric coefficient of variation of the
#'   non-missing values if non-negative, or \code{NA}
#'   \item \code{qXX}: various quantiles (percentiles) of the non-missing
#'   values (q01: 1\%, q02.5: 2.5\%, q05: 5\%, q10: 10\%, q25: 25\% (first
#'   quartile), q33.3: 33.33333\% (first tertile), q50: 50\% (median, or second
#'   quartile), q66.7: 66.66667\% (second tertile), q75: 75\% (third quartile),
#'   q90: 90\%, q95: 95\%, q97.5: 97.5\%, q99: 99\%)
#'   \item \code{Q1}: the first quartile of the non-missing values (alias \code{q25})
#'   \item \code{Q2}: the second quartile of the non-missing values (alias \code{q50} or \code{Median})
#'   \item \code{Q3}: the third quartile of the non-missing values (alias \code{q75})
#'   \item \code{IQR}: the inter-quartile range of the non-missing values (i.e., \code{Q3 - Q1})
#'   \item \code{T1}: the first tertile of the non-missing values (alias \code{q33.3})
#'   \item \code{T2}: the second tertile of the non-missing values (alias \code{q66.7})
#' }
#' If \code{x} is categorical (i.e. factor, character or logical), the list
#' contains a sublist for each category, where each sublist contains the
#' numeric elements:
#' \itemize{
#'   \item \code{FREQ}: the frequency count
#'   \item \code{PCT}: the percent relative frequency, including NA in the denominator
#'   \item \code{PCTnoNA}: the percent relative frequency, excluding NA from the denominator
#' }
#'
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' stats.default(x)
#' 
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' stats.default(y)
#' stats.default(is.na(y))
#'
#' @keywords utilities
#' @export
#' @importFrom stats sd median quantile IQR na.omit
stats.default <- function(x, quantile.type=7, ...) {
    if (is.logical(x)) {
        x <- factor(1-x, levels=c(0, 1), labels=c("Yes", "No"))
    }
    if (is.factor(x) || is.character(x)) {
        y <- table(x, useNA="no")
        nn <- names(y)
        nn[is.na(nn)] <- "Missing"
        names(y) <- nn
        lapply(y, function(z) list(FREQ=z, PCT=100*z/length(x), PCTnoNA=100*z/sum(y)))
    } else if (is.numeric(x) && sum(!is.na(x)) == 0) {
        list(
            N=sum(!is.na(x)),
            NMISS=sum(is.na(x)),
            SUM=NA,
            MEAN=NA,
            SD=NA,
            CV=NA,
            GMEAN=NA,
            GSD=NA,
            GCV=NA,
            MEDIAN=NA,
            MIN=NA,
            MAX=NA,
            q01=NA,
            q025=NA,
            q05=NA,
            q10=NA,
            q25=NA,
            q50=NA,
            q75=NA,
            q90=NA,
            q95=NA,
            q975=NA,
            q99=NA,
            Q1=NA,
            Q2=NA,
            Q3=NA,
            IQR=NA,
            T1=NA,
            T2=NA)
    } else if (is.numeric(x)) {
        q <- quantile(x, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 1/3, 0.5, 2/3, 0.75, 0.9, 0.95, 0.975, 0.99), na.rm=TRUE, type=quantile.type)
        list(
            N=sum(!is.na(x)),
            NMISS=sum(is.na(x)),
            SUM=sum(x, na.rm=TRUE),
            MEAN=mean(x, na.rm=TRUE),
            SD=sd(x, na.rm=TRUE),
            CV=100*sd(x, na.rm=TRUE)/abs(mean(x, na.rm=TRUE)),
            GMEAN=if (any(na.omit(x) <= 0)) NA else exp(mean(log(x), na.rm=TRUE)),
            GSD=if (any(na.omit(x) <= 0)) NA else exp(sd(log(x), na.rm=TRUE)),
            GCV=if (any(na.omit(x) <= 0)) NA else 100*sqrt(exp(sd(log(x), na.rm=TRUE)^2) -1),
            MEDIAN=median(x, na.rm=TRUE),
            MIN=min(x, na.rm=TRUE),
            MAX=max(x, na.rm=TRUE),
            q01=q["1%"],
            q02.5=q["2.5%"],
            q05=q["5%"],
            q10=q["10%"],
            q25=q["25%"],
            q50=q["50%"],
            q75=q["75%"],
            q90=q["90%"],
            q95=q["95%"],
            q97.5=q["97.5%"],
            q99=q["99%"],
            Q1=q["25%"],
            Q2=q["50%"],
            Q3=q["75%"],
            IQR=q["75%"] - q["25%"],
            T1=q["33.33333%"],
            T2=q["66.66667%"])
    } else {
        stop(paste("Unrecognized variable type:", class(x)))
    }
}

#' Apply rounding to basic descriptive statistics.
#'
#' Not all statistics should be rounded in the same way, or at all. This
#' function will apply rounding selectively to a list of statistics as returned
#' by \code{\link{stats.default}}. In particular we don't round counts (N and
#' FREQ), and for MIN, MAX and MEDIAN the \code{digits} is interpreted as the
#' \emph{minimum} number of significant digits, so that we don't loose any
#' precision. Percentages are rounded to a fixed number of decimal places
#' (default 1) rather than a specific number of significant digits.
#'
#' @param x A list, such as that returned by \code{\link{stats.default}}.
#' @param digits An integer specifying the number of significant digits to keep.
#' @param digits.pct An integer specifying the number of digits after the
#' decimal place for percentages.
#' @param round.median.min.max Should rounding applied to median, min and max?
#' @param round.integers Should rounding be limited to digits to the right of
#' the decimal point?
#' @param round5up Should numbers with 5 as the last digit always be rounded
#' up? The standard R approach is "go to the even digit" (IEC 60559 standard,
#' see \code{\link{round}}), while some other softwares (e.g. SAS, Excel)
#' always round up.
#' @param rounding.fn The function to use to do the rounding. Defaults to
#' \code{\link{signif_pad}}.
#' @param ... Further arguments.
#'
#' @return A list with the same number of elements as \code{x}. The rounded
#' values will be \code{character} (not \code{numeric}) and will have 0 padding
#' to ensure consistent number of significant digits. 
#'
#' @seealso
#' \code{\link{signif_pad}}
#' \code{\link{stats.default}}
#' @examples
#' x <- round(exp(rnorm(100, 1, 1)), 6)
#' stats.default(x)
#' stats.apply.rounding(stats.default(x), digits=3)
#' stats.apply.rounding(stats.default(round(x, 1)), digits=3)
#'
#' @keywords utilities
#' @export
stats.apply.rounding <- function(x, digits=3, digits.pct=1, round.median.min.max=TRUE, round.integers=TRUE, round5up=TRUE, rounding.fn=signif_pad, ...) {
    mindig <- function(x, digits) {
        cx <- format(x)
        ndig <- nchar(gsub("\\D", "", cx))
        ifelse(ndig > digits, cx, rounding.fn(x, digits=digits,
                round.integers=round.integers, round5up=round5up, ...))
    }
    format.percent <- function(x, digits) {
        if (x == 0) "0"
        else if (x == 100) "100"
        else round_pad(x, digits=digits.pct, ...)
    }
    if (!is.list(x)) {
        stop("Expecting a list")
    }
    if (is.list(x[[1]])) {
        # Apply recursively
        lapply(x, stats.apply.rounding, digits=digits, digits.pct=digits.pct,
            round.integers=round.integers, round5up=round5up, ...)
    } else {
        r <- lapply(x, rounding.fn, digits=digits,
                round.integers=round.integers, round5up=round5up, ...)
        nr <- c("N", "FREQ")       # No rounding
        nr <- nr[nr %in% names(x)]
        nr <- nr[!is.na(x[nr])]
        r[nr] <- lapply(x[nr], format_n, ...)
        if (!round.median.min.max) {
            sr <- c("MEDIAN", "MIN", "MAX")  # Only add significant digits, don't remove any
            sr <- sr[sr %in% names(x)]
            r[sr] <- lapply(x[sr], mindig, digits=digits)
        }
        pr <- c("PCT", "CV", "GCV")   # Percentages
        pr <- pr[pr %in% names(x)]
        pr <- pr[!is.na(x[pr])]
        r[pr] <- lapply(as.numeric(x[pr]), format.percent, digits=digits.pct)
        r
    }
}

#' Render values for table output.
#'
#' Called from \code{\link{table1}} by default to render values for
#' displaying in the table. This function forwards the call to separate
#' functions for rendering continuous, categorical and missing values.
#' The idea is that each of these functions can be overridden to customize
#' the table output.
#'
#' @param x A vector or numeric, factor, character or logical values.
#' @param name Name of the variable to be rendered (ignored). 
#' @param missing Should missing values be included?
#' @param transpose Logical indicating whether on not the table is transposed.
#' @param render.empty A \code{character} to return when \code{x} is empty.
#' @param render.continuous A function to render continuous (i.e.
#' \code{numeric}) values. Can also be a \code{character} string, in which case
#' it is passed to \code{\link{parse.abbrev.render.code}}.
#' @param render.categorical A function to render categorical (i.e.
#' \code{factor}, \code{character} or \code{logical}) values. Can also be a
#' \code{character} string, in which case it is passed to
#' \code{\link{parse.abbrev.render.code}}.
#' @param render.missing A function to render missing (i.e. \code{NA}) values.
#' Can also be a \code{character} string, in which case it is passed to
#' \code{\link{parse.abbrev.render.code}}. Set to \code{NULL} to ignore missing
#' values.
#' @param ... Further arguments, passed to \code{\link{stats.apply.rounding}}.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table. However, the first names should be empty as it
#' will be replaced by the name of the variable. Empty strings are allowed and
#' result in empty table cells.
#'
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' render.default(x)
#' render.default(x, TRUE)
#' 
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' render.default(y)
#'
#' @keywords utilities
#' @export
render.default <- function(x, name, missing=any(is.na(x)), transpose=F,
                           render.empty="NA",
                           render.continuous=render.continuous.default,
                           render.categorical=render.categorical.default,
                           render.missing=render.missing.default, ...) {
    if (is.character(render.continuous)) {
        render.continuous <- parse.abbrev.render.code(code=render.continuous, ...)
    }
    if (is.character(render.categorical)) {
        render.categorical <- parse.abbrev.render.code(code=render.categorical, ...)
    }
    if (!is.null(render.missing) && is.character(render.missing)) {
        render.missing <- parse.abbrev.render.code(code=render.missing, ...)
    }
    if (length(x) == 0) {
        return(render.empty)
    }
    if (is.logical(x)) {
        x <- factor(x, levels=c(T, F), labels=c("Yes", "No"))
    }
    if (is.factor(x) || is.character(x)) {
        r <- do.call(render.categorical, c(list(x=x), list(...)))
    } else if (is.numeric(x)) {
        r <- do.call(render.continuous, c(list(x=x), list(...)))
    } else {
        stop(paste("Unrecognized variable type:", class(x)))
    }
    if (missing && !is.null(render.missing)) {
        r <- c(r, do.call(render.missing, c(list(x=x), list(...))))
    }
    if (transpose) {
        if (!is.null(names(r))) {
            r <- paste0(sprintf("%s: %s", names(r), r), collapse="<br/>")
        } else {
            r <- paste0(r, collapse="<br/>")
        }
    }
    r
}

#' Parse abbreviated code for rendering table output.
#'
#' @param code A \code{character} vector specifying the statistics to display
#' in abbreviated code. See Details. 
#' @param ... Further arguments, passed to \code{\link{stats.apply.rounding}}.
#'
#' @return A function that takes a single argument and returns a
#' \code{character} vector.
#'
#' @details In abbreviated code, the words N, NMISS, MEAN, SD, MIN, MEDIAN,
#' MAX, IQR, CV, GMEAN, GSD, GCV, FREQ and PCT are substituted for their respective
#' values (see \code{\link{stats.default}}). The substitution is case
#' insensitive, and the substituted values are rounded appropriately (see
#' \code{\link{stats.apply.rounding}}). Other text is left unchanged. The
#' \code{code} can be a vector, in which case each element is displayed in its
#' own row in the table. The \code{names} of \code{code} are used as row
#' labels; if no names are present, then the \code{code} itself is used unless
#' \code{code} is of length 1, in which case no label is used (for numeric
#' variables only, categorical variables are always labeled by the class
#' label). The special name '.' also indicates that \code{code} itself be is
#' used as the row label.
#'
#' @examples
#' \dontrun{
#' x <- round(exp(rnorm(100, log(20), 1)), 2)
#' stats.default(x)
#' f <- parse.abbrev.render.code(c("Mean (SD)", "Median [Min, Max]"), 3)
#' f(x)
#' f2 <- parse.abbrev.render.code(c("Geo. Mean (Geo. CV%)" = "GMean (GCV%)"), 3)
#' f2(x)
#' f3 <- parse.abbrev.render.code(c("Mean (SD)"), 3)
#' f3(x)
#' 
#' x <- sample(c("Male", "Female"), 30, replace=T)
#' stats.default(x)
#' f <- parse.abbrev.render.code("Freq (Pct%)")
#' f(x)
#' }
#'
#' @keywords utilities
#' @export
parse.abbrev.render.code <-
function(code, ...) {
    codestr <- code
    if (is.null(names(codestr)) && length(codestr) > 1) {
        names(codestr) <- codestr
    }
    names(codestr)[names(codestr) == "."] <- codestr[names(codestr) == "."]
    function(x, ...) {
        s <- stats.apply.rounding(stats.default(x, ...), ...)
        g <- function(ss) {
            res <- codestr
            for (nm in names(ss)) {
                res <- gsub(paste0("\\b", nm, "\\b"), ss[[nm]], res, ignore.case=T)
            }
            names(res) <- names(codestr)
            res
        }
        if (!is.list(s)) {
            stop("Expecting a list")
        }
        if (is.list(s[[1]])) {
            res <- lapply(s, g)
            nm <- ifelse(sapply(res, seq_along)==1, "1", "")
            nm[nm=="1"] <- names(s)
            res <- unlist(res)
            names(res) <- nm 
            c("", res)
        } else {
            if (length(codestr) == 1 && is.null(names(codestr))) {
                g(s)
            } else {
                c("", g(s))
            }
        }
    }
}

#' Render continuous values for table output.
#'
#' Called from \code{\link{table1}} by default to render continuous (i.e.
#' \code{numeric}) values for displaying in the table.
#'
#' @param x A numeric vector.
#' @param ... Further arguments, passed to \code{\link{stats.apply.rounding}}.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table. However, the first names should be empty as it
#' will be replaced by the name of the variable. Empty strings are allowed and
#' result in empty table cells.
#'
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' render.continuous.default(x)
#' 
#' @keywords utilities
#' @export
render.continuous.default <- function(x, ...) {
    with(stats.apply.rounding(stats.default(x, ...), ...), c("",
        "Mean (SD)"         = sprintf("%s (%s)", MEAN, SD),
        "Median [Min, Max]" = sprintf("%s [%s, %s]", MEDIAN, MIN, MAX)))
}

#' Render categorical values for table output.
#'
#' Called from \code{\link{table1}} by default to render categorical (i.e.
#' \code{factor}, \code{character} or \code{logical}) values for displaying in the table.
#'
#' @param x A vector of type \code{factor}, \code{character} or \code{logical}.
#' @param ... Further arguments, passed to \code{\link{stats.apply.rounding}}.
#' @param na.is.category Include missing values in the denominator for
#' calculating percentages (the default) or omit them.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table. However, the first names should be empty as it
#' will be replaced by the name of the variable. Empty strings are allowed and
#' result in empty table cells.
#'
#' @examples
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' render.categorical.default(y)
#' @keywords utilities
#' @export
render.categorical.default <- function(x, ..., na.is.category=TRUE) {
    c("", sapply(stats.apply.rounding(stats.default(x, ...), ...), function(y) with(y,
        sprintf("%s (%s%%)", FREQ, if (na.is.category) PCT else PCTnoNA))))
}

#' Render missing values for table output.
#'
#' Called from \code{\link{table1}} by default to render missing (i.e.
#' \code{NA}) values for displaying in the table.
#'
#' @param x A vector.
#' @param ... Further arguments, passed to \code{\link{stats.apply.rounding}}.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table. Empty strings are allowed and
#' result in empty table cells.
#'
#' @examples
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' render.missing.default(y)
#' @keywords utilities
#' @export
render.missing.default <- function(x, ...) {
    with(stats.apply.rounding(stats.default(is.na(x), ...), ...)$Yes,
        c(Missing=sprintf("%s (%s%%)", FREQ, PCT)))
}

#' Render variable labels for table output.
#'
#' Called from \code{\link{table1.formula}} by default to render variable labels
#' for displaying in the table.
#'
#' @param x A vector, usually with the \code{\link{label}} and (if appropriate)
#' \code{\link{unit}} attributes.
#' @param transpose Logical indicating whether on not the table is transposed.
#'
#' @return A \code{character}, which may contain HTML markup.
#'
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' label(x) <- "Weight"
#' units(x) <- "kg"
#' render.varlabel(x)
#'
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' label(y) <- "Sex"
#' render.varlabel(y)
#' @keywords utilities
#' @export
render.varlabel <- function(x, transpose=F) {
    l <- if (has.units(x)) {
        sprintf("%s (%s)", label(x), units(x))
    } else {
        sprintf("%s", label(x))
    }
    attr(l, "html") <- if (has.units(x) && transpose) {
        sprintf("<span class='varlabel'>%s<br/><span class='varunits'>(%s)</span></span>", label(x), units(x))
    } else if (has.units(x)) {
        sprintf("<span class='varlabel'>%s<span class='varunits'> (%s)</span></span>", label(x), units(x))
    } else {
        sprintf("<span class='varlabel'>%s</span>", label(x))
    }
    l
}

#' Render strata labels for table output.
#'
#' Called from \code{\link{table1.formula}} to render strata labels
#' for displaying in the table.
#'
#' @param label A \code{character} vector containing the labels.
#' @param n A \code{numeric} vector containing the sizes.
#'
#' @return A \code{character}, which may contain HTML markup.
#' @keywords internal
#' @export
render.strat.default <- function(label, n, transpose=F) {
    sprintf(
        ifelse(is.na(n), 
            "<span class='stratlabel'>%s</span>",
            "<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>"),
        label, n)
}

#' Convert to HTML table rows.
#'
#' Many functions exist in R to generate HTML tables.  These functions are
#' useful for generating HTML table fragments (rather than whole tables), which
#' can then be used to build up complete tables. The first column my be used to
#' label the rows of the table. Row labels, if specified, can have a special
#' HTML \code{class} designated, which can be useful as a hook to customize
#' their appearance using CSS. The same is true for the the first and last row
#' of cells. 
#'
#' @param x A vector or table-like structure (e.g. a \code{\link{data.frame}} or \code{\link{matrix}}).
#' @param row.labels Values for the first column, typically used to label the row, or \code{NULL} to omit.
#' @param th A logical. Should \code{th} tags be used rather than \code{td}? 
#' @param class HTML class attribute. Can be a single \code{character}, a vector or a matrix.
#' @param rowlabelclass HTML class attribute for the row labels (i.e. first column).
#' @param firstrowclass HTML class attribute for the first row of cells.
#' @param lastrowclass HTML class attribute for the last row of cells.
#' @param ... Additional arguments.
#'
#' @return A \code{character} which contains an HTML table fragment.
#'
#' @examples
#' x <- matrix(signif_pad(exp(rnorm(5*5, 1, 1))), 5, 5)
#' table.data(x)
#' cat(table.rows(x, NULL))
#' cat(table.rows(x, LETTERS[1:nrow(x)]))
#' cat(table.rows(LETTERS[1:3], "Headings", th=TRUE))
#' @keywords utilities
#' @export
table.rows <- function(x, row.labels=rownames(x), th=FALSE, class=NULL, rowlabelclass="rowlabel", firstrowclass="firstrow", lastrowclass="lastrow", ...) {
    if (is.null(row.labels)) row.labels <- ""
    td <- table.data(x=x, row.labels=row.labels, th=th, class=class, rowlabelclass=rowlabelclass, firstrowclass=firstrowclass, lastrowclass=lastrowclass, ...)
    tr <- paste("<tr>\n", td, "\n</tr>\n", sep="")
    paste(tr, sep="", collapse="")
}

#' @describeIn table.rows Convert to HTML table data (cells).
#' @export
table.data <- function(x, row.labels=rownames(x), th=FALSE, class=NULL, rowlabelclass="rowlabel", firstrowclass="firstrow", lastrowclass="lastrow", ...) {
    tag <- ifelse(th, "th", "td")
    rl <- row.labels  # Make sure it gets evaluated early for default arg
    if (is.data.frame(x)) {
        x <- sapply(x, as.character)
    }
    if (is.null(dim(x)) || length(dim(x)) < 2) {
        x <- matrix(as.character(x), nrow=1)
    } else if (length(dim(x)) > 2) {
        stop("x cannot have more than 2 dimensions.")
    }
    nr <- nrow(x)
    nc <- ncol(x)
    firstrowclass <- rep_len(as.character(firstrowclass), nc)
    lastrowclass <- rep_len(as.character(lastrowclass), nc)
    cls <- if (is.null(class)) NA else class
    cls <- matrix(as.character(cls), nr, nc)
    if (!is.null(rl)) {
        rl <- rep_len(as.character(rl), nr)
        x <- cbind(rl, x)
        rowlabelclass <- rep_len(rowlabelclass, nr)
        if (!is.null(rowlabelclass)) {
            cls <- cbind(rowlabelclass, cls)
        } else {
            cls <- cbind(NA, cls)
        }
    }
    if (!is.null(firstrowclass)) {
        cls[1,] <- ifelse(is.na(cls[1,]), firstrowclass, paste(cls[1,], firstrowclass))
    }
    if (!is.null(lastrowclass)) {
        cls[nr,] <- ifelse(is.na(cls[nr,]), lastrowclass, paste(cls[nr,], lastrowclass))
    }
    cls <- ifelse(is.na(cls), "", paste0(" class='", cls, "'"))
    td <- paste0("<", tag, cls, ">", x, "</", tag, ">")
    dim(td) <- dim(x)
    apply(td, 1, paste, collapse="\n")
}


#' Label attribute.
#'
#' @param x An object.
#' @param value A \code{character} specifying the label.
#'
#' @examples
#' x <- 1:10
#' label(x) <- "Foo"
#' x <- setLabel(x, "Foo") # Alternative syntax
#' has.label(x)
#' label(x)
#' @keywords utilities
#' @export
'label' <- function(x) {
    attr(x, "label")
}

#' @describeIn label Set label attribute.
#' @export
'label<-' <- function(x, value) {
    attr(x, "label") <- value
    x
}

#' @describeIn label Set label attribute.
#' @export
setLabel <- function(x, value) {
    attr(x, "label") <- value
    x
}

#' @describeIn label Check for label attribute.
#' @export
has.label <- function(x) {
    !is.null(attr(x, "label"))
}

#' Units attribute.
#'
#' @param x An object.
#' @param value A \code{character} specifying the units
#'
#' @examples
#' x <- 1:10
#' units(x) <- "cm"
#' has.units(x)
#' units(x)
#' @keywords utilities
#' @export
'units' <- function(x) {
    attr(x, "units")
}

#' @describeIn units Set units attribute.
#' @export
'units<-' <- function(x, value) {
    attr(x, "units") <- value 
    x
}

#' @describeIn units Check for attribute.
#' @export
has.units <- function(x) {
    !is.null(attr(x, "units"))
}

#' Generate an HTML table of descriptive statistics.
#'
#' Produces a nicely formatted table of descriptive statistics for any number
#' of numeric or categorical variables, optionally stratified by a factor.
#'
#' @details
#' There are two interfaces, the default, which typically takes a list of
#' \code{data.frame}s for \code{x}, and the formula interface. The formula
#' interface is less flexible, but simpler to use and designed to handle the
#' most common use cases. It is important to use factors appropriately for
#' categorical variables (i.e. have the levels labeled properly and in the
#' desired order). The contents of the table can be customized by providing
#' user-defined `renderer' functions. Customization of the table appearance is
#' deliberately not attempted, as this is best accomplished with CSS. To
#' facilitate this, some tags (such as row labels) are given specific classes
#' for easy CSS selection.
#' 
#' For the formula version, the formula is expected to be a one-sided formula,
#' optionally  with a vertical bar separating the variables that are to appear
#' as data in the table (as rows) from those used for stratification (i.e.
#' columns). There can be at most 2 variables for stratification (and only one
#' if \code{transpose = TRUE} is specified), and if 2 are specified, the second
#' is nested within the first. The formula may contain a dot (".") to refer to
#' "all variables in \code{data} other than those that appear elsewhere in the
#' formula". It is legitimate to use functions inside the formula to create new
#' variables.
#'
#' For the default version, is is expected that \code{x} is a named
#' list of \code{data.frame}s, one for each stratum, with names corresponding to
#' strata labels.
#'
#' Extra columns can be added to the table using the \code{extra.col} argument.
#' This is an optional named list of functions, with the names corresponding to
#' the column headings. Each function will be called once for each variable
#' included in the table. Each function should expect 2 arguments, the first
#' being a list, the second the name of the variable. The contents of the
#' list passed in as the first argument will be the data associated with each
#' stratum in the table; i.e., one element for each normal column (not extra
#' column). It is then up the function to compute the value to appear in
#' the extra column and return it as a string. By default, extra columns will
#' be placed to the far right, after the normal columns, in the order they are
#' specified in. This can be overridden, however, using the
#' \code{extra.col.pos} vector of integer positions. For example, to place the
#' first extra column in position 1 (far left), and the second extra column in
#' position 3, use \code{extra.col.pos = c(1, 3)}; any extra columns that are
#' not assigned positions will be placed to the far right. A typical use case
#' for extra columns would be a column of p-values for differences between
#' strata. Note that this feature is not available when the option
#' \code{transpose = TRUE} is specified.
#'
#' @param x An object, typically a \code{formula} or list of \code{data.frame}s (see Details).
#' @param data For the formula interface, a \code{data.frame} from which the
#' variables in \code{x} should be taken.
#' @param overall A label for the "Overall" column. Specify \code{NULL} or
#' \code{FALSE} to omit the column altogether.
#' @param labels A list containing labels for variables, strata and groups (see Details).
#' @param groupspan A vector of integers specifying the number of strata to group together.
#' @param rowlabelhead A heading for the first column of the table, which contains the row labels.
#' @param droplevels Should empty factor levels be dropped?
#' @param transpose Logical. Should the table be transposed (i.e. strata as
#' rows and variables as columns)?
#' @param topclass A class attribute for the outermost (i.e. \code{<table>}) tag.
#' @param footnote A character string to be added as a footnote to the table.
#' Can also be a vector which results in multiple lines of footnotes.
#' The default \code{NULL} causes the footnote to be omitted.
#' @param caption A character string to be added as a caption to the table.
#' The default \code{NULL} causes the caption to be omitted.
#' @param render A function to render the table cells (see Details).
#' @param render.strat A function to render the stratum labels. Accepts 3
#' arguments: the stratum label, the stratum size (number of observations), and
#' a flag indicating whether we are in transpose mode or not. See
#' \code{\link{render.strat.default}} for an example.
#' @param extra.col An optional names list of functions that produce extra columns in the table (see Details).
#' @param extra.col.pos An optional integer vector given the positions of extra columns (see Details).
#' @param ... Further arguments, passed to \code{render}.
#'
#' @return An object of class "table1".
#'
#' @examples
#'
#' dat <- expand.grid(id=1:10, sex=c("Male", "Female"), treat=c("Treated", "Placebo"))
#' dat$age <- runif(nrow(dat), 10, 50)
#' dat$age[3] <- NA  # Add a missing value
#' dat$wt <- exp(rnorm(nrow(dat), log(70), 0.2))
#' 
#' label(dat$sex) <- "Sex"
#' label(dat$age) <- "Age"
#' label(dat$treat) <- "Treatment Group"
#' label(dat$wt) <- "Weight"
#' 
#' units(dat$age) <- "years"
#' units(dat$wt) <- "kg"
#' 
#' # One level of stratification
#' table1(~ sex + age + wt | treat, data=dat)
#' 
#' # Two levels of stratification (nesting)
#' table1(~ age + wt | treat*sex, data=dat)
#' 
#' # Switch the order or nesting
#' table1(~ age + wt | sex*treat, data=dat)
#' 
#' # No stratification
#' table1(~ treat + sex + age + wt, data=dat)
#' 
#' # Something more complicated
#' 
#' dat$dose <- ifelse(dat$treat=="Placebo", "Placebo",
#'                    sample(c("5 mg", "10 mg"), nrow(dat), replace=TRUE))
#' dat$dose <- factor(dat$dose, levels=c("Placebo", "5 mg", "10 mg"))
#' 
#' strata <- c(split(dat, dat$dose),
#'             list("All treated"=subset(dat, treat=="Treated")),
#'             list(Overall=dat))
#' 
#' labels <- list(
#'     variables=list(sex=render.varlabel(dat$sex),
#'                    age=render.varlabel(dat$age),
#'                    wt=render.varlabel(dat$wt)),
#'     groups=list("", "Treated", ""))
#' 
#' my.render.cont <- function(x) {
#'     with(stats.default(x), 
#'         sprintf("%0.2f (%0.1f)", MEAN, SD))
#' }
#' 
#' table1(strata, labels, groupspan=c(1, 3, 1), render.continuous=my.render.cont)
#'
#' # Transposed table
#' table1(~ age + wt | treat, data=dat, transpose=TRUE)
#' 
#' @keywords utilities
#' @export
table1 <- function(x, ...) {
    UseMethod("table1")
}

#' @describeIn table1 The default interface, where \code{x} is a \code{data.frame}.
#' @export
table1.default <- function(x, labels, groupspan=NULL, rowlabelhead="", transpose=FALSE, topclass="Rtable1", footnote=NULL, caption=NULL, render=render.default, render.strat=render.strat.default, extra.col=NULL, extra.col.pos=NULL, ...) {
    .table1.internal(
        x             = x,
        labels        = labels,
        groupspan     = groupspan,
        rowlabelhead  = rowlabelhead,
        transpose     = transpose,
        topclass      = topclass,
        footnote      = footnote,
        caption       = caption,
        render        = render,
        render.strat  = render.strat,
        extra.col     = extra.col,
        extra.col.pos = extra.col.pos, ...)
}

.table1.internal <- function(x, labels, groupspan=NULL, rowlabelhead="", transpose=FALSE, topclass="Rtable1", footnote=NULL, caption=NULL, render=render.default, render.strat=render.strat.default, extra.col=NULL, extra.col.pos=NULL, ...) {
    if (is.null(labels$strata)) {
        labels$strata <- names(x)
    }
    if (is.null(names(labels$strata))) {
        names(labels$strata) <- names(x)
    }
    if (is.character(render)) {
        render <- parse.abbrev.render.code(code=render, ...)
    }

    # Convert any character columns to factor
    char2factor <- function(df) {
        df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
        df
    }
    x <- lapply(x, char2factor)

    # Number of rows per stratum
    strat_n <- format_n(sapply(x, nrow), ...)

    any.missing <- sapply(names(labels$variables), function(v) do.call(sum, lapply(x, function(s) sum(is.na(s[[v]])))) > 0)

    if (transpose) {
        ncolumns <- length(labels$variables)
        if (ncolumns > 12) {
            warning(sprintf("Table has %d columns. Are you sure this is what you want?", ncolumns))
        }
        headings <- t(unlist(labels$variables))
        contents <- lapply(names(x), function(s) {
            do.call(cbind, lapply(names(labels$variables), function(v) {
                lvls <- unique(do.call(c, lapply(x, function(s) levels(s[[v]]))))
                z <- x[[s]][[v]]
                if (!is.null(lvls)) {
                    z <- factor(z, levels=lvls)
                }
                y <- render(x=z, name=v, missing=any.missing[v], transpose=T, ...)
                y <- paste0(y, collapse="<br/>")
                names(y) <- labels$variables[[v]]
                y <- t(y)
                rownames(y) <- render.strat(labels$strata[s], strat_n[s], ...)
                y }))})
    } else {
        headings <- rbind(labels$strata[names(x)], strat_n)
        if (!is.null(extra.col)) {
            headings <- cbind(headings, rbind(names(extra.col), rep(NA, length(extra.col))))
            if (!is.null(extra.col.pos)) {
                if (!is.numeric(extra.col.pos) || any(extra.col.pos > ncol(headings))) {
                    stop("extra.col.pos should be a vector of column positions")
                }
                if (length(extra.col.pos) > length(extra.col)) {
                    stop("length of extra.col.pos should not exceed that of extra.col")
                }
                # Permute columns
                s1 <- seq(length(x) + 1, length.out=length(extra.col.pos))
                s2 <- setdiff(1:ncol(headings), s1)
                colpermute <- rep(0, ncol(headings))
                colpermute[extra.col.pos] <- s1
                colpermute[-extra.col.pos] <- s2
                headings <- headings[, colpermute, drop=F]
            }
        }
        ncolumns <- ncol(headings)
        if (ncolumns > 12) {
            warning(sprintf("Table has %d columns. Are you sure this is what you want?", ncolumns))
        }
        contents <- lapply(names(labels$variables), function(v) {
            lvls <- unique(do.call(c, lapply(x, function(s) levels(s[[v]]))))
            y <- do.call(cbind, lapply(x, function(s) {
                z <- s[[v]]
                if (!is.null(lvls)) {
                    z <- factor(z, levels=lvls)
                }
                render(x=z, name=v, missing=any.missing[v], ...)}))

            if (!is.null(extra.col)) {
                pad_with_empty <- function(w, n) {
                    rep(c(as.character(w), rep("", n)), length.out=n)
                }
                y2 <- do.call(cbind, lapply(extra.col, function(f) {
                    pad_with_empty(f(lapply(x, getElement, name=v), v, ...), nrow(y))
                }))
                y <- cbind(y, y2)
                if (!is.null(extra.col.pos)) {
                    y <- y[, colpermute, drop=F]
                }
            }

            rownames(y) <- paste(rownames(y), sep="")
            rownames(y)[1] <- labels$variables[[v]]
            y })
    }

    obj <- list(
        contents     = contents,
        headings     = headings,
        labels       = labels,
        topclass     = topclass,
        ncolumns     = ncolumns,
        groupspan    = groupspan,
        transpose    = transpose,
        rowlabelhead = rowlabelhead,
        caption      = caption,
        footnote     = footnote,
        render.strat = render.strat)

    update_html(structure("", obj=obj))
}

#' Update HTML.
#'
#' Used to (re-)generate the HTML code for a \code{link{table1}} object. In
#' most cases, this should not be used direction, unless you know what you are
#' doing.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @return An object of class "table1" which contains the updated HTML.
#' @export
update_html <- function(x) {
    obj <- attr(x, "obj")
    with(obj, {
        if (transpose) {
            thead <- headings
        } else {
            thead <- t(render.strat(headings[1,], headings[2,]))
        }

        if (is.null(topclass) || topclass=="") {
            topclass <- ""
        } else if (is.character(topclass) && length(topclass)==1) {
            topclass <- sprintf(' class="%s"', topclass)
        } else {
            stop("topclass should be character and of length 1.")
        }

        if (!is.null(groupspan)) {
            if (transpose) {
                stop("Nesting/grouping not supported with transpose = TRUE.")
            }
            thead0 <- ifelse(is.na(labels$groups) | labels$groups=="", "", sprintf('<div>%s</div>', labels$groups))
            thead0 <- sprintf('<th colspan="%d" class="grouplabel">%s</th>', groupspan, thead0)
            thead0 <- c('<th class="grouplabel"></th>', thead0)
            thead0 <- paste("<tr>\n", paste(thead0, sep="", collapse="\n"), "\n</tr>\n", sep="", collapse="")
        } else {
            thead0 <- ""
        }

        if (is.null(rowlabelhead)) rowlabelhead <- ""

        if (!is.null(caption)) {
            caption <- sprintf('<caption>%s</caption>\n', caption)
        } else {
            caption <- ""
        }

        if (!is.null(footnote)) {
            footnote <- sprintf('<p>%s</p>\n', footnote)
            footnote <- paste0(footnote, collapse="\n")
            tfoot <- sprintf('<tfoot><tr><td colspan="%d" class="Rtable1-footnote">%s</td></tr></tfoot>\n', ncolumns + 1, footnote)
        } else {
            tfoot <- ""
        }

        x <- paste0(
            sprintf('<table%s>%s\n<thead>\n', topclass, caption),
            thead0,
            table.rows(thead, row.labels=rowlabelhead, th=T),
            tfoot,
            '</thead>\n<tbody>\n',
            paste(sapply(contents, table.rows), collapse=""),
            '</tbody>\n</table>\n')

        structure(x, class=c("table1", "html", "character"), html=TRUE, obj=obj)
    })
}

#' Convert a \code{table1} object to a \code{data.frame}.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param ... Ignored.
#' @return A \code{data.frame}.
#' @export
as.data.frame.table1 <- function(x, ...) {
    obj <- attr(x, "obj")
    with(obj, {
        rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
        z <- lapply(contents, function(y) {
            y <- as.data.frame(y, stringsAsFactors=F)
            y2 <- data.frame(x=paste0(c("", rep("\U{00A0}\U{00A0}", nrow(y) - 1)), rownames(y)), stringsAsFactors=F)
            y <- cbind(setNames(y2, rlh), y)
            y
        })
        df <- do.call(rbind, z)
        df <- rbind(c("", ifelse(is.na(headings[2,]), "", sprintf("(N=%s)", headings[2,]))), df)
        colnames(df) <- c(rlh, headings[1,])
        rownames(df) <- NULL
        noquote(df)
    })
}

#' Convert a \code{table1} object to \code{flextable}.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param ... Further options passed to \code{qflextable}.
#' @return A \code{flextable} object.
#' @note The \code{flextable} package needs to be installed for this to work.
#' @export
t1flex <- function(x, ...) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
        stop("This function requires package 'flextable'. Please install it and try again.", call.=F)
    }
    obj <- attr(x, "obj")
    with(obj, {
        rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
        i <- lapply(contents, function(y) {
            nrow(y)
        })
        i <- cumsum(c(1, i[-length(i)]))
        z <- lapply(contents, function(y) {
            y <- as.data.frame(y, stringsAsFactors=F)
            y2 <- data.frame(x=paste0(c("", rep("\U{00A0}\U{00A0}", nrow(y) - 1)), rownames(y)), stringsAsFactors=F)
            y <- cbind(setNames(y2, rlh), y)
            y
        })
        df <- do.call(rbind, z)
        colnames(df) <- c(rlh, sprintf(
            ifelse(is.na(headings[2,]), "%s", "%s\n(N=%s)"), headings[1,], headings[2,]))
        rownames(df) <- NULL
        out <- flextable::qflextable(df, ...)
        out <- flextable::align(out, j=2:(ncolumns+1), align="center", part="body")
        out <- flextable::align(out, j=2:(ncolumns+1), align="center", part="header")
        out <- flextable::bold(out, i=i, j=1)
        if (!is.null(groupspan)) {
            out <- flextable::add_header_row(out, values=c("", labels$groups), colwidths=c(1, groupspan))
            out <- flextable::align(out, i=1, align="center", part="header")
        }
        if (!is.null(caption)) {
            out <- flextable::set_caption(out, caption=caption)
        }
        if (!is.null(footnote)) {
            out <- flextable::footnote(out, i=1, j=1, value=as_paragraph(footnote), ref_symbols="")
        }
        out
    })
}

#' Convert a \code{table1} object to \code{kabelExtra}.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param booktabs Passed to \code{kbl} (default \code{TRUE}).
#' @param ... Other options passed to \code{kbl}.
#' @param format Passed to \code{kbl} (optional).
#' @return A \code{kabelExtra} object.
#' @note The \code{kableExtra} package needs to be installed for this to work.
#' @export
t1kable <- function(x, booktabs=TRUE, ..., format) {
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        stop("This function requires package 'kableExtra'. Please install it and try again.", call.=F)
    }
    if (missing(format) || is.null(format)) {
        format <- if (knitr::is_latex_output()) "latex" else "html"
    }
    obj <- attr(x, "obj")
    with(obj, {
        rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
        i <- lapply(contents, function(y) {
            nrow(y) - 1
        })
        z <- lapply(contents, function(y) {
            y <- as.data.frame(y[-1,], stringsAsFactors=F)
            y2 <- data.frame(x=rownames(y), stringsAsFactors=F)
            y <- cbind(setNames(y2, rlh), y)
            y
        })
        names(i) <- labels$variables
        df <- do.call(rbind, z)

        # Try to create a multiline header but does not work
        #if (format == "html") {
        #    cn <- c(rlh, sprintf(ifelse(is.na(headings[2,]), "%s", "%s<br/>(N=%s)"), headings[1,], headings[2,]))
        #} else {
        #    cn <- c(rlh, sprintf(ifelse(is.na(headings[2,]), "%s", "%s\n(N=%s)"), headings[1,], headings[2,]))
        #}
        #colnames(df) <- cn
        #if (format == "latex") {
        #    cn <- kableExtra::linebreak(cn, align="c")
        #}

        # Put the (N=xx) as first row of the table
        df <- rbind(c("", ifelse(is.na(headings[2,]), "", sprintf("(N=%s)", headings[2,]))), df)
        cn <- colnames(df) <- c(rlh, headings[1,])
        rownames(df) <- NULL
        out <- kableExtra::kbl(df, format=format, col.names=cn, row.names=F, escape=T, booktabs=booktabs, caption=caption, ...)
        out <- kableExtra::pack_rows(out, index=c(" "=1, i))
        #out <- kableExtra::pack_rows(out, index=i)
        if (!is.null(groupspan)) {
            groupspan <- setNames(groupspan, labels$groups)
            out <- kableExtra::add_header_above(out, c(" "=1, groupspan))
        }
        if (!is.null(footnote)) {
            out <- kableExtra::footnote(out, general=footnote, general_title="")
        }
        out
    })
}

#' Print \code{table1} object.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param ... Further arguments passed on to other \code{print} methods.
#' @return Returns \code{x} invisibly.
#' @details In an interactive context, the rendered table will be displayed in
#' a web browser. Otherwise, the HTML code will be printed as text.
#' @export
print.table1 <- function(x, ...) {
    if (interactive()) {
        x <- htmltools::HTML(x)
        default.style <- htmltools::htmlDependency("table1", "1.0",
            src=system.file(package="table1", "table1_defaults_1.0"),
            stylesheet="table1_defaults.css")
        x <- htmltools::div(class="Rtable1", default.style, x)
        x <- htmltools::browsable(x)
        print(x, ...) # Calls htmltools:::print.html(x, ...)
    } else {
        cat(x)
    }
    invisible(x)
}

#' Method for printing in a \code{knitr} context.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param ... Further arguments passed on to \code{knitr::knit_print}.
#' @details If the target is HTML, the usual internal formatting will be
#' applied; otherwise, fall back to a `data.frame`.
#' @importFrom knitr knit_print
#' @export
knit_print.table1 <- function(x, ...) {

    knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")

    knit_to_html  <- .isTRUE(knitr::is_html_output())
    knit_to_latex <- .isTRUE(knitr::is_latex_output())
    knit_to_docx  <- .isTRUE(knit_to == "docx")

    if (knit_to_html) {
        x <- htmltools::HTML(x)
        default.style <- htmltools::htmlDependency("table1", "1.0",
            src=system.file(package="table1", "table1_defaults_1.0"),
            stylesheet="table1_defaults.css")
        x <- htmltools::div(class="Rtable1", default.style, x)
        knitr::knit_print(x, ...)
    } else if (knit_to_latex) {
        # For latex, use kableExtra by default, if installed
        if (requireNamespace("kableExtra", quietly = TRUE)) {
            knitr::knit_print(t1kable(x), ...)
        } else {
            message("Get nicer `table1` LaTeX output by simply installing the `kableExtra` package")
            knitr::knit_print(knitr::kable(as.data.frame(x), booktabs=TRUE), ...)
        }
    } else if (knit_to_docx) {
        # For docx, use flextable by default, if installed
        if (requireNamespace("flextable", quietly = TRUE)) {
            knitr::knit_print(t1flex(x), ...)
        } else {
            message("Get nicer `table1` .docx output by simply installing the `flextable` package")
            knitr::knit_print(as.data.frame(x), ...)
        }
    } else {
        # If not fall back to printing as data.frame
        knitr::knit_print(as.data.frame(x), ...)
    }
}

#' @describeIn table1 The \code{formula} interface.
#' @export
#' @importFrom stats formula model.frame na.pass terms
#' @importFrom Formula Formula
table1.formula <- function(x, data, overall="Overall", rowlabelhead="", transpose=FALSE, droplevels=TRUE, topclass="Rtable1", footnote=NULL, caption=NULL, render=render.default, render.strat=render.strat.default, extra.col=NULL, extra.col.pos=NULL, ...) {
    f <- Formula(x)
    if (length(length(f)) != 2 || length(f)[2] < 1 || length(f)[2] > 2) {
        stop(paste0("Invalid formula: ", paste0(x, collapse="")))
    }
    if (length(f)[1] > 0) {
        warning("Unexpected LHS in formula ignored (table1 expects a 1-sided formula)")
    }
    if (length(f)[2] == 2) {
        f2 <- formula(f)
        f2[[2]][[3]] <- f[[2]][[2]]
        f2[[2]][[2]] <- f[[2]][[3]]
        f2 <- Formula(f2)

        dot <- !is.null(attr(terms(Formula(formula(f, rhs=2)), data=data), "Formula_without_dot"))
        dot2 <- !is.null(attr(terms(Formula(formula(f2, rhs=2)), data=data), "Formula_without_dot"))
        if (dot && dot2) {
            stop("Cannot have . in both parts of the formula")
        }

        if (dot || dot2) {
            f <- attr(terms(f, data=data, dot="sequential"), "Formula_without_dot")
            f2 <- attr(terms(f2, data=data, dot="sequential"), "Formula_without_dot")
        }

        m1 <- model.frame(formula(f2, rhs=2), data=data, na.action=na.pass)
        m2 <- model.frame(formula(f, rhs=2), data=data, na.action=na.pass)
        if (!all(sapply(m2, is.factor) | sapply(m2, is.character))) {
            warning("Terms to the right of '|' in formula 'x' define table columns and are expected to be factors with meaningful labels.")
        }
        m2 <- lapply(m2, as.factor)
        if (droplevels) {
            m2 <- lapply(m2, droplevels)
        }

        if (length(m2) > 1) {
            if (length(m2) > 2) {
                stop("Only 1 level of nesting is supported")
            }
            collabels <- tapply(m2[[2]], m2[[1]], levels, simplify=F)
            if (droplevels) {
                coln <- tapply(m2[[2]], m2[[1]], table, simplify=F)
                collabels <- mapply(function(x, y) x[y > 0], collabels, coln, SIMPLIFY=F)
            }
            grouplabel <- names(collabels)
            groupspan <- sapply(collabels, length)
            stratlabel <- unlist(collabels)
            if (!is.null(overall) && overall != FALSE) {
                grouplabel <- c(grouplabel, overall)
                groupspan <- c(groupspan, nlevels(m2[[2]]))
                stratlabel <- c(stratlabel, levels(m2[[2]]))
            }
        } else {
            stratlabel <- levels(m2[[1]])
            if (!is.null(overall) && overall != FALSE) {
                stratlabel <- c(stratlabel, overall)
            }
        }
    } else {
        m1 <- model.frame(formula(f, rhs=1), data=data, na.action=na.pass)
        m2 <- NULL
        if (is.null(overall) || (is.logical(overall) && overall == FALSE)) {
            stop("Table has no columns?!")
        }
        stratlabel <- overall 
    }
    for (i in 1:ncol(m1)) {
        if (!has.label(m1[[i]])) {
            label(m1[[i]]) <- names(m1)[i]
        }
    }

    if (!is.null(m2)) {
        strata <- split(m1, rev(m2))
        if (droplevels) {
            stratn <- sapply(strata, nrow)
            strata[stratn == 0] <- NULL
        }
        if (!is.null(overall) && overall != FALSE) {
            if (length(m2) > 1) {
                strata <- c(strata, split(m1, data.frame(m2[[2]], overall="overall")))
            } else {
                strata <- c(strata, list(overall=m1))
            }
        }
    } else {
        strata <- list(overall=m1)
    }

    labels <- list(
        strata=stratlabel,
        variables=lapply(m1, render.varlabel, transpose=transpose))
    names(labels$strata) <- names(strata)

    if (!is.null(m2) && length(m2) > 1) {
        labels$groups <- grouplabel
        table1.default(
            x             = strata,
            labels        = labels,
            groupspan     = groupspan,
            rowlabelhead  = rowlabelhead,
            transpose     = transpose,
            topclass      = topclass,
            footnote      = footnote,
            caption       = caption,
            render        = render,
            render.strat  = render.strat,
            extra.col     = extra.col,
            extra.col.pos = extra.col.pos, ...)
    } else {
        table1.default(
            x             = strata,
            labels        = labels,
            rowlabelhead  = rowlabelhead,
            transpose     = transpose,
            topclass      = topclass,
            footnote      = footnote,
            caption       = caption,
            render        = render,
            render.strat  = render.strat,
            extra.col     = extra.col,
            extra.col.pos = extra.col.pos, ...)
    }
}

#' Subset function that preserves column attributes.
#'
#' @param x An object to be subsetted (usually a \code{\link{data.frame}}).
#' @param ... Further arguments passed to \code{\link{subset}}.
#' @param droplevels If \code{TRUE} (the default), then unused factor levels are dropped (see \code{\link{droplevels}}).
#' @return An object similar to \code{x} containing just the selected elements.
#' In the case of a \code{\link{data.frame}}, attributes of columns (such as
#' \code{\link{label}} and \code{\link{units}}) are preserved.
#' @seealso
#' \code{\link{subset}}
#' \code{\link{droplevels}}
#' @keywords utilities
#' @export
subsetp <- function(x, ..., droplevels=TRUE) {
    y <- subset(x, ...)
    if (droplevels) {
        y <- droplevels(y)
    }
    if (is.data.frame(x)) {
        for (i in seq_along(x)) {
            a <- attributes(x[[i]])
            if (droplevels && is.factor(y[[i]])) {
                a$levels <- attributes(y[[i]])$levels
            }
            attributes(y[[i]]) <- a
        }
    }
    y
}

.isFALSE <- function (x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
.isTRUE <- function (x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && x
}

