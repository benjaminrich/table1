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
format_n <- function(x, ...) {
    args <- list(...)
    args <- args[!(names(args) %in% c("format"))]

    cx <- do.call(formatC,
        c(list(x=round(x), format="d"),
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
#'   \item \code{GSD}: the geometric standard deviation of the non-missing values if non-negative, or \code{NA}
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
#'   \item \code{NMISS}: the number of missing values
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
    if (!is.null(w <- weights.weighted(x))) {
        table <- function(x, ...) {
            t1 <- base::table(x, ...)
            t2 <- Hmisc::wtd.table(x, weights=w, type="table", na.rm=TRUE)
            t1[names(t2)] <- t2
            t1
        }
        mean <- function(...) Hmisc::wtd.mean(..., weights=w)
        sd <- function(...) sqrt(Hmisc::wtd.var(..., weights=w))
        quantile <- function(...) Hmisc::wtd.quantile(..., weights=w)
        qtypes <- eval(formals(Hmisc::wtd.quantile)$type)
        if (!(quantile.type %in% qtypes)) {
            quantile.type <- qtypes[1]
        }
    }
    if (is.logical(x)) {
        x <- factorp(x, levels=c(TRUE, FALSE), labels=c("Yes", "No"))
    }
    if (is.factor(x) || is.character(x)) {
        y <- table(x, useNA="no")
        nn <- names(y)
        nn[is.na(nn)] <- "Missing"
        names(y) <- nn
        lapply(y, function(z) list(
            FREQ=z,
            PCT=100*z/sum(table(rep(1, length(x)))),
            PCTnoNA=100*z/sum(y)
        ))
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
        p <- c(0.01, 0.025, 0.05, 0.1, 0.25, 1/3, 0.5, 2/3, 0.75, 0.9, 0.95, 0.975, 0.99) 
        q <- quantile(x, probs=p, na.rm=TRUE, type=quantile.type)
        names(q) <- c("0.01", "0.025", "0.05", "0.1", "0.25", "1/3", "0.5", "2/3", "0.75", "0.9", "0.95", "0.975", "0.99") 
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
            #MEDIAN=median(x, na.rm=TRUE),
            MEDIAN=q["0.5"],
            MIN=min(x, na.rm=TRUE),
            MAX=max(x, na.rm=TRUE),
            q01=q["0.01"],
            q02.5=q["0.025"],
            q05=q["0.05"],
            q10=q["0.1"],
            q25=q["0.25"],
            q50=q["0.5"],
            q75=q["0.75"],
            q90=q["0.9"],
            q95=q["0.95"],
            q97.5=q["0.975"],
            q99=q["0.99"],
            Q1=q["0.25"],
            Q2=q["0.5"],
            Q3=q["0.75"],
            IQR=q["0.75"] - q["0.25"],
            T1=q["1/3"],
            T2=q["2/3"])
    } else {
        stop(paste("Unrecognized variable type:", class(x)))
    }
}

#' Apply rounding to basic descriptive statistics.
#'
#' Not all statistics should be rounded in the same way, or at all. This
#' function will apply rounding selectively to a list of statistics as returned
#' by \code{\link{stats.default}}. In particular we don't round counts (N, NMISS and
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
        nr <- c("N", "FREQ", "NMISS")       # No rounding
        nr <- nr[nr %in% names(x)]
        nr <- nr[!is.na(x[nr])]
        r[nr] <- lapply(x[nr], format_n, ...)
        if (!round.median.min.max) {
            sr <- c("MEDIAN", "MIN", "MAX")  # Only add significant digits, don't remove any
            sr <- sr[sr %in% names(x)]
            r[sr] <- lapply(x[sr], mindig, digits=digits)
        }
        pr <- c("PCT", "PCTnoNA", "CV", "GCV")   # Percentages
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
#' @importFrom stats setNames
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
        nm <- names(render.missing)
        if (is.null(nm)) nm <- "Missing"
        render.missing.0 <- parse.abbrev.render.code(code=render.missing, ...)
        render.missing <- function(x, ...) {
            setNames(render.missing.0(is.na(x), ...)["Yes"], nm)
        }
    }
    if (length(x) == 0) {
        return(render.empty)
    }
    if (is.logical(x)) {
        x <- factorp(x, levels=c(TRUE, FALSE), labels=c("Yes", "No"))
    }
    if (is.factor(x) || is.character(x) || is.logical(x)) {
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
            i <- r != ""
            r[i] <- sprintf("%s: %s", names(r), r)[i]
        }
        r <- paste0(r, collapse="<br/>")
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
                res <- gsub(paste0("\\b", nm, "\\b"), paste0(ss[[nm]]), res, ignore.case=T)
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
    if (!is.null(w <- weights.weighted(x))) {
        missingx <- weighted.default(is.na(x), w)
    } else {
        missingx <- is.na(x)
    }
    with(stats.apply.rounding(stats.default(missingx, ...), ...)$Yes,
        c(Missing=sprintf("%s (%s%%)", FREQ, PCT)))
}

#' Render variable labels for default table1 output.
#'
#' Called from \code{\link{table1.formula}} by default to render variable
#' labels for displaying in the table. This is the default function, but it can
#' be overriden by a user-supplied function.
#'
#' @param x A vector, usually with the \code{\link{label}} and (if appropriate)
#' \code{\link{unit}} attributes.
#' @param ... Additional arguments.
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
render.varlabel<- function(x, ..., transpose=F) {
    html <- if (has.units(x) && transpose) {
        sprintf("%s<br/><span class='varunits'>(%s)</span>", label(x), units(x))
    } else if (has.units(x)) {
        sprintf("%s<span class='varunits'> (%s)</span>", label(x), units(x))
    } else {
        sprintf("%s", label(x))
    }

    nohtml <- if (has.units(x)) {
        sprintf("%s (%s)", label(x), units(x))
    } else {
        sprintf("%s", label(x))
    }

    setNames(html, nohtml)
    
}

#' Render strata labels for default table1 output.
#'
#' Called from \code{\link{table1}} to render the strata labels for display in
#' the table. This is the default function, but it can be overriden by a
#' user-supplied function.
#'
#' @param strata A named \code{list} of \code{data.frame}s.
#' @param ... Additional arguments.
#' @param transpose Logical indicating whether on not the table is transposed.
#'
#' @return A \code{character}, which may contain HTML markup.
#'
#' @examples
#' dat <- expand.grid(id=1:10, sex=c("Male", "Female"), treat=c("Treated", "Placebo"))
#' strata <- split(dat, dat$treat)
#' render.strat(strata)
#' @keywords internal
#' @export
render.strat <- function(strata, ..., transpose=F) {

    get_n <- function(x) {
        if (!is.null(w <- weights.weighted(x))) {
            sum(w)
        } else {
            nrow(x)
        }
    }

    stratn <- format_n(sapply(strata, get_n), ...)

    html <- ifelse(is.na(stratn), 
        names(strata),
        sprintf("%s<br/><span class='stratn'>(N=%s)</span>", names(strata), stratn)
    )

    nohtml <- ifelse(is.na(stratn),
        names(strata),
        sprintf("%s\n(N=%s)", names(strata), stratn)
    )

    setNames(html, nohtml)
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
    attr(x, "label", exact=TRUE)
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
    !is.null(attr(x, "label", exact=TRUE))
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
    attr(x, "units", exact=TRUE)
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
    !is.null(attr(x, "units", exact=TRUE))
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
#' is nested within the first. Stratification variables may not contain missing
#' values. The formula may contain a dot (".") to refer to "all variables in
#' \code{data} other than those that appear elsewhere in the formula". It is
#' legitimate to use functions inside the formula to create new variables.
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
#' \code{FALSE} to omit the column altogether. By default, the "Overall" column
#' appears at the right end of the table; to place it on the left instead use a
#' named \code{character} with the name "left", e.g. \code{c(left="Overall")}.
#' @param labels A list containing labels for variables, strata and groups (see Details).
#' @param groupspan A vector of integers specifying the number of strata to group together.
#' @param rowlabelhead A heading for the first column of the table, which contains the row labels.
#' @param droplevels Should empty factor levels be dropped?
#' @param transpose Logical. Should the table be transposed (i.e. strata as
#' rows and variables as columns)?  This flag is also passed to
#' \code{render.strat} and \code{render.varlabel}.
#' @param topclass A class attribute for the outermost (i.e. \code{<table>}) tag.
#' @param footnote A character string to be added as a footnote to the table.
#' Can also be a vector which results in multiple lines of footnotes.
#' The default \code{NULL} causes the footnote to be omitted.
#' @param caption A character string to be added as a caption to the table.
#' The default \code{NULL} causes the caption to be omitted.
#' @param render A function to render the table cells (see Details).
#' @param render.strat A function to render the stratum labels. The first
#' argument is a named list of \code{data.frame}s, and it should also accept
#' \code{...} arguments. The default is \code{\link{render.strat}}, but it can
#' be overriden with a user-supplied function.
#' @param render.varlabel A function to render the variable labels. The first
#' argument is a vector, and it should also accept \code{...} arguments. The
#' default is \code{\link{render.varlabel}}, but it can be overriden with a
#' user-supplied function.
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

#' @describeIn table1 The default interface, where \code{x} is a \code{list} of \code{data.frame}s.
#' @export
table1.default <- function(x, labels, groupspan=NULL, rowlabelhead="", transpose=FALSE, topclass="Rtable1", footnote=NULL, caption=NULL, render=render.default, render.strat=table1::render.strat, extra.col=NULL, extra.col.pos=NULL, ...) {
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

#' @importFrom methods formalArgs
.table1.internal <- function(x, labels, groupspan=NULL, rowlabelhead="", transpose=FALSE, topclass="Rtable1", footnote=NULL, caption=NULL, render=render.default, render.strat=table1::render.strat, extra.col=NULL, extra.col.pos=NULL, ...) {
    if (is.null(labels$strata)) {
        labels$strata <- names(x)
    }
    if (is.null(names(labels$strata))) {
        names(labels$strata) <- names(x)
    }
    names(x) <- labels$strata
    if (is.character(render)) {
        render <- parse.abbrev.render.code(code=render, ...)
    }

    # Convert any character columns to factor
    char2factor <- function(df) {
        df[,sapply(df, is.character)] <- lapply(df[,sapply(df, is.character), drop=FALSE], factorp)
        df
    }
    x <- lapply(x, char2factor)

    any.missing <- sapply(names(labels$variables), function(v) do.call(sum, lapply(x, function(s) sum(is.na(s[[v]])))) > 0)

    if ("..." %in% formalArgs(args(render.strat))) {
        headings <- render.strat(x, ..., transpose=transpose)
    } else {
        headings <- render.strat(x)
    }
    if (is.null(names(headings))) {
        names(headings) <- headings
    }

    if (transpose) {
        ncolumns <- length(labels$variables)
        if (ncolumns > 12) {
            warning(sprintf("Table has %d columns. Are you sure this is what you want?", ncolumns))
        }
        contents <- lapply(names(x), function(s) {
            do.call(cbind, lapply(names(labels$variables), function(v) {
                lvls <- unique(do.call(c, lapply(x, function(s) levels(s[[v]]))))
                z <- x[[s]][[v]]
                if (!is.null(lvls)) {
                    z <- factorp(z, levels=lvls)
                }
                y <- render(x=z, name=v, missing=any.missing[v], transpose=T, ...)
                y <- paste0(y, collapse="<br/>")
                names(y) <- if (!is.null(names(labels$variables[[v]]))) {
                    names(labels$variables[[v]])
                } else {
                    labels$variables[[v]]
                }

                y <- t(y)
                rownames(y) <- s
                y
            }))
        })
    } else {
        if (!is.null(extra.col)) {
            headings <- c(headings, names(extra.col))
            if (!is.null(groupspan)) {
                groupspan <- c(groupspan, rep(1, length(extra.col)))
                labels$groups <- c(labels$groups, rep("", length(extra.col)))
            }
            if (!is.null(extra.col.pos)) {
                if (!is.numeric(extra.col.pos) || any(extra.col.pos > length(headings))) {
                    stop("extra.col.pos should be a vector of column positions")
                }
                if (length(extra.col.pos) > length(extra.col)) {
                    stop("length of extra.col.pos should not exceed that of extra.col")
                }
                # Permute columns
                s1 <- seq(length(x) + 1, length.out=length(extra.col.pos))
                s2 <- setdiff(1:length(headings), s1)
                colpermute <- rep(0, length(headings))
                colpermute[extra.col.pos] <- s1
                colpermute[-extra.col.pos] <- s2
                headings <- headings[colpermute, drop=F]
                if (!is.null(groupspan)) {
                    grpermute <- rep(1:length(groupspan), times=groupspan)[colpermute]
                    grpermute <- grpermute[!duplicated(grpermute)]
                    groupspan <- groupspan[grpermute]
                    labels$groups <- labels$groups[grpermute]
                }
            }
        }
        ncolumns <- length(headings)
        if (ncolumns > 12) {
            warning(sprintf("Table has %d columns. Are you sure this is what you want?", ncolumns))
        }
        contents <- lapply(names(labels$variables), function(v) {
            lvls <- unique(do.call(c, lapply(x, function(s) levels(s[[v]]))))
            y <- do.call(cbind, lapply(x, function(s) {
                z <- s[[v]]
                if (!is.null(lvls)) {
                    z <- factorp(z, levels=lvls)
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
            rownames(y)[1] <- if (!is.null(names(labels$variables[[v]]))) {
                names(labels$variables[[v]])
            } else {
                labels$variables[[v]]
            }
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
        footnote     = footnote)

    update_html(structure("", obj=obj))
}

#' Update HTML.
#'
#' Used to (re-)generate the HTML code for a \code{link{table1}} object. In
#' most cases, this should not be used directly, unless you know what you are
#' doing.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @return An object of class "table1" which contains the updated HTML.
#' @export
update_html <- function(x) {
    obj <- attr(x, "obj", exact=TRUE)
    with(obj, {
        if (transpose) {
            thead <- sapply(labels$variables, function(x) {
                sprintf("<span class='varlabel'>%s</span>", x)
            })

            for (i in seq_along(contents)) {
                rownames(contents[[i]]) <- sprintf("<span class='stratlabel'>%s</span>", headings[i])
            }
        } else {
            thead <- sprintf("<span class='stratlabel'>%s</span>", headings)

            for (i in seq_along(contents)) {
                rownames(contents[[i]])[1] <- sprintf("<span class='varlabel'>%s</span>", labels$variables[[i]])
            }
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
    obj <- attr(x, "obj", exact=TRUE)
    with(obj, {
        rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") " " else rowlabelhead
        z <- lapply(contents, function(y) {
            y <- as.data.frame(y, stringsAsFactors=F)
            y2 <- data.frame(x=paste0(c("", rep("  ", nrow(y) - 1)), rownames(y)), stringsAsFactors=F)
            y <- cbind(setNames(y2, rlh), y)
            y
        })
        df <- do.call(rbind, z)

        # Use the non-HTML version of headings, if any
        if (!is.null(names(headings))) {
            headings <- names(headings)
        }

        # Put the (N=xx) as first row of the table
        headings <- strsplit(headings, "\n")
        headings.rows <- max(sapply(headings, length))
        headings <- lapply(headings, function(y) c(y, rep("", headings.rows - length(y))))
        headings <- do.call(cbind, headings)
        if (nrow(headings) > 1) {
            df <- rbind(c(rep("", nrow(headings)-1), headings[-1,,drop=F]), df)
        }
        colnames(df) <- c(rlh, headings[1,,drop=T])
        rownames(df) <- NULL
        noquote(df)
    })
}

#' Convert a \code{table1} object to \code{flextable}.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param tablefn Choose a function from the \code{flextable} package to use as
#' the basis for the table.
#' @param ... Further options passed to \code{tablefn}.
#' @return A \code{flextable} object.
#' @note The \code{flextable} package needs to be installed for this to work.
#' @importFrom utils getFromNamespace
#' @export
t1flex <- function(x, tablefn=c("qflextable", "flextable", "regulartable"), ...) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
        stop("This function requires package 'flextable'. Please install it and try again.", call.=F)
    }
    tablefn <- match.arg(tablefn)
    tablefn <- getFromNamespace(tablefn, "flextable")
    obj <- attr(x, "obj", exact=TRUE)
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

        # Use the non-HTML version of headings, if any
        if (!is.null(names(headings))) {
            headings <- names(headings)
        }

        header_df <- data.frame(
            labels = c(rlh, headings),
            keys   = LETTERS[1:ncol(df)]
        )

        if (!is.null(groupspan)) {
            zzz <- ncol(df) - sum(groupspan) - 1
            label2 <- c("", rep(labels$groups, times=groupspan), rep("", zzz))
            header_df <- cbind(data.frame(label2=label2), header_df)
        }

        colnames(df) <- header_df$keys
        rownames(df) <- NULL

        out <- tablefn(df, ...)
        out <- flextable::set_header_df(out,  header_df, key="keys")
        out <- flextable::merge_h(out, part = "header", i = 1)
        #out <- flextable::merge_v(out, part = "header", j = 1)
        #out <- flextable::theme_booktabs(out, bold_header = TRUE)
        out <- flextable::hline_top(out, border = officer::fp_border(width=1.5), part = "header")
        out <- flextable::hline_bottom(out, border = officer::fp_border(width=1.5), part = "header")
        out <- flextable::align(out, j=2:(ncolumns+1), align="center", part="body")
        out <- flextable::align(out, j=2:(ncolumns+1), align="center", part="header")
        out <- flextable::bold(out, part="header")
        out <- flextable::bold(out, i=i, j=1)

        if (!is.null(caption)) {
            out <- flextable::set_caption(out, caption=caption)
        }
        if (!is.null(footnote)) {
            out <- flextable::add_footer_lines(out, values=footnote)
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
#' @param align Passed to \code{kbl} (optional). The default is to left align
#' the labels (first column) and center everything else.
#' @param bold.headers Should the column headers be bolded?
#' @return A \code{kabelExtra} object.
#' @note The \code{kableExtra} package needs to be installed for this to work.
#' @export
t1kable <- function(x, booktabs=TRUE, ..., format, align, bold.headers=TRUE) {
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        stop("This function requires package 'kableExtra'. Please install it and try again.", call.=F)
    }
    if (missing(format) || is.null(format)) {
        format <- if (knitr::is_latex_output()) "latex" else "html"
    }
    default.align <- missing(align)

    obj <- attr(x, "obj", exact=TRUE)
    with(obj, {
        rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
        i <- lapply(contents, function(y) {
            if (all(y[1,, drop=T] == "")) {
                nrow(y) - 1
            } else {
                nrow(y)
            }
        })
        z <- lapply(contents, function(y) {
            if (all(y[1,, drop=T] == "")) {
                y <- as.data.frame(y[-1,, drop=F], stringsAsFactors=F)
                y2 <- data.frame(x=rownames(y), stringsAsFactors=F)
            } else {
                y2 <- data.frame(x="", stringsAsFactors=F)
            }
            y <- cbind(setNames(y2, rlh), y)
            y
        })
        names(i) <- labels$variables
        df <- do.call(rbind, z)

        if (format == "html") {
            cn <- c(rlh, headings)
        }
        # Create multiline header
        if (format == "latex") {
            # Use the non-HTML version of headings, if any
            if (!is.null(names(headings))) {
                headings <- names(headings)
            }
            cn <- c(rlh, headings)
            cn <- kableExtra::linebreak(cn, align="c")

            # Escape '%', but nothing else(?)
            df <- as.data.frame(gsub("%", "\\\\%", as.matrix(df)))
        }

        # Use the default alignment, first column left and others centered
        if (default.align) {
            align <- c("l", rep("c", ncol(df)-1))
        }

        rownames(df) <- NULL
        out <- kableExtra::kbl(df,
            format    = format,
            align     = align,
            col.names = cn,
            row.names = F,
            escape    = F,
            booktabs  = booktabs,
            caption   = caption,
            ...
        )
        out <- kableExtra::pack_rows(out, index=i)

        if (.isTRUE(bold.headers)) {
            out <- kableExtra::row_spec(out, 0, bold=TRUE) # Bold column headers
        }

        if (!is.null(groupspan)) {
            groupspan <- setNames(groupspan, labels$groups)
            zzz <- ncol(df) - sum(groupspan) - 1
            out <- kableExtra::add_header_above(out, bold=bold.headers,
                data.frame(c(" ", names(groupspan), rep(" ", zzz)), c(1, groupspan, rep(1, zzz))))
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
#' @exportS3Method knitr::knit_print
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
table1.formula <- function(x, data, overall="Overall", rowlabelhead="", transpose=FALSE, droplevels=TRUE, topclass="Rtable1", footnote=NULL, caption=NULL, render=render.default, render.strat=table1::render.strat, render.varlabel=table1::render.varlabel, extra.col=NULL, extra.col.pos=NULL, ...) {
    f <- Formula(x)
    if (length(length(f)) != 2 || length(f)[2] < 1 || length(f)[2] > 2) {
        stop(paste0("Invalid formula: ", paste0(x, collapse="")))
    }
    if (!is.null(overall) && length(overall) != 1) {
        stop("overall should have length 1 (unless NULL)")
    }
    if (length(f)[1] > 0) {
        warning("Unexpected LHS in formula ignored (table1 expects a 1-sided formula)")
    }
    if (length(f)[2] == 2) {
        f2 <- formula(f)
        f2[[2]][[3]] <- f[[2]][[2]]
        f2[[2]][[2]] <- f[[2]][[3]]
        f2 <- Formula(f2)

        dot <- !is.null(attr(terms(Formula(formula(f, rhs=2)), data=data), "Formula_without_dot", exact=TRUE))
        dot2 <- !is.null(attr(terms(Formula(formula(f2, rhs=2)), data=data), "Formula_without_dot", exact=TRUE))
        if (dot && dot2) {
            stop("Cannot have . in both parts of the formula")
        }

        if (dot || dot2) {
            f <- attr(terms(f, data=data, dot="sequential"), "Formula_without_dot", exact=TRUE)
            f2 <- attr(terms(f2, data=data, dot="sequential"), "Formula_without_dot", exact=TRUE)
        }

        m1 <- model.frame(formula(f2, rhs=2), data=data, na.action=na.pass)
        if (inherits(data, "weighted") && !is.null(w <- weights.weighted(data))) {
            m1 <- weighted.default(m1, w=w)
        }
        if (inherits(data, "indexed") && !is.null(i <- indices.indexed(data))) {
            m1 <- indexed.default(m1, i=i)
        }
        m2 <- model.frame(formula(f, rhs=2), data=data, na.action=na.pass)
        if (!all(sapply(m2, is.factor) | sapply(m2, is.character))) {
            warning("Terms to the right of '|' in formula 'x' define table columns and are expected to be factors with meaningful labels.")
        }
        if (any(sapply(m2, function(xx) any(is.na(xx))))) {
            stop("Stratification variable(s) should not contain missing values.")
        }
        m2 <- lapply(m2, function(x) factorp(x, levels = levels(x)))
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
                if (!is.null(names(overall)) && names(overall) == "left") {
                    grouplabel <- c(overall, grouplabel)
                } else {
                    grouplabel <- c(grouplabel, overall)
                }
                groupspan <- c(groupspan, nlevels(m2[[2]]))
                stratlabel <- c(stratlabel, levels(m2[[2]]))
            }
        } else {
            stratlabel <- levels(m2[[1]])
            if (!is.null(overall) && overall != FALSE) {
                if (!is.null(names(overall)) && names(overall) == "left") {
                    stratlabel <- c(overall, stratlabel)
                } else {
                    stratlabel <- c(stratlabel, overall)
                }
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
                overall.strata <- split(m1, data.frame(m2[[2]], overall="overall"))
            } else {
                overall.strata <- list(overall=m1)
            }
            if (!is.null(names(overall)) && names(overall) == "left") {
                strata <- c(overall.strata, strata)
            } else {
                strata <- c(strata, overall.strata)
            }
        }
    } else {
        strata <- list(overall=m1)
    }

    labels <- list(
        strata=stratlabel,
        variables=lapply(m1, render.varlabel, html=T, transpose=transpose))
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

#' Factor creation function that preserves column attributes.
#'
#' @param x An object to be converted to a a \code{\link{factor}}.
#' @param ... Further arguments passed to \code{\link{factor}}.
#' @return An \code{factor} with the attributes of \code{x} other than
#' \code{"class"} and \code{"levels"} preserved.
#' @seealso
#' \code{\link{factor}}
#' @keywords utilities
#' @export
factorp <- function(x, ...) {
    a <- attributes(x)
    y <- factor(x, ...)
    a <- c(a[setdiff(names(a), names(attributes(y)))], attributes(y))
    attributes(y) <- a
    if (inherits(x, "weighted")) {
        class(y) <- union("weighted", class(y))
    }
    if (inherits(x, "indexed")) {
        class(y) <- union("indexed", class(y))
    }
    y
}

.isFALSE <- function (x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
.isTRUE <- function (x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && x
}


#' A simple class for weighted data.
#'
#' @param x An atomic vector or \code{\link{data.frame}}.
#' @param w An numeric vector of weights.
#'
#' @return An object identical to \code{x} but with the additional class
#' \code{weighted} and a \code{weights} attribute.
#'
#' @examples
#' x <- c(3.7, 3.3, 3.5, 2.8)
#' y <- c(1, 2, 1, 2)
#' w <- c(5, 3, 4, 1)
#' 
#' z <- weighted(x=x, w=w)
#' weights(z)
#' weights(z[2:3])  # Weights are preserved
#' 
#' d <- weighted(
#'   data.frame(
#'     x=x,
#'     y=y
#'   ),
#'   w
#' )
#' 
#' weights(d)
#' weights(d[[1]])
#' weights(d$x)
#' weights(subset(d, y==1))
#' lapply(split(d, d$y), weights)
#' @export
weighted <- function(x, w) {
    UseMethod("weighted")
}

#' @export
weighted.data.frame <- function(x, w) {
    w <- eval(substitute(w), x, parent.frame())
    weighted.default(x, w)
}

#' @export
weighted.default <- function(x, w) {
    if (!is.null(dim(x))) {
        n <- dim(x)[1]
    } else {
        n <- length(x)
    }
    if (!is.numeric(w) || length(w) != n) {
        stop("w should be a numeric vector of the same length as x")
    }
    structure(x, class=union("weighted", class(x)), weights=w)
}

#' @exportS3Method stats::weights
weights.weighted <- function(object, ...) {
    attr(object, "weights", exact=TRUE)
}

#' @export
`[.weighted` <- function(x, i, ...) {
    w <- weights.weighted(x)[i]
    x <- NextMethod("[")
    weighted.default(x, w)
}

#' @export
`[[.weighted` <- function(x, i, ...) {
    if (is.data.frame(x)) {
        w <- weights.weighted(x)
        x <- NextMethod("[[")
        weighted.default(x, w)
    } else {
        NextMethod("[[")
    }
}

#' @export
`$.weighted` <- function(x, i, ...) {
    if (is.data.frame(x)) {
        w <- weights.weighted(x)
        x <- NextMethod("$")
        weighted.default(x, w)
    } else {
        NextMethod("$")
    }
}


#' A simple class for indexed data.
#'
#' @param x An atomic vector or \code{\link{data.frame}}.
#' @param i An optional vector of indices (dafaults to \code{1:n}, where
#' \code{n} is the number of elements in \code{x}).
#' @param ... Other arguments passed to methods.
#'
#' @return An object identical to \code{x} but with the additional class
#' \code{indexed} and a \code{indices} attribute.
#'
#' @examples
#' x <- c(3.7, 3.3, 3.5, 2.8)
#' y <- c(1, 2, 1, 2)
#' 
#' z <- indexed(x=x)
#' indices(z)
#' indices(z[2:3])  # Indices are preserved
#' 
#' d <- indexed(
#'   data.frame(
#'     x=x,
#'     y=y
#'   )
#' )
#' 
#' indices(d)
#' indices(d[[1]])
#' indices(d$x)
#' indices(subset(d, y==1))
#' lapply(split(d, d$y), indices)
#' @export
indexed <- function(x, i, ...) {
    UseMethod("indexed")
}

#' @export
indexed.data.frame <- function(x, i, ...) {
    if (missing(i)) {
        i <- 1:nrow(x)
    } else {
        i <- eval(substitute(i), x, parent.frame())
    }
    indexed.default(x, i)
}

#' @export
indexed.default <- function(x, i, ...) {
    if (!is.null(dim(x))) {
        n <- dim(x)[1]
    } else {
        n <- length(x)
    }
    if (missing(i)) {
        i <- 1:n
    }
    if (!is.numeric(i) || length(i) != n) {
        stop("i should be a numeric vector of the same length as x")
    }
    structure(x, class=union("indexed", class(x)), indices=i)
}

#' Extract indices from and indexed object.
#'
#' @param x An object of class \code{\link{indexed}}.
#' @param ... Other arguments passed to methods.
#' @return A vector of indices extracted from object \code{x}.
#' @export
indices <- function(x, ...) {
    UseMethod("indices")
}

#' @export
indices.indexed <- function(x, ...) {
    attr(x, "indices", exact=TRUE)
}

#' @export
`[.indexed` <- function(x, i, ...) {
    j <- indices.indexed(x)[i]
    x <- NextMethod("[")
    indexed.default(x, j)
}

#' @export
`[[.indexed` <- function(x, i, ...) {
    if (is.data.frame(x)) {
        j <- indices.indexed(x)
        x <- NextMethod("[[")
        indexed.default(x, j)
    } else {
        NextMethod("[[")
    }
}

#' @export
`$.indexed` <- function(x, i, ...) {
    if (is.data.frame(x)) {
        j <- indices.indexed(x)
        x <- NextMethod("$")
        indexed.default(x, j)
    } else {
        NextMethod("$")
    }
}

