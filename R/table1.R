#' Round numbers to specified significant digits with 0-padding.
#'
#' A utility function to round numbers to a specified number of
#' significant digits.  Zeros are kept if they are significant.
#'
#' @param x A numeric vector.
#' @param digits An interger specifying the number of significant digits to keep.
#' @param round.integers Should rounding be limited to digits to the right of
#' the decimal point?
#' @param round5up Should numbers with 5 as the last digit always be rounded
#' up? The standard R approach is "go to the even digit" (IEC 60559 standard,
#' see \code{\link{round}}), while some other softwares (e.g. SAS, Excel)
#' always round up.
#'
#' @return A character vector containing the rounded numbers.
#'
#' @seealso
#' \code{\link{signif}}
#' \code{\link{formatC}}
#' \code{\link{prettyNum}}
#' \code{\link{format}}
#'
#' @examples
#' x <- c(0.9001, 12345, 1.2, 1., 0.1)
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
signif_pad <- function(x, digits=3, round.integers=TRUE, round5up=TRUE) {
    eps <- ifelse(round5up, x*(10^(-(digits + 3))), 0)
    if (round.integers) {
        cx <- as.character(signif(x+eps, digits))  # Character representation of x
    } else {
        cx <- ifelse(x >= 10^digits, as.character(round(x)), as.character(signif(x+eps, digits)))  # Character representation of x
    }

    cx[is.na(x)] <- "0"                    # Put in a dummy value for missing x

    d <- gsub("[^0-9]", "", cx)            # The 'digits' of x
    d <- sub("^0*", "", d)                 # Remove any leading zeros
    nd <- nchar(d)                         # How many actual digits
    nd[cx=="0"] <- 1                       # Special case "0"
    npad <- pmax(0, digits - nd)           # How many digits are missing
    pad <- sapply(npad, function(n) paste(rep("0", times=n), collapse=""))

    has.dec <- grepl("\\.", cx)                      #  Does cx already contain a decimal point?
    add.dec <- ifelse(!has.dec & npad > 0, ".", "")  #  If not, and if padding is required, we need to add a decimal point first

    ifelse(is.na(x), NA, paste(cx, add.dec, pad, sep=""))
}

#' Compute some basic descriptive statistics.
#'
#' Values of type \code{factor}, \code{character} and \code{logical} are
#' treated as categorical. For logicals, the two categories are given the
#' labels `Yes` for \code{TRUE}, and `No` for \code{FALSE}.  Factor levels with
#' zero counts are retained.
#'
#' @param x A vector or numeric, factor, character or logical values.
#' @param useNA For categorical \code{x}, should missing values be treated as a category?
#'
#' @return A list. For numeric \code{x}, the list contains the numeric elements:
#' \itemize{
#'   \item \code{N}: the number of non-missing values
#'   \item \code{NMISS}: the number of missing values
#'   \item \code{MEAN}: the mean of the non-missing values
#'   \item \code{SD}: the standard deviation of the non-missing values
#'   \item \code{MIN}: the minimum of the non-missing values
#'   \item \code{MEDIAN}: the median of the non-missing values
#'   \item \code{MAX}: the maximum of the non-missing values
#'   \item \code{Q25}: the lower quartile of the non-missing values
#'   \item \code{Q75}: the upper quartile of the non-missing values
#'   \item \code{IQR}: the inter-quartile range of the non-missing values
#'   \item \code{CV}: the percent coefficient of variation of the non-missing values
#'   \item \code{GMEAN}: the geometric mean of the non-missing values if non-negative, or \code{NA}
#'   \item \code{GCV}: the percent geometric coefficient of variation of the non-missing values if non-negative, or \code{NA}
#' }
#' If \code{x} is categorical (i.e. factor, character or logical), the list
#' contains a sublist for each category, where each sublist contains the
#' numeric elements:
#' \itemize{
#'   \item \code{FREQ}: the frequency count
#'   \item \code{PCT}: the percent relative frequency
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
stats.default <- function(x, useNA=NULL) {
    if (is.logical(x)) {
        x <- factor(1-x, levels=c(0, 1), labels=c("Yes", "No"))
    }
    if (is.factor(x) || is.character(x)) {
        y <- table(x, useNA=useNA)
        nn <- names(y)
        nn[is.na(nn)] <- "Missing"
        names(y) <- nn
        lapply(y, function(z) list(FREQ=z, PCT=100*z/length(x)))
    } else if (is.numeric(x)) {
        list(
            N=sum(!is.na(x)),
            NMISS=sum(is.na(x)),
            MEAN=mean(x, na.rm=TRUE),
            SD=sd(x, na.rm=TRUE),
            MIN=min(x, na.rm=TRUE),
            MEDIAN=median(x, na.rm=TRUE),
            MAX=max(x, na.rm=TRUE),
            Q25=quantile(x, probs=0.25, na.rm=TRUE),
            Q75=quantile(x, probs=0.75, na.rm=TRUE),
            IQR=IQR(x, na.rm=TRUE),
            CV=100*sd(x, na.rm=TRUE)/abs(mean(x, na.rm=TRUE)),
            GMEAN=if (any(na.omit(x) <= 0)) NA else exp(mean(log(x), na.rm=TRUE)),
            GCV=if (any(na.omit(x) <= 0)) NA else 100*sqrt(exp(sd(log(x), na.rm=TRUE)^2) -1))
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
#' @param digits An interger specifying the number of significant digits to keep.
#' @param digits.pct An interger specifying the number of digits after the
#' decimal place for percentages.
#' @param round.median.min.max Should rounding applied to median, min and max?
#' @param round.integers Should rounding be limited to digits to the right of
#' the decimal point?
#' @param round5up Should numbers with 5 as the last digit always be rounded
#' up? The standard R approach is "go to the even digit" (IEC 60559 standard,
#' see \code{\link{round}}), while some other softwares (e.g. SAS, Excel)
#' always round up.
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
stats.apply.rounding <- function(x, digits=3, digits.pct=1, round.median.min.max=TRUE, round.integers=TRUE, round5up=TRUE, ...) {
    mindig <- function(x, digits) {
        cx <- format(x)
        ndig <- nchar(gsub("\\D", "", cx))
        ifelse(ndig > digits, cx, signif_pad(x, digits=digits,
                round.integers=round.integers, round5up=round5up))
    }
    if (!is.list(x)) {
        stop("Expecting a list")
    }
    if (is.list(x[[1]])) {
        # Apply recursively
        lapply(x, stats.apply.rounding, digits=digits, digits.pct=digits.pct,
            round.integers=round.integers, round5up=round5up, ...)
    } else {
        cx <- lapply(x, format)
        r <- lapply(x, signif_pad, digits=digits,
                round.integers=round.integers, round5up=round5up)
        nr <- c("N", "FREQ")       # No rounding
        nr <- nr[nr %in% names(x)]
        r[nr] <- cx[nr]
        if (!round.median.min.max) {
            sr <- c("MEDIAN", "MIN", "MAX")  # Only add significant digits, don't remove any
            sr <- sr[sr %in% names(x)]
            r[sr] <- lapply(x[sr], mindig, digits=digits)
        }
        pr <- c("PCT", "CV", "GCV")   # Percentages
        pr <- pr[pr %in% names(x)]
        r[pr] <- lapply(x[pr], formatC, digits=digits.pct, format="f")
        r
    }
}

#' Render values for table output.
#'
#' Called from \code{\link{table1}} by default to render values for
#' displaying in the table. This function forwards the call to separate
#' functions for rendering continuous, categorical and missing values.
#' The idea is that each of these functions can be overriden to customize
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
#' \code{\link{parse.abbrev.render.code}}.
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
    if (is.character(render.missing)) {
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
    if (missing) {
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
#' MAX, IQR, CV, GMEAN, GCV, FREQ and PCT are substituted for their respective
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
parse.abbrev.render.code <-
function(code, ...) {
    codestr <- code
    if (is.null(names(codestr)) && length(codestr) > 1) {
        names(codestr) <- codestr
    }
    names(codestr)[names(codestr) == "."] <- codestr[names(codestr) == "."]
    function(x, ...) {
        s <- stats.apply.rounding(stats.default(x), ...)
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
    with(stats.apply.rounding(stats.default(x), ...), c("",
        "Mean (SD)"=sprintf("%s (%s)", MEAN, SD),
        "Median [Min, Max]" = sprintf("%s [%s, %s]", MEDIAN, MIN, MAX)))
}

#' Render categorical values for table output.
#'
#' Called from \code{\link{table1}} by default to render categorical (i.e.
#' \code{factor}, \code{character} or \code{logical}) values for displaying in the table.
#'
#' @param x A vector of type \code{factor}, \code{character} or \code{logical}.
#' @param ... Further arguments, passed to \code{\link{stats.apply.rounding}}.
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
render.categorical.default <- function(x, ...) {
    c("", sapply(stats.apply.rounding(stats.default(x), ...), function(y) with(y,
        sprintf("%s (%s%%)", FREQ, PCT))))
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
    with(stats.apply.rounding(stats.default(is.na(x)), ...)$Yes,
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
    if (has.units(x) && transpose) {
        sprintf("<span class='varlabel'>%s<br/><span class='varunits'>(%s)</span></span>", label(x), units(x))
    } else if (has.units(x)) {
        sprintf("<span class='varlabel'>%s<span class='varunits'> (%s)</span></span>", label(x), units(x))
    } else {
        sprintf("<span class='varlabel'>%s</span>", label(x))
    }
}

#' Render stata labels for table output.
#'
#' Called from \code{\link{table1.formula}} to render strata labels
#' for displaying in the table.
#'
#' @param label A \code{character} vector containing the labels.
#' @param n A \code{numeric} vector containing the sizes.
#'
#' @return A \code{character}, which may contain HTML markup.
#' @keywords internal
render.strat <- function(label, n, transpose=F) {
    sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(n=%d)</span></span>", label, n)
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
#' x <- matrix(signif_pad(exp(rnorm(100, 1, 1))), 5, 5)
#' table.data(x)
#' cat(table.rows(x, NULL))
#' cat(table.rows(x, LETTERS[1:10]))
#' cat(table.rows(LETTERS[1:3], "Headings", th=TRUE))
#' @keywords utilities
#' @export
table.rows <- function(x, row.labels=rownames(x), th=FALSE, class=NULL, rowlabelclass="rowlabel", firstrowclass="firstrow", lastrowclass="lastrow", ...) {
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
#' There are two interfaces, the default, which typically takes a list of
#' \code{data.frame}s for \code{x}, and the formula interface. The formula
#' interface is less flexible, but simpler to use and designed to handle the
#' most common use cases. It is important to use factors appropriately for
#' categorical variables (i.e. have the levels labelled properly and in the
#' desired order). The contents of the table can be customized by providing
#' user-defined `renderer' functions. Customization of the table appearance is
#' deliberately not attempted, as this is best accomplished with CSS. To
#' facilitate this, some tags (such as row labels) are given specific classes
#' for easy CSS selection.
#' 
#' @details For the default version, is is expected that \code{x} is a named,
#' list of \code{data.frame}s, one for each stratum, with names corresponding to
#' strata labels.
#'
#' @param x An object, typically a \code{formula} or list of \code{data.frame}s.
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
#' @param render A function to render the table cells (see Details).
#' @param standalone Should a standalone HTML document be generated and
#' immediately displayed? Otherwise an HTML fragment is printed to
#' \code{\link{stdout}}.
#' @param ... Further arguments, passed to \code{render}.
#'
#' @return None (invisible \code{NULL}). Called for its side effects.
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
table1.default <- function(x, labels, groupspan=NULL, rowlabelhead="", transpose=FALSE, topclass="Rtable1", render=render.default, standalone=!isTRUE(getOption("knitr.in.progress")), ...) {
    if (standalone) {
        viewer <- getOption("viewer")
        if (is.null(viewer)) {
            viewer <- utils::browseURL
        }
        dir <- tempfile()
        dir.create(dir)
        html.file <- file.path(dir, "table1.html")
        cat(file=html.file, append=TRUE, html.standalone.head)
        utils::capture.output(file=html.file, append=TRUE,
            .table1.internal(x = x,
                labels       = labels,
                groupspan    = groupspan,
                rowlabelhead = rowlabelhead,
                transpose    = transpose,
                topclass     = topclass,
                render       = render,...))
        cat(file=html.file, append=TRUE, html.standalone.foot)
        viewer(html.file)
    } else {
        .table1.internal(x = x,
            labels       = labels,
            groupspan    = groupspan,
            rowlabelhead = rowlabelhead,
            transpose    = transpose,
            topclass     = topclass,
            render       = render, ...)
    }
}

html.standalone.head <- '
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta charset="utf-8">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="table1" />
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Table1 Output</title>
<style type="text/css">
table.Rtable1 {
    font-family: "Arial", Arial, sans-serif;
    font-size: 10pt;
    border-collapse: collapse;
    padding: 0px;
    margin: 0px;
    margin-bottom: 10pt;
}
.Rtable1 th, .Rtable1 td {
    border: 0;
    text-align: center;
    padding: 0.5ex 1.5ex;
    margin: 0px;
}
.Rtable1 thead>tr:first-child>th {
    border-top: 2pt solid black;
}
.Rtable1 thead>tr:last-child>th {
    border-bottom: 1pt solid black;
}
.Rtable1 tbody>tr:last-child>td {
    border-bottom: 2pt solid black;
}
.Rtable1 th.grouplabel {
    padding-left: 0;
    padding-right: 0;
}
.Rtable1 th.grouplabel>div {
    margin-left: 1.5ex;
    margin-right: 1.5ex;
    border-bottom: 1pt solid black;
}
.Rtable1 th.grouplabel:last-child>div {
    margin-right: 0;
}
.Rtable1 .rowlabel {
    text-align: left;
    padding-left: 2.5ex;
}
.Rtable1 .firstrow.rowlabel {
    padding-left: 0.5ex;
    font-weight: bold;
}
.Rtable1.zebra tbody tr:nth-child(odd) {
    background-color: #eee;
}
.Rtable1.grid th, .Rtable1.grid td {
    border: 1pt solid black;
}
table.grayheader th {
    background-color: #ccc;
}
.Rtable1.listing .stratn {
    display: none;
}
.Rtable1.times {
    font-family: "Times New Roman", Times, serif;
}
</style>
</head>
<body>
'

html.standalone.foot <- '
<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
  (function () {
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src  = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
    document.getElementsByTagName("head")[0].appendChild(script);
  })();
</script>
</body>
</html>
'

.table1.internal <- function(x, labels, groupspan=NULL, rowlabelhead="", transpose=FALSE, topclass="Rtable1", render=render.default, ...) {
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

    any.missing <- sapply(names(labels$variables), function(v) do.call(sum, lapply(x, function(s) sum(is.na(s[[v]])))) > 0)

    if (transpose) {
        ncolumns <- length(labels$variables)
        if (ncolumns > 12) {
            warning(sprintf("Table has %d columns. Are you sure this is what you want?", ncolumns))
        }
        thead <- t(unlist(labels$variables))
        tbody <- lapply(names(x), function(s) {
            do.call(cbind, lapply(names(labels$variables), function(v) {
                y <- render(x=x[[s]][[v]], name=v, missing=any.missing[v], transpose=T, ...)
                y <- paste0(y, collapse="<br/>")
                names(y) <- labels$variables[[v]]
                y <- t(y)
                rownames(y) <- render.strat(labels$strata[s], nrow(x[[s]]))
                y }))})
    } else {
        ncolumns <- length(x)
        if (ncolumns > 12) {
            warning(sprintf("Table has %d columns. Are you sure this is what you want?", ncolumns))
        }
        thead <- t(render.strat(labels$strata[names(x)], sapply(x, nrow)))
        tbody <- lapply(names(labels$variables), function(v) {
            y <- do.call(cbind, lapply(x, function(s) render(x=s[[v]], name=v, missing=any.missing[v], ...)))
            rownames(y) <- paste(rownames(y), sep="")
            rownames(y)[1] <- labels$variables[[v]]
            y })
    }

    if (is.null(topclass) || topclass=="") {
        cat('<table>\n<thead>\n')
    } else if (is.character(topclass) && length(topclass)==1) {
        cat(sprintf('<table class="%s">\n<thead>\n', topclass))
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
        cat(thead0)
    }

    if (is.null(rowlabelhead)) rowlabelhead <- ""
    cat(table.rows(thead, row.labels=rowlabelhead, th=T))

    cat('</thead>\n<tbody>\n')

    cat(paste(sapply(tbody, table.rows), collapse=""))

    cat('</tbody>\n</table>\n')

}

#' @describeIn table1 The \code{formula} interface.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula
table1.formula <- function(x, data, overall="Overall", rowlabelhead="", transpose=FALSE, droplevels=TRUE, topclass="Rtable1", render=render.default, standalone=!isTRUE(getOption("knitr.in.progress")), ...) {
    f <- Formula(x)
    m1 <- model.frame(formula(f, rhs=1), data=data, na.action=na.pass)
    for (i in 1:ncol(m1)) {
        if (!has.label(m1[[i]])) {
            label(m1[[i]]) <- names(m1)[i]
        }
    }
    if (length(f)[2] > 1) {
        m2 <- model.frame(formula(f, rhs=2), data=data, na.action=na.pass)
        if (!all(sapply(m2, is.factor) | sapply(m2, is.character))) {
            warning("Terms to the right of '|' in formula 'x' define table columns and are expected to be factors with meaningful labels.")
        }
        m2 <- lapply(m2, as.factor)
        if (droplevels) {
            m2 <- lapply(m2, droplevels)
        }
        colspan <- c(cumprod(sapply(m2, nlevels)[-1]), 1)
        collabel <- lapply(m2, levels)

        if (length(colspan) > 1) {
            if (length(colspan) > 2) {
                stop("Only 1 level of nesting is supported")
            }
            grouplabel <- collabel[[1]]
            if (!is.null(overall) && overall != FALSE) {
                grouplabel <- c(grouplabel, overall)
            }
            groupspan <- rep(colspan[1], length(grouplabel))
            stratlabel <- rep(collabel[[2]], length.out=groupspan[1]*length(grouplabel))
        } else {
            stratlabel <- collabel[[1]]
            if (!is.null(overall) && overall != FALSE) {
                stratlabel <- c(stratlabel, overall)
            }
        }
    } else {
        m2 <- NULL
        stratlabel <- overall 
    }

    if (!is.null(m2)) {
        strata <- split(m1, rev(m2))
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
        table1.default(x=strata,
            labels=labels,
            groupspan=groupspan,
            rowlabelhead=rowlabelhead,
            transpose=transpose,
            topclass=topclass,
            render=render,
            standalone=standalone, ...)
    } else {
        table1.default(x=strata,
            labels=labels,
            rowlabelhead=rowlabelhead,
            transpose=transpose,
            topclass=topclass,
            render=render,
            standalone=standalone, ...)
    }
}

