#' Round numbers to specified significant digits with 0-padding.
#'
#' A utility function to round numbers to a specified number of
#' significant digits.  Zeros are kept if they are significant.
#'
#' @param x A numeric vector.
#' @param digits An interger specifying the number of significant digits to keep.
#' @param round.integers Should rounding be limited to digits to the right of the decimal point?
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
signif_pad <- function(x, digits=3, round.integers=FALSE) {
    if (round.integers) {
        cx <- as.character(signif(x, digits))  # Character representation of x
    } else {
        cx <- ifelse(x >= 10^digits, as.character(round(x)), as.character(signif(x, digits)))  # Character representation of x
    }

    d <- gsub("[^0-9]", "", cx)            # The 'digits' of x
    d <- sub("^0*", "", d)                 # Remove any leading zeros
    nd <- nchar(d)                         # How many actual digits
    npad <- pmax(0, digits - nd)           # How many digits are missing
    pad <- sapply(npad, function(n) paste(rep("0", times=n), collapse=""))

    has.dec <- grepl("\\.", cx)                      #  Does cx already contain a decimal point?
    add.dec <- ifelse(!has.dec & npad > 0, ".", "")  #  If not, and if padding is required, we need to add a decimal point first

    paste(cx, add.dec, pad, sep="")
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
#' @importFrom stats sd median IQR na.omit
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
            IQR=IQR(x, na.rm=TRUE),
            CV=100*sd(x, na.rm=TRUE)/abs(mean(x, na.rm=TRUE)),
            GMEAN=if (any(na.omit(x) <= 0)) NA else exp(mean(log(x), na.rm=TRUE)),
            GCV=if (any(na.omit(x) <= 0)) NA else 100*sqrt(exp(sd(log(x), na.rm=TRUE)^2) -1))
    } else {
        stop(paste("Unrecognized variable type:", class(x)))
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
#' @param missing Should missing values be included?
#' @param render.continuous A function to render continuous (i.e. \code{numeric}) values.
#' @param render.categorical A function to render categorical
#'        (i.e. \code{factor}, \code{character} or \code{logical}) values.
#' @param render.missing A function to render missing (i.e. \code{NA}) values.
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
render.default <- function(x, missing=any(is.na(x)),
                           render.continuous="render.continuous",
                           render.categorical="render.categorical",
                           render.missing="render.missing") {
    if (is.logical(x)) {
        x <- factor(1-x, levels=c(0, 1), labels=c("Yes", "No"))
    }
    if (is.factor(x) || is.character(x)) {
        r <- do.call(render.categorical, list(x=x))
    } else if (is.numeric(x)) {
        r <- do.call(render.continuous, list(x=x))
    } else {
        stop(paste("Unrecognized variable type:", class(x)))
    }
    if (missing) {
        r <- c(r, do.call(render.missing, list(x=x)))
    }
    r
}

#' Render continuous values for table output.
#'
#' Called from \code{\link{table1}} by default to render continuous (i.e.
#' \code{numeric}) values for displaying in the table.
#'
#' @param x A numeric vector.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table. However, the first names should be empty as it
#' will be replaced by the name of the variable. Empty strings are allowed and
#' result in empty table cells.
#'
#' @examples
#' x <- exp(rnorm(100, 1, 1))
#' render.continuous(x)
#' 
#' @keywords utilities
#' @export
render.continuous <- function(x) {
    with(lapply(stats.default(x), signif_pad), c("",
        "Mean (SD)"=sprintf("%s (%s)", MEAN, SD),
        "Median [Min, Max]" = sprintf("%s [%s, %s]", MEDIAN, MIN, MAX)))
}

#' Render categorical values for table output.
#'
#' Called from \code{\link{table1}} by default to render categorical (i.e.
#' \code{factor}, \code{character} or \code{logical}) values for displaying in the table.
#'
#' @param x A vector of type \code{factor}, \code{character} or \code{logical}.
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
#' render.categorical(y)
#' @keywords utilities
#' @export
render.categorical <- function(x) {
    c("", sapply(stats.default(x), function(y) with(y,
        sprintf("%d (%0.1f%%)", FREQ, PCT))))
}

#' Render missing values for table output.
#'
#' Called from \code{\link{table1}} by default to render missing (i.e.
#' \code{NA}) values for displaying in the table.
#'
#' @param x A vector.
#'
#' @return A \code{character} vector. Each element is to be displayed in a
#' separate cell in the table. The \code{\link{names}} of the vector are the
#' labels to use in the table. Empty strings are allowed and
#' result in empty table cells.
#'
#' @examples
#' y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
#' y[1:10] <- NA
#' render.missing(y)
#' @keywords utilities
#' @export
render.missing <- function(x) {
    with(stats.default(is.na(x))$Yes,
        c(Missing=sprintf("%d (%0.1f%%)", FREQ, PCT)))
}

#' Render variable labels for table output.
#'
#' Called from \code{\link{table1.formula}} by default to render variable labels
#' for displaying in the table.
#'
#' @param x A vector, usually with the \code{\link{label}} and (if appropriate)
#' \code{\link{unit}} attributes.
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
render.varlabel <- function(x) {
    if (has.units(x)) {
        sprintf("%s (%s)", label(x), units(x))
    } else {
        sprintf("%s", label(x))
    }
}

#' Convert to HTML table rows.
#'
#' Many functions exist in R to generate HTML tables.  This function is useful
#' for generating an HTML table fragment (rather than a whole table), which can
#' be embedded in a larger table structure. Row labels, if specified, have a
#' special HTML \code{class} \code{rowlabel}, which can be useful as a hook to
#' customize their appearance using CSS; the first row label has an additional
#' \code{class} \code{firstrowlabel}.
#'
#' @param x A vector or table-like structure (e.g. a \code{\link{data.frame}} or \code{\link{matrix}}).
#' @param row.labels Values for the first column, typically used to label the row, or \code{NULL} to omit.
#' @param th A logical. Should \code{th} tags be used rather than \code{td}? 
#'
#' @return A \code{character} which contains an HTML table fragment.
#'
#' @examples
#' x <- matrix(signif_pad(exp(rnorm(100, 1, 1))), 10, 10)
#' table.rows(x, NULL)
#' table.rows(x, LETTERS[1:10])
#' table.rows(LETTERS[1:3], "Headings", th=TRUE)
#' @keywords utilities
#' @export
table.rows <- function(x, row.labels=rownames(x), th=FALSE) {
    tag <- ifelse(th, "th", "td")
    rowlabelclass <- rep("rowlabel", length(row.labels))
    rowlabelclass[1] <- paste0(rowlabelclass[1], " firstrowlabel")
    if (is.data.frame(x)) {
        x <- sapply(x, as.character)
    } else if (is.null(dim(x))) {
        x <- matrix(as.character(x), nrow=1)
    }
    td <- paste("<", tag, ">", x, "</", tag, ">", sep="")
    dim(td) <- dim(x)
    if (!is.null(row.labels)) {
        td <- cbind(paste("<", tag, " class=\"", rowlabelclass, "\">", row.labels, "</", tag, ">", sep=""), td)
    }
    tr <- paste("<tr>\n", apply(td, 1, paste, collapse="\n"), "\n</tr>\n", sep="")
    paste(tr, sep="", collapse="")
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
#' There are two interfaces, the default, which typically takes a
#' \code{data.frame} for \code{x}, and the formula interface. The formula
#' interface is less flexible, but simpler to use and designed to handle the
#' most common use cases. It is important to use factors appropriately for
#' categorical variables (i.e. have the levels labelled properly and in the
#' desired order). The contents of the table can be customized by providing
#' user-defined `renderer' functions. Customization of the table appearance is
#' deliberately not attempted, as this is best accomplished with CSS. To
#' facilitate this, some tags (such as row labels) are given specific classes
#' for easy CSS selection.
#'
#' @param x An object, typically a \code{formula} or \code{data.frame}.
#' @param data For the formula interface, a \code{data.frame} from which the
#'        variables in \code{x} should be taken.
#' @param overall Should a column for the total population be included and if so, what label
#'        should the column have? Specify \code{NULL} or \code{FALSE} to omit the column.
#' @param labels A list containing labels for variables, strata and groups (see Details).
#' @param groupspan A vector of integers specifying the number of strata to group together.
#' @param rowlabelhead A heading for the first column of the table, which contains the row labels.
#' @param topclass A class attribute for the outermost (i.e. \code{<table>}) tag.
#' @param render A function to render the table cells (see Details).
#' @param ... Further arguments, passed to \code{render}.
#'
#' @return None (invisible \code{NULL}). Called for its side effects.
#'
#' @examples
#'
#' dat <- expand.grid(id=1:10, sex=c("Male", "Female"), treat=c("Treated", "Placebo"))
#' dat$age <- runif(nrow(dat), 10, 50)
#' dat$age[3] <- NA
#' 
#' label(dat$sex) <- "Sex"
#' label(dat$age) <- "Age"
#' label(dat$treat) <- "Treatment Group"
#' 
#' units(dat$age) <- "years"
#' 
#' # One level of stratification
#' table1(~ sex + age | treat, data=dat)
#' 
#' # Two levels of stratification (nesting)
#' table1(~ age | treat*sex, data=dat)
#' 
#' # Switch the order or nesting
#' table1(~ age | sex*treat, data=dat)
#' 
#' # No stratification
#' table1(~ treat + sex + age, data=dat)
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
#'                    age=render.varlabel(dat$age)),
#'     groups=list("", "Treated", ""))
#' 
#' my.render.cont <- function(x) {
#'     with(stats.default(x), 
#'         sprintf("%0.2f (%0.1f)", MEAN, SD))
#' }
#' 
#' table1(strata, labels, groupspan=c(1, 3, 1), render.continuous=my.render.cont)
#' 
#' @keywords utilities
#' @export
table1 <- function(x, ...) {
    UseMethod("table1")
}

#' @describeIn table1 The default interface, where \code{x} is a \code{data.frame}.
#' @export
table1.default <- function(x, labels, groupspan=NULL, rowlabelhead="", topclass="Rtable1", render=render.default, ...) {
    if (is.null(labels$strata)) {
        labels$strata <- names(x)
    }
    if (is.null(names(labels$strata))) {
        names(labels$strata) <- names(x)
    }
    thead <- t(sprintf("%s<br>(n=%d)", labels$strata[names(x)], sapply(x, nrow)))

    any.missing <- sapply(names(labels$variables), function(v) do.call(sum, lapply(x, function(s) sum(is.na(s[[v]])))) > 0)

    tbody <- lapply(names(labels$variables), function(v) {
        y <- do.call(cbind, lapply(x, function(s) render(x=s[[v]], missing=any.missing[v], ...)))
        rownames(y) <- paste(rownames(y), sep="")
        rownames(y)[1] <- labels$variables[[v]]
        y })

    if (is.null(topclass) || topclass=="") {
        cat('<table>
<thead>')
    } else if (is.character(topclass) && length(topclass)==1) {
        cat(sprintf('<table class="%s">
<thead>', topclass))
    } else {
        stop("topclass should be character and of length 1.")
    }

    if (!is.null(groupspan)) {
        thead0 <- ifelse(is.na(labels$groups) | labels$groups=="", "", sprintf('<div>%s</div>', labels$groups))
        thead0 <- sprintf('<th colspan="%d" class="grouplabel">%s</th>', groupspan, thead0)
        thead0 <- c('<th class="grouplabel"></th>', thead0)
        thead0 <- paste("<tr>\n", paste(thead0, sep="", collapse="\n"), "\n</tr>\n", sep="", collapse="")
        cat(thead0)
    }

    if (is.null(rowlabelhead)) rowlabelhead <- ""
    cat(table.rows(thead, row.labels=rowlabelhead, th=T))

    cat('</thead>
<tbody>')

    cat(paste(sapply(tbody, table.rows), collapse=""))

    cat('</tbody>
</table>')

}

#' @describeIn table1 The \code{formula} interface.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula
table1.formula <- function(x, data, overall="Overall", rowlabelhead="", topclass="Rtable1", render=render.default, ...) {
    f <- Formula(x)
    m1 <- model.frame(formula(f, rhs=1), data=data, na.action=na.pass)
    for (i in 1:ncol(m1)) {
        if (!has.label(m1[[i]])) {
            label(m1[[i]]) <- names(m1)[i]
        }
    }
    if (length(f)[2] > 1) {
        m2 <- model.frame(formula(f, rhs=2), data=data, na.action=na.pass)
        ncolumns <- prod(sapply(m2, nlevels))
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
        variables=lapply(m1, render.varlabel))
    names(labels$strata) <- names(strata)

    if (!is.null(m2) && length(m2) > 1) {
        labels$groups <- grouplabel
        table1.default(x=strata, labels=labels, groupspan=groupspan, rowlabelhead=rowlabelhead, topclass=topclass, render=render, ...)
    } else {
        table1.default(x=strata, labels=labels, rowlabelhead=rowlabelhead, topclass=topclass, render=render, ...)
    }
}

