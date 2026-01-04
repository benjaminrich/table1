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
    stop("This function requires package 'kableExtra'. Please install it and try again.", call.=FALSE)
  }
  if (missing(format) || is.null(format)) {
    format <- if (knitr::is_latex_output()) "latex" else "html"
  }
  default.align <- missing(align)

  obj <- attr(x, "obj", exact=TRUE)
  with(obj, {
    rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
    i <- lapply(contents, function(y) {
      if (all(y[1,, drop=TRUE] == "")) {
        nrow(y) - 1
      } else {
        nrow(y)
      }
    })
    z <- lapply(contents, function(y) {
      if (all(y[1,, drop=TRUE] == "")) {
        y <- as.data.frame(y[-1,, drop=FALSE], stringsAsFactors=FALSE)
        y2 <- data.frame(x=rownames(y), stringsAsFactors=FALSE)
      } else {
        y2 <- data.frame(x="", stringsAsFactors=FALSE)
      }
      y <- cbind(setNames(y2, rlh), y)
      y
    })
    if (format == "html") {
      names(i) <- labels$variables
    } else {
      names(i) <- vapply(X = labels$variables, FUN = names, FUN.VALUE = "")
    }
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
                           row.names = FALSE,
                           escape    = FALSE,
                           booktabs  = booktabs,
                           caption   = caption,
                           ...
    )
    out <- kableExtra::pack_rows(out, index=i, escape = FALSE)

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
