#' Convert a \code{table1} object to \code{flextable}.
#'
#' @param x An object returned by \code{\link{table1}}.
#' @param tablefn Choose a function from the \code{flextable} package to use as
#' the basis for the table.
#' @param ... Further options passed to \code{tablefn}.
#' @param label_colspan Have variable labels span all columns in the table
#' @return A \code{flextable} object.
#' @note The \code{flextable} package needs to be installed for this to work.
#' @importFrom utils getFromNamespace
#' @export
t1flex <- function(x, tablefn=c("qflextable", "flextable", "regulartable"), ..., label_colspan = FALSE) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("This function requires package 'flextable'. Please install it and try again.", call. = FALSE) # nocov
  }
  tablefn <- match.arg(tablefn)
  tablefn <- getFromNamespace(tablefn, "flextable")
  obj <- attr(x, "obj", exact = TRUE)
  rlh <- if (is.null(obj$rowlabelhead) || obj$rowlabelhead=="") "\U{00A0}" else obj$rowlabelhead
  i <- vapply(X = obj$contents, FUN = nrow, FUN.VALUE = 1)
  i <- cumsum(c(1, i[-length(i)]))
  each_group_df <- lapply(obj$contents, function(y) {
    y <- as.data.frame(y, stringsAsFactors = FALSE)
    y2 <- data.frame(x=paste0(c("", rep("\U{00A0}\U{00A0}", nrow(y) - 1)), rownames(y)), stringsAsFactors = FALSE)
    y <- cbind(setNames(y2, rlh), y)
    if (label_colspan) {
      y[1,] <- y[1,1]
    }
    y
  })
  all_group_df <- do.call(rbind, each_group_df)

  header_df <- data.frame(
    labels = c(rlh, obj$headings),
    keys   = LETTERS[1:ncol(all_group_df)]
  )

  if (!is.null(obj$groupspan)) {
    zzz <- ncol(all_group_df) - sum(obj$groupspan) - 1
    label2 <- c("", rep(obj$labels$groups, times=obj$groupspan), rep("", zzz))
    header_df <- cbind(data.frame(label2=label2), header_df)
  }

  colnames(all_group_df) <- header_df$keys
  rownames(all_group_df) <- NULL
  out <- tablefn(all_group_df, ...)
  out <- flextable::set_header_df(out,  header_df, key="keys")
  out <- flextable::merge_h(out, part = "header", i = 1)
  #out <- flextable::merge_v(out, part = "header", j = 1)
  #out <- flextable::theme_booktabs(out, bold_header = TRUE)

  # Add line above the top of the table
  out <- flextable::hline_top(out, border = officer::fp_border(width=1.5), part = "header")
  # Add line below the bottom of the header
  out <- flextable::hline_bottom(out, border = officer::fp_border(width=1.5), part = "header")
  # Center both data and header cells, except for the first column
  out <- flextable::align(out, j=2:(obj$ncolumns+1), align="center", part="body")
  out <- flextable::align(out, j=2:(obj$ncolumns+1), align="center", part="header")
  # Make the top header bold
  out <- flextable::bold(out, part="header")
  # Make the category headers bold
  out <- flextable::bold(out, i=i, j=1)
  if (label_colspan) {
    out <- flextable::merge_h(out, i = i)
  }

  if (!is.null(obj$caption)) {
    out <- flextable::set_caption(out, caption=obj$caption)
  }
  if (!is.null(obj$footnote)) {
    out <- flextable::add_footer_lines(out, values=obj$footnote)
  }
  out
}
