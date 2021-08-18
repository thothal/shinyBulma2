#' Create a bulma Table
#'
#' @param data \[`matrix-ish`\]\cr
#'        A matrix, data.frame or alike which can be processed by [xtable::xtable()].
#' @param stripes,hover,border,narrow,fullwidth \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, apply the corresponding bulma table format to the table.
#' @param scrollable \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, wrap the table in a scrollable container.
#' @param align \[`character(1)`: \sQuote{NULL}\]\cr
#'        The alignment string, see [shiny::renderTable()] for the specification of it.
#' @param row_names,col_names \[`logical(1)`: \sQuote{FALSE} and \sQuote{TRUE}\]\cr
#'        If \sQuote{TRUE}, row (column) names are included in the ouptut.
#' @param digits \[`integer(1)`: \sQuote{NULL}\]\cr
#'        An integer specifying the number of decimal places for numeric columns,
#'        see [shiny::renderTable()] for full specification.
#' @param na \[`character(1)`: \sQuote{NA}\]\cr
#'        The string to use int he table cells whose values are missing.
#' @param escape \[`logical(1)`: \sQuote{TRUE}\]\cr
#'        Should cell contents be escaped or printed as is (which allows arbitrary HTML
#'        code to be included in the table, see example).
#' @param selected_row \[`integer(1)`: \sQuote{NULL}\]\cr
#'        If not \sQuote{NULL}, mark the given row.
#' @param convert_to_tags \[`logical(1)`: \sQuote{TRUE} if package `{xml2}` exists\]\cr
#'        If \sQuote{FALSE}, an `html` object (i.e. a string marked via
#'        [htmltools::HTML()]), otherwise the `html` object is parsed to shiny tags using
#'        library `{xml2}`.
#' @param ... Arguments to be passed through to [xtable::xtable()] and
#'        [xtable::print.xtable()].
#'
#' @note
#' This function is basically a copy & paste from [shiny::renderTable()]. Thus, the
#' workhorse function for translating the data to HTML is [xtable::xtable()]. This
#' function, however, does return only *HTML* and **not** `htmltools::tags`. While
#' `htmltools` can work with literal HTML code via [htmltools::HTML()], it makes
#' subsequent changes via `tagAppendAttributes` for instance impossible. Furthermore,
#' it would be inconsistent with the other functions in this package, if `bulma_table`
#' would return a `html` object and not a `shiny.tag`. Hence, we added a parser which
#' translates the HTML string into a `shiny.tag` with the help of package `xml2`.
#'
#' This approach seems to be rather complicated, but it has the advantage that especially
#' the translation of `data.frame` to HTML is done by a well tested function and we do
#' not have to reinvent the wheel.
#'
#' @return A bulma table.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'    cars <- mtcars[1:5, ]
#'    cars[2, 3] <- NA
#'
#'    ui <- bulma_page(
#'      bulma_block(
#'        bulma_title("Basic Table"),
#'        bulma_table(cars)
#'      ),
#'      bulma_block(
#'        bulma_title("Highlight a Row"),
#'        bulma_table(cars, selected_row = 3)
#'      ),
#'      bulma_block(
#'        bulma_title("Include Rownames"),
#'        bulma_table(cars, row_names = TRUE)
#'      ),
#'      bulma_block(
#'        bulma_title("Several Style Flags"),
#'        bulma_table(cars, stripes = TRUE, hover = TRUE, border = TRUE, narrow = TRUE,
#'                    fullwidth = TRUE)
#'      ),
#'      bulma_block(
#'        bulma_title("Format & Align Columns and NAs"),
#'        bulma_table(cars, align = paste(rep(c("c", "r", "l"), c(4, 4, 3)),
#'                                        collapse = ""),
#'                    na = "Missing", digits = -1)
#'      ),
#'      bulma_block(
#'        bulma_title("Add Custom HTML"),
#'        bulma_table(data.frame(`<abbr title = "Played">P</abbr>` = rep(3, 3),
#'                               `<abbr title = "Lost">L</abbr>` = 1:3,
#'                               `<abbr title = "Draw">D</abbr>` = c(1, 0, 0),
#'                               `<abbr title = "Won">W</abbr>` = c(1, 1, 0),
#'                               check.names = FALSE),
#'                    digits = 0,
#'                    escape = FALSE)
#'      ),
#'      bulma_block(
#'        bulma_title("Scrollable Table"),
#'        bulma_table(matrix(rnorm(1000), 10),
#'                    scrollable = TRUE)
#'      ),
#'      bulma_block(
#'        bulma_title("Pass Arguments to `print.xtable`"),
#'        bulma_table(cars,
#'                    html.table.attributes =
#'                      "style = 'background-color: steelblue; color:white'")
#'      )
#'    )
#
#'    server <- function(input, output) {
#'    }
#'
#'    shinyApp(ui, server)
#' }
bulma_table <- function(data, stripes = FALSE, hover = FALSE, border = FALSE,
                        narrow = FALSE, fullwidth = FALSE, scrollable = FALSE,
                        align = NULL, row_names = FALSE, col_names = TRUE,
                        digits = NULL, na = "NA", escape = TRUE,
                        selected_row = NULL,
                        convert_to_tags = requireNamespace("xml2", quietly = TRUE),
                        ...) {
  format <- c(bordered = border, striped = stripes, hoverable = hover,
              narrow = narrow, fullwidth = fullwidth)
  base_classes <- add_class("table",
                            make_class(names(format)[format]))
  ## mostly taken from shiny::renderTable
  dots <- list(...)
  data <- as.data.frame(data)
  if (convert_to_tags && !requireNamespace("xml2", quietly = TRUE)) {
    warning("`convert_to_tags = TRUE` requires package `xml2` to be installed",
            domain = NA)
    convert_to_tags <- FALSE
  }
  converter <- if (convert_to_tags) html_to_tags else identity
  if (is.null(data) || (is.data.frame(data) && nrow(data) ==
                        0 && ncol(data) == 0)) {
    tab <- as.character(htmltools::tags$table(class = base_classes))
  }  else {
    xtable_argnames <- setdiff(names(formals(xtable::xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]
    defaultAlignment <- function(col) {
      if (is.numeric(col))
        "r"
      else "l"
    }
    if (is.null(align) || align == "?") {
      names <- defaultAlignment(attr(data, "row.names"))
      cols <- paste(vapply(data, defaultAlignment, character(1)),
                    collapse = "")
      cols <- paste0(names, cols)
    }
    else {
      num_cols <- if (row_names)
        nchar(align)
      else nchar(align) + 1
      valid <- !grepl("[^lcr\\?]", align)
      if (num_cols == ncol(data) + 1 && valid) {
        cols <- if (row_names)
          align
        else paste0("r", align)
        defaults <- grep("\\?", strsplit(cols,
                                         "")[[1]])
        if (length(defaults) != 0) {
          vals <- vapply(data[, defaults - 1], defaultAlignment,
                         character(1))
          for (i in seq_len(length(defaults))) {
            substr(cols, defaults[i], defaults[i]) <- vals[i]
          }
        }
      }
      else if (nchar(align) == 1 && valid) {
        cols <- paste0(rep(align, ncol(data) + 1), collapse = "")
      }
      else {
        stop("`align` must contain only the characters `l`, `c`, `r` and/or `?` and ",
             "have length either equal to 1 or to the total number of columns")
      }
    }
    xtable_args <- c(xtable_args, align = cols, digits = digits)
    xtable_res <- do.call(xtable::xtable, c(list(data), xtable_args))
    print_args <- list(x = xtable_res, type = "html",
                       include.rownames = {
                         if ("include.rownames" %in% names(dots)) dots$include.rownames
                         else row_names
                       }, include.colnames = {
                         if ("include.colnames" %in% names(dots)) dots$include.colnames
                         else col_names
                       }, NA.string = {
                         if ("NA.string" %in% names(dots)) dots$NA.string else na
                       }, html.table.attributes = paste0({
                         if ("html.table.attributes" %in% names(dots))
                           dots$html.table.attributes
                         else ""
                       }, " ", "class = '", base_classes, "' "),
                       comment = {
                         if ("comment" %in% names(dots)) dots$comment else FALSE
                       }
    )
    print_args <- c(print_args, non_xtable_args)
    if (!escape) {
      if (!is.null(print_args$sanitize.text.function)) {
        warning("setting `escape = FALSE` will overwrite argument ",
                "`sanitize.text.function`",
                domain = NA)
      }
      print_args$sanitize.text.function <- identity
    }
    print_args <- print_args[unique(names(print_args))]
    tab <- utils::capture.output(do.call(print, print_args))
    if (!is.null(selected_row)) {
      if (selected_row < !col_names | selected_row > nrow(data)) {
        warning("`selected_row` must be between ", 1 - col_names, " and `nrow(data)`")
      } else {
        tab[selected_row + 2 - !col_names] <- sub("<tr", "<tr class = 'is-selected'",
                                                  tab[selected_row + 2 - !col_names])
      }
    }
    tab <- paste(tab, collapse = "\n")
    tab <- gsub("align=\"([^\"]+)\"", "class = 'has-text-\\1'", tab)
    tab <- gsub("has-text-center", "has-text-centered", tab, fixed = TRUE)
    tab <- gsub(paste(">", na, "<"),
                paste0(" class='",
                       make_class("text-grey", prefix = "has"),
                       " ",
                       make_class("italic"),
                       "'>",
                       na, "<"), tab)
    tab <- gsub("class\\s*=\\s*(['\"])(.*)\\1\\s+class\\s*=\\s*(['\"])(.*)\\3",
                "class = '\\2 \\4'", tab)
    if (col_names) {
      tab <- sub("<tr( class = '[^']+')?>", "<thead> <tr\\1>", tab)
      tab <- sub("</tr>", "</tr> </thead>\n<tbody>",
                 tab)
      tab <- sub("</table>$", "</tbody>\n</table>",
                 tab)
      cols <- if (row_names)
        cols
      else substr(cols, 2, nchar(cols))
      cols <- strsplit(cols, "")[[1]]
      cols[cols == "l"] <- "has-text-left"
      cols[cols == "r"] <- "has-text-right"
      cols[cols == "c"] <- "has-text-centered"
      for (i in seq_len(length(cols))) {
        tab <- sub("<th>", paste0("<th class = '",
                                  cols[i], "'>"), tab)
      }
    }
  }
  if (scrollable) {
    tab <- as.character(htmltools::div(class = "table-container", htmltools::HTML(tab)))
  }
  converter(tab)
}
