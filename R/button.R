#' Create a bulma Button
#'
#' @param label \[`character(1)`: \sQuote{NULL}\]\cr
#'        The label of the button.
#' @param icon \[`character(1)`: \sQuote{NULL}\]\cr
#'        The name of the icon.
#' @param color \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma color name.
#' @param light \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} the `is-light` modifier is used, resulting in a lighter color.
#' @param size \[`character(1)`: \sQuote{NULL}\]\cr
#'        The size of the button or the button list. Valid sizes are
#'        `small`, `normal`, `medium`, `large`.
#' @param icon_size,icon_pos,icon_class,icon_lib \[`character(1)`: \sQuote{normal},
#'        \sQuote{start} and \sQuote{NULL} respectively\]\cr
#'        Arguments passed down `bulma_icon` controlling the appearance of the icon.
#' @param full_width \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} the button spans the full width.
#' @param disabled \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, the button will be disabled.
#' @param style \[`character(n)`: \sQuote{NULL}\]\cr
#'        Style modifiers to be applied to the button. Several modifiers can be combined.
#'        Valid styles are `outlined`, `inverted` and `rounded`.
#' @param state \[`character(1)`: \sQuote{NULL}\]\cr
#'        State modifier to be applied to the button. Valid states are `hovered`,
#'        `focused`, `active`, `selected`, `loading` and `static`.
#' @param container \[`function`\]\cr
#'        The container to be used for the `button` or the button list.
#'        For the `button` it Should be a function creating either
#'        a `<button>`, an `<a>` or an `<input type = "submit|reset">` HTML tag.
#' @param alignment \[`character(1)`: \sQuote{NULL}\]\cr
#'        The alignment to be used for the button list. Valid alignments are `centered` or
#'        `right`.
#' @param addons \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} the buttons in the button list are attached together.
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Further arguments passed down to `container`.
#'
#' ## Note
#' This function creates the HTML to render a bulma button. It does **not**, however, add
#' any reactivity.
#'
#' @seealso [bulma_icon], [Bulma Button](https://bulma.io/documentation/elements/button/)
#'
#' @return A bulma `button`, a `delete` button or a buttons list.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'    ui <- bulma_page(
#'      bulma_block(
#'        h1("Simple Button", class = "title"),
#'        bulma_button("Button")
#'      ),
#'      bulma_block(
#'        h1("Different Containers", class = "title"),
#'        bulma_button("Anchor", container = a),
#'        bulma_button(container = function(...) tags$input(..., type = "submit",
#'                                                          value = "Submit Input")),
#'        bulma_button(container = function(...) tags$input(..., type = "reset",
#'                                                          value = "Reset Input"))
#'      ),
#'      bulma_block(
#'        h1("Different Colors", class = "title"),
#'        bulma_button("White", color = "white"),
#'        bulma_button("Light", color = "light"),
#'        bulma_button("Dark", color = "dark"),
#'        bulma_button("Black", color = "black"),
#'        bulma_button("Text", color = "text"),
#'        bulma_button("Ghost", color = "ghost")
#'      ),
#'      bulma_block(
#'        h1("Button Groups", class = "title"),
#'        bulma_buttons(
#'          bulma_button("Primary", color = "primary"),
#'          bulma_button("Link", color = "link")
#'        ),
#'        bulma_buttons(
#'          bulma_button("Info", color = "info"),
#'          bulma_button("Success", color = "success"),
#'          bulma_button("Warning", color = "warning"),
#'          bulma_button("Danger", color = "danger")
#'        )
#'      ),
#'      bulma_block(
#'        h1("Light Colors", class = "title"),
#'        bulma_buttons(
#'          bulma_button("Primary", color = "primary", light = TRUE),
#'          bulma_button("Link", color = "link", light = TRUE)
#'        ),
#'        bulma_buttons(
#'          bulma_button("Info", color = "info", light = TRUE),
#'          bulma_button("Success", color = "success", light = TRUE),
#'          bulma_button("Warning", color = "warning", light = TRUE),
#'          bulma_button("Danger", color = "danger", light = TRUE)
#'        )
#'      ),
#'      bulma_block(
#'        h1("Different Sizes", class = "title"),
#'        bulma_button("Small", size = "small"),
#'        bulma_button("Normal [Default]", size = "normal"),
#'        bulma_button("Medium", size = "medium"),
#'        bulma_button("Large", size = "large"),
#'        bulma_buttons(
#'          size = "medium",
#'          bulma_button("All"),
#'          bulma_button("Medium"),
#'          bulma_button("Size")
#'        ),
#'        bulma_buttons(
#'          size = "small",
#'          bulma_button("Small"),
#'          bulma_button("Small"),
#'          bulma_button("Small"),
#'          bulma_button("Normal", size = "normal"),
#'          bulma_button("Small")
#'        )
#'      ),
#'      bulma_block(
#'        h1("Fullwidth", class = "title"),
#'        bulma_button("Small", size = "small", full_width = TRUE),
#'        bulma_button("Normal", full_width = TRUE),
#'        bulma_button("Medium", size = "medium", full_width = TRUE),
#'        bulma_button("Large", size = "large", full_width = TRUE)
#'      ),
#'      bulma_block(
#'        h1("Different Styles", class = "title"),
#'        h2("Outlined", class = "subtitle"),
#'        bulma_button("Outlined", style = "outlined"),
#'        bulma_button("Outlined", color = "primary", style = "outlined"),
#'        bulma_button("Outlined", color = "link", style = "outlined"),
#'        bulma_button("Outlined", color = "info", style = "outlined"),
#'        bulma_button("Outlined", color = "danger", style = "outlined"),
#'        bulma_button("Outlined", color = "warning", style = "outlined"),
#'        bulma_button("Outlined", color = "success", style = "outlined")
#'      ),
#'      bulma_block(
#'        h2("Inverted", class = "subtitle"),
#'        bulma_button("Inverted", style = "inverted"),
#'        bulma_button("Inverted", color = "primary", style = "inverted"),
#'        bulma_button("Inverted", color = "link", style = "inverted"),
#'        bulma_button("Inverted", color = "info", style = "inverted"),
#'        bulma_button("Inverted", color = "danger", style = "inverted"),
#'        bulma_button("Inverted", color = "warning", style = "inverted"),
#'        bulma_button("Inverted", color = "success", style = "inverted")
#'      ),
#'      bulma_block(
#'        h2("Rounded", class = "subtitle"),
#'        bulma_button("Rounded", style = "rounded"),
#'        bulma_button("Rounded", color = "primary", style = "rounded"),
#'        bulma_button("Rounded", color = "link", style = "rounded"),
#'        bulma_button("Rounded", color = "info", style = "rounded"),
#'        bulma_button("Rounded", color = "danger", style = "rounded"),
#'        bulma_button("Rounded", color = "warning", style = "rounded"),
#'        bulma_button("Rounded", color = "success", style = "rounded")
#'      ),
#'      bulma_block(
#'        h1("Different States", class = "title"),
#'        bulma_button("Hovered", state = "hovered"),
#'        bulma_button("Focused", color = "primary", state = "focused"),
#'        bulma_button("Active", color = "link", state = "active"),
#'        bulma_button("Selected", color = "info", state = "selected"),
#'        bulma_button("Loading", color = "danger", state = "loading"),
#'        bulma_button("Static", color = "warning", state = "static"),
#'        bulma_button("Disabled", color = "success", disabled = TRUE)
#'      ),
#'      bulma_block(
#'        h1("Icons", class = "title"),
#'        bulma_buttons(
#'          bulma_button(icon = "bold", icon_class = "fas"),
#'          bulma_button(icon = "italic", icon_class = "fas"),
#'          bulma_button(icon = "underline", icon_class = "fas"),
#'          container = p
#'        ),
#'        bulma_buttons(
#'          bulma_button("GitHub", "github", icon_class = "fab"),
#'          bulma_button("@jgthms", "twitter", color = "primary", icon_class = "fab"),
#'          bulma_button("Save", "check", color = "success", icon_class = "fab",
#'                       icon_size = "small"),
#'          bulma_button("Delete", "times", color = "danger", style = "outlined",
#'                       class = "fab", icon_size = "small", icon_pos = "end"),
#'          container = p
#'        ),
#'        bulma_buttons(
#'          bulma_button("GitHub", "github", icon_class = "fab", size = "small",
#'                       icon_size = "small"),
#'          bulma_button("GitHub", "github", icon_class = "fab"),
#'          bulma_button("GitHub", "github", icon_class = "fab", size = "medium"),
#'          bulma_button("GitHub", "github", icon_class = "fab", size = "large",
#'                       icon_size = "medium"),
#'          container = p
#'        ),
#'        bulma_buttons(
#'          bulma_button(icon = "heading", icon_class = "fas", size = "small",
#'                       icon_size = "small"),
#'          container = p
#'        ),
#'        bulma_buttons(
#'          bulma_button(icon = "heading", icon_class = "fas", icon_size = "small"),
#'          bulma_button(icon = "heading", icon_class = "fas fa-lg"),
#'          container = p
#'        ),
#'        bulma_buttons(
#'          bulma_button(icon = "heading", icon_class = "fas", size = "medium",
#'                       icon_size = "small"),
#'          bulma_button(icon = "heading", icon_class = "fas fa-lg", size = "medium"),
#'          bulma_button(icon = "heading", icon_class = "fas fa-2x", size = "medium",
#'                       icon_size = "medium"),
#'          container = p
#'        ),
#'        bulma_buttons(
#'          bulma_button(icon = "heading", icon_class = "fas", size = "large",
#'                       icon_size = "small"),
#'          bulma_button(icon = "heading", icon_class = "fas fa-lg", size = "large"),
#'          bulma_button(icon = "heading", icon_class = "fas fa-2x", size = "large",
#'                       icon_size = "medium"),
#'          container = p
#'        )
#'      ),
#'      bulma_block(
#'        h1("List of buttons", class = "title"),
#'        bulma_buttons(
#'          bulma_button("Yes"),
#'          bulma_button("Maybe"),
#'          bulma_button("No"),
#'          addons = TRUE
#'        ),
#'        bulma_buttons(
#'          bulma_button("Yes"),
#'          bulma_button("Maybe"),
#'          bulma_button("No"),
#'          addons = TRUE,
#'          alignment = "centered"
#'        ),
#'        bulma_buttons(
#'          bulma_button("Yes"),
#'          bulma_button("Maybe"),
#'          bulma_button("No"),
#'          addons = TRUE,
#'          alignment = "right"
#'        ),
#'        bulma_buttons(
#'          bulma_button("Yes", color = "success", state = "selected"),
#'          bulma_button("Maybe"),
#'          bulma_button("No"),
#'          addons = TRUE
#'        ),
#'        bulma_buttons(
#'          bulma_button("Yes"),
#'          bulma_button("Maybe", color = "link", state = "selected"),
#'          bulma_button("No"),
#'          addons = TRUE
#'        ),
#'        bulma_buttons(
#'          bulma_button("Yes"),
#'          bulma_button("Maybe"),
#'          bulma_button("No", color = "danger", state = "selected"),
#'          addons = TRUE
#'        )
#'      ),
#'      bulma_block(
#'        h1("Delete Button", class = "title"),
#'        bulma_delete(size = "small"),
#'        bulma_delete(),
#'        bulma_delete(size = "medium"),
#'        bulma_delete(size = "large")
#'      )
#'    )
#'
#'    server <- function(input, output) {
#'    }
#'
#'    shinyApp(ui, server)
#' }
bulma_button <- function(label = NULL,
                         icon = NULL,
                         color = NULL,
                         light = FALSE,
                         size = NULL,
                         icon_size = "normal",
                         icon_class = NULL,
                         icon_lib = "font-awesome",
                         icon_pos = c("start", "end"),
                         full_width = FALSE,
                         disabled = FALSE,
                         style = NULL,
                         state = NULL,
                         container = htmltools::tags$button,
                         ...) {
  size_class <- validate_bulma_size(size, FALSE)
  valid_styles <- c("outlined", "inverted", "rounded")
  style_class <- if (!is.null(style)) make_class(match.arg(style, valid_styles, TRUE))
  valid_states <- c("hovered", "focused", "active", "selected", "loading", "static")
  state_class <- if (!is.null(state)) make_class(match.arg(state, valid_states))
  icon_pos <- match.arg(icon_pos)
  if (any(c("ghost", "text") %in% color) && light) {
    warning("'light' is not defined for color \"", color, "\"")
    light <- FALSE
  }
  color_class <- validate_bulma_color(color, "button")
  full_width_class <- if (full_width) make_class("fullwidth")
  if (light) {
    color_class <- add_class(color_class, make_class("light"))
  }
  icon <- if (!is.null(icon)) bulma_icon(icon, size = icon_size, class = icon_class,
                                         lib = icon_lib)
  label_element <- label
  if (!is.null(label)) {
    if (!is.null(icon)) {
      label_element <- htmltools::span(label)
    }
  }
  cls <- add_class("button",
                   c(color_class, size_class, style_class, state_class, full_width_class))
  if (icon_pos == "start") {
    btn <- container(class = cls, icon, label_element, ...)
  } else if (icon_pos == "end") {
    btn <- container(class = cls, label_element, icon, ...)
  }
  if (disabled) {
    btn <- htmltools::tagAppendAttributes(btn, disabled = NA)
    if (!is.null(label)) {
      btn <- htmltools::tagAppendAttributes(btn, title = label)
    }
  }
  btn
}

#' @rdname bulma_button
#' @export
bulma_buttons <- function(...,
                          alignment = NULL,
                          addons = FALSE,
                          size = NULL,
                          container = htmltools::div) {
  valid_alignments <- c("centered", "right")
  alignment_class <- if (!is.null(alignment)) make_class(match.arg(alignment,
                                                                   valid_alignments))
  addons_class <- if (addons) make_class("addons", prefix = "has")
  size_class <- validate_bulma_size(size, prefix = "are")
  container(..., class = add_class("buttons",
                                   c(alignment_class, addons_class, size_class)))

}

#' @rdname bulma_button
#' @export
bulma_delete <- function(size = NULL) {
  size_class <- validate_bulma_size(size)
  htmltools::tags$button(class = add_class("delete", size_class))
}
