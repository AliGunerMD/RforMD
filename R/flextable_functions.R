#' @title Define Colors for Table Styling
#' @description
#' This code chunk defines colors for different elements of a table, such as the table header, footer,
#' border, background of the table header, and the overall table border.
#'
#' @author Ali Guner
#'
#' @return A set of color values for table styling.
#'
#' @examples
#' \dontrun{
#' # Assuming you want to use these colors in table styling
#' iris %>%
#' ag_flex() %>%
#' flextable::bg(i = 1, j = NULL, bg = col_table_header, part = "header")
#' }
#'
#' @importFrom officer fp_border

col_table_header <- ag_colors_standard("blues")[4]
col_footer <- ag_colors_standard("grays")[6]
col_table_border <- ag_colors_standard("grays")[5]
table_border <- officer::fp_border(color = col_table_border)
col_table_header_bg <- ag_colors_standard("grays")[2]






#' @title Create a Styled Flextable
#' @description
#' This function generates a styled flextable with specified font family and font size.
#' The flextable is configured with default font settings, the 'theme_booktabs', and additional styling
#' to make the first row bold in the header.
#' @param data A data frame or a tibble to be displayed in the flextable.
#' @param flex_font_family Character, font family for the text in the flextable.
#' @param flex_font_size Numeric, font size for the text in the flextable.
#' @return A flextable object with customized styling.
#' @author Ali Guner
#' @export
#'

#' @examples
#' \dontrun{
#' # Assuming you have a data frame named 'my_data'
#' my_data %>% ag_flex()
#' my_data %>% ag_flex(flex_font_family = "Times New Roman", flex_font_size = 14)
#'
#' # If you want to undo bold()
#' bold(i = 1, j = NULL, bold = FALSE, part = "header")
#' }
#'
#' @importFrom flextable flextable set_flextable_defaults theme_booktabs bold
#' @importFrom officer fp_border






ag_flex <- function(data, flex_font_family = "Arial", flex_font_size = 11) {

        flextable::set_flextable_defaults(
                font.size = flex_font_size,
                font.family = flex_font_family
                )


        data %>%
                flextable::flextable() %>%
                flextable::theme_booktabs() %>%
                flextable::set_table_properties(
                        layout = "autofit",
                        width = 1) %>%
                flextable::bold(
                        i = 1,
                        j = NULL,
                        bold = TRUE,
                        part = "header")
}




#' @title Page section
#' @param orientation A character value specifying the page orientation. Use "landscape" for landscape orientation and "portrait" for portrait orientation.
#' @return A page section object for use in a flextable document (while saving with pr_section ()).
#' @author Ali Guner
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you want to create a landscape page section
#' ag_flex_page_section("landscape")
#'
#' # Assuming you want to create a portrait page section
#' ag_flex_page_section("portrait")
#' }
#'
#' @importFrom officer prop_section page_size page_mar

ag_flex_page_section <- function(orientation) {
        if (orientation %in% c("landscape", "portrait")) {

                width <- if (orientation == "portrait") 14 else NULL
                return(
                        officer::prop_section(
                                page_size = officer::page_size(orient = orientation,
                                                               width = width),
                                type = "nextPage",
                                page_margins = officer::page_mar()
                        )
                )
        } else {
                stop("Invalid orientation. Use 'landscape' or 'portrait'.")
        }
}
