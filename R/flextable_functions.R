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
#' @param .data A data frame or a tibble to be displayed in the flextable.
#' @param flex_font_family Character, font family for the text in the flextable.
#' @param flex_font_size Numeric, font size for the text in the flextable.
#' @param color_header Logical. if TRUE, which is default value, header will be in blue background.
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
#' @importFrom flextable flextable set_flextable_defaults theme_booktabs bold bg
#' @importFrom officer fp_border





ag_flex <- function(.data, flex_font_family = "Arial", flex_font_size = 10, color_header = TRUE) {

        table_border <- officer::fp_border(color = col_table_border)

        flextable::set_flextable_defaults(
                font.size = flex_font_size,
                font.family = flex_font_family
                )


        ag_flextable <- .data %>%
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

        if(color_header){

                ag_flextable <- ag_flextable %>%
                        flextable::bg(bg = col_table_header, part = "header", i = 1)

        }

                return(ag_flextable)

}




#' @title Page section to save flextable as .docx
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



# stratas <- "Total" = glue::glue("All patients \n(n = ", nrow(breast_data), ")"),



#
#         pull(final) %>%
#         paste(collapse = ", ")
#
#
#
# split(1:nrow(.)) %>%
#         lapply(as.list)
#
# extracted_elements <- lapply(stratas, function(element) {
#         element[[1]]
# })
#
#
# unlist(extracted_elements)
tibble(!!enquo("species") := "Total", n = nrow(penguins))

vbl <- enquo(variable)
strata_headers <- penguins %>%
        group_by( species ) %>%
        summarise(n = n()) %>%
        add_row(species = "Total",
                n = nrow(.data)) %>%
        mutate(final = paste0(species, "\n(n = ", n, ")")) %>%
        select(species, final)


overall_group = function(data, col_name){

        d1 = data %>%
                mutate(summary_level = "grouped")

        d2 = data %>%
                mutate(summary_level = "ungrouped") %>%
                mutate(!!sym(col_name) := NA)

        d12 = rbind(d1, d2) %>%
                group_by(summary_level, !!sym(col_name))

        return(d12)
}





ag_flex_header_labels <- function(data, strata){
        overall_group = function(data, strata){

                d1 = data %>%
                        mutate(summary_level = "grouped")

                d2 = data %>%
                        mutate(summary_level = "ungrouped") %>%
                        mutate(!!sym(strata) := "Total")

                d12 = rbind(d1, d2) %>%
                        group_by(summary_level, !!sym(strata))

                return(d12)
        }


        strata_headers <- overall_group(data, strata) %>%
                summarise(n = n()) %>%
                ungroup() %>%
                mutate(final = paste0(.data[[strata]],"\n(n = ", n, ")")) %>%
                select({{ strata }}, final)

base_headers <- c("label" = "Variable", "levels" = " ", "p" = "p value")

all_header <- c(base_headers, deframe(strata_headers))
all_header
}

ag_flex_header_labels(data = penguins, strata = "species")

ag_ff_summary(
            .data = penguins,
            strata = "species",
            table_vars = table_vars_1) %>%
        ag_flex() %>%
        set_header_labels(i = 1, values = ag_flex_header_labels(data = penguins, strata = "species"))


summary_factorlist(
        .data = penguins,
        explanatory  = table_vars_1,
        dependent = "species", add_col_totals = TRUE, total_col = TRUE) %>%
        slice(1)



