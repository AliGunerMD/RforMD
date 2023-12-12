
#' Constants for package
#'
#' This file contains commonly used constants for the package.
#'
#' @importFrom officer fp_border prop_section page_size page_mar
#' @keywords internals
#' @format a vector with the colors I need
#'

# Flextable constants
col_table_header <- "#9ECAE1"                   # ag_colors_standard("blues")[4]
col_footer <- "#737373"                         # ag_colors_standard("grays")[6]
col_table_border <- "#969696"                   # ag_colors_standard("grays")[5]
col_table_header_bg <- "#F0F0F0"                # ag_colors_standard("grays")[2]
table_border <- officer::fp_border(color = col_table_border)


myref_symbols <- c("\U2020", "\U2021", "\U00A7", "\U00A5")

# Dagger symbol          †
# Double dagger symbol   ‡
# Section symbol         §
# Yen symbol             ¥

sect_properties_landscape <- officer::prop_section(
        page_size = officer::page_size(orient = "landscape"),
        type = "nextPage",
        page_margins = officer::page_mar()
)

sect_properties_portrait <- officer::prop_section(
        page_size = officer::page_size(orient = "portrait",
                              width = 14),
        type = "nextPage",
        page_margins = officer::page_mar()
)
