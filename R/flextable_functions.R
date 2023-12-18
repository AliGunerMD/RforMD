




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





ag_flex <- function(.data, flex_font_family = "Arial", flex_font_size = 11, color_header = TRUE) {

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

#' @title Add Custom Headers (with n) to a flextable Object
#' @description
#' This function adds custom headers to a flextable object based on specified
#' dataset, strata variable, and custom levels.
#'
#' @param flex_obj A flextable object to which headers will be added.
#' @param .dataset The dataset used for generating the headers.
#' @param strata The name of the variable used for grouping or stratifying the data.
#' @param custom Logical value indicating whether to use custom levels or not.
#' @param custom_levels A named vector specifying custom levels for the strata variable.
#'
#' @return A flextable object with custom headers added.
#' @author Ali Guner
#' @importFrom dplyr mutate summarise group_by select
#' @importFrom rlang sym
#' @importFrom stringr str_to_title str_replace_all
#' @importFrom tibble deframe
#' @importFrom flextable set_header_labels
#'
#' @examples
#' \dontrun{
#'
#' custom_levels <- c("Gentoo" = "MyGentoo",
#' "Adelie" = "MyAdelie")
#'
#' ag_ff_summary(penguins, "species", penguins %>% select(-species) %>% names()) %>%
#'         ag_flex() %>%
#'         ag_flex_header_labels(penguins, strata = "species", custom = TRUE, custom_levels = custom_levels)
#'
#' }
#' @export


ag_flex_header_labels <- function(flex_obj, .dataset, strata, custom = FALSE, custom_levels) {
  if (!inherits(flex_obj, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_header()"))
  }

  if (!custom) {
    message("Because custom = FALSE, original stratas will be used.")
  }

  if (custom && missing(custom_levels)) {
    stop("The names of strata variable should be provided as a vector, if custom = TRUE.")
  }


  overall_group <- function(.dataset, strata) {
    d1 <- .dataset %>%
      dplyr::mutate(summary_level = "grouped")
    d2 <- .dataset %>%
      dplyr::mutate(summary_level = "ungrouped") %>%
      dplyr::mutate(!!sym(strata) := "Total")
    d12 <- rbind(d1, d2)
    return(d12)
  }




  ag_calc_header_labels <- function(.dataset, strata, custom, custom_levels) {
    if (custom) {
      strata_headers <- overall_group(.dataset, strata) %>%
        dplyr::mutate(new_var = .data[[strata]]) %>%
        dplyr::mutate(new_var = dplyr::recode(new_var, !!!custom_levels)) %>%
        dplyr::group_by(summary_level, new_var, !!rlang::sym(strata)) %>%
        dplyr::summarise(n = n(), .groups = "drop") %>%
        dplyr::mutate(
          final = paste0(new_var, "\n(n = ", n, ")"),
          final = stringr::str_replace_all(final, "Total\n", "All\n")
        ) %>%
        dplyr::select({{ strata }}, final)
    } else {
      strata_headers <- overall_group(.dataset, strata) %>%
        dplyr::group_by(summary_level, !!rlang::sym(strata)) %>%
        dplyr::summarise(n = n(), .groups = "drop") %>%
        dplyr::mutate(
          final = paste0(stringr::str_to_title(.data[[strata]]), "\n(n = ", n, ")"),
          final = stringr::str_replace_all(final, "Total\n", "All\n")
        ) %>%
        dplyr::select({{ strata }}, final)
    }


    return(strata_headers)
  }



  ag_calc_header_labels_nostrata <- function(.dataset, custom, custom_levels) {
    if (custom) {
      strata_headers <- .dataset %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(
          Total = "Total",
          new_var = recode(Total, !!!custom_levels)
        ) %>%
        dplyr::mutate(final = paste0(new_var, "\n(n = ", n, ")")) %>%
        dplyr::select(Total, final)
    } else {
      strata_headers <- .dataset %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(Total = "Total") %>%
        dplyr::mutate(final = paste0(Total, "\n(n = ", n, ")")) %>%
        dplyr::select(Total, final)
    }


    return(strata_headers)
  }



  if (missing(strata)) {
    strata <- NULL
  }

  if (is.null(strata)) {
    strata_headers <- ag_calc_header_labels_nostrata(.dataset, custom = custom, custom_levels = custom_levels)
  } else {
    strata_headers <- ag_calc_header_labels(.dataset, strata = strata, custom = custom, custom_levels = custom_levels)
  }


  base_headers <- c(
    "label" = "Variable",
    "levels" = " ",
    "p" = "p value"
  )

  all_header <- c(base_headers, tibble::deframe(strata_headers))


  flex_obj %>%
    flextable::set_header_labels(
      i = max(flex_obj$header$content$content$nrow),
      values = all_header
    )
}





#' @title Center align the content of a flextable
#' @description
#' This function center aligns the content of a flextable starting from the third column to the last column.
#'
#' @param flex_obj The flextable object for which the content alignment is applied.
#'
#' @return The flextable object with the content aligned to the center.
#'
#' @author Ali Guner
#'
#' @importFrom flextable align
#'
#' @examples
#' \dontrun{
#' ag_flex_center(flex_obj)
#' }
#
#' @seealso \code{\link{ag_flex()}}
#'
#' @export
#'
#'
#'

ag_flex_center <- function(flex_obj){

        if (!inherits(flex_obj, "flextable")) {
                stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_center()"))
        }


        flex_obj %>%
                flextable::align(j = 3:ncol(flex_obj$body$dataset),
                      part = "all",
                      align = "center")

}


#' @title Add horizontal lines to a flextable
#' @description
#' This function adds horizontal lines to a flextable based on the non-empty label rows in the dataset.
#'
#' @param flex_obj The flextable object to which the horizontal lines are added.
#'
#' @return The flextable object with horizontal lines added.
#'
#' @author Ali Guner
#'
#' @importFrom dplyr filter distinct pull
#' @importFrom flextable hline
#'
#' @examples
#' \dontrun{
#' ag_flex_hline(flex_obj)
#' }
#'
#' @seealso \code{\link{ag_flex()}}
#'
#' @export




ag_flex_hline <- function(flex_obj){

        if (!inherits(flex_obj, "flextable")) {
                stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_hline()"))
        }


        # Because I ll use non-empty label names (not p, because p can be NULL)
        ag_flex_nonempty_label <- function(flex_obj){
                nonempty_label_rows <- flex_obj$body$dataset %>%
                        dplyr::filter(label != "") %>%
                        dplyr::distinct(label) %>%
                        dplyr::pull(label)

                nonempty_label_rows
        }


        NE_label_rows <- which(flex_obj$body$dataset$label %in% ag_flex_nonempty_label(flex_obj))
        NE_label_rows <- NE_label_rows[NE_label_rows != 1]
        NE_label_rows <- NE_label_rows -1

        flex_obj %>%
                flextable::hline(i = NE_label_rows,
                      part = "body",
                      border = table_border)

}




#' @title Add a title to a flextable
#' @description
#' This function adds a title to a flextable object.
#'
#' @param flex_obj The flextable object to which the title is added.
#' @param n The table number.
#' @param title The title text.
#'
#' @return The flextable object with the title added.
#'
#' @author Ali Guner
#'
#' @importFrom flextable add_header_lines bg color
#'
#' @examples
#' \dontrun{
#' ag_flex_title(flex_obj, 1, "Table Title")
#' }
#'
#' @seealso \code{\link{}}
#'
#' @export
#'
#'
#'

ag_flex_title <- function(flex_obj, n, title){
        if (!inherits(flex_obj, "flextable")) {
                stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_title()"))
        }
        title <- paste0("Table ", n, ". ", title)

        flex_obj %>%
                flextable::add_header_lines(values = title) %>%
                flextable::bg(bg = col_table_header_bg, part = "header", i = 1)
}



#' @title Add a footnote to a flextable
#' @description
#' This function adds a footnote to a flextable object.
#'
#' @param flex_obj The flextable object to which the footnote is added.
#' @param footnote The text of the footnote.
#'
#' @return The flextable object with the footnote added.
#'
#' @author Ali Guner
#'
#' @importFrom flextable flextable add_footer_lines color
#'
#' @examples
#' \dontrun{
#' ag_flex_footnote(flex_obj, "This is a footnote.")
#' }
#'
#' @seealso \code{\link{}}
#'
#' @export
#'
#'
#'

ag_flex_footnote <- function(flex_obj, footnote = NULL){
        if (!inherits(flex_obj, "flextable")) {
                stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_footnote()"))
        }

        if(is.null(footnote)){
                stop("Please provide a footnote.")
        }

        flex_obj %>%
                flextable::add_footer_lines(values = footnote) %>%
                flextable::color(part = "footer", color = col_footer)

}


#' @title Add abbreviations to a flextable
#' @description
#' This function adds abbreviations to a flextable object.
#'
#' @param flex_obj The flextable object to which the abbreviations are added.
#' @param abbr The abbreviations. It can be a character vector or a named list.
#' @param use_df A logical value indicating whether to use a data frame format for the abbreviations.
#' @param prefix The prefix text to be displayed before the abbreviations.
#'
#' @return The flextable object with the abbreviations added.
#'
#' @author Ali Guner
#'
#' @importFrom flextable add_footer_lines color
#'
#' @examples
#' \dontrun{
#'
#' ag_flex_abbr(flex_obj, abbr = abbr)
#' }
#'
#' @seealso \code{\link{flextable::add_footer_lines()}}
#'
#' @export
#'
#'
#'
ag_flex_abbr <- function(flex_obj, abbr = NULL, use_df = TRUE, prefix = "Abbreviations: "){
        if (!inherits(flex_obj, "flextable")) {
                stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_abbr()"))
        }

        if(is.null(abbr)){
                stop("Please provide an abbreviation.")
        }


        if(!use_df){
                my_abrr <- abbr
                warning("Check the format of abbreviations vector.\nIf a list is prepared, convert use_df = TRUE.")

        } else {
                my_abrr <- paste0(names(abbr), ", ", abbr, collapse = "; ")

        }



        flex_obj %>%
                flextable::add_footer_lines(values = paste0(prefix, my_abrr)) %>%
                flextable::color(part = "footer", color = col_footer)

}




#' @title Save a flextable as a Word document
#' @description
#' This function saves a flextable object as a Word document.
#'
#' @param flex_obj The flextable object to be saved.
#' @param n The table number or identifier. Default is "x".
#' @param orientation The orientation of the page. Can be "Landscape" or "Portrait". Default is "Landscape".
#' @param mypath The path where the Word document will be saved. Default is "_Outputs/_Tables" in the current working directory.
#'
#' @return The saved Word document.
#'
#' @author Ali Guner
#'
#' @importFrom flextable fix_border_issues save_as_docx
#' @importFrom officer prop_section page_size page_mar
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' ag_flex_save(flex_obj, n = 1, orientation = "Landscape", mypath = "path/to/save")
#' }
#'
#' @seealso \code{\link{flextable::save_as_docx()}}
#'
#' @export




ag_flex_save <- function(flex_obj, n = "x", orientation = "Landscape", mypath = "_Outputs/_Tables"){

        if (!inherits(flex_obj, "flextable")) {
                stop(sprintf("Function `%s` supports only flextable objects.", "ag_flex_save()"))
        }

        if (!orientation %in% c("Landscape", "Portrait")) {

                stop("Invalid orientation. Use 'Landscape' or 'Portrait'.")

        }

        if(is.null(mypath)){
                mypath <- ""
        }


        width <- if (orientation == "Portrait") 14 else NULL

        pr_section <- officer::prop_section(
                                page_size = officer::page_size(orient = tolower(orientation),
                                                               width = width),
                                type = "nextPage",
                                page_margins = officer::page_mar()
                        )


        flex_obj %>%
                flextable::fix_border_issues() %>%
                flextable::save_as_docx(
                        path =  here::here(mypath, paste0(format(Sys.time(), "%Y%m%d_%H%M"),"_" , "Table_", n , ".docx")),
                                           pr_section = pr_section
                )

}


