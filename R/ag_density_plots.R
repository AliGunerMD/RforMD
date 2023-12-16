#' @title Generate Density Plots for Numeric Variables
#' @description
#' This function generates density plots for numeric variables in a dataset. It can also handle stratified plots if a strata variable is provided.
#'
#' @param dataset The input dataset.
#' @param strata (Optional) The name of the strata variable for stratified plots.
#' @param table_vars (Optional) The names of the variables to be plotted. If not provided, all numeric variables in the dataset will be plotted.
#'
#' @return A density plot for each numeric variable.
#' @author Ali Guner
#' @examples
#' \dontrun{
#' # Generate density plots for numeric variables
#' ag_density_plots(penguins)
#'
#' # Generate stratified density plots
#' ag_density_plots(penguins, strata = "species")
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_to_sentence
#' @importFrom tidyr pivot_longer
#' @export



ag_density_plots <- function(dataset, strata = NULL, table_vars = NULL) {
  if (is.null(table_vars)) {
    message("No table_vars were defined. Decide which variables you want to check!! or \nAll numeric variables in dataset will be evaluated.")
    if (is.null(strata) || !is.numeric(dataset[[strata]])) {
      table_vars <- dataset %>%
        dplyr::select(where(is.numeric)) %>%
        names()
    } else {
      table_vars <- dataset %>%
        dplyr::select(where(is.numeric), -{{ strata }}) %>%
        names()
    }
  }

  numeric_variables <- dataset %>%
    dplyr::select(tidyselect::all_of(table_vars)) %>%
    dplyr::select_if(where(is.numeric)) %>%
    names()

  if (!is.null(strata)) {
    noempty <- dataset %>%
      dplyr::filter(!is.na(.data[[strata]])) %>%
      droplevels()

    strata_levels <- levels(dataset[[strata]])
  } else {
    noempty <- dataset
    strata_levels <- NULL
  }


  n_empty_strata <- nrow(dataset) - nrow(noempty)


  long_noempty <- noempty %>%
    tidyr::pivot_longer(all_of(numeric_variables),
      names_to = "variable",
      values_to = "values"
    ) %>%
    dplyr::filter(!is.na(values))



  if (!is.null(strata)) {
    caption_lab <- paste0("There were ", n_empty_strata, " missing values in strata (", strata, ")")
  } else {
    caption_lab <- paste0("No strata was defined")
  }



  title_lab <- "Density plots for numeric variables"
  x_lab <- " "
  y_lab <- "Density\n"
  my_label <- ggplot2::as_labeller(\(x) stringr::str_to_sentence(x))



  if (!is.null(strata)) {
    long_noempty %>%
      ggplot2::ggplot() +
      # ggplot(aes(values, fill = .data[[strata]], color = .data[[strata]])) +
      ggplot2::geom_density(aes(values, fill = .data[[strata]], color = .data[[strata]]), alpha = .3) +
      ggplot2::facet_wrap(. ~ variable,
        scales = "free") +
      ggplot2::theme_light() +
      ggplot2::labs(
        title = title_lab,
        x = x_lab,
        y = y_lab,
        caption = caption_lab
      ) +
      RforMD::scale_color_karadeniz() +
      RforMD::scale_fill_karadeniz() +
      ggplot2::theme(
        strip.text.x = element_text(color = "black", face = "bold"),
        strip.text.y = element_text(color = "black"),
        strip.background = element_rect(fill = "#ECECEC"),
        plot.title = element_text(hjust = .5, size = 14, face = "bold"),
        legend.position = "bottom",
        panel.grid = element_blank()
      )
  } else {
    long_noempty %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(aes(values), color = "#5A0E27", fill = "#75B7E5", alpha = .5) +
      ggplot2::facet_wrap(. ~ variable,
        scales = "free") +
      ggplot2::labs(
        title = title_lab,
        x = x_lab,
        y = y_lab,
        caption = caption_lab
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        strip.text.x = element_text(color = "black", face = "bold"),
        strip.background = element_rect(fill = "#ECECEC"),
        # strip.text.y = element_blank(),
        plot.title = element_text(hjust = .5, size = 14, face = "bold"),
        legend.position = "bottom",
        panel.grid = element_blank()
      )
  }
}
