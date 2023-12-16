#' @title Generate QQ plots for numeric variables
#' @description
#' This function generates QQ plots for numeric variables in a dataset. It allows for stratified analysis based on a specified variable.
#'
#' @param dataset The input dataset.
#' @param strata The variable used for stratification. If NULL, no stratification is performed.
#' @param table_vars Optional. The variables to be included in the QQ plots. If NULL, all numeric variables in the dataset are used.
#'
#' @return A list containing the generated plot and the number of missing values in the stratification variable (if applicable).
#'
#' @examples
#' \dontrun{
#' ag_qq_plot_single(penguins, strata = NULL)$plot
#'
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>% names()
#' ag_qq_plot_single(penguins, strata = "species", table_vars = table_vars_1)$plot
#' }
#'
#' @author Ali Guner
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggh4x facet_grid2
#' @importFrom ggh4x strip_themed
#' @importFrom viridisLite viridis
#' @importFrom rlang .data
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_longer
#'


ag_qq_plot_single <- function(dataset, strata = NULL, table_vars = NULL) {


        if(is.null(table_vars)){

                message("No table_vars were defined. Decide which variables you want to check!! or \nAll numeric variables in dataset will be evaluated (not a good idea) or \nUse ag_qq_plots() instead.")
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

        if(!is.null(strata)){
                noempty <- dataset %>%
                        dplyr::filter(!is.na(.data[[strata]])) %>%
                        droplevels()

                strata_levels <- levels(dataset[[strata]])
        } else {
                noempty <- dataset
                strata_levels <- NULL
        }

# ag_shapiro_results


                n_empty_strata <- nrow(dataset) - nrow(noempty)


        # Function to compute Shapiro-Wilk test and format the result
        annotations <- ag_shapiro_results(noempty, strata = strata, table_vars = table_vars, asteriks = FALSE) %>%
                dplyr::mutate(shapiro_results = format(shapiro_results, scientific = TRUE, digits = 3),
                       shapiro_results = as.numeric(shapiro_results)) %>%
                dplyr::mutate(values_p = paste0("p value: ", shapiro_results),
                       values_label = paste0(variable))




        long_noempty <- noempty %>%
                tidyr::pivot_longer(all_of(numeric_variables),
                             names_to = "variable",
                             values_to = "values") %>%
                dplyr::filter(!is.na(values))



        if(!is.null(strata)){
                n_color_x <- length(unique(dataset[[strata]]))
        } else {
                n_color_x <- 1
        }

        n_color_y <- length(numeric_variables)





        if(!is.null(strata)){

                strip <- ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = viridisLite::viridis(n_color_x, begin = 0.6)), by_layer_x = FALSE,
                                      background_y = ggh4x::elem_list_rect(fill = ag_colors_scale("gray", num_colors = n_color_y, start = 0.7, end = 0.4)), by_layer_y = FALSE)

        } else {

                strip <- ggh4x::strip_themed(background_y = ggh4x::elem_list_rect(fill = viridisLite::viridis(n_color_y, begin = 0.7, option = "G")), by_layer_x = FALSE) # mako
        }





        if(!is.null(strata)){
                caption_lab <- paste0("There were ", n_empty_strata, " missing values in strata (", strata, ")
                                      p value was calculated with Shapiro-Wilk test")
        } else {
                caption_lab <- paste0("No strata was defined.
                                       p value was calculated with Shapiro-Wilk test")
        }



        title_lab <- "QQ plots for numeric variables"
        x_lab <- "\nTheoretical Quantiles"
        y_lab <- "Sample Quantiles\n"
        my_label <- ggplot2::as_labeller(\(x) if_else(x %in% numeric_variables & !x %in% strata_levels, " ", stringr::str_to_title(x)))


        if(!is.null(strata)){

                plot <- long_noempty %>%
                        ggplot2::ggplot() +
                        ggplot2::geom_qq(aes(sample = values)) +
                        ggplot2::geom_qq_line(aes(sample = values)) +
                        ggh4x::facet_grid2(variable ~ .data[[strata]], scales = "free",
                                    switch = "y",
                                    independent = "all",
                                    strip = strip,
                                    labeller = my_label) +
                        ggplot2::geom_text(data = annotations, aes(x = .data[[strata]],
                                                          label = values_label, fontface = ifelse(shapiro_results <0.05, "bold", "plain")),
                                  x = -Inf, y = Inf, hjust = 0, vjust = 1, size = 3) +
                        ggplot2::geom_text(data = annotations, aes(x = .data[[strata]],
                                                          label = values_p, fontface = ifelse(shapiro_results <0.05, "bold", "plain")),
                                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 3) +
                        ggplot2::labs(title = title_lab,
                             x = x_lab,
                             y = y_lab,
                             caption = caption_lab) +
                        ggplot2::theme_light() +
                        ggplot2::theme(strip.text.x = element_text(color = "black", face = "bold"),
                              strip.text.y = element_text(color = "black"),
                              plot.title = element_text(hjust = .5, size = 14, face = "bold"))
        } else {
                plot <- long_noempty %>%
                        ggplot2::ggplot() +
                        ggplot2::geom_qq(aes(sample = values)) +
                        ggplot2::geom_qq_line(aes(sample = values)) +
                        ggh4x::facet_grid2(variable ~. , scales = "free",
                                    switch = "y",
                                    independent = "all",
                                    strip = strip,
                                    labeller = my_label) +
                        ggplot2::geom_text(data = annotations, aes(x = variable,
                                                          label = values_label, fontface = ifelse(shapiro_results <0.05, "bold", "plain")),
                                  x = -Inf, y = Inf, hjust = 0, vjust = 1, size = 4) +
                        ggplot2::geom_text(data = annotations, aes(x = variable,
                                                          label = values_p, fontface = ifelse(shapiro_results <0.05, "bold", "plain")),
                                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 4) +
                        ggplot2::labs(title = title_lab,
                             x = x_lab,
                             y = y_lab,
                             caption = caption_lab) +
                        ggplot2::theme_light() +
                        ggplot2::theme(strip.text.x = element_text(color = "black", face = "bold"),
                              # strip.text.y = element_blank(),
                              plot.title = element_text(hjust = .5, size = 14, face = "bold"))


        }


        return(list(plot = plot, n_empty_strata = n_empty_strata))

}





#' @title Generate multiple QQ plots for all/selected numeric variables
#' @description
#' This function generates multiple QQ plots for numeric variables in a dataset. It allows for stratified analysis based on a specified variable.
#'
#' @param dataset The input dataset.
#' @param strata The variable used for stratification. If NULL, no stratification is performed.
#' @param table_vars Optional. The variables to be included in the QQ plots. If NULL, all numeric variables in the dataset are used.
#' @param n_row The number of plots to display in each plot. Default is 5.
#'
#' @return None (plots are displayed)
#' @seealso [func(ag_qq_plot_single)]
#'
#' @examples
#' \dontrun{
#' ag_qq_plots(penguins)
#' #' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>% names()
#'
#' ag_qq_plots(penguins, "sex", table_vars_1)
#' }
#'
#' @author Ali Guner
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom ggh4x facet_grid2
#' @importFrom ggh4x strip_themed
#' @importFrom rlang .data
#' @importFrom stringr str_to_title
#' @importFrom scales rescale
#' @importFrom viridisLite viridis
#'
#' @export



ag_qq_plots <- function(dataset, strata = NULL, table_vars = NULL, n_row = 5) {

        if(length(strata) > 1){
                stop("Could it be possible that the strata argument is not entered correctly?")
        }


        if(is.null(table_vars)){

                message("No table_vars were defined. Decide which variables you want to check!! or \nAll numeric variables in dataset will be evaluated (not a good idea)")
                if (is.null(strata)) {
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


        num_plots <- ceiling(length(numeric_variables) / n_row)


        for (i in 1:num_plots) {
                start_index <- (i - 1) * n_row + 1
                end_index <- min(i * n_row, length(numeric_variables))
                selected_vars <- numeric_variables[start_index:end_index]

                n_empty_strata <- ag_qq_plot_single(dataset, strata, selected_vars)$n_empty_strata

                plot <- ag_qq_plot_single(dataset, strata, selected_vars)$plot


                # Create the caption with information about missing values and the Shapiro-Wilk test
                if(!is.null(strata)){
                        caption_lab <- paste0(
                                "There were ", n_empty_strata, " missing values in strata (", strata, ").\n",
                                "p value was calculated with Shapiro-Wilk test."
                        )
                } else {
                        caption_lab <- paste0("No strata was defined.
                                       p value was calculated with Shapiro-Wilk test")
                }



                title <- paste0("QQ plots (", i, " of ", num_plots ," plots) for ", length(selected_vars), "/", length(numeric_variables) ," numeric variables (Index: ", start_index, "-", end_index, ")")


                # Add the caption to the plot
                plot <- plot + ggplot2::labs(caption = caption_lab,
                                             title = title)


                print(plot)
        }
}







































