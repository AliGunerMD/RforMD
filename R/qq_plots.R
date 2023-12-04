
single_qq_plot <- function(dataset, strata, table_vars) {


        numeric_variables <- dataset %>%
                dplyr::select(tidyselect::all_of(table_vars)) %>%
                dplyr::select_if(where(is.numeric)) %>%
                names()

        noempty <- dataset %>%
                filter(!is.na(.data[[strata]])) %>%
                droplevels()

        n_empty_strata <- nrow(dataset) - nrow(noempty)

        # Function to compute Shapiro-Wilk test and format the result
        annotations <- ag_shapiro_results(noempty, strata = strata, table_vars = table_vars) %>%
                mutate(shapiro_results = format(shapiro_results, scientific = TRUE, digits = 3),
                       shapiro_results = as.numeric(shapiro_results)) %>%
                mutate(values_text = paste0("p value: ", shapiro_results))




        long_noempty <- noempty %>%
                pivot_longer(all_of(numeric_variables),
                             names_to = "variable",
                             values_to = "values") %>%
                filter(!is.na(values))

        # long_noempty <- long_dataset

        n_color_x <- length(unique(dataset[[strata]]))
        n_color_y <- length(numeric_variables)



        strip <- strip_themed(background_x = elem_list_rect(fill = ag_make_colors(n_color_x)$light), by_layer_x = FALSE,
                              background_y = elem_list_rect(fill = ag_colors("grays")[2]), by_layer_y = TRUE)




        plot <- long_noempty %>%
                ggplot() +
                geom_qq(aes(sample = values)) +
                geom_qq_line(aes(sample = values)) +
                facet_grid2(variable ~ .data[[strata]], scales = "free",
                            switch = "y",
                            independent = "all",
                            strip = strip,
                            labeller = as_labeller(\(x) stringr::str_to_title(x))) +
                geom_text(data = annotations, aes(x = .data[[strata]],
                                                  label = values_text, fontface = ifelse(shapiro_results <0.05, "bold", "plain")),
                          x = -Inf, y = Inf, hjust = 0, vjust = 1, size = 4) +
                labs(title = "QQ plots for numeric variables",
                     x = "\nTheoretical Quantiles",
                     y = "Sample Quantiles\n",
                     caption = paste0("There were ", n_empty_strata, " missing values in strata (", strata, ")
                                      p value was calculated with Shapiro-Wilk test")) +
                theme_light() +
                theme(strip.text.x = element_text(color = "black", face = "bold"),
                      strip.text.y = element_text(color = "black"),
                      plot.title = element_text(hjust = .5, size = 14, face = "bold"))


        return(list(plot = plot, n_empty_strata = n_empty_strata))

}

# Example usage:
single_qq_plot(penguins, "sex", table_vars_1)$plot




ag_qq_plots <- function(dataset, strata, table_vars) {
        single_plot <- single_qq_plot(dataset, strata, table_vars)
        return(single_plot$plot)
}

ag_qq_plots(penguins, "sex", table_vars_1)







ag_qq_plots_multiple <- function(dataset, strata, table_vars, n_row = 5) {

        numeric_variables <- dataset %>%
                dplyr::select(tidyselect::all_of(table_vars)) %>%
                dplyr::select_if(where(is.numeric)) %>%
                names()


        num_plots <- ceiling(length(numeric_variables) / n_row)

        for (i in 1:num_plots) {
                start_index <- (i - 1) * n_row + 1
                end_index <- min(i * n_row, length(numeric_variables))
                selected_vars <- numeric_variables[start_index:end_index]

                n_empty_strata <- single_qq_plot(dataset, strata, selected_vars)$n_empty_strata

                plot <- ag_qq_plots(dataset, strata, selected_vars)


                # Create the caption with information about missing values and the Shapiro-Wilk test
                caption <- paste0(
                        # length(selected_vars), " of ", length(numeric_variables) ," numeric variables (Index: ", start_index, "-", end_index, ") are being plotted.\n",
                        "There were ", n_empty_strata, " missing values in strata (", strata, ").\n",
                        "p value was calculated with Shapiro-Wilk test."
                )

                title <- paste0("QQ plot (", i, " of ", num_plots ," plots) for ", length(selected_vars), "/", length(numeric_variables) ," numeric variables (Index: ", start_index, "-", end_index, ")")


                # Add the caption to the plot
                plot <- plot + ggplot2::labs(caption = caption,
                                             title = title)


                print(plot)
        }
}



ag_qq_plots_multiple(new_pen, "species", table_vars_2, 5)


