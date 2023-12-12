#' @title Identify Factors with Low Expected Cell Frequencies (for Fisher test)
#'
#' @description
#' This function performs a contingency table analysis on specified factor or character variables in a given .dataset,
#' with an option to identify variables exhibiting expected cell frequencies less than 5 in more than 20% of cells.
#' It is designed to handle potential issues gracefully, providing informative warnings and excluding rows with
#' missing values in the strata variable when necessary. The function can display observed and/or expected values
#' tables based on user preferences.
#'
#' @param .dataset The .dataset containing the variables of interest.
#' @param strata The stratifying factor variable for the analysis.
#' @param table_vars A character vector of factor variable names to analyze.
#' @param silence Logical, indicating whether to suppress warnings. Default is TRUE.
#' @param observed_tables Logical, indicating whether to display contingency tables for observed values. Default is FALSE.
#' @param expected_tables Logical, indicating whether to display tables for expected values. Default is FALSE.
#'
#' @return A character vector of factor variable names with low expected cell frequencies, or NULL if none are found.
#' @author Ali Guner
#'
#' @examples
#' \dontrun{
#' # Usage example:
#' library(palmerpenguins)
#'
#' table_vars <- penguins %>%
#' dplyr::select(-species) %>%
#' names()
#'
#' strata <- "species"
#'
#' ag_fisher(penguins, strata, table_vars,  silence = TRUE, observed_tables = TRUE)
#' ag_fisher(penguins, strata, table_vars,   expected_tables = TRUE)
#' }
#'
#' @import dplyr
#' @import tidyselect
#'
#' @keywords contingency-tables fisher chi-square
#' @seealso \code{\link{chisq.test}}
#' @export
#'
#'
#'

ag_fisher <- function(.dataset, strata, table_vars, silence = TRUE, observed_tables = FALSE, expected_tables = FALSE) {
        library(dplyr)


        # Check if .dataset argument is missing
        if (missing(.dataset)) {
                stop("Missing required argument: .dataset")
        }

        # Check if strata argument is missing
        if (missing(strata)) {
                stop("Missing required argument: strata")
        }

        # Check if table_vars argument is missing
        if (missing(table_vars)) {
                stop("Missing required argument: table_vars")
        }

        # Check if .dataset is a data frame
        if (!is.data.frame(.dataset)) {
                stop("The '.dataset' argument should be a data frame.")
        }

        # Check if strata is a vector with a single element
        if (!is.vector(strata) || length(strata) != 1) {
                stop("The 'strata' argument should be a vector with a single element.")
        }

        # Check if table_vars is a vector
        if (!is.vector(table_vars)) {
                stop("The 'table_vars' argument should be a vector.")
        }


        # Check if strata variable of .dataset is factor or character
        if (!is.factor(.dataset[[strata]]) && !is.character(.dataset[[strata]])) {
                stop("The 'strata' variable of the '.dataset' should be a factor or character.")
        }




        # Select factor variables
        my_factors <- .dataset %>%
                dplyr::select(all_of({{ strata }}), tidyselect::all_of(table_vars)) %>%
                dplyr::select_if(function(var) is.factor(var) || is.character(var))




        # # Check for missing values in 'strata'
        # if(any(is.na(my_factors %>%
        #              dplyr::select(all_of({{ strata }}))))) {
        #         warning(paste0("Important! There are missing values in ", {{ strata }}, ". Rows with missing values in ",{{ strata }}, " will be removed from the analysis."))
        #         my_factors <- my_factors %>%
        #                 dplyr::filter(!is.na({{ strata }}))
        # }

        # Handle missing values in 'strata'
        missing_strata <- sum(is.na(my_factors[[1]]))
        if (missing_strata > 0) {
                warning(paste0("Important! There are ", missing_strata, " missing values in ", {{ strata }}, ". Rows with missing values in ", {{ strata }}, " will be removed from the analysis."))
                my_factors <- my_factors[!is.na(my_factors[[1]]), ]
        }


        # Get the variable names (excluding strata)

        non_strata_vars <- my_factors %>%
                dplyr::select(-tidyselect::all_of({{ strata }}))


        # Create an empty list to store the contingency tables
        contingency_tables <- list()


        # Calculate contingency tables for each variable
        for (var in names(non_strata_vars)) {
                contingency_table <- table(my_factors[[1]], non_strata_vars[[var]])

                # Check for zero counts in the contingency table
                if (any(contingency_table == 0)) {
                        if (!silence) {
                        warning("Contingency table with zero counts detected for variable: ", var)
                        }
                        contingency_tables[[var]] <- contingency_table
                } else {
                        contingency_tables[[var]] <- contingency_table
                }
        }



        # Show contingency tables (observed values) if observed_tables is TRUE
        if (!silence && observed_tables) {
                cat("Observed values:\n")
                for (i in seq_along(contingency_tables)) {
                        cat("Variable:", names(non_strata_vars)[i], "\n")
                        print(contingency_tables[[i]])
                }
        }

        # Show tables for expected values if expected_tables is TRUE
        if (!silence && expected_tables) {
                cat("Expected values:\n")
                for (i in seq_along(contingency_tables)) {
                        cat("Variable:", names(non_strata_vars)[i], "\n")
                        expected_values <- round(stats::chisq.test(contingency_tables[[i]])$expected, digits = 1) %>%
                                suppressWarnings()
                        print(expected_values)
                }
        }

        # Create an empty vector to store variable names with low expected values
        variables_low_expected_values <- character()

        # Iterate through each contingency table
        for (i in seq_along(contingency_tables)) {
                # Get the current contingency table and its variable name
                current_table <- contingency_tables[[i]]
                variable_name <- names(contingency_tables)[i]

                # Calculate the chi-square test and expected values
                expected_values <- stats::chisq.test(current_table)$expected %>%
                        suppressWarnings()
                # expected_values <- result$expected

                # Count the cells where the expected value is less than 5
                cells_less_than_5 <- sum(expected_values < 5, na.rm = TRUE)

                # Calculate the percentage of cells with expected values less than 5
                percentage_less_than_5 <- cells_less_than_5 / length(expected_values)

                # Check if the percentage exceeds 20%
                if (percentage_less_than_5 > 0.2) {
                        variables_low_expected_values <- c(variables_low_expected_values, variable_name)
                }
        }

        # Return NULL if no variables meet the condition
        if (length(variables_low_expected_values) == 0) {
                if (!silence) {
                message(paste("Checked variables:", toString(names(non_strata_vars))))
                message(paste("Stratified by:", strata))
                message("There is no factor/character variable with the expected cell frequencies are less than 5 in more than 20% of the cells in a contingency table.")
                }
                        return(NULL)

        } else {
                if(!silence){
                        message(paste("Checked variables:", toString(names(non_strata_vars))))
                        message(paste("Stratified by:", strata))
                        message(paste("Variables may require Fisher test: ", toString(variables_low_expected_values)))
                }
                return(variables_low_expected_values)
        }
}



