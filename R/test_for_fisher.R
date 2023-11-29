
#' @title Identify Factors with Low Expected Cell Frequencies (for Fisher test)
#'
#' @description
#' This function analyzes contingency tables for factor variables within a dataset.
#' It checks for zero counts and low expected cell frequencies in the chi-square test.
#' Factors with expected cell frequencies less than 5 in more than 20% of the cells are flagged.
#'
#'
#' @param dataset The dataset containing the variables of interest.
#' @param strata The stratifying factor variable for the analysis.
#' @param table_vars A character vector of factor variable names to analyze.
#' @param silence Logical, indicating whether to suppress warnings. Default is TRUE.
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
#' ag_fisher(penguins, table_vars = table_vars, strata = strata, silence = FALSE)
#'
#' }
#'
#' @import dplyr
#' @import tidyselect
#'
#' @keywords contingency-tables warning suppression
#' @seealso \code{\link{chisq.test}}
#' @export
#'
#'
#'

ag_fisher <- function(dataset, strata, table_vars, silence = TRUE) {
        library(dplyr)


        # Select factor variables
        my_factors <- dataset %>%
                dplyr::select({{ strata }}, tidyselect::all_of(table_vars)) %>%
                dplyr::select_if(is.factor)

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

        # Create an empty vector to store variable names with low expected values
        variables_low_expected_values <- character()

        # Iterate through each contingency table
        for (i in seq_along(contingency_tables)) {
                # Get the current contingency table and its variable name
                current_table <- contingency_tables[[i]]
                variable_name <- names(contingency_tables)[i]

                # Calculate the chi-square test and expected values
                expected_values <- stats::chisq.test(current_table)$expected
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
                warning("There is no factor variable with the expected cell frequencies are less than 5 in more than 20% of the cells in a contingency table.")
                }
                        return(NULL)
        } else {
                return(variables_low_expected_values)
        }
}
