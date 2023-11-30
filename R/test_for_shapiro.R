

#'
#' @title Perform Shapiro-Wilk Test for Normality
#'
#' @description
#' This function conducts the Shapiro-Wilk test for normality on numeric variables in a dataset,
#' either for the entire dataset or within specified strata. It returns the names or counts of
#' variables that are found to be non-normally distributed based on a significance level of 0.05.
#' Before this test, all variables which are desired to be tests, should be defined in explanatory vector.
#'
#' @param dataset A data frame containing the variables of interest.
#' @param strata An optional grouping variable for stratified analysis. Default is NULL.
#' @param table_vars A character vector of variable names to be tested for normality. Can be defined with names(select()) functions.
#' @param names Logical. If TRUE, returns the names of non-normally distributed variables;
#' if FALSE, returns the indices of these variables. Default is FALSE.
#' @param message Logical. If TRUE, displays informative messages about the analysis. Default is FALSE.
#
#' @return A character vector containing the names of non-normally distributed variables or an integer count,
#' based on the specified value of the 'names' argument.
#' @author Ali Guner
#'
#'
#' @examples
#' \dontrun{
#'
#' table_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#' or
#' table_vars <- iris %>%
#' dplyr::select(-Species) %>%
#' names()

#' dependent <- "Species"
#'
#' ag_shapiro(dataset = iris, table_vars = table_vars)
#' ag_shapiro(dataset = iris, table_vars = table_vars, names = TRUE, message = TRUE)
#' }
#'
#'
#' @import dplyr
#' @import tidyr
#' @import tidyselect
#'
#' @export


ag_shapiro <- function(dataset, strata = NULL, table_vars, names = FALSE, message = FALSE){

        # Check if dataset is a data frame
        if (!is.data.frame(dataset)) {
                stop("Input 'dataset' must be a data frame.")
        }

        # Check if strata is NULL or a vector
        if (!is.null(strata) && !is.vector(strata)) {
                stop("Input 'strata' must be NULL or a vector.")
        }

        # Check if table_vars is a vector
        if (!is.vector(table_vars)) {
                stop("Input 'table_vars' must be a vector.")
        }




        if (is.null(strata)) {

                shapiro_results <- dataset %>%
                        dplyr::select(tidyselect::all_of(table_vars)) %>%
                        dplyr::summarise(across(where(is.numeric), ~ stats::shapiro.test(.)$p.value)) %>%
                        tidyr::pivot_longer(tidyselect::everything(),
                                            names_to = "variable",
                                            values_to = "shapiro_results") %>%
                        dplyr::filter(shapiro_results < 0.05) %>%
                        dplyr::distinct(variable) %>%
                        dplyr::pull(variable)

                if (message) {

                message("Shapiro-Wilk test results for normality:")
                message(paste("Variables:", toString(table_vars)))
                message(paste("Non-normally distributed variables:", toString(shapiro_results)))

                }

        } else {

                shapiro_results <- dataset %>%
                        dplyr::select(tidyselect::all_of(table_vars), {{ strata }}) %>%
                        dplyr::filter(!is.na({{ strata }})) %>%
                        dplyr::summarise(across(where(is.numeric), ~ stats::shapiro.test(.)$p.value), .by = {{ strata }}) %>%
                        # I guess, after dplyr updates, group_by, ungroup behaviour was changed. I solved with .by argument.
                        tidyr::pivot_longer(cols = -c(1),
                                            names_to = "variable",
                                            values_to = "shapiro_results") %>%
                        dplyr::filter(shapiro_results < 0.05) %>%
                        dplyr::distinct(variable) %>%
                        dplyr::pull(variable)

                if (message) {

                message("Shapiro-Wilk test results for normality within strata:")
                message(paste("Variables:", toString(table_vars)))
                message(paste("Stratified by:", strata))
                message(paste("Non-normally distributed variables:", toString(shapiro_results)))

                }
        }


        if (names){

                non_param_vars <- shapiro_results

        } else {
                non_param_vars <- which(table_vars %in% shapiro_results)
        }


                return(non_param_vars)


}


#' @title Extract indices for Non-Normally Distributed Variables
#' @description
#' This function serves as a helper for finalfit functions, extracting the indices of non-normally
#' distributed variables based on the Shapiro-Wilk test for normality. It utilizes the
#' `ag_shapiro` function with specific parameters. Good to use as cont_nonpara argument in summary_factorlist().
#' names and message arguments are always FALSE.
#'
#' @param dataset A data frame containing the variables of interest.
#' @param strata An optional grouping variable for stratified analysis. Default is NULL.
#' @param table_vars A character vector of variable names to be tested for normality.
#'
#' @return A vector containing the indices of non-normally distributed variables.
#'
#' @author Ali Guner
#' @examples
#' \dontrun{
#' #' table_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#' or
#' table_vars <- iris %>%
#' dplyr::select(-Species) %>%
#' names()

#' dependent <- "Species"
#'
#' ag_ff_non_param_vars(dataset = iris,
#' strata = dependent,
#' table_vars = table_vars
#' )
#' }
#' @import dplyr
#' @import tidyr
#' @export
#'
#'
#'


ag_ff_non_param_vars <- function(dataset, strata = NULL, table_vars){
        # To use in finalfit functions (but may not be needed because the defaults are same)

        non_param_vars <- ag_shapiro(dataset = dataset, strata = strata, table_vars = table_vars, names = FALSE, message = FALSE)

        non_param_vars
}









