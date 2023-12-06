#' @title Calculate Shapiro-Wilk p-values
#' @description
#' This function calculates Shapiro-Wilk p-values for normality testing of numeric variables in a dataset.
#'
#'
#' @param dataset The input dataset.
#' @param strata (Optional) A column name in the dataset to stratify the analysis by. Default is \code{NULL}.
#' @param table_vars A character vector of variable names to include in the analysis.
#' @param asteriks if \code{TRUE}, test results will be reader-friendly. Default is \code{TRUE}.
#'
#' @return A tibble with columns for the variable names and Shapiro-Wilk p-values.
#'
#' @examples
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>% names()
#'
#' ag_shapiro_results(penguins, "species", table_vars_1)
#'
#' @import dplyr
#' @import tidyr
#' @export




ag_shapiro_results <- function(dataset, strata = NULL, table_vars = NULL, asteriks = TRUE){

        if(is.null(table_vars)){

                message("No table_vars were defined. All numeric variables in dataset will be evaluated")
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

        if (is.null(strata)) {

                shapiro_results <- dataset %>%
                        dplyr::select(tidyselect::all_of(table_vars)) %>%
                        dplyr::summarise(across(where(is.numeric), ~ stats::shapiro.test(.)$p.value)) %>%
                        tidyr::pivot_longer(tidyselect::everything(),
                                            names_to = "variable",
                                            values_to = "shapiro_results")

                } else {

                shapiro_results <- dataset %>%
                        dplyr::select(tidyselect::all_of(table_vars), {{ strata }}) %>%
                        dplyr::filter(!is.na(.data[[strata]])) %>%
                        dplyr::summarise(across(where(is.numeric), ~ stats::shapiro.test(.)$p.value), .by = {{ strata }}) %>%
                        # I guess, after dplyr updates, group_by, ungroup behaviour was changed. I solved with .by argument.
                        tidyr::pivot_longer(cols = -c(1),
                                            names_to = "variable",
                                            values_to = "shapiro_results")
        }




        if(asteriks){
                shapiro_results <- shapiro_results %>%
                        dplyr::mutate(shapiro_results_ns = if_else(shapiro_results < 0.001, "<0.001", as.character(round(shapiro_results, 3))),
                                      shapiro_results_ns = if_else(shapiro_results < 0.05, paste0(shapiro_results_ns, "*"), shapiro_results_ns))
        }


        return(shapiro_results)
}










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
#' If NULL, function will use all numeric variables in dataset, except for strata if present (NOT RECOMMENDED,
#' it is better idea to define table_vars)
#' @param names Logical. If TRUE, returns the names of non-normally distributed variables;
#' if FALSE, returns the indices of these variables. Default is TRUE
#' @param silence Logical. If FALSE, displays informative messages about the analysis. Default is FALSE
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
#' ag_shapiro(dataset = iris, table_vars = table_vars, silence = TRUE, names = FALSE)
#' }
#'
#'
#' @import dplyr
#' @import tidyr
#' @import tidyselect
#'
#' @export
#'
#'

# xxx <- function(dataset, strata = NULL) {
#         if (is.null(strata) || !is.numeric(dataset[[strata]])) {
#                 dataset %>%
#                         dplyr::select(where(is.numeric))
#         } else {
#                 dataset %>%
#                         dplyr::select(where(is.numeric), -{{ strata }})
#         }
# }
#
#        xxx(penguins, strata = "species")
#
#         dataset %>%
#                 dplyr::select(where(is.numeric)) %>%
#                 dplyr::select(-one_of(.data[[strata]]))
# }
#
# xxx(penguins, strata = "species")
#
#
#
#
# penguins %>%
#         mutate(species = as.numeric(species)) %>%
#         dplyr::select(-species, where(is.numeric))



ag_shapiro <- function(dataset, strata = NULL, table_vars = NULL, silence = FALSE, names = TRUE){

        # Check if dataset is a data frame
        if (!is.data.frame(dataset)) {
                stop("Input 'dataset' must be a data frame.")
        }

        # Check if strata is NULL or a vector
        if (!is.null(strata) && !is.vector(strata)) {
                stop("Input 'strata' must be NULL or a vector.")
        }



        if(is.null(table_vars)){

                if(silence){
                        message("Because all variables were included, silence argument were converted to FALSE")
                        silence <- FALSE
                }


                message("No table_vars were defined. All numeric variables in dataset will be evaluated")
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


        # Check if table_vars is a vector
        if (!is.vector(table_vars)) {
                stop("Input 'table_vars' must be a vector or NULL.")
        }


        checked_variables <- dataset %>%
                dplyr::select(tidyselect::all_of(table_vars)) %>%
                dplyr::select_if(where(is.numeric)) %>%
        names()


        if (is.null(strata)) {

                shapiro_results <- ag_shapiro_results(dataset, strata = strata, table_vars, asteriks = FALSE) %>%
                        dplyr::filter(shapiro_results < 0.05) %>%
                        dplyr::distinct(variable) %>%
                        dplyr::pull(variable)

                if (!silence) {

                message("Shapiro-Wilk test results for normality (No strata variable):")
                message(paste("Checked variables:", toString(checked_variables)))
                message(paste("Non-normally distributed variables:", toString(shapiro_results)))

                }

        } else {



                shapiro_results <- ag_shapiro_results(dataset, strata = strata, table_vars, asteriks = FALSE) %>%
                        dplyr::filter(shapiro_results < 0.05) %>%
                        dplyr::distinct(variable) %>%
                        dplyr::pull(variable)

                if (!silence) {

                message("Shapiro-Wilk test results for normality within strata:")
                message(paste("Checked variables:", toString(checked_variables)))
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
#' ag_non_param_vars(dataset = iris,
#' strata = dependent,
#' table_vars = table_vars
#' )
#' }
#' @import dplyr
#' @import tidyr
#'
#'
#'


ag_non_param_vars <- function(dataset, strata = NULL, table_vars){
        # To use in ff functions (but may not be needed because the defaults are same)

        non_param_vars <- ag_shapiro(dataset = dataset, strata = strata, table_vars = table_vars, names = FALSE, silence = TRUE)

        non_param_vars
}







