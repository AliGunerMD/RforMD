#' @title Aggregated Summary Factor List (extras for finalfit::summary_factorlist)
#'
#' @description
#'
#' Generate aggregated summary statistics and tests for categorical and continuous variables.
#'
#' This function generates summary statistics and statistical tests for both categorical and continuous
#' variables in a dataset. It is designed to work with the `finalfit` package and provides flexibility
#' in specifying the type of summary statistics and tests to be performed.

#' @param dataset A data frame containing the variables of interest.
#' @param dependent The dependent variable (outcome) in the analysis, should be a factor.
#' @param table_vars A character vector specifying the explanatory variables (predictors) for the analysis.
#'                  These can be factors, characters, or numerics.
#' @param na_include Logical, indicating whether missing values should be included in the analysis.
#'                  Default is \code{TRUE}.
#' @param row_col_sums A character string specifying the type of summary statistics to be generated.
#'                    Options include "row_based" for row-based summaries, "col_based" for column-based summaries,
#'                    and "row_col_based" for both row and column-based summaries. Default is "row_col_based".
#' @param all_cont A character string specifying the approach for continuous variables. Options include
#'                "parametric" for parametric tests (mean), "nonparametric" for non-parametric tests (median),
#'                and "shapiro" for using the Shapiro-Wilk test for normality and choosing between parametric
#'                and non-parametric accordingly. Default is "shapiro".
#' @param ... Additional arguments to be passed to the \code{\link{finalfit::summary_factorlist}} function.
#'
#' @return A data frame containing the aggregated summary statistics and test results.
#'
#' @examples
#' \dontrun{
#' library(palmerpenguins)
#' dependent <- "species"
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>% names()
#' ag_ff_summary_factorlist(penguins, dependent, table_vars_1, na_include = TRUE, row_col_sums = "row_col_based", all_cont = "parametric")
#' }
#'
#' @import finalfit
#' @import dplyr
#'
#' @seealso \code{\link{finalfit::summary_factorlist}}, \code{\link{ag_shapiro}}
#'
#' @author Ali Guner
#'
#' @keywords dataset summary analysis EDA
#'
# #' NOT Needed @export






ag_ff_summary_factorlist <- function(dataset, dependent, table_vars,
                                     na_include = TRUE,                         # Change only you want to see missing row
                                     row_col_sums = "row_col_based",
                                     all_cont = "shapiro",
                                     ...) {

        # Check if the dependent variable is a factor (not mandatory but good for further arrangements)
        if (!is.factor(dataset[[dependent]])) {
                stop("The dependent variable should be a factor. Please check the data type.")
                return(NULL)
        }


        # Handle missing values in the dependent variable
        missing_dependents <- sum(is.na(dataset[[dependent]]))
        if (missing_dependents > 0) {
                warning(paste0("Important! There are ", missing_dependents, " missing values in ", {{ dependent }}, ". Rows with missing values in ", {{ dependent }}, " will be removed from the analysis."))
                dataset <- dataset[!is.na(dataset[[dependent]]), ]
        }



        # Check data types of table_vars
        valid_vars <- sapply(table_vars, function(var) {
                if (is.factor(dataset[[var]]) || is.character(dataset[[var]]) || is.numeric(dataset[[var]])) {
                        return(var)
                }
        })

        valid_vars <- as.vector(unlist(valid_vars))
        invalid_vars <- setdiff(table_vars, valid_vars)

        if (length(invalid_vars) > 0) {
                stop(paste("Invalid data type for the following variables:", paste(invalid_vars, collapse = ", "), ".
               Should be factor, character, or numeric!!"))
        }


        # Check for continuous variables

        if (all_cont == "nonparametric") {
                cont <- "median"
                cont_nonpara <- NULL
        } else if (all_cont == "parametric") {
                cont <- "mean"
                cont_nonpara <- NULL
        } else if (all_cont == "shapiro") {
                cont <- "mean"
                cont_nonpara <- ag_shapiro(dataset, table_vars, strata = dependent)
        } else {
                stop("Invalid value for 'all_cont'. Use 'parametric', 'nonparametric', or 'shapiro'.")
        }

        # Determine the column argument based on row_col_sums
        if (row_col_sums == "row_based") {
                column <- FALSE
        } else if (row_col_sums == "col_based") {
                column <- TRUE
        } else if (row_col_sums == "row_col_based") {
                row_based <- dataset %>%
                        finalfit::summary_factorlist(
                                dependent = dependent,
                                explanatory = table_vars,
                                column = FALSE,  # Set column to FALSE for row-based
                                p = TRUE,
                                na_include = na_include,
                                total_col = TRUE,
                                cont = cont,
                                cont_nonpara = cont_nonpara,
                                cont_range = TRUE,
                                ...
                        ) %>%
                        suppressWarnings()

                col_based <- dataset %>%
                        finalfit::summary_factorlist(
                                dependent = dependent,
                                explanatory = table_vars,
                                column = TRUE,  # Set column to TRUE for col-based
                                p = TRUE,
                                na_include = na_include,
                                total_col = TRUE,
                                cont = cont,
                                cont_nonpara = cont_nonpara,
                                cont_range = TRUE,
                                ...
                        ) %>%
                        suppressWarnings() %>%
                        dplyr::select(Total)

                # Combine both row and col based outputs with cbind
                combined_ff <- cbind(row_based %>% select(-Total), col_based) %>%
                        dplyr::relocate(Total, .before = p)

                return(combined_ff)
        } else {
                stop("Invalid value for row_col_sums. Use 'row_based', 'col_based', or 'row_col_based'.")
        }

        # Perform the rest of the operations
        combined_ff <- dataset %>%
                finalfit::summary_factorlist(
                        dependent = dependent,
                        explanatory = table_vars,
                        column = column,
                        p = TRUE,
                        na_include = na_include,
                        total_col = TRUE,
                        cont = cont,
                        cont_nonpara = cont_nonpara,
                        cont_range = TRUE,
                        ...
                ) %>%
                suppressWarnings()

        return(combined_ff)
}






#' @title Aggregated Summary with Shapiro arrangement / Fisher Correction
#'
#' @description
#'
#' Generate aggregated summary statistics and tests for categorical and continuous variables,
#' with an option for Fisher's correction in contingency tables.
#'
#' This function extends the functionality of the `ag_ff_summary_factorlist` function by introducing
#' Fisher's correction for expected cell frequencies less than 5 in more than 20% of the cells in a
#' contingency table. The function provides flexibility in specifying the type of summary statistics
#' and tests to be performed, including the option for Fisher's correction.
#'
#' Normality distributions of continuous variables is evaluated by Shapiro-Wilk test and required test is decided based on the test.
#' For categorical variables, expected cell frequencies is calculated and required test (Chi-square or Fisher) is decided based on this.
#'
#'
#' @param dataset A data frame containing the variables of interest.
#' @param dependent The dependent variable (outcome) in the analysis, should be a factor.
#' @param table_vars A character vector specifying the explanatory variables (predictors) for the analysis.
#'                  These can be factors, characters, or numerics.
#' @param na_include Logical, indicating whether missing values should be included in the analysis.
#'                  Default is \code{TRUE}.
#' @param row_col_sums A character string specifying the type of summary statistics to be generated.
#'                    Options include "row_based" for row-based summaries, "col_based" for column-based summaries,
#'                    and "row_col_based" for both row and column-based summaries. Default is "row_col_based".
#' @param all_cont A character string specifying the approach for continuous variables. Options include
#'                "parametric" for parametric tests (mean), "nonparametric" for non-parametric tests (median),
#'                and "shapiro" for using the Shapiro-Wilk test for normality and choosing between parametric
#'                and non-parametric accordingly. Default is "shapiro".
#' @param fisher_correction Logical, indicating whether Fisher's correction should be applied for expected cell
#'                         frequencies less than 5 in more than 20% of the cells in a contingency table.
#'                         Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{ag_ff_summary_factorlist}.
#'
#' @return A data frame containing the aggregated summary statistics and test results, with or without Fisher's correction.
#'
#'
#' @import dplyr
#'
#'
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   result <- ag_ff_summary(penguins, dependent = "species",
#'                                                 table_vars = c("bill_length_mm", "bill_depth_mm"),
#'                                                 na_include = FALSE, row_col_sums = "row_col_based",
#'                                                 all_cont = "shapiro", fisher_correction = TRUE)
#' }
#'
#' @seealso \code{\link{ag_ff_summary}}, \code{\link{ag_fisher}}
#'
#' @author Ali Guner
#'
#' @keywords EDA
#'
#' @export




ag_ff_summary <- function(dataset, dependent, table_vars, na_include = TRUE, row_col_sums = "row_col_based",
                                all_cont = "shapiro", fisher_correction = TRUE, ...) {

        # Generate chisquare summary
        chisquare_ff <- ag_ff_summary_factorlist(dataset, dependent, table_vars, na_include = na_include, row_col_sums = row_col_sums, all_cont = all_cont, ...)

        # Identify variables requiring Fisher's test
        explanatory_fisher <- ag_fisher(dataset = dataset, table_vars = table_vars, strata = dependent)


        if (fisher_correction && !is.null(explanatory_fisher)) {
                # Generate fisher summary
                fisher_ff <- ag_ff_summary_factorlist(dataset, dependent, table_vars, na_include = na_include, row_col_sums = row_col_sums, all_cont = all_cont, p_cat = "fisher", ...) %>%
                        dplyr::filter(label %in% explanatory_fisher) %>%
                        dplyr::select(label, p_fisher = p)

                # Combine chisquare and fisher summaries
                combined_result <- chisquare_ff %>%
                        dplyr::left_join(fisher_ff, by = "label") %>%
                        dplyr::mutate(p = ifelse(is.na(p_fisher), p, p_fisher)) %>%
                        dplyr::select(-p_fisher)

                return(combined_result)

        } else {

                # Return chisquare_ff if fisher_correction is FALSE
                return(chisquare_ff)

        }
}





