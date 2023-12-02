#' @title minimized version of summary_factorlist
#' @description
#' Perform Short Summary Statistics and Hypothesis Testing

#'
#' This function generates short summary statistics and conducts hypothesis testing, including chi-square tests and Fisher's exact tests if specified.
#'
#' @param dataset A data frame containing the variables of interest.
#' @param strata The dependent variable for hypothesis testing.
#' @param table_vars A vector of explanatory variables for analysis.
#' @param na_include Logical. Should NA values be included in the analysis? Default is NULL.
#' @param cont The type of measure for continuous variables. Default is NULL.
#' @param cont_nonpara A vector specifying non-parametric measures for continuous variables. Default is NULL.
#' @param fisher_correction Logical. Should Fisher's exact tests be corrected? Default is NULL.
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return A summary factor list containing short summary statistics and hypothesis testing results.
#'
#' @author Ali Guner
#'
#' @importFrom finalfit summary_factorlist
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Example usage of short_ff function
#' short_result <- short_ff(my_data, strata = "Response", table_vars = c("Var1", "Var2"))
#'}
#' @seealso \code{\link{summary_factorlist}}, \code{\link{ag_fisher}}
#'


short_ff <- function(dataset, strata = NULL, table_vars = NULL,
                     na_include = NULL, cont = NULL, cont_nonpara = NULL,
                     fisher_correction = fisher_correction,
                     ...) {
  if (is.null(strata)) {
    init_ff <- summary_factorlist(dataset,
      dependent = strata,
      explanatory = table_vars,
      na_include = na_include,
      cont = cont,
      cont_nonpara = cont_nonpara,
      total_col = TRUE,
      p = FALSE,
      ...
    ) %>%
            suppressWarnings()

    return(init_ff)
  } else {
    chisquare_ff <- summary_factorlist(dataset,
      dependent = strata,
      explanatory = table_vars,
      na_include = na_include,
      cont = cont,
      cont_nonpara = cont_nonpara,
      total_col = TRUE,
      p = TRUE,
      ...
    ) %>%
            suppressWarnings()


    if (fisher_correction) {
      explanatory_fisher <- ag_fisher(dataset = dataset, table_vars = table_vars, strata = strata)

      if (is.null(explanatory_fisher)) {
        return(chisquare_ff)
      } else {
        fisher_ff <- summary_factorlist(dataset,
          dependent = strata,
          explanatory = table_vars,
          na_include = na_include,
          cont = cont,
          cont_nonpara = cont_nonpara,
          total_col = TRUE,
          p = TRUE,
          p_cat = "fisher",
          ...
        ) %>%
                suppressWarnings()


        fisher_short <- fisher_ff %>%
          dplyr::filter(label %in% explanatory_fisher) %>%
          dplyr::select(label, p_fisher = p)

        combined_ff <- chisquare_ff %>%
          dplyr::left_join(fisher_short, by = "label") %>%
          dplyr::mutate(p = ifelse(is.na(p_fisher), p, p_fisher)) %>%
          dplyr::select(-p_fisher)

        return(combined_ff)
      }
    } else {
      # Return chisquare_ff if fisher_correction is FALSE
      return(chisquare_ff)
    }
  }
}




#' @title Generate Row-, Column-based or both-based Summary Statistics
#'
#' @description
#' This function generates row or column-based summary statistics based on the specified method.
#'
#' @param dataset A data frame containing the variables of interest.
#' @param strata The dependent variable for hypothesis testing.
#' @param table_vars A vector of explanatory variables for analysis.
#' @param na_include Logical. Should NA values be included in the analysis? Default is NULL.
#' @param cont The type of measure for continuous variables. Default is NULL.
#' @param cont_nonpara A vector specifying non-parametric measures for continuous variables. Default is NULL.
#' @param fisher_correction Logical. Should Fisher's exact tests be corrected? Default is NULL.
#' @param row_col_sums The method for generating row or column-based summary statistics. Use 'row_based' or 'col_based'.
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return A summary factor list containing row or column-based summary statistics.
#'
#' @author Ali Guner
#'
#' @examples
#' \dontrun{
#' # Example usage of ff_row_col_sums function
#' row_col_result <- ff_row_col_sums(my_data, strata = "Response", table_vars = c("Var1", "Var2"), row_col_sums = "row_based")
#'}
#' @seealso \code{\link{short_ff}}


ff_row_col_sums <- function(dataset,
                            strata = NULL,
                            table_vars = NULL,
                            na_include = NULL,
                            cont = NULL,
                            cont_nonpara = NULL,
                            fisher_correction = NULL,
                            row_col_sums,
                            ...) {
  if (row_col_sums == "row_based") {
    column <- FALSE
  } else if (row_col_sums == "col_based") {
    column <- TRUE
  }

  return(short_ff(dataset,
    strata = strata,
    table_vars = table_vars,
    na_include = na_include,
    cont = cont,
    cont_nonpara = cont_nonpara,
    column = column,
    fisher_correction = fisher_correction,
    ...
  ))
}




#' @title Enhanced version of finalfit::summary_factorlist

#'
#' @description
#' Generate aggregated summary statistics and tests for categorical and continuous variables,
#' with an option for Fisher's correction in contingency tables.
#'
#' This function extends the functionality of the `summary_factorlist` function by introducing
#' Fisher's test and Shapiro-Wilk test.
#' Normality distributions of continuous variables is evaluated by Shapiro-Wilk test
#' and required test is decided based on the test.
#' For categorical variables, expected cell frequencies is calculated
#' (expected cell frequencies less than 5 in more than 20% of the cells in a
#' contingency table may require using Fisher test) and required test
#' (Chi-square or Fisher) is decided based on this.
#'
#'
#' @param dataset A data frame containing the variables of interest.
#' @param strata The dependent variable (outcome) in the analysis, should be a factor.
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
#'                and non-parametric accordingly. Default is "shapiro". Manual option is also possible with \code{manual_nonparams}.
#' @param fisher_correction Logical, indicating whether Fisher's correction should be applied for expected cell
#'                         frequencies less than 5 in more than 20% of the cells in a contingency table.
#'                         Default is \code{TRUE}.
#' @param manual_nonparams If \code{all_cont} is set to "manual", this argument should contain the values (not index, the name of the varible) for
#'                         \code{cont_nonpara}. It cannot be empty in this case.
#' @param summary_factorlist_args A list containing additional arguments to be passed to \code{ag_ff_summary}.
#' All comes from the finalfit::summaryfactorlist function.
#'                                Default is an empty list.
#' @param ... Additional arguments to be passed to \code{ag_ff_summary}.
#'
#' @return A data frame containing the aggregated summary statistics and test results.
#'
#' @author Ali Guner
#'
#' @importFrom dplyr relocate
#' @examples
#' \dontrun{
#' # Example usage of ag_ff_summary function
#' library(palmerpenguins)
#'
#' strata <- "species"
#' table_vars_1 <- penguins %>%
#' dplyr::select(-species) %>%
#' names()
#'
#' ag_ff_summary(
#' dataset = penguins,
#' table_vars = table_vars_1,
#' strata = "species"
#' )
#' }
#' @seealso \code{\link{ff_row_col_sums}}
#' @export



ag_ff_summary <- function(dataset, strata = NULL, table_vars,
                          row_col_sums = "row_col_based",
                          all_cont = "shapiro",
                          manual_nonparams = NULL,
                          fisher_correction = TRUE,
                          na_include = FALSE,
                          summary_factorlist_args = list(),
                          ...) {


  # Checks
  if (!is.data.frame(dataset)) stop("Your dataset is not a dataframe.")
  if (any(class(dataset) %in% c("tbl_df", "tbl"))) dataset <- data.frame(dataset)
  if (is.null(table_vars)) stop("table_vars is empty")





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



  if (!is.null(strata)) {
          if (!is.factor(dataset[[strata]]) && !is.character(dataset[[strata]])) stop("The strata variable should be a factor/character. Please check the data type.")


          # Handle missing values in the strata
          missing_strata <- sum(is.na(dataset[[strata]]))

          if (missing_strata > 0) {
                  warning(paste0("Important! There are ", missing_strata, " missing values in ", strata, ". Rows with missing values in ", strata, " will be removed from the analysis."))
                  dataset <- dataset[!is.na(dataset[[strata]]), ]
          }

  }

  if(is.null(strata)){
          message("No strata was provided")
  }







  # Additional arguments from finalfit::summary_factorlist
  summary_factorlist_args <- list(
    cont_cut = 5,
    cont_range = TRUE,
    p_cont_para = "aov",
    orderbytotal = FALSE,
    digits = c(1, 1, 3, 1, 0),
    # na_include_dependent = FALSE,         # already excluded. consider to relevel strata
    na_complete_cases = FALSE,
    na_to_p = FALSE,
    na_to_prop = TRUE,
    add_dependent_label = FALSE,
    dependent_label_prefix = "Dependent: ",
    dependent_label_suffix = "",
    add_col_totals = FALSE,
    include_col_totals_percent = TRUE,
    col_totals_prefix = "",
    add_row_totals = FALSE,
    include_row_totals_percent = TRUE,
    include_row_missing_col = TRUE,
    row_totals_colname = "Total N",
    row_missing_colname = "Missing N"
  )



  # all_cont argument
  if (all_cont == "nonparametric") {

    if (!is.null(manual_nonparams)) message("manual_nonparams argument is useless, if all_cont is not 'manual'")
    cont <- "median"
    cont_nonpara <- NULL

  } else if (all_cont == "parametric") {

    if (!is.null(manual_nonparams)) message("manual_nonparams argument is useless, if all_cont is not 'manual'")
    cont <- "mean"
    cont_nonpara <- NULL

  } else if (all_cont == "shapiro") {

    if (!is.null(manual_nonparams)) message("manual_nonparams argument is useless, if all_cont is not 'manual'")
    cont <- "mean"
    cont_nonpara <- ag_shapiro(dataset, strata = strata, table_vars = table_vars)

  } else if (all_cont == "manual") {

    if (is.null(manual_nonparams)) stop("When all_cont argument is manual, manual_nonparams argument can not be empty")
    cont <- "mean"
    cont_nonpara <- which(table_vars %in% manual_nonparams)

  } else {
    stop("Invalid value for 'all_cont'. Use 'parametric', 'nonparametric', 'shapiro' or 'manual'.")
  }





  if (row_col_sums == "row_based") {
    row_col_sums_df <- ff_row_col_sums(dataset,
      strata = strata,
      table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
      fisher_correction = fisher_correction,
      row_col_sums = "row_based", ...
    ) %>%
      suppressMessages()

  } else if (row_col_sums == "col_based") {
    row_col_sums_df <- ff_row_col_sums(dataset,
      strata = strata,
      table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
      fisher_correction = fisher_correction,
      row_col_sums = "col_based", ...
    ) %>%
      suppressMessages()

  } else if (row_col_sums == "row_col_based") {
    row_col_sums_df_comb <- cbind(
      ff_row_col_sums(dataset,
        strata = strata,
        table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
        fisher_correction = fisher_correction, row_col_sums = "row_based", ...
      ) %>%
        suppressMessages() %>%
        select(-Total),
      ff_row_col_sums(dataset,
        strata = strata,
        table_vars = table_vars, na_include = na_include, cont = cont, cont_nonpara = cont_nonpara,
        fisher_correction = fisher_correction, row_col_sums = "col_based", ...
      ) %>%
        suppressMessages() %>%
        select(Total)
    )

    row_col_sums_df <- if ("p" %in% colnames(row_col_sums_df_comb)) {
      dplyr::relocate(row_col_sums_df_comb, Total, .before = p)
    } else {
      dplyr::relocate(row_col_sums_df_comb, Total, .after = last_col())
    }
  } else {
    stop("Invalid value for row_col_sums. Use 'row_based', 'col_based', or 'row_col_based'.")
  }


  if (is.null(strata)) {
    return(row_col_sums_df %>% select(-all))
  } else {
    return(row_col_sums_df)
  }
}




