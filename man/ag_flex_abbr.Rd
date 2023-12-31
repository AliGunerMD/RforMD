% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flextable_functions.R
\name{ag_flex_abbr}
\alias{ag_flex_abbr}
\title{Add abbreviations to a flextable}
\usage{
ag_flex_abbr(flex_obj, abbr = NULL, use_df = TRUE, prefix = "Abbreviations: ")
}
\arguments{
\item{flex_obj}{The flextable object to which the abbreviations are added.}

\item{abbr}{The abbreviations. It can be a character vector or a named list.}

\item{use_df}{A logical value indicating whether to use a data frame format for the abbreviations.}

\item{prefix}{The prefix text to be displayed before the abbreviations.}
}
\value{
The flextable object with the abbreviations added.
}
\description{
This function adds abbreviations to a flextable object.
}
\examples{
\dontrun{

ag_flex_abbr(flex_obj, abbr = abbr)
}

}
\seealso{
\code{\link{flextable::add_footer_lines()}}
}
\author{
Ali Guner
}
