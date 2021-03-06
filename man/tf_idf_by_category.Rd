% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tf_idf_by_category.R
\name{tf_idf_by_category}
\alias{tf_idf_by_category}
\title{tf-idf analysis of text verbatim, segmented by categorical variable}
\usage{
tf_idf_by_category(df, text_col, categories_col, number_of_words = 1,
  number_of_words_to_plot = 10, clean_text = FALSE, plot = TRUE)
}
\arguments{
\item{df}{a dataframe/tribble.}

\item{text_col}{the name of the text column within df}

\item{categories_col}{the name of the factor/categorical for segments i.e. facets}

\item{number_of_words}{return a plot/df of single, bigram or trigrams within each category? returns single words in each category by default}

\item{number_of_words_to_plot}{how many words/terms to plot within each level of categories_col? Plots Top 10 words in each category by default}

\item{clean_text}{pre-process text? FALSE by default Lammatizes and get rid of extra spaces before and words before counting}

\item{plot}{return a ggplot2? TRUE by default}
}
\description{
function performs \code{tf-idf} analysis of a given text by categorical variable and plots top n words per group in verbatim by each level of categorical variable. Particularly useful to compare how different two categories are.
}
\examples{
\dontrun{
data("text_data")
tf_idf_by_category(verbatim,categories_col = NPS_RATING,text_col = text)
tf_idf_by_category(verbatim,categories_col = Qtr,text_col = text)
tf_idf_by_category(verbatim, Qtr, text,number_of_words = 3,clean_text = TRUE)
}
}
\seealso{
\code{\link[tidytext]{bind_tf_idf}}
}
