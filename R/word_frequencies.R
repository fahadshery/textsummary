#' @title Return top n word frequencies in a collection of text documents
#'
#' @description Splits the text verbatim into words, count the number of words. Either plots or returns the top n words in a collection of text documents.
#' @author Fahad Usman
#' @param df a tidy dataframe/tribble.
#' @param text_col the name of the text column within df
#' @param number_of_words return a plot/df of single, bigram or trigrams? returns single words by default
#' @param plot return a ggplot2? TRUE by default
#' @param number_of_words_to_plot how many words/terms to plot? Plots Top 10 words in a collection of text documents by default.
#' @param clean_text pre-process text? FALSE by default due to performance issues. Lammatizes and singularises words before counting
#' @param get_all_word_freqs return all words with their respective frequencies i.e. occurance within the whole text document (whole text_col)
#' @importFrom magittr %>%
#' @name word_frequencies
#' @rdname word_frequencies
#' @export
#' @seealso \code{\link[textSummary]{word_frequencies_by_category}}
#' @examples \dontrun {
#' }
#'
word_frequencies <- function(df,
                             text_col,
                             number_of_words = 1,
                             plot = TRUE,
                             number_of_words_to_plot = 10,
                             clean_text = FALSE,
                             get_all_word_freqs = FALSE
                             )
{

}

