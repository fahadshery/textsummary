#' @title Create wordcloud for a given text verbatim collection
#'
#' @description A convenient wrapper function around wordcloud within wordcloud package. Breaks verbatim into word counts and plots according to user specification
#' @param df  a dataframe/tribble.
#' @param text_col text column within df
#' @param max_words_to_plot Maximum number of words to be plotted. least frequent terms dropped
#' @param min_freq words with frequency below min_freq will not be plotted
#' @importFrom magrittr %>%
#' @name text_summary_cloud
#' @rdname text_summary_cloud
#' @export
#' @seealso \code{\link[wordcloud]{wordcloud}}
#' #' @seealso \code{\link[textSummary]{text_summary_cloud_by_category}}
#' @examples \dontrun{
#' data("text_data")
#' text_summary_cloud(verbatim,text_col=text,max_words_to_plot = 300,min_freq = 100)
#' }
#'
text_summary_cloud <- function(df,
                       text_col,
                       max_words_to_plot=100,
                       min_freq=300){

  ## quiets concerns of R CMD check re: the vars's having no visible binding
  my_cloud <- NULL

  text_col <- rlang::enquo(text_col)

  my_cloud <- textSummary::word_frequencies(df,plot = FALSE,text_col = !! text_col)

  my_cloud <-as.data.frame(my_cloud)
  wordcloud::wordcloud(my_cloud$word,my_cloud$n,min.freq = min_freq, max.words = max_words_to_plot,
                       colors=RColorBrewer::brewer.pal(8, "Dark2"))
}
