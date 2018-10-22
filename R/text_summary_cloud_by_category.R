#' @title Create wordcloud for a given text verbatim collection, segmented by each level within a categorical variable
#'
#' @description A convenient wrapper function around wordcloud method within wordcloud package. Breaks verbatim into word counts and plots according to user specification
#' @param df  a dataframe/tribble.
#' @param text_col text column within df
#' @param categories_col text column within df
#' @param max_words_to_plot Maximum number of words to be plotted. least frequent terms dropped
#' @param min_freq words with frequency below min_freq in the respective category_col level will not be plotted
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#' @name text_summary_cloud_by_category
#' @rdname text_summary_cloud_by_category
#' @export
#' @seealso \code{\link[wordcloud]{wordcloud}}
#' #' @seealso \code{\link[textSummary]{text_summary_cloud}}
#' @examples \dontrun{
#' data("text_data")
#' text_summary_cloud_by_category(verbatim,text,NPS_RATING)
#' }
#'
text_summary_cloud_by_category <- function(df,
                       text_col,
                       categories_col,
                       max_words_to_plot=100,
                       min_freq=50){

  ## quiets concerns of R CMD check re: the vars's having no visible binding
  word <- word_total_by_this_category <- NULL

   categories_col <- rlang::enquo(categories_col)
   text_col <- rlang::enquo(text_col)

   my_cloud <- textSummary::word_frequencies_by_category(df = df,text_col = !!text_col,categories_col = !!categories_col,
                                                         plot = FALSE)

   my_cloud <- my_cloud %>% dplyr::select(!!categories_col,word,word_total_by_this_category) %>%
                             dplyr::filter(word_total_by_this_category >=min_freq)

   number_of_factor_levels <- my_cloud %>% dplyr::filter(!is.na(!!categories_col)) %>% dplyr::summarise(count_all = dplyr::n_distinct(!!categories_col))
   number_of_factor_levels <- sum(number_of_factor_levels$count_all)
   #create pallet fill cols
   my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00",
                       "#333333","#666666","#DDDDDD"), length.out = number_of_factor_levels)

   reshape2::acast(my_cloud,list(names(my_cloud)[2], names(my_cloud)[1]), value.var = "word_total_by_this_category", fill = 0) %>%
     wordcloud::comparison.cloud(colors = my_palette,
                                 max.words = max_words_to_plot)


}
