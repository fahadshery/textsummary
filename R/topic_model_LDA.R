#' @title Create topic models using LDA
#'
#' @description Divide collection of text into their natural groups. Get the most informative terms by a specificed number of topics, using LDA method. Please process your text column first i.e. remove stop words etc. prior to calling this function.
#' @param input_text  should be a text columm from a dataframe
#' @param plot return a plot? TRUE by defult
#' @param number_of_topics number of topics/themes to return (4 by default)
#' @param number_of_top_terms_by_themes number of words per theme
#' @importFrom magrittr %>%
#' @name topic_model_LDA
#' @rdname topic_model_LDA
#' @export
#' @seealso \code{\link[topicmodels]{LDA}}
#' @examples \dontrun{
#' data("text_data")
#' top_terms_by_topic_LDA(verbatim$text,number_of_topics = 5)
#'verbatim %>% filter(NPS_RATING %in% c("Detractor")) %>%
#'select(text) %>% top_terms_by_topic_LDA(.,number_of_topics = 5)
#' }

topic_model_LDA <- function(input_text,
                                   plot = T,
                                   number_of_topics = 4,
                                   number_of_top_terms_by_themes=10)
{
  ## quiets concerns of R CMD check re: the vars's having no visible binding
  term <- topic <- NULL
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- tm::Corpus(tm::VectorSource(input_text)) # make a corpus object
  DTM <- tm::DocumentTermMatrix(Corpus) # get the count of words/document

  # remove any empty rows in our document term matrix (if there are any
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes

  # preform LDA & get the words/topic in a tidy text format
  lda <- topicmodels::LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidytext::tidy(lda, matrix = "beta")

  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    dplyr::group_by(topic) %>% # treat each topic as a different group
    dplyr::top_n(number_of_top_terms_by_themes, beta) %>% # get the top 10 most informative words
    dplyr::ungroup() %>% # ungroup
    dplyr::arrange(topic, -beta) # arrange words in descending informativeness

  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # count the needed levels of a factor to fill the colour bars
    number_of_factor_levels <- topics %>% dplyr::summarise(count_all = dplyr::n_distinct(term))
    number_of_factor_levels <- number_of_factor_levels$count_all
    #create pallet fill bars
    my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00",
                        "#333333","#666666","#DDDDDD"), length.out = number_of_factor_levels)
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      dplyr::mutate(term = reorder(term, beta)) %>% # sort terms by beta value
      ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) + # plot beta by theme
      ggplot2::geom_col(show.legend = FALSE) + # as a bar plot
      theme_BT()+
      ggplot2::scale_fill_manual(values = my_palette)+
      ggplot2::facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      ggplot2::labs(title = paste("Top",number_of_topics,"themes in text using LDA Modelling"),x = NULL, y = "Beta") + # no x label, change y label
      ggplot2::coord_flip() # turn bars sideways
  }else{
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}
