#' @title Return top n word frequencies in a collection of text documents
#'
#' @description Splits the text verbatim into words, count the number of words. Either plots or returns the top n words in a collection of text documents.
#' @param df a tidy dataframe/tribble.
#' @param text_col the name of the text column within df
#' @param number_of_words return a plot/df of single, bigram or trigrams? returns single words by default
#' @param plot return a ggplot2? TRUE by default
#' @param number_of_words_to_plot how many words/terms to plot? Plots Top 10 words in a collection of text documents by default.
#' @param clean_text pre-process text? FALSE by default. Lammatizes and get rid of extra spaces before and words before counting
#' @name word_frequencies
#' @rdname word_frequencies
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#' @export
#' @seealso \code{\link[textSummary]{word_frequencies_by_category}}
#' @examples \dontrun{
#' data("text_data")
#' word_frequencies(verbatim,text_col = text)
#' word_frequencies(verbatim,text_col = text,number_of_words = 2,clean_text = TRUE)
#' word_frequencies(verbatim,text_col = text,number_of_words = 3)
#' }
#'

word_frequencies <- function(df,
                             text_col,
                             number_of_words = 1,
                             plot = TRUE,
                             number_of_words_to_plot = 10,
                             clean_text = FALSE
                             )
{
  ## quiets concerns of R CMD check re: the vars's having no visible binding
  p <- line_no <-  n <-  word <-  word_prop_by_total_rows <- bigram <-  bigram_prop_by_total_rows <- bigram_total <-  trigram <- trigram_prop_by_total_rows <-  trigram_total <-  word1 <-  word2 <-  word3 <- NULL

  if (number_of_words > 0 & number_of_words < 4) {
    text_col <- rlang::enquo(text_col)
    df <- df %>%
              dplyr::mutate(line_no = dplyr::row_number()) %>%
              dplyr::ungroup()

    if (number_of_words == 1) {
      all_words <- df %>%
                      tidytext::unnest_tokens(word, !! text_col, drop = FALSE) %>%
                      dplyr::filter(!is.na(word))
      if (clean_text == TRUE) {
        all_words$word <- all_words$word %>%
                                          stringr::str_replace_all('[:space:]',' ')%>%
                                          stringr::str_trim() %>%
                                         # pluralize::singularize()%>%
                                          textstem::lemmatize_words()
      }
      all_words <- all_words %>%
                              dplyr::distinct(line_no,word) %>%
                              dplyr::count(word,sort = TRUE) %>%
                              dplyr::ungroup() %>%
                              dplyr::mutate(word_prop_by_total_rows = n / nrow(df))
      if (plot == FALSE) {
        return(all_words)
      } else{
        #calculate and get top n words
        top_words <- all_words %>%
                                dplyr::distinct(word,n,.keep_all = TRUE) %>% #dedup words before getting top_n
                                dplyr::top_n(number_of_words_to_plot,n) %>%
                                dplyr::ungroup()
        # count the number of words so that each word bar is coloured differently from each other
        number_of_top_words <- top_words %>% dplyr::summarise(count_all = dplyr::n_distinct(word))
        number_of_top_words <- number_of_top_words$count_all

        # create BT colour pallet for each word to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        # plot the top n terms
        p <- top_words %>%
          dplyr::mutate(word = reorder(word,n)) %>%
          ggplot2::ggplot(ggplot2::aes(word,word_prop_by_total_rows,fill = factor(word))) +
          ggplot2::geom_col(show.legend = FALSE) +
          ggplot2::geom_label(ggplot2::aes(label = sprintf("%1.0f%%", 100 * word_prop_by_total_rows), hjust = -0.2,
                                           fontface = "bold"),show.legend = FALSE, colour = "white",size = 5) +
          ggplot2::geom_text(ggplot2::aes(label = n),check_overlap = TRUE,fontface = "bold",vjust = 3, hjust = -0.5) +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          theme_BT() +
          ggplot2::theme(legend.position = "none",
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         legend.title = ggplot2::element_blank()) +
          ggplot2::scale_fill_manual( values = my_palette) +
          ggplot2::labs(title = paste("Top", number_of_words_to_plot,"Most common words in all verbatim"),
                        subtitle = paste("Total sample size =",nrow(df)),
                        y = "% of rows containing the word") +
          ggplot2::coord_flip()
          print(p)


      }
    }

    if (number_of_words == 2) {
      all_bigrams <- df %>%
        tidytext::unnest_tokens(bigram, !! text_col,token = "ngrams", n = 2, drop = FALSE) %>%
        dplyr::filter(!is.na(bigram))

      if (clean_text == TRUE) {
        all_bigrams_seps <- all_bigrams %>%
          tidyr::separate(bigram, c("word1","word2"),sep = " ")

        all_bigrams_seps$word1 <- all_bigrams_seps$word1 %>%
          stringr::str_replace_all('[:space:]',' ')%>%
          stringr::str_trim() %>%
         # pluralize::singularize()%>%
          textstem::lemmatize_words()

        all_bigrams_seps$word2 <- all_bigrams_seps$word2 %>%
          stringr::str_replace_all('[:space:]',' ')%>%
          stringr::str_trim() %>%
        #  pluralize::singularize()%>%
          textstem::lemmatize_words()
        all_bigrams <- all_bigrams_seps %>%
                        tidyr::unite(bigram, word1, word2, sep=" ")
      }

      all_bigrams <- all_bigrams %>%
        dplyr::distinct(line_no,bigram) %>%
        dplyr::group_by(bigram) %>%
        dplyr::mutate(bigram_total = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(bigram_prop_by_total_rows = bigram_total / nrow(df))

      if (plot == FALSE) {
        return(all_bigrams)
      } else{
        #calculate and get top n words
        top_words <- all_bigrams %>%
          dplyr::distinct(bigram,bigram_total,.keep_all = TRUE) %>% #dedup bigrams before getting top_n
          dplyr::top_n(number_of_words_to_plot,bigram_total) %>%
          dplyr::ungroup()
        # count the number of words so that each word bar is coloured differently from each other
        number_of_top_words <- top_words %>% dplyr::summarise(count_all = dplyr::n_distinct(bigram))
        number_of_top_words <- number_of_top_words$count_all

        # create BT colour pallet for each word to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        # plot the top n terms
        p <- top_words %>%
          dplyr::mutate(bigram = reorder(bigram,bigram_total)) %>%
          ggplot2::ggplot(ggplot2::aes(bigram,bigram_prop_by_total_rows,fill = factor(bigram))) +
          ggplot2::geom_col(show.legend = FALSE) +
          ggplot2::geom_label(ggplot2::aes(label = sprintf("%1.0f%%", 100 * bigram_prop_by_total_rows), hjust = -0.2,
                                           fontface = "bold"),show.legend = FALSE, colour = "white",size = 5) +
          ggplot2::geom_text(ggplot2::aes(label = bigram_total),check_overlap = TRUE,fontface = "bold",vjust = 3, hjust = -0.5) +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          theme_BT() +
          ggplot2::theme(legend.position = "none",
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         legend.title = ggplot2::element_blank()) +
          ggplot2::scale_fill_manual( values = my_palette) +
          ggplot2::labs(title = paste("Top", number_of_words_to_plot,"Most common bigrams in all verbatim"),
                        subtitle = paste("Total sample size =",nrow(df)),
                        y = "% of rows containing the bigram") +
          ggplot2::coord_flip()
          print(p)

      }
    }

    if (number_of_words == 3) {
      all_trigrams <- df %>%
        tidytext::unnest_tokens(trigram, !! text_col,token = "ngrams", n = 3, drop = FALSE) %>%
        dplyr::filter(!is.na(trigram))

      if (clean_text == TRUE) {
        all_trigram_seps <- all_trigrams %>%
          tidyr::separate(trigram, c("word1","word2","word3"),sep = " ")

        all_trigram_seps$word1 <- all_trigram_seps$word1 %>%
          stringr::str_replace_all('[:space:]',' ')%>%
          stringr::str_trim() %>%
          #pluralize::singularize()%>%
          textstem::lemmatize_words()

        all_trigram_seps$word2 <- all_trigram_seps$word2 %>%
          stringr::str_replace_all('[:space:]',' ')%>%
          stringr::str_trim() %>%
         # pluralize::singularize()%>%
          textstem::lemmatize_words()

        all_trigram_seps$word3 <- all_trigram_seps$word3 %>%
          stringr::str_replace_all('[:space:]',' ')%>%
          stringr::str_trim() %>%
          #pluralize::singularize()%>%
          textstem::lemmatize_words()

        all_trigrams <- all_trigram_seps %>%
          tidyr::unite(trigram, word1, word2,word3, sep=" ")
      }

      all_trigrams <- all_trigrams %>%
        dplyr::distinct(line_no,trigram) %>%
        dplyr::group_by(trigram) %>%
        dplyr::mutate(trigram_total = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(trigram_prop_by_total_rows = trigram_total / nrow(df))

      if (plot == FALSE) {
        return(all_trigrams)
      } else{
        #calculate and get top n words
        top_words <- all_trigrams %>%
          dplyr::distinct(trigram,trigram_total,.keep_all = TRUE) %>% #dedup bigrams before getting top_n
          dplyr::top_n(number_of_words_to_plot,trigram_total) %>%
          dplyr::ungroup()
        # count the number of words so that each word bar is coloured differently from each other
        number_of_top_words <- top_words %>% dplyr::summarise(count_all = dplyr::n_distinct(trigram))
        number_of_top_words <- number_of_top_words$count_all

        # create BT colour pallet for each word to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        # plot the top n terms
        p <- top_words %>%
          dplyr::mutate(trigram = reorder(trigram,trigram_total)) %>%
          ggplot2::ggplot(ggplot2::aes(trigram,trigram_prop_by_total_rows,fill = factor(trigram))) +
          ggplot2::geom_col(show.legend = FALSE) +
          ggplot2::geom_label(ggplot2::aes(label = sprintf("%1.0f%%", 100 * trigram_prop_by_total_rows), hjust = -0.2,
                                           fontface = "bold"),show.legend = FALSE, colour = "white",size = 5) +
          ggplot2::geom_text(ggplot2::aes(label = trigram_total),check_overlap = TRUE,fontface = "bold",vjust = 3, hjust = -0.5) +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          theme_BT() +
          ggplot2::theme(legend.position = "none",
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         legend.title = ggplot2::element_blank()) +
          ggplot2::scale_fill_manual( values = my_palette) +
          ggplot2::labs(title = paste("Top", number_of_words_to_plot,"Most common trigrams in all verbatim"),
                        subtitle = paste("Total sample size =",nrow(df)),
                        y = "% of rows containing the trigram") +
          ggplot2::coord_flip()
          print(p)

      }
    }

  } else
    stop("you can ask for single, 2-words and 3-words together only, therefore enter a number between 1-3")

}

