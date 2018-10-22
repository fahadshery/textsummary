#' @title Return top n word frequencies in a collection of text documents, segmented by categorical variable
#'
#' @description Splits the text verbatim into words, count the number of words by each level in a categorical variable. Either plots or returns the top n words in each level of categorical variable in a whole dataset or collection of text documents.
#' @param df a tidy dataframe/tribble.
#' @param text_col the name of the text column within df
#' @param categories_col the name of the factor/categorical column to calculate the words in each category or level
#' @param number_of_words return a plot/df of single, bigram or trigrams within each category? returns single words in each category by default
#' @param plot return a ggplot2? TRUE by default
#' @param number_of_words_to_plot how many words/terms to plot within each level of categories_col? Plots Top 10 words in each category by default
#' @param clean_text pre-process text? FALSE by default Lammatizes and get rid of extra spaces before and words before counting
#' @name word_frequencies_by_category
#' @rdname word_frequencies_by_category
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @seealso \code{\link[textSummary]{word_frequencies}}
#' @examples \dontrun{
#' data("text_data")
#' word_frequencies_by_category(verbatim,text_col = text,categories_col = Qtr)
#' word_frequencies_by_category(verbatim,text_col = text,categories_col = Qtr,clean_text = TRUE)
#' word_frequencies_by_category(verbatim,text,Qtr,number_of_words = 2,number_of_words_to_plot = 20)
#' word_frequencies_by_category(verbatim,text,Qtr,clean_text = TRUE,number_of_words = 3)
#'}
word_frequencies_by_category <- function(df,
                                         text_col,
                                         categories_col,
                                         number_of_words = 1,
                                         number_of_words_to_plot = 10,
                                         plot = TRUE,
                                         clean_text = FALSE)
{
  ## quiets concerns of R CMD check re: the vars's having no visible binding
  p <- line_no <-  n <-  word <-  word_total_by_this_category <- total_comments_by_this_category <- term <- word_prop_of_total_comments_by_this_category <- bigram <-  bigram_total_by_this_category <- bigram_total_by_this_category <- bigram_prop_of_total_comments_by_this_category <- bigram_total_by_this_category <-  trigram <- trigram_total_by_this_category <-  trigram_prop_of_total_comments_by_this_category <-  word1 <-  word2 <-  word3 <- NULL

  if (number_of_words > 0 & number_of_words < 4) {
    categories_col <- rlang::enquo(categories_col)
    text_col <- rlang::enquo(text_col)

    df <- df %>%
      dplyr::group_by(!! categories_col)%>%
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
        #  pluralize::singularize()%>%
          textstem::lemmatize_words()
      }
      all_words <- all_words%>%
        dplyr::distinct(!! categories_col,line_no, word, .keep_all = TRUE)%>%
        dplyr::group_by(!! categories_col,word) %>%
        dplyr::mutate(word_total_by_this_category = n()) %>%
        dplyr::ungroup()%>%
        dplyr::group_by(!! categories_col)%>%
        dplyr::mutate(total_comments_by_this_category = max(line_no))%>%
        dplyr::mutate(word_prop_of_total_comments_by_this_category =
                        word_total_by_this_category / total_comments_by_this_category)

      if (plot == FALSE) {
        return(all_words)
      } else{

        #calculate and get top n words
        top_terms_by_category <-  all_words %>%
          dplyr::filter(!is.na(word))%>%
          dplyr::distinct(!! categories_col,word, .keep_all = TRUE)%>% #dedup before getting top 10 by category_col
          dplyr::group_by(!! categories_col) %>%
          dplyr::top_n(n = number_of_words_to_plot, wt = word_total_by_this_category)%>%
          dplyr::ungroup()

        # count the number of words so that each word bar is coloured differently from each other
        number_of_top_words <- top_terms_by_category %>% dplyr::summarise(count_all = dplyr::n_distinct(word))
        number_of_top_words <- number_of_top_words$count_all

        # create BT colour pallet for each word to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        # plot the top n terms
        p <- top_terms_by_category %>%
          dplyr::mutate(term = drlib::reorder_within(word, word_total_by_this_category, !! categories_col)) %>% #order group within group
          ggplot2::ggplot(ggplot2::aes(term, word_prop_of_total_comments_by_this_category, fill = factor(word))) + # plot beta by theme
          ggplot2::geom_col(show.legend = FALSE) + # as a bar plot
          drlib::scale_x_reordered()+
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          ggplot2::facet_wrap(ggplot2::vars(!!categories_col), scales = "free") + # which each categories_col in a seperate plot
          theme_BT()+
          ggplot2::scale_fill_manual(values = my_palette)+
          ggplot2::labs(title = paste("Top",number_of_words_to_plot,"terms in",rlang::quo_name(categories_col)),x = NULL, y = "% of comments containing the word") + # no x label, change y label
          ggplot2::coord_flip() # turn bars sideways
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

      all_bigrams <- all_bigrams%>%
        dplyr::distinct(!! categories_col,line_no, bigram)%>%
        dplyr::group_by(!! categories_col,bigram) %>%
        dplyr::mutate(bigram_total_by_this_category = n()) %>%
        dplyr::ungroup()%>%
        dplyr::group_by(!! categories_col)%>%
        dplyr::mutate(total_comments_by_this_category = max(line_no))%>%
        dplyr::mutate(bigram_prop_of_total_comments_by_this_category = bigram_total_by_this_category /
                        total_comments_by_this_category)

      if (plot == FALSE) {
        return(all_bigrams)
      } else{
        #calculate and get top n words
        top_terms_by_category <-  all_bigrams %>%
          dplyr::filter(!is.na(bigram))%>%
          dplyr::distinct(!! categories_col,bigram, .keep_all = TRUE)%>% #dedup before getting top 10 by categories_col
          dplyr::group_by(!! categories_col) %>%
          dplyr::top_n(n = number_of_words_to_plot, wt = bigram_total_by_this_category)%>%
          dplyr::ungroup()

        # count the number of words so that each word bar is coloured differently from each other
        number_of_top_words <- top_terms_by_category %>% dplyr::summarise(count_all = dplyr::n_distinct(bigram))
        number_of_top_words <- number_of_top_words$count_all

        # create BT colour pallet for each word to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        # plot the top n terms
        p <- top_terms_by_category %>%
          dplyr::mutate(term = drlib::reorder_within(bigram, bigram_total_by_this_category, !! categories_col)) %>% #order group within group
          ggplot2::ggplot(ggplot2::aes(term, bigram_prop_of_total_comments_by_this_category, fill = factor(bigram))) + # plot beta by theme
          ggplot2::geom_col(show.legend = FALSE) + # as a bar plot
          drlib::scale_x_reordered()+
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          ggplot2::facet_wrap(ggplot2::vars(!!categories_col), scales = "free") + # which each group_var in a seperate plot
          theme_BT()+
          ggplot2::scale_fill_manual(values = my_palette)+
          ggplot2::labs(title = paste("Top",number_of_words_to_plot,"terms in",rlang::quo_name(categories_col)),x = NULL, y = "% of verbatim containing the bigram") + # no x label, change y label
          ggplot2::coord_flip() # turn bars sideways
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

      all_trigrams <- all_trigrams%>%
        dplyr::distinct(!! categories_col,line_no, trigram)%>%
        dplyr::group_by(!! categories_col,trigram) %>%
        dplyr::mutate(trigram_total_by_this_category = n()) %>%
        dplyr::ungroup()%>%
        dplyr::group_by(!! categories_col)%>%
        dplyr::mutate(total_comments_by_this_category = max(line_no))%>%
        dplyr::mutate(trigram_prop_of_total_comments_by_this_category = trigram_total_by_this_category /
                        total_comments_by_this_category)

      if (plot == FALSE) {
        return(all_trigrams)
      } else{
        #calculate and get top n words
        top_terms_by_category <-  all_trigrams %>%
          dplyr::filter(!is.na(trigram))%>%
          dplyr::distinct(!! categories_col,trigram, .keep_all = TRUE)%>% #dedup before getting top 10 by categories_col
          dplyr::group_by(!! categories_col) %>%
          dplyr::top_n(n = number_of_words_to_plot, wt = trigram_total_by_this_category)%>%
          dplyr::ungroup()

        # count the number of words so that each word bar is coloured differently from each other
        number_of_top_words <- top_terms_by_category %>% dplyr::summarise(count_all = dplyr::n_distinct(trigram))
        number_of_top_words <- number_of_top_words$count_all

        # create BT colour pallet for each word to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        # plot the top n terms
        p <- top_terms_by_category %>%
          dplyr::mutate(term = drlib::reorder_within(trigram, trigram_total_by_this_category, !! categories_col)) %>% #order group within group
          ggplot2::ggplot(ggplot2::aes(term, trigram_prop_of_total_comments_by_this_category, fill = factor(trigram))) + # plot beta by theme
          ggplot2::geom_col(show.legend = FALSE) + # as a bar plot
          drlib::scale_x_reordered()+
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          theme_BT()+
          ggplot2::scale_fill_manual(values = my_palette)+
          ggplot2::facet_wrap(ggplot2::vars(!!categories_col), scales = "free") + # which each categories_col in a seperate plot
          ggplot2::labs(title = paste("Top",number_of_words_to_plot,"terms in",rlang::quo_name(categories_col)),x = NULL, y = "% of verbatim containing the trigram") + # no x label, change y label
          ggplot2::coord_flip() # turn bars sideways
        print(p)

      }
    }

  } else
    stop("you can ask for single, 2-words and 3-words together only, therefore enter a number between 1-3")

}




