#' @title tf-idf analysis of text verbatim, segmented by categorical variable
#'
#' @description function performs \code{tf-idf} analysis of a given text by categorical variable and plots top n words per group in verbatim by each level of categorical variable. Particularly useful to compare how different two categories are.
#' @param df  a dataframe/tribble.
#' @param text_col the name of the text column within df
#' @param categories_col the name of the factor/categorical for segments i.e. facets
#' @param number_of_words return a plot/df of single, bigram or trigrams within each category? returns single words in each category by default
#' @param number_of_words_to_plot how many words/terms to plot within each level of categories_col? Plots Top 10 words in each category by default
#' @param clean_text pre-process text? FALSE by default Lammatizes and get rid of extra spaces before and words before counting
#' @param plot return a ggplot2? TRUE by default
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @name tf_idf_by_category
#' @rdname tf_idf_by_category
#' @export
#' @seealso \code{\link[tidytext]{bind_tf_idf}}
#' @examples \dontrun{
#' data("text_data")
#' tf_idf_by_category(verbatim,categories_col = NPS_RATING,text_col = text)
#' tf_idf_by_category(verbatim,categories_col = Qtr,text_col = text)
#' tf_idf_by_category(verbatim, Qtr, text,number_of_words = 3,clean_text = TRUE)
#' }

tf_idf_by_category <- function(df,
                               text_col,
                               categories_col,
                               number_of_words = 1,
                               number_of_words_to_plot=10,
                               clean_text = FALSE,
                               plot = TRUE){

  ## quiets concerns of R CMD check re: the vars's having no visible binding
  p <-  n <-  word <-  tf_idf_by_category <- tf_idf_by_category <- tf_idf <- bigram <- word1 <- word2 <- word3 <- trigram <- NULL

  if (number_of_words > 0 & number_of_words < 4) {

    categories_col <- rlang::enquo(categories_col)
    text_col <- rlang::enquo(text_col)

  if (number_of_words == 1) {

    feedback_words <- df %>%
      tidytext::unnest_tokens(word, !! text_col, drop = FALSE) %>%
      dplyr::filter(!is.na(word))

    if(clean_text == TRUE){
      feedback_words$word <- feedback_words$word%>%
        stringr::str_replace_all('[:space:]',' ')%>%
        stringr::str_trim() %>%
        #  pluralize::singularize()%>%
        textstem::lemmatize_words()
    }

     feedback_words <- feedback_words%>%
       dplyr::count(!! categories_col, word, sort = TRUE) %>%
       dplyr::ungroup()

     total_words <- feedback_words %>%
       dplyr::group_by(!! categories_col) %>%
       dplyr::summarize(total = sum(n))

     feedback_words <- dplyr::left_join(feedback_words, total_words)

     feedback_words <- feedback_words %>%
       tidytext::bind_tf_idf(word, !! categories_col, n)
     #calculate and get top n words
     tf_idf_data_to_plot <- feedback_words %>%
       dplyr::arrange(dplyr::desc(tf_idf)) %>%
       dplyr::mutate(word = factor(word, levels = rev(unique(word))))%>%
       dplyr::group_by(!! categories_col) %>%
       dplyr::top_n(number_of_words_to_plot) %>%
       dplyr::ungroup()

     if (plot == FALSE) {
       return(feedback_words)
     } else{

    #   # count the number of words so that each word bar is coloured differently from each other
       number_of_top_words <- tf_idf_data_to_plot %>% dplyr::summarise(count_all = dplyr::n_distinct(word))
       number_of_top_words <- number_of_top_words$count_all

    #   # create BT colour pallet for each word to be plotted on y-axis
       my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                           "#DDDDDD"), length.out = number_of_top_words)

    #   # plot the top n terms
       p <-   ggplot2::ggplot(data=tf_idf_data_to_plot,ggplot2::aes(word, tf_idf, fill = !! categories_col)) +
         ggplot2::geom_col(show.legend = FALSE) +
         ggplot2::labs(x = NULL, y = "tf-idf") +
         theme_BT()+
         ggplot2::theme(legend.position = "none",
                        axis.text.x = ggplot2::element_blank()) +
         ggplot2::facet_wrap(ggplot2::vars(!!categories_col), scales = "free") + # which each level within categories_col in a seperate plot
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

      feedback_words <- all_bigrams%>%
        dplyr::count(!! categories_col, bigram, sort = TRUE) %>%
        dplyr::ungroup()

      total_words <- feedback_words %>%
        dplyr::group_by(!! categories_col) %>%
        dplyr::summarize(total = sum(n))

      feedback_words <- dplyr::left_join(feedback_words, total_words)

      feedback_words <- feedback_words %>%
        tidytext::bind_tf_idf(bigram, !! categories_col, n)
      #calculate and get top n words
      tf_idf_data_to_plot <- feedback_words %>%
        dplyr::arrange(dplyr::desc(tf_idf)) %>%
        dplyr::mutate(bigram = factor(bigram, levels = rev(unique(bigram))))%>%
        dplyr::group_by(!! categories_col) %>%
        dplyr::top_n(number_of_words_to_plot) %>%
        dplyr::ungroup()

      if (plot == FALSE) {
        return(feedback_words)
      } else{

        #   # count the number of words so that each bigram bar is coloured differently from each other
        number_of_top_words <- tf_idf_data_to_plot %>% dplyr::summarise(count_all = dplyr::n_distinct(bigram))
        number_of_top_words <- number_of_top_words$count_all

        #   # create BT colour pallet for each bigram to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        #   # plot the top n terms
        p <-   ggplot2::ggplot(data=tf_idf_data_to_plot,ggplot2::aes(bigram, tf_idf, fill = !! categories_col)) +
          ggplot2::geom_col(show.legend = FALSE) +
          ggplot2::labs(x = NULL, y = "tf-idf") +
          theme_BT()+
          ggplot2::theme(legend.position = "none",
                         axis.text.x = ggplot2::element_blank()) +
          ggplot2::facet_wrap(ggplot2::vars(!!categories_col), scales = "free") + # which each level within categories_col in a seperate plot
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

      feedback_words <- all_trigrams%>%
        dplyr::count(!! categories_col, trigram, sort = TRUE) %>%
        dplyr::ungroup()

      total_words <- feedback_words %>%
        dplyr::group_by(!! categories_col) %>%
        dplyr::summarize(total = sum(n))

      feedback_words <- dplyr::left_join(feedback_words, total_words)

      feedback_words <- feedback_words %>%
        tidytext::bind_tf_idf(trigram, !! categories_col, n)
      #calculate and get top n words
      tf_idf_data_to_plot <- feedback_words %>%
        dplyr::arrange(dplyr::desc(tf_idf)) %>%
        dplyr::mutate(trigram = factor(trigram, levels = rev(unique(trigram))))%>%
        dplyr::group_by(!! categories_col) %>%
        dplyr::top_n(number_of_words_to_plot) %>%
        dplyr::ungroup()

      if (plot == FALSE) {
        return(feedback_words)
      } else{

        #   # count the number of words so that each trigram bar is coloured differently from each other
        number_of_top_words <- tf_idf_data_to_plot %>% dplyr::summarise(count_all = dplyr::n_distinct(trigram))
        number_of_top_words <- number_of_top_words$count_all

        #   # create BT colour pallet for each trigram to be plotted on y-axis
        my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                            "#DDDDDD"), length.out = number_of_top_words)

        #   # plot the top n terms
        p <-   ggplot2::ggplot(data=tf_idf_data_to_plot,ggplot2::aes(trigram, tf_idf, fill = !! categories_col)) +
          ggplot2::geom_col(show.legend = FALSE) +
          ggplot2::labs(x = NULL, y = "tf-idf") +
          theme_BT()+
          ggplot2::theme(legend.position = "none",
                         axis.text.x = ggplot2::element_blank()) +
          ggplot2::facet_wrap(ggplot2::vars(!!categories_col), scales = "free") + # which each level within categories_col in a seperate plot
          ggplot2::coord_flip()
        print(p)
      }
    }

   }else
  stop("you can ask for single, 2-words and 3-words together only, therefore enter a number between 1-3")

}
