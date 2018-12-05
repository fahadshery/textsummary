#' @title Plot or get top n word frequencies using English grammar in a collection of text documents and compare by categories variable
#'
#' @description Tokenising, Lemmatising, Tagging and Dependency Parsing of raw text using udpipe as a backend. Counts the number of words by each level in a categorical variable. Either plots or returns the tokenised df
#' @param df a data.frame or a tibble/tribble
#' @param text_col the name of the text column within df
#' @param categories_col the name of the factor/categorical column to calculate the words in each category or level
#' @param number_of_words_to_plot how many words/terms to plot within each level of categories_col? Plots Top 10 words in each category by default
#' @param plot return a ggplot2? or get a tokenised/lemmatised df created by udpipe model. TRUE by default
#' @param grammer_phrase what to filter on? Possible options include all the universal parts of speech tags such as noun, verb, adj, pron, aux, num etc. more info here: \url{https://polyglot.readthedocs.io/en/latest/POS.html}
#' @param word_type tokens or lemmas to plot
#' @name word_freqs_by_category
#' @rdname word_freqs_by_category
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @seealso \code{\link[textSummary]{word_frequencies_by_category}}
#' @examples \dontrun{
#' data("text_data")
#' word_freqs_by_category(verbatim,text_col = text, categories_col = NPS_RATING)
#' word_freqs_by_category(verbatim,text_col = text, categories_col = NPS_RATING, word_type = token)
#' word_freqs_by_category(verbatim,text,Qtr,number_of_words_to_plot = 20)
#' }
word_freqs_by_category <- function(df,
                                         text_col,
                                         categories_col,
                                         number_of_words_to_plot = 10,
                                         plot = TRUE,
                                         grammer_phrase = "NOUN",
                                         word_type = lemma)
{
  ## quiets concerns of R CMD check re: the vars's having no visible binding
  p <- p1 <- upos <- my_palette <- lemma <- df_tokens  <- . <- n <- total_lemma <- prop_by_this_category <- top_terms_overall <- top_terms_by_category <- df_tokens <- NULL
    grammer_phrase <- stringr::str_to_upper(grammer_phrase)
    categories_col <- rlang::enquo(categories_col)
    text_col <- rlang::enquo(text_col)
    word_type <- rlang::enquo(word_type)

    # create doc_id because it is essential for udpipe tokenisation and also create totals by category
    df <- df %>%
      dplyr::mutate(doc_id = dplyr::row_number()) %>%
      dplyr::group_by(!! categories_col)%>%
      dplyr::mutate(total_comments_by_this_category = dplyr::n()) %>%
      dplyr::ungroup()

    df <- dplyr::rename(df, text = !! text_col)

    #included the udpipe english model with the textSummary package. Located in extdata directory
    model_f_path <- system.file("extdata", "english-ud-2.1-20180111.udpipe", package="textSummary")
    df_tokens <- udpipe::udpipe(df, model_f_path)

    #you can download the model for the first time from github repo and then will tokenise, lemmatise etc.
    # df_tokens <- udpipe::udpipe(df, "english",udpipe_model_repo = "bnosac/udpipe.models.ud")

    df_tokens$doc_id <- as.integer(df_tokens$doc_id)

    #udpipe gets rid of other existing cols within df, so merge to bring those in on doc_id
    df_tokens <- df_tokens %>% dplyr::inner_join(df)

    # create BT colour pallet for each word to be plotted on y-axis
    my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00","#333333","#666666",
                        "#DDDDDD"), length.out = number_of_words_to_plot)

    p1 <-     df_tokens %>%
      dplyr::count(upos,sort = TRUE) %>%
      dplyr::mutate(upos = reorder(upos,n)) %>%
      ggplot2::ggplot(ggplot2::aes(x = upos, y = n, fill = as.factor(upos))) +
      ggplot2::geom_bar(stat = "identity") +
      theme_BT()+
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none")+
      ggplot2::labs(title = paste("Universal Parts of Speech Distribution within Text"),
                    subtitle = "Frequency of Occurance",
                    y = "Term Frequency",
                    x = NULL,
                    fill = NULL)
    print(p1)

    #plot top n words

    p2 <-     df_tokens %>%
      dplyr::filter(upos %in% c(!! grammer_phrase)) %>%
      dplyr::count(!! word_type,sort = TRUE) %>%
      dplyr::top_n(number_of_words_to_plot) %>%
      dplyr::mutate(word_type = reorder(!! word_type,n)) %>%
      ggplot2::ggplot(ggplot2::aes(x = word_type, y = n, fill = as.factor(word_type))) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = my_palette)+
      theme_BT()+
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none")+
      ggplot2::labs(title = paste("Top",number_of_words_to_plot,"Most occurring terms"),
                    subtitle = paste("Total Distinct Terms by:",grammer_phrase ," =",dplyr::n_distinct(dplyr::select(df_tokens,!!word_type))),
                    y = "Term Frequency",
                    x = NULL,
                    fill = NULL)
    print(p2)

    #if plot is not required then return the whole tokenised df

      if (plot == FALSE) {

        return(df_tokens)
      } else{

        #calculate and get top n words
        top_terms_overall <- df_tokens %>%
          dplyr::filter(upos %in% c(!! grammer_phrase)) %>%
          dplyr::count(!!word_type, sort = TRUE) %>%
          dplyr::top_n(number_of_words_to_plot) %>%
          dplyr::select(!!word_type)  %>% dplyr::collect() %>% .[[1]]

        top_terms_by_category <-  df_tokens %>%
          dplyr::filter(upos %in% c(!! grammer_phrase))%>%
          dplyr::count(!!word_type, !! categories_col, sort = TRUE) %>%
          dplyr::group_by(!!word_type) %>%
          dplyr::mutate(total_lemma = sum(n)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(prop_by_this_category = n/total_lemma) %>%
          dplyr::filter(!! word_type %in% top_terms_overall)

        # plot the top n terms
        p <- top_terms_by_category %>%
          dplyr::mutate(word_type = reorder(!!word_type, total_lemma)) %>%
          ggplot2::ggplot(ggplot2::aes(word_type, prop_by_this_category, fill = factor(!! categories_col))) + # plot beta by theme
          ggplot2::geom_col() + # as a bar plot
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          ggplot2::scale_fill_manual(values = my_palette)+
          theme_BT()+
          ggplot2::labs(title = paste("Top",number_of_words_to_plot,"Most occurring terms by",rlang::quo_name(categories_col)),
                        x = NULL,
                        y = paste("% of terms by",rlang::quo_name(categories_col)),
                        fill = NULL) + # no x label, change y label
          ggplot2::coord_flip() # turn bars sideways
        print(p)


      }

  }



