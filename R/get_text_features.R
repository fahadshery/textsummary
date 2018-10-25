#' @title Get verbatim text feature
#'
#' @description This is convenient wrapper function around \code{textfeatures} methods. It creates summary of text features such as how many hashtags, URLs, periods etc. are present in a verbatim row and appends and return the dataframe with textfeatures.
#' @param df  a dataframe/tibble
#' @param text_col factor/character variable that contains verbatim
#' @importFrom magrittr %>%
#' @name get_text_features
#' @rdname get_text_features
#' @export
#' @seealso \code{\link[textfeatures]{textfeatures}}
#' @examples \dontrun{
#' df <- data.frame(
#' id = c(1, 2, 3),
#' text = c("this is a feedback!\t I am fedup!! https://twitter about #service @yourCare",
#'                  "doh", "The following list:\n- one\n- two\n- three\nOkay!?!"))
#' get_text_features(df,text_col = text)
#'}
#'
get_text_features <- function(df,
                              text_col
                              ){

  ## quiets concerns of R CMD check re: the vars's having no visible binding
  text_features <- NULL

  text_col <- rlang::enquo(text_col)
  df_text <- df %>%
    dplyr::select(!! text_col)
  text_features <- textfeatures::textfeatures(df_text)
  df <- dplyr::bind_cols(df,text_features)
  return(df)
}
