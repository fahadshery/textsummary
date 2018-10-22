#' @title Create a summary of a categorical variable
#'
#' @description This function creates a bar plot by one discrete/factor/charactor variable, calculates the number of levels with counts and proportions of these levels with respect to the whole dataset and plots levels on the y-axis.
#' @param df  a dataframe/tibble
#' @param categorical_col factor/character variable to be plotted on the y-axis
#' @param get_table if TRUE, then returns a tibble/data frame of categorical_col with counts and proportions instead of a plot
#' @param plot_labs pass as a list of plot labels such as x, y, title, subtitle etc.
#' @param custom_colour_pallet pass as a vector of custom colour pallet. i.e. custom_colour_pallet = c("Promoter" = "#6400AA","Detractor" = "#E60050","Passive"="#00A0D6")
#' @param guide_legend_reverse if plot legend needs to be in reversed order
#' @param data_ordered Set to TRUE to order data for plotting
#' @importFrom magrittr %>%
#' @importFrom forcats fct_reorder
#' @importFrom stats setNames
#' @importFrom rlang :=
#' @name categorical_var_summary
#' @rdname categorical_var_summary
#' @export
#' @seealso \code{\link[textSummary]{categorical_var_summary_by_segments}}
#' @examples \dontrun{
#' categorical_var_summary(mtcars,cyl)
#' categorical_var_summary(mtcars,cyl,
#'                        custom_colour_pallet = c("8" = "#6400AA","4" = "#E60050","6"="#00A0D6"))
#'}
#'
categorical_var_summary <- function(df,
                                    categorical_col,
                                    get_table = F,
                          plot_labs = list(title = paste("Overall",
                                                         rlang::quo_name(rlang::enquo(group_var_y))," Distribution"),
                                           subtitle = paste("Total sample size = ",nrow(df)),
                                           fill = NULL),
                          custom_colour_pallet=NULL,
                          guide_legend_reverse = F,
                          data_ordered = TRUE){
  ## quiets concerns of R CMD check re: the vars's having no visible binding
  n <- total_by_group_var <- prop <- NULL
  group_var_y <- rlang::enquo(categorical_col)

  if(is.null(custom_colour_pallet)){
    # count the needed levels of a factor to fill the colour bars
    number_of_factor_levels <- df %>% dplyr::summarise(count_all = dplyr::n_distinct(!!group_var_y))
    number_of_factor_levels <- number_of_factor_levels$count_all

    #create pallet fill bars
    my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00",
                        "#333333","#666666","#DDDDDD"), length.out = number_of_factor_levels)
  }else{
    my_palette = custom_colour_pallet
  }

  total_comments_by_group_var_y <- df%>%
    dplyr::count(!! group_var_y) %>%
    dplyr::mutate(total_by_group_var = sum(n),
                  prop = n/total_by_group_var)

  if(data_ordered==TRUE){
    group_var_y_name <- rlang::quo_name(group_var_y)
    total_comments_by_group_var_y <- total_comments_by_group_var_y %>%
      dplyr::mutate(!! group_var_y_name := forcats::fct_reorder(as.factor(!! group_var_y), prop))
  }

  if(get_table == TRUE){
    y_var_name <- paste0(rlang::quo_name(group_var_y))
    total_comments_by_group_var_y <- dplyr::rename_(total_comments_by_group_var_y, .dots = setNames("total_by_group_var", paste0("total_",y_var_name)))
    return(total_comments_by_group_var_y)
  }
  else{

    total_comments_by_group_var_y%>%
      ggplot2::ggplot(ggplot2::aes(x=as.factor(!! group_var_y),y=prop,fill=as.factor(!! group_var_y)))+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_label(ggplot2::aes(label=sprintf("%1.0f%%", 100*prop, colour="white"), hjust=-0.2,fontface="bold"),
                          colour="white",size=5) +
      ggplot2::geom_text(ggplot2::aes(label=n),check_overlap = TRUE, fontface = "bold",vjust = 3,hjust = -0.5)+
      theme_BT()+
      ggplot2::scale_y_continuous(labels=scales::percent)+
      ggplot2::scale_fill_manual(values = my_palette,
                                 guide = ggplot2::guide_legend(reverse = guide_legend_reverse))+
      ggplot2::coord_flip()+
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.title=ggplot2::element_blank()) +
      ggplot2::labs(plot_labs)

  }

}
