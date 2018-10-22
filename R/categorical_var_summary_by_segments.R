#' @title Create a summary of a categorical variable segmented by another factor variable
#'
#' @description This function creates a bar plot by one discrete/factor/charactor variable, calculates the number of levels within this variable, plots it on the y-axis. Then, it looks at the other categorical variable and calculates counts and percentage of its levels with respect to the first factor variable. Therefore, creating first categorical segments by the second categorical levels.
#' @param df  a dataframe/tibble
#' @param categorical_col_y factor/character variable to be plotted on the y-axis
#' @param categorical_col_x factor/character variable used to calculate segments of the categorical_col_y
#' @param get_group_counts if TRUE, then returns a tibble/data frame of categorical_col_y with counts and proportions of the categorical_col_x
#' @param guide_legend_reverse if plot legend needs to be in reversed order
#' @param plot_labs pass as a list of plot labels such as x, y, title, subtitle etc.
#' @param custom_colour_pallet pass as a vector of custom colour pallet. i.e. custom_colour_pallet = c("Promoter" = "#6400AA","Detractor" = "#E60050","Passive"="#00A0D6")
#' @importFrom magrittr %>%
#' @importFrom forcats fct_reorder
#' @importFrom stats setNames
#' @importFrom rlang :=
#' @name categorical_var_summary_by_segments
#' @rdname categorical_var_summary_by_segments
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @examples \dontrun{
#' categorical_var_summary_by_segments(mtcars,cyl,gear)
#' plot_labs <-  list(title = "Overall Cyl Distribution in mtcars segmented by gear",fill = NULL)
#' categorical_var_summary_by_segments(mtcars,cyl,gear,plot_labs ,get_group_counts = TRUE)
#' }
categorical_var_summary_by_segments <- function(df,
                                       categorical_col_y,
                                       categorical_col_x,
                                       get_group_counts = FALSE,
                                       guide_legend_reverse = TRUE,
                                       plot_labs = list(title = paste(rlang::quo_name(rlang::enquo(categorical_col_y)),"by",
                                                                      paste(rlang::quo_name(rlang::enquo(categorical_col_x))," Distribution")),
                                                        subtitle = paste("Total sample size = ",nrow(df)),
                                                        fill = NULL),
                                       custom_colour_pallet=NULL){
  ## quiets concerns of R CMD check re: the vars's having no visible binding
  n <-  perc_of_comments_by_group_var_y <- NULL


  group_var_y <- rlang::enquo(categorical_col_y)
  group_var_x <- rlang::enquo(categorical_col_x)

  if(is.null(custom_colour_pallet)){

    # count the needed levels of a factor to fill the colour bars on y-axis
    number_of_factor_levels <- df %>% dplyr::summarise(count_all = dplyr::n_distinct(!!group_var_x))
    number_of_factor_levels <- number_of_factor_levels$count_all

    #create pallet fill bars
    my_palette <- rep(c("#6400AA","#E60050","#00A0D6","#E60014","#14AA37","#FFDC00",
                        "#333333","#666666","#DDDDDD"), length.out = number_of_factor_levels)

  }else{
    my_palette = custom_colour_pallet
  }

  y_var_name <- paste0(rlang::quo_name(group_var_y))
  x_var_name <- paste0(rlang::quo_name(group_var_x))

  total_comments_by_group_var_y <- df%>%
    dplyr::count(!! group_var_y)

  total_comments_by_group_var_y <- dplyr::rename_(total_comments_by_group_var_y, .dots = setNames("n", "total_comments_by_group_var_y"))

  if(get_group_counts == TRUE){

    total_comments_by_groups <- df%>%
      dplyr::count(!! group_var_x,!! group_var_y)%>%
      dplyr::inner_join(total_comments_by_group_var_y)%>%
      dplyr::mutate(perc_of_comments_by_group_var_y = n/total_comments_by_group_var_y)

    total_comments_by_groups <- dplyr::rename_(total_comments_by_groups, .dots = setNames("total_comments_by_group_var_y", paste0("total_",y_var_name)))
    total_comments_by_groups <- dplyr::rename_(total_comments_by_groups, .dots = setNames("perc_of_comments_by_group_var_y", paste0("perc_of_comments_by_",y_var_name,"_by_",x_var_name)))

    return(total_comments_by_groups)
  }
  else{
    df%>%
      dplyr::count(!! group_var_x,!! group_var_y)%>%
      dplyr::inner_join(total_comments_by_group_var_y)%>%
      dplyr::mutate(perc_of_comments_by_group_var_y = n/total_comments_by_group_var_y)%>%
      ggplot2::ggplot(ggplot2::aes(x=as.factor(!! group_var_y),y=perc_of_comments_by_group_var_y,fill=as.factor(!! group_var_x)))+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_text(ggplot2::aes(label = paste0("(",n,")")),position = ggplot2::position_stack(vjust = 0.5),
                         fontface="bold",colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label=sprintf("%1.0f%%", 100*perc_of_comments_by_group_var_y)),
                         position = ggplot2::position_stack(vjust = 0.2),fontface="bold",colour = "white")+
      theme_BT()+
      ggplot2::scale_y_continuous(labels=scales::percent)+
      ggplot2::scale_fill_manual(values = my_palette,
                                 guide = ggplot2::guide_legend(reverse = guide_legend_reverse))+
      ggplot2::coord_flip()+
      ggplot2::theme(legend.position = "top",
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.title= ggplot2::element_blank()) +
      ggplot2::labs(plot_labs)

  }
}
