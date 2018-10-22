#' @title Create a summary of categorical variable with respect to a numeric variable
#'
#' @description This function creates a bar plot by one discrete/factor/charactor variable, calculates the number of levels within this variable & plots it on the y-axis, then summarises this variable's each level with a numeric variable by a given mathematical function. Also labels the numerical summary by each group.
#' @param df  a dataframe/tribble
#' @param categorical_col_y factor/character variable to be plotted on the y-axis
#' @param num_var numeric variable which will be used for mathematical summary
#' @param get_summary if TRUE, then returns a tibble/data frame of categorical_col_y with mathematical summary by the num_var
#' @param plot_labs pass as a list of plot labels such as x, y, title, subtitle etc.
#' @param custom_colour_pallet pass as a vector of custom colour pallet. i.e. custom_colour_pallet = c("Promoter" = "#6400AA","Detractor" = "#E60050","Passive"="#00A0D6")
#' @param guide_legend_reverse if plot legend needs to be in reversed order
#' @param data_ordered if the statiscal summary needs to be ordered
#' @param FUN mathematical operation to be performed i.e. mean, median, sd, mode etc.
#' @importFrom magrittr %>%
#' @name categorical_var_numeric_summary
#' @rdname categorical_var_numeric_summary
#' @export
#' @seealso \code{\link[textSummary]{categorical_var_summary_by_segments}}
#' @examples \dontrun{
#' categorical_var_numeric_summary(mtcars,cyl,mpg)
#' my_colours <- c("4" = "#6400AA","6" = "#E60050","8" = "black")
#' categorical_var_numeric_summary(mtcars, cyl,mpg,FUN = mean, custom_colour_pallet = my_colours)
#'categorical_var_numeric_summary(mtcars, cyl,mpg,FUN = sum, get_summary = TRUE)
#'}
#'
categorical_var_numeric_summary <- function(df,
                                            categorical_col_y,
                                          num_var,
                                          get_summary = F,
                                          plot_labs = list(title = paste("Summary of", rlang::quo_name(rlang::enquo(categorical_col_y))," by",
                                                                         lazyeval::expr_text(FUN),
                                                                         rlang::quo_name(rlang::enquo(num_var))),
                                                           subtitle = paste("Total verbatim = ",nrow(df)),
                                                           fill = NULL,
                                                           y = paste(lazyeval::expr_text(FUN),rlang::quo_name(rlang::enquo(num_var)))),
                                          custom_colour_pallet=NULL,
                                          guide_legend_reverse = F,
                                          data_ordered = TRUE,
                                          FUN=mean){

  fun_result <-  n <-  sample_size <- NULL

  group_var_y <- rlang::enquo(categorical_col_y)
  num_var <- rlang::enquo(num_var)

  summary_by_group_var_y <- df%>%
    dplyr::group_by(!! group_var_y) %>%
    dplyr::summarise(fun_result = FUN(!! num_var),
                     sample_size = n())

  if(data_ordered==TRUE){
    group_var_y_name <- rlang::quo_name(group_var_y)
    summary_by_group_var_y <- summary_by_group_var_y %>%
      dplyr::mutate(!! group_var_y_name := forcats::fct_reorder(as.factor(!! group_var_y), fun_result))
  }

  if(get_summary == TRUE){
    y_var_name <- paste0(rlang::quo_name(group_var_y))
    x_var_name <- paste0(rlang::quo_name(num_var))
    summary_by_group_var_y <- dplyr::rename_(summary_by_group_var_y, .dots = setNames("sample_size", paste0("total_",y_var_name)))
    summary_by_group_var_y <- dplyr::rename_(summary_by_group_var_y, .dots = setNames("fun_result", paste0(lazyeval::expr_text(FUN),"_",x_var_name)))
    return(summary_by_group_var_y)
  }
  else{
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

    summary_by_group_var_y%>%
      ggplot2::ggplot(ggplot2::aes(x=as.factor(!! group_var_y),y=fun_result,fill=as.factor(!! group_var_y)))+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_label(ggplot2::aes(label=round(fun_result,1),hjust=-0.2,fontface="bold"),
                          show.legend = FALSE,
                          colour="white",size=5) +
      ggplot2::geom_text(ggplot2::aes(label=sample_size),check_overlap = TRUE, fontface = "bold",vjust = 3,hjust = -0.5)+
      theme_BT()+
      ggplot2::scale_y_continuous(labels=scales::percent)+
      ggplot2::scale_fill_manual(values = my_palette,
                                 guide = ggplot2::guide_legend(reverse = guide_legend_reverse))+
      ggplot2::coord_flip()+
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.title=ggplot2::element_blank()) +
      ggplot2::labs(plot_labs)

  }

}
