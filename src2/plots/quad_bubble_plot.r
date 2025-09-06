# src/plots/quad_bubble_plot.r
quad_bubble_plot <- function(df, xvar, yvar, labelvar="country",
                             xlab=NULL, ylab=NULL, title=NULL) {
  ggplot(df, aes(x=.data[[xvar]], y=.data[[yvar]], label=.data[[labelvar]])) +
    geom_point(size=3, alpha=0.7) +
    ggrepel::geom_text_repel() +
    scale_x_log10() + scale_y_log10() +
    geom_hline(yintercept = median(df[[yvar]], na.rm=TRUE), linetype="dashed") +
    geom_vline(xintercept = median(df[[xvar]], na.rm=TRUE), linetype="dashed") +
    labs(x=xlab, y=ylab, title=title) +
    theme_minimal(base_family="serif")
}
