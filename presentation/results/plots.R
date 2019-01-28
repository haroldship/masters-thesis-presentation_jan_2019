library(ggplot2)
library(Hmisc)

make_accuracy_plot <- function(df, label) {
  g <- ggplot(df) +
    theme_bw(base_size=24) +
    xlab(label) +
    theme(axis.title.y=element_blank()) +
    theme(legend.position='none') +
    scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                          values=c("twodash", "solid", "dashed"))
  g
}
