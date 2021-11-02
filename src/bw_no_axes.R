bw_no_axes <- function() {
    theme_bw() + 
    theme(axis.title = element_text(size = 11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 14),
          text=element_text(family="Times New Roman", face="bold", size=11)
    )
}
