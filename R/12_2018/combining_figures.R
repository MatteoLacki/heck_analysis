library(tidyverse)
library(cowplot)

load(file = "data/december/figs/explained_intensity.Rd")
load(file = "data/december/figs/timings_plot.Rd")

explained_intensity_plot
timings_plot

exlained_intensity_run_time_plot = 
  plot_grid(explained_intensity_plot,
            timings_plot + theme(axis.text.y = element_blank(),
                                 axis.title.y = element_blank()),
            ncol=2, align='h', axis='r', rel_widths = c(4,3))

cowplot::ggsave(plot     = exlained_intensity_run_time_plot,
                filename = "data/december/figs/exlained_intensity_run_time_plot.pdf",
                width    = 4.5,
                height   = 2.5,
                limitsize= F)
cowplot::ggsave(plot     = exlained_intensity_run_time_plot,
                filename = "data/december/figs/exlained_intensity_run_time_plot.png",
                width    = 4.5,
                height   = 2.5,
                limitsize= F,
                dpi =500)

