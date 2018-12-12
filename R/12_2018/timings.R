library(readr)
library(tidyverse); library(ggthemes); library(cowplot)
library(scales)
library(wesanderson)

ETD = read_csv("data/december/ETD_fit_stats.csv")
ETD = mutate(ETD, explained_intensity = 1-solutions_l1_error_rel)

with(ETD, plot(t_total, q))
with(ETD, plot(t_total, l1_rel))
with(ETD, plot(t_total, l1_rel))

# plot(select(ETD, l1_abs, l1_rel, q, solutions_l1_error_abs, solutions_l1_error_rel, t_total))


tile_width = .3
timings_plot =
  ggplot(ETD, aes(x=ordered(q), y=t_total, color=ordered(q))) +
  geom_vline(xintercept = 1:4, linetype="dashed", alpha=.2)+
  xlab("Charge") +
  ylab("Run time [s]") +
  # scale_y_continuous(labels = scales::percent) +
  geom_jitter(width = tile_width, size=1, alpha=.8, aes(shape=factor(q))) +
  scale_x_discrete(labels=c(bquote(18^'+'), bquote(19^'+'), bquote(22^'+'), bquote(24^'+'))) +
  coord_flip() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values=wes_palette("GrandBudapest1")) +
  guides(shape=F, color=F)


comment(timings_plot) = "file: december/timings.R"
save(timings_plot, file = "data/december/figs/timings_plot.Rd")
cowplot::ggsave(plot     = timings_plot,
                filename = "data/december/figs/timings_plot.pdf",
                width    = 4.5,
                height   = 3,
                limitsize= F)
cowplot::ggsave(plot     = timings_plot,
                filename = "data/december/figs/timings_plot.png",
                width    = 4.5,
                height   = 3,
                limitsize= F,
                dpi =500)
