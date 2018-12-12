library(readr)
library(tidyverse); library(ggthemes); library(cowplot)
library(scales)
library(wesanderson)

ETD = read_csv("data/december/ETD_fit_stats.csv")
all(ETD$success == 'True')

# plot(ETD[,c("l1_abs", "l1_rel", 'q', 'scan', 'solutions_l1_error_abs', 'solutions_l1_error_rel', 't_total')])
# qplot(x=t_total, y=q, data=ETD)
# qplot(x=t_total, y=solutions_l1_error_rel, size=q, data=ETD)
ETD = mutate(ETD, explained_intensity = 1-solutions_l1_error_rel)
# with(ETD, plot(l1_rel, solutions_l1_error_rel))

tile_width = .3
explained_intensity_plot =
  ggplot(ETD, aes(x=ordered(q), y=explained_intensity, color=ordered(q))) +
  geom_vline(xintercept = 1:4, linetype="dashed", alpha=.2)+
  xlab("") +
  ylab("% of explained intensity") +
  scale_y_continuous(labels = scales::percent, breaks = c(.75, .8, .85)) +
  geom_jitter(width = tile_width, size=1, alpha=.8, aes(shape=factor(q))) +
  scale_x_discrete(labels=c(bquote(18^'+'), bquote(19^'+'), bquote(22^'+'), bquote(24^'+'))) +
  coord_flip() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values=wes_palette("GrandBudapest1")) +
  guides(shape=F, color=F)

comment(explained_intensity_plot) = "file: december/explained_intensity.R"
save(explained_intensity_plot, file = "data/december/figs/explained_intensity.Rd")
cowplot::ggsave(plot     = explained_intensity_plot,
                filename = "data/december/figs/explained_intensity.pdf",
                width    = 4.5,
                height   = 3,
                limitsize= F)
cowplot::ggsave(plot     = explained_intensity_plot,
                filename = "data/december/figs/explained_intensity.png",
                width    = 4.5,
                height   = 3,
                limitsize= F,
                dpi =500)
