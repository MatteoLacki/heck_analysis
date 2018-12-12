library(tidyverse)
library(readr)
library(ggthemes)
library(cowplot)
library(scales)

D = read_csv("~/Projects/masstodon/dumps/belgian/synapt_results.csv")
# getting rid of one data point
D %>% group_by(WV, WH) %>% summarize(n=n(), exp_no = paste0(i, collapse=" ")) %>% ungroup %>% filter(n == 2)
D = D %>% filter(i != 11)
D$exp = tools::file_path_sans_ext(D$exp)

prob_panel_bc_fill = "#ECF7FF"
prob_panel_bc_fill = "white"

D4errorplot = D %>% select(WH, WV, l1_rel, solutions_l1_error_rel) %>% gather("error", "value", 3:4) 
p1 = 
  D4errorplot %>% filter(WV == 300) %>%
  ggplot(aes(x = ordered(WH), y=1-value,color=error, group=error)) +
  xlab("wave height [velocity at 300 m/s]") +
  ylab("explained intensity")
  
p2 = 
  D4errorplot %>% filter(WH == 1.5) %>%
  ggplot(aes(x = ordered(WV), y=1-value, color=error, group=error)) +
  xlab("wave velocity [height at 1.5 V]") +
  ylab("explained intensity")
  
add_geoms = function(p, axis_position="left") p +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1),
                     breaks = (0:10)/10,
                     position=axis_position) +
  scale_color_manual(values=c("#999999", "#E69F00"),
                     name="explained intensity",
                     labels=c("all spectrum", "fitted spectrum"))+
  theme_minimal() +
  geom_hline(yintercept=c(0,1)) +
  theme(legend.position=c(.3,.8))+
  coord_flip()

r1 = add_geoms(p1); r2 = add_geoms(p2, "right")
error_panel = plot_grid(r2, r1, ncol=1, rel_heights = c(2.5, 1))

aas = "RPKPQQFFGLM"
aas = strsplit(aas, '')[[1]]


names(aas) = as.character(0:10)

prob_plot = function(cz = "cz.prob", axis_position="left"){
  D4probs =
    D %>% select(WV, WH, contains("cz.prob")) %>%
    gather("aa_no", "p", 3:13) %>%
    mutate(aa_no = as.integer(gsub("cz.prob.", "", aa_no)))
  add_plot = function(pl) pl +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1),
                       breaks = c(0.2, .5, .8)) +
    geom_hline(yintercept=1/9, color="orange", size=2) +
    geom_line()  +
    theme_minimal() +
    geom_point() +
    facet_grid(.~aa_no,
               labeller = as_labeller(aas))+
    theme(
      panel.background = element_rect(fill = prob_panel_bc_fill)
    )+
    coord_flip() 
  p1 = D4probs %>% filter(WV == 300) %>%
    ggplot(aes(x = ordered(WH), y=p, group=WV)) +
    xlab("wave height [velocity at 300 m/s]") +
    ylab("probability")
  p2 = D4probs %>% filter(WH == 1.5) %>%
    ggplot(aes(x = ordered(WV), y=p, group=WH)) +
    xlab("wave velocity [height at 1.5 V]") +
    ylab("probability")
  q1 = add_plot(p1)
  q2 = add_plot(p2)
  return(list(q2, q1))
}
prob_plots = prob_plot("cz_simple.prob")
probs = plot_grid(plotlist = prob_plots, ncol=1, rel_heights = c(2.5,1))


D4intensity = 
  D %>% select(WH, WV, cz.ETnoD_precursor, cz.PTR_precursor) %>%
  rename(ETnoD = cz.ETnoD_precursor, PTR = cz.PTR_precursor) %>%
  gather("reaction", "intensity", 3:4)

i1 =D4intensity %>% filter(WV == 300) %>%
    ggplot(aes(x = ordered(WH), y=intensity, group=reaction, color=reaction)) +
    xlab("wave height [velocity at 300 m/s]") +
    ylab("intensity")

i2 = D4intensity %>% filter(WH == 1.5) %>%
    ggplot(aes(x = ordered(WV), y=intensity + 1, group=reaction, color=reaction)) +
    xlab("wave velocity [height at 1.5 V]") +
    ylab("intensity")

int = D4intensity$intensity + 1

add_geoms = function(p, axis_position="left") p + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(trans = "log10",
                     labels = scales::comma,
                     position = axis_position,
                     limits = c(min(int), max(int))
                     ) +
  theme_minimal() +
  theme(legend.position="bottom")

g1 = add_geoms(i1)
g2 = add_geoms(i2, "right") + ylab("")
intensities_plot = plot_grid(g1, g2, ncol=2, rel_widths = c(1, 2))


top = plot_grid(error_panel, probs, ncol=2, rel_widths = c(.7, 2), labels = c("A", "B"))
voltron = plot_grid(top, intensities_plot, nrow=2, rel_heights = c(2,.5), labels = c("", "C"))

ggsave("synapt.pdf", voltron, width = 17, height = 15, units = "in", limitsize = F)



