library(readr)
library(tidyverse)


I = read_csv("~/Projects/masstodon/dumps/belgian/synapt/intensities.csv")
I = I %>% mutate(frag_name = str_sub(name,1,1),
                 frag_ind  = as.integer(ifelse(frag_name == 'p', -1, str_sub(name,start=2))))

IS = I %>%
  group_by(index, WH, WV, formula, name, frag_name, frag_ind) %>%
  summarize(intensity=sum(intensity)) %>%
  ungroup

meta = IS %>% group_by(name, WH, WV, frag_name, frag_ind) %>% summarize %>% ungroup

ISS = IS %>% select(index,WH, WV,name,intensity) %>% spread(name, intensity)
ISS[is.na(ISS)] = NA
ISS = ISS %>% gather("mol", "intensity", 4:ncol(ISS))
ISS = ISS %>% mutate(frag_name = str_sub(mol,1,1),
                     frag_ind  = as.integer(ifelse(frag_name == 'p', -1, str_sub(mol, start=2))),
                     frag_name = ifelse(frag_name == 'c', 'c fragments', ifelse(frag_name == 'z', 'z fragments', 'precursor')))

ISS

WH15plot =
  ISS %>% filter(WH == 1.5) %>%
  ggplot(aes(x=ordered(WV), y=intensity, color=factor(frag_ind), group=mol))+
  geom_line() +
  scale_y_log10() +
  xlab("wave velocity [wave height at 1.5 V]") +
  coord_flip() +
  facet_grid(.~frag_name) +
  theme_minimal()

WV300plot =
ISS %>% filter(WV == 300) %>%
  ggplot(aes(x=ordered(WH), y=intensity, color=factor(frag_ind), group=mol))+
  geom_line() +
  scale_y_log10() +
  xlab("wave height [wave velocity at 300 m/s]") +
  coord_flip() +
  facet_grid(.~frag_name) +
  theme_minimal()

cowplot::plot_grid(WH15plot + theme(axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    legend.position = 'none'),
                   WV300plot + theme(strip.text = element_blank(),
                                     legend.position = 'none'),
                   nrow=2, rel_heights=c(2,1),
                   align = 'b')

ISS = ISS %>% mutate(band = ifelse(WV == 300, "wave velocity at 300 m/s", "wave height at 1.5 V"),
                     band = ifelse(index == 22, "wave height at 1.5 V", band))

ISS %>% filter(band == "wave velocity at 300 m/s")
ISS %>% filter(band == "wave height at 1.5 V")

ISS %>%
  gather("what_changes", "value", 2:3) %>%
  ggplot(aes(x=value, y=intensity, color=factor(frag_ind), group=mol))+
  geom_line() +
  scale_y_log10() +
  facet_grid(frag_name~band,
             scales = "free_x",
             space = "free_x") +
  theme_minimal()
