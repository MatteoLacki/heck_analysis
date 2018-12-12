library(readr)
library(tidyverse); library(ggthemes); library(cowplot)
library(scales)
library(wesanderson)

MD = as_tibble(read.csv('/Users/matteo/Projects/masstodon/AC_plots/masstodon/data/december/intensities.csv'))
csv2folder = read_csv("data/csv2folders.csv")
colnames(csv2folder) = c("csv", "folder")

precursors_stats = csv2folder %>% count(folder) %>% group_by(n) %>% count %>%
  rename(precursors_No=n, count=nn) %>% ungroup

# Blaise's Heck's data
load("data/september/parsed_csv.Rdata")
full_data$file_name = tools::file_path_sans_ext(full_data$file_name) 
FD = full_data %>% filter(str_detect(file_name, "ETD")) # we analysed only ETD anyway.
# FD has all necessary information about the proteins: it's always the same protein, but with different
# modifications.

# We neglect the precursor in the c-z fragment.
no_prec_FD =
  left_join(FD, csv2folder, by=c("file_name"="csv")) %>%
  group_by(folder, fragment_name) %>% 
  summarize(precursors_No = n(), intensity=sum(intensity)) %>% ungroup

D = bind_rows(rename(no_prec_FD, frag=fragment_name, exp=folder) %>%
              select(-precursors_No) %>%
              mutate(who="Heck"),
              mutate(MD, who="masstodon"))
SD = D %>% 
  group_by(exp, frag) %>% summarize(
    who = paste(who, sep="", collapse="_"),
    whoCnt = n()
  ) %>% ungroup

SD_stat = 
  SD %>% group_by(frag_type=str_sub(frag,1,1), exp) %>%
  count(who) %>% ungroup %>%
  mutate(exp=str_replace_all(exp, "20141202_AMB_", ""),
         exp=str_replace_all(exp, "_10x_40MeOH_1FA_OT_","_"),
         exp=str_replace_all(exp, "_10uscans_", "_"),
         exp=str_replace_all(exp, "_ETD_", "_"),
         exp=str_replace_all(exp, "precZ", ""))

who_what_base = SD_stat %>%
  filter(frag_type != "p") %>%
  ggplot(aes(x=exp, y=n, fill=who)) +
  geom_bar(stat="identity", position='dodge') +
  facet_grid(.~frag_type) +
  coord_flip()

SD_stat_wide = SD_stat %>% spread(who, n)
SD_stat_wide[is.na(SD_stat_wide)] = 0
SD_stat_wide = SD_stat_wide %>% mutate(Heck = Heck+Heck_masstodon,
                                       masstodon = masstodon + Heck_masstodon,
                                       exp = str_replace_all(exp, "_", " "),
                                       frag_type = paste(frag_type, "fragments", sep=' '),
                                       exp_type = ifelse(str_detect(exp, "pBora"),
                                                         "phosphorylated", "canonical"),
                                       exp = str_replace(exp, "pBora ",""),
                                       exp = str_replace(exp, "Bora ",""),
                                       z = as.integer(str_replace(str_extract(exp, "ms \\d\\d"), "ms ", "")),
                                       exp = str_c(exp, "+"))

SD_stat_long = SD_stat_wide %>% filter(frag_type != "p fragments") %>%
  select(-Heck_masstodon) %>%
  gather("who", "peptide_No", 3:4)

who_what_med =
  SD_stat_wide %>%
  filter(frag_type != 'p fragments') %>%
  ggplot(aes(x=exp)) +
  geom_bar(aes(y=Heck_masstodon, fill=''), stat='identity') +
  geom_point(data=SD_stat_long,
             aes(y=peptide_No, color=who, shape=who), size=3) +
  ylab("Number of Peptides") +
  coord_flip() +
  facet_grid(exp_type~frag_type, scales="free_y", space="free_y") +
  scale_color_manual(values=c("red", "orange"), labels=c("Heck", "Masstodon"), name="") +
  scale_shape_manual(values=c(18,20), labels=c("Heck", "Masstodon"), name="") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values=c("grey"), labels=c("Both"), name="") +
  guides(fill = guide_legend(order = 1), 
         color = guide_legend(order = 2),
         shape = guide_legend(order = 2))

cowplot::ggsave("data/december/figs/who_found_what.pdf",
                plot=who_what_med, height = 5, width = 4)
cowplot::ggsave("data/december/figs/who_found_what.png",
                plot=who_what_med, height = 5, width = 8, dpi=600)
comment(who_what_med) = "who_found_what.R"
save(who_what_med, file="data/december/figs/who_found_what.Rd")

