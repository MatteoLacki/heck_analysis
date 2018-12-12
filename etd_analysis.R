library(readr)
library(tidyverse); library(ggthemes); library(cowplot)
library(scales)

# Our data
ETD = read_csv("data/september/ETD_fit_stats.csv")
# plot(ETD[,c("l1_abs", "l1_rel", 'q', 'scan', 'solutions_l1_error_abs', 'solutions_l1_error_rel', 't_total')])
ETD1 = ETD %>% filter(scan == 1)
indices_corresponding_to_first_scans = unique(ETD1$X1)



# qplot(x=t_total, y=q, data=ETD1)
# qplot(x=t_total, y=solutions_l1_error_rel, size=q, data=ETD1)

# blaise's Heck's data
load("data/parsed_csv.Rdata")
full_data$file_name = tools::file_path_sans_ext(full_data$file_name) 

csv2folder = read_csv("data/csv2folders.csv")
colnames(csv2folder) = c("csv", "folder")
# choosing only those entries where csv2folder is bijective
csv2folder_bij = csv2folder %>% group_by(folder) %>% mutate(n = n()) %>% ungroup %>% filter(n == 1)
heck = left_join(csv2folder_bij, full_data, by=c(csv='file_name'))
heck = heck %>% select(-csv, -n) %>% rename(exp=folder, name=fragment_name)
heck$source = "Heck"

# getting our estimates
our_estimates = read_csv("data/estimates.csv")
# select folders with named esimates: these correspond to single sources.
our_estimates = our_estimates %>% na.omit("name")
our_estimates = our_estimates %>% filter(i %in% indices_corresponding_to_first_scans)
findings      = our_estimates %>% group_by(i, name) %>% summarise(intensity = sum(intensity)) %>% ungroup




# ETD = left_join(ETD %>% select(-estimates), findings, by=c(X1="i"))
our_aggregated_finding = left_join(findings, 
                                    ETD %>% select(X1, exp),
                                    by=c(i="X1")) %>% select(-i)
our_aggregated_finding$source = "masstodon"

# comparison
D = bind_rows(heck %>% filter(exp %in% our_aggregated_finding$exp), 
              our_aggregated_finding)
D %>% na.omit() %>% group_by(source) %>% summarise(normed_intensity = sum(intensity))
D = D %>% na.omit() %>% group_by(source) %>% mutate(normed_intensity = intensity/max(intensity))
D = D %>% mutate(type = substr(name, 1, 1)) %>% ungroup
# 
counts = D %>% filter(type != "p") %>%
  group_by(exp, source, type) %>%
  summarize(found_frags = n_distinct(name)) %>% ungroup

counts$simple_exp = sapply(counts$exp,
                           function(x) unlist(strsplit(x,split="ETD_"), use.names = F)[2],
                           USE.NAMES = F)

found_frags_plot =
  counts %>%
  ggplot(aes(x=simple_exp, y=found_frags, fill=source)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(.~type) +
  coord_flip() +
  ylab("Number of Fragments") +
  xlab("Experiment") +
  theme(legend.position  = "top") 

ggsave("found_frags_plot.pdf", found_frags_plot, height = 5, width = 10 )

# comparing of shared and not shared fragments
fragments = D %>% 
  split(.$type)   %>%
  lapply(function(fragment_data)
    fragment_data %>%
    split(.$source) %>% 
    lapply(function(x)
      unique(x$name)
    )
  )

# what we have that heck doesn't, beyond bigger dicks, even Ania.
who_found_what = 
  D %>% group_by(exp, type, name) %>%
  summarize(who_found_it = paste(source, collapse = " and ")) %>% ungroup

who_found_what$simple_exp = sapply(who_found_what$exp,
                                   function(x) unlist(strsplit(x,split="ETD_"), use.names = F)[2],
                                   USE.NAMES = F)
who_found_what = who_found_what %>% select(-exp)


who_found_what_summary =
  who_found_what %>%
  group_by(type, simple_exp, who_found_it) %>%
  summarize(n = n()) %>%
  ungroup

position = "dodge"
  
who_found_what_summary_plot =
  who_found_what_summary %>% 
  filter(type != "p") %>%
  ggplot(aes(x=simple_exp, y=n, fill=who_found_it)) +
  geom_bar(stat="identity", position = position) +
  facet_grid(.~type) +
  coord_flip() +
  ylab("Number of Fragments") +
  xlab("Experiment") +
  theme(legend.position  = "top") +
  labs(fill = "Who found what?")

ggsave("who_found_what_summary_plot_dodge.pdf", who_found_what_summary_plot, height = 5, width = 10 )
# ggsave("who_found_what_summary_plot_stack.pdf", who_found_what_summary_plot, height = 5, width = 10 )

# boxplots: charge ~ relative error
ETD1 %>% ggplot(aes(x=q, y=1-l1_rel, group=q)) +
  # geom_violin() +
  # geom_jitter(color="red", width=.2)  +
  geom_point() +
  xlab("Charge") +
  ylab("Explained Intensity") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  scale_x_continuous(breaks=18:24) +
  coord_flip() +
  theme_minimal() +
  geom_hline(yintercept =  c(0,1), color='red')


# now: the intensities vs explained spectrum

# 
# 
plots = D %>% split(.$type) %>% lapply(function(x)
  x %>% filter(exp == "20141202_AMB_Bora_10x_40MeOH_1FA_OT_120k_10uscans_728_ETD_4ms_24precZ") %>%
  ggplot(aes(x = name, y=normed_intensity, fill=source)) +
  geom_bar(position = "dodge",
           stat     = "identity") +
  coord_flip())
#   
W = plot_grid(plotlist = plots[c(1,3)], ncol= 2)
ggsave("heck_plot.pdf", W, height = 15, width = 10 )
# 
# 
# D %>% filter(type != "p") %>%
#   group_by(source) %>% summarize(found_frags = n_distinct(name))
# 
# 
plots = D %>% split(.$type) %>% lapply(function(x)
  x %>% filter(exp == "20141202_AMB_Bora_10x_40MeOH_1FA_OT_120k_10uscans_728_ETD_4ms_24precZ") %>%
  ggplot(aes(x = name, y=intensity, fill=source)) +
  geom_bar(position = "dodge",
           stat     = "identity") +
  coord_flip())
#   
G = plot_grid(plotlist = plots[c(1,3)], ncol= 2)
ggsave("heck_plot_notnormed.png", G, height = 15, width = 15 )
# ggsave("heck_plot_notnormed.pdf", G, height = 15, width = 10 )
# 
# 
# D %>% ggplot(aes(x = name, y=intensity, fill=source)) +
#   geom_bar(position = "dodge",
#            stat     = "identity")
# 
# D %>% 
#   filter(exp == "20141202_AMB_Bora_10x_40MeOH_1FA_OT_120k_10uscans_728_ETD_4ms_24precZ") %>%
#   ggplot(aes(x = name, y=intensity, fill=source)) +
#   geom_bar(position = "dodge",
#            stat     = "identity") +
#   facet_grid(type~.)

# Times
ETD1
