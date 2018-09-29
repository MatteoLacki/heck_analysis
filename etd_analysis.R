library(readr)
library(tidyverse); library(ggthemes); library(cowplot)

# Our data
ETD = read_csv("data/ETD_fit_stats.csv")
# plot(ETD[,c("l1_abs", "l1_rel", 'q', 'scan', 'solutions_l1_error_abs', 'solutions_l1_error_rel', 't_total')])
ETD1 = ETD %>% filter(scan == 1)
qplot(x=t_total, y=q, data=ETD1)
qplot(x=t_total, y=solutions_l1_error_rel, size=q, data=ETD1)

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
findings      = our_estimates %>% group_by(i, name) %>% summarise(intensity = sum(intensity)) %>% ungroup


# ETD = left_join(ETD %>% select(-estimates), findings, by=c(X1="i"))
our_aggregated_finding = left_join(findings, 
                                    ETD %>% select(X1, exp),
                                    by=c(i="X1")) %>% select(-i)

our_aggregated_finding$source = "masstodon"

# comparison
D = bind_rows(heck, our_aggregated_finding)
D %>% ggplot(aes(x = name, y=intensity, fill=source)) +
  geom_bar(position = "dodge",
           stat     = "identity")



