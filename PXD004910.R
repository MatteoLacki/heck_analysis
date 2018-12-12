library(tidyverse)
library(mzR)

aa <- openMSfile("test.mzXML") 
aa <- openMSfile("test.mzXML")
str(aa)
runInfo(aa)
D = as_tibble(peaks(aa))
colnames(D) = c("mz", "intensity")

ggplot(D, aes(x=mz, y=intensity)) +
  geom_segment(aes(x=mz, xend=mz, y=intensity, yend=0))
