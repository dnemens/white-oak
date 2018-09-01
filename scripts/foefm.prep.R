##Preps data for entry into FOEFM

fofem <- read.csv("C:/Users/dnemens/Dropbox/OWO/Fofem/fofem_in.csv")

#loads necessary libraries
library(tidyverse)

############################################
#bin tree size 
bins <- cut(fofem$DBH, breaks = 5)
cr <- fofem$CR

#add factor as new column, condense data into bins

summed <- fofem %>% 
  mutate(dbh.bin = bins) %>% 
  group_by(X.Std, dbh.bin) %>%
  summarize(DBH = mean(DBH), Ht = mean(Ht), Dens = n(), cr = mean(CR), FLn.SHt = mean(FLn.SHt), FS = '"S"', Fsev = "L") %>%
  mutate(Spec="QUGA4", CR = round(cr, digits = 0)) %>%
  select(X.Std, Spec, Dens, DBH, Ht,  CR, FLn.SHt, FS, Fsev) %>%
  rename('#Std'= X.Std)

write.csv(summed, file = "C:/Users/dnemens/Dropbox/OWO/Fofem/fofem_sum.csv", row.names = F)
