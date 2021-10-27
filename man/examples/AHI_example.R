library(dplyr)
data(djan)
output_table <- hydro_events(dataframe = djan,
                             q = discharge,
                             datetime = time,
                             window = 21)

output_table %>%
  filter(he == 2) %>%
  AHI(q = discharge, ssc = SS)
