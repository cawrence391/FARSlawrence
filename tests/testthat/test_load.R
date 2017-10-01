library(readr)
library(dplyr)

filename <- make_filename(2013)
data<-fars_read(filename)
expect_output(str(data), "50 VARIABLES", ignore.case = TRUE)

