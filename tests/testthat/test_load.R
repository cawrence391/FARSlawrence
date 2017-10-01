library(readr)
library(dplyr)

filename <- make_filename(2013)
file <- system.file("extdata",filename, package = "FARS")
data<-fars_read(file)
expect_output(str(data), "50 VARIABLES", ignore.case = TRUE)
expect_is(data,"data.frame")

