library(tidyverse)
library(kableExtra)
#list all tbl files
tbl_files<-list.files('results/', '*tbl.csv', full.names = T)
#combine them to one dataframe
suppl_tbl <- lapply(tbl_files, read.csv) %>% bind_rows()

#suppl_tbl %>% kbl()

suppl_tbl %>%
knitr::kable(
  format = "simple",
  align = "l",
  booktabs = TRUE,
  longtable = TRUE,
  linesep = "",
) %>%
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15"
  )
