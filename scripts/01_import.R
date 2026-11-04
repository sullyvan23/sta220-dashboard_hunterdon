library(tidyverse)

### labels for these datasets:
### https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.nj.gov%2Feducation%2Fbudget%2Fufb%2F26%2Fdownload%2Fuserfr_layout.xlsx&wdOrigin=BROWSELINK
employees <- read_csv("data/employees.csv")
pupcst <- read_csv("data/pupcst.csv")
raw_enroll <- read_csv("data/raw_enroll.csv")

person_crash_reports_hunterdon_25 <- read_csv("data/person_crash_reports_hunterdon_25.csv")
