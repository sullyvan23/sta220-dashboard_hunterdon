library(tidyverse)

### labels for these datasets:
### https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.nj.gov%2Feducation%2Fbudget%2Fufb%2F26%2Fdownload%2Fuserfr_layout.xlsx&wdOrigin=BROWSELINK
pupcst <- read_csv("data/pupcst.csv")
raw_enroll <- read_csv("data/raw_enroll.csv")

person_crash_reports_hunterdon_25 <- read_csv("data/person_crash_reports_hunterdon_25.csv")

hunterdon_ela_performance_24_25 <- read_csv("data/hunterdon_ela_performance_24_25.csv") %>%
                                   filter(schoolyear == "2024-25")
hunterdon_math_performance_24_25 <- read_csv("data/hunterdon_math_performance_24_25.csv") %>%
                                    filter(schoolyear == "2024-25")
hunterdon_graduation_24_25 <- read_csv("data/hunterdon_graduation_24_25.csv") %>%
                              filter(schoolyear == "2024-25")
hunterdon_grad_ready_24_25 <- read_csv("data/hunterdon_grad_ready_24_25.csv") %>%
                              filter(schoolyear == "2024-25")
hunterdon_hib_24_25 <- read_csv("data/school_hib_24_25.csv") %>%
                    filter(countyname == "Hunterdon",
                           schoolyear == "2024-25")
hunterdon_incidents_24_25 <- read_csv("data/school_incidents_24_25.csv") %>%
                          filter(countyname == "Hunterdon",
                                 schoolyear == "2024-25")
hunterdon_ratios_24_25 <- read_csv("data/school_ratios_24_25.csv") %>%
                        filter(countyname == "Hunterdon")
hunterdon_sat_24_25 <- read_csv("data/school_sat_24_25.csv") %>%
                    filter(countyname == "Hunterdon",
                           schoolyear == "2024-25")



