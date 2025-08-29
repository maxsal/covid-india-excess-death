# download and save raw data from JHU CSSEGIS COVID-19 repository for
# Jan 1, 2021 and Dec 31, 2021
# ran: 2025-08-29
library(data.table)
library(glue)

path <- "~/git/covid-india-excess-death/"
data_path <- glue("{path}data/")
raw_path <- glue("{data_path}raw/")

covid20210101 <- fread(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/01-01-2021.csv"
)

covid20211231 <- fread(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/12-31-2021.csv"
)

fwrite(covid20210101, glue("{raw_path}covid20210101.csv"))
fwrite(covid20211231, glue("{raw_path}covid20211231.csv"))
