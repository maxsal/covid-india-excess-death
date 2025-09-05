# libraries and such -----------------------------------------------------------
library(data.table)
library(tidyverse)
library(janitor)
library(patchwork)
library(glue)

path <- "~/git/covid-india-excess-death/"
data_path <- glue("{path}data/")
raw_path <- glue("{data_path}raw/")
fig_path <- glue("{path}figures/")

# data -------------------------------------------------------------------------
## population
### read in population data by state/union territory from 2018-2021
pop <- fread(glue("{raw_path}india_pop.csv"))

### create a combined Ladakh and Jammu & Kashmir entry to align with Covid data
pop <- rbindlist(
  list(
    pop,
    data.table(
      sut = "Ladakh and Jammu & Kashmir",
      p2014 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2014]),
      p2015 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2015]),
      p2016 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2016]),
      p2017 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2017]),
      p2018 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2018]),
      p2019 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2019]),
      p2020 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2020]),
      p2021 = sum(pop[sut %in% c("Ladakh", "Jammu & Kashmir"), p2021])
    )
  ),
  fill = TRUE,
  use.names = TRUE
)

## reported covid deaths
### pulled from JHU CSSE GitHub (see `pull_jhu_covid_data.R`)
covid20210101 <- fread(glue("{raw_path}covid20210101.csv"))[
  Country_Region == "India",
] |>
  clean_names()

covid20211231 <- fread(glue("{raw_path}covid20211231.csv"))[
  Country_Region == "India",
] |>
  clean_names()

covid <- merge.data.table(
  covid20210101[, .(sut = province_state, covid20210101 = deaths)],
  covid20211231[, .(sut = province_state, covid20211231 = deaths)],
  by = "sut",
  all = TRUE
)

covid[, covid2021 := covid20211231 - covid20210101]
covid <- covid[sut != "Unknown"]

### update state/union territory names to match population and CRS data
covid[sut == "Andaman and Nicobar Islands", sut := "A & N Islands"]
covid[
  sut == "Dadra and Nagar Haveli and Daman and Diu",
  sut := "D & N Haveli and Daman & Diu"
]
covid[sut == "Jammu and Kashmir", sut := "Jammu & Kashmir"]

### add combined Ladakh and Jammu & Kashmir and India entries
covid <- rbindlist(
  list(
    covid,
    data.table(
      sut = "Ladakh and Jammu & Kashmir",
      covid2021 = sum(covid[sut %in% c("Ladakh", "Jammu & Kashmir"), covid2021])
    )
  ),
  fill = TRUE,
  use.names = TRUE
)
covid <- rbindlist(
  list(
    covid,
    data.table(
      sut = "India",
      covid2021 = sum(covid[, covid2021])
    )
  ),
  fill = TRUE,
  use.names = TRUE
)

## CRS deaths
### CRS deaths data from Table 5 in 2025 report on 2021 CRS data
### url:
crs <- fread(glue("{raw_path}india_registered_deaths.csv"), header = TRUE) |>
  clean_names()
setnames(crs, "state_ut", "sut")
years <- 2014:2021
for (y in years) {
  crs[[paste0("x", y)]] <- as.numeric(crs[[paste0("x", y)]])
  setnames(crs, old = paste0("x", y), new = paste0("crs", y))
}
crs_vars <- c("sut", "type", paste0("crs", years))
crs <- crs[, ..crs_vars]

## merge population, covid, and crs data
merged <- Reduce(
  function(x, y) merge(x, y, by = "sut", all = TRUE),
  list(pop, covid, crs)
)

int_vars <- names(merged)[sapply(merged, is.integer)]
merged[,
  (int_vars) := lapply(.SD, function(x) as.numeric(x)),
  .SDcols = int_vars
]

# calculate --------------------------------------------------------------------
## excess deaths
### estimate death rate: CRS deaths / population
### estimated for 2019 and average for 2014-2019
merged[, `:=`(
  # death rate for 2019
  dr2019 = crs2019 / p2019,
  # average of death rates from 2014-2019
  dr2014_2019 = rowMeans(
    mapply(
      `/`,
      .SD[, .(crs2014, crs2015, crs2016, crs2017, crs2018, crs2019)],
      .SD[, .(p2014, p2015, p2016, p2017, p2018, p2019)]
    ),
    na.rm = TRUE
  )
)]

### estimate 2021 expected deaths: population * 2019 death rate
merged[, `:=`(
  expected2021_2019 = p2021 * dr2019,
  expected2021_2014_2019 = p2021 * dr2014_2019
)]

### estimate 2021 excess deaths: CRS deaths - expected deaths
merged[, `:=`(
  excess2021_2019 = crs2021 - expected2021_2019,
  excess2021_2014_2019 = crs2021 - expected2021_2014_2019
)]

### simple difference between 2021 and 2020
merged[, `:=`(
  excess2021 = crs2021 - crs2020
)]

### estimate p-score
merged[, `:=`(
  pscore2021 = (excess2021 / expected2021_2019),
  pscore2021_2019 = (excess2021_2019 / expected2021_2019),
  pscore2021_2014_2019 = (excess2021_2014_2019 / expected2021_2014_2019),
  ratio2021 = (excess2021 / covid2021),
  ratio2021_2019 = (excess2021_2019 / covid2021),
  ratio2021_2014_2019 = (excess2021_2014_2019 / covid2021)
)]

india <- merged[sut == "India", ]
merged <- merged[!c(sut %in% c("India", "Ladakh", "Jammu & Kashmir")), ]

# stats ------------------------------------------------------------------------
## reported deaths in 2021
cat("CRS deaths in 2021:", india[, crs2021], "\n")
cat("CRS deaths in 2020:", india[, crs2020], "\n")
cat(
  "% increase in CRS deaths from 2020 to 2021:",
  round((india[, crs2021] - india[, crs2020]) / india[, crs2020] * 100, 2),
  "\n"
)
cat(
  "Increase in CRS deaths from 2020 to 2021:",
  (india[, crs2021] - india[, crs2020]),
  "\n"
)

# plot -------------------------------------------------------------------------
# split by state vs. union territory
# bin by ratio (>10, 5-10, 2-5, < 2)
cols <- c(
  "10+" = "#D55E00",
  "[5, 10)" = "#FF9933",
  "(2, 5)" = "#F0E442",
  "<=2" = "#138808"
)

merged[, `:=`(
  ratio2021_2019_cat = factor(
    fcase(
      ratio2021_2019 >= 10,
      "10+",
      ratio2021_2019 > 5,
      "[5, 10)",
      ratio2021_2019 > 2,
      "(2, 5)",
      ratio2021_2019 <= 2,
      "<=2"
    ),
    levels = c("<=2", "(2, 5)", "[5, 10)", "10+")
  ),
  ratio2021_2014_2019_cat = factor(
    fcase(
      ratio2021_2014_2019 >= 10,
      "10+",
      ratio2021_2014_2019 > 5,
      "[5, 10)",
      ratio2021_2014_2019 > 2,
      "(2, 5)",
      ratio2021_2014_2019 <= 2,
      "<=2"
    ),
    levels = c("<=2", "(2, 5)", "[5, 10)", "10+")
  )
)]
india[, `:=`(
  ratio2021_2019_cat = factor(
    fcase(
      ratio2021_2019 >= 10,
      "10+",
      ratio2021_2019 > 5,
      "[5, 10)",
      ratio2021_2019 > 2,
      "(2, 5)",
      ratio2021_2019 <= 2,
      "<=2"
    ),
    levels = c("<=2", "(2, 5)", "[5, 10)", "10+")
  ),
  ratio2021_2014_2019_cat = factor(
    fcase(
      ratio2021_2014_2019 >= 10,
      "10+",
      ratio2021_2014_2019 > 5,
      "[5, 10)",
      ratio2021_2014_2019 > 2,
      "(2, 5)",
      ratio2021_2014_2019 <= 2,
      "<=2"
    ),
    levels = c("<=2", "(2, 5)", "[5, 10)", "10+")
  )
)]

indias <- india |>
  ggplot(aes(x = reorder(sut, -ratio2021_2014_2019), y = ratio2021_2014_2019)) +
  geom_col(aes(fill = ratio2021_2014_2019_cat), width = 0.8) +
  geom_hline(yintercept = 1, linewidth = 1, color = "black") +
  geom_hline(
    yintercept = india[, ratio2021_2014_2019],
    linewidth = 1,
    color = "#FF9933"
  ) +
  geom_label(
    aes(
      y = 0,
      label = trimws(format(round(ratio2021_2014_2019, 1), nsmall = 1))
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    hjust = 0.5,
    size = 3,
    angle = 90,
    color = "black",
    fontface = "bold"
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    expand = expansion(mult = c(0.1, 0))
  ) +
  scale_fill_manual(values = cols) +
  labs(
    title = "Ratio of excess deaths to reported COVID deaths in India, 2021",
    subtitle = "Zoomed in on the y-axis",
    x = "",
    y = "Excess death ratio",
    caption = paste0(
      "Note: Black line at 1 represents equal excess and COVID deaths. ",
      "Orange line represents the national ratio of ",
      round(india[, ratio2021_2014_2019], 1),
      ". "
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "top",
    plot.title = element_text(hjust = 0, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    legend.title = element_blank()
  )

states <- merged[type == "State" & covid2021 >= 500] |>
  ggplot(aes(x = reorder(sut, -ratio2021_2014_2019), y = ratio2021_2014_2019)) +
  geom_col(aes(fill = ratio2021_2014_2019_cat), width = 0.8) +
  geom_hline(yintercept = 1, linewidth = 1, color = "black") +
  geom_hline(
    yintercept = india[, ratio2021_2014_2019],
    linewidth = 1,
    color = "#FF9933"
  ) +
  geom_label(
    aes(
      y = 0,
      label = trimws(format(round(ratio2021_2014_2019, 1), nsmall = 1))
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    hjust = 0.5,
    size = 3,
    angle = 90,
    color = "black",
    fontface = "bold"
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    expand = expansion(mult = c(0.1, 0))
  ) +
  scale_fill_manual(values = cols) +
  labs(
    title = "Ratio of excess deaths to reported COVID deaths in India, 2021",
    subtitle = "Zoomed in on the y-axis",
    x = "",
    y = "Ratio",
    caption = paste0(
      "Note: Black line at 1 represents equal excess and COVID deaths. ",
      "Orange line represents the national ratio of ",
      round(india[, ratio2021_2014_2019], 1),
      ". "
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "top",
    plot.title = element_text(hjust = 0, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    legend.title = element_blank()
  )

uts <- merged[type == "Union Territory" & covid2021 >= 500] |>
  ggplot(aes(x = reorder(sut, -ratio2021_2014_2019), y = ratio2021_2014_2019)) +
  geom_col(aes(fill = ratio2021_2014_2019_cat), width = 0.8) +
  geom_hline(yintercept = 1, linewidth = 1, color = "black") +
  geom_hline(
    yintercept = india[, ratio2021_2014_2019],
    linewidth = 1,
    color = "#FF9933"
  ) +
  geom_label(
    aes(
      y = 0,
      label = trimws(format(round(ratio2021_2014_2019, 1), nsmall = 1))
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    hjust = 0.5,
    size = 3,
    angle = 90,
    color = "black",
    fontface = "bold"
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    expand = expansion(mult = c(0.1, 0))
  ) +
  scale_fill_manual(values = cols) +
  labs(
    title = "Ratio of excess deaths to reported COVID deaths in India, 2021",
    subtitle = "Zoomed in on the y-axis",
    x = "",
    y = "Ratio",
    caption = paste0(
      "Note: Black line at 1 represents equal excess and COVID deaths. ",
      "Orange line represents the national ratio of ",
      round(india[, ratio2021_2014_2019], 1),
      ". "
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "top",
    plot.title = element_text(hjust = 0, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    legend.title = element_blank()
  )

leg <- ggpubr::get_legend(states)

dsn <- "
##AA##
BBCCDD
"

edr_plot <- wrap_plots(
  # leg,
  indias +
    labs(title = "B.", x = "Nationwide", subtitle = "", caption = "") +
    theme(legend.position = "none"),
  states +
    labs(title = "", x = "States", subtitle = "", caption = "") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top"
    ),
  uts +
    labs(title = "", x = "Union territories", subtitle = "", caption = "") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    ),
  # design = dsn,
  widths = c(1, 25, 4)
) +
  plot_annotation(
    # title = "Ratio of excess deaths to reported COVID deaths in India, 2021",
    # caption = "Zoomed in on the y-axis [0, 10).",
    theme = theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0, face = "bold"),
      plot.caption = element_text(hjust = 0, color = "gray30")
    )
  )

####
deaths <- data.table(
  year = 2012:2021,
  death_reg = c(
    5850176,
    6086616,
    6138182,
    6267685,
    6349259,
    6463779,
    6950607,
    7641076,
    8115882,
    10224506
  )
)
deaths[, excess := death_reg - shift(death_reg, 1)]
deaths[, excess_pct := (excess / shift(death_reg, 1))]
deaths

coef <- 2500000 / .30

deaths_plot <- deaths[year != 2012, ] |>
  ggplot(aes(x = year, y = excess)) +
  geom_col(fill = "#138808") +
  geom_text(
    aes(
      x = year,
      y = excess,
      label = scales::percent(excess_pct, accuracy = 0.1)
    ),
    hjust = 0.5,
    vjust = -0.3,
    color = "#FF9933",
    fontface = "bold"
  ) +
  # geom_line(aes(y = excess_pct * coef), linewidth = 1, color = "#FF9933") +
  # geom_hline(yintercept = 0, linewidth = 1, color = "black") +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 2500000, 500000),
    limits = c(0, 2500000)
  ) +
  scale_x_continuous(breaks = seq(2012, 2021, 1)) +
  labs(
    title = "A.",
    # title = "Excess deaths in India, 2013-2021",
    x = "",
    y = "Change in number of deaths from previous year",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    legend.position = "top",
    plot.title = element_text(hjust = 0, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    axis.title.y = element_text()
  )
ggsave(
  plot = deaths_plot,
  filename = glue("{fig_path}excess_deaths.png"),
  width = 6,
  height = 4,
  dpi = 320
)

(deaths_over_edr_plot <- deaths_plot / edr_plot)
ggsave(
  filename = glue("{fig_path}excess_deaths_ratio_plot2021_2014_2019.pdf"),
  plot = deaths_over_edr_plot,
  width = 10,
  height = 10,
  device = cairo_pdf
)

ggsave(
  filename = glue("{fig_path}excess_deaths_ratio_plot2021_2014_2019.tiff"),
  plot = deaths_over_edr_plot,
  width = 10,
  height = 10,
  units = "in",
  dpi = 600
)

# load coordinates data for India states and union territories
cord <- fread(glue("{raw_path}india_cord.csv"))
cols_dt <- unique(cord[, .(Source, color)])
cols <- cols_dt[, color]
names(cols) <- cols_dt[, Source]

completeness_plot <- cord |>
  ggplot(aes(x = year, y = cord, group = Source, color = Source)) +
  geom_rect(
    aes(xmin = 2020.25, xmax = 2021.5, ymin = -Inf, ymax = Inf),
    fill = "gray90",
    color = NA,
    alpha = 0.5
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 2000:2021) +
  scale_color_manual(values = cols) +
  scale_y_continuous(breaks = seq(50, 85, 5), limits = c(50, 85)) +
  labs(
    x = "",
    y = "Completeness of Death Record (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    legend.position = "top",
    plot.title = element_text(hjust = 0, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    axis.title.y = element_text()
    # legend.title = element_blank()
  )
completeness_plot
ggsave(
  plot = completeness_plot,
  filename = glue("{fig_path}cord.pdf"),
  width = 6,
  height = 4,
  device = cairo_pdf
)

#####
dt2019 <- fread("~/Downloads/india_mortality_2019_csv.txt")

younger <- c(
  "Less than 1 Year",
  "1 Year - 4 Year",
  "5 Year - 14 Year",
  "15 Year - 24 Year",
  "25 Year - 34 Year",
  "35 Year - 44 Year",
  "45 Year - 54 Year",
  "55 Year - 64 Year"
)
older <- c("65 Year - 69 Year", "70 Year & above")

dt2019[Age_Group %in% younger, sum(Total_Deaths_2019)]
dt2019[Age_Group %in% older, sum(Total_Deaths_2019)]

death <- fread(glue("{raw_path}annual-deaths-by-age.csv"))

death2019 <- death[Entity == "India" & Year == 2019, ] |>
  melt(
    id.vars = c("Entity", "Code", "Year")
  )

death2021 <- death[Entity == "India" & Year == 2021, ] |>
  melt(
    id.vars = c("Entity", "Code", "Year")
  )

younger <- c(
  "Deaths - Sex: all - Age: 0-4 - Variant: estimates",
  "Deaths - Sex: all - Age: 5-9 - Variant: estimates",
  "Deaths - Sex: all - Age: 10-14 - Variant: estimates",
  "Deaths - Sex: all - Age: 15-19 - Variant: estimates",
  "Deaths - Sex: all - Age: 20-24 - Variant: estimates",
  "Deaths - Sex: all - Age: 25-29 - Variant: estimates",
  "Deaths - Sex: all - Age: 30-34 - Variant: estimates",
  "Deaths - Sex: all - Age: 35-39 - Variant: estimates",
  "Deaths - Sex: all - Age: 40-44 - Variant: estimates",
  "Deaths - Sex: all - Age: 45-49 - Variant: estimate",
  "Deaths - Sex: all - Age: 50-54 - Variant: estimates",
  "Deaths - Sex: all - Age: 55-59 - Variant: estimates",
  "Deaths - Sex: all - Age: 60-64 - Variant: estimates"
)
older <- setdiff(death2019[, unique(variable)], younger)

death2019[,
  age_cat := fcase(
    variable %in% younger,
    "[0, 65)",
    variable %in% older,
    "[65+)"
  )
]
death2021[,
  age_cat := fcase(
    variable %in% younger,
    "[0, 65)",
    variable %in% older,
    "[65+)"
  )
]


death2019[, .(deaths = sum(value)), age_cat]
death2021[, .(deaths = sum(value)), age_cat]

###
pop <- fread(glue("{raw_path}population-by-age-group-with-projections.csv"))

change_vars <- names(pop)[!names(pop) %in% c("Entity", "Code", "Year")]

pop[, (change_vars) := lapply(.SD, function(x) x / 1), .SDcols = change_vars]

pop2019 <- pop[Entity == "India" & Year == 2019, ] |>
  melt(
    id.vars = c("Entity", "Code", "Year")
  )
pop2021 <- pop[Entity == "India" & Year == 2021, ] |>
  melt(
    id.vars = c("Entity", "Code", "Year")
  )

youngPop <- c(
  "Population - Sex: all - Age: 25-64 - Variant: estimates",
  "Population - Sex: all - Age: 0-24 - Variant: estimates"
)
oldPop <- c(
  "Population - Sex: all - Age: 65+ - Variant: estimates"
)

pop2019[,
  age_cat := fcase(
    variable %in% youngPop,
    "[0, 65)",
    variable %in% oldPop,
    "[65+)"
  )
]
pop2021[,
  age_cat := fcase(
    variable %in% youngPop,
    "[0, 65)",
    variable %in% oldPop,
    "[65+)"
  )
]

pop2019[, sum(value), age_cat]
pop2021[, sum(value), age_cat]
