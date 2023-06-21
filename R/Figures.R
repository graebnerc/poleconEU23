here::i_am("R/Figures.R")
library(here)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyr)
library(countrycode)
library(data.table)
library(icaeDesign)
library(ggrepel)
library(ggpubr)

source(here("R/country_classification.R"))

# Figure 1: divergence-----------------
## Setup data---------------------------

income_growth <- fread(here("data/wdi_gdp.csv"))

filter_y_min <- 1995
filter_y_max <- 2020

income_growth_groups <- income_growth %>%
  dplyr::filter(year>=filter_y_min, year<=filter_y_max) %>%
  mutate(
    iso3c = countrycode(iso2c, "iso2c", "iso3c"),
    c_group_jee = get_country_classification(iso3c, "jee"),
    c_group_geo = get_country_classification(iso3c, "geo_struc")
    )

min_year <- min(income_growth_groups$year)
max_year <- max(income_growth_groups$year)
gdp_minyear <- paste0("gdp_ppp_pc_", min_year)
gdp_maxyear <- paste0("gdp_ppp_pc_", max_year)
pop_minyear <- paste0("population_", min_year)
pop_maxyear <- paste0("population_", max_year)

mean_gdp <- income_growth_groups %>%
  group_by(year) %>%
  summarise(
    mean_gdp_ppp_pc = mean(gdp_ppp_pc), 
    mean_gdp_ppp_pc_w = weighted.mean(gdp_ppp_pc, population)
  )

income_growth_itager <- income_growth_groups %>% 
  dplyr::filter(country %in% c("Italy", "Germany")) %>% 
  dplyr::mutate(
    g_avg_gdp_ppp_pc = gdp_ppp_pc,
    g_avg_gdp_ppp_pc_w = gdp_ppp_pc
  ) %>%
  left_join(mean_gdp, by = "year") %>%
  mutate(
    gdp_dev = g_avg_gdp_ppp_pc - mean_gdp_ppp_pc,
    gdp_dev_w = g_avg_gdp_ppp_pc_w - mean_gdp_ppp_pc_w
  ) %>% 
  dplyr::select(all_of(c(
    'year', 'country', 'g_avg_gdp_ppp_pc', 'g_avg_gdp_ppp_pc_w', 
    'mean_gdp_ppp_pc', 'mean_gdp_ppp_pc_w', 'gdp_dev', 'gdp_dev_w'
  ))) %>% 
  as_tibble(.)

## JEE-based descriptives---------------------

income_groups_jee <- income_growth_groups %>%
  group_by(year, c_group_jee) %>%
  summarise(
    g_avg_gdp_ppp_pc = mean(gdp_ppp_pc), 
    g_avg_gdp_ppp_pc_w = weighted.mean(gdp_ppp_pc, population), 
    .groups = "drop"
  ) %>%
  left_join(mean_gdp, by = "year") %>%
  mutate(
    gdp_dev = g_avg_gdp_ppp_pc - mean_gdp_ppp_pc,
    gdp_dev_w = g_avg_gdp_ppp_pc_w - mean_gdp_ppp_pc_w
  ) 

income_growth_core <- income_groups_jee %>% 
  dplyr::filter(c_group_jee=="Core") %>% 
  dplyr::select(year, g_avg_gdp_ppp_pc) %>% 
  dplyr::rename(g_avg_gdp_ppp_core = g_avg_gdp_ppp_pc)

income_groups_jee_final <- income_groups_jee %>% 
  bind_rows(rename(income_growth_itager, c_group_jee=country)) %>% 
  left_join(., income_growth_core, by=c("year")) %>% 
  dplyr::mutate(gdp_rel_core = g_avg_gdp_ppp_pc / g_avg_gdp_ppp_core) %>% 
  dplyr::mutate(
    gdp_rel_central = g_avg_gdp_ppp_pc / g_avg_gdp_ppp_core, 
    c_group_jee_lab = gsub(pattern=" ", replacement="\\\n ", x=c_group_jee))

## Struc-geo-based descriptives--------------
income_groups_geo <- income_growth_groups %>%
  group_by(year, c_group_geo) %>%
  summarise(
    g_avg_gdp_ppp_pc = mean(gdp_ppp_pc), 
    g_avg_gdp_ppp_pc_w = weighted.mean(gdp_ppp_pc, population), 
    .groups = "drop"
  ) %>%
  left_join(mean_gdp, by = "year") %>%
  mutate(
    gdp_dev = g_avg_gdp_ppp_pc - mean_gdp_ppp_pc,
    gdp_dev_w = g_avg_gdp_ppp_pc_w - mean_gdp_ppp_pc_w
  ) 

income_growth_centraleu <- income_groups_geo %>% 
  dplyr::filter(c_group_geo=="Central Europe") %>% 
  dplyr::select(year, g_avg_gdp_ppp_pc) %>% 
  dplyr::rename(g_avg_gdp_ppp_central = g_avg_gdp_ppp_pc)

income_groups_geo_final <- income_groups_geo %>% 
  bind_rows(rename(income_growth_itager, c_group_geo=country)) %>% 
  left_join(., income_growth_centraleu, by=c("year")) %>% 
  dplyr::mutate(
    gdp_rel_central = g_avg_gdp_ppp_pc / g_avg_gdp_ppp_central, 
    c_group_geo_lab = gsub(pattern=" ", replacement="\\\n ", x=c_group_geo))
  
## Plots--------------------------------
set.seed(123)
income_groups_geo_plt <- income_groups_geo_final %>% 
  dplyr::filter(!c_group_geo %in% c("Italy", "Germany")) %>% 
  ggplot(
  data = ., 
  mapping = aes(
    x=year, y=gdp_rel_central, color=c_group_geo)) +
  geom_point(
    mapping = aes(shape=c_group_geo, color=c_group_geo)) + 
  geom_line(
    mapping = aes(linetype=c_group_geo, color=c_group_geo), 
    show.legend = FALSE) +
  scale_y_continuous(labels = label_percent(), limits = c(0.3, 1.4)) +
  scale_x_continuous(limits = c(1995, 2025), breaks = seq(1995, 2020, 5)) +
  scale_color_grey(start = 0.0, 0.6) +
  geom_label_repel(
    data = dplyr::filter(
      income_groups_geo_final, 
      year==max_year, 
      !c_group_geo %in% c("Italy", "Germany")
      ),
    mapping = aes(x=year, y=gdp_rel_central, label=c_group_geo_lab), 
    xlim = c(2021, 2025), size = 2.75, max.time = 1, max.iter = 50000,
    show.legend = FALSE) +
  labs(
    title = paste0("Deviation from average income of Central Europe" ),
    y = "Deviation (average GDP p.c. \n of Central Europe=100%)") +
  guides(color = "none", shape = guide_legend(ncol = 3)) +
  theme_icae() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 13),
        legend.text = element_text(size = 11))
income_groups_geo_plt

income_groups_jee_plt <- ggplot(
  data = income_groups_jee_final, 
  mapping = aes(
    x=year, y=gdp_rel_core, color=c_group_jee)) +
  geom_point(
    mapping = aes(shape=c_group_jee, color=c_group_jee)) + 
  geom_line(
    mapping = aes(linetype=c_group_jee, color=c_group_jee), 
    show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0.4, 1.4, 0.2), labels = label_percent(), limits = c(0.3, 1.5)) +
  scale_x_continuous(limits = c(1995, 2025), breaks = seq(1995, 2020, 5)) +
  scale_color_grey(start = 0.0, 0.6) +
  geom_label_repel(
    data = dplyr::filter(income_groups_jee_final, year==max_year),
    mapping = aes(x=year, y=gdp_rel_core, label=c_group_jee_lab), 
    xlim = c(2021, 2025), size = 2.75,
    show.legend = FALSE) +
  labs(
    title = paste0("Deviation from average income of the Core" ),
    y = "Deviation (average GDP p.c. \n of core countries=100%)") +
  guides(color = "none", shape = guide_legend(ncol = 3)) +
  theme_icae() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 13),
        legend.text = element_text(size = 11))
income_groups_jee_plt

## Countries within groups--------------

single_countries_jee <- income_growth_groups %>% 
  dplyr::mutate(
    mean_gdp=weighted.mean(gdp_ppp_pc, w=population), 
    .by = c("year", "c_group_jee")
  ) %>% 
  dplyr::mutate(c_group_jee=factor(
    c_group_jee, 
    levels = c("Core", "Periphery", "East", "Finance"), 
    ordered = TRUE)
  ) %>% 
  ggplot(
    mapping = aes(
      x=year, 
      y=gdp_ppp_pc, 
      group=country)
  ) +
  geom_point(
    alpha=0.25,color="white") +
  geom_line(alpha=0.25) +
  geom_line(
    mapping = aes(
      x=year, 
      y=mean_gdp, 
      group=country)
  ) +
  scale_color_grey(aesthetics = c("color", "fill")) + 
  # scale_color_euf(
  #   palette = "mixed", 
  #   reverse = -1, 
  #   aesthetics=c("color", "fill")
  # ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "k")
  ) +
  scale_x_continuous(breaks = seq(1995, 2020, 10)) +
  facet_wrap(~ c_group_jee, scales = "fixed", ncol = 4) +
  labs(
    title = "GDP per capita in the four development models",
    y = "GDP per capita\n (PPP; population weighted avg.)"
  ) +
  theme_icae() +
  theme(
    axis.title.x = element_blank()
  )
single_countries_jee

## Full plot------

ineq_plot <- ggarrange(
  income_groups_geo_plt, income_groups_jee_plt, ncol = 2, 
  labels = c("A)", "B)"))

ineq_plot <- ggarrange(
  ineq_plot, single_countries_jee, nrow = 2,
  labels = c(NA, "C)"), heights = c(1.2, 1))

ineq_plot <- annotate_figure(
  ineq_plot, 
  bottom = text_grob("Data: World Bank; own calculation.", hjust = -0.73))

ggsave(
  plot = ineq_plot, 
  filename = here("figures/Figure-1-DevelopmentModels.pdf"), 
  width = 9, height = 7)


# Figure 2: challenges general -------------------------

long_term_gdp_data <- as_tibble(fread(here("data/wdi_gdp.csv")))
long_term_gdp_data_groups <- long_term_gdp_data %>%
  mutate(
    iso3c = countrycode(iso2c, "iso2c", "iso3c"),
    c_group_jee = get_country_classification(iso3c, "jee")
  ) %>% 
  as_tibble()
 
## Challenge of dynamics - Ireland----------------

dyn_challenge_ire_groups <- long_term_gdp_data_groups %>% 
  filter(c_group_jee %in% c("Finance", "Periphery")) %>% 
  filter(country!="Cyprus") %>% 
  summarize(
    gdp_mean =mean(GDP_constUSD),
    gdp_sd = sd(GDP_constUSD),
    gdp_lower = quantile(GDP_constUSD, 0.25, na.rm = TRUE),
    gdp_upper = quantile(GDP_constUSD, 0.75, na.rm = TRUE),
    .by = c("year", "c_group_jee"))

dyn_challenge_ire <- long_term_gdp_data_groups %>% 
  filter(country == "Ireland") %>% 
  select(year, GDP_constUSD) %>% 
  rename(gdp_mean=GDP_constUSD) %>% 
  mutate(c_group_jee = "Ireland", gdp_sd=NA, gdp_lower=NA, gdp_upper=NA) %>% 
  bind_rows(dyn_challenge_ire_groups) %>% 
  filter(year>=1970)

dyn_challenge_ire_plot <- ggplot(
  data = dyn_challenge_ire, 
  aes(x=year, y=gdp_mean, color=c_group_jee)
  ) +
  geom_line() + geom_point(aes(shape=c_group_jee)) +
  scale_colour_grey() +
  scale_y_continuous(
    limits = c(0, 100000), 
    labels = number_format(scale = 0.001, suffix = "k")
    ) +
  labs(
    title = "The challenge of dynamics for Ireland",
    y = "GDP per capita (cons. USD)",
    caption = "Data: Eurostat, own calculation.") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  theme_icae() +
  theme(axis.title.x = element_blank())
dyn_challenge_ire_plot

## Challenge of granularity - Germany----------------

local_data_hr <- as_tibble(fread(here("data/eurostat_nuts2_ready.csv")))

geo_geo_code <- local_data_hr %>% 
  select(geo_code, geo) %>% 
  unique()

local_data_de <- local_data_hr %>% 
  filter(country=="Germany", time>=2016) %>% 
  summarise(gdp_pc_mean=mean(values), .by = "geo") %>% 
  left_join(., geo_geo_code, by=c("geo"))

spatial_data_raw <- as_tibble(fread(here("data/eurostat_spatial.csv")))
local_data_de_full <- left_join(
  x = local_data_de, 
  y = spatial_data_raw, 
  by = c("geo_code"="geo"))

local_plot_de <- ggplot() + 
  geom_polygon(
    data = local_data_de_full, 
    mapping = aes(
      x=long, y = lat, group = group, fill=gdp_pc_mean),
    colour = "black", linewidth = 0.2) +
  # scale_color_viridis_c(
  #   labels=number_format(scale = 0.001, suffix = "k"),
  #   option = "D", aesthetics=c("color", "fill")) +
  scale_color_distiller(
    labels=number_format(
      scale = 0.001, suffix = "k"), 
    palette = "Greys", 
    aesthetics=c("fill"), 
    direction = 1
    ) +
  labs(
    title = "The challenge of granularity in Germany",
    caption = "Data: Eurostat.") +
  guides(fill = guide_colorbar(
    title = "GDP per\n inhabitant", label = TRUE)) +
  theme_void() +
  theme(legend.position = "left", plot.title = element_text(size=11, hjust = 0.5))
local_plot_de

## Overalls challenge plot I--------------------
challenge_plot_I <- ggarrange(
  local_plot_de, dyn_challenge_ire_plot, ncol = 2, labels = c("A)", "B)"))

ggsave(
  plot = challenge_plot_I, 
  filename = here("figures/Figure-2-Challenge-I.pdf"),
  width = 8, height = 3)

# Figure 3: challenges - Italy -------------------------
## Challenge of dynamics - Italy-------------------
dyn_challenge_ita_groups <- long_term_gdp_data_groups %>% 
  filter(c_group_jee %in% c("Core", "Periphery")) %>% 
  summarize(
    gdp_mean =mean(GDP_constUSD, na.rm = TRUE),
    gdp_sd = sd(GDP_constUSD, na.rm = TRUE),
    gdp_lower = quantile(GDP_constUSD, 0.25, na.rm = TRUE),
    gdp_upper = quantile(GDP_constUSD, 0.75, na.rm = TRUE),
    .by = c("year", "c_group_jee"))

dyn_challenge_ita <- long_term_gdp_data_groups %>% 
  filter(country == "Italy") %>% 
  select(year, GDP_constUSD) %>% 
  rename(gdp_mean=GDP_constUSD) %>% 
  mutate(c_group_jee = "Italy", gdp_sd=NA, gdp_lower=NA, gdp_upper=NA) %>% 
  bind_rows(dyn_challenge_ita_groups) 

dyn_challenge_ita_plot <- ggplot(
  data = dyn_challenge_ita, 
  aes(x=year, y=gdp_mean, color=c_group_jee)
) +
  geom_line() +
  scale_colour_grey() +
  scale_y_continuous(
    limits = c(0, 55000), 
    labels = number_format(scale = 0.001, suffix = "k")
  ) +
  labs(
    title = "The challenge of dynamics for Italy",
    y = "GDP per capita (cons. USD)",
    caption = "Data: Eurostat, own calculation.") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  theme_icae() +
  theme(axis.title.x = element_blank())
dyn_challenge_ita_plot

## Challenge of granularity - Italy----------------
local_data_ita <- local_data_hr %>% 
  filter(country=="Italy", time>=2016) %>% 
  summarise(gdp_pc_mean=mean(values), .by = "geo") %>% 
  left_join(., geo_geo_code, by=c("geo"))

local_data_ita_full <- left_join(
  x = local_data_ita,
  y = spatial_data_raw, 
  by = c("geo_code"="geo"))

local_plot_ita <- ggplot() + 
  geom_polygon(
    data = local_data_ita_full, 
    mapping = aes(
      x=long, y = lat, group = group, fill=gdp_pc_mean),
    colour = "black", linewidth = 0.2) +
  # scale_color_viridis_c(
  #   labels=number_format(scale = 0.001, suffix = "k"),
  #   option = "D", aesthetics=c("color", "fill")) +
  scale_color_distiller(
    labels=number_format(
      scale = 0.001, suffix = "k"), 
    palette = "Greys", 
    aesthetics=c("fill"), 
    direction = 1
  ) +
  labs(
    title = "The challenge of granularity in Italy",
    caption = "Data: Eurostat.") +
  guides(fill = guide_colorbar(
    title = "GDP per\n inhabitant", label = TRUE)) +
  theme_void() +
  theme(
    legend.position = "left", 
    plot.title = element_text(size=11, hjust = 0.5)
    )
local_plot_ita

## Challenge plot II--------------------
challenge_plot_II <- ggarrange(
  local_plot_ita, dyn_challenge_ita_plot, ncol = 2, labels = c("A)", "B)"))

ggsave(
  plot = challenge_plot_II, 
  filename = here("figures/Figure-3-Challenge-II.pdf"),
  width = 8, height = 3)
