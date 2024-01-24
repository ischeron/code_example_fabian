

# Datenset geladen von Kaggle: 
# https://www.kaggle.com/datasets/nelgiriyewithana/new-york-housing-market

df_houseprice <- read.csv("data/NY-House-Dataset.csv") %>% 
  rename_all(tolower) %>% 
  as_tibble()

df_houseprice %>% glimpse()

# ausreisser und na's checken ---------------------------------------------

## na's --------------------------------------------------------------------
df_houseprice %>% DataExplorer::plot_missing()

## hampel filter für outlier detection ------------------------------------
df_houseprice_hampel <- 
  df_houseprice %>% 
  mutate(
    lower_bound = median(price) - 3 * mad(price, constant = 1),
    upper_bound = median(price) + 3 * mad(price, constant = 1),
    flag_outlier = ifelse(between(price, lower_bound, upper_bound), 0, 1)
  )

df_houseprice_hampel %>% 
  count(flag_outlier) %>% 
  mutate(percent = n / sum(n))

# Datenverust von über 17%, Ampelfilter zu konservativ für die Varianz
df_houseprice_hampel %>% 
  filter(price<4e7) %>%
  ggplot(aes(x = propertysqft,
             y = price,
             col = flag_outlier)) +
  geom_point(alpha = 0.7) +
  theme_minimal()

## top 1% des Preises der Daten entfernen ---------------------------------------------
upper_bound <- df_houseprice$price %>% quantile(probs = 0.99)

df_houseprice %>% 
  filter(price < upper_bound) %>% 
  ggplot(aes(x = propertysqft,
             y = price)) +
  geom_point(alpha = 0.7) +
  theme_minimal()

df_houseprice <- df_houseprice %>% 
  filter(price < upper_bound)

rm(df_houseprice_hampel, upper_bound)


# Landkarte -------------------------------------------------------------------

## lade Karte von New York -------------------------------------------------
ny_county <- map_data("county") %>% 
  filter(region=="new york",
         subregion %in% c("bronx", "kings", "new york", "queens", "richmond"))


## plots -------------------------------------------------------------------

# Alle Adressen auf der Karte
ny_county %>% 
  ggplot(aes(x=long, y=lat, group=group))+
  geom_polygon(fill = NA, col = "black") +
  geom_point(data=df_houseprice, 
             aes(x=longitude, y=latitude, color = price), 
             alpha = .1, size=2, inherit.aes=FALSE) +
  scale_color_viridis_c(direction = -1) +
  coord_fixed(1.3) +
  theme_void()

# Verteilung der durchschnittlichen Preise
df_houseprice %>%
  ggplot(aes(x = longitude, y = latitude, z = price)) +
  stat_summary_hex(alpha = 0.8, bins = 50) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "mean price") +
  theme_minimal()

# Verteilung der durchschnittlichen Preise auf der Karte
(p <- ny_county %>% 
  ggplot(aes(x=long, y=lat, group=group))+
  stat_summary_hex(data=df_houseprice, 
                   aes(x=longitude, y=latitude, z = price), 
                   alpha = 0.8, bins = 70, inherit.aes=FALSE) +
  geom_polygon(fill = NA, col = "black") +
  scale_fill_viridis_c(
    direction = -1,
    breaks = c(1e7, 7.5e6, 5e6, 2.5e6),
    labels = c("10 Mio $", "7.5 Mio $", "5 Mio $", "2.5 Mio $")) +
  # coord_fixed(1.3) +
  labs(fill = "Durchschn. Preis") +
  theme_void())

ggsave(plot = p, "mean_price_histogramm.png", 
       width = 700, height = 600, units = "px", dpi = 100)

rm(ny_county, p)

# andere plots ------------------------------------------------------------

## continuous --------------------------------------------------------------
# preisverteilung
df_houseprice %>% 
  ggplot(aes(x = price)) +
  geom_density()


# bath, beds & propertysqft scheinen eine positive Korrelation zum price zu haben
df_houseprice %>% DataExplorer::plot_boxplot(by = "price")
df_houseprice %>% DataExplorer::plot_correlation()
df_houseprice %>% DataExplorer::plot_correlation(type = "continuous")

plot_price <- function(var, title){
  df_houseprice %>% 
    ggplot(aes(x = {{ var }}, y = price)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(title = title) +
    theme_minimal()
}
  
plot_price(bath, "bath") + plot_price(beds, "beds") / plot_price(propertysqft, "sqft")

rm(plot_price)

## discrete ----------------------------------------------------------------

# administrative_area_level_2 fast nur auf 2 levels konzentriert
df_houseprice %>% DataExplorer::plot_bar()

### type --------------------------------------------------------------------

df_houseprice %>% 
  ggplot(aes(x = price, y = type)) +
  ggridges::geom_density_ridges() +
  theme_minimal()

# Kleinstbestände unter 1% zusammenfassen
v_type_aggregate <- df_houseprice %>% 
  count(type, sort = TRUE) %>% 
  mutate(percent = 100*n/sum(n),
         percent_cum = cumsum(percent)) %>% 
  filter(percent_cum > 99) %>% 
  pull(type)

# neue buckets mit ähnlicher Verteilung
df_houseprice %>% 
  filter(price<2000000) %>%
  mutate(type = ifelse(type %in% v_type_aggregate, "other", type)) %>%
  ggplot(aes(x = price, y = type)) +
  ggridges::geom_density_ridges() +
  theme_minimal()

df_houseprice <- df_houseprice %>% 
  mutate(
    type = case_when(
      type %in% c("Pending", "Land for sale", "For sale", "Foreclosure",         
                  "Condop for sale", "Coming Soon", "Mobile house for sale") ~ "other",
      type %in% c("House for sale", "Condo for sale") ~ "house/condo for sale",
      TRUE ~ type
    )
  )

df_houseprice %>% 
  filter(price<2000000) %>%
  ggplot(aes(x = price, y = type)) +
  ggridges::geom_density_ridges() +
  theme_minimal()

rm(v_type_aggregate)

### locality  ---------------------------------------------------------------

df_houseprice %>% distinct(locality)
df_houseprice %>% filter(locality == "United States")

df_houseprice <- df_houseprice %>% 
  filter(locality != "United States") %>% 
  mutate(
    locality = case_when(
      locality == "New York" ~ "New York County",
      locality == "The Bronx" ~ "Bronx County",
      locality == "Queens" ~ "Queens County",
      TRUE ~ locality
    )
      )

df_houseprice %>% 
  filter(price<1000000) %>% 
  ggplot(aes(x = locality, y = price)) +
  geom_boxplot()



### streetname --------------------------------------------

df_houseprice %>% distinct(street_name)
df_houseprice %>% distinct(long_name)
df_houseprice %>% distinct(formatted_address)

# formatted_address plz enziehen:
# plz korreliert mit street_name

df_houseprice %>% 
  group_by(street_name) %>% 
  mutate(med = median(price)) %>% ungroup() %>% 
  ggplot(aes(x = reorder(street_name, med), y = price)) + 
  geom_boxplot() +
  theme_minimal()

df_houseprice %>% 
  filter(price < 3000000) %>% 
  group_by(street_name) %>% 
  mutate(med = median(price)) %>% ungroup() %>% 
  ggplot(aes(x = price, y = reorder(street_name, med))) + 
  ggridges::geom_density_ridges() +
  theme_minimal()

df_street_name_med <- df_houseprice %>% 
  group_by(street_name) %>% 
  summarise(median = median(price)) %>% 
  ungroup() %>% 
  arrange(median)

n_low_middle_median <- round(0.80 * nrow(df_street_name_med))
n_low_middle_group <- 20

# kleiner und mittlerer Median price
df_street_name_gruppe <- df_street_name_med %>% 
  filter(row_number() <= n_low_middle_median) %>% 
  mutate(gruppe = cut_number(median, n = n_low_middle_group) %>% as.numeric()) %>% 
  rbind(
    df_street_name_med %>% 
      filter(row_number() > n_low_middle_median) %>% 
      mutate(gruppe = cut_number(median, n = 12) %>% as.numeric(),
             gruppe = gruppe + n_low_middle_group)
  ) %>% select(street_name, gruppe)


df_houseprice %>% 
  left_join(df_street_name_gruppe) %>% 
  # filter(price < 3000000) %>% 
  group_by(street_name) %>% 
  mutate(med = median(price)) %>% ungroup() %>% 
  ggplot(aes(x = reorder(street_name, med), y = price, col = as.factor(gruppe))) + 
  geom_boxplot() +
  theme_minimal()

rm(df_houseprice, df_street_name_med, n_low_middle_median, 
   n_low_middle_group, df_street_name_gruppe)
