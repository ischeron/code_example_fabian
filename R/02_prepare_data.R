

# lade daten --------------------------------------------------------------

# Datenset geladen von Kaggle: 
# https://www.kaggle.com/datasets/nelgiriyewithana/new-york-housing-market

df_houseprice <- read.csv("data/NY-House-Dataset.csv") %>% 
  rename_all(tolower) %>% 
  as_tibble()



# mutationen --------------------------------------------------------------

df_houseprice <- df_houseprice %>% 
  filter(price < 22500000,
         locality != "United States") %>% 
  mutate(
    type = case_when(
      type %in% c("Pending", "Land for sale", "For sale", "Foreclosure",         
                  "Condop for sale", "Coming Soon", "Mobile house for sale") ~ "other",
      type %in% c("House for sale", "Condo for sale") ~ "house/condo for sale",
      TRUE ~ type
    ),
    locality = case_when(
      locality == "New York" ~ "New York County",
      locality == "The Bronx" ~ "Bronx County",
      locality == "Queens" ~ "Queens County",
      TRUE ~ locality
    )
  )


# erstelle Gruppen anhand der Strassen ------------------------------------
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
  ) %>% 
  mutate(gruppe = as.factor(gruppe)) %>% 
  select(street_name, gruppe)

df_houseprice <- df_houseprice %>% 
  left_join(df_street_name_gruppe)

rm(df_street_name_gruppe, df_street_name_med,   
   n_low_middle_group, n_low_middle_median)
