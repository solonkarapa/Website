
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)

##########################
##########################
##########################
#path_data <- "/Users/solon/Desktop/Website/content/post/baby_names/data/"
path_data <- "/Users/solon/Desktop/baby_names/"

uk_names_with_popularity_and_connotations <- read.csv(paste0(path_data, "uk_names_with_popularity_and_connotations.csv")) %>%
   mutate(country = "UK")

us_names_with_popularity_and_connotations <- read.csv(paste0(path_data, "us_names_with_popularity_and_connotations.txt")) %>%
   mutate(country = "US")

data <- rbind(uk_names_with_popularity_and_connotations,
              us_names_with_popularity_and_connotations)

##########################
##########################
##########################

# ---- 1. annual name shares, US girls, two snapshot years -------------------
shares <- data %>%
   filter(country == "US", sex == "F", year %in% c(1880, 2023)) %>%
   group_by(year) %>%
   mutate(share = n / sum(n)) %>%
   ungroup()

# ---- 2. expected number of children sharing a name, class of 30 -----------
#Treat each of a focal child's 29 roommates as an independent Bernoulli trial with collision probability S.
#(1 - S)^29 ≈ probability the focal child is unique in the room. This is an approximation. 
#1 - (1 - S)^29 ≈ probability she shares with at least one other. 
#30 * (...) scales to the expected count of non-unique children.
expected <- shares %>%
   group_by(year) %>%
   summarise(e = 30 * (1 - (1 - sum(share^2))^29), .groups = "drop") %>%
   mutate(label = paste0("~", round(e), " of 30 share a name"),
          col = 2.5, row = -5.4)

# ---- 3. draw a classroom -------------------------------------------------
# or each year it samples 30 names with replacement from that year's distribution 
draw_class <- function(seed) {
   set.seed(seed)
   shares %>%
      group_by(year) %>%
      group_modify(~ tibble(
         child = 1:30,
         name  = sample(.x$lowercase_name, 30, replace = TRUE, prob = .x$share)
      )) %>%
      group_by(year, name) %>%
      mutate(shared = n() > 1) %>%
      ungroup()
}

# find a seed whose draw matches the expected counts (~10 and ~1)
seed_ok <- detect(1:500, function(s) {
   d <- draw_class(s) %>% count(year, wt = shared)
   d$n[d$year == 1880] %in% 9:11 && d$n[d$year == 2023] %in% 1:2
})

classroom <- draw_class(seed_ok) %>%
   mutate(row = (child - 1) %/% 6,
          col = (child - 1) %%  6,
          label = str_to_title(name))

##########################
##########################
##########################

us <- data %>% filter(country == "US")

# annual share of the cohort
shares <- us %>%
   group_by(sex, year) %>%
   mutate(cohort = sum(n)) %>%
   group_by(sex, year, lowercase_name) %>%
   summarise(share = sum(n) / first(cohort), .groups = "drop")

# every name that was ever #1
champions <- shares %>%
   group_by(sex, year) %>%
   slice_max(share, n = 1, with_ties = FALSE) %>%
   ungroup() %>%
   distinct(sex, lowercase_name)

waves <- shares %>%
   semi_join(champions, by = c("sex", "lowercase_name"))

# one label per name, at its own peak
peaks <- waves %>%
   group_by(sex, lowercase_name) %>%
   slice_max(share, n = 1, with_ties = FALSE) %>%
   ungroup() %>%
   mutate(label = str_to_title(lowercase_name))

ggplot(waves, aes(year, share, group = lowercase_name, colour = sex)) +
   geom_line(linewidth = 0.6, alpha = 0.85) +
   ggrepel::geom_text_repel(data = peaks, aes(label = label),
                            size = 3, seed = 1, min.segment.length = 0.3,
                            segment.size = 0.2, segment.colour = "grey70",
                            box.padding = 0.35, max.overlaps = 40) +
   scale_y_continuous(labels = scales::percent, expand = expansion(c(0, 0.05))) +
   scale_colour_manual(values = c(F = "#C1666B", M = "#4281A4"), guide = "none") +
   labs(x = NULL, y = "Share of babies given this name",
        title = "Every name that was ever America's most popular",
        subtitle = "Each wave is smaller than the last") +
   theme_minimal(base_size = 11) +
   theme(panel.grid.minor = element_blank(),
         plot.subtitle = element_text(colour = "grey35"))

##########################
##########################
##########################
denom <- data %>%
   distinct(country, sex, year, per_year) %>%
   mutate(decade = floor(year / 10) * 10) %>%
   group_by(country, sex, decade) %>%
   summarise(cohort = sum(per_year), n_years = n_distinct(year), .groups = "drop") %>%
   filter(n_years == 10)

top10 <- data %>%
   mutate(decade = floor(year / 10) * 10) %>%
   group_by(country, sex, decade, lowercase_name) %>%
   summarise(births = sum(n), .groups = "drop") %>%
   inner_join(denom, by = c("country", "sex", "decade")) %>%
   mutate(prop = births / cohort) %>%
   group_by(country, sex, decade) %>%
   slice_max(prop, n = 10, with_ties = FALSE) %>%
   mutate(rank = row_number()) %>%
   ungroup()

band <- top10 %>%
   group_by(country, sex, decade) %>%
   summarise(lo = min(prop), hi = max(prop), .groups = "drop")

# one label per reign, placed at its midpoint
leaders <- top10 %>%
   filter(rank == 1) %>%
   arrange(country, sex, decade) %>%
   group_by(country, sex) %>%
   mutate(spell = cumsum(lowercase_name != lag(lowercase_name, default = ""))) %>%
   group_by(country, sex, spell, lowercase_name) %>%
   summarise(decade = mean(decade), prop = mean(prop), .groups = "drop") %>%
   mutate(label = str_to_title(lowercase_name))

ggplot(band, aes(decade)) +
   geom_ribbon(aes(ymin = lo, ymax = hi, fill = country), alpha = 0.2) +
   geom_line(data = filter(top10, rank == 1),
             aes(y = prop, colour = country), linewidth = 0.7) +
   ggrepel::geom_text_repel(data = leaders, aes(y = prop, label = label),
                            size = 3, direction = "y", min.segment.length = 0.2,
                            segment.size = 0.2, seed = 1) +
   facet_grid(country ~ sex, scales = "free_x", space = "free_x") +
   scale_y_log10(labels = scales::percent) +
   scale_colour_brewer(palette = "Set1", guide = "none") +
   scale_fill_brewer(palette = "Set1", guide = "none") +
   labs(x = NULL, y = "Share of births in decade",
        title = "The most popular names are far less popular than they used to be",
        subtitle = "Line: the decade's top name. Band: spread across the top ten.") +
   theme_minimal(base_size = 11) +
   theme(panel.grid.minor = element_blank(),
         strip.text = element_text(face = "bold"),
         panel.spacing = unit(2, "lines"))

##########################
##########################
##########################

## diversity for each year - “How concentrated are names in a few popular choices?”
hill_data <- data %>%
   group_by(year, country, sex) %>%
   group_modify(~ .x %>%
                   count(lowercase_name, wt = n, name = "births") %>%
                   mutate(p = births / sum(births)) %>%
                   summarise(
                      q0 = n(),
                      q1 = exp(-sum(p * log(p))),
                      q2 = 1 / sum(p^2),
                      .groups = "drop"
                   )) %>%
   ungroup()

hill_long <- hill_data %>%
   pivot_longer(q0:q2, names_to = "order", values_to = "effective") %>%
   mutate(order = recode(order,
                         q0 = "Richness (q=0)",
                         q1 = "Shannon (q=1)",
                         q2 = "Simpson (q=2)"))

# endpoint labels instead of a legend
ends <- hill_long %>%
   filter(order == "Shannon (q=1)") %>%
   group_by(country, sex) %>%
   slice_max(year, n = 1) %>%
   ungroup()

ggplot(filter(hill_long, order == "Shannon (q=1)"),
       aes(year, effective, colour = country)) +
   geom_line(data = filter(hill_long, order == "Simpson (q=2)"),
             aes(group = interaction(country, sex)),
             linewidth = 0.5, alpha = 0.35, linetype = "22") +
   geom_line(linewidth = 0.8) +
   ggrepel::geom_text_repel(data = ends, aes(label = toupper(country)),
                            hjust = 0, nudge_x = 4, direction = "y",
                            size = 3.2, segment.size = 0.2, seed = 1) +
   facet_wrap(~ sex, labeller = as_labeller(c(F = "Girls", M = "Boys"))) +
   scale_y_log10(breaks = c(10, 30, 100, 300, 1000, 3000),
                 labels = scales::comma) +
   scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
   scale_colour_manual(values = c(UK = "#C1666B", US = "#4281A4"),
                       guide = "none") +
   labs(x = NULL, y = "Effective number of names",
        title = "Naming has diversified for a century",
        subtitle = paste("Solid: Shannon effective names (Hill q=1).",
                         "Dashed: Simpson (q=2), which discounts rare names.")) +
   theme_minimal(base_size = 11) +
   theme(panel.grid.minor = element_blank(),
         strip.text = element_text(face = "bold", hjust = 0),
         plot.subtitle = element_text(colour = "grey35", size = 9),
         panel.spacing = unit(2, "lines"))

##########################
##########################
##########################
connotations <- c("intelligence","beauty","strength","wealth", "love","joy","religious","tradition")

#connotations <- top10$connotation

conn_time <- data %>%
   group_by(country, sex, year) %>%
   mutate(cohort = sum(n)) %>%
   ungroup() %>%
   pivot_longer(all_of(connotations), names_to = "connotation", values_to = "flag") %>%
   filter(flag) %>%
   group_by(country, sex, year, connotation) %>%
   summarise(share = sum(n) / first(cohort), .groups = "drop")

conn_plot <- conn_time %>%
   mutate(connotation = fct_reorder(connotation, share, .fun = max, .desc = TRUE),
          sex = recode(sex, F = "Girls", M = "Boys"))

ggplot(conn_plot, aes(year, share, colour = sex, linetype = country)) +
   geom_vline(xintercept = 1960, colour = "grey80", linewidth = 0.3) +
   geom_line(linewidth = 0.7) +
   facet_wrap(~ connotation, scales = "free_y", ncol = 4) +
   scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
   scale_colour_manual(values = c(Girls = "#C1666B", Boys = "#4281A4")) +
   scale_linetype_manual(values = c(US = "solid", UK = "22")) +
   labs(x = NULL, y = "Share of births", colour = NULL, linetype = NULL,
        title = "What parents named children for, 1880–2023",
        subtitle = "Share of each birth cohort given a name carrying this association.") +
   theme_minimal(base_size = 11) +
   theme(panel.grid.minor = element_blank(),
         strip.text = element_text(face = "bold", hjust = 0),
         legend.position = "top")

##########################
##########################
##########################
setwd("~/Desktop/Website/content/post/baby_names")
save(data, 
     classroom, expected, 
     waves, peaks, 
     band, leaders, top10, 
     hill_long, ends, 
     conn_plot, file = "data_processed_analysed.Rdata")
