
library(dplyr)
library(ggplot2)
library(ggrepel)

bananaindex <- read.csv("~/Desktop/Website/content/post/banana_index/bananaindex.csv")



colnames(bananaindex)

bananaindex %>% filter(entity == "Bananas")

bananaindex %>% mutate(mine_banana_kg = emissions_kg/X[1])


banana_index <- bananaindex %>% mutate(
   Bananas.index..100g_fat = emissions_100g_fat/20.46922,
   Bananas.index..land_use_kg = land_use_kg/2.639824,
   Bananas.index..land_use_1000kcal = land_use_1000kcal/2.717877,
   Bananas.index..land_use_100g_protein = Land.use.per.100.grams.of.protein/22.82123,
   Banana.index..land_use_100g_fat = Land.use.per.100.grams.of.fat/64.90734
)

colnames(banana_index)

## plot emissions kg
set.seed(100)
dev <- rnorm(160, sd = 10)
banana_index %>% mutate(y = 0 + dev) %>%
   ggplot(., aes(x = log(Bananas.index..kg.), y = y, label = entity, size = emissions_kg)) +
   geom_point() +
   #ggtitle("BANANA INDEX; Land use") +
   #geom_jitter(position = postition_jitter(width = 0, height = 0.001)) +
   geom_text_repel(size = 3.5) +
   geom_vline(xintercept = 0)

banana_index %>% filter(entity == "Beef burger")
banana_index %>% filter(entity == "Beef mince")
banana_index %>% filter(entity == "Strawberries")

## plot land use 
banana_index %>% arrange(land_use_kg) %>% head()

set.seed(100)
dev <- rnorm(160, sd = 10)
banana_index %>% mutate(y = 0 + dev) %>%
   ggplot(., aes(x = log(Bananas.index..land_use_kg), y = y, label = entity, size = land_use_kg)) +
   geom_point() +
   ggtitle("BANANA INDEX; Land use") +
   #geom_jitter(position = postition_jitter(width = 0, height = 0.001)) +
   geom_text_repel(size = 3.5) +
   geom_vline(xintercept = 0)

# emission vs land use 1
ggplot(banana_index, aes(x = Bananas.index..1000.kcalories., y = Bananas.index..land_use_1000kcal)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_text_repel(aes(label = entity),
                   size = 3.5) +
   geom_hline(yintercept = 1) +
   geom_vline(xintercept = 1) +
   theme_classic(base_size = 10)

# emission vs land use 2
ggplot(banana_index, aes(x = Bananas.index..kg., y = Bananas.index..land_use_kg)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_text_repel(aes(label = entity),
                   size = 3.5) +
   geom_hline(yintercept = 1) +
   geom_vline(xintercept = 1) +
   theme_classic(base_size = 10)

# emission vs land use 3
ggplot(banana_index, aes(x = Bananas.index..100g.protein., y = Bananas.index..land_use_100g_protein)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_text_repel(aes(label = entity),
                   size = 3.5) +
   geom_hline(yintercept = 1) +
   geom_vline(xintercept = 1) +
   theme_classic(base_size = 10)

# bottom left quadrant are foods that are more land energy producing efficient than bananas
ggplot(banana_index, aes(x = Banana.index..land_use_100g_fat, y = Bananas.index..land_use_100g_protein)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_text_repel(aes(label = entity),
                   size = 3.5) +
   geom_hline(yintercept = 1) +
   geom_vline(xintercept = 1) +
   theme_classic(base_size = 10)

ggplot(banana_index, aes(x = Bananas.index..land_use_1000kcal, y = Bananas.index..land_use_kg)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_text_repel(aes(label = entity),
                   size = 3.5) +
   geom_hline(yintercept = 1) +
   geom_vline(xintercept = 1) +
   theme_classic(base_size = 10)

ggplot(banana_index, aes(x = Bananas.index..kg., y = Bananas.index..1000.kcalories.)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1) +
   geom_text_repel(aes(label = entity),
                   size = 3.5) +
   geom_hline(yintercept = 1) +
   geom_vline(xintercept = 1) +
   theme_classic(base_size = 10)
