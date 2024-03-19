# Packages ----------------------------------------------------------------

pkgs <- c('tidyverse','viridis','RColourBrewer','ggdark','gganimate','ggrepel')
#install.packages(pkgs)
#devtools::install_github("thomasp85/transformr") # transformr package required for gganimate

library(ggdark)
library(tidyverse); theme_set(theme_light(base_size = 20))
library(viridis)
library(RColorBrewer)
library(gganimate)
library(ggrepel)


# Load and tidy the data --------------------------------------------------

wildPops <- read.csv("WCVI_wild_KOBE_data.csv") %>% 
  mutate(updated_smsy = updated_smsy - (4200-1964)) %>% # Change value for San Juan. Was 4200 should be 1964.
  pivot_longer(matches("Parken|updated"),
               names_sep = "_",
               names_to = c("model","param")) %>% 
  pivot_wider(names_from = param, 
              values_from = value) %>% 
  mutate(y = nonterm_Claq_ER/umsy,
         x = spawners/smsy,
         quadrant = case_when(
           y >= 1 & x <= 1 ~ 1,
           y >= 1 & x > 1 ~ 5, # Nothing falls in this zone
           y < 1 & between(x,0.8,1) ~ 2, # Amber zone
           y < 1 & x <= 0.8 ~ 3,
           y < 1 & x > 1 ~ 4
         ) %>% factor(),
         colour = case_when(
           quadrant == 1 ~ "firebrick2",
           quadrant == 2 ~ "darkgoldenrod1",
           quadrant == 3 ~ "darkorange2",
           quadrant == 4 ~ "springgreen3",
           quadrant == 5 ~ "darkgoldenrod1" # Nothing falls in this quadrant
         ))


# Plots -------------------------------------------------------------------

# Dataframe for quadrant labels
quadLabs <- data.frame(
  label = c(
    "Overfishing",
    "Overfishing but spawning\nbiomass sufficient",
    "Fisheries reduced to\nallow rebuilding",
    "Sustainable fishery"
  ),
  x = c(0.2,1.8,0.2,1.8),
  y = c(1.9,1.9,0.1,0.1)
) 

# Base KOBE plot
kobe1 <- wildPops %>% 
  filter(model == "updated") %>% 
  arrange(year) %>% 
  ggplot(aes(x, y)) +
  geom_hline(yintercept = 1, lty = 2, colour = "grey70") +
  geom_vline(xintercept = 1, lty = 2, colour = "grey70") +
  annotate("rect", xmin = 0.8, xmax = 1, ymin = -1, ymax = 1, alpha = .2) +
  annotate("text",label = "Amber zone", x = 0.9, y = 0.25, angle = 90) +
  geom_text(data = quadLabs, aes(label = label)) +
  coord_cartesian(xlim = c(0,2),ylim = c(0,2)) +
  scale_size_continuous(labels = scales::comma) +
  guides(colour = "none",alpha = "none") +
  labs(x = "Spawner abundance / Spawner abundance at MSY",
       y = "Fishery ER / ER at MSY",
       size = "Spawner\nabundance") +
  theme(legend.position = c(0.65,0.7),
        legend.background = element_rect(colour="black"),
        panel.grid = element_blank())

# Save base KOBE plot 
ggsave("KOBE_plot_2d_base.png",
       kobe1
       ,width = 13.3333, height = 7.5, units = "in")

# Save 2D kobe plot
ggsave("KOBE_plot_2d.png",
       kobe1 + 
         geom_text_repel(aes(label=year)) +
         geom_point(aes(size = spawners), colour = filter(wildPops, model=="updated")$colour)
       ,width = 13.3333, height = 7.5, units = "in")


# Animated version
(kobe2 <- kobe1 +
    geom_path(aes(alpha = year)) +
    geom_point(aes(size = spawners), colour = filter(wildPops, model=="updated")$colour) +
    transition_reveal(along = year, keep_last = T) +
    labs(subtitle = 'Year: {frame_along}') 
)

# Animate it
anim <- animate(kobe2, duration = 60, fps = 20, 
                width = 13.3333, height = 7.5, units = "in", 
                res = 150, #Resolution is in pixels-per-inch (ppi)
                renderer = gifski_renderer(), 
                end_pause = 20*10) # End pause is in frames: 20 frames * 10 = 10 seconds
anim_save("KOBE_ts_animation.gif",animation = anim)

