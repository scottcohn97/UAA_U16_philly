#' Scott Cohn
#' UAA U16

library("tidyverse")
library("readxl")

# Import
# font_import(paths = "~/.local/share/fonts/",prompt = F)

df_basic <- read_excel("Desktop/UAA_U16_Philly.xlsx", 
                       sheet = "Basic Stats")

df_adv <- read_excel("Desktop/UAA_U16_Philly.xlsx", 
                       sheet = "Advanced Stats")

# Drop outlier
df_basic <- df_basic %>%
  filter(`Games Played` < 12) 

# Highlight players in list
highlight.player <- c("Zachariah Hicks", "Stephen Mitchell")

# Determine whether to highlight player
df_basic$highlight <- ifelse(df_basic$Player %in% highlight.player, "highlight", "normal")
textdf <- df_basic[df_basic$Player %in% highlight.player, ]

# Points (PTS) ------------------------------------------------------------------

p0_pts <- ggplot() +
  geom_jitter(data = df_basic %>% filter(highlight == "normal"), aes(x = `Games Played`, y = PTS), color = "#bdc7cf" , size = 5, alpha = 0.8) +
  geom_point(data = df_basic %>% filter(highlight == "highlight"), 
             aes(x = `Games Played`, y = PTS), size = 5, color = "#cb3143", alpha = 0.8) + 
  geom_text(data = df_basic %>% filter(highlight == "highlight"),
            aes(x = `Games Played`, y = PTS, label = paste0(Player)), nudge_y = -5, size = 5)


p1_pts <- p0_pts + geom_abline(intercept = 0, slope = 20, color = "#bdc7cf", 
              linetype = "dashed", size = 0.5) + 
  annotate(geom = "text", x = 2, y = 50, label = "20 Points Per Game Line", color = "#296d77",
           angle = 38, size = 5 ) 

p2_pts <- p1_pts + 
  labs(y = "Points") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plotpanel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 17, color = "black", family = "Helvetica"),
        axis.text = element_text(size = 15, color = "black", family = "Helvetica"),
        axis.line = element_line(color = "black"))

ggsave("UAA_U16_Philly_PTS.pdf", width = 9, height = 7, units = "in", bg = "#f3fafc")

# Assists (AST) ----------------------------------------------------------------

p0_ast <- ggplot() +
  geom_jitter(data = df_basic %>% filter(highlight == "normal"), aes(x = `Games Played`, y = AST), color = "#bdc7cf" , size = 5, alpha = 0.8) +
  geom_point(data = df_basic %>% filter(highlight == "highlight"), 
             aes(x = `Games Played`, y = AST), size = 5, color = "#cb3143", alpha = 0.8) + 
  geom_text(data = df_basic %>% filter(highlight == "highlight"),
            aes(x = `Games Played`, y = AST, label = paste0(Player)), nudge_y = -2, size = 5)


p1_ast <- p0_ast + geom_abline(intercept = 0, slope = 5, color = "#bdc7cf", 
                               linetype = "dashed", size = 0.5) + 
  annotate(geom = "text", x = 2, y = 12, label = "5 Assists Per Game Line", color = "#296d77",
           angle = 41, size = 5 ) 

p2_ast <- p1_ast + 
  labs(y = "Total Assists") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plotpanel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 17, color = "black", family = "Helvetica"),
        axis.text = element_text(size = 15, color = "black", family = "Helvetica"),
        axis.line = element_line(color = "black"))

ggsave("UAA_U16_Philly_AST.pdf", width = 9, height = 7, units = "in", bg = "#f3fafc")


# Total Rebounds (TRB) ----------------------------------------------------------------

p0_trb <- ggplot() +
  geom_jitter(data = df_basic %>% filter(highlight == "normal"), aes(x = `Games Played`, y = TRB), color = "#bdc7cf" , size = 5, alpha = 0.8) +
  geom_point(data = df_basic %>% filter(highlight == "highlight"), 
             aes(x = `Games Played`, y = TRB), size = 5, color = "#cb3143", alpha = 0.8) + 
  geom_text(data = df_basic %>% filter(highlight == "highlight"),
            aes(x = `Games Played`, y = TRB, label = paste0(Player)), nudge_y = -3, size = 5)


p1_trb <- p0_trb + geom_abline(intercept = 0, slope = 5, color = "#bdc7cf", 
                       linetype = "dashed", size = 0.5) + 
  annotate(geom = "text", x = 3, y = 18, label = "5 Rebounds Per Game Line", color = "#296d77",
           angle = 25, size = 5 ) 

p2_trb <- p1_trb + 
  labs(y = "Total Rebounds") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plotpanel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 17, color = "black", family = "Helvetica"),
        axis.text = element_text(size = 15, color = "black", family = "Helvetica"),
        axis.line = element_line(color = "black"))

ggsave("UAA_U16_Philly_TRB.pdf", width = 9, height = 7, units = "in", bg = "#f3fafc")

# Turnovers (AST) ----------------------------------------------------------------

p0_tov <- ggplot() +
  geom_jitter(data = df_basic %>% filter(highlight == "normal"), aes(x = `Games Played`, y = TOV), color = "#bdc7cf" , size = 5, alpha = 0.8) +
  geom_point(data = df_basic %>% filter(highlight == "highlight"), 
             aes(x = `Games Played`, y = TOV), size = 5, color = "#cb3143", alpha = 0.8) + 
  geom_text(data = df_basic %>% filter(highlight == "highlight"),
            aes(x = `Games Played`, y = TOV, label = paste0(Player)), nudge_y = -0.5, size = 5)

p1_tov <- p0_tov + geom_abline(intercept = 0, slope = 2, color = "#bdc7cf", 
                               linetype = "dashed", size = 0.5) + 
  annotate(geom = "text", x = 2, y = 5, label = "2 Turnovers Per Game Line", color = "#296d77",
           angle = 39, size = 5 )

p2_tov <- p1_tov + 
  labs(y = "Turnovers") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plotpanel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 17, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.line = element_line(color = "black"))

ggsave("UAA_U16_Philly_TOV.pdf", width = 9, height = 7, units = "in", bg = "#f3fafc")
