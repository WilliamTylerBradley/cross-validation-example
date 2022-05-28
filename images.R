library(tidyverse)
library(e1071)
library(gt)

set.seed(11)

df <- expand.grid(x1 = seq(1, 5),
                  x2 = seq(1, 5)) %>%
  mutate(class = as.factor(if_else(sqrt((x1 - 5)^2 + (x2 - 5)^2) < 3.5, 'A', 'B'))) %>%
  arrange(sample(1:25)) %>%
  group_by(class) %>%
  slice(1:6) %>%
  mutate(fold = sample(c(1, 1,
                          2, 2,
                          3, 3))) %>%
  ungroup() %>%
  arrange(-x2, x1) %>%
  mutate(id = row_number())

ggplot(data = df,
       aes(x1, x2, color = class, label = id)) +
  geom_point(size = 5,
             fill = "white",
             shape = 21) +
  geom_text(size = 3) +
  scale_color_manual(values = c("#366442", "#DEAC10"),
                     breaks = c("A", "B")) +
  coord_equal() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white"))
ggsave("images/ids.png",
       height = 3,
       width = 4)

## Try out models
pred_model_1 <- svm(class ~ x1 + x2, data = df, kernel = "linear", scale = FALSE)
df <- df %>%
  mutate(model_1 = predict(pred_model_1, .))

background <- expand_grid(x1 = seq(1, 5, .01),
                          x2 = seq(1, 5, .01))
background <- background %>%
  mutate(model_1 = predict(pred_model_1, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2,
                fill = model_1),
            color = NA,
            alpha = .5) +
  geom_point(data = df,
             aes(x1, x2,
                 color = model_1),
             size = 5) +
  geom_point(data = df,
             aes(x1, x2,
                 color = class),
             size = 3) +
  geom_text(size = 3) +
  scale_color_manual(values = c("#366442", "#DEAC10"),
                     breaks = c("A", "B")) +
  scale_fill_manual(values = c("#366442", "#DEAC10"),
                     breaks = c("A", "B")) +
  coord_equal() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white"))
ggsave("images/full_model_1.png",
       height = 3,
       width = 4)

pred_model_2 <- svm(class ~ x1 + x2, data = df, kernel = "polynomial", degree = 2, scale = FALSE)
df <- df %>%
  mutate(model_2 = predict(pred_model_2, .))

background <- background %>%
  mutate(model_2 = predict(pred_model_2, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2,
                fill = model_2),
            color = NA,
            alpha = .5) +
  geom_point(data = df,
             aes(x1, x2,
                 color = model_2),
             size = 5) +
  geom_point(data = df,
             aes(x1, x2,
                 color = class),
             size = 3) +
  geom_text(size = 3) +
  scale_color_manual(values = c("#366442", "#DEAC10"),
                     breaks = c("A", "B")) +
  scale_fill_manual(values = c("#366442", "#DEAC10"),
                    breaks = c("A", "B")) +
  coord_equal() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white"))
ggsave("images/full_model_2.png",
       height = 3,
       width = 4)

pred_model_3 <- svm(class ~ x1 + x2, data = df, kernel = "radial", scale = FALSE,
                    gamma = 1/4)
df <- df %>%
  mutate(model_3 = predict(pred_model_3, .))

background <- background %>%
  mutate(model_3 = predict(pred_model_3, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2,
                fill = model_3),
            color = NA,
            alpha = .5) +
  geom_point(data = df,
             aes(x1, x2,
                 color = model_3),
             size = 5) +
  geom_point(data = df,
             aes(x1, x2,
                 color = class),
             size = 3) +
  geom_text(size = 3) +
  scale_color_manual(values = c("#366442", "#DEAC10"),
                     breaks = c("A", "B")) +
  scale_fill_manual(values = c("#366442", "#DEAC10"),
                    breaks = c("A", "B")) +
  coord_equal() +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white"))
ggsave("images/full_model_3.png",
       height = 3,
       width = 4)

df %>%
  summarise(accuracy_1 = mean(class == model_1),
            accuracy_2 = mean(class == model_2),
            accuracy_3 = mean(class == model_3))

## Try folds
ggplot(data = df,
       aes(x1, x2, color = class, label = id)) +
  geom_point(size = 5,
             fill = "white",
             shape = 21) +
  geom_text(size = 3) +
  scale_color_manual(values = c("#366442", "#DEAC10"),
                     breaks = c("A", "B")) +
  coord_equal() +
  facet_wrap(. ~ fold) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(2, "lines"))
ggsave("images/folds.png",
       height = 3,
       width = 3 * 3)

df_assessment <- data.frame(accuracy_1 = numeric(),
                            accuracy_2 = numeric(),
                            accuracy_3 = numeric(),
                            fold = numeric())

for(fold_number in seq_along(1:3)) {
  df <- df %>%
    mutate(current_fold = if_else(fold == fold_number, 'Assessment', 'Analysis'))

  df_train <- df %>% filter(current_fold == 'Analysis')

  ggplot(data = df_train,
         aes(x1, x2, color = class, label = id)) +
    geom_point(size = 5,
               fill = "white",
               shape = 21) +
    geom_text(size = 3) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_analysis.png"),
         height = 3,
         width = 4)

  # Model 1
  pred_model_1 <- svm(class ~ x1 + x2, data = df_train, kernel = "linear", scale = FALSE)
  df <- df %>%
    mutate(model_1 = predict(pred_model_1, .))

  background <- expand_grid(x1 = seq(1, 5, .01),
                            x2 = seq(1, 5, .01))
  background <- background %>%
    mutate(model_1 = predict(pred_model_1, .))

  model_line <- background %>%
    mutate(x1 = x1 + .01,
           x2 = x2 + .01) %>%
    mutate(model_1_next = predict(pred_model_1, .)) %>%
    filter(model_1 != model_1_next) %>%
    filter(x1 <= 5 & x2 <= 5)

  ggplot() +
    geom_tile(data = background,
              aes(x1, x2,
                  fill = model_1),
              color = NA,
              alpha = .5) +
    geom_point(data = model_line,
               aes(x1, x2), color = "#5C5C5C",
               size = .5) +
    geom_point(data = df[df$current_fold == 'Analysis', ],
               aes(x1, x2,
                   color = model_1),
               size = 4.5) +
    geom_point(data = df[df$current_fold == 'Analysis', ],
               aes(x1, x2,
                   color = class),
               size = 2.5) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    scale_fill_manual(values = c("#366442", "#DEAC10"),
                      breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_analysis_model_1.png"),
         height = 3,
         width = 4)

  ggplot() +
    geom_tile(data = background,
              aes(x1, x2,
                  fill = model_1),
              color = NA,
              alpha = .5) +
    geom_point(data = model_line,
               aes(x1, x2), color = "#5C5C5C",
               size = .5) +
    geom_point(data = df[df$current_fold == 'Assessment', ],
               aes(x1, x2,
                   color = model_1),
               size = 4.5) +
    geom_point(data = df[df$current_fold == 'Assessment', ],
               aes(x1, x2,
                   color = class),
               size = 2.5) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    scale_fill_manual(values = c("#366442", "#DEAC10"),
                      breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_assessment_model_1.png"),
         height = 3,
         width = 4)

  # Model 2
  pred_model_2 <- svm(class ~ x1 + x2, data = df_train, kernel = "polynomial", degree = 2, scale = FALSE)
  df <- df %>%
    mutate(model_2 = predict(pred_model_2, .))

  background <- expand_grid(x1 = seq(1, 5, .01),
                            x2 = seq(1, 5, .01))
  background <- background %>%
    mutate(model_2 = predict(pred_model_2, .))

  model_line <- background %>%
    mutate(x1 = x1 + .01,
           x2 = x2 + .01) %>%
    mutate(model_2_next = predict(pred_model_2, .)) %>%
    filter(model_2 != model_2_next) %>%
    filter(x1 <= 5 & x2 <= 5)

  ggplot() +
    geom_tile(data = background,
              aes(x1, x2,
                  fill = model_2),
              color = NA,
              alpha = .5) +
    geom_point(data = model_line,
               aes(x1, x2), color = "#5C5C5C",
               size = .5) +
    geom_point(data = df[df$current_fold == 'Analysis', ],
               aes(x1, x2,
                   color = model_2),
               size = 4.5) +
    geom_point(data = df[df$current_fold == 'Analysis', ],
               aes(x1, x2,
                   color = class),
               size = 2.5) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    scale_fill_manual(values = c("#366442", "#DEAC10"),
                      breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_analysis_model_2.png"),
         height = 3,
         width = 4)

  ggplot() +
    geom_tile(data = background,
              aes(x1, x2,
                  fill = model_2),
              color = NA,
              alpha = .5) +
    geom_point(data = model_line,
               aes(x1, x2), color = "#5C5C5C",
               size = .5) +
    geom_point(data = df[df$current_fold == 'Assessment', ],
               aes(x1, x2,
                   color = model_2),
               size = 4.5) +
    geom_point(data = df[df$current_fold == 'Assessment', ],
               aes(x1, x2,
                   color = class),
               size = 2.5) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    scale_fill_manual(values = c("#366442", "#DEAC10"),
                      breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_assessment_model_2.png"),
         height = 3,
         width = 4)

  # Model 3
  pred_model_3 <- svm(class ~ x1 + x2, data = df_train, kernel = "radial", scale = FALSE,
                      gamma = 1/4)
  df <- df %>%
    mutate(model_3 = predict(pred_model_3, .))

  background <- expand_grid(x1 = seq(1, 5, .01),
                            x2 = seq(1, 5, .01))
  background <- background %>%
    mutate(model_3 = predict(pred_model_3, .))

  model_line <- background %>%
    mutate(x1 = x1 + .01,
           x2 = x2 + .01) %>%
    mutate(model_3_next = predict(pred_model_3, .)) %>%
    filter(model_3 != model_3_next) %>%
    filter(x1 <= 5 & x2 <= 5)

  ggplot() +
    geom_tile(data = background,
              aes(x1, x2,
                  fill = model_3),
              color = NA,
              alpha = .5) +
    geom_point(data = model_line,
               aes(x1, x2), color = "#5C5C5C",
               size = .5) +
    geom_point(data = df[df$current_fold == 'Analysis', ],
               aes(x1, x2,
                   color = model_3),
               size = 4.5) +
    geom_point(data = df[df$current_fold == 'Analysis', ],
               aes(x1, x2,
                   color = class),
               size = 2.5) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    scale_fill_manual(values = c("#366442", "#DEAC10"),
                      breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_analysis_model_3.png"),
         height = 3,
         width = 4)

  ggplot() +
    geom_tile(data = background,
              aes(x1, x2,
                  fill = model_3),
              color = NA,
              alpha = .5) +
    geom_point(data = model_line,
               aes(x1, x2), color = "#5C5C5C",
               size = .5) +
    geom_point(data = df[df$current_fold == 'Assessment', ],
               aes(x1, x2,
                   color = model_3),
               size = 4.5) +
    geom_point(data = df[df$current_fold == 'Assessment', ],
               aes(x1, x2,
                   color = class),
               size = 2.5) +
    scale_color_manual(values = c("#366442", "#DEAC10"),
                       breaks = c("A", "B")) +
    scale_fill_manual(values = c("#366442", "#DEAC10"),
                      breaks = c("A", "B")) +
    coord_equal() +
    scale_x_continuous(limits = c(1, 5)) +
    scale_y_continuous(limits = c(1, 5)) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = .5),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"))
  ggsave(paste0("images/fold_", fold_number, "_assessment_model_3.png"),
         height = 3,
         width = 4)

  df_assessment <- df %>%
    filter(current_fold == "Assessment") %>%
    summarise(accuracy_1 = mean(model_1 == class),
              accuracy_2 = mean(model_2 == class),
              accuracy_3 = mean(model_3 == class)) %>%
    mutate(fold = fold_number) %>%
    rbind(df_assessment)
}

df_assessment <- df_assessment %>%
  summarise(accuracy_1 = mean(accuracy_1),
            accuracy_2 = mean(accuracy_2),
            accuracy_3 = mean(accuracy_3)) %>%
  mutate(fold = 0) %>%
  rbind(df_assessment)

write_csv(df_assessment,
  "Assessment.csv")

df_assessment <- read_csv("Assessment.csv") %>%
  mutate(fold = as.character(fold),
         accuracy_1 = round(accuracy_1, 2),
         accuracy_2 = round(accuracy_2, 2),
         accuracy_3 = round(accuracy_3, 2)) %>%
  mutate(fold = if_else(fold == '0', "Overall", fold)) %>%
  arrange(fold) %>%
  relocate(fold)

accuracy_table <- df_assessment %>%
  gt() %>%
  cols_label(fold = "Fold",
             accuracy_1 = "Model 1",
             accuracy_2 = "Model 2",
             accuracy_3 = "Model 3")
gtsave(accuracy_table, file = "images/accuracy_table.png")
