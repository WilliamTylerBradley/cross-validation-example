# Load libraries
library(tidyverse)
library(e1071)
library(patchwork)

# Set up data
set.seed(1)
df <- expand.grid(x1 = seq(1, 5),
                  x2 = seq(1, 5)) %>%
  mutate(class = as.factor(if_else(sqrt((x1 - 5)^2 + (x2 - 5)^2) < 3.5, 'A', 'B')))

ggplot(data = df,
       aes(x1, x2, color = class)) +
  geom_point() +
  coord_equal()

df <- expand.grid(x1 = seq(1, 5),
                  x2 = seq(1, 5)) %>%
  mutate(class = as.factor(if_else(sqrt((x1 - 5)^2 + (x2 - 5)^2) < 3.5, 'A', 'B'))) %>%
  arrange(sample(1:25)) %>%
  group_by(class) %>%
  slice(1:6) %>%
  mutate(fold = sample(c(1, 1,
                          2, 2,
                          3, 3))) %>%
  ungroup()

ggplot(data = df,
       aes(x1, x2, color = class)) +
  geom_point() +
  coord_equal()

ggplot(data = df,
       aes(x1, x2, color = as.factor(fold))) +
  geom_point() +
  coord_equal()

# svm
pred_model <- svm(class ~ x1 + x2, data = df, kernel = "linear", scale = FALSE)
predict(pred_model, df)

background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df,
             aes(x1, x2, color = class),
             size = 2) +
  coord_equal() +
  theme_minimal()

pred_model <- svm(class ~ x1 + x2, data = df, kernel = "polynomial", degree = 2, scale = FALSE)
predict(pred_model, df)

background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df,
             aes(x1, x2, color = class),
             size = 2) +
  coord_equal() +
  theme_minimal()

pred_model <- svm(class ~ x1 + x2, data = df, kernel = "radial", scale = FALSE, gamma = 1/4)
predict(pred_model, df)

background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df,
             aes(x1, x2, color = class),
             size = 2) +
  coord_equal() +
  theme_minimal()

# analysis/assessment folds
df_train <- df %>% filter(fold != 1)
df_assessment <- df %>% filter(fold == 1)
pred_model_1 <- svm(class ~ x1 + x2, data = df_train, kernel = "linear", scale = FALSE)
background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model_1, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df_train,
             aes(x1, x2, color = class),
             size = 1) +
  geom_point(data = df_assessment,
             aes(x1, x2, color = class),
             shape = 21,
             size = 2) +
  coord_equal() +
  theme_minimal()

pred_model_2 <- svm(class ~ x1 + x2, data = df_train, kernel = "polynomial", degree = 2, scale = FALSE)
background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model_2, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df_train,
             aes(x1, x2, color = class),
             size = 1) +
  geom_point(data = df_assessment,
             aes(x1, x2, color = class),
             shape = 21,
             size = 2) +
  coord_equal() +
  theme_minimal()

pred_model_3 <- svm(class ~ x1 + x2, data = df_train, kernel = "radial", scale = FALSE, gamma = 1/4)
background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model_3, .))

ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df_train,
             aes(x1, x2, color = class),
             size = 1) +
  geom_point(data = df_assessment,
             aes(x1, x2, color = class),
             shape = 21,
             size = 2) +
  coord_equal() +
  theme_minimal()

df_assessment_summary <- df_assessment %>%
  mutate(response_1 = predict(pred_model_1, .),
         response_2 = predict(pred_model_2, .),
         response_3 = predict(pred_model_3, .)) %>%
  mutate(check_1 = if_else(class == response_1, 1, 0),
         check_2 = if_else(class == response_2, 1, 0),
         check_3 = if_else(class == response_3, 1, 0)) %>%
  summarise(accuracy_1 = mean(check_1),
            accuracy_2 = mean(check_2),
            accuracy_3 = mean(check_3))

# loop over all folds
df_assessment_summary <- data.frame(fold = numeric(),
                             accuracy_1 = numeric(),
                             accuracy_2 = numeric(),
                             accuracy_3 = numeric())

for(i in 1:3) {
  df_train <- df %>% filter(fold != i)
  df_assessment <- df %>% filter(fold == i)
  pred_model_1 <- svm(class ~ x1 + x2, data = df_train, kernel = "linear", scale = FALSE)
  pred_model_2 <- svm(class ~ x1 + x2, data = df_train, kernel = "polynomial", degree = 2, scale = FALSE)
  pred_model_3 <- svm(class ~ x1 + x2, data = df_train, kernel = "radial", scale = FALSE, gamma = 1/4)

  df_assessment_summary <- df_assessment %>%
    mutate(response_1 = predict(pred_model_1, .),
           response_2 = predict(pred_model_2, .),
           response_3 = predict(pred_model_3, .)) %>%
    mutate(check_1 = if_else(class == response_1, 1, 0),
           check_2 = if_else(class == response_2, 1, 0),
           check_3 = if_else(class == response_3, 1, 0)) %>%
    summarise(fold = i,
              accuracy_1 = mean(check_1),
              accuracy_2 = mean(check_2),
              accuracy_3 = mean(check_3)) %>%
    rbind(df_assessment_summary)
}

df_assessment_final <- df_assessment_summary %>%
  summarise(accuracy_1 = mean(accuracy_1),
            accuracy_2 = mean(accuracy_2),
            accuracy_3 = mean(accuracy_3))

# now to try out different seeds until we get a set that has good variation
df_assessment_final <- data.frame(seed = numeric(),
                             accuracy_1 = numeric(),
                             accuracy_2 = numeric(),
                             accuracy_3 = numeric())

for(seed in 1:100) {
  set.seed(seed)
  df <- expand.grid(x1 = seq(1, 5),
                    x2 = seq(1, 5)) %>%
    mutate(class = as.factor(if_else(sqrt((x1 - 5)^2 + (x2 - 5)^2) < 3.5, 'A', 'B'))) %>%
    arrange(sample(1:25)) %>%
    group_by(class) %>%
    slice(1:6) %>%
    mutate(fold = sample(c(1, 1,
                            2, 2,
                            3, 3))) %>%
    ungroup()

  df_assessment_summary <- data.frame(fold = numeric(),
                               accuracy_1 = numeric(),
                               accuracy_2 = numeric(),
                               accuracy_3 = numeric())

  for(i in 1:3) {
    df_train <- df %>% filter(fold != i)
    df_assessment <- df %>% filter(fold == i)
    pred_model_1 <- svm(class ~ x1 + x2, data = df_train, kernel = "linear", scale = FALSE)
    pred_model_2 <- svm(class ~ x1 + x2, data = df_train, kernel = "polynomial", degree = 2, scale = FALSE)
    pred_model_3 <- svm(class ~ x1 + x2, data = df_train, kernel = "radial", scale = FALSE, gamma = 1/4)

    df_assessment_summary <- df_assessment %>%
      mutate(response_1 = predict(pred_model_1, .),
             response_2 = predict(pred_model_2, .),
             response_3 = predict(pred_model_3, .)) %>%
      mutate(check_1 = if_else(class == response_1, 1, 0),
             check_2 = if_else(class == response_2, 1, 0),
             check_3 = if_else(class == response_3, 1, 0)) %>%
      summarise(fold = i,
                accuracy_1 = mean(check_1),
                accuracy_2 = mean(check_2),
                accuracy_3 = mean(check_3)) %>%
      rbind(df_assessment_summary)
  }

  df_assessment_final <- df_assessment_summary %>%
    summarise(seed = seed,
              accuracy_1 = mean(accuracy_1),
              accuracy_2 = mean(accuracy_2),
              accuracy_3 = mean(accuracy_3)) %>%
    rbind(df_assessment_final)
}

df_assessment_final <- df_assessment_final %>%
  filter(accuracy_1 < accuracy_2 &
           accuracy_3 < accuracy_2 &
           accuracy_1 < accuracy_3 &
           accuracy_2 != 1)
df_assessment_final # 61, 11, 6

## try one out
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
  ungroup()

# loop through 1, 2, 3 by hand
current_fold <- 1
df_train <- df %>% filter(fold != current_fold)
df_assessment <- df %>% filter(fold == current_fold)
pred_model_1 <- svm(class ~ x1 + x2, data = df_train, kernel = "linear", scale = FALSE)
background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model_1, .))

p_1 <- ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df_train,
             aes(x1, x2, color = class),
             size = 1) +
  geom_point(data = df_assessment,
             aes(x1, x2, color = class),
             shape = 21,
             size = 2) +
  coord_equal() +
  theme_minimal()

pred_model_2 <- svm(class ~ x1 + x2, data = df_train, kernel = "polynomial", degree = 2, scale = FALSE)
background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model_2, .))

p_2 <- ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df_train,
             aes(x1, x2, color = class),
             size = 1) +
  geom_point(data = df_assessment,
             aes(x1, x2, color = class),
             shape = 21,
             size = 2) +
  coord_equal() +
  theme_minimal()

pred_model_3 <- svm(class ~ x1 + x2, data = df_train, kernel = "radial", scale = FALSE,
                    gamma = 1/4)
background <- expand_grid(x1 = seq(1, 5, .05),
                          x2 = seq(1, 5, .05))
background <- background %>%
  mutate(response = predict(pred_model_3, .))

p_3 <- ggplot() +
  geom_tile(data = background,
            aes(x1, x2, fill = response),
            alpha = .5) +
  geom_point(data = df_train,
             aes(x1, x2, color = class),
             size = 1) +
  geom_point(data = df_assessment,
             aes(x1, x2, color = class),
             shape = 21,
             size = 2) +
  coord_equal() +
  theme_minimal()

p_1 + p_2 + p_3 +
  plot_layout(guides = 'collect')

df_assessment %>%
  mutate(response_1 = predict(pred_model_1, .),
         response_2 = predict(pred_model_2, .),
         response_3 = predict(pred_model_3, .)) %>%
  mutate(check_1 = if_else(class == response_1, 1, 0),
         check_2 = if_else(class == response_2, 1, 0),
         check_3 = if_else(class == response_3, 1, 0)) %>%
  summarise(accuracy_1 = mean(check_1),
            accuracy_2 = mean(check_2),
            accuracy_3 = mean(check_3))

# seed = 11, 61
