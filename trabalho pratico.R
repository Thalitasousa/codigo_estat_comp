# PROJETO SPOTIFY + MONTE CARLO -------------------------------------------
library(readr)
# IMPORTAÇÃO --------------------------------------------------------------
spotify_songs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv", show_col_types = FALSE)
print(head(spotify_songs),width = Inf)

dim(spotify_songs)
table(spotify_songs$playlist_genre)

summary(spotify_songs)










# fase 1 ------------------------------------------------------------------
library(dplyr)

# AMOSTRAGEM ESTRATIFICADA POR GÊNERO ------------------------------------
set.seed(123)
spotify_sample <- spotify_songs %>%
  group_by(playlist_genre) %>%
  sample_n(200) %>%
  ungroup()

dim(spotify_sample)

# LIMPEZA DOS DADOS -------------------------------------------------------
sum(is.na(spotify_sample))
spotify_sample <- spotify_sample[!duplicated(spotify_sample$track_id), ]

# SELEÇÃO E NORMALIZAÇÃO DAS FEATURES ------------------------------------
audio_features <- c("danceability", "energy", "valence", "tempo", 
                    "acousticness", "speechiness", "instrumentalness", 
                    "liveness", "loudness")

spotify_clean <- spotify_sample %>%
  select(track_id, track_name, track_artist, track_popularity, 
         playlist_genre, playlist_subgenre, all_of(audio_features)) %>%
  mutate(
    tempo_norm = scale(tempo)[,1],
    loudness_norm = scale(loudness)[,1]
  ) %>%
  select(-tempo, -loudness)

audio_features_norm <- c("danceability", "energy", "valence", "tempo_norm", 
                         "acousticness", "speechiness", "instrumentalness", 
                         "liveness", "loudness_norm")

# CRIAÇÃO DE PERFIS DE USUÁRIO -------------------------------------------
user_profiles <- spotify_clean %>%
  group_by(playlist_genre, playlist_subgenre) %>%
  summarise(across(all_of(audio_features_norm), mean), .groups = 'drop') %>%
  mutate(user_id = row_number())

dim(spotify_clean)
dim(user_profiles)










# MATRIZ DE SIMILARIDADE --------------------------------------------------
feature_matrix <- as.matrix(spotify_clean[, audio_features_norm])
rownames(feature_matrix) <- spotify_clean$track_id

distance_matrix <- dist(feature_matrix, method = "euclidean")
similarity_matrix <- as.matrix(distance_matrix)

get_similar_songs <- function(track_id, n_similar = 10) {
  if (!track_id %in% rownames(similarity_matrix)) return(NULL)
  
  distances <- similarity_matrix[track_id, ]
  distances <- distances[distances > 0]
  similar_tracks <- names(sort(distances)[1:n_similar])
  
  return(similar_tracks)
}

# VERIFICAÇÃO DA FASE 1 ---------------------------------------------------
dim(feature_matrix)
dim(similarity_matrix)
length(unique(spotify_clean$track_id))
nrow(user_profiles)

# TESTE DA FUNÇÃO DE SIMILARIDADE ----------------------------------------
sample_track <- spotify_clean$track_id[1]
similar_tracks <- get_similar_songs(sample_track, 5)
test_result <- spotify_clean %>% 
  filter(track_id %in% c(sample_track, similar_tracks)) %>%
  select(track_name, track_artist, playlist_genre, danceability, energy, valence)

print(test_result)



# FASE 2: IMPLEMENTAÇÃO MONTE CARLO ---------------------------------------

calculate_diversity <- function(playlist_ids) {
  if (length(playlist_ids) <= 1) return(0)
  
  playlist_features <- feature_matrix[playlist_ids, ]
  feature_vars <- apply(playlist_features, 2, var)
  mean(feature_vars, na.rm = TRUE)
}

calculate_user_preference <- function(playlist_ids, user_profile) {
  if (length(playlist_ids) == 0) return(0)
  
  playlist_features <- feature_matrix[playlist_ids, ]
  playlist_mean <- colMeans(playlist_features)
  
  user_preferences <- as.numeric(user_profile[audio_features_norm])
  distance <- sqrt(sum((playlist_mean - user_preferences)^2))
  
  max_distance <- sqrt(sum((rep(1, length(audio_features_norm)) - rep(0, length(audio_features_norm)))^2))
  similarity_score <- 1 - (distance / max_distance)
  
  return(max(0, similarity_score))
}

reward_function <- function(playlist_ids, user_profile, w_pop = 0.3, w_div = 0.3, w_pref = 0.4) {
  if (length(playlist_ids) == 0) return(0)
  
  playlist_data <- spotify_clean[spotify_clean$track_id %in% playlist_ids, ]
  
  popularity_score <- mean(playlist_data$track_popularity) / 100
  diversity_score <- calculate_diversity(playlist_ids)
  preference_score <- calculate_user_preference(playlist_ids, user_profile)
  
  total_score <- w_pop * popularity_score + w_div * diversity_score + w_pref * preference_score
  return(total_score)
}

generate_random_playlist <- function(n_songs = 10, exclude_ids = NULL) {
  available_songs <- spotify_clean$track_id
  if (!is.null(exclude_ids)) {
    available_songs <- setdiff(available_songs, exclude_ids)
  }
  
  if (length(available_songs) < n_songs) {
    return(available_songs)
  }
  
  sample(available_songs, n_songs, replace = FALSE)
}

monte_carlo_playlist <- function(user_id, n_simulations = 1000, playlist_size = 10) {
  user_profile <- user_profiles[user_profiles$user_id == user_id, ]
  
  best_score <- -Inf
  best_playlist <- NULL
  scores <- numeric(n_simulations)
  
  for (i in 1:n_simulations) {
    playlist <- generate_random_playlist(playlist_size)
    score <- reward_function(playlist, user_profile)
    scores[i] <- score
    
    if (score > best_score) {
      best_score <- score
      best_playlist <- playlist
    }
  }
  
  return(list(
    best_playlist = best_playlist,
    best_score = best_score,
    all_scores = scores,
    user_id = user_id
  ))
}

baseline_recommendation <- function(user_id, playlist_size = 10) {
  user_profile <- user_profiles[user_profiles$user_id == user_id, ]
  user_genre <- user_profile$playlist_genre
  
  genre_songs <- spotify_clean[spotify_clean$playlist_genre == user_genre, ]
  top_songs <- genre_songs[order(genre_songs$track_popularity, decreasing = TRUE), ]
  
  baseline_playlist <- head(top_songs$track_id, playlist_size)
  baseline_score <- reward_function(baseline_playlist, user_profile)
  
  return(list(
    playlist = baseline_playlist,
    score = baseline_score
  ))
}

set.seed(456)
test_user <- 1
mc_result <- monte_carlo_playlist(test_user, n_simulations = 500, playlist_size = 8)
baseline_result <- baseline_recommendation(test_user, playlist_size = 8)

mc_result$best_score
baseline_result$score
mean(mc_result$all_scores)
max(mc_result$all_scores)



# FASE 3: ANÁLISE E MÉTRICAS -----------------------------------------------

library(ggplot2)
library(tidyr)

convergence_analysis <- function(user_id, n_simulations = 2000, playlist_size = 10) {
  user_profile <- user_profiles[user_profiles$user_id == user_id, ]
  
  scores <- numeric(n_simulations)
  best_scores <- numeric(n_simulations)
  current_best <- -Inf
  
  for (i in 1:n_simulations) {
    playlist <- generate_random_playlist(playlist_size)
    score <- reward_function(playlist, user_profile)
    scores[i] <- score
    
    if (score > current_best) {
      current_best <- score
    }
    best_scores[i] <- current_best
  }
  
  return(data.frame(
    iteration = 1:n_simulations,
    score = scores,
    best_score = best_scores
  ))
}

evaluate_all_users <- function(n_simulations = 1000, playlist_size = 10) {
  results <- data.frame()
  
  for (user_id in user_profiles$user_id) {
    mc_result <- monte_carlo_playlist(user_id, n_simulations, playlist_size)
    baseline_result <- baseline_recommendation(user_id, playlist_size)
    
    user_data <- user_profiles[user_profiles$user_id == user_id, ]
    
    results <- rbind(results, data.frame(
      user_id = user_id,
      genre = user_data$playlist_genre,
      subgenre = user_data$playlist_subgenre,
      mc_score = mc_result$best_score,
      baseline_score = baseline_result$score,
      improvement = mc_result$best_score - baseline_result$score,
      improvement_pct = ((mc_result$best_score - baseline_result$score) / baseline_result$score) * 100
    ))
  }
  
  return(results)
}

detailed_metrics <- function(playlist_ids, user_profile) {
  playlist_data <- spotify_clean[spotify_clean$track_id %in% playlist_ids, ]
  
  popularity_score <- mean(playlist_data$track_popularity)
  diversity_score <- calculate_diversity(playlist_ids)
  preference_score <- calculate_user_preference(playlist_ids, user_profile)
  
  feature_diversity <- apply(feature_matrix[playlist_ids, ], 2, var)
  
  return(list(
    popularity = popularity_score,
    diversity = diversity_score,
    user_preference = preference_score,
    n_songs = length(playlist_ids),
    feature_variances = feature_diversity
  ))
}

benchmark_performance <- function() {
  start_time <- Sys.time()
  conv_data <- convergence_analysis(1, 1000, 8)
  convergence_time <- as.numeric(Sys.time() - start_time)
  
  start_time <- Sys.time()
  all_results <- evaluate_all_users(500, 8)
  evaluation_time <- as.numeric(Sys.time() - start_time)
  
  return(list(
    convergence_time = convergence_time,
    evaluation_time = evaluation_time,
    results = all_results
  ))
}

set.seed(789)
convergence_data <- convergence_analysis(1, 1500, 8)
performance_data <- benchmark_performance()
genre_analysis <- performance_data$results %>%
  group_by(genre) %>%
  summarise(
    n_users = n(),
    avg_mc_score = mean(mc_score),
    avg_baseline_score = mean(baseline_score),
    avg_improvement = mean(improvement),
    avg_improvement_pct = mean(improvement_pct),
    .groups = 'drop'
  )

convergence_plot <- ggplot(convergence_data, aes(x = iteration)) +
  geom_line(aes(y = score), alpha = 0.3, color = "gray") +
  geom_line(aes(y = best_score), color = "red", size = 1) +
  labs(x = "Iterações", y = "Score", title = "Convergência Monte Carlo") +
  theme_minimal()

comparison_plot <- ggplot(performance_data$results, aes(x = baseline_score, y = mc_score, color = genre)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Score Baseline", y = "Score Monte Carlo", title = "Monte Carlo vs Baseline") +
  theme_minimal()

print(convergence_plot)
print(comparison_plot)
print(genre_analysis)
summary(performance_data$results$improvement_pct)
performance_data$convergence_time
performance_data$evaluation_time






library(viridis)
library(gridExtra)

improvement_boxplot <- ggplot(performance_data$results, aes(x = genre, y = improvement_pct, fill = genre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  scale_fill_viridis_d() +
  labs(x = "Gênero Musical", y = "Melhoria (%)", 
       title = "Distribuição da Melhoria do Monte Carlo por Gênero") +
  theme_minimal() +
  theme(legend.position = "none")

feature_heatmap_data <- user_profiles %>%
  select(playlist_genre, all_of(audio_features_norm)) %>%
  group_by(playlist_genre) %>%
  summarise(across(all_of(audio_features_norm), mean), .groups = 'drop') %>%
  pivot_longer(cols = all_of(audio_features_norm), names_to = "feature", values_to = "value")

feature_heatmap <- ggplot(feature_heatmap_data, aes(x = feature, y = playlist_genre, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "Features de Áudio", y = "Gênero Musical", 
       title = "Perfil de Features por Gênero", fill = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

diversity_popularity_data <- data.frame()
for (i in 1:nrow(performance_data$results)) {
  user_id <- performance_data$results$user_id[i]
  user_profile <- user_profiles[user_profiles$user_id == user_id, ]
  
  mc_result <- monte_carlo_playlist(user_id, 100, 8)
  baseline_result <- baseline_recommendation(user_id, 8)
  
  mc_metrics <- detailed_metrics(mc_result$best_playlist, user_profile)
  baseline_metrics <- detailed_metrics(baseline_result$playlist, user_profile)
  
  diversity_popularity_data <- rbind(diversity_popularity_data, data.frame(
    genre = performance_data$results$genre[i],
    method = "Monte Carlo",
    diversity = mc_metrics$diversity,
    popularity = mc_metrics$popularity
  ))
  
  diversity_popularity_data <- rbind(diversity_popularity_data, data.frame(
    genre = performance_data$results$genre[i],
    method = "Baseline",
    diversity = baseline_metrics$diversity,
    popularity = baseline_metrics$popularity
  ))
}

diversity_vs_popularity <- ggplot(diversity_popularity_data, aes(x = diversity, y = popularity, 
                                                                 color = method, shape = genre)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Monte Carlo" = "red", "Baseline" = "blue")) +
  labs(x = "Diversidade Musical", y = "Popularidade Média", 
       title = "Trade-off: Diversidade vs Popularidade",
       color = "Método", shape = "Gênero") +
  theme_minimal()

convergence_multiple <- data.frame()
sample_users <- sample(user_profiles$user_id, 6)
for (user in sample_users) {
  conv_data <- convergence_analysis(user, 800, 8)
  conv_data$user_id <- user
  conv_data$genre <- user_profiles[user_profiles$user_id == user, ]$playlist_genre
  convergence_multiple <- rbind(convergence_multiple, conv_data)
}

multi_convergence_plot <- ggplot(convergence_multiple, aes(x = iteration, y = best_score, color = factor(user_id))) +
  geom_line(size = 1) +
  facet_wrap(~ genre, scales = "free_y") +
  scale_color_viridis_d() +
  labs(x = "Iterações", y = "Melhor Score", 
       title = "Convergência por Gênero Musical", color = "Usuário") +
  theme_minimal()

print(improvement_boxplot)
print(feature_heatmap)
print(diversity_vs_popularity)
print(multi_convergence_plot)
