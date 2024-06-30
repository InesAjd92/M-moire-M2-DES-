library(tidyverse)
library(lubridate)
library(highcharter)
library(sentimentr)

### Traitement des données ###

## Nettoyage des données ##

# Charger les jeux de données nettoyés
twitter_df <- read.csv("/mnt/data/dataset_easy-twitter-search-scraper_2024-06-12_15-28-02-578_cleaned.csv", stringsAsFactors = FALSE)
facebook_df <- read.csv("/mnt/data/dataset_facebook-search-task_2024-06-12_15-16-45-746_cleaned.csv", stringsAsFactors = FALSE)
reddit_df <- read.csv("/mnt/data/dataset_reddit-scraper-task_2024-06-12_15-01-30-852_cleaned.csv", stringsAsFactors = FALSE)

# Nettoyer et renommer les colonnes pour le jeu de données Twitter
twitter_df <- twitter_df %>%
  rename(ID = id, posts = text, username = username, date = timestamp, 
         like = likes, comment = quotes, share = retweets) %>%
  select(ID, posts, username, date, like, comment, share) %>%
  mutate(date = format(as.Date(date, format = "%Y-%m-%d"), "%d/%m/%Y"))

# Nettoyer et renommer les colonnes pour le jeu de données Facebook
facebook_df <- facebook_df %>%
  rename(ID = post_id, posts = message, username = `author/name`, date = create_date, 
         like = reactions_count, comment = comments_count) %>%
  select(ID, posts, username, date, like, comment) %>%
  mutate(date = format(as.Date(date, format = "%Y-%m-%d"), "%d/%m/%Y")) %>%
  mutate(share = NA)

# Nettoyer et renommer les colonnes pour le jeu de données Reddit
reddit_df <- reddit_df %>%
  rename(ID = id, posts = body, username = username, date = createdAt, 
         like = upVotes, comment = numberOfComments) %>%
  select(ID, posts, username, date, like, comment) %>%
  mutate(date = format(as.Date(date, format = "%Y-%m-%d"), "%d/%m/%Y")) %>%
  mutate(share = NA)

# Définir les dates clés
start_date <- as.Date("28/01/2023", format = "%d/%m/%Y")
peak_date <- as.Date("04/02/2023", format = "%d/%m/%Y")
end_date <- as.Date("11/02/2023", format = "%d/%m/%Y")
debunk_date <- as.Date("01/09/2023", format = "%d/%m/%Y")

# Ajouter la colonne de temporalité en fonction de la date pour chaque jeu de données
add_temporality <- function(df) {
  df$date <- as.Date(df$date, format = "%d/%m/%Y")
  df <- df %>%
    mutate(event_phase = case_when(
      date < start_date ~ "Before Event",
      date >= start_date & date < peak_date ~ "Début du fait",
      date >= peak_date & date < end_date ~ "Pic du fait",
      date >= end_date & date < debunk_date ~ "Fin du fait",
      date >= debunk_date ~ "Après débunkage du fait",
      TRUE ~ "Unknown"
    ))
  return(df)
}

twitter_df <- add_temporality(twitter_df)
facebook_df <- add_temporality(facebook_df)
reddit_df <- add_temporality(reddit_df)

## Analyse univariée ##
# Compter le nombre de réactions par temporalité de l'événement pour chaque réseau social
count_reactions_by_temporality <- function(df) {
  df %>%
    group_by(event_phase) %>%
    summarise(
      total_likes = sum(like, na.rm = TRUE),
      total_comments = if("comment" %in% colnames(df)) sum(comment, na.rm = TRUE) else NA,
      total_shares = if("share" %in% colnames(df)) sum(share, na.rm = TRUE) else NA
    )
}

twitter_reactions <- count_reactions_by_temporality(twitter_df) %>%
  mutate(platform = "Twitter")

facebook_reactions <- count_reactions_by_temporality(facebook_df) %>%
  mutate(platform = "Facebook")

reddit_reactions <- count_reactions_by_temporality(reddit_df) %>%
  mutate(platform = "Reddit")

# Combiner les résultats
all_reactions <- bind_rows(twitter_reactions, facebook_reactions, reddit_reactions)

# Restructurer les données pour ggplot2
all_reactions_long <- all_reactions %>%
  pivot_longer(cols = starts_with("total_"), names_to = "reaction_type", values_to = "count") %>%
  mutate(reaction_type = recode(reaction_type, 
                                "total_likes" = "Likes",
                                "total_comments" = "Comments",
                                "total_shares" = "Shares"))

# Créer le graphique en barres groupées avec ggplot2
ggplot(all_reactions_long, aes(x = event_phase, y = count, fill = reaction_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ platform) +
  labs(title = "Reactions by Event Phase and Platform",
       x = "Event Phase",
       y = "Count",
       fill = "Reaction Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# exporter les data pour un plus beau graph sur flourish !!

write.csv(all_reactions_long, "all_reactions_long.csv")

write.csv(all_reactions, "all_reactions.csv")


### Analyse de sentiment ###
# Fonction pour analyser le sentiment
analyze_sentiment <- function(df) {
  df$sentiment <- sentiment_by(df$posts)$ave_sentiment
  df <- df %>%
    mutate(sentiment_category = case_when(
      sentiment < 0 ~ "Négatif",
      sentiment == 0 ~ "Neutre",
      sentiment > 0 ~ "Positif"
    ))
  return(df)
}

twitter_df <- analyze_sentiment(twitter_df)
facebook_df <- analyze_sentiment(facebook_df)
reddit_df <- analyze_sentiment(reddit_df)

# Convertir les ID en caractère pour éviter les problèmes de type lors de la combinaison
twitter_df$ID <- as.character(twitter_df$ID)
facebook_df$ID <- as.character(facebook_df$ID)
reddit_df$ID <- as.character(reddit_df$ID)

# Combiner les résultats
all_sentiments <- bind_rows(
  twitter_df %>% mutate(platform = "Twitter"),
  facebook_df %>% mutate(platform = "Facebook"),
  reddit_df %>% mutate(platform = "Reddit")
)

# Résumer les scores de sentiment par phase et par réseau social
sentiment_summary <- all_sentiments %>%
  group_by(platform, event_phase) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))

# Filtrer les données pour supprimer la phase "Unknown"
sentiment_summary <- all_sentiments %>%
  group_by(platform, event_phase, sentiment_category) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = sentiment_category, values_from = count, values_fill = list(count = 0))


# Créer le graphique en barres amélioré avec ggplot2
ggplot(sentiment_summary, aes(x = event_phase, y = mean_sentiment, fill = platform)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Twitter" = "#1DA1F2", "Facebook" = "#4267B2", "Reddit" = "#FF4500")) +
  labs(title = "Sentiment moyen par phase de l'évenement",
       x = "Phase de l'événement",
       y = "Sentiment moyen",
       fill = "Plateforme") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

write.csv(sentiment_summary, "sentiment_summary.csv")



### traitement pour iramuteq ###

# Charger les bibliothèques nécessaires
library(dplyr)

### Préparer les données pour IRaMuTeQ ###

# Fonction pour formater un dataframe en IRaMuTeQ
format_iramuteq <- function(df, platform_name) {
  df %>%
    mutate(
      metadata = paste0("**** *id_", ID, " *platform_", platform_name, " *phase_", event_phase),
      text_content = posts
    ) %>%
    select(metadata, text_content) %>%
    arrange(metadata) %>%
    mutate(iramuteq_format = paste(metadata, text_content, sep = "\n")) %>%
    select(iramuteq_format)
}

# Formater les données pour Twitter
twitter_iramuteq <- format_iramuteq(twitter_df, "Twitter")

# Formater les données pour Facebook
facebook_iramuteq <- format_iramuteq(facebook_df, "Facebook")

# Formater les données pour Reddit
reddit_iramuteq <- format_iramuteq(reddit_df, "Reddit")

# Combiner les données formatées
all_iramuteq <- bind_rows(
  twitter_iramuteq,
  facebook_iramuteq,
  reddit_iramuteq
)

# Écrire le fichier IRaMuTeQ
write.table(all_iramuteq$iramuteq_format, "corpus_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")



write.table(twitter_iramuteq$iramuteq_format, "corpus_twitter_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

write.table(facebook_iramuteq$iramuteq_format, "corpus_facebook_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")


write.table(reddit_iramuteq$iramuteq_format, "corpus_reddit_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")




# Nombre de posts par phase

library(tidyverse)
library(lubridate)
library(highcharter)
library(sentimentr)

### Traitement des données ###

## Nettoyage des données ##

# Charger les jeux de données nettoyés
twitter_df <- read.csv("/mnt/data/dataset_easy-twitter-search-scraper_2024-06-12_15-28-02-578_cleaned.csv", stringsAsFactors = FALSE)
facebook_df <- read.csv("/mnt/data/dataset_facebook-search-task_2024-06-12_15-16-45-746_cleaned.csv", stringsAsFactors = FALSE)
reddit_df <- read.csv("/mnt/data/dataset_reddit-scraper-task_2024-06-12_15-01-30-852_cleaned.csv", stringsAsFactors = FALSE)

# Nettoyer et renommer les colonnes pour le jeu de données Twitter
twitter_df <- twitter_df %>%
  rename(ID = id, posts = text, username = username, date = timestamp, 
         like = likes, comment = quotes, share = retweets) %>%
  select(ID, posts, username, date, like, comment, share) %>%
  mutate(date = format(as.Date(date, format = "%Y-%m-%d"), "%d/%m/%Y"))

# Nettoyer et renommer les colonnes pour le jeu de données Facebook
facebook_df <- facebook_df %>%
  rename(ID = post_id, posts = message, username = `author/name`, date = create_date, 
         like = reactions_count, comment = comments_count) %>%
  select(ID, posts, username, date, like, comment) %>%
  mutate(date = format(as.Date(date, format = "%Y-%m-%d"), "%d/%m/%Y")) %>%
  mutate(share = NA)

# Nettoyer et renommer les colonnes pour le jeu de données Reddit
reddit_df <- reddit_df %>%
  rename(ID = id, posts = body, username = username, date = createdAt, 
         like = upVotes, comment = numberOfComments) %>%
  select(ID, posts, username, date, like, comment) %>%
  mutate(date = format(as.Date(date, format = "%Y-%m-%d"), "%d/%m/%Y")) %>%
  mutate(share = NA)

# Définir les dates clés
start_date <- as.Date("28/01/2023", format = "%d/%m/%Y")
peak_date <- as.Date("04/02/2023", format = "%d/%m/%Y")
end_date <- as.Date("11/02/2023", format = "%d/%m/%Y")
debunk_date <- as.Date("01/09/2023", format = "%d/%m/%Y")

# Ajouter la colonne de temporalité en fonction de la date pour chaque jeu de données
add_temporality <- function(df) {
  df$date <- as.Date(df$date, format = "%d/%m/%Y")
  df <- df %>%
    mutate(event_phase = case_when(
      date < start_date ~ "Before Event",
      date >= start_date & date < peak_date ~ "Début du fait",
      date >= peak_date & date < end_date ~ "Pic du fait",
      date >= end_date & date < debunk_date ~ "Fin du fait",
      date >= debunk_date ~ "Après débunkage du fait",
      TRUE ~ "Unknown"
    ))
  return(df)
}

twitter_df <- add_temporality(twitter_df)
facebook_df <- add_temporality(facebook_df)
reddit_df <- add_temporality(reddit_df)

## Analyse univariée ##
# Compter le nombre de réactions par temporalité de l'événement pour chaque réseau social
count_reactions_by_temporality <- function(df) {
  df %>%
    group_by(event_phase) %>%
    summarise(
      total_likes = sum(like, na.rm = TRUE),
      total_comments = if("comment" %in% colnames(df)) sum(comment, na.rm = TRUE) else NA,
      total_shares = if("share" %in% colnames(df)) sum(share, na.rm = TRUE) else NA
    )
}

twitter_reactions <- count_reactions_by_temporality(twitter_df) %>%
  mutate(platform = "Twitter")

facebook_reactions <- count_reactions_by_temporality(facebook_df) %>%
  mutate(platform = "Facebook")

reddit_reactions <- count_reactions_by_temporality(reddit_df) %>%
  mutate(platform = "Reddit")

# Combiner les résultats
all_reactions <- bind_rows(twitter_reactions, facebook_reactions, reddit_reactions)

# Restructurer les données pour ggplot2
all_reactions_long <- all_reactions %>%
  pivot_longer(cols = starts_with("total_"), names_to = "reaction_type", values_to = "count") %>%
  mutate(reaction_type = recode(reaction_type, 
                                "total_likes" = "Likes",
                                "total_comments" = "Comments",
                                "total_shares" = "Shares"))

# Créer le graphique en barres groupées avec ggplot2
ggplot(all_reactions_long, aes(x = event_phase, y = count, fill = reaction_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ platform) +
  labs(title = "Reactions by Event Phase and Platform",
       x = "Event Phase",
       y = "Count",
       fill = "Reaction Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# exporter les data pour un plus beau graph sur flourish !!

write.csv(all_reactions_long, "all_reactions_long.csv")

write.csv(all_reactions, "all_reactions.csv")


### Analyse de sentiment ###
# Fonction pour analyser le sentiment
analyze_sentiment <- function(df) {
  df$sentiment <- sentiment_by(df$posts)$ave_sentiment
  df <- df %>%
    mutate(sentiment_category = case_when(
      sentiment < 0 ~ "Négatif",
      sentiment == 0 ~ "Neutre",
      sentiment > 0 ~ "Positif"
    ))
  return(df)
}

twitter_df <- analyze_sentiment(twitter_df)
facebook_df <- analyze_sentiment(facebook_df)
reddit_df <- analyze_sentiment(reddit_df)

# Convertir les ID en caractère pour éviter les problèmes de type lors de la combinaison
twitter_df$ID <- as.character(twitter_df$ID)
facebook_df$ID <- as.character(facebook_df$ID)
reddit_df$ID <- as.character(reddit_df$ID)

# Combiner les résultats
all_sentiments <- bind_rows(
  twitter_df %>% mutate(platform = "Twitter"),
  facebook_df %>% mutate(platform = "Facebook"),
  reddit_df %>% mutate(platform = "Reddit")
)

# Résumer les scores de sentiment par phase et par réseau social
sentiment_summary <- all_sentiments %>%
  group_by(platform, event_phase) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))

# Filtrer les données pour supprimer la phase "Unknown"
sentiment_summary <- all_sentiments %>%
  group_by(platform, event_phase, sentiment_category) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = sentiment_category, values_from = count, values_fill = list(count = 0))


# Créer le graphique en barres amélioré avec ggplot2
ggplot(sentiment_summary, aes(x = event_phase, y = mean_sentiment, fill = platform)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Twitter" = "#1DA1F2", "Facebook" = "#4267B2", "Reddit" = "#FF4500")) +
  labs(title = "Sentiment moyen par phase de l'évenement",
       x = "Phase de l'événement",
       y = "Sentiment moyen",
       fill = "Plateforme") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

write.csv(sentiment_summary, "sentiment_summary.csv")



### traitement pour iramuteq ###

# Charger les bibliothèques nécessaires
library(dplyr)

### Préparer les données pour IRaMuTeQ ###

# Fonction pour formater un dataframe en IRaMuTeQ
format_iramuteq <- function(df, platform_name) {
  df %>%
    mutate(
      metadata = paste0("**** *id_", ID, " *platform_", platform_name, " *phase_", event_phase),
      text_content = posts
    ) %>%
    select(metadata, text_content) %>%
    arrange(metadata) %>%
    mutate(iramuteq_format = paste(metadata, text_content, sep = "\n")) %>%
    select(iramuteq_format)
}

# Formater les données pour Twitter
twitter_iramuteq <- format_iramuteq(twitter_df, "Twitter")

# Formater les données pour Facebook
facebook_iramuteq <- format_iramuteq(facebook_df, "Facebook")

# Formater les données pour Reddit
reddit_iramuteq <- format_iramuteq(reddit_df, "Reddit")

# Combiner les données formatées
all_iramuteq <- bind_rows(
  twitter_iramuteq,
  facebook_iramuteq,
  reddit_iramuteq
)

# Écrire le fichier IRaMuTeQ
write.table(all_iramuteq$iramuteq_format, "corpus_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")



write.table(twitter_iramuteq$iramuteq_format, "corpus_twitter_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

write.table(facebook_iramuteq$iramuteq_format, "corpus_facebook_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")


write.table(reddit_iramuteq$iramuteq_format, "corpus_reddit_iramuteq.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")



### Nombre de post par phase ###*

# Compter le nombre de posts par phase pour Twitter
twitter_phase_count <- twitter_df %>%
  group_by(event_phase) %>%
  summarise(count = n()) %>%
  mutate(platform = "Twitter")

# Compter le nombre de posts par phase pour Facebook
facebook_phase_count <- facebook_df %>%
  group_by(event_phase) %>%
  summarise(count = n()) %>%
  mutate(platform = "Facebook")

# Compter le nombre de posts par phase pour Reddit
reddit_phase_count <- reddit_df %>%
  group_by(event_phase) %>%
  summarise(count = n()) %>%
  mutate(platform = "Reddit")

# Combiner les résultats
phase_counts <- bind_rows(twitter_phase_count, facebook_phase_count, reddit_phase_count)

# Afficher les résultats
print(phase_counts)

# Écrire les résultats dans un fichier CSV
write.csv(phase_counts, "phase_counts.csv", row.names = FALSE)


#Anova test ##



# Préparer les données pour le test du Chi-Deux
chi_square_data <- phase_counts %>%
  select(-platform) %>%
  as.matrix()

# Effectuer le test du Chi-Deux
chi_square_test <- chisq.test(chi_square_data)

# Afficher les résultats
print(chi_square_test)

