# ==============================================================================
# ÉTUDE PERCEPT'URG - ANALYSE QUANTITATIVO-QUALITATIVE
# Commission Jeunes de la Société Française de Médecine d'Urgence (CJ-SFMU)
# Version optimisée utilisant FEEL (French Expanded Emotion Lexicon)
# ==============================================================================

# ==============================================================================
# 1. CONFIGURATION ET CHARGEMENT DES PACKAGES
# ==============================================================================

# Installation des packages (décommenter si nécessaire)
# install.packages(c("tidyverse", "readr", "psych", "rstatix", "car", 
#                    "tidytext", "tm", "wordcloud", "ggplot2", "textdata",
#                    "text2vec", "stopwords", "SnowballC", "grid", "gridExtra"))

# Chargement des librairies
library(tidyverse)    # Manipulation de données
library(readr)        # Import CSV
library(psych)        # Statistiques descriptives et IC
library(rstatix)      # Tests statistiques
library(car)          # Tests de normalité et homogénéité
library(tidytext)     # Analyse de texte (NLP)
library(tm)           # Traitement de texte
library(wordcloud)    # Nuages de mots
library(ggplot2)      # Visualisation
library(stopwords)    # Mots vides français
library(SnowballC)    # Stemming
library(gridExtra)    # Arrangements graphiques

cat("================================================================================\n")
cat("ÉTUDE PERCEPT'URG - Analyse de la perception de la médecine d'urgence\n")
cat("Commission Jeunes - SFMU\n")
cat("Utilisation du lexique FEEL (French Expanded Emotion Lexicon)\n")
cat("================================================================================\n\n")

# ==============================================================================
# 2. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ==============================================================================

cat("--- 2. CHARGEMENT DES DONNÉES ---\n")

# Vérification de l'existence du fichier
if (!file.exists("reponses_questionnaire.csv")) {
  stop("❌ ERREUR: Le fichier 'reponses_questionnaire.csv' est introuvable.\n",
       "   Veuillez placer le fichier dans le répertoire: ", getwd())
}

# Chargement des données
data_raw <- read_csv("reponses_questionnaire.csv", 
                     locale = locale(encoding = "UTF-8"),
                     show_col_types = FALSE)

cat("✓ Données chargées:", nrow(data_raw), "réponses\n\n")

# ------------------------------------------------------------------------------
# 2.1. Nettoyage et Renommage des Colonnes
# ------------------------------------------------------------------------------

cat("--- 2.1. NETTOYAGE ET PRÉPARATION ---\n")

data <- data_raw %>%
  rename(
    Horodateur = 1,
    Consentement = 2,
    Sexe = 3,
    Tranche_Age = 4,
    Fonction = 5,
    Experience_Urgence = 6,
    Type_Structure = 7,
    Postes_Exerces = 8,
    # Questions qualitatives (texte libre)
    Conditions_Ideales = 9,
    Structure_Ideale = 10,
    Service_Urgence_Ideal = 11,
    SAMU_SAS_Ideal = 12,
    SMUR_Ideal = 13,
    Lieu_Travail_Ideal = 14,
    Heures_Hebdo_Ideales = 15,
    Nuitees_Hebdo_Ideales = 16,
    Medecine_Urgence_Patients = 17,
    Medecine_Urgence_Soins = 18,
    Situation_Urgence_Def = 19,
    Situation_Non_Urgente_Def = 20,
    Situation_Non_Programmee_Def = 21,
    # Échelles de Likert (0-10)
    Perception_Actu_Specialite = 22,
    Perception_Fut_Specialite = 23,
    Perception_Actu_Exercice = 24,
    Perception_Fut_Exercice = 25,
    Commentaire_Libre = 26
  ) %>%
  # Conversion des échelles de Likert en numérique
  mutate(across(starts_with("Perception_"), as.numeric)) %>%
  # Filtre sur le consentement
  filter(Consentement == "Oui")

# Nettoyage des variables catégorielles
data <- data %>%
  mutate(
    Sexe = as.factor(Sexe),
    Tranche_Age = as.factor(Tranche_Age),
    Fonction = as.factor(Fonction),
    Type_Structure = as.factor(Type_Structure)
  )

cat("✓ Données nettoyées:", nrow(data), "réponses consentantes\n")
cat("✓ Professions représentées:", nlevels(data$Fonction), "catégories\n\n")

# ==============================================================================
# 3. ANALYSE QUANTITATIVE - CRITÈRE DE JUGEMENT PRINCIPAL
# ==============================================================================

cat("================================================================================\n")
cat("3. ANALYSE QUANTITATIVE - ÉCHELLES DE LIKERT (0 = médiocre à 10 = excellent)\n")
cat("================================================================================\n\n")

# Variables d'intérêt (protocole)
likert_vars <- c("Perception_Actu_Specialite", 
                 "Perception_Fut_Specialite",
                 "Perception_Actu_Exercice", 
                 "Perception_Fut_Exercice")

# Variables de sous-population (objectifs secondaires)
group_vars <- c("Sexe", "Tranche_Age", "Fonction", "Type_Structure")

# ------------------------------------------------------------------------------
# 3.1. Statistiques Descriptives Globales (Moyenne, ET, IC95%)
# ------------------------------------------------------------------------------

cat("--- 3.1. STATISTIQUES DESCRIPTIVES GLOBALES ---\n")

# Calcul des statistiques avec IC à 95%
stats_globales <- data %>%
  select(all_of(likert_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Score") %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Score)),
    Moyenne = mean(Score, na.rm = TRUE),
    ET = sd(Score, na.rm = TRUE),
    Mediane = median(Score, na.rm = TRUE),
    Q1 = quantile(Score, 0.25, na.rm = TRUE),
    Q3 = quantile(Score, 0.75, na.rm = TRUE),
    Min = min(Score, na.rm = TRUE),
    Max = max(Score, na.rm = TRUE),
    Erreur_Standard = ET / sqrt(N),
    IC95_Inf = Moyenne - qt(0.975, df = N - 1) * Erreur_Standard,
    IC95_Sup = Moyenne + qt(0.975, df = N - 1) * Erreur_Standard
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(stats_globales)
cat("\n")

# Visualisation des perceptions
plot_perceptions <- ggplot(stats_globales, aes(x = Variable, y = Moyenne)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = IC95_Inf, ymax = IC95_Sup), width = 0.2) +
  geom_text(aes(label = round(Moyenne, 1)), vjust = -0.5, size = 4) +
  scale_x_discrete(labels = c("Perception\nActuelle\nSpécialité",
                               "Perception\nFuture\nSpécialité",
                               "Perception\nActuelle\nExercice",
                               "Perception\nFuture\nExercice")) +
  ylim(0, 10) +
  labs(title = "Perceptions de la Médecine d'Urgence (Moyenne ± IC95%)",
       subtitle = "Échelle de Likert: 0 = médiocre, 10 = excellent",
       x = NULL, y = "Score moyen") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

print(plot_perceptions)

# ------------------------------------------------------------------------------
# 3.2. Tests de Normalité et Homogénéité des Variances
# ------------------------------------------------------------------------------

cat("\n--- 3.2. TESTS DE NORMALITÉ (Shapiro-Wilk) ---\n")

# Test de normalité pour chaque variable
normalite_tests <- data %>%
  select(all_of(likert_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  group_by(Variable) %>%
  summarise(
    N = n(),
    Shapiro_W = shapiro.test(Score)$statistic,
    Shapiro_p = shapiro.test(Score)$p.value,
    Distribution = ifelse(Shapiro_p > 0.05, "Normale", "Non-normale")
  ) %>%
  mutate(across(c(Shapiro_W, Shapiro_p), ~round(., 4)))

print(normalite_tests)
cat("\nInterprétation: p > 0.05 = distribution normale\n\n")

# ------------------------------------------------------------------------------
# 3.3. Analyse Inférentielle - Comparaisons de Sous-Populations
# ------------------------------------------------------------------------------

cat("--- 3.3. ANALYSES INFÉRENTIELLES PAR SOUS-POPULATIONS ---\n")
cat("Seuil de significativité: α = 0.05 (5%)\n")
cat("Tests utilisés selon normalité et nombre de groupes:\n")
cat("  - 2 groupes + normalité: t de Student\n")
cat("  - 2 groupes + non-normalité: U de Mann-Whitney\n")
cat("  - >2 groupes + normalité: ANOVA\n")
cat("  - >2 groupes + non-normalité: Kruskal-Wallis\n\n")

# Fonction d'analyse comparative adaptée au protocole
analyse_comparative <- function(data, score_var, group_var) {
  
  # Préparation des données
  df_analyse <- data %>%
    filter(!is.na(!!sym(score_var)) & !is.na(!!sym(group_var))) %>%
    mutate(!!group_var := as.factor(!!sym(group_var)))
  
  n_groups <- nlevels(df_analyse[[group_var]])
  
  if (n_groups < 2) {
    return(NULL)
  }
  
  # Test de normalité par groupe
  normalite_par_groupe <- df_analyse %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      shapiro_p = ifelse(n >= 3 & n <= 5000, 
                         shapiro.test(!!sym(score_var))$p.value, 
                         NA)
    )
  
  est_normal <- all(normalite_par_groupe$shapiro_p > 0.05, na.rm = TRUE)
  
  # Test d'homogénéité des variances (Levene) si normal
  homogeneite <- NA
  if (est_normal && n_groups >= 2) {
    levene_result <- tryCatch(
      leveneTest(as.formula(paste(score_var, "~", group_var)), data = df_analyse),
      error = function(e) NULL
    )
    homogeneite <- if (!is.null(levene_result)) levene_result$`Pr(>F)`[1] else NA
  }
  
  # Choix et exécution du test approprié
  if (n_groups == 2) {
    if (est_normal && (is.na(homogeneite) || homogeneite > 0.05)) {
      # t de Student
      test_result <- t.test(as.formula(paste(score_var, "~", group_var)), 
                           data = df_analyse, var.equal = TRUE)
      resultat <- tibble(
        Score_Var = score_var,
        Group_Var = group_var,
        Test = "t de Student",
        Statistique = test_result$statistic,
        p_value = test_result$p.value,
        Significatif = ifelse(test_result$p.value < 0.05, "Oui", "Non")
      )
    } else {
      # U de Mann-Whitney
      test_result <- wilcox_test(data = df_analyse, 
                                 as.formula(paste(score_var, "~", group_var)))
      resultat <- tibble(
        Score_Var = score_var,
        Group_Var = group_var,
        Test = "Mann-Whitney U",
        Statistique = test_result$statistic,
        p_value = test_result$p,
        Significatif = ifelse(test_result$p < 0.05, "Oui", "Non")
      )
    }
  } else {
    if (est_normal && (is.na(homogeneite) || homogeneite > 0.05)) {
      # ANOVA
      anova_result <- aov(as.formula(paste(score_var, "~", group_var)), 
                         data = df_analyse)
      anova_summary <- summary(anova_result)
      resultat <- tibble(
        Score_Var = score_var,
        Group_Var = group_var,
        Test = "ANOVA",
        Statistique = anova_summary[[1]]$`F value`[1],
        p_value = anova_summary[[1]]$`Pr(>F)`[1],
        Significatif = ifelse(anova_summary[[1]]$`Pr(>F)`[1] < 0.05, "Oui", "Non")
      )
    } else {
      # Kruskal-Wallis
      test_result <- kruskal_test(data = df_analyse, 
                                  as.formula(paste(score_var, "~", group_var)))
      resultat <- tibble(
        Score_Var = score_var,
        Group_Var = group_var,
        Test = "Kruskal-Wallis",
        Statistique = test_result$statistic,
        p_value = test_result$p,
        Significatif = ifelse(test_result$p < 0.05, "Oui", "Non")
      )
    }
  }
  
  return(resultat)
}

# Exécution de toutes les comparaisons
resultats_comparaisons <- list()
for (score in likert_vars) {
  for (group in group_vars) {
    cat(paste("  Analyse:", score, "×", group, "\n"))
    result <- analyse_comparative(data, score, group)
    if (!is.null(result)) {
      resultats_comparaisons <- append(resultats_comparaisons, list(result))
    }
  }
}

# Agrégation des résultats
df_comparaisons <- bind_rows(resultats_comparaisons) %>%
  mutate(p_value = round(p_value, 4))

cat("\n✓ Comparaisons réalisées:", nrow(df_comparaisons), "\n\n")

# Résultats significatifs (p < 0.05)
resultats_significatifs <- df_comparaisons %>%
  filter(Significatif == "Oui") %>%
  arrange(p_value)

cat("RÉSULTATS SIGNIFICATIFS (p < 0.05):\n")
if (nrow(resultats_significatifs) > 0) {
  print(resultats_significatifs)
} else {
  cat("  Aucune différence significative détectée\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# 3.4. Statistiques Descriptives par Sous-Populations
# ------------------------------------------------------------------------------

cat("--- 3.4. STATISTIQUES PAR SOUS-POPULATIONS ---\n\n")

# Fonction pour calculer les stats par groupe
stats_par_groupe <- function(data, score_var, group_var) {
  data %>%
    filter(!is.na(!!sym(score_var)) & !is.na(!!sym(group_var))) %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      Variable = score_var,
      N = n(),
      Moyenne = mean(!!sym(score_var), na.rm = TRUE),
      ET = sd(!!sym(score_var), na.rm = TRUE),
      Mediane = median(!!sym(score_var), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(Groupe = 1) %>%
    mutate(across(c(Moyenne, ET, Mediane), ~round(., 2)))
}

# Génération des tableaux par groupe
for (group in group_vars) {
  cat(paste0("Statistiques par ", group, ":\n"))
  for (score in likert_vars) {
    stats <- stats_par_groupe(data, score, group)
    if (nrow(stats) > 0) {
      print(stats)
      cat("\n")
    }
  }
}

# ==============================================================================
# 4. ANALYSE QUALITATIVE - PRÉPARATION NLP
# ==============================================================================

cat("================================================================================\n")
cat("4. ANALYSE QUALITATIVE - NATURAL LANGUAGE PROCESSING (NLP)\n")
cat("Utilisation du lexique FEEL (French Expanded Emotion Lexicon)\n")
cat("================================================================================\n\n")

# Colonnes de texte libre
text_cols <- c("Conditions_Ideales", "Structure_Ideale", "Service_Urgence_Ideal",
               "SAMU_SAS_Ideal", "SMUR_Ideal", "Lieu_Travail_Ideal",
               "Heures_Hebdo_Ideales", "Nuitees_Hebdo_Ideales", 
               "Medecine_Urgence_Patients", "Medecine_Urgence_Soins", 
               "Situation_Urgence_Def", "Situation_Non_Urgente_Def",
               "Situation_Non_Programmee_Def", "Commentaire_Libre")

# ------------------------------------------------------------------------------
# 4.1. Création du Corpus Textuel
# ------------------------------------------------------------------------------

cat("--- 4.1. CRÉATION DU CORPUS TEXTUEL ---\n")

corpus_textuel <- data %>%
  select(all_of(c("Sexe", "Fonction", "Type_Structure", text_cols))) %>%
  rowid_to_column(var = "document_id") %>%
  pivot_longer(cols = all_of(text_cols),
               names_to = "question",
               values_to = "texte") %>%
  filter(!is.na(texte) & texte != "" & nchar(texte) > 5) %>%
  mutate(texte = as.character(texte))

cat("✓ Corpus créé:", nrow(corpus_textuel), "réponses textuelles\n")
cat("✓ Nombre de répondants:", n_distinct(corpus_textuel$document_id), "\n\n")

# ------------------------------------------------------------------------------
# 4.2. Tokenisation et Nettoyage
# ------------------------------------------------------------------------------

cat("--- 4.2. TOKENISATION ET NETTOYAGE ---\n")

# Tokenisation (extraction des mots)
tokens <- corpus_textuel %>%
  unnest_tokens(word, texte, token = "words")

cat("✓ Tokens extraits:", nrow(tokens), "mots bruts\n")

# Chargement des mots vides en français
stopwords_fr <- stopwords("fr", source = "snowball")
custom_stopwords <- c(stopwords_fr, "c", "d", "l", "m", "n", "s", "t", 
                      "qu", "j", "être", "avoir", "faire", "etc")

# Nettoyage: suppression des mots vides et tokens non alphabétiques
tokens_clean <- tokens %>%
  filter(str_detect(word, "^[a-zàâäçéèêëïîôùûüÿæœ]+$")) %>%
  filter(!word %in% custom_stopwords) %>%
  filter(nchar(word) >= 3)  # Mots d'au moins 3 caractères

cat("✓ Tokens nettoyés:", nrow(tokens_clean), "mots\n")
cat("✓ Vocabulaire unique:", n_distinct(tokens_clean$word), "mots distincts\n\n")

# ==============================================================================
# 5. CHARGEMENT ET PRÉPARATION DU LEXIQUE FEEL
# ==============================================================================

cat("================================================================================\n")
cat("5. CHARGEMENT DU LEXIQUE FEEL (French Expanded Emotion Lexicon)\n")
cat("================================================================================\n\n")

cat("⚠️  INSTRUCTIONS CRITIQUES:\n")
cat("   Téléchargez FEEL.csv depuis: http://advanse.lirmm.fr/feel.php\n")
cat("   Placez le fichier dans le répertoire de travail R\n\n")

# Vérification de l'existence de FEEL.csv
if (!file.exists("FEEL.csv")) {
  stop("❌ ERREUR: Le fichier 'FEEL.csv' est introuvable.\n",
       "   Téléchargez-le depuis: http://advanse.lirmm.fr/feel.php\n",
       "   Placez-le dans le répertoire: ", getwd())
}

# Chargement de FEEL
feel_raw <- read_csv("~\data\FEEL.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("✓ Lexique FEEL chargé:", nrow(feel_raw), "mots français\n")

# Vérification de la structure de FEEL
expected_cols <- c("word", "polarity", "anger", "anticipation", "disgust", 
                   "fear", "joy", "sadness", "surprise", "trust")
missing_cols <- setdiff(expected_cols, names(feel_raw))

if (length(missing_cols) > 0) {
  stop("❌ ERREUR: Colonnes manquantes dans FEEL.csv: ", paste(missing_cols, collapse = ", "), "\n",
       "   Structure attendue: word, polarity, anger, anticipation, disgust, fear, joy, sadness, surprise, trust")
}

# ------------------------------------------------------------------------------
# 5.1. Préparation de FEEL pour l'Analyse BING (Polarité: Positif/Négatif)
# ------------------------------------------------------------------------------

cat("\n--- 5.1. PRÉPARATION POUR ANALYSE BING (Polarité) ---\n")

french_bing_lexicon <- feel_raw %>%
  select(word, polarity) %>%
  filter(!is.na(polarity) & polarity != "") %>%
  rename(sentiment = polarity) %>%
  mutate(sentiment = tolower(sentiment)) %>%
  filter(sentiment %in% c("positive", "negative"))

cat("✓ Lexique BING (polarité) créé:\n")
cat("  - Mots positifs:", sum(french_bing_lexicon$sentiment == "positive"), "\n")
cat("  - Mots négatifs:", sum(french_bing_lexicon$sentiment == "negative"), "\n\n")

# ------------------------------------------------------------------------------
# 5.2. Préparation de FEEL pour l'Analyse NRC (8 Émotions + 2 Polarités)
# ------------------------------------------------------------------------------

cat("--- 5.2. PRÉPARATION POUR ANALYSE NRC (Émotions) ---\n")

# Transformation au format long (une ligne par mot-émotion)
french_nrc_lexicon <- feel_raw %>%
  pivot_longer(
    cols = c(polarity, anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
    names_to = "sentiment",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  filter(
    (sentiment == "polarity" & value %in% c("positive", "negative")) |
    (sentiment != "polarity" & value == 1)
  ) %>%
  mutate(
    sentiment = case_when(
      sentiment == "polarity" & value == "positive" ~ "positive",
      sentiment == "polarity" & value == "negative" ~ "negative",
      TRUE ~ sentiment
    )
  ) %>%
  select(word, sentiment) %>%
  distinct()

cat("✓ Lexique NRC (émotions) créé:\n")
emotions_count <- french_nrc_lexicon %>%
  count(sentiment, name = "n_mots") %>%
  arrange(desc(n_mots))
print(emotions_count)
cat("\n")

# ------------------------------------------------------------------------------
# 5.3. Préparation de FEEL pour l'Analyse AFINN (Scores Numériques)
# ------------------------------------------------------------------------------

cat("--- 5.3. PRÉPARATION POUR ANALYSE AFINN (Scores) ---\n")
cat("⚠️  FEEL ne contient pas de scores numériques natifs.\n")
cat("   Création de scores basés sur la polarité et les émotions:\n")
cat("   - Mots uniquement positifs: +2\n")
cat("   - Mots uniquement négatifs: -2\n")
cat("   - Mots avec émotions positives (joy, trust, anticipation): +1 par émotion\n")
cat("   - Mots avec émotions négatives (anger, fear, disgust, sadness): -1 par émotion\n\n")

# Calcul des scores basés sur FEEL
french_afinn_lexicon <- feel_raw %>%
  mutate(
    # Score de base selon la polarité
    score_polarity = case_when(
      polarity == "positive" ~ 2,
      polarity == "negative" ~ -2,
      TRUE ~ 0
    ),
    # Score basé sur les émotions positives
    score_positive_emotions = (joy + trust + anticipation),
    # Score basé sur les émotions négatives
    score_negative_emotions = -(anger + fear + disgust + sadness),
    # Score total
    value = score_polarity + score_positive_emotions + score_negative_emotions
  ) %>%
  select(word, value) %>%
  filter(value != 0)  # Garder seulement les mots avec un score non-nul

cat("✓ Lexique AFINN (scores) créé:\n")
cat("  - Mots avec score:", nrow(french_afinn_lexicon), "\n")
cat("  - Score moyen:", round(mean(french_afinn_lexicon$value), 2), "\n")
cat("  - Score min:", min(french_afinn_lexicon$value), "\n")
cat("  - Score max:", max(french_afinn_lexicon$value), "\n")

# Distribution des scores
score_distribution <- french_afinn_lexicon %>%
  mutate(score_category = case_when(
    value <= -3 ~ "Très négatif (≤-3)",
    value == -2 ~ "Négatif (-2)",
    value == -1 ~ "Légèrement négatif (-1)",
    value == 1 ~ "Légèrement positif (+1)",
    value == 2 ~ "Positif (+2)",
    value >= 3 ~ "Très positif (≥+3)",
    TRUE ~ "Neutre (0)"
  )) %>%
  count(score_category) %>%
  arrange(desc(n))

cat("\nDistribution des scores:\n")
print(score_distribution)
cat("\n")

# ==============================================================================
# 6. ANALYSE DE SENTIMENT - TYPE BING (Polarité Positive/Négative)
# ==============================================================================

cat("================================================================================\n")
cat("6. ANALYSE DE SENTIMENT - TYPE BING (Positif/Négatif)\n")
cat("================================================================================\n\n")

cat("--- 6.1. ANALYSE BING (Termes Positifs et Négatifs) ---\n")

sentiment_bing <- tokens_clean %>%
  inner_join(french_bing_lexicon, by = "word") %>%
  count(word, sentiment, sort = TRUE)

cat("\nTop 10 des mots POSITIFS les plus fréquents:\n")
sentiment_bing %>%
  filter(sentiment == "positive") %>%
  head(10) %>%
  print()

cat("\nTop 10 des mots NÉGATIFS les plus fréquents:\n")
sentiment_bing %>%
  filter(sentiment == "negative") %>%
  head(10) %>%
  print()

# Visualisation
if (nrow(sentiment_bing) > 0) {
  plot_bing <- sentiment_bing %>%
    group_by(sentiment) %>%
    top_n(15, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(title = "Mots les plus fréquents par sentiment (Lexique BING/FEEL)",
         subtitle = "Basé sur la polarité du lexique FEEL",
         x = NULL, y = "Fréquence") +
    coord_flip() +
    scale_fill_manual(values = c("negative" = "red3", "positive" = "green4")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(plot_bing)
} else {
  cat("⚠️  Aucun mot trouvé dans le lexique BING\n")
}

# Statistiques globales de sentiment
sentiment_bing_stats <- tokens_clean %>%
  inner_join(french_bing_lexicon, by = "word") %>%
  count(sentiment) %>%
  mutate(proportion = n / sum(n) * 100)

cat("\n\nRépartition globale des sentiments:\n")
print(sentiment_bing_stats)
cat("\n")

# ==============================================================================
# 7. ANALYSE DE SENTIMENT - TYPE NRC (10 Émotions)
# ==============================================================================

cat("================================================================================\n")
cat("7. ANALYSE DE SENTIMENT - TYPE NRC (10 Émotions)\n")
cat("================================================================================\n\n")

cat("--- 7.1. ANALYSE NRC (Catégorisation par Émotions) ---\n")

sentiment_nrc <- tokens_clean %>%
  inner_join(french_nrc_lexicon, by = "word") %>%
  count(sentiment, sort = TRUE) %>%
  mutate(proportion = n / sum(n) * 100)

cat("\nRépartition des émotions:\n")
print(sentiment_nrc %>% mutate(proportion = round(proportion, 2)))

# Visualisation
if (nrow(sentiment_nrc) > 0) {
  plot_nrc <- ggplot(sentiment_nrc, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(round(proportion, 1), "%")), 
              hjust = -0.1, size = 3) +
    coord_flip() +
    labs(title = "Distribution des Émotions (Lexique NRC/FEEL)",
         subtitle = "10 catégories émotionnelles basées sur FEEL",
         x = NULL, y = "Nombre d'occurrences") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold")) +
    scale_fill_brewer(palette = "Set3")
  
  print(plot_nrc)
} else {
  cat("⚠️  Aucune émotion trouvée dans le lexique NRC\n")
}

# Top mots par émotion
cat("\n--- 7.2. TOP MOTS PAR ÉMOTION ---\n\n")

top_mots_emotions <- tokens_clean %>%
  inner_join(french_nrc_lexicon, by = "word") %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  arrange(sentiment, desc(n))

for (emotion in unique(top_mots_emotions$sentiment)) {
  cat(paste0("Émotion: ", toupper(emotion), "\n"))
  mots <- top_mots_emotions %>%
    filter(sentiment == emotion) %>%
    select(word, n)
  print(mots)
  cat("\n")
}

# ==============================================================================
# 8. ANALYSE DE SENTIMENT - TYPE AFINN (Score de Sentiment par Question)
# ==============================================================================

cat("================================================================================\n")
cat("8. ANALYSE DE SENTIMENT - TYPE AFINN (Scores Numériques)\n")
cat("================================================================================\n\n")

cat("--- 8.1. ANALYSE AFINN (Score de Sentiment) ---\n")

sentiment_afinn_question <- tokens_clean %>%
  inner_join(french_afinn_lexicon, by = "word") %>%
  group_by(document_id, question) %>%
  summarise(
    score_sentiment = sum(value),
    n_mots = n(),
    .groups = 'drop'
  ) %>%
  group_by(question) %>%
  summarise(
    N_reponses = n(),
    Score_Moyen = mean(score_sentiment),
    ET_Score = sd(score_sentiment),
    Score_Min = min(score_sentiment),
    Score_Max = max(score_sentiment),
    .groups = 'drop'
  ) %>%
  mutate(across(c(Score_Moyen, ET_Score), ~round(., 2))) %>%
  arrange(desc(Score_Moyen))

cat("\nScore de sentiment moyen par question (échelle AFINN/FEEL):\n")
print(sentiment_afinn_question)

# Visualisation
if (nrow(sentiment_afinn_question) > 0) {
  plot_afinn <- ggplot(sentiment_afinn_question, 
                       aes(x = reorder(question, Score_Moyen), y = Score_Moyen)) +
    geom_col(aes(fill = Score_Moyen > 0)) +
    geom_errorbar(aes(ymin = Score_Moyen - ET_Score, 
                      ymax = Score_Moyen + ET_Score), width = 0.2) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "green4", "FALSE" = "red3")) +
    labs(title = "Score de Sentiment par Question (Lexique AFINN/FEEL)",
         subtitle = "Valeurs positives = sentiment positif, négatives = sentiment négatif",
         x = NULL, y = "Score moyen (± ET)") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"),
          axis.text.y = element_text(size = 8))
  
  print(plot_afinn)
} else {
  cat("⚠️  Aucun score trouvé dans le lexique AFINN\n")
}

# Score global
sentiment_afinn_global <- tokens_clean %>%
  inner_join(french_afinn_lexicon, by = "word") %>%
  summarise(
    N_mots = n(),
    Score_Total = sum(value),
    Score_Moyen = mean(value),
    Score_Median = median(value)
  )

cat("\n\nScore de sentiment GLOBAL (tous les textes):\n")
print(sentiment_afinn_global)
cat("\n")

# ==============================================================================
# 9. ANALYSE LEXICALE - FRÉQUENCE ET VISUALISATION
# ==============================================================================

cat("================================================================================\n")
cat("9. ANALYSE LEXICALE - FRÉQUENCE DES MOTS\n")
cat("================================================================================\n\n")

# ------------------------------------------------------------------------------
# 9.1. Fréquence Globale des Mots
# ------------------------------------------------------------------------------

cat("--- 9.1. MOTS LES PLUS FRÉQUENTS ---\n")

frequence_mots <- tokens_clean %>%
  count(word, sort = TRUE) %>%
  mutate(proportion = n / sum(n) * 100)

cat("\nTop 30 des mots les plus fréquents:\n")
print(head(frequence_mots, 30))

# Visualisation
plot_freq <- frequence_mots %>%
  head(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "20 Mots les Plus Fréquents",
       subtitle = "Après suppression des mots vides",
       x = NULL, y = "Fréquence") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot_freq)

# ------------------------------------------------------------------------------
# 9.2. Nuage de Mots
# ------------------------------------------------------------------------------

cat("\n--- 9.2. NUAGE DE MOTS ---\n")

if (nrow(frequence_mots) > 50) {
  set.seed(1234)
  wordcloud(words = frequence_mots$word, 
            freq = frequence_mots$n, 
            min.freq = 3,
            max.words = 150, 
            random.order = FALSE, 
            rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
  cat("✓ Nuage de mots généré (150 mots max, fréquence min = 3)\n\n")
} else {
  cat("⚠️  Vocabulaire insuffisant pour générer un nuage de mots\n\n")
}

# ------------------------------------------------------------------------------
# 9.3. Fréquence par Question
# ------------------------------------------------------------------------------

cat("--- 9.3. MOTS CLÉS PAR QUESTION ---\n\n")

frequence_par_question <- tokens_clean %>%
  count(question, word, sort = TRUE) %>%
  group_by(question) %>%
  top_n(10, n) %>%
  ungroup()

for (q in unique(frequence_par_question$question)) {
  cat(paste0("Question: ", q, "\n"))
  freq_q <- frequence_par_question %>%
    filter(question == q) %>%
    select(word, n) %>%
    arrange(desc(n))
  print(freq_q)
  cat("\n")
}

# ==============================================================================
# 10. VECTORISATION ET GESTION DES NÉGATIONS
# ==============================================================================

cat("================================================================================\n")
cat("10. VECTORISATION DE PARAGRAPHE ET GESTION DES NÉGATIONS\n")
cat("================================================================================\n\n")

cat("⚠️  SECTION AVANCÉE - Nécessite des packages et modèles spécialisés\n")
cat("   Le protocole mentionne:\n")
cat("   - Vectorisation de paragraphe (Word2Vec, Doc2Vec)\n")
cat("   - Gestion des formules négatives et conservation du contexte\n")
cat("   - Utilisation de modèles de langage (BERT/CamemBERT)\n\n")

cat("📚 Packages recommandés:\n")
cat("   - text2vec: pour Word2Vec/GloVe\n")
cat("   - reticulate + transformers (Python): pour BERT/CamemBERT\n")
cat("   - quanteda: pour analyse avancée de texte\n\n")

# ------------------------------------------------------------------------------
# 10.1. Détection des Négations (Approche Simple)
# ------------------------------------------------------------------------------

cat("--- 10.1. DÉTECTION DES NÉGATIONS (Approche simplifiée) ---\n")

# Mots de négation en français
negation_words <- c("ne", "pas", "non", "jamais", "aucun", "aucune", 
                    "rien", "sans", "ni", "guère", "point")

# Bigrammes avec négations
bigrams <- corpus_textuel %>%
  unnest_tokens(bigram, texte, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% negation_words)

negations_frequentes <- bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  head(20)

cat("\nBigrammes avec négation les plus fréquents:\n")
print(negations_frequentes)

cat("\n⚠️  Pour une analyse complète avec gestion du contexte,\n")
cat("   implémentez Word2Vec ou utilisez un modèle de langage pré-entraîné\n\n")

# ==============================================================================
# 11. ANALYSE THÉMATIQUE COMPLÉMENTAIRE
# ==============================================================================

cat("================================================================================\n")
cat("11. ANALYSE THÉMATIQUE - TOPIC MODELING (LDA)\n")
cat("================================================================================\n\n")

cat("--- 11.1. MODÉLISATION DE SUJETS (Latent Dirichlet Allocation) ---\n\n")

# Installation et chargement du package topicmodels
if (!require("topicmodels", quietly = TRUE)) {
  cat("Installation du package 'topicmodels'...\n")
  install.packages("topicmodels")
  library(topicmodels)
}

# Création de la matrice Document-Terme (DTM)
dtm <- tokens_clean %>%
  count(document_id, word) %>%
  cast_dtm(document_id, word, n)

cat("✓ Matrice Document-Terme créée:\n")
cat("  - Documents:", dtm$nrow, "\n")
cat("  - Termes:", dtm$ncol, "\n")
cat("  - Sparsité:", round(100 * (1 - dtm$v / (dtm$nrow * dtm$ncol)), 2), "%\n\n")

# Modèle LDA avec K sujets (à ajuster selon les données)
K_topics <- 5  # Nombre de sujets à identifier

cat(paste0("Entraînement du modèle LDA avec ", K_topics, " sujets...\n"))
cat("(Cette étape peut prendre quelques minutes)\n\n")

lda_model <- LDA(dtm, k = K_topics, control = list(seed = 1234))

# Extraction des termes principaux par sujet
topics_terms <- terms(lda_model, 10)

cat("SUJETS IDENTIFIÉS (Top 10 mots par sujet):\n\n")
for (i in 1:K_topics) {
  cat(paste0("Sujet ", i, ": ", paste(topics_terms[, i], collapse = ", "), "\n"))
}

# Distribution des sujets dans les documents
topics_docs <- topics(lda_model)
topics_distribution <- as.data.frame(table(topics_docs)) %>%
  rename(Sujet = topics_docs, Nombre_Documents = Freq) %>%
  mutate(Proportion = round(Nombre_Documents / sum(Nombre_Documents) * 100, 1))

cat("\nDistribution des documents par sujet:\n")
print(topics_distribution)

cat("\n⚠️  Le protocole mentionne aussi l'utilisation de logiciels spécialisés\n")
cat("   comme NVivo ou MAXQDA pour une analyse thématique approfondie\n\n")

# ==============================================================================
# 12. TESTS DE ROBUSTESSE
# ==============================================================================

cat("================================================================================\n")
cat("12. TESTS DE ROBUSTESSE\n")
cat("================================================================================\n\n")

cat("Conformément au protocole, des tests de robustesse doivent être réalisés:\n")
cat("- Variation des seuils dans les lexiques de sentiment\n")
cat("- Variation des pondérations\n")
cat("- Évaluation de la stabilité des résultats\n\n")

# ------------------------------------------------------------------------------
# 12.1. Sensibilité du Nombre de Sujets (LDA)
# ------------------------------------------------------------------------------

cat("--- 12.1. SENSIBILITÉ AU NOMBRE DE SUJETS (LDA) ---\n\n")

# Test avec différents nombres de sujets
k_values <- c(3, 5, 7, 10)
perplexity_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  lda_temp <- LDA(dtm, k = k, control = list(seed = 1234))
  perplexity_scores[i] <- perplexity(lda_temp)
  cat(paste0("K = ", k, " | Perplexité = ", round(perplexity_scores[i], 2), "\n"))
}

cat("\nInterprétation: Une perplexité plus faible indique un meilleur ajustement\n\n")

# ------------------------------------------------------------------------------
# 12.2. Analyse de Sensibilité des Sentiments
# ------------------------------------------------------------------------------

cat("--- 12.2. ANALYSE DE SENSIBILITÉ (Échelle AFINN/FEEL) ---\n\n")

# Variation du seuil minimum de fréquence
seuils_freq <- c(1, 3, 5, 10)

for (seuil in seuils_freq) {
  tokens_filtered <- tokens_clean %>%
    group_by(word) %>%
    filter(n() >= seuil) %>%
    ungroup()
  
  sentiment_test <- tokens_filtered %>%
    inner_join(french_afinn_lexicon, by = "word") %>%
    summarise(Score_Moyen = mean(value))
  
  cat(paste0("Seuil fréquence >= ", seuil, " | Score moyen = ", 
             round(sentiment_test$Score_Moyen, 3), "\n"))
}

cat("\n")

# ==============================================================================
# 13. GÉNÉRATION DES RAPPORTS ET EXPORTS
# ==============================================================================

cat("================================================================================\n")
cat("13. GÉNÉRATION DES RAPPORTS\n")
cat("================================================================================\n\n")

# ------------------------------------------------------------------------------
# 13.1. Export des Résultats Quantitatifs
# ------------------------------------------------------------------------------

cat("--- 13.1. EXPORT DES RÉSULTATS QUANTITATIFS ---\n")

# Statistiques globales
write_csv(stats_globales, "resultats_stats_globales.csv")
cat("✓ Fichier exporté: resultats_stats_globales.csv\n")

# Comparaisons inférentielles
write_csv(df_comparaisons, "resultats_comparaisons.csv")
cat("✓ Fichier exporté: resultats_comparaisons.csv\n")

# Résultats significatifs
write_csv(resultats_significatifs, "resultats_significatifs.csv")
cat("✓ Fichier exporté: resultats_significatifs.csv\n\n")

# ------------------------------------------------------------------------------
# 13.2. Export des Résultats Qualitatifs
# ------------------------------------------------------------------------------

cat("--- 13.2. EXPORT DES RÉSULTATS QUALITATIFS ---\n")

# Sentiments BING
write_csv(sentiment_bing, "resultats_sentiment_bing.csv")
cat("✓ Fichier exporté: resultats_sentiment_bing.csv\n")

# Sentiments NRC
write_csv(sentiment_nrc, "resultats_sentiment_nrc.csv")
cat("✓ Fichier exporté: resultats_sentiment_nrc.csv\n")

# Sentiments AFINN par question
write_csv(sentiment_afinn_question, "resultats_sentiment_afinn.csv")
cat("✓ Fichier exporté: resultats_sentiment_afinn.csv\n")

# Fréquence des mots
write_csv(frequence_mots, "resultats_frequence_mots.csv")
cat("✓ Fichier exporté: resultats_frequence_mots.csv\n")

# Sujets LDA
write.csv(topics_terms, "resultats_topics_lda.csv", row.names = TRUE)
cat("✓ Fichier exporté: resultats_topics_lda.csv\n\n")

# ------------------------------------------------------------------------------
# 13.3. Export des Visualisations
# ------------------------------------------------------------------------------

cat("--- 13.3. EXPORT DES VISUALISATIONS ---\n")

# Sauvegarde des graphiques en haute résolution
ggsave("plot_perceptions.png", plot_perceptions, width = 10, height = 6, dpi = 300)
cat("✓ Graphique exporté: plot_perceptions.png\n")

if (exists("plot_bing") && nrow(sentiment_bing) > 0) {
  ggsave("plot_sentiment_bing.png", plot_bing, width = 12, height = 6, dpi = 300)
  cat("✓ Graphique exporté: plot_sentiment_bing.png\n")
}

if (exists("plot_nrc") && nrow(sentiment_nrc) > 0) {
  ggsave("plot_sentiment_nrc.png", plot_nrc, width = 10, height = 6, dpi = 300)
  cat("✓ Graphique exporté: plot_sentiment_nrc.png\n")
}

if (exists("plot_afinn") && nrow(sentiment_afinn_question) > 0) {
  ggsave("plot_sentiment_afinn.png", plot_afinn, width = 12, height = 8, dpi = 300)
  cat("✓ Graphique exporté: plot_sentiment_afinn.png\n")
}

ggsave("plot_frequence_mots.png", plot_freq, width = 10, height = 6, dpi = 300)
cat("✓ Graphique exporté: plot_frequence_mots.png\n\n")

# ==============================================================================
# 14. SYNTHÈSE FINALE ET INTERPRÉTATION
# ==============================================================================

cat("================================================================================\n")
cat("14. SYNTHÈSE FINALE - CRITÈRE DE JUGEMENT PRINCIPAL\n")
cat("================================================================================\n\n")

cat("OBJECTIF PRINCIPAL:\n")
cat("Créer une perception de la part des professionnels de l'urgence\n")
cat("sur ce que devrait idéalement être la médecine d'urgence.\n\n")

cat("CRITÈRE DE JUGEMENT PRINCIPAL:\n")
cat("Agrégation des termes et sentiments majoritaires projetés\n")
cat("vis-à-vis de la médecine d'urgence idéale.\n\n")

# ------------------------------------------------------------------------------
# 14.1. Synthèse Quantitative
# ------------------------------------------------------------------------------

cat("--- 14.1. SYNTHÈSE QUANTITATIVE ---\n\n")

perception_actuelle_specialite <- stats_globales %>%
  filter(Variable == "Perception_Actu_Specialite") %>%
  pull(Moyenne)

perception_future_specialite <- stats_globales %>%
  filter(Variable == "Perception_Fut_Specialite") %>%
  pull(Moyenne)

perception_actuelle_exercice <- stats_globales %>%
  filter(Variable == "Perception_Actu_Exercice") %>%
  pull(Moyenne)

perception_future_exercice <- stats_globales %>%
  filter(Variable == "Perception_Fut_Exercice") %>%
  pull(Moyenne)

cat("Perceptions moyennes (échelle 0-10):\n")
cat(paste0("  - Perception actuelle de la spécialité: ", 
           round(perception_actuelle_specialite, 2), "/10\n"))
cat(paste0("  - Perception future de la spécialité: ", 
           round(perception_future_specialite, 2), "/10\n"))
cat(paste0("  - Perception actuelle de l'exercice: ", 
           round(perception_actuelle_exercice, 2), "/10\n"))
cat(paste0("  - Perception future de l'exercice: ", 
           round(perception_future_exercice, 2), "/10\n\n"))

# Évolution des perceptions
evolution_specialite <- perception_future_specialite - perception_actuelle_specialite
evolution_exercice <- perception_future_exercice - perception_actuelle_exercice

cat("Évolution des perceptions:\n")
cat(paste0("  - Spécialité: ", ifelse(evolution_specialite > 0, "+", ""),
           round(evolution_specialite, 2), " points\n"))
cat(paste0("  - Exercice: ", ifelse(evolution_exercice > 0, "+", ""),
           round(evolution_exercice, 2), " points\n\n"))

# ------------------------------------------------------------------------------
# 14.2. Synthèse Qualitative
# ------------------------------------------------------------------------------

cat("--- 14.2. SYNTHÈSE QUALITATIVE ---\n\n")

# Top 5 mots positifs et négatifs
if (nrow(sentiment_bing) > 0) {
  top_positifs <- sentiment_bing %>%
    filter(sentiment == "positive") %>%
    head(5) %>%
    pull(word)
  
  top_negatifs <- sentiment_bing %>%
    filter(sentiment == "negative") %>%
    head(5) %>%
    pull(word)
  
  cat("Termes majoritaires POSITIFS (BING/FEEL):\n")
  cat(paste0("  ", paste(top_positifs, collapse = ", "), "\n\n"))
  
  cat("Termes majoritaires NÉGATIFS (BING/FEEL):\n")
  cat(paste0("  ", paste(top_negatifs, collapse = ", "), "\n\n"))
}

# Émotions dominantes
if (nrow(sentiment_nrc) > 0) {
  top_emotions <- sentiment_nrc %>%
    arrange(desc(n)) %>%
    head(3) %>%
    pull(sentiment)
  
  cat("Émotions dominantes (NRC/FEEL):\n")
  for (i in 1:min(3, length(top_emotions))) {
    cat(paste0("  ", i, ". ", top_emotions[i], "\n"))
  }
  cat("\n")
}

# ------------------------------------------------------------------------------
# 14.3. Réponse à la Question de Recherche
# ------------------------------------------------------------------------------

cat("--- 14.3. DÉFINITION DE LA MÉDECINE D'URGENCE IDÉALE ---\n\n")

cat("Sur la base de l'agrégation des données quantitatives et qualitatives,\n")
cat("la médecine d'urgence idéale selon les professionnels interrogés se caractérise par:\n\n")

if (exists("top_positifs") && length(top_positifs) > 0) {
  cat("POINTS POSITIFS MAJEURS (analyse qualitative FEEL):\n")
  for (i in 1:min(5, length(top_positifs))) {
    cat(paste0("  • ", top_positifs[i], "\n"))
  }
  cat("\n")
}

if (exists("top_negatifs") && length(top_negatifs) > 0) {
  cat("POINTS DE PRÉOCCUPATION (analyse qualitative FEEL):\n")
  for (i in 1:min(5, length(top_negatifs))) {
    cat(paste0("  • ", top_negatifs[i], "\n"))
  }
  cat("\n")
}

cat("PERCEPTIONS QUANTITATIVES:\n")
cat(paste0("  • Perception actuelle de la spécialité: ", 
           round(perception_actuelle_specialite, 1), "/10\n"))
cat(paste0("  • Perspective d'évolution: ", 
           ifelse(evolution_specialite > 0, "positive", "négative"), 
           " (", ifelse(evolution_specialite > 0, "+", ""),
           round(evolution_specialite, 1), " points)\n\n"))

# ------------------------------------------------------------------------------
# 14.4. Différences par Sous-Populations
# ------------------------------------------------------------------------------

cat("--- 14.4. DIFFÉRENCES PAR SOUS-POPULATIONS ---\n\n")

if (nrow(resultats_significatifs) > 0) {
  cat("Différences significatives identifiées selon:\n")
  sous_pops_significatives <- unique(resultats_significatifs$Group_Var)
  for (pop in sous_pops_significatives) {
    n_diffs <- sum(resultats_significatifs$Group_Var == pop)
    cat(paste0("  • ", pop, " (", n_diffs, " différence(s) significative(s))\n"))
  }
} else {
  cat("Aucune différence significative entre sous-populations détectée.\n")
}

cat("\n")

# ==============================================================================
# 15. CONFORMITÉ AU PROTOCOLE ET RECOMMANDATIONS
# ==============================================================================

cat("================================================================================\n")
cat("15. CONFORMITÉ AU PROTOCOLE ET RECOMMANDATIONS\n")
cat("================================================================================\n\n")

cat("✓ Analyses réalisées conformément au protocole PERCEPT'urg:\n")
cat("  [✓] Analyse quantitative (échelles de Likert 0-10)\n")
cat("  [✓] Statistiques descriptives (Moyenne, ET, IC95%)\n")
cat("  [✓] Tests inférentiels (t-Student, Mann-Whitney, ANOVA, Kruskal-Wallis)\n")
cat("  [✓] Analyse qualitative (NLP avec lexique FEEL)\n")
cat("  [✓] Analyse BING (polarité positive/négative via FEEL)\n")
cat("  [✓] Analyse NRC (10 émotions via FEEL)\n")
cat("  [✓] Analyse AFINN (scores numériques calculés via FEEL)\n")
cat("  [✓] Analyse lexicale (fréquence, nuages de mots)\n")
cat("  [✓] Topic modeling (LDA)\n")
cat("  [✓] Tests de robustesse\n")
cat("  [✓] Comparaisons par sous-populations\n\n")

cat("📚 LEXIQUE UTILISÉ:\n")
cat("  FEEL (French Expanded Emotion Lexicon)\n")
cat("  - Source: LIRMM, Université de Montpellier\n")
cat("  - Référence: Abdaoui et al. (2017), Language Resources and Evaluation\n")
cat("  - Contenu: ", nrow(feel_raw), " mots français\n")
cat("  - Format: 2 polarités + 8 émotions (modèle d'Ekman)\n")
cat("  - URL: http://advanse.lirmm.fr/feel.php\n\n")

cat("⚠️  Compléments recommandés:\n")
cat("  [ ] Vectorisation avancée (Word2Vec, Doc2Vec)\n")
cat("  [ ] Gestion complète des négations (modèles contextuels)\n")
cat("  [ ] Analyse thématique NVivo/MAXQDA\n")
cat("  [ ] Calibration sur corpus témoin (1984)\n")
cat("  [ ] Conformité TRIPOD-AI pour publication\n\n")

cat("📊 Fichiers de résultats générés:\n")
cat("  - resultats_stats_globales.csv\n")
cat("  - resultats_comparaisons.csv\n")
cat("  - resultats_significatifs.csv\n")
cat("  - resultats_sentiment_bing.csv\n")
cat("  - resultats_sentiment_nrc.csv\n")
cat("  - resultats_sentiment_afinn.csv\n")
cat("  - resultats_frequence_mots.csv\n")
cat("  - resultats_topics_lda.csv\n")
cat("  - plot_perceptions.png\n")
cat("  - plot_sentiment_bing.png\n")
cat("  - plot_sentiment_nrc.png\n")
cat("  - plot_sentiment_afinn.png\n")
cat("  - plot_frequence_mots.png\n\n")

cat("📝 CITATION POUR LE MANUSCRIT:\n\n")
cat('L\'analyse de sentiment a utilisé le lexique FEEL (French Expanded Emotion Lexicon,\n')
cat('Abdaoui et al., 2017) contenant plus de 14 000 mots français annotés selon\n')
cat('la polarité (positive/négative) et 8 émotions de base (colère, anticipation,\n')
cat('dégoût, peur, joie, tristesse, surprise, confiance). Ce lexique unique a permis\n')
cat('de réaliser simultanément:\n')
cat('- L\'analyse de polarité (équivalent BING)\n')
cat('- L\'analyse émotionnelle à 10 dimensions (équivalent NRC)\n')
cat('- L\'analyse de sentiment avec scores numériques (équivalent AFINN)\n\n')

cat("📚 Référence bibliographique:\n")
cat("Abdaoui, A., Azé, J., Bringay, S., & Poncelet, P. (2017).\n")
cat("FEEL: a French expanded emotion lexicon.\n")
cat("Language Resources and Evaluation, 51(3), 833-855.\n")
cat("https://doi.org/10.1007/s10579-016-9364-5\n\n")

cat("================================================================================\n")
cat("ANALYSE TERMINÉE\n")
cat("================================================================================\n\n")

cat("Pour toute question sur la méthodologie ou l'interprétation des résultats,\n")
cat("veuillez consulter le protocole expérimental PERCEPT'urg.\n\n")

# Fin du script
