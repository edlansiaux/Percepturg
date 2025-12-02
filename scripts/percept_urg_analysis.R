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
# 5. CHARGEMENT ET PRÉPARATION DU LEXIQUE FEEL (VERSION CORRIGÉE ET ROBUSTE)
# ==============================================================================

cat("================================================================================\n")
cat("5. CHARGEMENT DU LEXIQUE FEEL (French Expanded Emotion Lexicon)\n")
cat("================================================================================\n\n")

feel_raw <- NULL
feel_source <- NULL

# Vérification de l'existence du fichier
if (!file.exists("FEEL.csv")) {
  stop("❌ ERREUR: Le fichier 'FEEL.csv' est introuvable. Veuillez le placer dans le dossier de travail.")
}

# TENTATIVE DE CHARGEMENT INTELLIGENT
cat("Tentative de chargement et détection du séparateur pour FEEL.csv...\n")

# 1. Essai avec point-virgule (format standard FEEL)
tryCatch({
  temp_feel <- read_delim("FEEL.csv", delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  if (ncol(temp_feel) > 2) {
    feel_raw <- temp_feel
    feel_source <- "FEEL.csv (point-virgule)"
    cat("✓ Chargé avec succès (séparateur: point-virgule)\n")
  }
}, error = function(e) {})

# 2. Essai avec virgule (si le point-virgule a échoué ou donné 1 colonne)
if (is.null(feel_raw) || ncol(feel_raw) < 2) {
  tryCatch({
    temp_feel <- read_csv("FEEL.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    if (ncol(temp_feel) > 2) {
      feel_raw <- temp_feel
      feel_source <- "FEEL.csv (virgule)"
      cat("✓ Chargé avec succès (séparateur: virgule)\n")
    }
  }, error = function(e) {})
}

# 3. Essai avec tabulation
if (is.null(feel_raw) || ncol(feel_raw) < 2) {
  tryCatch({
    temp_feel <- read_delim("FEEL.csv", delim = "\t", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    if (ncol(temp_feel) > 2) {
      feel_raw <- temp_feel
      feel_source <- "FEEL.csv (tabulation)"
      cat("✓ Chargé avec succès (séparateur: tabulation)\n")
    }
  }, error = function(e) {})
}

# Vérification finale du chargement
if (is.null(feel_raw) || ncol(feel_raw) < 2) {
  stop("❌ ERREUR CRITIQUE: Impossible de lire correctement la structure de FEEL.csv.\n",
       "   Assurez-vous que le fichier contient des délimiteurs (;, ou tabulation) corrects.")
}

# Nettoyage des noms de colonnes (suppression espaces, mise en minuscule)
names(feel_raw) <- tolower(trimws(names(feel_raw)))

# Identification des colonnes
col_names <- names(feel_raw)
word_col <- col_names[grepl("word|mot|term", col_names)][1]
if (is.na(word_col)) word_col <- col_names[2] # Fallback souvent la 2eme colonne dans FEEL brut

polarity_col <- col_names[grepl("polarity|polarite|sentiment", col_names)][1]

# Mapping des émotions
emotion_map <- list(
  anger = c("anger", "colere", "colère"),
  anticipation = c("anticipation"),
  disgust = c("disgust", "degout", "dégoût"),
  fear = c("fear", "peur"),
  joy = c("joy", "joie"),
  sadness = c("sadness", "tristesse"),
  surprise = c("surprise"),
  trust = c("trust", "confiance")
)

# Renommage standardisé
feel_clean <- feel_raw
names(feel_clean)[names(feel_clean) == word_col] <- "word"
if (!is.na(polarity_col)) names(feel_clean)[names(feel_clean) == polarity_col] <- "polarity"

# Renommage des émotions
for (emo in names(emotion_map)) {
  found <- intersect(names(feel_clean), emotion_map[[emo]])
  if (length(found) > 0) {
    names(feel_clean)[names(feel_clean) == found[1]] <- emo
  } else {
    # Création colonne vide si manquante
    feel_clean[[emo]] <- 0 
  }
}

# Sélection finale des colonnes utiles
required_cols <- c("word", "polarity", names(emotion_map))
feel_raw <- feel_clean %>%
  select(any_of(required_cols)) %>%
  filter(!is.na(word) & word != "") %>%
  distinct(word, .keep_all = TRUE)

cat("\n✓ Lexique FEEL standardisé:\n")
cat("  - Mots:", nrow(feel_raw), "\n")
cat("  - Colonnes:", paste(names(feel_raw), collapse = ", "), "\n\n")

# ------------------------------------------------------------------------------
# 5.1. Préparation de FEEL pour l'Analyse BING (Polarité: Positif/Négatif)
# ------------------------------------------------------------------------------

cat("\n--- 5.1. PRÉPARATION POUR ANALYSE BING (Polarité) ---\n")

if ("polarity" %in% names(feel_raw)) {
  french_bing_lexicon <- feel_raw %>%
    select(word, polarity) %>%
    filter(!is.na(polarity) & polarity != "") %>%
    rename(sentiment = polarity) %>%
    mutate(sentiment = tolower(sentiment)) %>%
    filter(sentiment %in% c("positive", "negative"))
  
  cat("✓ Lexique BING (polarité) créé:\n")
  cat("  - Mots positifs:", sum(french_bing_lexicon$sentiment == "positive"), "\n")
  cat("  - Mots négatifs:", sum(french_bing_lexicon$sentiment == "negative"), "\n\n")
} else {
  cat("⚠️ Colonne 'polarity' absente. Analyse BING impossible.\n")
  french_bing_lexicon <- tibble(word = character(), sentiment = character())
}

# ------------------------------------------------------------------------------
# 5.2. Préparation de FEEL pour l'Analyse NRC (8 Émotions + 2 Polarités)
# ------------------------------------------------------------------------------

cat("--- 5.2. PRÉPARATION POUR ANALYSE NRC (Émotions) ---\n")

# Partie 1 : Polarités
nrc_polarity <- french_bing_lexicon

# Partie 2 : Émotions
# On pivote les colonnes d'émotion qui sont à 1
emo_cols_present <- intersect(names(feel_raw), names(emotion_map))

if (length(emo_cols_present) > 0) {
  nrc_emotions <- feel_raw %>%
    select(word, all_of(emo_cols_present)) %>%
    pivot_longer(cols = all_of(emo_cols_present),
                 names_to = "sentiment",
                 values_to = "value") %>%
    filter(value == 1) %>%
    select(word, sentiment)
  
  french_nrc_lexicon <- bind_rows(nrc_polarity, nrc_emotions) %>% distinct()
  
  cat("✓ Lexique NRC (émotions) créé.\n\n")
} else {
  cat("⚠️ Colonnes d'émotions absentes. Analyse NRC impossible.\n")
  french_nrc_lexicon <- tibble(word = character(), sentiment = character())
}

# ------------------------------------------------------------------------------
# 5.3. Préparation de FEEL pour l'Analyse AFINN (Scores Numériques)
# ------------------------------------------------------------------------------

cat("--- 5.3. PRÉPARATION POUR ANALYSE AFINN (Scores) ---\n")

# Calcul des scores basés sur FEEL
# +2/-2 pour polarité, +/-1 pour chaque émotion
french_afinn_lexicon <- feel_raw %>%
  mutate(
    score_polarity = case_when(
      "polarity" %in% names(.) & polarity == "positive" ~ 2,
      "polarity" %in% names(.) & polarity == "negative" ~ -2,
      TRUE ~ 0
    ),
    score_positive_emotions = rowSums(select(., any_of(c("joy", "trust", "anticipation"))), na.rm = TRUE),
    score_negative_emotions = -rowSums(select(., any_of(c("anger", "fear", "disgust", "sadness"))), na.rm = TRUE),
    value = score_polarity + score_positive_emotions + score_negative_emotions
  ) %>%
  select(word, value) %>%
  filter(value != 0)

cat("✓ Lexique AFINN (scores) créé.\n")
cat("  - Mots avec score:", nrow(french_afinn_lexicon), "\n\n")


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
sentiment_bing %>% filter(sentiment == "positive") %>% head(10) %>% print()

cat("\nTop 10 des mots NÉGATIFS les plus fréquents:\n")
sentiment_bing %>% filter(sentiment == "negative") %>% head(10) %>% print()

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
  cat("⚠️  Aucun mot trouvé dans le lexique BING (données vides ou pas de correspondances)\n")
}

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

if(nrow(top_mots_emotions) > 0) {
  for (emotion in unique(top_mots_emotions$sentiment)) {
    cat(paste0("Émotion: ", toupper(emotion), "\n"))
    mots <- top_mots_emotions %>%
      filter(sentiment == emotion) %>%
      select(word, n)
    print(mots)
    cat("\n")
  }
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

# Vérification qu'il y a assez de données pour le LDA
if (dtm$nrow > 0 && dtm$ncol > 0) {
  lda_model <- LDA(dtm, k = K_topics, control = list(seed = 1234))
  
  # Extraction des termes principaux par sujet
  topics_terms <- terms(lda_model, 10)
  
  cat("SUJETS IDENTIFIÉS (Top 10 mots par sujet):\n\n")
  for (i in 1:K_topics) {
    cat(paste0("Sujet ", i, ": ", paste(topics_terms[, i], collapse = ", "), "\n"))
  }
} else {
  cat("⚠️ Pas assez de données pour le modèle LDA.\n")
}

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
if(exists("sentiment_bing") && nrow(sentiment_bing) > 0) write_csv(sentiment_bing, "resultats_sentiment_bing.csv")
cat("✓ Fichier exporté: resultats_sentiment_bing.csv\n")

# Sentiments NRC
if(exists("sentiment_nrc") && nrow(sentiment_nrc) > 0) write_csv(sentiment_nrc, "resultats_sentiment_nrc.csv")
cat("✓ Fichier exporté: resultats_sentiment_nrc.csv\n")

# Sentiments AFINN par question
if(exists("sentiment_afinn_question") && nrow(sentiment_afinn_question) > 0) write_csv(sentiment_afinn_question, "resultats_sentiment_afinn.csv")
cat("✓ Fichier exporté: resultats_sentiment_afinn.csv\n")

# Fréquence des mots
write_csv(frequence_mots, "resultats_frequence_mots.csv")
cat("✓ Fichier exporté: resultats_frequence_mots.csv\n")

# Sujets LDA
if(exists("topics_terms")) write.csv(topics_terms, "resultats_topics_lda.csv", row.names = TRUE)
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

cat("================================================================================\n")
cat("ANALYSE TERMINÉE\n")
cat("================================================================================\n\n")
