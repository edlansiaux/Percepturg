# PERCEPT'urg: Emergency Medicine Perception Study

[![R Version](https://img.shields.io/badge/R-%3E%3D%204.4.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![FEEL License](https://img.shields.io/badge/FEEL-Research%20Only-orange)](http://advanse.lirmm.fr/feel.php)

**A quantitative-qualitative analysis tool for studying emergency medicine professionals' perceptions**

Developed by the Young Professionals Committee (Commission Jeunes) of the French Society of Emergency Medicine (SFMU)

---

## 📋 Table of Contents

- [About the Project](#about-the-project)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Data Structure](#data-structure)
- [Output Files](#output-files)
- [Methodology](#methodology)
- [Citation](#citation)
- [License](#license)
- [Acknowledgments](#acknowledgments)
- [Contact](#contact)

---

## 🎯 About the Project

**PERCEPT'urg** is a comprehensive research study designed to understand how emergency medicine professionals perceive their specialty, working conditions, and the ideal characteristics of emergency medical practice.

This repository contains an optimized R script for analyzing survey responses using:
- **Quantitative analysis**: Likert scales (0-10), descriptive statistics, inferential tests
- **Qualitative analysis**: Natural Language Processing (NLP) using sentiment analysis
- **Topic modeling**: Latent Dirichlet Allocation (LDA) for thematic analysis

### Key Innovation

The analysis uses **FEEL (French Expanded Emotion Lexicon)** as a single, unified lexicon source to perform three types of sentiment analysis:
- **BING-type analysis**: Positive/negative polarity
- **NRC-type analysis**: 10 emotional dimensions (8 emotions + 2 polarities)
- **AFINN-type analysis**: Numerical sentiment scores

This approach ensures consistency, simplicity, and scientific rigor with a single, peer-reviewed French lexicon.

---

## ✨ Features

### Quantitative Analysis
- ✅ Descriptive statistics (Mean, SD, 95% CI)
- ✅ Normality tests (Shapiro-Wilk)
- ✅ Homogeneity of variance tests (Levene)
- ✅ Inferential tests:
  - Student's t-test (2 groups, normal distribution)
  - Mann-Whitney U test (2 groups, non-normal distribution)
  - ANOVA (>2 groups, normal distribution)
  - Kruskal-Wallis test (>2 groups, non-normal distribution)
- ✅ Subpopulation comparisons (by gender, age, profession, facility type)

### Qualitative Analysis
- ✅ Tokenization and text cleaning
- ✅ Sentiment analysis (BING/FEEL: positive/negative polarity)
- ✅ Emotion analysis (NRC/FEEL: 10 emotional categories)
- ✅ Sentiment scoring (AFINN/FEEL: numerical values)
- ✅ Word frequency analysis
- ✅ Word clouds
- ✅ Topic modeling (LDA)
- ✅ Negation detection

### Visualizations
- 📊 Bar charts with error bars
- 📈 Sentiment distribution plots
- 🎨 Emotion radial charts
- ☁️ Word clouds
- 📉 Topic distribution graphs

### Export & Reporting
- 📄 CSV files for all analyses
- 🖼️ High-resolution PNG graphics (300 DPI)
- 📋 Console output with detailed statistics
- ✅ Publication-ready results

---

## 🔧 Prerequisites

### R Environment
- **R version**: ≥ 4.4.0
- **RStudio**: Recommended (optional)

### Required R Packages

The script will attempt to install missing packages automatically, but you can pre-install them:

```r
install.packages(c(
  "tidyverse",      # Data manipulation (dplyr, ggplot2, etc.)
  "readr",          # CSV import
  "psych",          # Descriptive statistics
  "rstatix",        # Statistical tests
  "car",            # Normality and homogeneity tests
  "tidytext",       # Text analysis (NLP)
  "tm",             # Additional text processing
  "wordcloud",      # Word cloud generation
  "ggplot2",        # Data visualization
  "stopwords",      # French stopwords
  "SnowballC",      # Stemming
  "topicmodels",    # LDA topic modeling
  "grid",           # Graphics arrangements
  "gridExtra"       # Multiple plots
))
```

---

## 📥 Installation

### Option 1: Clone the Repository

```bash
git clone https://github.com/yourusername/percept-urg-analysis.git
cd percept-urg-analysis
```

### Option 2: Download ZIP

1. Click the green **Code** button on GitHub
2. Select **Download ZIP**
3. Extract the archive to your desired location

---

## 🚀 Usage

### Step 1: Prepare Your Data

Export your survey responses from Google Forms as a CSV file:
- File name: `reponses_questionnaire.csv`
- Encoding: UTF-8
- Separator: comma (`,`) or semicolon (`;`)

Place the file in the same directory as the R script.

### Step 2: Verify FEEL Lexicon

The repository includes `FEEL.csv`. If missing, download it from:
- **URL**: http://advanse.lirmm.fr/feel.php
- **File**: FEEL.csv (UTF-8 encoded)

### Step 3: Run the Analysis

#### In RStudio:
1. Open `percept_urg_analysis.R`
2. Set working directory: `Session > Set Working Directory > To Source File Location`
3. Run the entire script: `Ctrl+Shift+Enter` (Windows) or `Cmd+Shift+Return` (Mac)

#### In R Console:
```r
setwd("/path/to/percept-urg-analysis")
source("percept_urg_analysis.R")
```

#### In Command Line:
```bash
Rscript percept_urg_analysis.R
```

### Step 4: Review Results

All outputs are saved in the working directory:
- CSV files: `resultats_*.csv`
- Graphics: `plot_*.png`
- Console output with detailed statistics

---

## 📊 Data Structure

### Input File: `reponses_questionnaire.csv`

Expected column structure (from Google Forms):

| Column # | Variable Name | Type | Description |
|----------|---------------|------|-------------|
| 1 | Horodateur | DateTime | Timestamp |
| 2 | Consentement | Text | Consent (must be "Oui") |
| 3 | Sexe | Categorical | Gender |
| 4 | Tranche_Age | Categorical | Age group |
| 5 | Fonction | Categorical | Profession |
| 6 | Experience_Urgence | Numeric | Years of emergency medicine experience |
| 7 | Type_Structure | Categorical | Facility type |
| 8 | Postes_Exerces | Text | Positions held |
| 9-21 | [Various] | Text | Open-ended qualitative questions |
| 22 | Perception_Actu_Specialite | Numeric (0-10) | Current perception of specialty |
| 23 | Perception_Fut_Specialite | Numeric (0-10) | Future perception of specialty |
| 24 | Perception_Actu_Exercice | Numeric (0-10) | Current perception of practice |
| 25 | Perception_Fut_Exercice | Numeric (0-10) | Future perception of practice |
| 26 | Commentaire_Libre | Text | Free comments |

### FEEL Lexicon: `FEEL.csv`

Structure:

| Column | Type | Description |
|--------|------|-------------|
| word | Text | French word |
| polarity | Text | "positive" or "negative" |
| anger | Binary (0/1) | Anger emotion |
| anticipation | Binary (0/1) | Anticipation emotion |
| disgust | Binary (0/1) | Disgust emotion |
| fear | Binary (0/1) | Fear emotion |
| joy | Binary (0/1) | Joy emotion |
| sadness | Binary (0/1) | Sadness emotion |
| surprise | Binary (0/1) | Surprise emotion |
| trust | Binary (0/1) | Trust emotion |

**Source**: Abdaoui et al. (2017), Language Resources and Evaluation

---

## 📁 Output Files

### Quantitative Results

| File | Description |
|------|-------------|
| `resultats_stats_globales.csv` | Global descriptive statistics (Mean, SD, 95% CI) |
| `resultats_comparaisons.csv` | All inferential tests results |
| `resultats_significatifs.csv` | Significant differences only (p < 0.05) |

### Qualitative Results

| File | Description |
|------|-------------|
| `resultats_sentiment_bing.csv` | Positive/negative word frequencies |
| `resultats_sentiment_nrc.csv` | Emotion distribution (10 categories) |
| `resultats_sentiment_afinn.csv` | Sentiment scores by question |
| `resultats_frequence_mots.csv` | Word frequency analysis |
| `resultats_topics_lda.csv` | LDA topic modeling results |

### Visualizations

| File | Description |
|------|-------------|
| `plot_perceptions.png` | Bar chart of perceptions (Mean ± 95% CI) |
| `plot_sentiment_bing.png` | Top positive/negative words |
| `plot_sentiment_nrc.png` | Emotion distribution chart |
| `plot_sentiment_afinn.png` | Sentiment scores by question |
| `plot_frequence_mots.png` | Top 20 most frequent words |

All graphics are exported in high resolution (300 DPI) for publication.

---

## 🔬 Methodology

### Statistical Tests Selection Algorithm

The script automatically selects the appropriate statistical test:

```
For 2 groups:
├─ Normal distribution + homogeneous variances → Student's t-test
└─ Non-normal distribution OR heterogeneous variances → Mann-Whitney U test

For >2 groups:
├─ Normal distribution + homogeneous variances → ANOVA
└─ Non-normal distribution OR heterogeneous variances → Kruskal-Wallis test
```

**Significance threshold**: α = 0.05

### Sentiment Analysis Workflow

1. **Tokenization**: Text split into individual words
2. **Cleaning**: Remove stopwords, non-alphabetic tokens, words < 3 characters
3. **Lexicon matching**: Match words with FEEL entries
4. **Sentiment calculation**:
   - **BING**: Count positive/negative words
   - **NRC**: Count words by emotion
   - **AFINN**: Sum sentiment scores

### AFINN Score Calculation from FEEL

Since FEEL doesn't provide native numerical scores, the script generates them:

```r
score = score_polarity + score_positive_emotions + score_negative_emotions

Where:
- score_polarity = +2 (positive) or -2 (negative)
- score_positive_emotions = joy + trust + anticipation (each +1)
- score_negative_emotions = -(anger + fear + disgust + sadness) (each -1)
```

**Example**:
- Word: "excellent"
- Polarity: positive (+2)
- Emotions: joy (1), trust (1)
- **Total score**: +4

### Topic Modeling (LDA)

- **Algorithm**: Latent Dirichlet Allocation
- **Default topics**: 5 (adjustable in script)
- **Robustness test**: Tests with K = 3, 5, 7, 10 topics

---

## 📝 Citation

### For This Analysis Tool

If you use this script in your research, please cite:

```bibtex
@software{percept_urg_2024,
  title = {PERCEPT'urg: Emergency Medicine Perception Analysis Tool},
  author = {{Commission Jeunes - SFMU}},
  year = {2024},
  url = {https://github.com/yourusername/percept-urg-analysis},
  note = {R script for quantitative-qualitative analysis using FEEL lexicon}
}
```

### For the FEEL Lexicon

**Mandatory citation** when using FEEL:

```bibtex
@article{abdaoui2017feel,
  title={FEEL: a French expanded emotion lexicon},
  author={Abdaoui, Amine and Az{\'e}, J{\'e}r{\^o}me and Bringay, Sandra and Poncelet, Pascal},
  journal={Language Resources and Evaluation},
  volume={51},
  number={3},
  pages={833--855},
  year={2017},
  publisher={Springer},
  doi={10.1007/s10579-016-9364-5}
}
```

### For Your Manuscript

**Methods section example**:

> Sentiment analysis was performed using the French Expanded Emotion Lexicon (FEEL; Abdaoui et al., 2017), containing over 14,000 French words annotated for polarity (positive/negative) and eight basic emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, trust). This unified lexicon enabled simultaneous polarity analysis (BING-equivalent), 10-dimensional emotional analysis (NRC-equivalent), and numerical sentiment scoring (AFINN-equivalent). All statistical analyses were conducted in R (version 4.4.0 or higher).

---

## 📜 License

### Analysis Script

This R script is licensed under the **MIT License**.

```
MIT License

Copyright (c) 2024 Commission Jeunes - Société Française de Médecine d'Urgence (SFMU)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
