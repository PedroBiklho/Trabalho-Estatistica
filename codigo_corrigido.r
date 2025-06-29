# =============================================================================
# Relatório de Análise Estatística - Ranking de Raças de Cães (VERSÃO CORRIGIDA)
# Autores: Pedro Henrique de Oliveira Bicalho e Vitor Hugo de Pizzol dos Santos
# Data: 28/06/2025
# =============================================================================

# Carregamento das bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

# =============================================================================
# 1. CARREGAMENTO E VERIFICAÇÃO DOS DADOS
# =============================================================================

# Carregamento dos dados
df <- read_csv("dogs-ranking-dataset.csv")

# Visualização inicial dos dados
cat("=== ESTRUTURA INICIAL DOS DADOS ===\n")
print(head(df))
cat("\nDimensões do dataset:", nrow(df), "linhas x", ncol(df), "colunas\n")
cat("\nNomes das colunas:\n")
print(colnames(df))

# =============================================================================
# 2. LIMPEZA E PREPARAÇÃO DOS DADOS (VERSÃO SEGURA)
# =============================================================================

# Função auxiliar para verificar se coluna existe
col_exists <- function(df, col_name) {
  return(col_name %in% colnames(df))
}

# Limpeza dos dados - apenas para colunas que existem
df_clean <- df

# Limpar colunas de custo se existirem
if(col_exists(df, "$LIFETIME COST")) {
  df_clean <- df_clean %>% 
    mutate(lifetime_cost_clean = as.numeric(gsub("[,$]", "", `$LIFETIME COST`)))
}

if(col_exists(df, "PURCHASE PRICE")) {
  df_clean <- df_clean %>% 
    mutate(purchase_price_clean = as.numeric(gsub("[,$]", "", `PURCHASE PRICE`)))
}

if(col_exists(df, "FOOD COSTS PER YEAR")) {
  df_clean <- df_clean %>% 
    mutate(food_costs_clean = as.numeric(gsub("[,$]", "", `FOOD COSTS PER YEAR`)))
}

if(col_exists(df, "INTELLIGENCE %")) {
  df_clean <- df_clean %>% 
    mutate(intelligence_pct = as.numeric(gsub("%", "", `INTELLIGENCE %`)))
}

# Renomear apenas as colunas que existem
rename_list <- list()

if(col_exists(df, "Breed")) rename_list[["breed"]] <- "Breed"
if(col_exists(df, "type")) rename_list[["type"]] <- "type"
if(col_exists(df, "score")) rename_list[["score"]] <- "score"
if(col_exists(df, "popularity ranking")) rename_list[["popularity_ranking"]] <- "popularity ranking"
if(col_exists(df, "intelligence")) rename_list[["intelligence"]] <- "intelligence"
if(col_exists(df, "score for kids")) rename_list[["score_for_kids"]] <- "score for kids"
if(col_exists(df, "LONGEVITY(YEARS)")) rename_list[["longevity_years"]] <- "LONGEVITY(YEARS)"
if(col_exists(df, "NUMBER OF GENETIC AILMENTS")) rename_list[["num_genetic_ailments"]] <- "NUMBER OF GENETIC AILMENTS"
if(col_exists(df, "GENETIC AILMENTS")) rename_list[["genetic_ailments"]] <- "GENETIC AILMENTS"
if(col_exists(df, "GROOMING FREQUNCY")) rename_list[["grooming_frequency"]] <- "GROOMING FREQUNCY"
if(col_exists(df, "SUITABILITY FOR CHILDREN")) rename_list[["suitability_for_children"]] <- "SUITABILITY FOR CHILDREN"

# Verificar se existe coluna de tamanho com nomes alternativos
size_col <- NULL
possible_size_cols <- c("size", "Size", "SIZE", "dog_size", "breed_size", "tamanho")
for(col in possible_size_cols) {
  if(col_exists(df, col)) {
    size_col <- col
    rename_list[["size"]] <- col
    break
  }
}

# Aplicar renomeação apenas para colunas que existem
if(length(rename_list) > 0) {
  df_clean <- df_clean %>% rename(!!!rename_list)
}

cat("\n=== COLUNAS RENOMEADAS COM SUCESSO ===\n")
print(names(rename_list))

if(is.null(size_col)) {
  cat("\n⚠️  AVISO: Coluna 'size' não encontrada. Análises por tamanho serão omitidas.\n")
}

cat("\n=== DADOS APÓS LIMPEZA ===\n")
print(summary(df_clean))

# =============================================================================
# 3. MEDIDAS DE TENDÊNCIA CENTRAL (ADAPTADO)
# =============================================================================

cat("\n=== MEDIDAS DE TENDÊNCIA CENTRAL ===\n")

# Função para calcular moda
calcular_moda <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Verificar quais variáveis numéricas existem e calcular estatísticas
if("score_for_kids" %in% colnames(df_clean)) {
  media_score_kids <- mean(df_clean$score_for_kids, na.rm = TRUE)
  mediana_score_kids <- median(df_clean$score_for_kids, na.rm = TRUE)
  moda_score_kids <- calcular_moda(df_clean$score_for_kids)
  cat("Score for Kids - Média:", round(media_score_kids, 2), 
      "| Mediana:", mediana_score_kids, 
      "| Moda:", moda_score_kids, "\n")
}

if("longevity_years" %in% colnames(df_clean)) {
  media_longevity <- mean(df_clean$longevity_years, na.rm = TRUE)
  mediana_longevity <- median(df_clean$longevity_years, na.rm = TRUE)
  moda_longevity <- calcular_moda(df_clean$longevity_years)
  cat("Longevidade - Média:", round(media_longevity, 2), "anos",
      "| Mediana:", mediana_longevity, "anos",
      "| Moda:", moda_longevity, "anos\n")
}

if("lifetime_cost_clean" %in% colnames(df_clean)) {
  media_lifetime_cost <- mean(df_clean$lifetime_cost_clean, na.rm = TRUE)
  mediana_lifetime_cost <- median(df_clean$lifetime_cost_clean, na.rm = TRUE)
  cat("Custo de Vida - Média: $", round(media_lifetime_cost, 2),
      "| Mediana: $", mediana_lifetime_cost, "\n")
}

if("intelligence_pct" %in% colnames(df_clean)) {
  media_intelligence_pct <- mean(df_clean$intelligence_pct, na.rm = TRUE)
  cat("Intelligence % - Média:", round(media_intelligence_pct, 2), "%\n")
}

# =============================================================================
# 4. MEDIDAS DE DISPERSÃO (ADAPTADO)
# =============================================================================

cat("\n=== MEDIDAS DE DISPERSÃO ===\n")

if("score_for_kids" %in% colnames(df_clean)) {
  std_score_kids <- sd(df_clean$score_for_kids, na.rm = TRUE)
  var_score_kids <- var(df_clean$score_for_kids, na.rm = TRUE)
  amplitude_score_kids <- max(df_clean$score_for_kids, na.rm = TRUE) - min(df_clean$score_for_kids, na.rm = TRUE)
  
  q1_score <- quantile(df_clean$score_for_kids, 0.25, na.rm = TRUE)
  q3_score <- quantile(df_clean$score_for_kids, 0.75, na.rm = TRUE)
  iqr_score <- q3_score - q1_score
  
  cat("Score for Kids:\n")
  cat("  Desvio Padrão:", round(std_score_kids, 2), "\n")
  cat("  Variância:", round(var_score_kids, 2), "\n")
  cat("  Amplitude:", round(amplitude_score_kids, 2), "\n")
  cat("  IQR:", round(iqr_score, 2), "\n\n")
}

if("longevity_years" %in% colnames(df_clean)) {
  std_longevity <- sd(df_clean$longevity_years, na.rm = TRUE)
  var_longevity <- var(df_clean$longevity_years, na.rm = TRUE)
  amplitude_longevity <- max(df_clean$longevity_years, na.rm = TRUE) - min(df_clean$longevity_years, na.rm = TRUE)
  
  q1_longevity <- quantile(df_clean$longevity_years, 0.25, na.rm = TRUE)
  q3_longevity <- quantile(df_clean$longevity_years, 0.75, na.rm = TRUE)
  iqr_longevity <- q3_longevity - q1_longevity
  
  cat("Longevidade:\n")
  cat("  Desvio Padrão:", round(std_longevity, 2), "\n")
  cat("  Variância:", round(var_longevity, 2), "\n")
  cat("  Amplitude:", round(amplitude_longevity, 2), "anos\n")
  cat("  IQR:", round(iqr_longevity, 2), "\n\n")
}

# =============================================================================
# 5. VISUALIZAÇÕES (ADAPTADAS)
# =============================================================================

cat("=== GERANDO VISUALIZAÇÕES ===\n")

# 5.1 Histograma da Expectativa de Vida (se disponível)
if("longevity_years" %in% colnames(df_clean)) {
  hist_longevity <- ggplot(df_clean, aes(x = longevity_years)) +
    geom_histogram(bins = 15, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Distribuição da Expectativa de Vida das Raças de Cães",
         x = "Expectativa de Vida (anos)",
         y = "Frequência") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(hist_longevity)
  cat("✓ Histograma de longevidade gerado\n")
}

# 5.2 Gráfico de Dispersão Intelligence vs Score for Kids (se disponível)
if("intelligence_pct" %in% colnames(df_clean) && "score_for_kids" %in% colnames(df_clean)) {
  scatter_intel_kids <- ggplot(df_clean, aes(x = intelligence_pct, y = score_for_kids)) +
    geom_point(alpha = 0.7, color = "darkblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "Relação entre Inteligência e Adequação para Crianças",
         x = "Percentual de Inteligência (%)",
         y = "Score para Crianças") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(scatter_intel_kids)
  cat("✓ Scatter plot Intelligence vs Kids gerado\n")
}

# 5.3 Gráfico de Barras dos Tipos de Raças (se disponível)
if("type" %in% colnames(df_clean)) {
  # Remover valores NA para o gráfico
  df_clean_type <- df_clean %>% filter(!is.na(type))
  
  bar_types <- ggplot(df_clean_type, aes(x = type)) +
    geom_bar(fill = "lightgreen", color = "black") +
    labs(title = "Distribuição dos Tipos de Raças",
         x = "Tipo de Raça",
         y = "Frequência") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  
  print(bar_types)
  cat("✓ Gráfico de barras dos tipos gerado\n")
}

# 5.4 Boxplot Score por Tamanho (se disponível)
if("size" %in% colnames(df_clean) && "score_for_kids" %in% colnames(df_clean)) {
  df_clean_size <- df_clean %>% filter(!is.na(size) & !is.na(score_for_kids))
  
  boxplot_size_score <- ggplot(df_clean_size, aes(x = size, y = score_for_kids)) +
    geom_boxplot(fill = "orange", alpha = 0.7) +
    labs(title = "Distribuição do Score para Crianças por Tamanho da Raça",
         x = "Tamanho",
         y = "Score para Crianças") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(boxplot_size_score)
  cat("✓ Boxplot por tamanho gerado\n")
} else {
  cat("⚠️  Boxplot por tamanho não pôde ser gerado (colunas ausentes)\n")
}

# 5.5 Matriz de Correlação (apenas variáveis disponíveis)
numeric_cols <- sapply(df_clean, is.numeric)
numeric_data <- df_clean[, numeric_cols, drop = FALSE]

if(ncol(numeric_data) >= 2) {
  # Remover colunas com apenas NAs
  numeric_data <- numeric_data[, colSums(is.na(numeric_data)) < nrow(numeric_data), drop = FALSE]
  
  if(ncol(numeric_data) >= 2) {
    correlation_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Verificar se há correlações válidas
    if(!all(is.na(correlation_matrix))) {
      corrplot(correlation_matrix, method = "color", type = "upper", 
               order = "hclust", tl.cex = 0.8, tl.col = "black")
      cat("✓ Matriz de correlação gerada\n")
    }
  }
}

# =============================================================================
# 6. ANÁLISES ESTATÍSTICAS DISPONÍVEIS
# =============================================================================

cat("\n=== ANÁLISES ESTATÍSTICAS ===\n")

# Teste de correlação (se possível)
if("intelligence_pct" %in% colnames(df_clean) && "score_for_kids" %in% colnames(df_clean)) {
  # Remover NAs para o teste
  data_for_test <- df_clean %>% 
    select(intelligence_pct, score_for_kids) %>% 
    na.omit()
  
  if(nrow(data_for_test) > 10) {
    teste_correlacao <- cor.test(data_for_test$intelligence_pct, data_for_test$score_for_kids)
    cat("Correlação Intelligence-Kids:", round(teste_correlacao$estimate, 3), 
        "(p-valor:", round(teste_correlacao$p.value, 4), ")\n")
  }
}

# Top raças (se dados disponíveis)
if("score_for_kids" %in% colnames(df_clean) && "breed" %in% colnames(df_clean)) {
  top_kids <- df_clean %>%
    filter(!is.na(score_for_kids)) %>%
    arrange(desc(score_for_kids)) %>%
    select(breed, score_for_kids) %>%
    head(10)
  
  cat("\nTOP 10 RAÇAS PARA CRIANÇAS:\n")
  print(top_kids)
}

cat("\n=== ANÁLISE CONCLUÍDA ===\n")
cat("Análise executada com as colunas disponíveis no dataset.\n")
cat("Verifique quais gráficos foram gerados com sucesso.\n")