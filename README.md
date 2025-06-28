# Relatório de Análise Estatística

**Tema**: *\[Análise Estatística do Ranking de Raças de Cães: Fatores Determinantes da Adequação]*  
**Autores**: *\[Pedro Henrique de Oliveira Bicalho e Vitor Pizzol]*  
**Data**: *\[28/06/2025]*

## 1\. Introdução

Este relatório apresenta uma análise estatística abrangente do dataset "Dogs Ranking", que contém informações detalhadas sobre diferentes raças de cães e suas características. O objetivo é aplicar conceitos de probabilidade e estatística para extrair insights significativos sobre as raças caninas, seus custos, inteligência, adequação para famílias e características físicas.
-----

## 2\. Descrição do Conjunto de Dados

* Fonte dos dados: [Link para o dataset](https://www.kaggle.com/datasets/jainaru/dog-breeds-ranking-best-to-worst?resource=download)
* Número de observações (linhas): `87`
* Número de variáveis (colunas): `18`

### 2.1. O que representam os dados?

Este conjunto de dados contém informações sobre [descreva o que cada linha representa, ex: clientes, experimentos, observações, etc.] e foi obtido a partir de [descreva a fonte dos dados, ex: um repositório público, coleta própria, etc.].

| Coluna | Descrição | Tipo de Dado | Exemplo |
|---|---|---|---|
| `Breed` | Nome da Raça do Cão | Categórico | "Border Terrier" |
| `Intelligence` | Nível de Inteligência | Categórico | Above average |
| `Score for kids` | Pontuação de adequação com crianças | Numérico | 4.99 |
| `Lifetime cost` | Custo de vida estimado | Numérico | "$22,638" |
| `Longevity` | Expectativa de vida em anos | Numérico | 14 |

### 2.2. Como os dados foram obtidos?

Os dados foram coletados por meio de compilação de informações de diversas fontes especializadas em cães, incluindo associações cinológicas, veterinários e especialistas em comportamento animal. Os dados foram pré-processados para padronizar as unidades monetárias em dólares americanos e normalizar as categorias de inteligência e tamanho.

-----

## 3\. Questões Motivadoras

Nesta análise, buscamos responder às seguintes questões/problemas:

  * **Questão 1:** Existe uma relação significativa entre a inteligência das raças e sua adequação para crianças?
  * **Questão 2:** Qual o impacto do tamanho da raça e dos custos de manutenção na pontuação geral?
  * **Questão 3:** Raças com mais problemas genéticos tendem a ter menor expectativa de vida?
  * **Questão 4:** Existe diferença significativa na pontuação geral entre os diferentes grupos de raças (terrier, sporting, working, etc.)?

-----

## 4\. Códigos Utilizados

Todos os códigos para esta análise foram desenvolvidos em **R**.

### 4.1 Carregamento de Biblioteca e Dados

```r

# Carregamento das bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

# Carregamento dos dados
df <- read_csv("dogs-ranking-dataset.csv")

# Visualização inicial dos dados
head(df)
str(df)
```

### 4.2 Medidas de Tendência Central
#### Média 

```r
# Média das variáveis numéricas
media_score_kids <- mean(df$`score for kids`, na.rm = TRUE)
media_longevity <- mean(df$`LONGEVITY(YEARS)`, na.rm = TRUE)

# Tratamento da coluna de custo (remover $ e vírgulas)
df$lifetime_cost_clean <- as.numeric(gsub("[,$]", "", df$`LIFETIME COST`))
media_lifetime_cost <- mean(df$lifetime_cost_clean, na.rm = TRUE)

cat("Média Score for Kids:", round(media_score_kids, 2), "\n")
cat("Média Longevidade:", round(media_longevity, 2), "anos\n")
cat("Média Custo de Vida: $", round(media_lifetime_cost, 2), "\n")

```

#### Mediana 

```r 
# Mediana das variáveis numéricas
mediana_score_kids <- median(df$`score for kids`, na.rm = TRUE)
mediana_longevity <- median(df$`LONGEVITY(YEARS)`, na.rm = TRUE)
mediana_lifetime_cost <- median(df$lifetime_cost_clean, na.rm = TRUE)

cat("Mediana Score for Kids:", mediana_score_kids, "\n")
cat("Mediana Longevidade:", mediana_longevity, "anos\n")
cat("Mediana Custo de Vida: $", mediana_lifetime_cost, "\n")

```

#### Moda

```r
# Função para calcular moda
calcular_moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Moda das variáveis
moda_intelligence <- calcular_moda(df$intelligence)
moda_score_kids <- calcular_moda(df$`score for kids`)
moda_longevity <- calcular_moda(df$`LONGEVITY(YEARS)`)

cat("Moda Inteligência:", moda_intelligence, "\n")
cat("Moda Score for Kids:", moda_score_kids, "\n")
cat("Moda Longevidade:", moda_longevity, "anos\n")

```

### 4.3 Medidas de Dispersão

```r

# Desvio padrão
std_score_kids <- sd(df$`score for kids`, na.rm = TRUE)
std_longevity <- sd(df$`LONGEVITY(YEARS)`, na.rm = TRUE)

# Variância
var_score_kids <- var(df$`score for kids`, na.rm = TRUE)
var_longevity <- var(df$`LONGEVITY(YEARS)`, na.rm = TRUE)

cat("Desvio Padrão Score for Kids:", round(std_score_kids, 2), "\n")
cat("Variância Longevidade:", round(var_longevity, 2), "\n")

```
### 4.4 Análise de Correlação 

```r
# Seleção de variáveis numéricas para correlação
numeric_vars <- df %>% 
  select(`score for kids`, `LONGEVITY(YEARS)`, `INTELLIGENCE %`) %>%
  na.omit()

# Matriz de correlação
correlation_matrix <- cor(numeric_vars)
print(correlation_matrix)

# Coeficiente de correlação específico
corr_score_longevity <- cor(df$`score for kids`, df$`LONGEVITY(YEARS)`, use = "complete.obs")
cat("Correlação Score-Longevidade:", round(corr_score_longevity, 3), "\n")

```
### 4.5 Análise Descritiva Completa 

```r 

# Estatísticas descritivas
desc_stats <- df %>% 
  select(`score for kids`, `LONGEVITY(YEARS)`) %>%
  summary()
print(desc_stats)

# Quartis e IQR
q1_score <- quantile(df$`score for kids`, 0.25, na.rm = TRUE)
q3_score <- quantile(df$`score for kids`, 0.75, na.rm = TRUE)
iqr_score <- q3_score - q1_score

cat("Q1 Score for Kids:", q1_score, "\n")
cat("Q3 Score for Kids:", q3_score, "\n")
cat("IQR Score for Kids:", round(iqr_score, 2), "\n")

```

## 5\. Figuras Geradas e Explicações

Insira aqui gráficos com breve explicação. Para adicionar imagens no Markdown do GitHub:

```markdown
![Texto alternativo](caminho/para/imagem.png)
```

### 5.1. Histograma da Expectativa de Vida

  * **Explicação:** Este histograma mostra a distribuição da variável `Idade`. Observa-se que [descreva as principais características da distribuição, ex: a maioria dos indivíduos está na faixa de 30-40 anos, a distribuição é levemente assimétrica à direita, etc.]. Isso é importante para entender [justifique a relevância da visualização, ex: a composição etária da nossa amostra, a presença de outliers, etc.].

### 5.2. Gráfico de Dispersão entre [Variável X] e [Variável Y]

  * **Explicação:** O gráfico de dispersão acima ilustra a relação entre as variáveis `Variável X` e `Variável Y`. É possível observar [descreva a tendência, ex: uma correlação positiva, negativa, ausência de correlação, agrupamentos, etc.]. Esta visualização auxilia na [justifique a relevância, ex: identificação de padrões, detecção de relações lineares ou não lineares, etc.].

### 5.3. Gráfico de Barras da Variável Categórica [Nome da Variável]

  * **Explicação:** Este gráfico de barras apresenta a frequência de cada categoria da variável `Região`. [Descreva o que o gráfico mostra, ex: a maioria dos dados pertence à "Região Sul", enquanto a "Região Norte" possui a menor representatividade]. Essa visualização é crucial para [justifique a relevância, ex: entender a distribuição das categorias, identificar desequilíbrios na amostra, etc.].

-----

## 6\. Justificativa de Utilização de Cada Medida

### 6.1. Medidas de Tendência Central (Média, Mediana, Moda)

  * **Média:** Utilizada para [justifique a escolha, ex: fornecer uma visão geral do valor "típico" em distribuições aproximadamente simétricas, pois é sensível a valores extremos].
  * **Mediana:** Empregada para [justifique a escolha, ex: representar o valor central em distribuições assimétricas ou com outliers, pois não é afetada por valores extremos].
  * **Moda:** Aplicada para [justifique a escolha, ex: identificar o valor mais frequente em variáveis categóricas ou discretas, sendo útil para entender a categoria mais comum].

### 6.2. Medidas de Dispersão (Desvio Padrão, Amplitude, Intervalo Interquartil - IQR)

  * **Desvio Padrão:** Escolhido para [justifique a escolha, ex: quantificar a dispersão dos dados em relação à média, sendo útil em distribuições normais ou aproximadamente normais].
  * **Amplitude:** Utilizada para [justifique a escolha, ex: fornecer uma medida rápida da variação total dos dados, indicando a diferença entre o valor máximo e mínimo].
  * **Intervalo Interquartil (IQR):** Aplicado para [justifique a escolha, ex: medir a dispersão dos 50% centrais dos dados, sendo robusto a outliers e útil em distribuições assimétricas].

### 6.3. Medidas de Posição (Quartis, Percentis)

  * **Quartis/Percentis:** Empregados para [justifique a escolha, ex: dividir os dados em partes iguais, permitindo analisar a distribuição e identificar a posição relativa de valores, útil para identificar outliers e entender a cauda da distribuição].

### 6.4. Medidas de Associação (Coeficiente de Correlação de Pearson/Spearman)

  * **Coeficiente de Correlação de Pearson:** Utilizado para [justifique a escolha, ex: medir a força e direção de uma relação linear entre duas variáveis numéricas, assumindo normalidade].
  * **Coeficiente de Correlação de Spearman:** Empregado para [justifique a escolha, ex: medir a força e direção de uma relação monotônica entre duas variáveis, sem assumir normalidade, sendo robusto a outliers e útil para dados ordinais].

-----

## 7\. Testes Estatísticos Utilizados

### 7.1. Teste [Nome do Teste, ex: Teste t de Student para duas amostras independentes]

  * **Objetivo:** [Descreva o objetivo do teste, ex: Comparar as médias de duas amostras independentes para determinar se há uma diferença estatisticamente significativa entre elas.]
  * **Hipóteses:**
      * **H0 (Hipótese Nula):** [Descreva a hipótese nula, ex: Não há diferença significativa entre as médias dos grupos.]
      * **H1 (Hipótese Alternativa):** [Descreva a hipótese alternativa, ex: Há uma diferença significativa entre as médias dos grupos.]
  * **Resultado:**
    ```
    # Código R do teste
    t.test(dados$VariávelNumérica ~ dados$VariávelCategórica, data = dados)
    ```
      * **P-valor:** [Valor do p-valor]
      * **Interpretação:** [Com base no p-valor e nível de significância, ex: Com um p-valor de [X] e um nível de significância de 0.05, rejeitamos/não rejeitamos a hipótese nula. Isso indica que há/não há evidências suficientes para afirmar uma diferença significativa entre as médias dos grupos.]

### 7.2. Teste [Nome do Teste, ex: Análise de Variância (ANOVA)]

  * **Objetivo:** [Descreva o objetivo do teste, ex: Comparar as médias de três ou mais grupos para determinar se há uma diferença estatisticamente significativa entre elas.]
  * **Hipóteses:**
      * **H0:** [Descreva a hipótese nula, ex: As médias de todos os grupos são iguais.]
      * **H1:** [Descreva a hipótese alternativa, ex: Pelo menos uma média de grupo é diferente das outras.]
  * **Resultado:**
    ```r
    # Código R do teste
    modelo_aov <- aov(VariávelResposta ~ Fator, data = dados)
    summary(modelo_aov)
    ```
      * **P-valor:** [Valor do p-valor]
      * **Interpretação:** [Com base no p-valor, ex: Dado o p-valor de [X], menor que 0.05, rejeitamos a hipótese nula, indicando que existe uma diferença significativa entre as médias de pelo menos um grupo.]

-----

## 8\. Análise e Discussão dos Resultados Obtidos

Nesta seção, discuta os achados mais importantes da sua análise em relação às questões motivadoras. Não apenas reporte os números, mas interprete-os e conecte-os ao contexto do problema.

  * **Para a Questão 1 ([Reformule a questão]):**

      * [Apresente os principais achados relacionados a essa questão. Ex: Os resultados do teste t indicaram uma diferença estatisticamente significativa na média de [Variável Resposta] entre os grupos A e B (p \< 0.05). O Grupo A apresentou uma média [maior/menor] de [Valor] em comparação com o Grupo B, que teve uma média de [Valor].]
      * [Discuta as implicações desses achados. Ex: Isso sugere que [implicação prática, ex: a intervenção X realmente teve um impacto positivo/negativo].]

  * **Para a Questão 2 ([Reformule a questão]):**

      * [Apresente os resultados dos modelos ou análises. Ex: O modelo de regressão linear múltipla revelou que [Variável Preditiva X] é um preditor significativo de [Variável Resposta] (coeficiente = [Valor], p \< 0.01), enquanto [Variável Preditiva Y] não se mostrou significativa.]
      * [Discuta o significado prático e teórico desses achados. Ex: Este achado confirma que [ideia central] e pode ser usado para [aplicação prática].]

  * **Para a Questão 3 ([Reformule a questão]):**

      * [Descreva os padrões ou agrupamentos encontrados. Ex: A análise exploratória de dados, como o gráfico de dispersão, sugeriu a existência de dois grupos distintos de dados, com diferentes comportamentos em relação às variáveis X e Y. Isso foi corroborado por [mencione outro teste ou análise se aplicável].]
      * [Analise as possíveis razões para esses padrões. Ex: Acreditamos que esses grupos possam representar [explicação, ex: diferentes segmentos de clientes, condições experimentais distintas, etc.].]

-----

## 9\. Conclusão

Este relatório explorou o conjunto de dados [Nome do Conjunto de Dados] com o objetivo de [reafirme o objetivo geral]. As principais descobertas incluem:

  * [Resumo do principal achado 1, ex: Houve uma diferença significativa em [Variável] entre os grupos A e B.]
  * [Resumo do principal achado 2, ex: [Variável X] foi um preditor significativo de [Variável Y].]
  * [Resumo do principal achado 3, ex: Foram identificados dois padrões distintos nos dados, sugerindo a presença de subgrupos.]

Esses resultados têm implicações para [mencione as implicações práticas ou teóricas, ex: a tomada de decisão em marketing, o desenvolvimento de novas políticas, a compreensão de um fenômeno, etc.].

### Limitações e Trabalhos Futuros

É importante notar que [mencione quaisquer limitações da análise, ex: o tamanho da amostra, a ausência de algumas variáveis importantes, vieses nos dados, etc.].

Para trabalhos futuros, sugerimos:

  * [Sugestão de pesquisa futura 1, ex: Coletar mais dados para aumentar a robustez das análises.]
  * [Sugestão de pesquisa futura 2, ex: Explorar modelos mais complexos, como machine learning, para previsões mais precisas.]
  * [Sugestão de pesquisa futura 3, ex: Realizar análises de sensibilidade para avaliar a robustez dos resultados a diferentes suposições.]

-----

## Referências / Links Úteis

  * [Conjunto de Dados Original](https://www.google.com/search?q=https://example.com/link_para_dados)
  * [Documentação do Pacote ggplot2](https://ggplot2.tidyverse.org/)
  * [Guia de Markdown para GitHub](https://docs.github.com/pt/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)



