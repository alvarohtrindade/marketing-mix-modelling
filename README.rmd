---
title: "Plano de Desenvolvimento (PDi) - Marketing Mix Model (Robyn)"
author: "Álvaro Henrique D. da Trindade"
date: "`r Sys.Date()`"
output:
  rmdformats::downcute:
    self_contained: true
    default_style: "light"
    downcute_theme: "default"
---

```{r setup, include=FALSE}
## Global options
knitr::opts_chunk$set(cache = TRUE)
```

## <img src="C:\Users\alvaro.trindade_ghfl\Downloads\logo_ghfly.png" alt="drawing" width="200"/>

# Introdução

Robyn é um pacote de modelagem de mix de marketing (MMM) de código aberto baseado em IA/ML da Meta Marketing Science. **É uma ferramenta que fornece várias técnicas de aprendizado de máquina** como regressão Ridge, algoritmo evolutivo multiobjetivo para otimização de hiperparâmetros, decomposição de série temporal para tendência, otimização baseada em gradiente para alocação de orçamento, entre outros.

Robyn é capaz de realizar uma análise estatística baseada em dados reais que quantifica o impacto e efetividade dos canais de marketing online e offline através de um KPI (Receita ou Vendas).

# 1- Configuração do Ambiente

Primeiro, precisamos instalar e carregar a biblioteca Robyn no R para verificar a versão instalada. Também forçamos a execução de vários núcleos da máquina com o comando "Sys.setenv", além de marcar com **TRUE** a opção de criar arquivos.

Vamos carregar os dados de mídia em um dataset chamado "data". Adicionamos também o conjunto de dados com feriados da biblioteca Prophet.

```{r}
#install.packages("Robyn")
library(Robyn)
packageVersion("Robyn")

Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

create_files <- TRUE

data = read.csv("C:/Users/alvaro.trindade_ghfl/Desktop/Módulos de Análise/advanced_analytics/df_mmm.csv")
head(dt_prophet_holidays)

robyn_directory <- "C:/Users/alvaro.trindade_ghfl/Desktop/R/robyn_mmm"
```

# 2- Definindo inputs do modelo

Com o comando InputCollect, definimos quais serão os inputs treinados pelo modelo, dividindo em variáveis mandatórias.

-   **dt_input** = dataset com valores de cada mídia.

-   **dt_holidays** = feriados.

-   **date_var\*** = Coluna de data no formato YYYY-YY-YY.

-   **dep_var** = variável dependente (Receita ou Vendas)

-   **dep_var_type** = tipo da variável dependente

-   **prophet_vars** = Finais de semana e feriados

-   **prophet_country** = País

-   **paid_media_spends** = Investimento em cada canal de mídia

-   **organic_vars** = Número de usuários do canal orgânico

-   **adstock** = Geometric, weibull_cdf ou weibull_pdf

```{r}
InputCollect <- robyn_inputs(
  dt_input = data,
  dt_holidays = dt_prophet_holidays,
  date_var = "Date", # formato de data deve ser "2020-01-01"
  dep_var = "Receita", # variável dependente
  dep_var_type = "revenue", # "revenue" (ROI) ou "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # Prophet
  prophet_country = "BR",
  context_vars = c("Custo_mkt", "ROAS"),
  paid_media_spends = c("Gasto_Google", "Facebook", "Gasto_CRITEO", "Gasto_Blue", "Gasto_Awin"), # mandatórias
  paid_media_vars = c("Gasto_Google", "Facebook", "Gasto_CRITEO", "Gasto_Blue", "Gasto_Awin"), # mandatórias
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  organic_vars = "Organic", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2023-10-01",
  window_end = "2024-05-22",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)
```

## 2.1- Hiperparâmetros

Os Hiperparâmetros vão ser usados para controlar o processo de treinamento do modelo de machine learning. No nosso contexto, cada canal de mídia vai receber hiperparâmetros (alphas, gammas e thetas), de acordo com o modelo de astock escolhido.

Quando se trata de marketing, um dos maiores desafios enfrentados é compreender e medir o impacto real das campanhas publicitárias ao longo do tempo. Aqui é onde o conceito de **Adstock** e **Saturação** se tornam fundamental para criação do MMM.

## 2.2- Adstock Geométrico

Adstock é um termo utilizado para descrever o fenômeno cumulativo e decrescente do efeito de cada canal. Em outras palavras, é a persistência do impacto da publicidade na consciência dos consumidores. Há varios tipos de Adstock entre eles o Geométrico (qual vamos usar).

Essa abordagem presume que o impacto da publicidade decai em uma taxa constante ao longo do tempo, seguindo uma curva exponencial.

$$Adstock(t) = Valor \ gasto(t) + Decay \ rate(t) * Valor \ gasto(t-1)$$

*"t" é um determinado período de tempo. O "decay_rate" é um valor constante por variável de mídia*

## 2.3- Saturação

A segunda característica diz respeito ao fato de que *a relação entre investimento e retorno não é linear* . Isso significa que chega um momento em que o aumento de investimento em uma mídia não traz mais um incremento. Este efeito é chamado de **saturação de investimentos**. O modelo é capaz de indicar o ponto em que o incremento não é mais vantajoso.

```{r}
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)
hyper_limits()

# Ranges dos hiperparâmetros do Adstock Geométrico
hyperparameters <- list(
  Gasto_Google_alphas = c(0.5, 3),
  Gasto_Google_gammas = c(0.3, 1),
  Gasto_Google_thetas = c(0, 0.3),
  Facebook_alphas = c(0.5, 3),
  Facebook_gammas = c(0.3, 1),
  Facebook_thetas = c(0.1, 0.4),
  Gasto_CRITEO_alphas = c(0.5, 3),
  Gasto_CRITEO_gammas = c(0.3, 1),
  Gasto_CRITEO_thetas = c(0.3, 0.8),
  Gasto_Blue_alphas = c(0.5, 3),
  Gasto_Blue_gammas = c(0.3, 1),
  Gasto_Blue_thetas = c(0, 0.3),
  Gasto_Awin_alphas = c(0.5, 3),
  Gasto_Awin_gammas = c(0.3, 1),
  Gasto_Awin_thetas = c(0.1, 0.4),
  Organic_alphas = c(0.5, 3),
  Organic_gammas = c(0.3, 1),
  Organic_thetas = c(0.1, 0.4),
  train_size = c(0.5, 0.8)
)

# Adicionando os hiperparâmetros ao modelo
InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}
```

# 3- Construindo modelo inicial

```{r}
OutputModels <- robyn_run(
  InputCollect = InputCollect,
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "1", # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)
```

## 3.1- Selecionando e salvando o melhor modelo

Para definir o melhor modelo treinado pelo Robyn,devemos analisar as métricas de performance como DECOMP.RSSD e NRMSE.

### DECOMP.RSSD (Decomposition Root Sum of Squared Differences)

**Definição**\
É uma métrica utilizada para avaliar a precisão da decomposição de efeitos em modelos de marketing mix modeling (MMM). Ela mede a diferença entre os efeitos observados e os efeitos preditos pelo modelo.

**Importância**\
A DECOMP.RSSD é importante porque ajuda a entender quão bem o modelo está decompondo os efeitos de diferentes canais de marketing. Um valor menor de DECOMP.RSSD indica uma melhor precisão na decomposição dos efeitos, significando que o modelo está capturando adequadamente as contribuições individuais dos canais de marketing.

### NRMSE (Normalized Root Mean Square Error)

**Definição**\
É uma variação da métrica RMSE (Root Mean Square Error) que é normalizada pela amplitude dos valores observados. Isso facilita a comparação da precisão do modelo em diferentes escalas.

**Importância**\
A NRMSE é crucial para avaliar a precisão geral do modelo em prever os valores observados. Ela é especialmente útil quando se lida com dados de diferentes escalas, pois a normalização permite uma comparação mais justa entre diferentes conjuntos de dados ou modelos.

```{r, warning=FALSE, message=FALSE}
print(OutputCollect)
select_model <- "1_451_2" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = FALSE)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

```

# 4- Criando um alocador de budget usando o modelo salvo

```{r}
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.

# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition

# NOTE: The order of constraints should follow:
InputCollect$paid_media_spends

# Scenario "max_response": "What's the max. return given certain spend?"
# Example 1: max_response default setting: maximize response for latest month
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = "all", # Default to "all"
  # total_budget = NULL, # When NULL, default is total spend in date_range
  channel_constr_low = 0.7,
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  # channel_constr_multiplier = 3,
  scenario = "max_response",
  export = TRUE
)
# Print & plot allocator's output
print(AllocatorCollect1)
#plot(AllocatorCollect1)

# Example 2: maximize response for latest 10 periods with given spend
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_10", # Last 10 periods, same as c("2018-10-22", "2018-12-31")
  total_budget = 5000000, # Total budget for date_range period simulation
  channel_constr_low = c(0.8, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 5, # Customise bound extension for wider insights
  scenario = "max_response",
  export = TRUE
)
print(AllocatorCollect2)
#plot(AllocatorCollect2)

# Scenario "target_efficiency": "How much to spend to hit ROAS or CPA of x?"
# Example 3: Use default ROAS target for revenue or CPA target for conversion
# Check InputCollect$dep_var_type for revenue or conversion type
# Two default ROAS targets: 0.8x of initial ROAS as well as ROAS = 1
# Two default CPA targets: 1.2x and 2.4x of the initial CPA
AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = NULL, # Default last month as initial period
  scenario = "target_efficiency",
  # target_value = 2, # Customize target ROAS or CPA value
  export = TRUE
)
print(AllocatorCollect3)
#plot(AllocatorCollect3)
```

# 5- Resultados finais

## Diagnóstico geral

![](robyn_mmm/Robyn_202406100118_init/1_451_2.png)

## Alocação de budget (Forecast)

![](robyn_mmm/Robyn_202406072036_init/1_451_2_reallocated_best_roas.png)
