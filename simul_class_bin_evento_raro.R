# nolint start

library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
source('class_bin_evento_raro.R')

dados <- read.csv2('simul\\dados_simul.csv') %>%

    mutate(cat_covar_continua = cut(covar_continua, 10)) %>%

    mutate(cat_covar_discreta = cut(covar_discreta, 10))

estratos <- c('cat_covar_continua', 'cat_covar_discreta', 'covar_binaria')

teste_class_bin_evento_raro <- ClassBinEventoRaro(dados = dados, resp = 'resposta')

teste_class_bin_evento_raro$particao_endogena(estratos = estratos, percent_casos = 0.95)

modelos <- c()

modelos <- append(modelos, 'resposta ~ covar_continua')

modelos <- append(modelos, 'resposta ~ covar_continua + covar_discreta')

modelos <- append(modelos, 'resposta ~ covar_continua:covar_binaria + covar_discreta')

modelos <- append(modelos, 'resposta ~ covar_continua + covar_discreta:covar_binaria')

teste_class_bin_evento_raro$selecao_va_5_nos(modelos = modelos)

teste_class_bin_evento_raro$resultados_va %>% View(., 'Resultados da validação cruzada')

teste_class_bin_evento_raro$plota_curva_roc_teste()

teste_class_bin_evento_raro$tabela_metricas_teste(ponto_corte = 0.32)

teste_class_bin_evento_raro$resultados_teste %>% View(., 'Resultados do conjunto de teste')

# nolint end