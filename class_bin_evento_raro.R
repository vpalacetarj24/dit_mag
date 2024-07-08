# nolint start

library(dplyr, warn.conflicts = FALSE)
library(cvTools, warn.conflicts = FALSE)
library(tidymodels, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

ClassBinEventoRaro <- setRefClass(

    Class = 'ClassBinEventoRaro',

    fields = list(

        dados = 'data.frame',
        resp = 'character',
        treino = 'data.frame',
        X_teste = 'data.frame',
        Y_teste = 'vector',
        resultados_va = 'data.frame',
        melhor_ajuste = 'glm',
        resultados_teste = 'data.frame'

    ), methods = list(

        particao_endogena = function(estratos, percent_casos){

            dados <<- dados %>% count(!!!syms(estratos)) %>%

                mutate(pesos = prop.table(n)) %>% select(-n) %>%

                merge(dados, ., by = estratos)

            treino_casos <- dados %>% filter(!!sym(resp) == 1) %>%

                slice_sample(prop = percent_casos)

            treino_controles <- dados %>% filter(!!sym(resp) == 0) %>%

                slice_sample(n = nrow(treino_casos), weight_by = pesos)

            treino <<- rbind(treino_casos, treino_controles) %>% slice_sample(prop = 1)

            X_teste <<- dados %>% slice(-row_number(treino)) %>% select(-!!sym(resp))

            Y_teste <<- dados %>% slice(-row_number(treino)) %>% pull(!!sym(resp))

        }, selecao_va_5_nos = function(modelos, offset){

            auc_neg <- function(observado, prob_pred){

                auc <- data.frame(observado = as.factor(observado), prob_pred = prob_pred) %>% 
            
                    roc_auc(observado, prob_pred, event_level = 'second') %>% pull(.estimate)

                return(-auc)

            }

            if(!missing(offset)){

                dados <<- dados %>% mutate(offset = !!sym(offset))

                treino <<- treino %>% mutate(offset = !!sym(offset))

                X_teste <<- X_teste %>% mutate(offset = !!sym(offset))

                va_5_nos <- function(modelo){

                    ajuste_va <- cvFit(

                        glm, formula = modelo, family = binomial(), data = treino, 
                        weights = pesos^(-1), offset = offset,
                        cost = auc_neg, predictArgs = list(type = 'response')

                    )
                
                    return(ajuste_va)

                }

            } else {

                va_5_nos <- function(modelo){

                    ajuste_va <- cvFit(

                        glm, formula = modelo, family = binomial(), data = treino, weights = pesos^(-1),
                        cost = auc_neg, predictArgs = list(type = 'response')

                    )
                
                    return(ajuste_va)

                }

            }

            ajustes_va <- modelos %>% setNames(., .) %>%
            
                lapply(., va_5_nos) %>% do.call(cvSelect, .)

            resultados_va <<- ajustes_va$cv %>% rename(modelo = Fit, AUC = CV) %>%

                mutate(AUC = -AUC, modelo = as.character(modelo)) %>% arrange(desc(AUC))

            if(!missing(offset)){

                melhor_ajuste <<- resultados_va %>% slice(1) %>% pull(modelo) %>%

                    glm(formula = ., family = binomial(), offset = offset, data = dados)

            } else{
               
                melhor_ajuste <<- resultados_va %>% slice(1) %>% pull(modelo) %>%

                    glm(formula = ., family = binomial(), data = dados)

            }

        }, plota_curva_roc_teste = function(){

            dados_curva_roc <- data.frame(

                observado = as.factor(Y_teste), 
                prob_pred = predict(melhor_ajuste, X_teste, type = 'response')

            ) %>% roc_curve(observado, prob_pred, event_level = 'second')

            dados_curva_roc %>% plot_ly(

                x = ~ 1 - sensitivity, y = ~ specificity,
                text = ~ round(.threshold, 4), hoverinfo = 'text',
                mode = 'lines', type = 'scatter', line = list(color = 'blue')

            ) %>% add_segments(

                x = 0, xend = 1, y = 0, yend = 1,
                line = list(dash = 'dash', color = 'black')

            ) %>% layout(

                showlegend = FALSE,

                xaxis = list(

                    range = c(-0.05, 1.05), 
                    gridcolor = 'white', 
                    zeroline = FALSE, 
                    title = '1 - Sensibilidade'

                ), yaxis = list(

                    range = c(-0.05, 1.05), 
                    gridcolor = 'white', 
                    zeroline = FALSE, 
                    title = 'Especificidade'

                ), plot_bgcolor = 'lightgray'

            ) %>% toWebGL()

        }, tabela_metricas_teste = function(ponto_corte){

            prob_pred <- predict(melhor_ajuste, X_teste, type = 'response')

            dados_obs_pred <- data.frame(

                observado = as.factor(Y_teste), 
                prob_pred = prob_pred,
                pred = as.factor(if_else(prob_pred > ponto_corte, 1, 0))

            )

            resultados_teste <<- f_meas(dados_obs_pred, observado, pred, event_level = 'second') %>%

                rbind(recall(dados_obs_pred, observado, pred, event_level = 'second')) %>%

                rbind(precision(dados_obs_pred, observado, pred, event_level = 'second')) %>%

                rbind(roc_auc(dados_obs_pred, observado, prob_pred, event_level = 'second')) %>%

                select(-.estimator)

        }

    )

)

# nolint end