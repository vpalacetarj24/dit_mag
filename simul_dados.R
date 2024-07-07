# nolint start

library(dplyr, warn.conflicts = FALSE)

num_observacoes <- 100000

params <- c(-5, 3, -1)

dados_simul <- data.frame(

    covar_continua = rnorm(n = num_observacoes),
    covar_discreta = ceiling(rgamma(n = num_observacoes, shape = 40)),
    covar_binaria = rbinom(n = num_observacoes, size = 1, prob = 0.5)

)

X_desing <- model.matrix(

    object = ~ covar_continua + covar_binaria:covar_discreta, 
    data = dados_simul

)

prob_sucesso <- (1 + exp(-X_desing %*% params))^(-1)

resposta <- if_else(runif(n = num_observacoes) < prob_sucesso, 1, 0)

dados_simul <- dados_simul %>% mutate(resposta = resposta)

saveRDS(params, 'simul\\params.RDS')

write.csv2(dados_simul, 'simul\\dados_simul.csv', row.names = FALSE)

# nolint end