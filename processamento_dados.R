# nolint start

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

dados <- read.csv2('dados\\dit_bruto.csv') %>% 

    mutate(renda = replace_na(renda, median(renda, na.rm = TRUE))) %>%
    
    mutate(quantidadeFilhos = if_else(quantidadeFilhos >= 3, '3 ou mais', as.character(quantidadeFilhos)))

uso_profissao <- dados %>% count(profissao, usoNuso) %>% 

    pivot_wider(names_from = usoNuso, values_from = n) %>% 

    rename(`não uso` = `0`, uso = `1`) %>%

    select(-`não uso`) %>% mutate(uso = if_else(is.na(uso), 0, uso))

cluster_profissao <- kmeans(uso_profissao %>% filter(uso != 0) %>% pull(uso), centers = 8, nstart = 10000)

uso_profissao <- uso_profissao %>% filter(uso != 0) %>% 

    mutate(cluster = chartr('12345678', 'BCDEFGHI', cluster_profissao$cluster)) %>% 
    
    merge(uso_profissao, all.y = TRUE) %>% 
    
    mutate(cluster = if_else(is.na(cluster), 'A', cluster)) %>% 
    
    arrange(desc(uso))

uso_profissao %>% write.csv2('dados\\dit_cluster_profissao.csv', row.names = FALSE)

dados <- dados %>% merge(uso_profissao %>% select(-uso), by = 'profissao') %>%
    
    select(-profissao) %>% rename(profissao = cluster)

dados %>% write.csv2('dados\\dit_processado.csv', row.names = FALSE)

# nolint end