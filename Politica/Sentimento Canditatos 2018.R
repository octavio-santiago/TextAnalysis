library(Rfacebook) # usado para extrair dados do facebook
library(tidyverse) # pq nao da pra viver sem
library(ggExtra)
library(magrittr) # <3
library(lubridate)
library(stringr) # essencial para trabalhar com textos
library(tidytext) # um dos melhores pacotes para text mining
library(lexiconPT)

bolsonaro <- read.delim("~/Desktop/IA/Text/Politica/2018/Bolsonaro/bolsonaro.txt", header=FALSE)
haddad <- read.delim("~/Desktop/IA/Text/Politica/2018/Haddad/haddad.txt", header=FALSE)

#grupo1 <- separate(Conversa.do.WhatsApp,V1,c("Timestamp","Message"),sep="-",extra="drop")

#grupo2 <- separate(grupo1,Message,c("Nome","Message"),sep=":",extra="drop")

#write.table(grupo2$Message, file = "mwsvg.txt", sep = "\t",row.names = FALSE, col.names = FALSE)

# carregar datasets
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

glimpse(op30)

#preparar para analise de sentimento
#frases <- grupo2$Message
#pessoas <- grupo2$Nome

#criar dataframe
#mensagensVG <- data.frame("Nome"=pessoas,"Frase"=frases)

# remover NAs 
bolsonaro <- bolsonaro %>% filter(!is.na(bolsonaro$V1))
haddad <- haddad %>% filter(!is.na(haddad$V1))
#tirar as virgulas
bolsonaro$V1 <- str_replace_all(",", "")
haddad$V1 <- str_replace_all(",", "")

# criar ID unica para cada comentario
bolsonaro <- bolsonaro %>% mutate(col_id = row_number())
haddad <- haddad %>% mutate(col_id = row_number())

#converter de factor para char vector
bolsonaro$V1 <- as.character(bolsonaro$V1)
haddad$V1 <- as.character(haddad$V1)

# usar funçao do tidytext para criar uma linha para cada palavra de um comentario
bolsonaro_unnested <- bolsonaro %>% unnest_tokens(term,V1)
haddad_unnested <- haddad %>% unnest_tokens(term,V1)
# visualizar o resultado
bolsonaro_unnested %>%
  select(col_id, term) %>%
  head(20)
haddad_unnested %>%
  select(col_id, term) %>%
  head(20)

#quantificar o sentimento das frases
bolsonaro_unnested %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(col_id, term, polarity, lex_polarity) %>% 
  head(10)
haddad_unnested %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(col_id, term, polarity, lex_polarity) %>% 
  head(10)

#manter apenas as que tem polaridade nas duas livrarias
bolsonaro_unnested <- bolsonaro_unnested %>% 
  inner_join(op30, by = "term") %>% 
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(col_id) %>% 
  summarise(
    comment_sentiment_op = sum(polarity),
    comment_sentiment_lex = sum(lex_polarity),
    n_words = n()
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    most_neg = min(comment_sentiment_lex, comment_sentiment_op),
    most_pos = max(comment_sentiment_lex, comment_sentiment_op)
  )

head(bolsonaro_unnested)

haddad_unnested <- haddad_unnested %>% 
  inner_join(op30, by = "term") %>% 
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(col_id) %>% 
  summarise(
    comment_sentiment_op = sum(polarity),
    comment_sentiment_lex = sum(lex_polarity),
    n_words = n()
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    most_neg = min(comment_sentiment_lex, comment_sentiment_op),
    most_pos = max(comment_sentiment_lex, comment_sentiment_op)
  )

head(haddad_unnested)

#detectar outliers
g <- bolsonaro_unnested %>% 
  ggplot(aes(x = comment_sentiment_op, y = comment_sentiment_lex)) +
  geom_point(aes(color = n_words)) + 
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")

g

p <- haddad_unnested %>% 
  ggplot(aes(x = comment_sentiment_op, y = comment_sentiment_lex)) +
  geom_point(aes(color = n_words)) + 
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")

p

#detectar frases mais positivas e mais negativas
bolsonaro_unnested %<>% filter(between(comment_sentiment_op, -10, 10))
haddad_unnested %<>% filter(between(comment_sentiment_op, -10, 10))

# comentario mais positivo e mais negativo da campanha
most_posB <- which.max(bolsonaro_unnested$most_pos)
most_negB <- which.min(bolsonaro_unnested$most_neg)
most_posH <- which.max(bolsonaro_unnested$most_pos)
most_negH <- which.min(bolsonaro_unnested$most_neg)

# mais positivo e negativo Bolsonaro
cat(bolsonaro$V1[bolsonaro$col_id == bolsonaro_unnested$col_id[most_posB]])
cat(bolsonaro$V1[bolsonaro$col_id == bolsonaro_unnested$col_id[most_negB]])

# mais positivo e negativo Haddad
cat(haddad$V1[haddad$col_id == haddad_unnested$col_id[most_posH]])
cat(haddad$V1[haddad$col_id == haddad_unnested$col_id[most_negH]])

#usar somente o OPLexicon para analise de sentimento de acordo com o texto inteiro
bolsonaro %<>% inner_join(
  bolsonaro_unnested %>% select(col_id, sentiment = comment_sentiment_op),
  by = "col_id"
)
haddad %<>% inner_join(
  haddad_unnested %>% select(col_id, sentiment = comment_sentiment_op),
  by = "col_id"
)

bolsonaro_wide <- bolsonaro %>% 
  # filtrar fora palavras neutras
  filter(sentiment != 0) %>% 
  # converter numerico para categorico
  mutate(sentiment = ifelse(sentiment < 0, "negativo", "positivo")) %>% 
  # agrupar os dados
  count(col_id, sentiment) %>% 
  # converter para formato wide
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentimento = positivo - negativo) %>% 
  ungroup() %>% 
  arrange(col_id)

head(bolsonaro_wide) %>% knitr::kable()

bolsonaro_wide <- bolsonaro %>% 
  # filtrar fora palavras neutras
  filter(sentiment != 0) %>% 
  # converter numerico para categorico
  mutate(sentiment = ifelse(sentiment < 0, "negativo", "positivo")) %>% 
  # agrupar os dados
  count(col_id, sentiment) %>% 
  # converter para formato wide
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentimento = positivo - negativo) %>% 
  ungroup() %>% 
  arrange(col_id)

head(bolsonaro_wide) %>% knitr::kable()

haddad_wide <- haddad %>% 
  # filtrar fora palavras neutras
  filter(sentiment != 0) %>% 
  # converter numerico para categorico
  mutate(sentiment = ifelse(sentiment < 0, "negativo", "positivo")) %>% 
  # agrupar os dados
  count(col_id, sentiment) %>% 
  # converter para formato wide
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentimento = positivo - negativo) %>% 
  ungroup() %>% 
  arrange(col_id)

head(haddad_wide) %>% knitr::kable()


#plotar um grafico com os sentimentos de ambos no texto
bolsonaro_wide %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(x = index, y = sentimento)) +
  geom_col(aes(fill = col_id)) +
  scale_y_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2)) +
  labs(x = "Índice da Frases", y = "Sentimento",
       fill = NULL, title = "Evolução do sentimento no programa Bolsonaro")

haddad_wide %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(x = index, y = sentimento)) +
  geom_col(aes(fill = col_id)) +
  scale_y_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2)) +
  labs(x = "Índice da Frases", y = "Sentimento",
       fill = NULL, title = "Evolução do sentimento no programa Haddad")

#somar os sentimentos gerais dos dois programas
bolsoTot <- bolsonaro_wide %>% summarise_all(funs(sum))
hadTot <- haddad_wide %>% summarise_all(funs(sum))




# qual o sentimento mais associado a palavras em especifico
bolsonaro %>% 
  mutate(
    emprego = str_detect(str_to_lower(V1), "emprego"),
    salario = str_detect(str_to_lower(V1), "salário"),
    bolsa_familia = str_detect(str_to_lower(V1), "bolsa família"),
    familia = str_detect(str_to_lower(V1), "família"),
    pec95 = str_detect(str_to_lower(V1), "pec 95"),
    gastos = str_detect(str_to_lower(V1), "gastos"),
    brasil = str_detect(str_to_lower(V1), "brasil"),
    impostos = str_detect(str_to_lower(V1), "impostos"),
    diversidade = str_detect(str_to_lower(V1), "diversidade"),
    imprensa = str_detect(str_to_lower(V1), "imprensa"),
    lgbt = str_detect(str_to_lower(V1), "lgbt"),
    lavajato = str_detect(str_to_lower(V1), "lava jato"),
    aborto = str_detect(str_to_lower(V1), "aborto"),
    corrupcao = str_detect(str_to_lower(V1), "corrupção"),
    policia = str_detect(str_to_lower(V1), "policia"),
    cadeia = str_detect(str_to_lower(V1), "cadeia"),
    ir = str_detect(str_to_lower(V1), "imposto de renda"),
    saude= str_detect(str_to_lower(V1), "saúde"),
    educacao = str_detect(str_to_lower(V1), "educação"),
    economia = str_detect(str_to_lower(V1), "economia")
  ) %>% 
  gather(termo, eh_presente, emprego:economia) %>% 
  filter(eh_presente) %>% 
  group_by(termo) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  ggplot(aes(x = termo, y = sentiment)) + 
  geom_col(fill = "#C10534")

haddad %>% 
  mutate(
    emprego = str_detect(str_to_lower(V1), "emprego"),
    salario = str_detect(str_to_lower(V1), "salário"),
    bolsa_familia = str_detect(str_to_lower(V1), "bolsa família"),
    familia = str_detect(str_to_lower(V1), "família"),
    pec95 = str_detect(str_to_lower(V1), "pec 95"),
    gastos = str_detect(str_to_lower(V1), "gastos"),
    brasil = str_detect(str_to_lower(V1), "brasil"),
    impostos = str_detect(str_to_lower(V1), "impostos"),
    diversidade = str_detect(str_to_lower(V1), "diversidade"),
    imprensa = str_detect(str_to_lower(V1), "imprensa"),
    lgbt = str_detect(str_to_lower(V1), "lgbt"),
    lavajato = str_detect(str_to_lower(V1), "lava jato"),
    aborto = str_detect(str_to_lower(V1), "aborto"),
    corrupcao = str_detect(str_to_lower(V1), "corrupção"),
    policia = str_detect(str_to_lower(V1), "policia"),
    cadeia = str_detect(str_to_lower(V1), "cadeia"),
    ir = str_detect(str_to_lower(V1), "imposto de renda"),
    saude= str_detect(str_to_lower(V1), "saúde"),
    educacao = str_detect(str_to_lower(V1), "educação"),
    economia = str_detect(str_to_lower(V1), "economia")
  ) %>% 
  gather(termo, eh_presente, emprego:economia) %>% 
  filter(eh_presente) %>% 
  group_by(termo) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  ggplot(aes(x = termo, y = sentiment)) + 
  geom_col(fill = "#C10534")
