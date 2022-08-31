library(dplyr)
library(plotly)
library(knitr)
library(kableExtra)
library(overdisp)
library(glmmTMB)
library(tibble)
library(forcats)

### dados violencia
load(file = "violencia_novo.RData")

#Visualização das observações e das  especificações 
#referentes às variáveis da base de dados
glimpse(violencia) 

#Estatísticas descritivas univariadas e tabela de frequências
summary(violencia)

#Exploração visual da violência (todo tipo de denúncia) no de 09/2021 a 06/2022
df_m  %>%
  ggplot() +
  geom_line(aes(x = month, y = n, 
                group = 1, color = "Total de denúncias por mês"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Mês",
       y = "Denúncias") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Exploração visual dos contratos com pendência no período de 09/2021 a 06/2022
df_m  %>%
  ggplot() +
  geom_line(aes(x = month, y = endiv, 
                group = 1, color = "Percentual de contratos com pendência"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Mês",
       y = "Percentual") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Histograma da variável dependente
ggplotly(
  violencia %>%
    ggplot(aes(x = denuncias,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(violencia) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Registros de violência ligue 180",
         y = "Frequência") +
    theme_bw()
)

#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'denuncias'
violencia %>%
  summarise(Média = mean(denuncias),
            Variância = var(denuncias)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)


#CAMERON, A. C.; TRIVEDI, P. K. Regression-based tests for overdispersion in
#the Poisson model. Journal of Econometrics, v. 46, n. 3, p. 347-364, 1990.
overdisp(x = violencia,
         dependent.position = 3,
         predictor.position = 4:9)


#Estimação do modelo Binomial Negativo pela função glmmTMB do pacote glmmTMB
modelo_nbm <- glmmTMB(formula = denuncias ~ endiv + renda_media + 
                        ln_obitos_covid + (endiv + ln_obitos_covid | UF),
                        family = nbinom2,
                        data = violencia)
summary(modelo_nbm)
logLik(modelo_nbm)


#Para observarmos graficamente o comportamento dos valores de v0j
ranef(modelo_nbm)[["cond"]][["UF"]] %>% 
  rownames_to_column("UF") %>% 
  rename(v0j = 2) %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(UF) %>% 
  ggplot(aes(label = round(v0j, digits = 3), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(UF), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = UF, y = 0), size = 3.1, color = "black") +
  coord_flip() +
  labs(x = "UF",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("darkorchid","orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

