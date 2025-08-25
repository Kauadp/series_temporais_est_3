if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("forecast")) install.packages("forecast")

dados_treino <- read.csv("dados/treino.csv")

str(dados_treino)

dados_treino$date <- as.Date(dados_treino$date)

summary(dados_treino)

## FUNÇÕES ##

criar_grafico_densidade <- function(dados, var, cor, x.title) {
  titulo <- paste("Densidade", x.title, sep = " ")
  dados |> 
    ggplot(aes(x = .data[[var]])) +
    geom_density(fill = cor, linewidth = .8, alpha = .7) +
    theme_fivethirtyeight() +
    labs(
      title = titulo,
      x = x.title,
      y = "Densidade"
    ) +
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text()
    )
}

## ANÁLISES ##

cores <- c("#780000","#c1121f","#fdf0d5","#003049","#669bbc")

g1 <- criar_grafico_densidade(dados_treino, names(dados_treino)[2], cores[1], "Temperatura Média (ºC)")
g2 <- criar_grafico_densidade(dados_treino, names(dados_treino)[2], cores[2], "Humidade")
g3 <- criar_grafico_densidade(dados_treino, names(dados_treino)[3], cores[3], "Velocidade do Vento (Km/h)")
g4 <- criar_grafico_densidade(dados_treino, names(dados_treino)[4], cores[4], "Pressão Média (atm)")
grid.arrange(g1,g2,g3,g4)

dados_treino |> 
  ggplot(aes(x = date, y = meantemp)) +
  geom_line(linewidth = .8) +
  geom_point(size = .5) +
  theme_fivethirtyeight() +
  labs(
    title = "Análise da Temperatura Média Durante os Anos",
    subtitle = "Existe uma Sazonalidade Clara",
    x = "Anos",
    y = "Temperatura Média (ºC)"
  )

dados_treino |> 
  filter(date <= "2014-01-01") |> 
  ggplot(aes(x = date, y = meantemp)) +
  geom_line(linewidth = .8) +
  geom_point(size = .5) +
  theme_fivethirtyeight() +
  labs(
    title = "Zoom na Data de 2013 Durante os Meses",
    subtitle = "",
    x = "Anos",
    y = "Temperatura Média (ºC)"
  )


temperatura_ts <- ts(dados_treino$meantemp, frequency = 365.25, start = c(2013, 1))

decomposicao <- decompose(temperatura_ts, type = "additive")
plot(decomposicao)

antes <- dados_treino |> 
  filter(date <= "2015-07-01") |> 
  select(meantemp)
depois <- dados_treino |> 
  filter(date > "2015-07-01") |> 
  select(meantemp)

mean(antes$meantemp)
mean(depois$meantemp)
mean(depois$meantemp)/mean(antes$meantemp)*100

wilcox.test(antes$meantemp,depois$meantemp)

mean(decomposicao$random, na.rm = TRUE)

## MODELANDO ##

modelo_sarima <- auto.arima(temperatura_ts)
summary(modelo_sarima)

modelo_ets <- ets(temperatura_ts)
summary(modelo_ets)


previsao_sarima_30d <- forecast(modelo_sarima, h = 30)

previsao_ets_30d <- forecast(modelo_ets, h = 30)

dados_previsao <- as.data.frame(previsao_sarima_30d)

dados_previsao <- dados_previsao |> 
  rownames_to_column(var = "data")

dados_previsao$data <- as.Date(date_decimal(as.numeric(dados_previsao$data)))

p1 <- ggplot() +
  geom_line(data = dados_treino, aes(x = date, y = meantemp), linewidth = 0.8) +
  geom_point(data = dados_treino, aes(x = date, y = meantemp), size = 0.5) +
  # Intervalo de confiança (sombreado)
  geom_ribbon(data = dados_previsao, aes(x = data, ymin = `Lo 80`, ymax = `Hi 80`), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = dados_previsao, aes(x = data, ymin = `Lo 95`, ymax = `Hi 95`), fill = "blue", alpha = 0.1) +
  # Linha da previsão
  geom_line(data = dados_previsao, aes(x = data, y = `Point Forecast`), color = "blue", linewidth = 1) +
  theme_fivethirtyeight() +
  labs(
    title = "Previsão de Temperatura Média com SARIMA",
    subtitle = "Previsão de 30 dias",
    x = "Data",
    y = "Temperatura Média (ºC)"
  ) +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text()
  )

dados_previsao_ets <- as.data.frame(previsao_ets_30d)

dados_previsao_ets <- dados_previsao_ets |> 
  rownames_to_column(var = "data")

dados_previsao_ets$data <- as.Date(date_decimal(as.numeric(dados_previsao_ets$data)))

p2 <- ggplot() +
  geom_line(data = dados_treino, aes(x = date, y = meantemp), linewidth = 0.8) +
  geom_point(data = dados_treino, aes(x = date, y = meantemp), size = 0.5) +
  # Intervalo de confiança (sombreado)
  geom_ribbon(data = dados_previsao_ets, aes(x = data, ymin = `Lo 80`, ymax = `Hi 80`), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = dados_previsao_ets, aes(x = data, ymin = `Lo 95`, ymax = `Hi 95`), fill = "blue", alpha = 0.1) +
  # Linha da previsão
  geom_line(data = dados_previsao_ets, aes(x = data, y = `Point Forecast`), color = "blue", linewidth = 1) +
  theme_fivethirtyeight() +
  labs(
    title = "Previsão de Temperatura Média com ETS",
    subtitle = "Previsão de 30 dias",
    x = "Data",
    y = "Temperatura Média (ºC)"
  ) +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text()
  )
grid.arrange(p1,p2)

previsao_sarima_2y <- forecast(modelo_sarima, h = 365)

dados_previsao_sarima_2y <- as.data.frame(previsao_sarima_2y)

dados_previsao_sarima_2y <- dados_previsao_sarima_2y |> 
  rownames_to_column(var = "data")

dados_previsao_sarima_2y$data <- as.Date(date_decimal(as.numeric(dados_previsao_sarima_2y$data)))

ggplot() +
  geom_line(data = dados_treino, aes(x = date, y = meantemp), linewidth = 0.8) +
  geom_point(data = dados_treino, aes(x = date, y = meantemp), size = 0.5) +
  # Intervalo de confiança (sombreado)
  geom_ribbon(data = dados_previsao_sarima_2y, aes(x = data, ymin = `Lo 80`, ymax = `Hi 80`), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = dados_previsao_sarima_2y, aes(x = data, ymin = `Lo 95`, ymax = `Hi 95`), fill = "blue", alpha = 0.1) +
  # Linha da previsão
  geom_line(data = dados_previsao_sarima_2y, aes(x = data, y = `Point Forecast`), color = "blue", linewidth = 1) +
  theme_fivethirtyeight() +
  labs(
    title = "Previsão de Temperatura Média com SARIMA",
    subtitle = "Previsão de 1 ano (padrão sazonal mantido)",
    x = "Data",
    y = "Temperatura Média (ºC)"
  ) +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text()
  )


temperatura_subconjunto <- window(temperatura_ts, start = c(2016, 1))

f_sarima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

erros_sarima <- tsCV(temperatura_subconjunto, f_sarima, h = 1)

rmse_sarima_subconjunto <- sqrt(mean(erros_sarima^2, na.rm = TRUE))

print(paste("RMSE (Validação Cruzada - Subconjunto):", round(rmse_sarima_subconjunto, 4)))

dados_teste <- read.csv("dados/test.csv")
dados_teste$date <- as.Date(dados_teste$date)

dados_teste |> 
  ggplot(aes(x = date, y = meantemp)) +
  geom_line(linewidth = .8) +
  geom_point(size = .5) +
  theme_fivethirtyeight() +
  labs(
    title = "Análise da Temperatura Média nos Dados de Teste",
    subtitle = "Mesmo Padrão Notado Anteriormente",
    x = "Meses",
    y = "Temperatura Média (ºC)"
  )

str(dados_teste)

n_teste <- length(dados_teste$meantemp)

previsao_teste <- forecast(modelo_sarima, h = n_teste)

accuracy(previsao_teste, dados_teste$meantemp)

temperatura_completa <- c(as.numeric(temperatura_ts), as.numeric(dados_teste$meantemp))

n_total <- length(temperatura_completa)
nova_ts <- ts(temperatura_completa, start = c(2013, 1), frequency = 365.25)

modelo_sarima_reajustado <- auto.arima(nova_ts)

dias_restantes <- 250  
previsao_final <- forecast(modelo_sarima_reajustado, h = dias_restantes)

dados_plot <- data.frame(
  data = as.Date(date_decimal(as.numeric(time(nova_ts)))),
  temperatura_observada = as.numeric(nova_ts)
)

# Converta a previsão final para um data frame
previsao_plot <- as.data.frame(previsao_final) |> 
  rownames_to_column(var = "data_col")

previsao_plot$data_col <- as.Date(date_decimal(as.numeric(previsao_plot$data_col)))

ggplot() +
  geom_line(data = dados_plot, aes(x = data, y = temperatura_observada), linewidth = 0.8, color = "black") +
  geom_point(data = dados_plot, aes(x = data, y = temperatura_observada), size = 0.5, color = "black") +
  geom_ribbon(data = previsao_plot, aes(x = data_col, ymin = `Lo 80`, ymax = `Hi 80`), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = previsao_plot, aes(x = data_col, ymin = `Lo 95`, ymax = `Hi 95`), fill = "blue", alpha = 0.1) +
  geom_line(data = previsao_plot, aes(x = data_col, y = `Point Forecast`), color = "blue", linewidth = 1) +
  theme_fivethirtyeight() +
  labs(
    title = "Previsão Final de Temperatura com SARIMA",
    subtitle = "Série Histórica e Previsão para o Final de 2017",
    x = "Data",
    y = "Temperatura Média (ºC)"
  ) +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text()
  )

previsao_plot |> 
  filter(data_col >= "2017-06-20" & data_col <= "2017-09-22") |> 
  select(`Point Forecast`) |> 
  summarise(
    mean(`Point Forecast`)
  )

dados_2017 <- dados_plot |>
  filter(data >= "2017-01-01")

media_total <- mean(c(dados_2017$temperatura_observada, previsao_plot$`Point Forecast`))


ggplot() +
  geom_line(data = dados_plot |> 
              filter(data >= "2017-01-01"),
            aes(x = data, y = temperatura_observada), linewidth = 0.8, color = "black") +
  geom_point(data = dados_plot |> 
               filter(data >= "2017-01-01"),
             aes(x = data, y = temperatura_observada), size = 0.5, color = "black") +
  geom_ribbon(data = previsao_plot, aes(x = data_col, ymin = `Lo 80`, ymax = `Hi 80`), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = previsao_plot, aes(x = data_col, ymin = `Lo 95`, ymax = `Hi 95`), fill = "blue", alpha = 0.1) +
  geom_line(data = previsao_plot, aes(x = data_col, y = `Point Forecast`), color = "blue", linewidth = 1) +
  geom_hline(aes(yintercept = media_total), linetype = "dashed", color = "red", linewidth = 1) +
  theme_fivethirtyeight() +
  labs(
    title = "Zoom Previsão Final de Temperatura com SARIMA",
    subtitle = "Zoom na Série Histórica e Previsão para o Final de 2017",
    x = "Data",
    y = "Temperatura Média (ºC)"
  ) +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text()
  ) +
  ylim(c(0,50))

