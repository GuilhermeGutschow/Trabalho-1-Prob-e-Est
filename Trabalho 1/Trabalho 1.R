#Importação dos dados:

X202309_soft_eng_jobs_pol <- read.csv("202309_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202310_soft_eng_jobs_pol <- read.csv("202310_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202311_soft_eng_jobs_pol <- read.csv("202311_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202312_soft_eng_jobs_pol <- read.csv("202312_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202401_soft_eng_jobs_pol <- read.csv("202401_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202402_soft_eng_jobs_pol <- read.csv("202402_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202403_soft_eng_jobs_pol <- read.csv("202403_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202404_soft_eng_jobs_pol <- read.csv("202404_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202405_soft_eng_jobs_pol <- read.csv("202405_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202406_soft_eng_jobs_pol <- read.csv("202406_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")
X202407_soft_eng_jobs_pol <- read.csv("202407_soft_eng_jobs_pol.csv", fileEncoding = "Latin1")

X202309_soft_eng_jobs_pol$ano = "0923"
X202310_soft_eng_jobs_pol$ano = "1023"
X202311_soft_eng_jobs_pol$ano = "1123"
X202312_soft_eng_jobs_pol$ano = "1223"
X202401_soft_eng_jobs_pol$ano = "0124"
X202402_soft_eng_jobs_pol$ano = "0224"
X202403_soft_eng_jobs_pol$ano = "0324"
X202404_soft_eng_jobs_pol$ano = "0424"
X202405_soft_eng_jobs_pol$ano = "0524"
X202406_soft_eng_jobs_pol$ano = "0624"
X202407_soft_eng_jobs_pol$ano = "0724"

dados = rbind(X202309_soft_eng_jobs_pol, X202310_soft_eng_jobs_pol,
              X202311_soft_eng_jobs_pol, X202312_soft_eng_jobs_pol,
              X202401_soft_eng_jobs_pol, X202402_soft_eng_jobs_pol,
              X202403_soft_eng_jobs_pol, X202404_soft_eng_jobs_pol,
              X202405_soft_eng_jobs_pol, X202406_soft_eng_jobs_pol,
              X202407_soft_eng_jobs_pol)
write.csv2(dados,"dados_final.csv",row.names=FALSE)

#Piechart tecnlogias:
# Preparação dos dados
library(dplyr)
library(plotly)

# Preparar os dados
contagem_tec <- dados %>%
  filter(!is.na(technology), technology != "") %>%
  count(technology) %>%
  mutate(percentual = 100 * n / sum(n)) %>%
  mutate(technology = ifelse(percentual < 2, "Outros", technology)) %>%
  group_by(technology) %>%
  summarise(n = sum(n), percentual = sum(percentual), .groups = "drop") %>%
  arrange(desc(percentual))

# Criar pie chart
fig <- plot_ly(
  contagem_tec,
  labels = ~technology,
  values = ~percentual,
  type = "pie",
  text = ~paste0(round(percentual, 1), "%"),  # só percentual dentro
  textinfo = "text",
  hovertemplate = paste(
    "<b>%{label}</b><br>",
    "Percentual: %{value:.1f}%<br>",
    "N: %{customdata}<extra></extra>"
  ),
  customdata = contagem_tec$n,  # valores brutos
  sort = FALSE,
  direction = "clockwise"
) %>%
  layout(
    title = list(text = "Distribuição de Tecnologias (%)", x = 0.5),
    legend = list(title = list(text = "<b>Tecnologias</b>"))
  )

fig






#Gráfico de barras de salmin e max porlibrary(dplyr)
library(ggplot2)
library(scales)

# Calcular médias e n
resumo_salario <- dados %>%
  filter(!is.na(technology), technology != "",
         !is.na(salary.employment.min),
         !is.na(salary.employment.max)) %>%
  group_by(technology) %>%
  summarise(
    salario_min_medio = mean(salary.employment.min, na.rm = TRUE),
    salario_max_medio = mean(salary.employment.max, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n)) %>% 
  mutate(technology = factor(technology, levels = unique(technology))) # ordenar pelo n

# Gráfico
ggplot(resumo_salario, aes(x = technology)) +
  # Barra adicional do mínimo até o máximo (mais escuro)
  geom_col(aes(y = salario_max_medio), fill = "#e76f51", width = 0.6) +
  # Barra até o salário mínimo (claro)
  geom_col(aes(y = salario_min_medio), fill = "#f4a261", width = 0.6) +
  # Texto com valor de n (apenas número)
  geom_text(aes(y = -2000, label = n), vjust = 1, size = 3) +
  labs(
    title = "Salário Médio (Mínimo e Máximo) por Tecnologia",
    x = "Tecnologia",
    y = "Salário Médio"
  ) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

