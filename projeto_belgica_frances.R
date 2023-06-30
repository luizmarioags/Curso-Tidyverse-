library(tidyverse)
library(basedosdados)
library(scales)
#Definindo caminho 
caminho <- "caminho_para_salvar_os_graficos"
#Criando dataset
# Defina o seu projeto no Google Cloud
set_billing_id("penn-world-table-bd")

# Para carregar o dado direto no R
query <- bdplyr("nl_ug_pwt.microdados")
df <- bd_collect(query)

#Filtrando para mostrar a Bélgica e outros países francofonos 
df_francofonos <- df %>% 
  filter(country_id_iso_3 %in% c("FRA", "CAN", "BEL", "CHE", "TGO", "SEN","LUX")) %>%
  mutate(pib_per_capita = real_gdp_output / population) %>%
  mutate(tx_desemprego = ((population - employees) / population) * 100) %>%
  mutate(importacoes_pct = sum(current_share_m) / sum(real_gdp_na) * 100,
         exportacoes_pct = sum(current_share_x) / sum(real_gdp_na) * 100,
         pib_pct = 100 - exportacoes_pct)
#Filtrando para mostrar outros países europeus 
df_euro <- df %>% 
  filter(country_id_iso_3 %in% c("FRA", "BEL", "CHE", "DEU", "ESP","LUX","PRT","ITA","AUT","NLD")) %>%
  mutate(pib_per_capita = real_gdp_output / population) %>%
  mutate(tx_desemprego = ((population - employees) / population) * 100) %>%
  mutate(importacoes_pct = sum(current_share_m) / sum(real_gdp_na) * 100,
         exportacoes_pct = sum(current_share_x) / sum(real_gdp_na) * 100,
         pib_pct = 100 - exportacoes_pct)

#Gráfico comparado da Bélgica com outros países francofonos

# Lista de cores correspondentes a cada país
cores_paises_fr <- c("France" = "blue", "Canada" = "red", "Belgium" = "green", "Switzerland" = "orange", "Togo" = "purple", "Senegal" = "black", "Luxembourg" = "orchid")
#Gráfico comparado da Bélgica com outros países francofonos 
plot_2_eco_fr <- ggplot(df_francofonos, aes(x = year, y = pib_per_capita, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises_fr) +
  labs(title = 'Belgique en face de otre pays francophones - PIB per Capita', x = 'Ans', y = 'PIB per Capita') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_PIB_fr.jpg"),
  plot = plot_2_eco_fr,
  width = 10,
  height = 6,
  dpi = 300
)

#Pessoas Empregadas 
plot_3_eco_fr <- ggplot(df_francofonos, aes(x = year, y = employees, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises_fr) +
  labs(title = 'Belgique en face de otre pays francophones - Personnes Employées', x = 'Ans', y = 'Personnes Employées') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_employees_fr.jpg"),
  plot = plot_3_eco_fr,
  width = 10,
  height = 6,
  dpi = 300
)
#Taxa de Desemprego 
plot_4_eco_fr <- ggplot(df_francofonos, aes(x = year, y = tx_desemprego, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises_fr) +
  labs(title = 'Belgique en face de otre pays francophones - Taux de Chômage', x = 'Ans', y = 'Taux de Chômage (%)') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_desemprego_fr.jpg"),
  plot = plot_4_eco_fr,
  width = 10,
  height = 6,
  dpi = 300
)
#Horas Trabalhadas 
plot_5_eco_fr <- ggplot(df_francofonos, aes(x = year, y = average_hours_worked, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises_fr) +
  expand_limits(y = 0) + 
  labs(title ="Belgique en face de otre pays francophones - Heures travaillées", x = 'Ans', y = 'Heures travaillées
(Moyen)') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_hrs_fr.jpg"),
  plot = plot_5_eco_fr,
  width = 10,
  height = 6,
  dpi = 300
)
#População 
plot_6_eco_fr <- ggplot(df_francofonos, aes(x = year, y = population, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises_fr) +
  expand_limits(y = 0) + 
  labs(title = 'Belgique en face de otre pays francophones - Population', x = 'Ans', y = 'Population') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_pop_fr.jpg"),
  plot = plot_6_eco_fr,
  width = 10,
  height = 6,
  dpi = 300
)
#Bélgica e Países Europeus 

paises_UE <- c("FRA", "BEL", "CHE", "DEU", "ESP","LUX","PRT","ITA","AUT","NLD")
cores_paises <- c("#0000FF", "#FF6103", "#FF0000", "#006400", "#FF1493", "#68228B", "#FFB90F", "#8B2252",
                  "#00CDCD", "#104E8B", "#030303", "#8B3A62", "#FF00FF", "#C71585", "#8E388E", "#A0522D",
                  "#808A87", "#6B8E23")
#PIB per Capita
plot_eco_1_ue <- ggplot(df_euro, aes(x = year, y = pib_per_capita, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face de otre pays d'Europe - PIB per Capita" , x = 'Ans', y = 'PIB per Capita') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_pib_ue.jpg"),
  plot = plot_eco_1_ue,
  width = 10,
  height = 6,
  dpi = 300
)
#Pessoas Empregadas 
plot_eco_2_ue <-ggplot(df_euro, aes(x = year, y = employees, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises) +
  labs(title ="Belgique en face de otre pays d'Europe - Personnes Employées", x = 'Ans', y = 'Personnes Employées') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_employees_ue.jpg"),
  plot = plot_eco_2_ue,
  width = 10,
  height = 6,
  dpi = 300
)
#Taxa de Desemprego 
plot_eco_3_ue <-ggplot(df_euro, aes(x = year, y = tx_desemprego, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises) +
  labs(title ="Belgique en face de otre pays d'Europe - Taux de Chômage", x = 'Ans', y = 'Taux de Chômage(%)') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_desemprego_ue.jpg"),
  plot = plot_eco_3_ue,
  width = 10,
  height = 6,
  dpi = 300
)
#Horas Trabalhadas 
plot_eco_4_ue <-ggplot(df_euro, aes(x = year, y = average_hours_worked, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises) +
  labs(title ="Belgique en face de otre pays d'Europe - Heures travaillées", x = 'Ans', y = 'Heures travaillées
(Moyen)') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_hrs_ue.jpg"),
  plot = plot_eco_4_ue,
  width = 10,
  height = 6,
  dpi = 300
)
#População 
plot_eco_5_ue <-ggplot(df_euro, aes(x = year, y = population, color = country)) +
  geom_line(size = 1.0) + 
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face de otre pays d'Europe - Population", x = 'Ans', y = 'Population') + 
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 1,
           label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_pop_ue.jpg"),
  plot = plot_eco_5_ue,
  width = 10,
  height = 6,
  dpi = 300
)
#Consumo de Álcool 
df_alcool_total = read_csv("caminho_do_arquivo")
df_alcool_cerveja = read_csv("caminho_do_arquivo")
df_alcool_vinho = read_csv("caminho_do_arquivo")
#Filtrar Dataframes de Álcool para UE 
df_alcool_total_UE <- df_alcool_total %>%
  filter(Code %in% c("FRA", "BEL", "CHE", "DEU", "ESP","LUX","PRT","ITA","AUT","NLD")) %>% 
  rename(`Consumption per Capita` = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`)



view(df_alcool_total_UE)
#Cerveja 
df_alcool_cerveja_UE <- df_alcool_cerveja %>%
  filter(Code %in% c("FRA", "BEL", "CHE", "DEU", "ESP","LUX","PRT","ITA","AUT","NLD")) %>% 
  rename(`Consumption per Capita` = `Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Beer`)
#Vinho 
df_alcool_vinho_UE <- df_alcool_vinho %>%
  filter(Code %in% c("FRA", "BEL", "CHE", "DEU", "ESP","LUX","PRT","ITA","AUT","NLD")) %>% 
  rename(`Consumption per Capita` = `Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Wine`)
names(df_alcool_total_UE)
#Gráficos 
#Definindo paleta de cores 
paises_UE <- c("FRA", "BEL", "CHE", "DEU", "ESP","LUX","PRT","ITA","AUT","NLD")
cores_paises <- c("#0000FF", "#FF6103", "#FF0000", "#006400", "#FF1493", "#68228B", "#FFB90F", "#8B2252",
                  "#00CDCD", "#104E8B", "#030303", "#8B3A62", "#FF00FF", "#C71585", "#8E388E", "#A0522D",
                  "#808A87", "#6B8E23")
#Grafico
plot <- ggplot(df_alcool_total_UE, aes(x = Year, y = `Consumption per Capita`, color = Entity)) +
  geom_line(size = 1.0) +
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face des autres pays d'Europe", x = "Année", y = "Consommation d'alcool (Litres)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Source: Our World in Data", size = 4) 
ggsave(
  filename = paste0(caminho, "grafico_total_alcool_ue.jpg"),
  plot = plot,
  width = 10,
  height = 6,
  dpi = 300
)
  
#Cerveja
plot <- ggplot(df_alcool_cerveja_UE, aes(x = Year, y = `Consumption per Capita`, color = Entity)) +
  geom_line(size = 1.0) +
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face des autres pays d'Europe", x = "Année", y = "Consommation de Bière (Litres)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Source: Our World in Data", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_cerveja_ue.jpg"),
  plot = plot,
  width = 10,
  height = 6,
  dpi = 300
)


#Vinho 
plot <- ggplot(df_alcool_vinho_UE, aes(x = Year, y = `Consumption per Capita`, color = Entity)) +
  geom_line(size = 1.0) +
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face des autres pays d'Europe", x = "Année", y = "Consommation de Vin (Litres)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Source: Our World in Data", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_vinho_alcool_ue.jpg"),
  plot = plot,
  width = 10,
  height = 6,
  dpi = 300
)
#Países Francófonos 
df_alcool_total_fr <-df_alcool_total %>%
  filter(Code %in% c("FRA", "CAN", "BEL", "CHE", "TGO", "SEN","LUX")) %>% 
  rename(`Consumption per Capita` = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`)
df_cerveja_fr <- df_alcool_cerveja %>%
  filter(Code %in% c("FRA", "CAN", "BEL", "CHE", "TGO", "SEN","LUX")) %>% 
  rename(`Consumption per Capita` = `Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Beer`)
df_vinho_fr <- df_alcool_vinho %>%
  filter(Code %in% c("FRA", "CAN", "BEL", "CHE", "TGO", "SEN","LUX")) %>% 
  rename(`Consumption per Capita` = `Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Wine`)
#Grafico
cores_paises_fr <- c("France" = "blue", "Canada" = "red", "Belgium" = "green", "Switzerland" = "orange", "Togo" = "purple", "Senegal" = "black", "Luxembourg" = "orchid")
plot_1_fr <- ggplot(df_alcool_total_fr, aes(x = Year, y = `Consumption per Capita`, color = Entity)) +
  geom_line(size = 1.0) +
  scale_color_manual(values = cores_paises_fr) +
  labs(title = "Belgique en face des autres pays francophones", x = "Année", y = "Consommation d'alcool (Litres)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Source: PWT", size = 4) 
ggsave(
  filename = paste0(caminho, "grafico_total_alcool_fr.jpg"),
  plot = plot_1_fr,
  width = 10,
  height = 6,
  dpi = 300
)

#Cerveja
plot_2_fr <- ggplot(df_cerveja_fr, aes(x = Year, y = `Consumption per Capita`, color = Entity)) +
  geom_line(size = 1.0) +
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face des autres pays francophones", x = "Année", y = "Consommation de Bière (Litres)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_cerveja_fr.jpg"),
  plot = plot_2_fr,
  width = 10,
  height = 6,
  dpi = 300
)
#Vinho 
plot_3_fr <- ggplot(df_vinho_fr, aes(x = Year, y = `Consumption per Capita`, color = Entity)) +
  geom_line(size = 1.0) +
  scale_color_manual(values = cores_paises) +
  labs(title = "Belgique en face des autres pays francophones", x = "Année", y = "Consommation de Vin (Litres)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Source: PWT", size = 4)
ggsave(
  filename = paste0(caminho, "grafico_vinho_alcool_fr.jpg"),
  plot = plot_3_fr,
  width = 10,
  height = 6,
  dpi = 300
)

valores <- paste("Ano: ", df_belgium$year, ", Importações: ", df_belgium$importacoes_pct, "%, Exportações: ", df_belgium$exportacoes_pct, "%", df_belgium$real_gdp_na,"PIB")

# Imprimir a lista de valores
print(valores)
