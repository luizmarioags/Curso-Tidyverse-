#Chamando as bibliotecas
library(ggplot2)
library(dplyr)
library(gapminder)
#Gr�ficos de Linha
#Criando Dataframe
por_ano_continente <- gapminder %>%
  group_by(continent,year) %>%
  summarize(meanLifeExp = mean(lifeExp))
#Para fazer um gr�fico de linha adicionamos geom_line()
#Fazemos essa adi��o no lugar de geom_point()
ggplot(por_ano_continente,aes(x = year, y = meanLifeExp, color = continent)) + 
  geom_line() + #Especifica como gr�fico de linha
  expand_limits(y = 0)

#Gr�fico de Barras
#Criando Dataframe
by_continent <- gapminder %>% 
  filter(year == 2007) %>% #filtra para 2007
  group_by(continent) %>% #agrupar por continente 
  summarize(meanLifeExp = mean(lifeExp),
            pop_total = sum(pop)) #soma a populacao

#Para fazer um gr�fico de barra adicionamos geom_col()
ggplot(by_continent, aes(x = continent, y = meanLifeExp)) +
  geom_col() #Especifica como gr�fico de barra

#Em gr�ficos de barras o nosso x sempre � a vari�vel categ�rica

#Histogramas

#Histogramas mostram a distribui��o de uma vari�vel 
#Cada barra do histograma representa um compartimento da vari�vel 
#A altura da barra representa quantas observa�oes existem dentro desse compartimento 
#Para fazer histogramas usamos o geom_histogram()
#O compartimento pode ser alterado usando o bin dentro do geom_histogram()
#Exemplo
gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Histograma da popula��o
ggplot(gapminder_1952, aes(x=pop_by_mil)) + 
  geom_histogram()

#Gr�fico de Boxplot 
#Exemplo 
#Crie o data set filtrando para o ano de 1952
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Fa�a o boxplot comparando o PIB per Capita por continente
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) + 
  geom_boxplot() + 
  scale_y_log10() #colocando o eixo y na escala log

#Adicionando t�tulo no gr�fico
# Para adicionar t�tulo usamos o labs(title ='')
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  labs(title = 'Comparando o PIB per Capita por continente') #Adiciona t�tulo

