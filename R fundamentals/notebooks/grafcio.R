
library(readxl)
Banco <- read_excel("D:/FIAP/BI/Banco.xlsx")
View(Banco)

attach(Banco)


#########################################################################

                # Gr�fico de Barras

#########################################################################

# Grafico Simples
barplot(table(Banco$sexo))
#table quantidade de observa��es

##########################################################################

# Grafico Colorido
#prop.table %
barplot(prop.table(table(Banco$sexo))*100,
  col=c("blue","red"))
  title("Tabela de Frequ�ncia", xlab = "Sexo", ylab="%")
  
prop.table(table(sexo))

##########################################################################

# Grafico package Plotly - Gr�ficos interativos com qualidade em publica��o.
#install.packages("plotly")
library(plotly)

barras <-plot_ly(x=Banco$sexo, y=Banco$sal�rio, type ="bar")
barras


##########################################################################

# Gr�fico Package ggplot.
#install.packages("ggplot2")
library(ggplot2)

grafico_barras <-ggplot(Banco, aes(x=catemp, fill=sexo))+
  #geom_bar n�o subscrever a barra
  #aes= x e y ondem sao exibidos
  geom_bar(position = 'dodge')+
  xlab("Grupos definido por sexo")+
  ylab("Numeros de clientes")+
  ggtitle("Gr�fico de Barra")


grafico_barras

# Gr�fico Package ggplot com plotly.
grafico_barras1 <-ggplotly(grafico_barras)
grafico_barras1

###########################################################################

                #Gr�fico de Pizza

###########################################################################

# Gr�fico simples
pizza <- pie(table(Banco$sexo))


# Gr�fico 3D
#install.packages("plotrix")
library(plotrix)

pie <-pie3D(table(Banco$sexo))


# Gr�fico colorido e interativo


grafico_pie <-table(Banco$sexo)
grafico_pie1 <-as.data.frame(grafico_pie)
grafico_pie1

grafico_pie2 <-plot_ly(grafico_pie1,
                       labels = ~Var1,
                       values = ~Freq,
                       type   = 'pie') %>%
                       layout(title = "Gr�fico de Pizza")

grafico_pie2


##############################################################################

              #  Gr�fico de Histograma

##############################################################################

# Gr�fico simples
hist(Banco$sal�rio)

# Gr�fico colorido pacote ggplot
histograma <-ggplot(Banco, aes(x=sal�rio))+
  geom_histogram(color="blue", fill="lightblue", bins = 30)+
  xlab("variancia")+
  ylab("Frequencia contagem")+
  ggtitle("Gr�fico_Histogrma")+
  theme()

histograma

# Grafico ggplot com plotly
# transforma grafico ggplot em plotly - interativo
ggplotly(histograma)


# ploty
plot_ly(x=Banco$sal�rio, type='histogram')


# ggplot - plotly e fun��o face_grid
grafico_histograma_catemp <-ggplot(Banco, aes(x=sal�rio))+
  geom_histogram(color="red", fill="lightblue", bins=30)+
  xlab("Variancia")+
  ylab("Frequencia")+
  ggtitle("Gr�fico histograma")+
  facet_grid(~catemp)


grafico_histograma_catemp

interativo_histograma <-ggplotly(grafico_histograma_catemp)
interativo_histograma

#grafico densidade

densidade <-ggplot(Banco, aes(x=sal�rio, fill=catemp))+
  geom_density(alpha=0.9)+
  xlab("Variancia")+
  ylab("Frequ�ncia")+
  ggtitle("Grafico_densidade")+
  facet_grid(~catemp)


densidade

ggplotly(densidade)


#########################################################

                # Gr�fico de dispers�o/ linhas

#########################################################

#Gr�fico simples
plot(Banco$sal�rio~Banco$estudo)

#Gr�fico ggplot
dispersao <-ggplot(Banco, aes(x=estudo, y=sal�rio, color=sexo))+
geom_point()

dispersao
ggplotly(dispersao)

#Gr�fico ggplot com fun��o face_wrap
dispersao1 <-ggplot(Banco, aes(x=estudo, y=sal�rio))+
  geom_point()+facet_wrap(~sexo)

dispersao1

#Gr�fico ggplot com legenda
dispersao2 <-ggplot(Banco, aes(x=estudo, y=sal�rio, color=sexo))+
  geom_point(size=5)+
  xlab("Variancia_estudo")+
  ylab("Variancia_salario")+
  ggtitle("Gr�fico de dispersao")


dispersao2
ggplotly(dispersao2)


#Gr�fico ggplot com plotly
dispersao <-plot_ly(x=Banco$estudo, y=Banco$sal�rio, 
                    color=Banco$sexo)
dispersao

#Gr�fico ggplot com legenda
dispersao4 <-ggplot(Banco, aes(x=estudo, y=sal�rio, color=sexo))+
  geom_point(aes(size=5))+
  xlab("Variancia_estudo")+
  ylab("Variancia_salario")+
  ggtitle("Gr�fico de dispersao")+
  facet_grid(~sexo)

dispersao4
ggplotly(dispersao4)


#####################################################################

            # Gr�fico BoxPlot

#####################################################################



# Gr�fico Simples
boxplot(Banco$cartao_credito)

# Gr�fico colorido e separado por categoria de emprego
boxplot(Banco$cartao_credito~Banco$catemp,
        main="Cartao Credito por categoria emprego",
        xlab= "Catemp", ylab="Cartao Credito",
        col=c("blue","red","yellow"))


# Gr�fico colorido e separado por categoria de emprego sem outliers.
boxplot(Banco$cartao_credito~Banco$catemp,
        main="Cartao Credito por categoria emprego",
        xlab= "Catemp", ylab="Cartao Credito",
        col=c("blue","red","yellow"), outline=FALSE, horizontal = TRUE)



# Gr�fico ggplot colorido por categoria de emprego
boxplot<-ggplot(Banco, aes(x=cartao_credito , y=sal�rio, fill=catemp))+
        geom_boxplot()+
        xlab("cartao de credito")+ 
        ylab("Cartao Credito")+
        ggtitle("Cartao Credito e salario por categoria emprego")


boxplot


# Gr�fico BoxPlot interativo
boxplot_interativo<-
  plot_ly(x=Banco$catemp, y=Banco$cartao_credito,
        main="Gr�fico interativo",
        xlab="Catemp",ylab="Cartao Credito",
        color=(Banco$catemp), outline = TRUE,type='box')

boxplot_interativo

# Gr�fico interativo simples.
plot_ly (Banco, x=cartao_credito, type='box')



#########################################################################

            # Gr�fico de linhas

#########################################################################

# Gr�fico Simples
plot(cartao_credito, type='line')



# Gr�fico colorido interativo
grafico_linha2 <- plot_ly(Banco, y=cartao_credito, type='scatter',mode='lines')
grafico_linha2


#Gr�fico duplo interativo
duplo1 <-plot_ly(Banco, y=cartao_credito)%>%
  add_trace(y=~cartao_credito, type='scatter', name='Cartao Credito', mode='lines')%>%
  add_trace(y=~Emprestimos, type='scatter', name='Emprestimos', mode='lines')

duplo1

#Gr�fico GGplot
linha <-ggplot(Banco, aes(x=datanasc, y=Emprestimos))+
  geom_line(col="blue")+
  xlab("Ano")+
  ylab("Emprestimos")+
  ggtitle("Emprestimos por ano")

linha
ggplotly(linha)


##############################################################################

                      # Gr�fico na mesmo janela

#############################################################################

#install.packages("GGally") 
#install.packages("gridExtra")
library(GGally)
library(gridExtra)
library(plotly)

#Gr�fico na mesma janela
grid.arrange(
  grafico_barras,
  histograma,
  linha,
  boxplot,
  dispersao2,
  grafico_barras,
  ncol=3, nrow=2)
  
  



##############################################################

          # Gr�ficos de anima��o

##############################################################

# Banco_1
attach(Banco_1)

install.packages("gganimate")
library(gganimate)

# Gr�ficos com pacote gganimation
animacao <-ggplot(Banco, aes(x=estudo, y=sal�rio,
                             size=Emprestimos, colour=catemp))+
                             geom_point(show.legend = FALSE, alpha=0.5)+
                             scale_color_viridis_d()+
                             scale_size(range=c(2,12))+
                             scale_x_log10()+
                             labs(x="Estudo", y="Salario")
                            
animacao

install.packages("gifski")
library(gifski)

# Continuacao para anima��o
# Primeira Anima��o
animacao + transition_time(Ano)+
  labs(title = "Ano:{frame_time}")

typeof(Banco_1$Ano)

# Segunda Anima��o
animacao + facet_wrap(~catemp)+
  transition_time(Ano)+
  labs(title="Ano:{frame_time}")

# Terceira Animacao
animacao + transition_time(Ano)+
  labs(title = "Ano:{frame_time}")+
  view_follow(fixed_y = TRUE)

# Quarta Animacao
animacao + transition_time(Ano)+
  labs(title = "Ano:{frame_time}")+
  shadow_wake(wake_length = 0.1, alpha = FALSE)

# Quinta Animacao
animacao + transition_time(Ano)+
  labs(title = "Ano:{frame_time}")+
  shadow_mark(alpha = 0.3, size = 0.5)


###################  outra anima��o #########################

# Anima��o com ggplot

animacao_ggplot <-ggplot(Banco, aes(Ano, sal�rio, group = catemp, color=factor(catemp)))+
  geom_line()+
  scale_color_viridis_d()+
  labs(x="Ano", y="Salario")+
  theme(legend.position = "top")

animacao_ggplot

# Primeira Animacao
animacao_ggplot + transition_reveal(Ano)

# Segunda Animacao
animacao_ggplot + geom_point()+
  transition_reveal(Ano)

#Terceira Anima��o
animacao_ggplot + geom_point(aes(group = seq_along(Ano)))+
  transition_reveal(Ano)



#############################################################

          #Anima��o com video

#############################################################

#install.packages("av")
library(av)

# Primeira anima��o com video
video <-ggplot(Banco, aes(x=estudo, y=sal�rio))+
  geom_point(aes(colour = catemp), size=2)+
  transition_states(catemp, transition_length = 10,
                    state_length = 1)

video
# Segunda anima��o
video + enter_fade()+
  exit_shrink()


# Agora com video - video outup
# Terceira Anima��o
animate (video + enter_fade()+
                   exit_fly(y_loc = 1),
                   renderer = av_renderer())
                            


###################################################################

              # Pacote esquisse

###################################################################

#install.packages("esquisse")
library(esquisse)


janela <-esquisser(Banco)

Banco <- Banco %>%
  filter(datanasc >= "1937-08-11 04:48:00" & datanasc <= "1971-02-10 00:00:00")

ggplot(Banco) +
  aes(x = estudo, y = sal�rio, fill = sexo, colour = catemp, size = Ano) +
  geom_point() +
  scale_fill_viridis_d(option = "plasma") +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Estudo", y = "Salario", title = "Grafico Esquisse", fill = "Sexo", color = "Catemp") +
  theme_minimal()





















  











































































