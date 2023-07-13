#01 - carregar pacote
library(ggplot2)

#02 - background e posicao da legenda para todos os plots
theme_set(
  theme_light()+
    theme(legend.position = "right"))

#03 - atribuicao e leitura da tabela
dtaula2 <- read.csv2("dtaula2.csv", stringsAsFactors = T)

#04 - atribuicao de objeto. dados verdadeiros
obj <- ggplot(dtaula2, aes(x = feve, y = marcador))

#05 - coluna "Grupos" como um fator
dtaula2$Grupos <- factor(dtaula2$Grupos)

#06 - niveis do marcador, pela fracao de ejecao em funcao dos grupos da doenca
obj + geom_point(aes(shape = Grupos, color = Grupos),
                 size = 3.5, alpha=0.6) +
  scale_color_manual(values = c("#f8766d", "#56a8f4", "#f8cf51"))+
  xlab("Fração de Ejeção do VE") + ylab("D.O Marcador")
  
#07 - Bubble chart - niveis do marcador, fracao de ejecao, NYHA e grupos
obj + geom_point(aes(color = Grupos, size = NYHA), alpha = 0.5) +
  scale_color_manual(values = c("#f8766d", "#56a8f4", "#f8cf51")) +
  scale_size(range = c(4, 9))+
  xlab("Fração de Ejeção do VE") + ylab("D.O Marcador")

#08 - atribuicao de novo objeto com dados falsos
objfake <- ggplot(dtaula2, aes(x = fevefake, y = marcadorfake))

#09 -  regressao linear. grafico basico
objfake + geom_point() + geom_smooth(method = lm)+
  xlab("Fração de Ejeção do VE") + ylab("D.O Marcador")

#10 - reegressao linear. grupos divididos por cores
objfake + geom_point(aes(color = Grupos, shape=Grupos))+
  geom_smooth(aes(color = Grupos, fill = Grupos), method = lm)+
  scale_color_manual(values = c("#f8766d", "#56a8f4", "#f8cf51"))+
  scale_fill_manual(values = c("#f8766d", "#56a8f4", "#f8cf51"))+
  xlab("Fração de Ejeção do VE") + ylab("D.O Marcador")
