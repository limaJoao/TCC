library(ggplot2)

x <- c(0,-.405,-.2,-.33,-.1,.37, 0.3786728, 0.2498911)
y <- c(0,.41,.2508,-.15153,-.3348,-.41, 0.2283077,0.4254680)
nome <- LETTERS[1:8]

df <- data.frame(f1=x, f2=y, nome)

rects <- data.frame(xstart = c(-Inf, 0), 
                    xend = c(0,Inf),
                    ystart = c(-Inf,0),
                    yend = c(0,Inf))

theme_set(theme_classic())

ggplot(df, aes(f1,f2))+
  lims(x=c(-.5,.5), y=c(-.5,.5))+
  geom_rect(xmin=-Inf, xmax=0, ymin=-Inf, ymax=0, fill='grey60')+
  geom_rect(xmin=0, xmax=Inf, ymin=0, ymax=Inf, fill='grey85')+
  geom_point(size=2)+
  geom_text(aes(label=nome), vjust=-0.3, hjust=-0.3)+
  geom_text(aes(x=-.499,y=.499,label='i'))+
  geom_text(aes(x=.499,y=.499,label='ii'))+
  geom_text(aes(x=.499,y=-.499,label='iii'))+
  geom_text(aes(x=-.499,y=-.499,label='iv'))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave("/home/limaJV/Documentos/UFJF/TCC/imagens/dominancia.png")
