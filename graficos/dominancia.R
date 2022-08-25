library(ggplot2)

x <- c(0,-.45,-.2756,-.38321,.45)
y <- c(0,.438,.15,-.2153,-.398)

df <- data.frame(x, y)

rects <- data.frame(xstart = c(-Inf, 0), 
                    xend = c(0,Inf),
                    ystart = c(-Inf,0),
                    yend = c(0,Inf))

theme_set(theme_classic())

ggplot(df, aes(x,y))+
  lims(x=c(-.5,.5), y=c(-.5,.5))+
  geom_rect(xmin=-Inf, xmax=0, ymin=-Inf, ymax=0, fill='grey68')+
  geom_rect(xmin=0, xmax=Inf, ymin=0, ymax=Inf, fill='grey87')+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
