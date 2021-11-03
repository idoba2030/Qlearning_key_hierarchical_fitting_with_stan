#-------------------------------------------------------
load('data/null_10subjects_6blocks_50trials_4arms_parameters.Rdata')
load('data/null_10subjects_6blocks_50trials_4arms_extracted_parameters.rdata')

library(ggplot2)
library(ggpubr)
#population level parameters
p1= ggplot(data.frame(x=pars$mu_alpha),aes(x=x))+geom_histogram()+ geom_vline(xintercept = 0.5, linetype="dotted",color = "blue", size=1.5)+
  xlab(expression(alpha))+ theme_classic()

p2= ggplot(data.frame(x=pars$mu_beta),aes(x=x))+geom_histogram()+ geom_vline(xintercept = 4, linetype="dotted",color = "blue", size=1.5)+
  xlab(expression(beta))+ theme_classic()+scale_color_brewer(palette="Dark2")

p3= ggplot(data.frame(x=pars$mu1_w_rel),aes(x=x))+geom_histogram()+ geom_vline(xintercept = 0, linetype="dotted",color = "blue", size=1.5)+
  xlab(expression(paste(mu,"1_relevant")))+ theme_classic()+scale_color_brewer(palette="Dark2")

p4= ggplot(data.frame(x=pars$mu0_w_irrel),aes(x=x))+geom_histogram()+ geom_vline(xintercept = 0.5, linetype="dotted",color = "blue", size=1.5)+
  xlab(expression(paste(mu,"0_irrelevant")))+ theme_classic()+scale_color_brewer(palette="Dark2")
p5= ggplot(data.frame(x=pars$mu1_w_irrel),aes(x=x))+geom_histogram()+ geom_vline(xintercept = -0.5, linetype="dotted",color = "blue", size=1.5)+
  xlab(expression(paste(mu,"1_irrelevant")))+ theme_classic()+scale_color_brewer(palette="Dark2")

ggarrange(p1,p2,p3,p4,p5)

#compare individual level parameters
p6=ggplot(data.frame(x =true.parameters[,'alpha'], y =apply(pars$alpha, 2, mean)),aes(x=x,y=y))+geom_point()+
    ggtitle(expression(alpha),
            subtitle = paste('r=',round(cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean)),2)))+
  xlab(paste('simulated',expression(alpha)))+ylab(paste('recovered',expression(alpha)))+ xlim(0,10)+ylim(0,10)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5,size=10))
    
p7=ggplot(data.frame(x =true.parameters[,'beta'], y =apply(pars$beta, 2, mean)),aes(x=x,y=y))+geom_point()+
    ggtitle(expression(beta),
          subtitle = paste('r=',round(cor(true.parameters[,'beta'], apply(pars$beta, 2, mean)),2)))+
    xlab(paste('simulated',expression(beta)))+ylab(paste('recovered',expression(beta)))+ xlim(0,10)+ylim(0,10)+
    theme_classic()+theme(plot.title = element_text(hjust = 0.5,size=10))

p8=ggplot(data.frame(x =true.parameters[,'slope_rel'], y =apply(pars$slope_rel, 2, mean)),aes(x=x,y=y))+geom_point()+
  ggtitle(expression(paste(mu,"1_relevant")),
          subtitle = paste('r=',round(cor(true.parameters[,'slope_rel'], apply(pars$slope_rel, 2, mean)),2)))+
  xlab(paste('simulated_slope_rel'))+ylab('recovered_slope_rel')+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5,size=10))

p9=ggplot(data.frame(x =true.parameters[,'intercept_irrel'], y =apply(pars$intercept_irrel, 2, mean)),aes(x=x,y=y))+geom_point()+
  ggtitle(expression(paste(mu,"0_irrelevant")),
          subtitle = paste('r=',round(cor(true.parameters[,'intercept_irrel'], apply(pars$intercept_irrel, 2, mean)),2)))+
  xlab(paste('simulated_intercept_irrel'))+ylab('recovered_intercept_irrel')+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5,size=10))

p10=ggplot(data.frame(x =true.parameters[,'slope_irrel'], y =apply(pars$slope_irrel, 2, mean)),aes(x=x,y=y))+geom_point()+
  ggtitle(expression(paste(mu,"1_irrelevant")),
          subtitle = paste('r=',round(cor(true.parameters[,'slope_irrel'], apply(pars$slope_irrel, 2, mean)),2)))+
  xlab(paste('simulated_slope_irrel'))+ylab('recovered_slope_irrel')+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5,size=10))

ggarrange(p6,p7,p8,p9,p10)

annotate(geom="text", x=3, y=30, color="black",
         label=paste('r=',cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean)),sep=""))

ggplot(pars,aes(x=beta))+geom_histogram()

plot(true.parameters[,'alpha'], apply(pars$alpha, 2, median))
plot(true.parameters[,'beta'],apply(pars$beta, 2, median))
mean(true.parameters[,'alpha'])
mean(true.parameters[,'beta'])
mean(apply(pars$beta, 2, mean))
mean(pars$mu_alpha)
mean(pars$mu_beta)
hist(pars$mu_beta)

hist(true.parameters[,'alpha']- apply(pars$alpha, 2, median))
hist(true.parameters[,'beta']-apply(pars$beta, 2, median))


cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean))
cor(true.parameters[,'beta'],apply(pars$beta, 2, mean))

plot(apply(pars$beta, 2, median),apply(pars$beta, 2, median))
mlv(pars$beta[,1])
mean(pars$beta[,1])
hist(true.parameters[,'alpha']-apply(pars$alpha, 2, median))
hist(true.parameters[,'beta']-apply(pars$beta, 2, median))
library(modeest)
