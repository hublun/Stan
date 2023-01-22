library(plyr)
library(dplyr)
library(tidyr)
library(MCMCpack)
#=========================== gamma function =================================
ggplot() +    
  stat_function(aes(x=seq(0, 20, 0.1)), geom="area" ,alpha=0.2, 
                fun = dgamma, args=list(shape = 2, rate = 1))


ggplot() +    
  stat_function(aes(x=seq(0, 1, 0.1)), geom="area" ,alpha=0.2, 
                fun = dbeta, args=list(shape1=0.1, shape2=0.1))



alpha=rmultinom(n=100, size=10,prob=c(1,2,8,3,2,1))
class(alpha)
df=data.frame(t(alpha))
df
df <- df %>%
  gather("alpha","freq")
ggplot(df, aes(x=alpha, y=freq)) + geom_point(shape=21, size=5, color="#236798", fill="green", stroke=2)

theta = rdirichlet(100,alpha=c(1,2,6,4,2))
class(theta)
df = data.frame(theta)
df <- df %>%
  gather("theta","density")
df
ggplot(df, aes(x=theta, y=density)) + geom_point(shape=21, size=5, color="#236798", fill="green", stroke=2)

#================================== 20 Topic Distribution==========================================
rmultinom(n=10, size=100, prob=mtxl[1,] )
rdirichlet(10, alpha=mtxl[2,])
#============================================================================
clus = ddply(HWS, .(Club), summarise, avg = mean(yH))

ggplot(data=clus, aes(x=clus$avg)) + geom_density() + 
  stat_function(geom="area" ,alpha=0.2, 
                fun = dgamma, args=list(shape = 18, rate = 5))+theme_light()


ggplot(data = PBS1000_1_, aes(x=Bias, y=Accuracy, label=Lab)) + 
  geom_hline(yintercept = 0.95022, linetype = 5) +
  geom_hline(yintercept = 0.9548, linetype = 5) +  
  geom_hline(yintercept = 0.9507, linetype = 5) + 
  geom_hline(yintercept = 0.943, linetype = 5) + 
  geom_hline(yintercept = 0.9566, linetype = 5) +
  
  geom_vline(xintercept = 0.959, linetype = 5) + 
  geom_vline(xintercept = 0.31, linetype = 5) + 
  geom_vline(xintercept = 1.535, linetype = 5) + 
  geom_vline(xintercept = 1.82, linetype = 5) + 
  geom_vline(xintercept = 1.44, linetype = 5) + 
  geom_point(color="#336655", size=4) + theme_gray()+
  geom_text(color="#123321", size=5, hjust = -0.5, vjust = -0.5)
