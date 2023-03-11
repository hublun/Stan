library(MASS)
library("MASS")
set.seed(42)
D <-3
P <- 10 
N <-300

mu_theta <-rep(0,D) # the mean of eta
mu_epsilon<-rep(0,P) # the mean of epsilon
Phi<-diag(rep(1,D))
Psi <- diag(c(0.2079, 0.19, 0.1525, 0.20, 0.36, 0.1875, 0.1875, 1.00, 0.27, 0.27))
l1 <- c(0.99, 0.00, 0.25, 0.00, 0.80, 0.00, 0.50, 0.00, 0.00, 0.00)
l2 <- c(0.00, 0.90, 0.25, 0.40, 0.00, 0.50, 0.00, 0.00, -0.30, -0.30)
l3<-  c(0.00, 0.00, 0.85, 0.80, 0.00, 0.75, 0.75, 0.00, 0.80, 0.80)
L <-cbind(l1,l2,l3) # the loading matrix

Theta <-mvrnorm(N, mu_theta, Phi) # sample factor scores
Epsilon <-mvrnorm(N, mu_epsilon, Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data

library("rbokeh")
df<-data.frame(Y)
tools <- c("pan", "wheel_zoom", "resize", "reset")
nms <- expand.grid(names(df)[1:P], rev(names(df)[1:P]), stringsAsFactors = FALSE)
nms$yaxis <- rep(c(TRUE, rep(FALSE, P-1)), P)
nms$xaxis <- c(rep(FALSE, (P-1)*P), rep(TRUE, P))
nms$h <- nms$w <- 75
nms$h[nms$xaxis] <- nms$w[nms$yaxis] <- 90
splom_list <- vector("list", P^2)

for(i in seq_along(splom_list)) {
   splom_list[[i]] <- figure(width = nms$w[i], height = nms$h[i],
    tools = tools, min_border = 2) %>%
    ly_points(nms$Var1[i], nms$Var2[i], data = df,
       size = 1) %>%
    x_axis(visible = nms$xaxis[i]) %>%
    y_axis(visible = nms$yaxis[i]) 
}

grid_plot(splom_list, nrow = P, ncol = P, same_axes = TRUE, link_data = TRUE)
