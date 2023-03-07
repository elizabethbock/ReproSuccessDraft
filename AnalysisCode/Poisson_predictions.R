
mod <- glm(survivingcalves~closeness + degree + eig + strength + 
             cluster + birthyear + log(counts), 
           data = repro[!repro$X %in% c("REG", "RHY"),], 
           family = "poisson") 

summary(mod)


# create dataframe for model predictions

range(mod$data$closeness)

newdata <- with(mod$data, expand.grid(closeness = seq(0.1, 0.6, length.out = 100), 
                                 degree = mean(degree),
                                 eig = mean(eig), 
                                 strength = mean(strength), 
                                 cluster = as.factor("TDPDFOR"), 
                                 birthyear = 1985, 
                                 counts = 53))
                              
ilink <- family(mod)$linkinv
p <- predict(mod, newdata, se.fit = TRUE)

t_predict <- cbind(newdata,
                   res  = ilink(p$fit),
                   Ures = ilink(p$fit + (1.96 * p$se.fit)),
                   Lres = ilink(p$fit - (1.96 * p$se.fit)))

plot(mod$data$closeness, mod$data$survivingcalves, ylim = c(0, 3),
     xlab = "Closeness",
     ylab = "Surviving calves",
     yaxt = "n",
     pch = 16,
     col = adjustcolor("black", alpha.f = 0.7),
     cex = 1.25)

axis(1)
axis(2, las = 1)

# making the predictions
p <- aggregate(res ~ closeness, data = t_predict, mean)

l <- aggregate(Lres ~ closeness, data = t_predict, mean)
lines(l$closeness, l$Lres, lty = 2)

u <- aggregate(Ures ~ closeness, data = t_predict, mean)
lines(u$closeness, u$Ures, lty = 2)

polygon(c(l$closeness, rev(l$closeness)), c(l$Lres, rev(u$Ures)),
        col = adjustcolor("grey", alpha.f = 0.6), border = NA)

lines(p$closeness, p$res, lty = 1)
