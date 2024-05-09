library(lme4)
summary(lme4::lmer(Y ~ I(X-X_bar) + X_bar + (1 | ID), data = dat))


coef(a)
summary(a<-lme4::lmer(Y ~ X_centered + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X_centered + C_centered + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X_centered + C_centered + (1 + X_centered| ID), data = dat))
summary(lme4::lmer(Y ~ X_centered + C_centered + (1 + C_centered| ID), data = dat))
#summary(lme4::lmer(Y ~X_centered) + I(C-C_bar) + C_bar + (1 + I(C-C_bar) + C_bar| ID), data = dat))


summary(lme4::lmer(Y ~ I(X-X_bar) + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + I(C-C_bar) + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + I(C-C_bar) + (1 +I(X-X_bar)| ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + I(C-C_bar) + (1 + I(C-C_bar)| ID), data = dat))
#summary(lme4::lmer(Y ~ I(X-X_bar) + I(C-C_bar) + C_bar + (1 + I(C-C_bar) + C_bar| ID), data = dat))


summary(lme4::lmer(Y ~ X + (1| ID), data = dat))

summary(lm(Y ~ X_bar, data = dat))
summary(lm(Y_bar ~ X_bar , data = dat))

summary(lme4::lmer(Y ~ X_bar + (1| ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C + (1 + C| ID), data = dat))

summary(lm(Y_bar ~ X_bar, data = dat %>% group_by(ID) %>% sample_n(1)))
summary(lme4::lmer(Y ~ X_bar + (1| ID), data = dat))
summary(lme4::lmer(Y_bar ~ X_bar + (1| ID), data = dat %>%
                     group_by(ID) %>% sample_n(3)))


summary(lme4::lmer(Y ~ I(X-X_bar) + X_bar + (1 | ID), data = dat))


sd(dat$X[dat$ID==10])
sd(dat$Y[dat$ID==10])
mean(dat$X[dat$ID==10])
mean(dat$Y[dat$ID==10])

summary(lme4::lmer(Y ~ I(X-X_bar) + C + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C + (1 + C| ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C + C_bar + (1 + C| ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C + C_bar + X_bar + (1 + C| ID), data = dat))
summary(lme4::lmer(U ~ Y  + (1 | ID), data = dat))
summary(lme4::lmer(U ~ Y + X+ (1 | ID), data = dat))
summary(lm(U ~ Y, data = dat))

summary(lme4::lmer(Y ~ X + X_bar + C_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + X_bar + C_bar + C + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X + X_bar + C_bar + I(C-C_bar) + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + X_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X + X_bar + C_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + X_bar + C_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X + C + X_bar + C_bar + (1 | ID), data = dat))
mean(dat$X_bar)
mean(dat$X)

summary(lme4::lmer(Y ~ I(X-X_bar) + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C +(1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + X_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ I(X-X_bar) + C_bar  + C + (1 | ID), data = dat))

summary(lme4::lmer(Y ~ X + X_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X + X_bar + C_bar + (1 | ID), data = dat))

summary(lme4::lmer(Y ~ X + C + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X + C + C_bar + (1 | ID), data = dat))

summary(lme4::lmer(X ~  C_bar + (1 | ID), data = dat))
summary(lm(C_bar ~ X_bar , data = dat))
summary(lm(Y ~ I(X-X_bar) , data = dat))
summary(lm(Y ~ I(X-X_bar) + C, data = dat))
summary(lm(Y ~ X + X_bar, data = dat))
summary(lm(Y ~ I(X-X_bar) + X_bar, data = dat))
summary(lm(Y ~ I(X-X_bar) + C + X_bar, data = dat))
summary(lm(Y ~ I(X-X_bar) + C + X_bar + C_bar, data = dat))
summary(lm(Y ~ I(X-X_bar) + C + C_bar, data = dat))

summary(lm(Y ~ X, data = dat))
summary(lm(Y ~ X + C , data = dat))
summary(lm(Y ~ X + C + C_bar, data = dat))
summary(lm(Y ~ X + C + C_bar + X_bar, data = dat))
summary(lm(Y ~ X + C + X_bar, data = dat))
summary(lm(Y ~ X_bar, data = dat))
summary(lme4::lmer(Y ~ X_bar +  (1 | ID), data = dat))
summary(lm(X_bar ~ C, data = dat))

summary(lme4::lmer(Y ~ X_bar +  (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X_bar + C_bar + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X_bar + C +  (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X_bar + C + C_bar + (1 | ID), data = dat))

summary(lm(Y ~ X_bar, data = dat))
summary(lm(Y ~ X_bar + C_bar, data = dat))
summary(lm(Y ~ X_bar + C_bar + X, data = dat))
summary(lm(Y ~ X_bar + C_bar + C, data = dat))
summary(lm(Y ~ X + X_bar + C_bar + C, data = dat))

summary(lme4::lmer(Y ~ X + X_bar + C_bar + C + (1 | ID), data = dat))
cor(dat$C, dat$C_bar)
cor(dat$X, dat$C_bar)
cor(dat$X_bar, dat$C)
cor(dat$X_bar, dat$X)

summary(lm(Y ~ X_bar + C_bar + C, data = dat))
summary(lm(Y_bar ~ X_bar + C_bar, data = dat))


summary(lme4::lmer(Y ~ X_bar + C +  (1 | ID), data = dat))
summary(lme4::lmer(Y ~ X_bar + C_bar + C + (1 | ID), data = dat))

summary(lm(Y ~ U + C + C_bar, data = dat))
summary(lme4::lmer(Y ~ U + C + X + (1 | ID), data = dat))
summary(lme4::lmer(Y ~ U + C + X + C_bar + (1 | ID), data = dat))
summary(lme4::lmer(U ~ Y + C +  X + C_bar+ (1 | ID), data = dat))

summary(lm(Y_bar ~ X_bar + C, data = dat))
summary(lm(Y_bar ~ X_bar + C_bar, data = dat))
mean(dat$C)

summary(lm(U_bar ~  C_bar, data = dat))
summary(lm(U_comp_bar ~  C_bar, data = dat))
summary(lm(U_comp ~  C_bar, data = dat))
cor(dat$U_comp, dat$C_bar)
cor(dat$U_comp, dat$C)
cor(dat$U_bar, dat$C_bar)
cor(dat$X_bar, dat$C_bar)
cor(dat$X_bar, dat$U_comp)
cor(dat$U_bar, dat$U_comp_bar)