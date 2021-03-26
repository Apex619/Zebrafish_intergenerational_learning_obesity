#Example Mixed Model Plotting code

# create a simulated dataset for prawn Hue through time for green and red prawns
# have a code for each individual that has multiple measurements across each time point
d <- data.frame(expand.grid(time = c(0, 30, 60, 90, 120), individual = 1:30, stringsAsFactors = FALSE)) %>%
  mutate(., prawn_col = ifelse(individual <= 15, 'red', 'green'))

# set up intercept
intercept = 4
# set up gradient for green and red
bRed <- rnorm(1, mean = 0.05, sd = 0.05)
bGreen <- rnorm(1, mean = 0.001, sd = 0.01)
# set up standard deviation
sigma = 5

# add simulated Hue - filter by green and red to make different data
d_red <- filter(d, prawn_col == 'red') %>%
  group_by(., individual) %>%
  mutate(., Hue = intercept + bRed*time + rnorm(time, 0, sigma)) %>%
  data.frame()
d_green <- filter(d, prawn_col == 'green') %>%
  group_by(., individual) %>%
  mutate(., Hue = intercept + bGreen*time + rnorm(time, 0, sigma)) %>%
  data.frame()

# bind together
d <- bind_rows(d_red, d_green)

# quick plot
# can plot linear models on using ggplot2 to see whether there is a relationship
ggplot(d) +
  geom_point(aes(time, Hue, col = prawn_col)) +
  stat_smooth(aes(time, Hue, col = prawn_col), se = FALSE, method = 'lm') +
  facet_wrap(~ prawn_col) +
  scale_color_manual(values = c('green4', 'red'))

# ok can see some variation between colours
# however we have mutliple measurements per individual which can be seen by plotting the lines between those points, group = individual within geom_line()
ggplot(d) +
  geom_line(aes(time, Hue, col = prawn_col, group = individual), alpha = 0.25) +
  stat_smooth(aes(time, Hue, col = prawn_col), se = FALSE, method = 'lm') +
  facet_wrap(~ prawn_col) +
  scale_color_manual(values = c('green4', 'red'))

# simplest statistics
# do a linear model between time and Hue, look for differences between prawn colours
fit_lm <- lm(Hue ~ time * prawn_col, d)

# HOWEVER THIS INFLATES OUR DEGREES OF FREEDOM AS WE DO NOT HAVE INDEPENDENCE BETWEEN DATA POINTS

# instead we can fit a linear mixed effects model with a random effect on individual
# this will allow a different intercept to be fit for each individual
fit_lme <- lmer(Hue ~ time * prawn_col + (1|individual), d, REML = FALSE)
summary(fit_lme)

# you can change the random effect notation to include both random intercept and random slopes by allowing a different slope for each individual
# this is what we will use for the rest of this tutorial
fit_lme <- lmer(Hue ~ time * prawn_col + (1 + time|individual), d, REML = FALSE)
summary(fit_lme)

# to check significance of the interaction, remove it, and compare models using an anova
fit_lme2 <- lmer(Hue ~ time + prawn_col + (1 + time|individual), d, REML = FALSE)
summary(fit_lme)
anova(fit_lme, fit_lme2)

# in this example there is a significant interaction so this is our best model!!!

# make predictions dataframe
new_data <- data.frame(expand.grid(time = unique(d$time), prawn_col = unique(d$prawn_col), stringsAsFactors = FALSE))

# add predictions 
new_data$Hue <- predict(fit_lme, newdata = new_data, re.form=NA)

# OPTIONAL EXTRA
# get confidence and prediction intervals around mean prediction 
# get model matrix
mm <- model.matrix(terms(fit_lme), new_data)

## or newdat$distance <- mm %*% fixef(fm1)
# variance of fixed effect
pvar <- diag(mm %*% tcrossprod(vcov(fit_lme), mm))
# variance of random effects
tvar <- pvar + VarCorr(fit_lme)$individual[1]  
cmult <- 1.96

# add columns to dataframes
# CI gives uncertainty around fixed effects only
# PI gives uncertainty based on random effects as well!
new_data <- mutate(new_data,
                   PI_low = Hue - cmult*sqrt(tvar),
                   PI_high = Hue + cmult*sqrt(tvar),
                   CI_low = Hue - cmult*sqrt(pvar), 
                   CI_high = Hue + cmult*sqrt(pvar))

# create dataframe for new_data for each individual ####
new_data_ind <- select(d, time, prawn_col, individual)
# predict each individuals relationship including the random effects
new_data_ind$Hue <- predict(fit_lme, new_data_ind, re.form = NULL)

##############################################################

# final fancy plot ####
# plot is rather fancy - plots the predictions and raw points as well as fixed effect and random uncertainty
ggplot(d) +
  geom_line(aes(time, Hue, col = prawn_col, group = individual), alpha = 0.1, d) +
  geom_line(aes(time, Hue, col = prawn_col), new_data) +
  geom_ribbon(aes(x = time, ymin = PI_low, ymax = PI_high, fill = prawn_col), alpha = 0.25, new_data) +
  geom_ribbon(aes(x = time, ymin = CI_low, ymax = CI_high, fill = prawn_col), alpha = 0.25, new_data) +
  ylab('Hue') +
  xlab('Time (minutes)') +
  facet_wrap(~ prawn_col) +
  scale_color_manual('Prawn colour', values = c('green4', 'red')) +
  scale_fill_manual('Prawn colour', values = c('green4', 'red'))

# option 2
# plots the mean relationship and each individual random relationship
ggplot(d) +
  geom_point(aes(time, Hue, col = prawn_col), alpha = 0.1, d) +
  geom_line(aes(time, Hue, col = prawn_col, group = individual), alpha = 0.25, new_data_ind) +
  geom_line(aes(time, Hue, col = prawn_col), new_data) +
  ylab('Hue') +
  xlab('Time (minutes)') +
  facet_wrap(~ prawn_col) +
  scale_color_manual('Prawn colour', values = c('green4', 'red')) +
  scale_fill_manual('Prawn colour', values = c('green4', 'red'))