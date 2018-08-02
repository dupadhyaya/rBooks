# Misc Examples
# Mayo Clinic Lung Cancer Data
library(survival)

# learn about the dataset
help(lung)

str(lung)
?Surv
# create a Surv object 
survobj <- with(lung, Surv(time,status))

# Plot survival distribution of the total sample
# Kaplan-Meier estimator 
fit0 <- survfit(survobj~1, data=lung)
summary(fit0)
plot(fit0, xlab="Survival Time in Days", 
     ylab="% Surviving", yscale=100,
     main="Survival Distribution (Overall)") 

# Compare the survival distributions of men and women 
fit1 <- survfit(survobj~sex,data=lung)

# plot the survival distributions by sex 
plot(fit1, xlab="Survival Time in Days", 
     ylab="% Surviving", yscale=100, col=c("red","blue"),
     main="Survival Distributions by Gender") 
legend("topright", title="Gender", c("Male", "Female"),
       fill=c("red", "blue"))

# test for difference between male and female 
# survival curves (logrank test) 
survdiff(survobj~sex, data=lung) 

# predict male survival from age and medical scores 
MaleMod <- coxph(survobj~age+ph.ecog+ph.karno+pat.karno,
                 data=lung, subset=sex==1)

# display results 
MaleMod

# evaluate the proportional hazards assumption 
cox.zph(MaleMod)