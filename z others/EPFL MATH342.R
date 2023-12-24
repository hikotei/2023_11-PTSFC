library(rugarch)
library(multitaper)

# Mean monthly temperature in UK, from Hadley center
data(CETmonthly, package = "multitaper")

# Keep period 1900-2000
mtempfull <- ts(CETmonthly[, 3], start = c(CETmonthly[1, 1], CETmonthly[1, 2]), 
                frequency = 12)
mtemp <- window(mtempfull, start = c(1900, 1), end = c(1999, 12))
months <- rep(1:12, 100)

# Fit our favorite model - Fourier basis + SARMA(1,0,1) x (1,0,1)
meanreg <- cbind(cos(2 * pi * months/12), sin(2 * pi * months/12), cos(4 * pi * months/12), sin(4 * pi * months/12))
sarima <- forecast::Arima(mtemp, order = c(3, 0, 1), seasonal = c(1, 0, 1), xreg = meanreg)

# Not great, but will serve for the illustration

# This SARIMA is basically an ARMA model of high order with constraints
# These constraints can be roughly replicated by fixing most components to
# zero For example, if we have a SARMA (0,1)x(0,1)[12], coefficients 2 to 11
# are zero We will however estimate an extra MA parameter at lag 13 which
# would normally be prod(sarima$model$theta[c(1,12)])

# Extract AR coefficients via $model$phi Extract MA coefficients via
# $model$theta Find which parameters are zero and constrain them

lnames <- c(paste0("ar", which(sapply(sarima$model$phi, function(th) {isTRUE(all.equal(th, 0))} ))), 
            paste0("ma", which(sapply(sarima$model$theta, function(th) {isTRUE(all.equal(th, 0))} ))) )

constraints <- rep(list(0), length(lnames))
names(constraints) <- lnames
order <- c(length(sarima$model$phi), length(sarima$model$theta))

# To have a non-constant variance, we can let it vary per month Could try
# with dummies, but will stick with Fourier (issues with convergence of optimization routine otherwise) 
# Creating a matrix of dummies varreg <- model.matrix(lm(months~as.factor(months)))[,-1] 

# Model specification
model <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(0, 0), external.regressors = meanreg), 
                    mean.model = list(armaOrder = order, fixed.pars = constraints))
                    
# Fit model
fitmodel <- ugarchfit(spec = model, data = mtemp)

# Coefficients of the model
fitmodel@fit$coef[fitmodel@fit$coef != 0]
