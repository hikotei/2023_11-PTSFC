# 2023_11-PTSFC

## DAX

= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

train test split

= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

fit ARIMA models using pmdarima to find best order

fit ARIMA + GARCH models using rpy2

    implement own grid search for best order
    also include best order from ARIMA model

save all models in dict: model params + in sample fcasts

- - - - - - - 

maybe try other variants of GARCH models :

    EGARCH, 
    GJR-GARCH, 
    GARCH-M, 
    TGARCH, AVGARCH, NGARCH, etc. 
    and/or stochastic volatility models

- - - - - - - 

Note that there does not seem to be an option to use SARMA models in the "rugarch" package. 
But if there is a seasonal pattern (and that is quite likely when it comes to tourist arrivals), you will have to account for it somehow. 
Consider using exogenous seasonal variables (dummies or Fourier terms) in the conditional mean model 
via the argument external.regressors inside the argument mean.model in function ugarchspec. 
Alternatively, note that a SARMA model corresponds to a restricted ARMA model. 
An approximation of SARMA could thus be an ARMA with the appropriate lag order but without the SARMA-specific parameter restrictions.
(see https://stats.stackexchange.com/a/176629/338210)

- - - - - - - 

Q = Is a GARCH model even necessary?
A = test residuals from the SARIMA model using ARCH-LM test or some other test for (G)ARCH effects.

    If we discover conditional heteroskedasticity in the residuals of an ARMA model, 
    it certainly makes sense to try appending a GARCH specification to ARMA to see what happens.

ARMA alone would explain more variance in sample than ARMA-GARCH 
(just as OLS would explain more than feasible GLS, regardless of which is closer to the true model in population). 
GARCH would not explain any variance if you leave the conditional mean part empty (without ARMA). 
And if the ARMA-GARCH model approximates the true DGP better than a plain ARMA and plain GARCH, 
the out of sample performance of ARMA-GARCH will be better -- as long as you can estimate the model sufficiently well. 
(And since ARMA-GARCH is a richer model than plain ARMA and plain GARCH, you would normally not be able to estimate it as precisely as plain ARMA and plain GARCH on any given dataset.)
(see https://stats.stackexchange.com/a/233165/338210)

= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

evaluate models on test set using rolling window fcast

in theory : 

    should select model with out of sample criteria
    under the assumptions of standard ARMA-GARCH, the residuals should be homoscedastic and more generally iid normal => TEST

    AIC estimates the model's likelihood of a new observation coming from the same distribution. 
    It does so based on in-sample information, i.e. uses in-sample likelihood and penalizes it for overfitting. 
    To underline, AIC does not measure in-sample fit, it estimates out-of-sample fit (expected likelihood of a new observation).
    (see https://stats.stackexchange.com/questions/159807/arma-garch-model-selection-fit-evaluation?rq=1#:~:text=AIC%20estimates%20the%20model%27s%20likelihood%20of%20a%20new%20observation%20coming%20from%20the%20same%20distribution)

create results df with:

    model params ie p, q, arch_p, arch_q
    model information criteria
    model log likelihood

    in sample errors: MSE, MAE, MAPE, SMAPE
    out of sample fcast errors: MSE, MAE, MAPE, SMAPE


## Energy
