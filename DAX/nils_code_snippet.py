from scipy.stats import norm, t
import rpy2
import rpy2.robjects as ro
import rpy2.robjects.pandas2ri as pandas2ri
from rpy2.robjects.conversion import localconverter

import rpy2.robjects.packages as rpackages
import rpy2.robjects as robjects

import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

def _get_rugarch_model(p: int, q: int, arch_p: int, arch_q: int, dist: str):
    """ Function to get rugarch model with specified parameters

    Parameters
    ----------
    p: AR-order
    q: MA-order
    arch_p: GARCH-order
    arch_q: GARCH-order
    dist: ['norm', 'std']

    Returns
    -------
    model: R-ugarchspec
    rugarch: link to library(rugarch)
    """
    rugarch = rpackages.importr('rugarch')
    variance_model = robjects.ListVector(
        {'model': "sGARCH",
         'garchOrder': robjects.IntVector([arch_p, arch_q])})
    mean_model = robjects.ListVector(
        {'armaOrder': robjects.IntVector([p, q]),
         'include.mean': True})
    #Params auskommentieren für freie Wahl dof und andere dist!!!
    if dist == 'std':
        fix_df = 3

        params= robjects.ListVector({'shape': fix_df})
        model = rugarch.ugarchspec(variance_model=variance_model, mean_model=mean_model,
                               distribution_model=dist,fixed_pars=params)

    elif dist == "norm":
        model = rugarch.ugarchspec(variance_model=variance_model, mean_model=mean_model,
                                   distribution_model=dist)



    return model, rugarch


def r_garch_forecast(y: np.ndarray, p: int, q: int, arch_p: int, arch_q: int, dist: str = 'std', external=[]):
    """ Uses the R function ugarchestimate to (in-sample) forecast timeseries ARMA-GARCH model with estimation
    on whole data

        Parameters
        ----------
        y: np.ndarray
            Time series values to fit
        p: int
            AR-order of ARMA model
        q: int
            MA-order of ARMA model
        arch_p: int
            AR-order of GARCH model
        arch_q: int
            order of GARCH model
        dist: str
            Innovations distribution (either 'norm' or 'std' for student's t distribution)
        refit_window: str
            'recursive' if estimation window should be increasing, 'moving' for fixed window length
        window_size

        Returns
        -------
        dict
            Dict with forecasts/realisationen: Mu (predicted mean), Sigma (Predicted variance), PIT (CDF-transform of residuals),
                Resid (Unexplained error), Resid_std (Resid / Sigma)
    """
    model, rugarch = _get_rugarch_model(p=p, 
                                        q=q, 
                                        arch_p=arch_p, 
                                        arch_q=arch_q,
                                        dist=dist)

#    model = _get_rugarch_model(p=p, 
#                               q=q, 
#                               arch_p=arch_p, 
#                               arch_q=arch_q,
#                               dist=dist)
    
    # y=np.reshape(y,newshape=(1,-1))

    modelfit = rugarch.ugarchfit(spec=model, data=y, solver='hybrid', tol=1e-3) #hybrid
    #pitvals = np.asarray(rugarch.pit(modelfit))
    sigma_hist = np.asarray(rugarch.sigma(modelfit))
    mu_hist = np.asarray(rugarch.fitted(modelfit))
    #pred=np.asarray(rugarch.predict)
    fore= rugarch.ugarchforecast(modelfit)
    sigma=np.asarray(rugarch.sigma(fore))[0]
    mu=np.asarray(rugarch.fitted(fore))[0]



    #raise NotImplementedError('Residual computation überarbeiten!')
    #resid = np.asarray(rugarch.residuals(modelfit))
    #resid_std = np.asarray(rugarch.residuals(modelfit, standardize=True))
    return {'mu_hist': mu_hist, 'sigma_hist': sigma_hist, 'mu': mu, 'sigma': sigma} #,'PIT': pitvals, 'Resid': resid, 'Resid_std': resid_std}

p=5
q=3
arch_p=2
arch_q=3
ignore_first = 2000

lr1_res = r_garch_forecast(hist["lr1"][ignore_first:].values,
                           p=p, 
                           q=q, 
                           arch_p=arch_p, 
                           arch_q=arch_q)
lr2_res = r_garch_forecast(hist["lr2"][ignore_first:].values,
                           p=p, 
                           q=q, 
                           arch_p=arch_p, 
                           arch_q=arch_q)
lr3_res = r_garch_forecast(hist["lr3"][ignore_first:].values,
                           p=p, 
                           q=q, 
                           arch_p=arch_p, 
                           arch_q=arch_q)
lr4_res = r_garch_forecast(hist["lr4"][ignore_first:].values,
                           p=p, 
                           q=q, 
                           arch_p=arch_p, 
                           arch_q=arch_q)
lr5_res = r_garch_forecast(hist["lr5"][ignore_first:].values,
                           p=p, 
                           q=q, 
                           arch_p=arch_p, 
                           arch_q=arch_q)

mean = [lr1_res["mu"], lr2_res["mu"], lr3_res["mu"], lr4_res["mu"], lr5_res["mu"]]
sigma = [lr1_res["sigma"], lr2_res["sigma"], lr3_res["sigma"], lr4_res["sigma"], lr5_res["sigma"]]

def get_q_from_dist(mean, std, q=[0.025, 0.25, 0.5, 0.75, 0.975]):
    quantiles = []
    eps = 1e-5
    for i in range(len(mean)):
        quants = t.ppf(q, df=3, loc=mean[i], scale=std[i]+eps)
        quantiles.append(quants)
    return np.array(quantiles)


quantiles_val = get_q_from_dist(mean, sigma)