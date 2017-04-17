
## some global variables

themes <- shinyAce::getAceThemes()

## for lavInspect
li_modmat <- c("","free", "partable", "standard.errors", "starting.values", 
               "estimates", "dx.free", "dx.all", "standardized", "std.lv",
               "std.nox")
li_infdata <- c("","data", "group", "ngroups", "group.label", "cluster", "ordered",
                "nobs", "norig", "ntotal", "case.idx", "empty.idx","patterns",
                "coverage")
li_obssampstat <- c("","samplestatistics", "sampstat.h1", "wls.obs", "wls.v",
                    "gamma")
li_modfeatures <- c("","meanstructure", "categorical", "fixed.x", "parameterization")
li_modimpliedstat <- c("","fitted", "residuals","cov.lv", "cor.lv", "mean.lv", 
                       "cov.ov", "cor.ov", "mean.ov", "cov.all", "cor.all", 
                       "thresholds", "wls.est", "vy", "rsquare")
li_optimizer <- c("","converged", "iterations", "optim", "npar", "coef")
li_gradhessinfo <- c("","gradient", "hessian", "information", "information.expected",
                     "information.observed", "information.first.order",
                     "augmented.information", "augmented.information.expected",
                     "augmented.information.observed", "augmented.information.first.order",
                     "inverted.information", "inverted.information.expected",
                     "inverted.information.observed", "inverted.information.first.order")
li_vcovmodpar <- c("","vcov", "vcov.std.all", "vcov.std.lv", "vcov.std.nox")
li_misc <- c("","UGamma", "list", "fit.measures", "modification.indices", "options",
             "call", "timing", "test", "post.check", "zero.cell.tables")
li_alias <- c("se", "std.err","start", "est", "x", "std", "std.all", "sampstat",
              "samp", "sample", "h1", "missing.h1", "sampstat.nacov", 
              "implied", "res", "resid", "residual","veta", "eeta", "sigma", 
              "sigma.hat", "mu", "mu.hat", "th", "r-square", "r2", "fit", 
              "fitmeasures", "fit.indices", "mi", "modindices")
li_all <- c(li_modmat, li_infdata[-1], li_obssampstat[-1], li_modfeatures[-1], 
            li_modimpliedstat[-1], li_optimizer[-1], li_gradhessinfo[-1], 
            li_vcovmodpar[-1], li_misc[-1])
li_all_alias <- c(li_all, li_alias)

