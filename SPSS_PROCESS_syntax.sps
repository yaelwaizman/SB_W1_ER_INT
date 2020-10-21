* Encoding: UTF-8.

*H4: PI group as the focal predictor, mean-centered suppression scores across all individuals as the moderator, sex and mean-centered age across all participants as covariates, and internalizing symptoms (CBCL  T-scores) as the outcome variable.
process cov = Sex Age_mc/y=CBCL_INT/x = Group/w = Supp_mc/model=1/boot = 10000/total=1/.

*H5: PI group as the focal predictor, mean-centered reappraisal scores across all individuals as the moderator, sex and mean-centered age across all participants as covariates, and internalizing symptoms (CBCL  T-scores) as the outcome variable.
process cov = Sex Age_mc/y=CBCL_INT/x = Group/w = Reapp_mc/model=1/boot = 10000/total=1/.

*Q1: PI group as the focal predictor, mean-centered suppression scores across all individuals as the mediator, sex and mean-centered age across all participants as covariates, and internalizing symptoms (CBCL  T-scores) as the outcome variable.
process cov = Sex Age_mc/y=CBCL_INT/x = Group/m = Supp_mc/model=4/boot = 10000/total=1/.

