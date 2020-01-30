1 In this simulation, we are looking at how family income affects student SAT scores. We are primarily interested in the following two
linear models:
SATi = 0 + 1incomei + ei (1)
SATi = 1incomei + 21(groupi = 1) + 31(groupi = 2) + 41(groupi = 3) + ei (2)
The first model is pooled and the second is a within-groups model.
2. We Run the pooled OLS model, the fixed-effects model, and individual models for each group separately. 
3 Model SAT using income, group, and both variables. 
4. We define the glmtree that best fits this data 
5. Using both the variables we should find the optimal number of groups using k-means
estimation (ignore scaling). Fit the k-means model and showing the correct means.
6. Is K means a good fit?
7. From this point, run the pooled model and the fixed-effects model using your endogeneously selected
groups.
8. Re-run the k-means estimation using only the income variable. 
9. Re-run the k-means estimation using both variables, but now scaling the variables beforehand. 
