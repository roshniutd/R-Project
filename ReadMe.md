Regression Analysis and plotting Correlation of various datasets using R

The data sets are available as tables in the wooldridge2.db file. 

Question 1
Use the data in the wage1 table for this exercise.
1. Find the average education level in the sample. What are the lowest and highest years of education?
2. Find the average hourly wage in the sample. Does it seem high or low?
3. The wage data are reported in 1976 dollars. Using the Economic Report of the President (2011 or
later), obtain and report the Consumer Price Index (CPI) for the years 1976 and 2010
4. Use the CPI values from above to find the average hourly wage in 2010 dollars. Now does the average
hourly wage seem reasonable?
5. How many women are in the sample? How many men?

Question 2
The data in meap01 table are for the state of Michigan in the year 2001. Use these data to answer the
following questions.
1. Find the largest and smallest values of math4. Does the range make sense? Explain.
2. How many schools have a perfect pass rate on the math test? What percentage is this of the total
sample?
3. How many schools have math pass rates of exactly 50%?
4. Compare the average pass rates for the math and reading scores. Which test is harder to pass?
5. Find the correlation between math4 and read4. What do you conclude?
6. The variable exppp is expenditure per pupil. Find the average of exppp along with its standard
deviation. Would you say there is wide variation in per pupil spending?
7. Suppose School A spends $6000 per student and School B spends $5500 per student. By what percentage
does School A’s spending exceed School B’s? Compare this to 100  [ln(6000) − ln(5500)], which is the
approximation percentage difference based on the difference in the natural logs.

Question 3
The data in 401k are a subset of data analyzed by Papke (1995) to study the relationship between participation
in a 401(k) pension plan and the generosity of the plan. The variable prate is the percentage of eligible
workers with an active account; this is the variable we would like to explain. The measure of generosity is the
plan match rate, mrate. This variable gives the average amount the firm contributes to each worker’s plan
1
for each $1 contribution by the worker. For example, if mrate = 0.50, then a $1 contribution by the worker
is matched by a 50? contribution by the firm.
1. Find the average participation rate and the average match rate in the sample of plans.
2. Now, estimate the simple regression equation:
prˆate = ˆ0 + ˆ1mrate,
and report the results along with the sample size and R-squared.
3. Interpret the intercept in your equation. Interpret the coefficient on mrate.
4. Find the predicted prate when mrate = 3.5. Is this a reasonable prediction? Explain what is happening
here.
5. How much of the variation in prate is explained by mrate?

Question 4
The data set in ceosal2 contains information on chief executive officers for U.S. corporations. The variable
salary is annual compensation, in thousands of dollars, and ceoten is prior number of years as company CEO.
1. Find the average salary and the average tenure in the sample.
2. How many CEOs are in their first year as CEO (that is, ceoten = 0)? What is the longest tenure as a
CEO?
3. Estimate the simple regression model
ln[salary] = 0 + 1ceoten + u
and report your results in the usual form. What is the (approximate) predicted percentage increase in
salary given one more year as a CEO?

Question 5
Use the data in wage2 to estimate a simple regression explaining monthly salary (wage) in terms of IQ score
(IQ).
1. Find the average salary and average IQ in the sample. What is the sample standard deviation of IQ?
(IQ scores are standardized so that the average in the population is 100 with a standard deviation equal
to 15.)
2. Estimate a simple regression model where a one-point increase in IQ changes wage by a constant dollar
amount. Use this model to find the predicted increase in wage for an increase in IQ of 15 points. Does
IQ explain most of the variation in wage?
3. Now, estimate a model where each one-point increase in IQ has the same percentage effect on wage. If
IQ increases by 15 points, what is the approximate percentage increase in predicted wage?

Question 6
Using the meap93 data, we want to explore the relationship between the math pass rate (math10) and spending
per student (expend).
1. Do you think each additional dollar spent has the same effect on the pass rate, or does a diminishing
effect seem more appropriate? Explain.
2. In the population model,
math10 = 0 + 1 ln[expend] + u
argue that 1/10 is the percentage point change in math10 given a 10% increase in expend.
2
3. Estimate this model. Report the estimated equation in the usual way, including the sample size and
R-squared.
4. How big is the estimated spending effect? Namely, if spending increases by 10%, what is the estimated
percentage point increase in math10?
5. One might worry that regression analysis can produce fitted values for math10 that are greater than
100. Why is this not much of a worry in this data set?

Question 7
Use the data in hprice1 to estimate the model
price = 0 + 1sqrft + 2bdrms + u,
where price is the house price measured in thousands of dollars.
1. Write out the results in equation form.
2. What is the estimated increase in price for a house with one more bedroom, holding square footage
constant?
3. What is the estimated increase in price for a house with an additional bedroom that is 140 square feet
in size? Compare this to your answer from above.
4. What percentage of the variation in price is explained by square footage and number of bedrooms?
5. The first house in the sample has sqrft = 2438 and bdrms = 4. Find the predicted selling price for this
house from the OLS regression line.
6. The actual selling price of the first house in the sample was $300000 (so price = 300). Find the residual
for this house. Does it suggest that the buyer underpaid or overpaid for the house?

Question 8
The file ceosal2 contains data on 177 chief executive officers and can be used to examine the effects of firm
performance on CEO salary.
1. Estimate a model relating annual salary to firm sales and market value. Make the model of the constant
elasticity variety for both independent variables. Write the results out in equation form.
2. Add profits to the model. Why can this variable not be included in logarithmic form? Would you say
that these firm performance variables explain most of the variation in CEO salaries?
3. Now also add the variable ceoten to the model. What is the estimated percentage return for another
year of CEO tenure, holding other factors fixed?
4. Find the sample correlation coefficient between the variables log(mktval) and profits. Are these variables
highly correlated? What does this say about the OLS estimators?

Question 9
Use the data in attend for this exercise. Create the variable atndrte which is attend/32 because there were
32 classes.
1. Obtain the minimum, maximum, and average values for the variables atndrte, priGPA, and ACT.
2. Estimate the model
atndrte = 0 + 1GPA + 2ACT + u
and write the results in equation form. Interpret the intercept. Does it have a useful meaning?
3. Discuss the estimated slope coefficients. Are there any surprises?
3
4. What is the predicted atndrte if priGPA = 3.65 and ACT = 20? What do you make of this result?
Are there any students in the sample with these values of the explanatory variables?
5. If Student A has priGPA = 3.1 and ACT = 21 and Student B has priGPA = 2.1 and ACT = 26,
what is the predicted difference in their attendance rates?

Question 10
Use the data in htv to answer this question. The data set includes information on wages, education, parents’
education, and several other variables for 1, 230 working men in 1991.
1. What is the range of the educ variable in the sample? What percentage of men completed 12th grade
but no higher grade? Do the men or their parents have, on average, higher levels of education?
2. Estimate the regression model
educ = 0 + 1motheduc + 2fatheduc + u
by OLS and report the results in the usual form. How much sample variation in educ is explained by
parents’ education? Interpret the coefficient on motheduc.
3. Add the variable abil (a measure of cognitive ability) to the regression above, and report the results in
equation form. Does ability help to explain variations in education, even after controlling for parents’
education? Explain.
4. Now estimate an equation where abil appears in quadratic form:
educ = 0 + 1motheduc + 2fatheduc + 3abil + 4abil2 + u.
With the estimated coefficients on ability, use calculus to find the value of abil where educ is minimized.
(The other coefficients and values of parents’ education variables have no effect; we are holding parents’
education fixed.) Notice that abil is measured so that negative values are permissible. You might also
verify that the second derivative is positive so that you do indeed have a minimum.
5. Argue that only a small fraction of men in the sample have ability less than the value calculated
above. Why is this important?
6. Use the estimates above to plot the relationship beween the predicted education and abil. Let motheduc
and fatheduc have their average values in the sample, 12.18 and 12.45, respectively.


The above series of questions are analysing various datasets and extracting the level of correlation between dependant variables and independant variables and plotting the relation between them using R



