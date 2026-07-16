intro<-"<h3>Welcome</h3>
<p>This app is a tool for studying the behavior of regression weight estimates involving regression
models with two predictors and their interaction. It is designed to be a decision help for determining
the feasibility of different research objectives involving the multiple regression model with an
interaction term under <em>specified data generating processes</em>. It also aims to give insight in the
sometimes quite unexpected effects that changes (minor or bigger ones) in the data characteristics
have on the estimates. In this way it gives insight in the stability of different model metrics given the
kind of data that one has.</p>
<h3.How to use the app?</h3>
<p>To use the app you have to do the following</p>
<ul>
<li>Go to the tab <strong>continuous-continuous</strong> if both of your predictor variables are continuous.</li>
<li>Go to the tab <strong>continuous-categorical</strong> if one of your
predictor variables is continuous, and the other is categorical.</li>
<li>In the tab you first have to go to <strong>Data Generation</strong> where you
find the subheading <strong>Descriptive Statistics</strong> and
<strong>True Effects</strong>. Please specify the required statistics, such as <em>means,
variance, and covariance between predictors</em>, specify <em>reliability parameters</em>,
and the expected <em>true effects</em>.</li>
<li>This input is used for the following computations.</li>
<li>Effects on population level can be found following <strong>Population Level</strong>.</li>
<li>Effects on sample level can be found following <strong>Sample Level</strong>.</li>
</ul>
<h3>Why should I use the app?</h3>
<p>The app shows you the comparison of two different linear regression models.
In the computations throughout the app we assume that a true interaction effect,
which can correctly be modelled as a product between predictor variables, exist.
We also assume that the data we observe and collect is subject to measurement error.
As said before, we compare to different linear regression models under these conditions.
In the first model, the existing interaction effect is modeled.
In the second model this interaction effect is omitted.
There are benefits of both models under certain conditions.</p>
<p>However, measurement error makes it difficult to anticipate the effects on either model.
Under some conditions leaving out an interaction effect can lead to qualitatively
drastically different conclusions than when interaction effects are included.
But it can also happen, that under the same conditions both models cannot reliably
differentiated. Not modelled interactions, can lead to more consistent results in
replication attempts, however this does not guarantee that these results are more valid.
And to complicate things, it can also be possible that if one were to use both models
for prediction, the difference is negligible.</p>
<p>To guide researcher in their decision of whether to model a suspected interaction
effect (because we never no the true data generation process behind our data) or
to not model an interaction we have developed this app, which allows researchers
to explore both linear regression models, under error conditions on all these properties.</p>
<h3>What can I explore in the app?</h3>
<p>We provide the <em>expected</em> <strong>regression weights</strong>,
the <strong>bias</strong> of the regression weights for both models. This gives an indication
whether qualitatively different conclusions can be expected.</p>
<p>We also provide the expected <strong>standard errors of regression weights</strong> and thus
the <strong>precision of the estimates</strong>, which can serve as an indicator for the replicability
of effects.</p>
<p>We provide <strong>residual related metrics</strong> such as the expected explained
variance <strong>R²</strong>(relative prediction accuracy), as well as prediction accuracy as
measured in the <strong>root mean squared error</strong> or approximations of those on
population- and sample level.</p>
<p> We also provide the expected <strong>variability of R²</strong>
for both models, which indicates how well the models can be differentiated.</p>


"


INS_descriptives<-"<p>Before you can use the app you have to fill in some descriptives statistics,
which will be used for defining the data characteristics in the population. All  computations in
the app are based on this defined population.
Required statistics are
<ul>
<li> The means of the predictor variables </li>
<li> The variance of the predictor variables </li>
<li> The reliability of the predictor variables </li>
<li> The covariance between the predictor variables </li>
</ul>
These descriptive statistics can be best obtained by validation studies, previous published research, or conducted pilot studies. </p>
"


INS_effects<-"<p>Similar to the descriptive statistics, you have to define the effect sizes first.
The effect sizes you define here are the assumed true regression weights.
Throughout the article we have established that empirically found effects are fragile.
Nonetheless, these are the best proxies for true effects that we have.
The true R² indicates the variance that would be explained by the three included
predictors if no measurement error were present, no other related variables were omitted,
and the reduction of R² can be considered random noise in the outcome.
It is assumed that the unexplained part of the R² is independent of the predictors and errors.</p>
<p>As for the descriptive statistics, the effect sizes can be obtained from previous research or pilot studies. </p>"

INS_uncertainty_true_effects<-
  "<p>Given that the specified 'true' effects are linked with uncertainty you can also indicate
  the standard error linked to these effect sizes. We can use this information to
  show how much the effects could vary under the empirically found uncertainty.
It is important to keep in mind, that the uncertainty, but also effect sizes are
always linked to the model that was applied.</p>"

EXPL_3d_vis<-"<p>In the following you can see a 3 dimensional visualization relating
to the previously specified effects (including descriptive statistics).
The reason why we use a 3 dimensional visualization is that interaction effects
are <em>non-linear effects</em> across the two predictor variables. This is less
visible in a 2 dimensional visualization such as a simple slopes plot.
A 3d visualization has the advantage that the curvature/non-linearity becomes
more visible. In general it can be said that the more curvature in the plane, the
stronger the underlying interaction.</p>
<p>You can use the input on the left to rotate the Figure.</p>"


EXPL_contour<-"<p>The two contour plots on the left illustrate how the estimated
simple effects (b₁ and b₂) change when accounting for the uncertainty
in the other effects Each horizontal line represents an estimated value of the
simple effect, and the accompanying numbers indicate how this estimate shifts
as the other simple effect and the interaction effect vary within their uncertainty
ranges.
Because the underlying model includes only two main effects and one interaction,
these contours are flat. Conceptually, these plots can be interpreted
similarly to simple slope plots, showing that the main effects remain robust while
the interaction effect introduces only limited distortion.</p>
<p>If you do not see any lines in the figure, it means that the effect is
constant at these settings. The reason for this may be that the true means are 0,
you are using mean centered effects, or you are encountering a knock-out effect.</p>"

EXPL_weights_pop<-"<p> The first table on the left gives the expectation of the
regression weights that you would obtain under the previously specified input.
These weights are based on infinite sample sizes. This means that with limited
sample sizes the results will be scattered around these values.
This scatter will decrease with increasing sample sizes. As just said one way to
get more precise estimates is to increase the sample size, the other way
is (if possible) collect data repetitively, repeat the same analysis on each sample,
and take the means of each estimate for all analyses.</p>
<p>In the table each row corresponds to a different model:</p>
<ul>
<li><strong>The full model, without error</strong> in the first row refers to the
model in which no measurement error is present and the model is correctly specified.
This model refers to the true effects. </li>
<li><strong>The full model, with error</strong> in the second row refers to the model
which is correctly specified, but measurement error is present and not accounted for.
Comparing this row to the true effects in row 1 indicates the bias of the effects,
solely due to measurement error in the predictors.</li>
<li><strong>The misspecified model, without error</strong> in row 3 refers to the
model when an existing interaction effect is omitted, but no measurement error is present.
Comparing this row to the true effects in row 1 indicates the omitted variable bias.</li>
<li><strong>The missspecified model, with error </strong> in row 4 refers to the situation when an existing interaction effect is omitted and measurement error is present.
Comparing this row to the true effects in row 1 indicates the joint bias due to measurement error and model misspecification. </li>
</ul>
<p>For explanatory research the bias of these effecs should be as small as possible--- in particular harmful is when regression weights have opposing signs,
because in this case very different qualitative conclusions would be drawn depending on the model applied.</p>
<p>Because the bias, so the difference between the true effects and the estimated effects, should be as small as possible for explanatory research we
also provide the expected bias in the second table on the left. This table is organized as the previous table, but now shows
the bias instead of expected regression weight. As you may have noticed,
the values in the second table are based on those in the first, by subtracting the
values in its first row (the true effect sizes) from the effect sizes in the other rows.</p>"
EXPL_accuracy_pop<-"
<p>Here, we can see different metrics related to the prediction accuracy in expectation.
Specifically, we provide the  <strong>population R²</strong> for an infinite sample,
and the population <strong>MSE</strong> (mean squared error) and <strong>RMSE</strong>
(root mean squared error) for an infinite sample. While  R² is no direct measure
of prediction accuracy it is closely related, because it is the <em>prediction
accuracy relative to the mean model of the outcome</em>.
On a finite sample  computing the sample R² is known to be an overestimation of
the population  R², thus procedures such as cross-validation are used to get a
more precise estimate of the population  R².
However, here we compute the actual population  R². Hence, this is the value one
could best obtain by different procedures such as cross-validation.</p>
<p>We decided to provide the RMSE, as an actual measure of prediction accuracy,
and (the population)  R² as a relative measure of prediction accuracy, because
both these measures are well known
and standard output for linear regression, and thus we assume known better by
people who are more fmamiliar with using regression in an explanatory context,
but also in a predictive context.</p>
<p>It is important to note, that all these metrics in this section are based on
infinite samples and typically obtained sample estimates of these metrics are biased.
Thus we later also provide metrics related to finite samples. </p>
<p>First, in the table on the left, you can see the numeric values of these
metrics for the two different models under measurement error.</p>
<p>The first heatmap below shows the difference between the population  R² if
the reliability coefficients of the predictors are varied.
The reliability parameters for this heatmap  range from 0.1 (left, bottom) to 1
(top, right). We provide different reliability parameters, because obtaining
correct reliability estimates in social science is often not feasible in social
and behavioral sciences.
The largest difference should always be located in the top right corner(perfectly
reliable predictors), and the smallest difference should always be in the left
bottom corner (very small reliability).
The smaller the population  R² difference is the more difficult it is to
differentiate the two models from each other. However, the smaller the difference
between  population R² the less predictive gain from adding
the interaction effect. Important to notice is, that even though the difference
in population  R² might be marginal, this does not guarantee that the model is
not qualitatively different (see previous section on regression weights and bias).</p>
<p> The second heatmap shows the difference in population RMSE across different
reliability parameters. This figure is similar to the previous figure about
the population  R², however shows an absolute measure of prediction accuracy.
Important to note is, that the RMSE is on the same scale as the outcome variable.
Standardizing the population RMSE to make it comparable across different data
generating mechanisms would simply yield a transformation of the population  R².
</p>
"

EXPL_sample_vs_pop<-"
<p>So far, we have considered different metrics on a population level Meaning,
how models behave if we could observe an infinite number of data points drawn
from the true data-generating process.
In that case, quantities such as R², bias, or MSE represent their expected or
long-run values. </p>
<p>In reality, however, we only have samples, so finite subsets of the population.
Many of the metrics that we typically estimate form samples, are <em>themselves
biased estimates of the population metric </em>. This is the reason that many
sample estimates are corrected for example for
degress of freddom or sample size. These correction methods are used, to get more
precise or unbiased estimates for the population. </p>
<p>A well known example is the sample R². If we apply multiple linear regression
models with differing numbers of predictor variables and compute R² for each model,
R² will always be larger for the more complex model.
However, this does not mean that the population R² for the more complex model is
always larger. To get a better estimate for the population R², we can take the
degress of freedom of the model, as well as the sample size into account.
If we take both of them into account we end up with R² adjusted. Because R²
adjusted takes the degress of freedom (number of predictors) of the regression
model into account, and the number of predictors differ between the models R²
adjusted can be
larger for the less complex model.
If we apply multiple linear regression models with different numbers of predictor
variables and compute R² for each model in the sample,
the more complex model will always have a higher R² (even if just in the 10th digit).
However, this does not necessarily mean that this model would also achieve a
larger R² in the <em>population</em>.
To better approximate the population R², we can adjust for the model’s degrees of
freedom and the sample size,
leading to the <strong>adjusted R²</strong>.
Because adjusted R² takes the degrees of freedom of the model into account
(penalizing additional predictors), it can be larger for a simpler model when
variables are added to a model, without increasing R² substantially.</p>

Due to the differing behavior of some metrics on sample and population level we
might sometimes provide different, but related, metrics for the sample expectations.
We will always explain why we chose the different metric, and how it relates to
the metric we provide for the population level.</p>
"

EXPL_est_precision_sample<-"<p>In this section it is possible to explore the
effects of the sample size on the precision of the regression estimates. On the
left you find a text field in which you can specify a sample size of interest.
The table will show you the expected standard errors for the regression estimates
under the specified data generating process for both models.
The expected regression estimates do not differ from the ones shown previously---
only the precision of these estimates change depending on the sample size.</p>"
EXPL_est_precision_sample_figure<-"<p>In contrast to the table, where you get
precise numeric values of the standard errors we also provide figures which
indicate the
regression estimates with associated 95% Confidence Intervals based on the
sampling dependent standard errors. These figures do not depend on the sample
size chosen previously.</p>
<p>EXPLAIN CI STUFF </p>"

EXPL_r2_comp<-"<p>
The differentiability of regression models typically depend in one way or another
on the comparison of the residuals for different regression models. Common metrics
include the Akaike information criterion (AIC) or
Bayesian information criterion (BIC), where model complexity is differently penalized.
However, a common method is also the statistical comparison of R².</p>
<p>On the left, we provide a figure which indicates the expected sample R²
following Olkin and Finn, including the associated standard deviation, across
different sample sizes.
The closer the expected sample R² are the harder it is to differentiate these
two models statistically.</p>
"

EXPL_r2_diff_sample<-"<p>
As explained earlier the sample R² is an overestimate of the population R². Thus, different methods to correct for this overestimation have been established.
In the first figure on the left we show the expected sample R² difference following Olkin and Finn. As with the previous figure the difference indicates
the differentiatability of the two models, but also the expected relative prediction accuracy gains of including an interaction effect.
The figure shows the expected sample R² difference across different reliability parameters of the predictors.
Blue indicates that the expected sample R² is larger for , white indicates that both models are equal, and red indicates that the model excluding the interaction effect performs better.

</p>"

EXPL_r2_adj_diff_sample<-"<p>
As explained earlier the sample R² is an overestimate of the population R². Thus, different methods to correct for this overestimation have been established.
In the first figure on the left we show the expected difference for R² adjusted on sample level across different reliability parameters of the predictors.
This indicates the relative prediction accuracy gains of including an interaction effect.
Blue indicates that the expected R² adjusted is larger for the mdoel which includes an interaction effect, white indicates that both models are equal,
and red indicates that the model excluding the interaction effect performs better.
For these figures it is important to keep in mind, that even small performance gains can lead to 'blue', this does not necessarily reflect empirically relevant
gains of prediction accuracy.
</p>"

EXPL_rse_diff_sample<-"<p>
To give a measure for actual prediction accuracy we provided the RMSE for the population level. However, for the comparison on sample level the RMSE is less suitable,
because the samplesize and number of predictors do not matter in our analytical framework.
Instead of the RMSE we provide the difference of residual standard errors (RSE) as an indicator for prediction accuracy gains, because the model complexity and
sample size can be taken into account. However, with an increase in samplesize the RSE approaches the RMSE asymptotically.
This is conceptually similar to the correction of R² adjusted to R².</p>

<p>
In the figure on the left, we show the expected difference in RSE across different reliability parameters of the predictors.
This reflects how the residual variability changes between the two models.
Blue areas indicate that the expected RSE is smaller for the model including the interaction effect, white indicates equality,
and red indicates that the model excluding the interaction performs better.
As before, even small absolute differences can appear visually pronounced, so 'blue' areas do not necessarily reflect substantial practical gains in prediction accuracy.
</p>"
