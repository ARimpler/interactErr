# Home --------------------------------------------------------------------
intro <- "<h3>Welcome</h3>
<p>This app helps you study how two regression models behave when a <strong>true interaction effect</strong>
is present in the data generating process and predictors are subject to <strong>classical measurement error</strong>.
The <em>full model</em> includes the interaction term; the <em>reduced model</em> omits it. Both are common analyst
choices — but under measurement error, their behavior relative to each other is often counterintuitive, and this
app is built to make the behavior of both models explorable before you collect data.<br>
  The computations are based on the derivations in this
<a href=https://doi.org/10.31234/osf.io/s47xz_v2>preprint, which provides the full technical derivations</a>.
The app is under active development; future versions may differ and bugs may be present.</p>
<h3>Who this is for</h3>
  <p>This tool works from <strong>user-specified true parameters</strong>: true effects, common descriptive statistics, and
reliability, rather than from your own collected data. It is intended for <strong>planning</strong>: exploring what you would
expect to find under plausible true data-generating processes, before conducting a study, if you suspect an interaction effect
may be present and your measures are imperfectly reliable.
Importantly, we provide information that is relevant if you want to
<strong>simplify or model the suspected complexity</strong>. </p>
<h3>How to use the app?</h3>
<p>To use the app you have to do the following</p>
<ul>
<li>Go to the tab <strong>Theoretical Effects</strong> if your analysis will be based on theoretically derived effects.</li>
<li>In the tab you first have to go to <strong>Data Generating Model</strong> where you
find the subheading <strong>Descriptive Statistics</strong> and
<strong>True Effects</strong>. Please specify the required statistics, such as <em>means,
variance, and covariance between predictors</em>, specify <em>reliability parameters</em>,
and the expected <em>true effects</em>.</li>
<li>This input is used for the following computations:</li>
<li>Effects on population level can be found following <strong>Population Level</strong>.</li>
<li>Effects on sample level can be found following <strong>Sample Level</strong>.</li>
<li>The probability of sign flipped regression estimates within a specified data range can be found in the tab <strong>Sign Flips</strong>. </li>
</ul>

<h3>Why this matters</h3>
<p>When modelling effects where analysts assume that the impact of a predictor on the outcome depends on a third variable,
product-like interactions are a common choice. These terms are notoriously prone to failing to replicate, especially when predictors are measured with error.
But omitting the interaction when it truly exists can badly bias the remaining regression weights. This means the
model's estimate can diverge substantially from the effect for a given subgroup.</p>
<p>However, much research is not about the average. Psychopathology, for example, is often defined by deviation from the average.
So a key question is: does simplifying a model to improve replicability risk producing misleading or harmful
conclusions for a subgroup at the extremes?</p>
<p>Measurement error makes this hard to anticipate. Sometimes omitting an interaction changes the practical
conclusions substantially. Other times, the two models are statistically barely distinguishable, despite
implying different, even oppositely-signed, effects in parts of the data. And all this while the global
prediction errors from both models may barely differ at all. This app is built to help you to anticipate these situations
and when possible to tell them apart,for your specific combination of effects, reliability, and predictor range,
before you run the study.</p>
<p>Another complication can be that some facts about purely additive models, without any non-linear effects, may not hold
for misspecified models as a result many intuitions can be misleading.
As an example, in a strictly linear model means generally do not matter, however this is not
the case if we used a strictly linear model to nonlinear data for pragmatic or statistical reasons. </p>
</p>

  <h3>What you can explore</h3>
  <p>The app provides:</p>
  <ul>
  <li><strong>Expected regression weights and their bias</strong> for both the full and reduced model, showing
whether the two models imply qualitatively different conclusions.</li>
  <li><strong>Visualizations</strong> of the specified <strong>true data generating mechanism</strong> and
  <strong>simplified model</strong>, also for the <strong>expected observed</strong> data.
  <li>Choosing <strong>Evaluation Points</strong> of the model, this is implied by re-parameteraizations such
  as mean centering and gives a simple tool to <strong>compare raw vs mean-centered models</strong>
  <li><strong>Residual-based metrics</strong>, R² and RMSE/MSE at the population level and, via
simulation, at realistic sample sizes.</li>
  <li><strong>Variability of these metrics across samples</strong>, indicating how reliably the two models can be
told apart in practice.</li>
  <li><strong>Sign-flip probabilities</strong> — how likely an estimated effect is to change direction within your
specified, realistic data range.</li>
  </ul>
"


# DGM ---------------------------------------------------------------------

INS_descriptives <- "
<p>Before using the app, specify the descriptive statistics that define your population of interest.
These describe the <strong>true</strong> predictor variables — i.e., the values you would observe if there
were no measurement error. All computations in the app build on this specification.</p>
The required statistics are:
<ul>
<li> The means of the predictor variables </li>
<li> The variance of the predictor variables </li>
<li> The reliability of the predictor variables </li>
<li> The covariance between the predictor variables </li>
</ul>
<p>Note that the covariance is constrained by the two variances (it cannot exceed
&radic;(Var(X)&middot;Var(Z)) in absolute value) and the variances and reliability cannot be zero—
the app will warn you if this is violated. </p>
<p>The descriptive statistics are best obtained through validation studies, previous published research or pilot studies. </p>
"


INS_effects <- "
<p>Next, specify the assumed true effects. These are the regression weights you believe would be recovered
in an infinite sample, if predictors were measured without error and the model — including the interaction —
were correctly specified.</p>

<p>As discussed in the accompanying preprint, empirically estimated effects are often fragile, particularly for
interaction terms. Nonetheless prior empirical findings remain the best available proxy for true effects and there
has been much discussion and literature on how to best establish the effects in the context of power analyses and
smallest effect size of interest, which also require pre-defined effect sizes.</p>

<p>The <strong>true R²</strong> reflects the variance explained by the three predictors (both main effects and
the interaction) under three conditions:</p>
<ul>
<li>no measurement error is present in the predictors,</li>
<li>no relevant variables correlated with the predictors have been omitted, and</li>
<li>the remaining unexplained variance is independent of the predictors, the outcome, and any measurement error.</li>
</ul>
<p>In other words, true R² represents the ceiling on explained variance from a correctly specified, error-free
model. Any gap between this and what you would observe in practice is attributable to measurement error and/or
model misspecification, both of which the app lets you explore.</p>
</p>"

INS_3d_vis <- "<p>Specify the range of the two predictor variables to control the plotted region, and choose an
<strong>evaluation point</strong> (a specific combination of X and Z) at which the two tangent lines are drawn.</p>
<p>The tangent lines are the <em>simple slopes</em> of the interaction (full) model at your chosen evaluation
point: one showing how the surface changes along X (holding Z fixed), the other along Z (holding X fixed).
Moving the evaluation point lets you read off the effect of one predictor at any specific level of the other,
including comparing the raw-scale model (evaluation point at the predictors' actual values) to a mean-centered
model (evaluation point at (0,0) after centering) or a theoretically meaningful point .</p>
<p>The plot can be rotated and zoomed using your mouse.</p>"


# Visualization -----------------------------------------------------------

BACKINF_3d_vis <- "<p>The purple surface shows the model that includes the interaction effect (full model);
the green surface shows the model that excludes it (reduced model).</p>
<p>Interaction effects are <em>non-linear</em> across the two predictors, which is difficult to see in a
2-dimensional plot such as a simple slopes plot. A 3-dimensional surface makes this curvature directly visible:
broadly, the more the plane curves within the specified data range, the stronger the underlying interaction
effect is within that range.</p>
<p>The tangent lines let you inspect this more precisely: they show the simple slope at a chosen evaluation
point, which is useful for interpreting effects at meaningful thresholds — for example, a clinical cut-off score
— rather than only at the variables means or sero points.</p>"

ASSUMPTIONS_3d_vis <- "<p>This visualization plots the regression surfaces implied by the parameters
specified elsewhere in the app. Depending on which tab you are viewing this from:</p>
<ul>
<li>On <strong>Data Generating Model</strong>, the surfaces reflect the <strong>true</strong>, error-free
parameters you specified.</li>
<li>On <strong>Population Level</strong>, the surfaces reflect the parameters as they would be
<strong>estimated on average given measurement error</strong> — i.e., observed-scale surfaces,
not the true ones.</li>
</ul>
<p>If viewed on <strong>Population Level</strong> the  same assumptions underlying the computations apply.</p>"


# Inifnite ----------------------------------------------------------------

# EXPL_weights_pop <- "<p> The first table on the left gives the expectation of the
# regression weights that you would obtain under the previously specified input.
# These weights are based on infinite sample sizes. This means that with limited
# sample sizes the results will be scattered around these values.
# This scatter will decrease with increasing sample sizes. As just said one way to
# get more precise estimates is to increase the sample size, the other way
# is (if possible) collect data repetitively, repeat the same analysis on each sample,
# and take the means of each estimate for all analyses.</p>
# <p>In the table each row corresponds to a different model:</p>
# <ul>
# <li><strong>The full model, without error</strong> in the first row refers to the
# model in which no measurement error is present and the model is correctly specified.
# This model refers to the true effects. </li>
# <li><strong>The full model, with error</strong> in the second row refers to the model
# which is correctly specified, but measurement error is present and not accounted for.
# Comparing this row to the true effects in row 1 indicates the bias of the effects,
# solely due to measurement error in the predictors.</li>
# <li><strong>The misspecified model, without error</strong> in row 3 refers to the
# model when an existing interaction effect is omitted, but no measurement error is present.
# Comparing this row to the true effects in row 1 indicates the omitted variable bias.</li>
# <li><strong>The missspecified model, with error </strong> in row 4 refers to the situation when an existing interaction effect is omitted and measurement error is present.
# Comparing this row to the true effects in row 1 indicates the joint bias due to measurement error and model misspecification. </li>
# </ul>
# <p>For explanatory research the bias of these effecs should be as small as possible--- in particular harmful is when regression weights have opposing signs,
# because in this case very different qualitative conclusions would be drawn depending on the model applied.</p>
# <p>Because the bias, so the difference between the true effects and the estimated effects, should be as small as possible for explanatory research we
# also provide the expected bias in the second table on the left. This table is organized as the previous table, but now shows
# the bias instead of expected regression weight. As you may have noticed,
# the values in the second table are based on those in the first, by subtracting the
# values in its first row (the true effect sizes) from the effect sizes in the other rows.</p>"

INS_infinite <- "<p>The table shows the regression weights you would obtain under the input you specified,
in an infinite sample. With a finite sample, estimates will scatter around these values; that scatter
shrinks as sample size increases (explored further on the Sample Level tab).</p>
<p>Each row corresponds to a different model:</p>
<ul>
<li><strong>Full model, without error</strong> (row 1): no measurement error, correctly specified model.
These are the true effects you specified.</li>
<li><strong>Full model, with error</strong> (row 2): correctly specified model, but measurement error is
present and not accounted for. Comparing this row to row 1 shows the bias due to measurement error alone.</li>
<li><strong>Misspecified model, without error</strong> (row 3): the interaction effect is omitted, but no
measurement error is present. Comparing this row to row 1 shows the omitted-variable bias.</li>
<li><strong>Misspecified model, with error</strong> (row 4): the interaction effect is omitted <em>and</em>
measurement error is present. Comparing this row to row 1 shows the joint bias from both sources combined.</li>
</ul>
<p>The second table shows this same comparison directly as bias: each value is the corresponding row in the
first table minus the true effect (row 1).</p>"



BACKINF_infinite <- "
<p>Measurement error in predictors and omitting a true interaction effect are each
well studied in isolation — their consequences for regression weights and prediction accuracy are individually
well understood. What is less studied is what happens when <strong>both occur together</strong>, which is the
realistic situation in most applied settings. One of the consequences form both modeling challenges is
that regression estimates become biased.</p>
<p>For explanatory research, bias should be as small as possible. Particularly
consequential is when a regression weight's <strong>sign flips</strong> relative to the true effect. This
means the model would lead to a qualitatively different conclusion depending on which specification was used.</p>
<p>These two sources of bias do not simply add up. Measurement error alone leaves means untouched, but the bias
from omitting an interaction effect depends directly on where the predictors are centered — the two forms of
bias interact in ways that are difficult to anticipate from either source alone. This page lets you see the
combined effect directly, for your specified true parameters.</p>
"

ASSUMPTIONS_infinite <- "
<p>
For the computations on this page we make the following assumptions:
<ul>
<li> the effects in the Tab `Data Generating Mechanism' are the true effects </li>
<li> the error in the predictor variables is classical measurement error</li>
<li> the unexplained variance in the `Data Gnerating Mechanism` tab is independent of the predictors</li>
<li> at least one of the predictors is symmetricly distributed</li>
</ul>
</p>"


INS_accuracy_pop <- "<p>The table on the left shows the numeric values of R², MSE, and RMSE for each model,
under measurement error, at an infinite sample size.</p>
<p>The first heatmap shows the difference in population R² between the full and misspecified model as the
reliability of each predictor varies from 0.1 to 1. The second heatmap shows the same comparison for MSE and the
third for RMSE.</p>"

BACKINF_accuracy_pop <- "<p>R² is not a direct measure of prediction accuracy, but it is closely related: it
reflects prediction accuracy relative to a model that only predicts the outcome's mean. On a finite sample,
sample R² is known to overestimate the population R², which is why procedures like cross-validation are used
to approximate the population value more precisely. The values shown here are the actual population R² —
i.e., what such procedures aim to estimate, computed directly rather than approximated.</p>
<p>We report MSE, RMSE (direct, outcome-scale dependend measures of prediction accuracy) and R²
(a relative, scale-free measure), since both are standard, widely understood output for linear regression in both explanatory and
predictive contexts.</p>
<p>All metrics on this page reflect infinite-sample values; sample-based estimates of these same metrics are
generally biased at finite sample sizes, which can be explored on the Sample Level tab.</p>
<p>The reliability parameters in the heatmaps are varied because obtaining accurate reliability estimates is
often difficult in the social and behavioral sciences. Typically, the largest R²/RMSE difference between models
occurs at high reliability (top-right of each heatmap) and the smallest at low reliability (bottom-left).</p>
<p>A small population R² (or RMSE) difference means the two models are difficult to distinguish empirically —
but this does <strong>not</strong> guarantee they are not qualitatively different (see the regression weights
and bias tables above). Standardizing RMSE for comparability across different data-generating processes would
simply yield a transformation of R², so we report both in their natural, unstandardized form.</p>"

# Finite ------------------------------------------------------------------
INS_finite <- "<p>Specify a random seed (for reproducibility note your seed down as the
default seed used depends on the date when the simulation was conducted) and the number of simulation iterations, then
click <strong>Run Simulation</strong>. The app will simulate data at several sample sizes under your specified
data-generating process, fit both the full and reduced model to each simulated sample, and summarize the
resulting estimates and fit statistics.</p>
<p>Optionally, enable <strong>skewed predictors</strong> to explore what happens when the true predictors are
not normally distributed (see Assumptions) — this changes only the simulated data, not the population-level
values shown elsewhere in the app.</p>
<p> Importantly, for skewed predictors some of the patterns do not need to match
the previously shown values. </p>"

ASSUMPTIONS_finite <- "<p>The computations on this page rely on the following:</p>
<ul>
<li>The effects specified in the <strong>Data Generating Model</strong> tab are the true effects.</li>
<li>Errors in predictors are <strong>classical measurement error</strong>.</li>
<li>The unexplained variance specified via true R² is independent of the predictors, the outcome, and the
measurement error.</li>
<li>By default, predictors are simulated as jointly normal, matching the assumption used in the Population
Level tab. Enabling <strong>skewed predictors</strong> allows to use simulate skewed
normal distributed predictors. In that case, results here reflect a
non-normal true distribution and may differ from the population-level values shown elsewhere.
<li> AIC and BIC values are naively computed as if the residuals were normally distributed,
which is not the case for the simplified model, representing many software defaults.</li>
</ul>"

# INS_finite<-"
# <p>So far, we have considered different metrics on a population level Meaning,
# how models behave if we could observe an infinite number of data points drawn
# from the true data-generating process.
# In that case, quantities such as R², bias, or MSE represent their expected or
# long-run values. </p>
# <p>In reality, however, we only have samples, so finite subsets of the population.
# Many of the metrics that we typically estimate form samples, are <em>themselves
# biased estimates of the population metric </em>. This is the reason that many
# sample estimates are corrected for example for
# degress of freddom or sample size. These correction methods are used, to get more
# precise or unbiased estimates for the population. </p>
# <p>A well known example is the sample R². If we apply multiple linear regression
# models with differing numbers of predictor variables and compute R² for each model,
# R² will always be larger for the more complex model.
# However, this does not mean that the population R² for the more complex model is
# always larger. To get a better estimate for the population R², we can take the
# degress of freedom of the model, as well as the sample size into account.
# If we take both of them into account we end up with R² adjusted. Because R²
# adjusted takes the degress of freedom (number of predictors) of the regression
# model into account, and the number of predictors differ between the models R²
# adjusted can be
# larger for the less complex model.
# If we apply multiple linear regression models with different numbers of predictor
# variables and compute R² for each model in the sample,
# the more complex model will always have a higher R² (even if just in the 10th digit).
# However, this does not necessarily mean that this model would also achieve a
# larger R² in the <em>population</em>.
# To better approximate the population R², we can adjust for the model’s degrees of
# freedom and the sample size,
# leading to the <strong>adjusted R²</strong>.
# Because adjusted R² takes the degrees of freedom of the model into account
# (penalizing additional predictors), it can be larger for a simpler model when
# variables are added to a model, without increasing R² substantially.</p>
#
# Due to the differing behavior of some metrics on sample and population level we
# might sometimes provide different, but related, metrics for the sample expectations.
# We will always explain why we chose the different metric, and how it relates to
# the metric we provide for the population level.</p>
# "

BACKINF_finite <- "<p>So far, we have considered metrics at the population level, meaning how models behave
if we could observe an infinite number of data points drawn from the true data-generating process. </p>
<p>In reality, we only ever observe finite samples. Many metrics estimated from a sample are themselves
<em>biased estimates of the population metric</em> — which is why estimators are often corrected for degrees
of freedom or sample size, to get closer to the population value.</p>
<p>A well-known example is sample R². If you fit regression models with differing numbers of predictors to
the same sample, R² will always be at least as large for the more complex model as for a simpler model
— even if only in the 10th decimal.
This does not mean the more complex model actually has a larger <em>population</em> R². To
better approximate the population value, we can adjust for the model's degrees of freedom and the
sample size, giving the <strong>adjusted R²</strong>. Because it penalizes additional predictors,
adjusted R² can be larger for the simpler model when adding predictors does not
meaningfully improve fit.</p>
<p>Because some metrics behave differently at the sample versus population level,
this tab sometimes reports a related but distinct metric from the one shown on the Population Level tab (e.g., adjusted R² instead of R²,
and MSE are adjusted to account for finite samples).</p>"

ASSUMPTIONS_finite <- "<p>The computations on this page rely on the following:</p>
<ul>
<li>The effects specified in the <strong>Data Generating Model</strong> tab are the true effects.</li>
<li>Errors in predictors are <strong>classical measurement error</strong>.</li>
<li>The unexplained variance specified via true R² is independent of the predictors, the outcome, and the
measurement error.</li>
<li>By default, predictors are simulated as jointly normal, matching the assumption used in the Population
Level tab. Enabling <strong>skewed predictors</strong> allows to use simulate skewed
normal distributed predictors. In that case, results here reflect a
non-normal true distribution and may differ from the population-level values shown elsewhere.
<li> AIC and BIC values are naively computed as if the residuals were normally distributed,
which is not the case for the simplified model.</li>
</ul>"

INS_est_precision_sample <- "<p>The table shows the expected regression estimates
and the resulting standard deviation of the simulated regression estimates
for both models, across several sample sizes.
</p>
<p>If normally distributed predictors are used the exprected regression weights
match those shown on the Population Level tab when many iterations are used —
only their precision changes with sample size.</p>"

BACKINF_est_precision_sample <- "<p>Larger samples produce more precise (lower standard error)
estimates, but do not change the expected value of the estimate itself, which is
determined (among others) by the true parameters, the measurement error, and the omission of a variable,
not by sample size.</p>"

INS_fit_sample<-"<p>
spaceholder
</p>"

BACKINF_fit_sample<-"
<p>
Distinguishing regression models statistically typically relies on comparing their
residuals. Common approaches include AIC, BIC (which penalize model complexity differently), or a direct
statistical comparison of R². The closer the expected sample R² of the two models, the harder it is to tell
them apart statistically, even if the underlying regression surfaces differ substantially.
</p>
"

# Sign flips --------------------------------------------------------------


INS_sign_flips <- "<p>In this tab it is possibel to estimates how likely a <strong>sign flip</strong> is within
a specified data range, i.e., how often a predictor's simple slope has the opposite sign from the average effect
implied by a main-effects-only model.</p>
<p>Enter the plausible lower and upper bounds for each predictor and click <strong>Run</strong>. These bounds
can often be derived directly from your measurement instrument — for example, a 7-point Likert scale composed
of 5 items has a minimum possible sum score of 5 (5&times;1) and a maximum of 35 (5&times;7).</p>
<p>Note that the sign-flip risk for predictor X depends on the specified range of predictor Z, and vice versa.</p>"
# backinf_uncertainty<-"
# <p>
# In the previous tabs we have provided the possibility to visualize the regression surface
# resulting from an interaction effect. In general it is correct that interaction effects have a global saddlepoint, similar to a quadratic function but across the preditor variables, however that was if the data
# was bound by negative infinity and infinity. With empirical data this is typically not the case and we can restrict the data range (see below). <br>
# We have also provided the possibility to compute the coeffcients
# for the model at the point x=0, z=0 (Tab: Population Level) and define different model evaluation points (Tab: Data Visualization),
# which is relevant for procedures like mean-centering or when certain cut-off values in the data exist.
# An important question that is not answered by this is whether in the data range that is empirically plausible estimates with flipped signs compared to the
# average effect are possible. We focus on sign flipped estimates as we consider this to be the case where not modelling an interaction effect can have the most negative effects. <br>
# In the following we provide the probability of sign flips for the estimated model based on the true effects that you have provided in the 'Tab: Data Generating Mechanism' for different sample sizes and the location of this sign flip.<br>
# It is also possible to specify a specific sample size and get an estimate for this value.
# </p>"


BACKINF_sign_flips <- "<p>An interaction effect implies a regression surface with a saddle point — but this
only guarantees a sign flip somewhere across the <em>entire</em> real-valued range of both predictors. Empirical,
bounded data (e.g., by a Likert scale) may never actually reach that point. The key practical question is whether
the flip falls <strong>within a realistic data range</strong>, not whether one exists mathematically.</p>
<p>We focus on sign flips specifically because this is the scenario where omitting a true interaction effect
has the most consequential impact: a main-effects-only model implies a single, constant direction of effect,
while the true effect may reverse direction entirely for part of the plausible range.</p>
<p>Below, we first show the flip location implied by your specified true effects with no sampling uncertainty
(the infinite-sample case), then the probability of a flip within your specified range at several sample sizes,
and finally, the same information for a sample size of your choosing, which you can use if you have a specific planned
study in mind.</p>"

ASSUMPTIONS_sign_flips <- "<p>The computations on this page rely on the following:</p>
<ul>
<li>The effects specified in the <strong>Data Generating Model</strong> tab are the true effects.</li>
<li>Predictor errors are <strong>classical measurement error</strong>.</li>
<li>Residuals are normally distributed, homoskedastic, and independent of the predictors.</li>
<li>The sampling distribution of the regression estimates is approximated analytically (a multivariate
t-distribution based on the asymptotic covariance of the OLS estimator), rather than obtained via simulation.
This is a default assumption in OLS regression, but can behave erratically at small sample sizes: since the flip location is a
<em>ratio</em> of two estimated coefficients, its distribution can become heavy-tailed or unstable if the
interaction coefficient's sampling distribution places meaningful probability near zero — most likely at
small planned sample sizes or small true interaction effects.</li>
</ul>"

INS_flips <- "<p>The table below shows, for several sample sizes, how often a regression estimate has a
different sign than the average effect (from a main-effects-only model) within your specified data range.
Values are based on 10,000 draws from the reconstructed sampling distribution of the regression estimates.</p>"

BACKINF_flips <- "<p>This is computed from the full model resulting from the statistics and effects specified
in the Data Generating Model tab. For example, if the average effect of predictor X is positive, we compute
how often it is negative within the specified Z range (and vice versa); if the average effect is negative, we
compute how often it becomes positive.</p>"

INS_flip_figs <- "<p>The figures show the distribution of the values at which the sign of each regression
estimate flips. Vertical lines mark your specified data bounds. Dark purple bars fall outside your data range
(the flip is not practically relevant); green bars fall inside it (the flip is practically relevant).</p>"

# expl_flips <- "
# <p>
# The table below shows the frequency of regression estimates for which the
# estimate has a different sign to the average effect within the specified
# data range.<br>
# This information is based on the full model resulting from the descriptive
# statistics and effects specified in the 'Data Generating Mechanism' tab.
# For example, if the average effect of predictor x (main effects only model)
# is positive, we calculate how often it is negative within the specified z
# range, and vice versa for predictor z. Similarly, if the average effect is
# negative, we calculate how often positive effects occur within the specified
# data range.<br>
# The numbers and figures below are based on 10,000 draws from a multivariate normal distribution for the expected regression weights.
# </p>
# "
# expl_flip_figs <- "
# <p>
# The figures below show the distributions of the values at which the sign of the regression weights flips.
# The vertical lines indicate the specified data bounds.
# If the bars are coloured dark blue, this indicates that these flip points are
# outside the data range; if they are coloured green, they are within the data range.
# </p>
# "


vis_applied <- "
<p>
Similar, to the previous visualization we indicate the resulting regression plane for the reduced mdoel
and the full model, based on noisy measurements.
</p>
"
