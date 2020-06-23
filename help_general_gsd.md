##### Achieved Power tab

This tab provides the resulting power depending on the proportion of available data for two different design options: (1) stop the trial and only analyze the available data, (2) introduce an interim analysis, i.e. switching from a fixed design to a groups sequential design (GSD). For the second option, the available data is being used as the stage one sample size and the originally planned sample size as total sample size. In addition to the significance level, the originally desired power at planning stage and the proportion of data available, stopping boundaries have to be specified with the option to choose between Pocok boundaries or O'Brien-Fleming boundaries.

For example, if in a study with level 0.025 and 90% power we analyze data with 50% of the target information, the power is reduced from 90% to 63% if only the available data is being used. For a GSD with Pocock boundaries, the power is 55% for the first stage and 87% overall.

In addition to the plot, the tick box if checked will show the resulting values in a table format with the resulting power for the given proportion as set by the slider being highlighted in the table.

Furthermore, the power can be calculted assuming that for the second stage of the trial, the treatment effect and/or the assumed standard deviation has changed. The first check box ("No dilution effect") allows specification of the assumed dilution effect of the treatment effect. For example, a value of 0.5 means that we assume the treatment effect for the second stage to be half the treatment effect for the first stage while a value of 0 means that no dilution is expected for the second stage and a value of 1 means that we do not assume a treatment effect for the second stage of the trial. The second check box ("Equal variances for second stage") allows specification of the change of the assumed variance for the treatment and the control arm. The standard setting is to assumed that the variance for the treatment and control arm are the same. In addition, we assume that the variance is the same for the first and second part of the trial. A variance in-/deflation factor of for example 2 means that we assume twice the variance for the second stage as compared to the first stage. A value of 1 referrs to equal variances for the two stages while a value of 0.5 referrs to assuming half the variance for the second stage as compared to the first stage.

For example, if a study with significance level 0.025 and 90% power is analyzed with 50% of the planned data available and Pocock boundaries the resulting overall power at the end of the trial will be 84% if a dilution effect of 0.1 and equal variances are assumed. The power reduces to 80% if we assume a variance inflation of 1.3.

##### Sample Size Adjustment

Given that the power might be reduced when changing from a fixed design to a GSD and further assuming a dilution effect and/or a variance inflation, the sample size adjustment tab provides the possibility to calculate the adjusted sample size needed to regain the desired power. The first three input parameters allow specification of the assumed treatment effect, the assumed variance and the randomization ratio for the first part of the trial. As before, the parameters "no dilution effect for second stage" and "equal variances for the second stage" can be used to set the assumed values for the second stage of the trial. The resulting sample sizes are then given in the main tab window. Note that it is possible to show the resulting sample sizes for a range of value of the dilution effect by ticking the check box underneath the table. As the adjusted sample sizes have to be found via a search algorithm, producing the table can take a few moments. A progress bar in the lower right corner of the app displays the progress being made so far.

##### Methodology

The underlying methodology can be found in the paper cited in the reference section below.

##### Reference

Kunz CU, Joergens S, Bretz F, Stallard N, Van Lancker K, Xi D, Zohar S, Gerlinger C, Friede T (2020): Clinical trials impacted by the COVID-19 pandemic: Adaptive designs to the rescue? https://arxiv.org/abs/2005.13979