##### Power Information tab

This tab provides power for any specified amount of information (up to 100%, defined as the study's originally designed information according to the specified level and power). For example, if in a study with level 0.025 and 90% power we analyze data with 50% of the target information, the power is reduced from 90% to 63%.

As a specific example, consider a study with a normal endpoint, designed to yield 90% power for a 2.5% level test, to detect a hypothesized mean difference of 10 units, and the assumed standard deviation is 21. This results in a sample size of 194 patients. Now say that the study is stopped when the endpoint has been assessed on 130 patients, i.e., 67% of the target. Entering "67" on the "Proportion of data available" slidebar, the app indicates in the 'Power (%) / Available' field that the actual power is 75.6%. Note that we did not need to enter the hypothesized treatment difference or standard deviation, the power can be determined solely through the level, original power, and amount of data relative to the original design.

The same calculation can be performed for binary outcomes, whether the data is analyzed on the scale of difference of means or log odds ratio. For time-to-event outcomes, the proportion of information is the ratio of the observed number of events to the number planned in trial design.

Adjacent to the power value, this tab also provides the value of the estimate that would reach statistical significance, a quantity that may have descriptive relevance for decisions in some situations. In the same continuous outcome example above, the original design would reach significance if the final observed treatment difference was at least 6 units, or 0.6 times the magnitude of the design effect of 10; in the reduced sample size setting with 67% of the data, we would need to observe a difference of at least 7.4 units, hence the indicated threshold would be 0.74 times the size of the design effect. Note that for time-to-event analysis or odds ratio-based binary analyses, this estimate provided is on the log scale. For example, if the 67% information example illustrated above corresponded to events in a time-to-event analysis, the required estimates need to be anti-logged to convert to hazard ratio (HR) estimates, for example: original design: $\text{exp}(-0.6)=0.55\text{HR}$ required; reduced design: $\text{exp}(-0.74)=0.48\text{HR}$ required.

##### Power vs Sample Size tab

This tab basically extends the "Original-design Power" and "Available Power" values as produced by the Power Information tab, allowing visualization of how power increases as the amount of data increases between the currently available proportion and the full amount planned in the original design. This allows visualization of the rate at which power would increase if more data were able to be included in the final analysis.

##### Power vs. Effect Size tab

This tab provides the power curve over a range of possible values of the true underlying treatment effect. It may be useful in situations where a power curve, or comparable information investigating different power alternatives, played a role in trial design. That is, power over a range a range of possible effect sizes, rather than focusing on the single value specified in the alternative hypothesis power values, was relevant for design, and understanding the impact of reduced data on the original power curve may be helpful for current decisions. Both the original design power curve and revised power curve are presented.

On this plot, the $x$-axis presents effect sizes relative to the hypothesized design effect size. We may think of this as $\Delta^*/\Delta$, where $\Delta$ is the effect specified in the study's alternative hypothesis yielding power $1-\beta$, and $\Delta^*$ varies on the $x$-axis. The quantity $\Delta$ is expressed on whatever scale the estimate is normally or asymptotically normally distributed. In our continuous endpoint example above, where the hypothesized effect was a 10 unit difference, this corresponds to "1" on the $x$-axis, a true difference of 6 units would correspond to 0.6, etc. The same idea applies to analysis of a difference between proportions using a normal approximation; for example, if the study was designed under a hypothesis that a treatment increased a control group response rate from 0.3 to 0.4, then the value "1" on the a-axis corresponds to the 0.1 proportion difference, etc.

Other data structures may need transformation for best interpretation. If for a time-to-event outcome a study hypothesized $\text{HR}=0.8$, then the treatment effect on the $x$-axis can be interpreted as $\text{ln}(\text{HR}) / \text{ln}(0.8)$. So for example, a true effect corresponding to $\text{HR}=0.75$, would be reflected on the $x$-axis as $\text{ln}(0.75) / \text{ln}(0.8)=1.29$, that is, on the normal scale this is a treatment effect 29% greater than hypothesized.

##### Methodology

Consider a normally distributed test statistic for a primary endpoint, in a study designed for level $\alpha$, power $1-\beta$, requiring $N$ patients. Say that we only obtain a fraction $p$ of the targeted information (for example, $pN$ patients or events). The resulting power can be shown to equal $$P\left[Z>(1-\sqrt{p})z_{\alpha}-\sqrt{p}z_{\beta}\right],$$
where $z_\gamma$ denotes the $(1-\gamma)$-quantile of the standard normal distribution. Let $1-\beta^*$ denote this revised power, that is, $\beta^*$ is the type II error in the reduced design.

Thus, for example, say that we designed a study for 90% power but only obtained 60% of the targeted patients. The power for the initially hypothesized effect would be $$1-\beta^*=P\left[Z>(1-\sqrt{0.6})\cdot 1.96 - \sqrt{0.6}\cdot 1.2816\right]=P(Z>-0.551)=0.709.$$

The estimate of the treatment effect, relative to the design effect $\Delta$, that would reach statistical significance at level $\alpha$ can be shown to be $$\frac{\Delta^*}{\Delta}=\frac{z_\alpha}{z_\alpha+z_{\beta^*}}.$$

Continuing the same illustrative example, the revised power as indicated above is 70.9%. This corresponds to $z_{\beta^*}=0.550$, and thus the app would indicate the value $$\frac{\Delta^*}{\Delta}=\frac{1.96}{1.96+0.550}=0.78$$to be the fraction of the hypothesized design effect that would need to be observed to reach significance in the reduced study.

The extension of the power calculation above to additionally accommodate varying the effect size, i.e., a power curve, is $$f(k)=P\left[Z>z_\alpha-k\sqrt{p}(z_\alpha+z_\beta)\right],$$ where $k$ denotes the ratio of the effect size relative to the hypothesized effect that yielded power $1-\beta$ for the original design.

##### Reference
TBD
