#### User Guide to Power Implications - R Shiny App

This app (https://power-implications.shinyapps.io/prod/) aims to provide insights on the power properties of trials if the amount of data in the final analysis is less than specified in the original design. It may be useful in the current COVID-19 situation for various reasons: a trial may not be feasible to continue because of COVID-19 issues; or it may be possible to continue but to a lesser amount of information than planned; or, even if a trial can continue, perhaps the data from this point forward may later be viewed by health authorities as compromised, so that main attention will be given to data already existing at this point. As a simple generic illustrative example: a trial was designed for 90% power for a specified clinically relevant effect size; in an analysis using data where only 70% of patients reach the trial's endpoint, how much does this reduce power?

Certainly for the most thorough understanding of a trial's properties, teams are able to extend the precise methodologies and computational approaches that were used for trial design. However, this app aims to facilitate a simple quick consideration and quantification. It is quite broad and generic. It can be applied to any design for a two-arm comparison with a normal or asymptotically normal endpoint; this includes continuous, binary, time-to-event outcomes and potentially others. All that needs to be specified is the design significance level and power, and the amount of statistical information available for analysis (for example, the fraction of patients with data for a continuous endpoint, or the proportion of patients with events relative to the design plan for a time-to-event outcome). Trial details such as hypothesized effect size estimates, nuisance parameters (e.g., variance), randomization ratio, etc., need not be specified. Please note that quantities must be entered or interpreted on the scale for which estimates are assumed to have an asymptotically normal distribution (e.g., difference of means, difference of proportions, log odds ratio, log hazard ratio). Further description and illustration of each tab in the app are below. Some methodological details appear at the end of this section.
