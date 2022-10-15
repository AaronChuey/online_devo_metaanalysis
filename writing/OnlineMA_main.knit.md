---
title             : "Conducting developmental research online vs. in-person: A meta-analysis"
shorttitle        : "Title"

author: 
  - name          : "First Author"
    affiliation   : "1"
    corresponding : yes    # Define only one corresponding author
    address       : "Postal address"
    email         : "my@email.com"
    role:         # Contributorship roles (e.g., CRediT, https://casrai.org/credit/)
      - "Conceptualization"
      - "Writing - Original Draft Preparation"
      - "Writing - Review & Editing"
  - name          : "Ernst-August Doelle"
    affiliation   : "1,2"
    role:
      - "Writing - Review & Editing"
      - "Supervision"

affiliation:
  - id            : "1"
    institution   : "Wilhelm-Wundt-University"
  - id            : "2"
    institution   : "Konstanz Business School"

authornote: |
  Add complete departmental affiliations for each author here. Each new line herein must be indented, like this line.

  Enter author note here.

abstract: |
  An increasing number of psychological experiments with children are being conducted using online platforms, in part due to the COVID-19 pandemic. Individual replications have compared the findings of particular experiments online and in-person, but the general effect of online data collection with children is still unknown. Therefore, the current meta-analysis examines how the effect sizes of developmental studies conducted online compare to the same studies conducted in-person, providing a synthetic view of how online experiments compare to in-person experiments. Our pre-registered analysis includes 145 effect sizes calculated from 24 papers with 2440 children, ranging in age from four months to six years. In addition to study modality (online vs in-person), we examined several moderators, including the role of dependent measure (looking vs verbal), online study method (moderated vs unmoderated), and age. The mean effect size of studies conducted in-person (d = .68) was slightly larger than the mean effect size of their counterparts conducted online (d = .54), but this difference was not significant. Additionally, we found no significant moderating effect of dependent measure, online study method, or age. Overall, the results of the current meta-analysis suggest developmental data collected online are generally comparable to data collected in-person.
  
  
keywords          : "keywords"
wordcount         : "X"

bibliography      : "r-references.bib"

floatsintext      : no
linenumbers       : yes
draft             : no
mask              : no

figurelist        : no
tablelist         : no
footnotelist      : no

classoption       : "man"
output            : papaja::apa6_pdf
---







# Introduction 


Developmental researchers are interested in studying children’s behavior, primarily by measuring their behavioral responses to experimental stimuli. Study sessions typically involve visits with local families in a laboratory setting or partnering with remote sites such as schools and museums. Although these interactions are a routine part of developmental research, they are time-consuming for both researchers and participants. Typical studies with dozens of infants or young children can require weeks or months of scheduling visits to a lab or many days of visits to testing sites. In-person testing also limits the participant pool to children living relatively close to the research site. Even more so than research with adults, developmental research has been plagued by small, undiverse samples bottlenecked by the demographics of the local population,  their access to researchers, as well as the cost of collecting data from them [@kidd2022diverse; @nielsen2017persistent]. 

Prior to the rise of video chat software, there were not any alternatives to in-person interaction for collecting experimental behavioral data from children. But with the development of inexpensive and reliable video conferencing technology in the 2010s, new frontiers began to emerge for developmental testing [^p  Observational and survey research has long been conducted through the phone or by mail [e.g.@fenson1994variability]; here we focus primarily on behavioral observation and experimental methods. 
]. Researchers soon experimented with conducting developmental studies through video-chat platforms, which in theory broaden the pool of participants to anyone with internet access at nearly any time and location. What began as a few research teams experimenting with online studies [e.g., Lookit: @scott2017lookit; The Child Lab: @sheskin2018thechildlab; Pandas: @rhodes2020advancing] quickly expanded to much of the field as researchers scrambled to conduct safe research during the Covid-19 pandemic. This shift in research practices has yielded many empirical publications where some or all of the data was collected online in addition to a growing literature on online methodology and best practices [for a recent review, see @chuey2021moderated]. 

Some researchers may be eager to return to in-person testing, but online research is likely here to stay and even escalate as communications technologies continue to improve and become more accessible. Online testing has immense potential to change developmental science [@sheskin2020online], much as crowdsourced testing of adults has changed adult behavioral science [@buhrmester2016amazon]. This potential has yet to be fully realized, however, as researchers are still struggling to understand the strengths and weaknesses of this method, and how to recruit diverse populations for online studies. Despite undersampling certain populations [@lourenco2020no], online studies do allow researchers to sample from a larger, broader pool of participants than ever before as access to the internet continues to increase worldwide. In turn, large, relatively low cost samples and cross cultural research from a distance may even prove viable in the coming years.

Is conducting developmental studies online an effective substitute for conducting them in-person, or do online studies yield systematically different effects? Direct comparison of effects measured in both modalities is critical to answering this question. Researchers have implemented a number of paradigms online and replicated their in-person findings, but the quality of data yielded from online studies in comparison to those conducted in-person more broadly is still largely unknown. Therefore, the current meta-analysis examines how data collected from children online compares to data collected in-person in closely-matched studies. Importantly, online studies themselves are not a monolith, and differ in a multitude of ways including the presence of a live experimenter, dependent measure, and the age of the sample being tested. We consider these three moderators in the current analysis.

# Moderators 

Online studies are generally conducted in one of two formats: moderated and unmoderated. In moderated studies, a live experimenter guides participants through a study much like they would in-person, except online, typically via video-chat. Moderated studies are often operationalized as slide share presentations or videos shared with participants while the participants’ verbal responses or looking is recorded. In unmoderated studies, participants complete a study without the guidance of a live experimenter. Instead, researchers create a preprogrammed module that participants or their parents initiate and complete according to instructions. Unmoderated studies offer the potential for fast, inexpensive data collection since no experimenter needs to be present and participants can participate at any time they choose. However, since they lack an experimenter, participants’ experience also deviates more from in-person studies compared to moderated studies which retain the same core social interaction between experimenter and participant. Therefore, it is possible that data collected via unmoderated sessions is comparatively noisier since an experimenter is unable to focus children’s attention or course correct like they can during a live interaction. We consider this possibility in the current meta-analysis.

Like developmental studies more broadly, online studies have also employed a number of dependent measures, including verbal measures  and looking measures. Verbal measures are typically straightforward to record, while recording looking measures is more complex. Accurate looking measures require precise camera positioning and coding schemes, and are thus more likely to deviate from their in-person counterparts compared to studies that measure children’s verbal responses. To that end, automated gaze annotation is currently being developed and represents an exciting future direction in online methodology [see @erel2022icatcher]. We examine how the kind of dependent measure employed (looking vs. verbal) might moderate the difference between online and in-person results.

The final moderator we consider is participants’ age. Online developmental studies have sampled from a wide age range, including infants [e.g., @dillon2020infants], toddlers [e.g., @lo2021tablet], preschoolers [e.g., @schidelko2021online], and elementary schoolers [e.g., @chuey2020children; Chuey et al., 2021]. Because online studies are often conducted in the comfort of their own homes, it is possible that children of all ages might benefit from this aspect of online studies. Conversely, because a child’s environment is more difficult to moderate online, infant studies, which often rely on precise environmental setups, may suffer when conducted online. In addition, as children get older they may gain more experience with on-screen displays, which can contribute to their performance in online studies. We test these competing age moderation hypotheses. In sum, our meta-analysis addresses the question of whether effect sizes tend to differ across online and in-lab experiments with children, and whether these differences are moderated by study format, dependent variable, or participant age. 



# Methods 


We conducted a literature search following the Preferred Reporting Items for Systematic Reviews and Meta-Analyses (PRISMA) procedure (CITE). For each set of studies determined to be an online replication, we calculated the effect size(s) and associated variance for the main effect of interest. We then conducted a series of random-effects multilevel meta-regressions to estimate the effect of online data collection, as well as three possible moderators (online study method, type of dependent measure, and participant age). Our preregistered data selection, coding, and analysis plan can be found at (insert url).

## Literature Search 

Our goal was to find as many published and unpublished online replications of developmental studies as possible. However, because there is no common nomenclature for online replications and the studies themselves cover a wide range of research questions and methodologies, searching via specific terms or keywords was difficult and produced many irrelevant papers. Instead, we preregistered a forward citation search strategy based on key papers on online developmental research. We used the papers that conducted initial validation of popular online testing platforms as our seeds, including Lookit (Scott & Schulz, 2017; Scott, Chu, & Schulz, 2017), The Child Lab (Sheskin & Keil, 2018), and Pandas (Rhodes et al, 2020). We also included all papers published in the Frontiers in Psychology Special Issue: Empirical Research at a Distance: New Methods for Developmental Science, which largely focused on online developmental studies and replications. Finally, we posted a call for contributions to the Cognitive Development Society and ICIS listservs, two popular emailing lists frequented by developmental researchers. This call yielded several publications our initial search strategy missed, as well as several unpublished but complete online replications.

We preregistered several eligibility criteria to filter articles from our search: 

1. The study must be experimental, where participants complete a task with a stimulus. This criterion precludes surveys or purely observational measures. 

2. The studies must report two groups of children, one tested online and another tested in-person. Although the online sample must be collected by the researchers reporting the results, the in-person sample could either be collected at the same time or referenced from an existing publication. 

3. The mean age of the sample should be under six years. This criterion limits the studies to those conducted on relatively younger children for whom online data collection methods have not been traditionally employed. 

4. All data reported or referred to must contain codeable effect sizes. Verbal comparison alone between an online or in-person study or a qualitative description of results is not enough to determine the precise effect size of interest. 

5. Data collection for both the in-person and online sample must be complete; any incomplete or partial samples were not considered.

6. The online and in-person methods must be directly comparable. Some alteration to the study methods is expected when adapting an in-person study to be run online (e.g., changing a preferential reaching measure into a preferential looking measure, having children refer to objects by color instead of pointing, etc). However, we excluded any studies whose methodologies altered the nature of the task or the conclusions that could be drawn from them (e.g., manipulating the identity of an object instead of its location). 

## Data Entry 

All papers (320) yielded by our search procedure went through three rounds of evaluation to determine if they met our inclusion criteria. First, we screened the titles of the papers to determine whether they might include an online experiment. Those that clearly did not meet one or more of our inclusion criteria were excluded from further evaluation. Next, we performed a similar evaluation based on the papers’ abstracts, before a final round based on the article as a whole. All remaining papers were entered into a spreadsheet that coded the necessary information for us to calculate the size of the main effect(s) of interest and their associated variance (sample size, group means and standard deviation, and t and F statistics when applicable), as well as our preregistered moderators (study modality, data collection method, dependent measure, and participant age). 

If a paper reported  an effect size in standardized mean difference (SMD), we coded it directly. Otherwise, we calculated the individual effect sizes for each main effect and each study (online and in-person) using reported means and standard deviations, t statistic, or directly from the data if it was available. If the main comparison was to chance performance, we first calculated log odds and then converted the effect size to SMD via the compute.es package in R (Del Re & Del Re, 2012). If a given study had multiple dependent measures or central hypotheses, we calculated an effect size and associated variance for each.

## Analytic Approach 

To determine whether study modality (online or in-person) moderated the size of the main effect of interest for each set of studies, we performed a random-effects multilevel meta-regression using the metafor package (Viechtbauer, 2010). The regression predicts effect size (SMD) with study modality as a fixed effect, and random intercepts fitted by study and participant. Rather than predicting the size of the online-offline difference for a particular measure, this model instead predicts individual experimental effect sizes, with the coefficient of interest being the study modality predictor. We chose this format because online and offline effect pairs were often heterogeneous (e.g., with different ages or sample sizes) and so this format allowed us to use that information in the main and moderated meta-analyses, rather than eliminating it by reducing the two studies to one difference of effect sizes. The random intercepts in our models control for dependencies between the particular studies themselves (e.g., effects yielded from a paper on false belief will be different than effects from an object recognition task), as well as between different samples that may or may not generated multiple effect sizes in our model (e.g., participants who completed a single task/measure vs participants who completed a battery of tasks with multiple main effects of interest reported as a single study). 

To determine the effect of additional moderators (online study method, dependent measure, and participant age), we conducted three additional multilevel meta-regressions each with an additional fixed effect plus the corresponding interaction with study modality. All analysis scripts were preregistered and available at [link].





# Results 



## Confirmatory Analysis

Overall, the meta-analysis estimated a small negative, non-significant effect of online study modality, p = .21, 95% CI [-.38, .084] FIXME. Additionally, we did not find any significant effect of our preregistered moderators (ps > .4) or any significant interactions between the moderators and study modality (ps > .3).


## Exploratory Analysis

We conducted an exploratory analysis looking at potential publication bias. It is unclear what direction we might expect publication biases to manifest, but we examined the existence of publication bias in the differences in effect size between each online and in-lab pair of samples. This checks for publication bias on the basis of whether online studies match the results of the in-person studies. For each online and in-person pair on the same study, we calculated a standard mean difference in effect size between the two studies as well as the variance of this difference. Funnel plot is shown in Fig TODO. Overall, we found no clear bias to publish papers with either larger or smaller differences in effect size than expected. We also considered the overall average age of participants in the pair of studies, as well as the response mode and method of the online sample as moderators. To that end, we found that neither age, dependent measure, nor online study method significantly moderated the difference in effect size between online and in-person studies.



# Discussion



By aggregating across a growing literature of online studies, the current meta-analysis provides a birds-eye view of how developmental studies traditionally conducted in-person fare compared to closely matched counterparts conducted online. Our results suggest that overall, the results of online studies are comparable to those conducted in-person. Additionally, we found that the method of online data collection, type of dependent measure, and participant age did not appear to have a significant impact either. Nonetheless, the relatively small sample size limits our ability to make sweeping generalizations about any of our moderators, so future analysis is needed to determine the moderating effect, if any, that these factors exercise on the outcome of developmental studies conducted online.

It is also important to consider additional factors that could influence these results or the way we interpret them. Chiefly, the current analysis is quite coarse-grained and considers one particular dichotomy within study modality: in-person vs online. Yet, there are many ways that developmental studies can be further subdivided. For example, studies are conducted both in quiet spaces (e.g., in lab, at home) and loud spaces (e.g., parks, museums). Therefore, online studies might out- or underperform studies conducted in particular in-person locations. Our moderators are also correspondingly course-grained, particularly dependent measure (looking vs verbal). Qualitatively, unmoderated looking time studies with infants appear to perform the worst online (insert average effect sizes). However, our small sample size likely renders our analysis underpowered to detect weaker effects of moderators, and our results themselves are subject to change as online methods improve.

Although developmental researchers have had decades of experience designing and running experiments in-person, most have only had a few years or less of experience developing online studies. Thus, our meta-analysis might underestimate the effectiveness of online studies due to researcher and experimenter inexperience. Over the next several years, as developmental researchers develop expertise and experience with online studies, effect sizes might increase for any number of reasons, including better experimenter-participant interactions, better stimulus design, and more accurate methods of measurements (i.e., automatic looking time measures, see Erel et al., 2022). Relatedly, as new methods are developed and adapted for online experiments, researchers should not take the current findings as a blanket declaration that all online studies produce comparable results to their in-person counterparts; some might underperform, while others might outperform. Nonetheless, the current results suggest that across currently employed developmental methodologies, studies conducted with children online are generally comparable to those conducted in-person.

The composition of our sample might also bias our results. To match online and in-person methods as closely as possible, we only considered direct online replications for the current meta-analysis. While this approach ensures that data were collected online and in-person using similar methods and procedures, it limits our sample size and may bias our sample. For example, perhaps researchers disproportionately choose to conduct online replications of strong or well-established effects rather than replicate more subtle, weaker effects. Nonetheless, our analysis found no significant publication bias in terms of favoring stronger online effect sizes or non-replications among the studies we sampled. We also included an open call for unpublished data in an attempt to limit the file drawer problem (see Rosenthal, 1979). Of the published and unpublished online replications that were available to include in our sample, we found comparable effect sizes online (compared to in-person); however, researchers should exercise caution as this sample may not be representative for their questions of interest.  

# Conclusion 

Although online data collection precludes certain research methodologies or measures (e.g., exploration of a physical environment), the general similarity in outcomes for in-person and online studies with children paint an optimistic picture for online development research going forward. However, beyond enabling the collection of high quality, low cost data, online research also stands to benefit the broader scientific community as a whole. Conducting studies online allows researchers to sample beyond the local community surrounding their home institution. And importantly, for many online participants, an online study with a developmental researcher is their first interaction with a scientist. As online research expands among developmental researchers, we are presented with an unprecedented outreach opportunity to directly interact more closely with those we hope our research will help – parents and children.



\newpage

# References

::: {#refs custom-style="Bibliography"}
:::
