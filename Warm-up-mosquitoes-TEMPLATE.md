Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Kyle Thomas
2025-10-10

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
- [DISCUSSION](#discussion)
  - [Interpretation - fill in
    analysis](#interpretation---fill-in-analysis)
  - [Interpretation - fill in
    analysis/plot](#interpretation---fill-in-analysisplot)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

West Nile Virus (WNV) first entered the USA in 1999 in New York, USA.
WNV requires a mosquito vector and is mostly transmitted from an
infected bird and transmitted through a mosquito to new bird. This study
examined the abundence of host species present in Salt Lake City, Utah.
Mosquitoes in this area were collected and the DNA from their most
recent blood meal was amplified with PCR and the DNA was then sequenced.
Using this data, barplots were created of blood meal ID by trap
locations for WNV negative and positive mosquito pools. These showed
that there was a higher rate of blood meals taken from House finches in
WNV positive areas than negative. Generalized Linear Modeling was ran,
showing that as the number of mosquito blood meals taken from the House
Finch increases so does the rate of infection for WNV.

# BACKGROUND

West Nile virus (WNV) is a mosquito-borne virus that was first
introduced into the United States in 1999. This virus affected various
types of animals like humans and birds. It is usually passed from a
mosquito vector taking a blood meal from an infected host then later
biting a new healthy host. Although it is less common, the virus can be
passed to humans in this same way.

This study’s focus is on mosquito blood meal data from Salt Lake City,
Utah. The first step was determining which animal the mosquito had taken
a blood meal from. The next step is DNA extraction followed by
polymerase chain reaction (PCR) and DNA sequencing was used to identify
the host species of the blood meal.

The House Finch is a common carrier of WNV and has the longest days of
viremia compared to all carrier species (Kumar et al., 2003). This
increase of viremia could likely cause the abundance of infection among
the species. Salt lake City is an urban environment, a common
environment for House Finches. A primary focus was on the correlation
between high House Finch populations and WNV cases in Salt Lake City.

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warm-up-mosquitoes-TEMPLATE_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

Is there correlation between the rate of WNV positive pools and the
density of the house finch population?

## Hypothesis

Due to the long duration of viremia in House Finches, areas with dense
House Finch populations are more likely to have increased WNV-positive
mosquito pools.

## Prediction

If House Finches play an important role as amplifying host and they have
around 7 days of viremia than the areas with more house finch with have
more positive WNV pools.

# METHODS

To identify the most common hosts on which mosquitoes feed, mosquitoes
were collected from the Salt Lake City area. DNA extraction was used on
each mosquito’s last blood meal. The extracted sequence was used to find
the host from the blood meal using BLAST. PCR was then used to amplify
the DNA. Using the BLAST data the most frequent host species was
identified and visual plots were made. These compared the distribution
of blood meals across host species at sites with and without
WNV-positive mosquito pools.

``` r
## Fill in first analysis
# Barplots of blood meal ID by trap locations with/without WNV positive mosquito pools

## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Warm-up-mosquitoes-TEMPLATE_files/figure-gfm/firstanalysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors


## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

Generalized linear models (GLMs) were used to statistically test whether
blood meals from House Finches were correlated with increased WNV
detection. The GLMs tested the binary presence and absence of WNV and
the WNV positivity rate in these areas. This explores the correlation
between the number of mosquito-House Finch blood meals and if the site
has WNV-positive pools(binary) or a higher WNV-positive rate at each
location (numeric).

``` r
# second-analysis-or-plot, glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

Both analyses show a strong correlation between WNV presence and
infection rate in areas where the House Finch population is more dense.
These variables both had a p-value less than 0.5, which infers strong
statistical significance in favor of the hypothesis. This provides
strong evidence that the House Finches play a meaningful role in WNV
transmission. The House Finch has a viremia of approximately seven days,
the highest of all hosts, and they usually live in urban areas, which
leads to increased rates of human infection due to proximity. The Great
Salt Lake, located in the Salt Lake City area, provides a favorable
environment for mosquito breeding, increasing the likelihood of West
Nile Virus transmission from mosquitoes to House Finches in areas with
dense House Finch populations.

A limitation of this study is that, while a strong correlation was seen,
causation cannot be inferred from this analysis alone. Although our
analyses show there is strong evidence in support of the hypothesis,
there may be many outside variables affecting these statistics. These
potential confounding factors cannot be identified or accounted for
without further research and testing.

## Interpretation - fill in analysis

Each bar in the plot represents the count of the host species in both
WNV-negative and WNV-positive locations based on bloodmeal counts from
mosquitoes. The House Finch is the leading host for both positive and
negative graphs. There are roughly 60 blood meals taken from house
finches on the positive graph, while the negative graph only has about
30 blood meals. The House Finch being the leading host for each graph,
it can be inferred that the House Finch is common in the Salt Lake City
area. The difference between the blood meal counts between each graph
shows that the House Finch is a prominent carrier of WNV in the area as
well.

## Interpretation - fill in analysis/plot

The data was analyzed using a generalized linear model (GLM). The GLM
was used to examine the correlation between the number of mosquito-House
Finch blood meals and if the site has WNV-positive pools(binary) or a
higher WNV-positive rate at each location (numeric).To predict whether
the presence of WNV at a site could be predicted based on the number of
blood meals mosquitoes had on House Finches, the first model used a
binomial GLM. The binomial GLM resulted in a p-value of 0.0287, meaning
it is statistically significant to the hypothesis. This supports the
idea that increased populations of House Finches at a site will increase
the likelihood of a WNV-positive mosquito pool.

Linear regression was used in the second model to examine whether the
number of House Finch blood meals predicted the WNV positivity rate. The
p-value for the linear regression was 4.54e-05, showing high statistical
significance, supporting the idea that an increase in House Finch blood
meals is correlated with an increase in WNV positivity rate.

# CONCLUSION

This study shows a significant correlation between House Finches and the
Salt Lake City area. The increase in days of viremia that House Finches
have than any other host, and that House Finches are the most abundant
blood meal for mosquitoes in the area, are likely associated with the
high values they have for WNV infection. Although House Finches were the
most abundant host for both WNV-negative and WNV-positive areas, the
study showed that a House Finch was more likely to be WNV-positive. Due
to possible confounding factors that were not examined within this
study, it is not possible to declare causation, but the observed
correlation indicates a meaningful relationship between host selection
and patterns of viral amplification.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-10-10.
