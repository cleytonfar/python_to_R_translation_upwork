library(ggplot2)
library(dplyr)
library(glue)
library(stringr)
library(patchwork)
set.seed(10)

myTheme = theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        title = element_text(size=15),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
    )

# Figure 11.1: Goals of the t-test ----
 
## panel A: one-sample t-test ----
data = rnorm(n = 30, mean = .5)
# convert to DF:
data = tibble(data_index = 1:length(data), data_value = data)
# plot:
pA = ggplot(data=data, aes(x=data_index, y = data_value)) + 
    geom_point(
        size=5, 
        shape=21,
        color = "black",
        fill = "gray"
        ) + 
    geom_hline(yintercept = 0, linetype="dashed") + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        title = element_text(size=15)
    ) + 
    labs(title = bquote(bold("A)")~"One sample"),
         x = "Data index", y = "Data value")
pA 


## panel B: paired-samples t-test ----
N = 20
data1 = rnorm(n=N)
data2 = data1 + .5 + rnorm(N)*.4

pB = ggplot() + 
     theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15, color="black"),
        title = element_text(size=15)
    )

for (i in 1:N) {
    # i =1
    data = tibble(data_index = factor(c(0, 1)), 
                  data_value = c(data1[[i]], data2[[i]]))
    # pick a random color
    rgb_code = runif(1,min = 0, max = .8)
    color_ = rgb(rgb_code, rgb_code, rgb_code)
    # plotting
    pB = pB + 
        aes(x=data_index, y=data_value, group=1) + 
        geom_line(data=data) +
        geom_point(
            data=data,
            size = 5,
            color = color_,
            fill = color_,
            shape=21) 
        
}

pB = pB + 
    scale_x_discrete(labels = c("pre", "post")) + 
labs(title = bquote(bold("B)")~"Paired samples"),
         x = "", y = "Data value")

pB

## panel C: two-samples t-test ----
pC = ggplot() + 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=15),
        title = element_text(size=15),
        legend.position = c(.9, .9),
        legend.title = element_blank(),
        legend.box.background = element_rect(color="gray", linewidth = 1),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.width = unit(1, "cm"),
    )

for (i in seq(0, 1)) {
    # i=1
    data = rnorm(1000, i, (i+1)/2)
    # histogram
    nbreaks_ = nclass.FD(data)
    hist_ = hist(data, breaks = nbreaks_, plot=F)
    # convert to DF:
    data = tibble(data_index = hist_$mids,
                  data_value = hist_$counts,
                  category=glue("Group {i+1}"))
    # plot
    pC = pC + 
        aes(x=data_index, y = data_value, color=category) + 
        geom_line(
            data=data,
            linewidth=2
        )}

pC = pC + 
    scale_color_manual(values=c("Group 1" = rgb(0, 0, 0),
                                "Group 2" = rgb(0.5, 0.5, 0.5)
                                )) + 
labs(title = bquote(bold("C)")~"Two ind. samples"),
         x = "Exam score", y = "Count")
pC


p11.1 = pA + pB + pC + plot_layout(ncol=3)

# saving
ggsave("ttest_ttestGoals.png", p11.1, width=18, height = 5)


# Figure 11.2: A t-pdf ----

t = seq(-4, 4, length=573)

# a pdf with df=20
tpdf = dt(t, df = 20)

# convert to DF:
data = tibble(data_index=t,
              data_value=tpdf)

p_ttest_tpdf = ggplot(data=data, aes(x=data_index, y=data_value)) + 
    geom_line() + 
    myTheme + 
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y="Probability", x = "T value")

p_ttest_tpdf

ggsave(pt, "ttest_tpdf.png")


# Computing p-values for one-tailed and two-tailed tests ----
tval = 2.1
df = 13

pvalL = pt(-tval, df, lower.tail = T)
pvalR = pt(tval, df, lower.tail = F)
pval2 = pvalR + pvalL

print(glue("One-tailed p-value on the left: {pvalL}"))
print(glue("One-tailed p-value on the rigth: {pvalR}"))
print(" ")
print(glue("Two-tailed p-value as the sum: {pvalR + pvalL}"))
print(glue("Two-tailed p-value by doubling: {2*pvalL}"))

# 
pvalS = pt(tval, df, lower.tail = F)
pvalC = 1 - pt(tval, df, lower.tail = T)

print(glue("P-value from 1-cdf: {pvalC}"))
print(glue("P-value from s.f.: {pvalS}"))
print(glue("Difference: {pvalC - pvalS}"))

# # Figure 11.3: T-values from p-values ----
t = seq(-4, 4, length=75)
df = 13

# cdf based on t-values
cdf = pt(t, df) # pt is the cdf
# convert to DF:
cdf = tibble(
    data_index = t,
    data_value = cdf
)

# t-values based on cdf
pvals = seq(.001, .999, length=73)
tVals = qt(pvals, df) # qt is inverse cdf
# convert to DF
tVals = tibble(
    data_index = pvals,
    data_value = tVals
)

pA = ggplot(
    data = cdf,
    aes(x=data_index, y = data_value)
    ) + 
    geom_line(
        linewidth=2
    ) + 
    myTheme + 
    labs(y="cdf", x = "t value",
         title=bquote(bold("A)")~"CDF from t-values"))

pB = ggplot(
    data = tVals,
    aes(x=data_index, y = data_value)
    ) + 
    geom_line(
        linewidth=2
    ) + 
    myTheme + 
    labs(y="t value", x = "cdf",
         title=bquote(bold("B)")~"T-values from CDF"))


p11.3 = pA + pB + plot_layout(ncol=2)
p11.3            

ggsave("ttest_tFromP.png", p11.3, width=10, height=4)


# example usage to get the t-value associated with p=.05 and df=13
pval = .05
tFromP_L = qt(pval, df, lower.tail = T) # inverse of P(X <= x)
tFromP_R1 = qt(1-pval, df, lower.tail = T) # inverse of P(X <= x)
tFromP_R2 = qt(pval, df, lower.tail = F)  # inverse of P(X > x)

print(glue('Variable tFromP_L:  {sprintf("%.3f",tFromP_L)}'))
print(glue('Variable tFromP_R1: {sprintf("%.3f", tFromP_R1)}'))
print(glue('Variable tFromP_R2: {sprintf("%.3f", tFromP_R2)}'))


# Figure 11.4: Example t-value ----

# empirical t-value and df:
tval = 1.6
df = 20
alpha_ = .05

# redifine the t-values and corresponding pdf
t = seq(-4, 4, length=573)
tpdf = dt(t, df = 20)

# its associated p-value (but this is one-tailed for visualization; see text and next cell!)
pval = pt(tval, df, lower.tail = F)

# critical t-value for apha
tCrit = qt(alpha_/2, df = df, lower.tail = F) # /2 for two-tailed!
pHalf = max(tpdf)/2 # 1/2 max. (vertical) p(t), used for plotting

data = tibble(
    data_index=t,
    data_value = tpdf
)

ggplot(data, aes(x=data_index, y=data_value)) + 
    geom_line(
        linewidth=1
    ) +
    geom_ribbon(
        data = filter(data, data_index >= tval), 
        aes(ymax = data_value, ymin=0),
        fill="#969696") + 
    geom_vline(
        xintercept = tCrit,
        linetype = "dashed",
        color="gray"
    ) +
    annotate(
        "text", 
        x = tCrit-0.2, y = pHalf*2, 
        label = bquote(alpha~"/2" == .(alpha_/2)),
        angle = 90
    ) + 
    myTheme + 
    labs(y = bquote(rho ~ "(t/" ~ H[0] ~ ")"), 
         x = "T value")

    
     