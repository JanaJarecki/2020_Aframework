###############################################################################
# Computes the inter-rater reliability for
# Inter-Coder Reliability for randomly selected subset of papers
# The subset included papers on cognitive modeling
# By Jana Jarecki, last edited 30.01.2019, jj@janajarecki.com
###############################################################################
library(irr)
library(data.table)

d = fread("../../data/processed/inter_coder_reliability.csv")

# Kappa for 2 raters rating the main field ("JDM" vs. "no JDM")
df2 = d
df2[d!="noJDM"] = "JDM"
kappa2(df2, "unweighted")
#     Kappa = 0.831 

#         z = 5.88 
#   p-value = 4.02e-09


# Kappa for 2 raters for rating the subfield
# If both rate it as JDM, which subfield is the article?
df3 = subset(d, ICRJJ!="noJDM" & ICRJT!="noJDM")
kappa2(df3, "unweighted")
# Subjects = 37 
# Raters    = 2 
# Kappa     = 0.872

#     z     = 12.9 
# p-value   = 0  