################################################################################
# Figure 1, citation count
# By Jana Jarecki, last edited 30.01.2019, jj@janajarecki.com
################################################################################
rm(list=ls(all=TRUE))
library(data.table)
library(ggplot2)
library(scales)
library(themejj)
theme_set(themejj()) #personal theme use theme_bw() as replacement

# the year is no filter criterion in web of knowledge!
# How to get numbers out of web of knowledge:
# 1. http://apps.webofknowledge.com
# 2. in search field enter
#  "process model" AND (("judgment and decision making" OR "decision making") AND psychology AND cognitive)
# 3. from drop down menu chose "topic"
# 4. hit search button
# 5. hit create citation report
# 6. copy the average citations for the years you're interested in into R (top row of table)


# SEARCH TERMS 
#  ("judgment and decision making" OR "decision making") AND psychology AND cognitive
# (("judgment and decision making" OR "decision making") AND psychology AND cognitive) AND "process model"
# (("judgment and decision making" OR "decision making") AND psychology AND cognitive) AND “agent-based model”
# (("judgment and decision making" OR "decision making") AND psychology AND cognitive) AND "formal model”
# (("judgment and decision making" OR "decision making") AND psychology AND cognitive) AND “computational model”


################################################################################
#
# Data from ISI (Access: May 2016)
# All citations 1995 - 2018
# 
###############################################################################
# Absolute citations
citationsYear <- c(49,69,99,189,208,309,289,384,466,488, 731,975, 1226, 1624, 2104,2389,2854,3299,3748,4475,4977,5351,6208,6495)

# ... AND "Process model"
citationsYearAND <- c(1, 0,  0,  2,  0, 6, 1,  1,  0,  2, 0, 3,  0,  7,  7, 18,   33, 39, 55, 78, 98, 97, 97, 112)

# ... AND "Agent-based model"
citationsYearAND2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 2, 4, 11, 6, 9, 7)

# ... AND "formal model"
citationsYearAND3 <- c(0,   0,  0,  0,  0, 0,   0,  0,  0,  0, 0,   0,  0,  0,  0, 0,   0,  0,  0,  0,  0,   1,  0,  1)

# ... AND "computational model"
citationsYearAND4 <- c(0, 0, 0, 0 , 0 , 0 , 0, 0, 0, 0, 2, 6, 4, 16, 16, 22, 20, 29, 29, 39, 50, 50, 61, 65)

# combine
DF <- data.frame(
   year = 1995:2018,
   citations = c(citationsYear, citationsYearAND, citationsYearAND2, citationsYearAND3, citationsYearAND4),
   proportion = c(citationsYear, citationsYearAND, citationsYearAND2, citationsYearAND3, citationsYearAND4) / citationsYear,
   label = factor(rep(c("All", "Process model","Agent-based model","Formal model","Computational model"), each = length(citationsYear)),
      levels = c("Computational model","Process model","Formal model", "Agent-based model", "All"))
)
DF <- as.data.table(DF)

# start year
from <- 2001
DF <- DF[year>=from,]

# increase of citation rate
DF[year %in% range(year), proportion[which.max(year)] / pmax(proportion[which.min(year)], .000000001), by = label]

# Mean annual growth rate (mittlere diskrete Wachstumsrate)
ny <- DF[, max(year)- min(year)]
DF[, (citations[which.max(year)] / citations[which.min(year)])^(1/ny) - 1, by = label]

# Plot
ggplot(DF[DF$year>=from,], aes(x = year, y = proportion, colour = label)) +
   geom_line(aes(linetype = label)) +
   ggtitle("Relative Citation Frequency") +
   labs(subtitle = 'Citations of models in CogSci and JDM relativ to citations in CogSci and JDM') +
   scale_x_continuous("Year", breaks=DF$year, limits=c(from,NA)) +
   scale_y_continuous("Proportion of Citations", labels = percent, lim = range(DF$proportion)) +
   scale_colour_manual(values = c("black", "grey20", "grey20", "grey20")) +
   scale_linetype_manual(values=c(3,1,2,6)) +
   guides(colour = guide_legend("Term", , keywidth = 1.), linetype = guide_legend("Term")) +
   theme(legend.position = "top", legend.key = element_blank(), legend.key.height = unit(.8, "lines"), legend.background = element_rect(fill = NA))

# Save
ggsave("../figures/fig1_citations.png", plot = last_plot(), dpi = 300, width = 8, height = 6, scale = .8)