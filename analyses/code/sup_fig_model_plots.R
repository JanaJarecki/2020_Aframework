################################################################################
# Supplementary plot tha tis not used in the paper
# Highlighting the ratings of certain models
# By Jana Jarecki, last edited 30.01.2019, jj@janajarecki.com
################################################################################
library(themejj)
theme_set(themejj()) #personal theme use theme_bw() as replacement

cols = c("powderblue","deeppink","plum","mediumspringgreen","lightslategrey")


d = read.csv("../../data/processed/models.csv", sep=";", header=T)
d$Area_label = factor(d$Area, levels=rev(names(sort(table(d$Area)))))
d$Class_label = factor(d$Class, levels=names(sort(table(d$Class))))

# Models by areas
ggplot(d, aes(x=Area_label)) +geom_histogram(width=.66, color=cols[5], fill=cols[1], stat = "count") +scale_x_discrete("Area") +
scale_y_continuous("Number of Models") +ggtitle("Count of Models by Area") + theme(axis.text.x=element_text(size=12, angle=90, hjust=1, vjust=.25), legend.position="none")

# Models by family
ggplot(d, aes(x=Class_label)) +scale_colour_grey() +geom_histogram(width=.66, color="black", fill="grey90", stat = "count") +scale_x_discrete("Family") +
scale_y_continuous("Number of Models") +ggtitle("Count of Model Types") +coord_flip() +
theme(aspect.ratio=1.681, axis.text.x=element_text(size=12, angle=90, hjust=1, vjust=.25), legend.position="none")
