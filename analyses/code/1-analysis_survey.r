################################################################################
# Analysis of the process model survey
# By Jana Jarecki, last edited 30.01.2019, jj@janajarecki.com
################################################################################
library(data.table)     #for data handling

d = fread("../../data/processed/survey.csv")
setnames(d, gsub("_.*", "", names(d)))

# How many respondents in total?
length(unique(d$id))



################################################################################# Exclude participants
################################################################################
d = d[level != "Masters student"] # MA students
d = d[id != "R_3kuELulavePsRkl"] # did not take survey seriously
d[, length(unique(id))]

################################################################################# Descriptive Analysis
################################################################################
# Check: Do we have all 116 models?
d[, length(unique(model))]

# How many respondents?
d[, length(unique(id))]
# 62

# How many think PM are important?
d[!duplicated(id), table(important)]
# No Yes 
# 11  51 
# 82 %

# Which expertise level?
setkey(d, id)
d[!duplicated(id), table(level)]
# Doctoral student         Professor         Research scientist 
#               11                35                16

# Have they taught a methods class?
d[!duplicated(id), table(instructor)]
# No, I have never taught a research methodology class.   Yes, I am urrently.        Yes, I have. 
#                                                    15                     5                  41 
 

# How many models did respondents know?
d[, sum(isPM!="No opinion"), by=c("id","level")][, list(min=min(V1), mw=mean(V1), max=max(V1)), by=level]

# Is Marr important
d[!duplicated(id), table(marr)]
d[!duplicated(id) & marr=="Yes", .N, by=marrclarifies][, sum(N), by = cut(marrclarifies, c(0,3,4,7), ordered = TRUE)]



############################################################################### Interrater-reliability: (Fleiss & Cuzick, 1979)
################################################################################
# Fleiss-Cuzick Interrater-Reliability (Fleiss & Cuzick, 1979)
source("kappaFC.R") # load function

# Kappa for all models and all raters
N =     d[isPM!="No opinion", length(unique(model))] #total number of models
ni =    d[isPM!="No opinion", .N, by=model]$N #number of judges rating the ith model
xi =    d[isPM!="No opinion", sum(isPM=="Yes"), by=c("model")]$V1 #number of positive judgments on the ith model
kappa = kappaFC(N,ni,xi)
kappa$kappa

# Kappa for the different seniority levels and all models
for (i in c("Professor ", "Researcher", "Doctoral student ")) {
    N =     d[isPM!="No opinion" & level==i, length(unique(model))] #total number of models
    ni =    d[isPM!="No opinion" & level==i, .N, by=model]$N #number of judges rating the ith model
    xi =    d[isPM!="No opinion" & level==i, sum(isPM=="Yes"), by=c("model")]$V1 #number of positive judgments on the ith model

    kappa = kappaFC(N,ni,xi)

    cat(i, kappa$kappa, "\n")
}

#################################################
# For which models was there most disagreement?
tmp <- d[isPM!="No opinion", list(N=.N, MW=mean(isPM=="Yes"), SUM = sum(isPM=="Yes")), by=model][MW < .60 & MW > .40]
setorder(tmp, -N)
tmp

d[, isPM := factor(isPM, levels = c("No opinion", "Yes", "No"), ordered = TRUE)]
 
library(plotly)
p <- ggplot(d, aes(x = model, fill = isPM)) +geom_bar(width = .6, color = "black", position = "dodge") +scale_fill_manual("Is this model a process model?", values = c("white", "grey70", "grey30")) +scale_y_continuous(expand = c(0,0)) +coord_flip() +theme(legend.direction = "vertical", legend.pos = c(-1.2,.95)) +geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = .65), hjust = -1)

ggsave("../figures/gcm_randomwalk_process_model.png")


############################################################################### Open-ended responses
###############################################################################
d[!duplicated(id), "definePM", with=F]

write.table(d[!duplicated(id)], file.path(ddir,"tmp.csv"), sep=";", row.names=F)







#######################################################################################################################
# Plots for presentations
#######################################################################################################################
library(ggplot2)
library(scales)
library(themejj)
theme_set(themejj())   #set theme for plotting, comment this out
cols = c("turquoise","grey80","plum","orange","blue") #colours for plotting
# # Sort models by judgments
setkey(d,mwIsPM,model) #sort
ord = d[isPM!="No opinion", unique(model)]
d[isPM!="No opinion", model_f := factor(model, levels=ord)]

# # Plot demographics
setkey(d,id)
ggplot(d[!duplicated(id)], aes(x=discipline, fill=level)) + geom_bar(width=.5) +theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.3)) +
    scale_x_discrete("") +  scale_y_continuous("Count") +ggtitle("Respondent Characteristics") +
    scale_fill_manual(values = cols[c(1,2,4)])
ggsave("fig1-demogr.png", h=7)


setkey(d,id)
ggplot(d[unique(id),mult="first"], aes(x=important, fill=level)) + geom_bar(width=.5) +theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.3)) +
    scale_x_discrete("") +  scale_y_continuous("Count") +ggtitle("Do you think process models are important?") +scale_fill_manual(values = cols)
ggsave("fig4.png_PMimportant", h=7)


# ggplot(d[unique(id),mult="first"], aes(x=MarrDefinesPM, fill=level)) + geom_bar() +
#     scale_x_discrete("", labels=c("1\nDoes not\nclarify at all","2\n\n","3\n\n","4\nNeutral\n","5\n\n","6\n\n","7\n\n","8\nClarifies\ncompletely")) +
#     scale_y_continuous("Count") +ggtitle("Do you think Marr's levels clarify 'process models'?") +scale_fill_manual(values = cols) +facet_grid(~Marr)
# ggsave("fig5.png_MarrClarifies")

ggplot(data.frame(x=rep(1:length(ord),each=10), isPM=factor(c(rep(c(1,1,1,1,1,1,1,1,1,2),.5*length(ord)), rep(c(2,2,2,2,2,2,2,2,1,2),.5*length(ord))),labels=c("No","Yes"))), aes(x=x, fill=isPM)) + geom_bar(width=.7, position="fill") +
    scale_x_discrete("models") + scale_y_continuous("Opinion scaled to 100 %", breaks=seq(0,1,.1), labels=percent) +
    ggtitle("HYPOTHETICAL DISTRIBUTION\n\nIs this model a process model? ") +
    scale_fill_manual(values = cols[c(2,1)]) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), aspect.ratio=1/4, legend.position="top", legend.direction = "horizontal")
ggsave("fig6.1_isPMexp.png", w=10, h=5)

ggplot(d[isPM!="No opinion"], aes(x=model_f, fill=isPM)) + geom_bar(width=.7, position="fill") +
    scale_x_discrete("models") + scale_y_continuous("Opinion scaled to 100 %", breaks=seq(0,1,.1), labels=percent) +
    ggtitle("SURVEY RESPONSE DISTRIBUTION\n\nIs this model a process model?") +
    #facet_grid(level~.) +
    #scale_fill_grey() +
    geom_hline(aes(yintercept=.5)) +
    scale_fill_manual(values = cols[c(2,1)]) +
    theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), aspect.ratio=1/4, legend.position="top", legend.direction = "horizontal")
ggsave("fig6.2_isPMsurv.png", w=10, h=5)

p <- last_plot()
p + annotate("rect", xmax=Inf, xmin=0, ymin=0, ymax=.75, fill="white", alpha=.7)

# FFH models highlighted
ffts <- "Brunswik|Tree|WADD|TTB|Lexicographic|Matching Heuristic|PH|EW|Naive Bayes"

highlight_models <- function(pattern) {
    ord <- tolower(ord)
    pattern <- tolower(pattern)
    pos.ffh = grep(pattern, ord)
    log.ffh = grepl(pattern, ord)
    lab.ffh = ord
    lab.ffh[!log.ffh] = ""
    lab.ffh = sub("\n \\(.*?\\)", "", lab.ffh) #remove citation

    p + geom_vline(xintercept=setdiff(1:116,pos.ffh), color="white", alpha=.9, size=3) +theme(axis.text.x=element_text(angle = 90, hjust=1)) +
        scale_x_discrete("models", labels=lab.ffh)
}

highlight_models(ffts)


# drift-diffucion
ddf <- "diff"
highlight_models(ddf) +labs(caption = paste("N =", length(unique(d$id))))


# ggsave("fig6.3.png")

# # Right of TTB
# pos.ffh = grep("^TTB", ord):116
# log.ffh = grepl("^TTB", ord)
# lab.ffh = ord
# lab.ffh[1:pos.ffh] = ""
# lab.ffh = sub("\n \\(.*?\\)", "", lab.ffh) #remove citation 
# p + geom_vline(xintercept=setdiff(1:116,pos.ffh), color="white", alpha=.9, size=3) +theme(axis.text.x=element_text(angle = 90, hjust=1)) +
#     scale_x_discrete("models", labels=lab.ffh)
# ggsave("fig6.4.png")


# #models of Tim
# setkey(d,model_f)
# ggplot(d[ord[pos.ffh]], aes(x=model_f, fill=isPM)) + geom_bar(width=.5, position="fill") +
#     scale_x_discrete("") + scale_y_continuous("Opinion scaled to 100 %") +ggtitle(paste("Is this model a process model?")) +
#     scale_fill_manual(values = cols[c(2,1)]) +
#     theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.3), aspect.ratio = 1, legend.position="top", legend.direction="horizontal") +
#     coord_flip()
# ggsave("fig7.png", scale=1.2)

# ggplot(d[ord[pos.ffh]], aes(x=model_f, fill=isPM)) + geom_bar(width=.5, position="dodge") +
#     scale_x_discrete("") + scale_y_continuous("Number of Responses") +ggtitle(paste("Is this model a process model?")) +
#     scale_fill_manual(values = cols[c(3,4)]) +
#     theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.3), aspect.ratio = 1, legend.position="top", legend.direction="horizontal") +
#     coord_flip()
# ggsave("fig7.2.png", scale=1.2)

# # models of Tim
# pos.ffh = grep("Pleskac", ord)
# setkey(d,model_f)
# ggplot(d[ord[pos.ffh]], aes(x=model_f, fill=isPM)) + geom_bar(width=.5, position="fill") +
#     scale_x_discrete("") + scale_y_continuous("Opinion scaled to 100 %") +ggtitle(paste("Is this model a process model?")) +
#     scale_fill_manual(values = cols[c(2,1)]) +
#     theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.3), aspect.ratio = 1, legend.position="top", legend.direction="horizontal") +
#     coord_flip()
# setkey(d,model_f)
# ggplot(d[ord[pos.ffh]], aes(x=model_f, fill=isPM)) + geom_bar(width=.5, position="dodge") +
#     scale_x_discrete("") + scale_y_continuous("Number of Responses", breaks=seq(0,14,2)) +ggtitle(paste("Is this model a process model?")) +
#     scale_fill_manual(values = cols[c(3,4)]) +
#     theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.3), aspect.ratio = 1, legend.position="top", legend.direction="horizontal") +
#     coord_flip()



