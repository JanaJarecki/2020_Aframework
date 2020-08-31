################################################################################
# Analysis of the process model survey
# By Jana Jarecki, last edited 30.01.2019, jj@janajarecki.com
################################################################################
library(data.table)     #for data handling

##############################################################################
# Prepate and recode data, only first time you read the qualtrics raw data
##############################################################################
# Load and rename viables in the raw data
d = fread("../../data/raw/survey.csv", skip=1)
nam = names(fread("../../data/raw/survey.csv", sep = ",", skip=1))
nam[11:13] = c("consent","acceptConsent","instruction")
nam[18:133] = gsub("Ã¶","ö",gsub("Ã¤","ä",gsub("&amp;","&",sub("\n$","",sub("</strong>","\n",unlist(strsplit(nam[18:133],"<strong>"))[seq(2,232,by=2)])), fixed=T), fixed=T), fixed=T)
nam[138:148] = c("Marr","MarrDefinesPM","importancePM","definePM","instructor","discipline","disciplineTxt","field","level","levelTxt","comments")

# Make codebook
var = nam
des = names(fread("../../data/raw/survey.csv", skip=1))
fwrite(data.frame(variable=var, description=des),
  "../../data/raw/codebook.csv")

# Rename variables
setnames(d, nam)

# Substitute open-text entries of "Other"-answer into variables
d[disciplineTxt!="" & discipline=="Others", "discipline" := gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", tolower(disciplineTxt), perl=TRUE)]
d[level=="Others", level := levelTxt]
d[, disciplineTxt := NULL]  #remove text-column
d[, levelTxt := NULL]       #remove text-column

# Aggregate levels Lecturer/Post-doctoral fellow/researcher into researcher
d[, "levelOrig" := level]
d[grepl("Lecturer|Post-doctoral fellow|Researcher", level), level := "Research scientist (postdoc, lecturer, researcher)"]


# Remove non-finished people and make long-format
d <- melt(d[Finished==1], id = c("ResponseID", "StartDate", "EndDate", "Marr", "MarrDefinesPM", "importancePM", "level", "field"), measure.vars=18:133, na.rm=TRUE, variable.name="model", value.name="isprocessmodel")
d[isPM=="", isPM := "No opinion"]
d[, MarrDefinesPM := as.numeric(gsub("\\[|\\].*", "", MarrDefinesPM))]
Sys.setlocale("LC_ALL","English")
d[, StartDate := format(as.POSIXct(StartDate), "%d-%m-%Y %H:%M")]
##############################################################################
# Prepate and recode data, only first time you read the qualtrics raw data
##############################################################################
# Load and rename viables in the raw data
d = fread("../../data/raw/survey.csv", skip=1)
nam = names(fread("../../data/raw/survey.csv", sep = ",", skip=1))
nam[11:13] = c("consent","acceptConsent","instruction")
nam[18:133] = gsub("Ã¶","ö",gsub("Ã¤","ä",gsub("&amp;","&",sub("\n$","",sub("</strong>","\n",unlist(strsplit(nam[18:133],"<strong>"))[seq(2,232,by=2)])), fixed=T), fixed=T), fixed=T)
nam[138:148] = c("Marr","MarrDefinesPM","importancePM","definePM","instructor","discipline","disciplineTxt","field","level","levelTxt","comments")

# Make codebook
var = nam
des = names(fread("../../data/raw/survey.csv", skip=1))
fwrite(data.frame(variable=var, description=des),
  "../../data/raw/codebook.csv")

# Rename variables
setnames(d, nam)

# Substitute open-text entries of "Other"-answer into variables
d[disciplineTxt!="" & discipline=="Others", "discipline" := gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", tolower(disciplineTxt), perl=TRUE)]
d[level=="Others", level := levelTxt]
d[, disciplineTxt := NULL]  #remove text-column
d[, levelTxt := NULL]       #remove text-column

# Aggregate levels Lecturer/Post-doctoral fellow/researcher into researcher
d[, "levelOrig" := level]
d[grepl("Lecturer|Post-doctoral fellow|Researcher", level), level := "Research scientist (postdoc, lecturer, researcher)"]


# Remove non-finished people and make long-format
d <- melt(d[Finished==1], id = c("ResponseID", "StartDate", "EndDate", "Marr", "MarrDefinesPM", "importancePM", "level", "instructor", "discipline"), measure.vars=18:133, na.rm=TRUE, variable.name="model", value.name="isPM")
d[isPM=="", isPM := "No opinion"]
d[, MarrDefinesPM := as.numeric(gsub("\\[|\\].*", "", MarrDefinesPM))]
Sys.setlocale("LC_ALL","English")
d[, StartDate := format(as.POSIXct(StartDate), "%d-%m-%Y %H:%M")]
d[, EndDate := format(as.POSIXct(EndDate), "%d-%m-%Y %H:%M")]


setnames(d, 1:6, c("id", "start", "end", "marr_familiarMarrsLevels", "marrclarifies_1not7completely", "important_toBuildProcessModels"))

setkey(d, id, model)
# Save data
fwrite(d, "../../data/processed/survey.csv")
