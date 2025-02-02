library('ggplot2')

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

# df <- read.csv("./user_study_results/studyResults.csv", header=T)
df <- read.csv("./studyResults_without_Sandra.csv", header=T)
df$Difficulty = as.factor(df$Difficulty)

tgc <- summarySE(df, measurevar="Accuracy", groupvars=c("Difficulty"))
print(tgc)
# Use dose as a factor rather than numeric
tgc2 <- tgc
# tgc2$dose <- factor(tgc2$dose)

# Error bars represent standard error of the mean
ggplot(tgc2, aes(x=Difficulty, y=Accuracy, fill=Difficulty)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))


# # Use 95% confidence intervals instead of SEM
# ggplot(tgc2, aes(x=Difficulty, y=Time.in.page, fill=Difficulty)) + 
#     geom_bar(position=position_dodge(), stat="identity") +
#     geom_errorbar(aes(ymin=Time.in.page-ci, ymax=Time.in.page+ci),
#                   width=.2,                    # Width of the error bars
#                   position=position_dodge(.9))