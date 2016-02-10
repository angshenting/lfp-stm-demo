# Lightly modified from an original demo by Molly Roberts
# Import our libraries
library(stm)
library(stringr)

# Read in the data
data <- read.csv("~/stm_demo/SingaporeanIdentityReference.csv", sep="\t", quote="")

# Read in stopwords
stopwords <- read.csv("~/stm_demo/all_stopwords.txt",header=F)
stopwords[] <- lapply(stopwords,as.character)
stopwords <- stopwords$V1

# Process the data
processed <- textProcessor(data$Contents, metadata=data, customstopwords=stopwords)
# Note the use of customstopwords - this is optional

# Process the metadata
meta <- processed$meta
# Process URLs
meta$origin <- sapply(meta$URL, function (x) str_split(x, "http://")[[1]][2])
meta$origin <- sapply(meta$origin, function (x) str_split(x, "/")[[1]][1])
# Check if it's from Facebook. You can substitite other patterns
meta$facebook <- ifelse(meta$origin=="www.facebook.com", 1,0)
meta$facebook[is.na(meta$facebook)] <- 0
# Process the date
meta$date <- as.numeric(as.Date(meta$Date..GMT., origin="1899/12/31"))
out <- prepDocuments(processed$documents, processed$vocab, meta)

# Call the STM function to fit
mod.out <- stm(out$documents, out$vocab, 30, prevalence = ~facebook + s(date),
               data=out$meta)

# Plot topics - this is similar to LDA still.
plot.STM(mod.out, xlim=c(0,.5), labeltype="prob", n=30)

# Additional in STM, estimation over covariates
prep <- estimateEffect(1:30~facebook + s(date), mod.out, metadata=out$meta)
plot.estimateEffect(prep, "date", topics=26, method="continuous")

#Topic 8: Entitlement topic
findThoughts(mod.out, meta$Content, topics=8)

#Topic 11 & 13 more recent topics.
#Topic 11: Protests against foreigners
#Topic 13: Migrant worker strike

findThoughts(mod.out, meta$Content, topics=20, n=5)


#Topic 15: Analyses of Singaporean identity -- more at the beginning of the period
#Topic 16: Racism awareness, small incidents of racism and how to deal with them.
#Topic 18: Language -- increases over time
#Topic 21: Speech by PM Lee
#Topic 23: Riot
#Topic 25: Advertisement to work on Singapore identity project
#Topic 30: Sports

#Talk about main topics of identity over 2008-2014 -- constant over time
#Food,
#Art,
#Societal Values cluster
plot.estimateEffect(prep, "date", topics=c(26,4,12), method="continuous", xaxt="n", ylim=c(-.05, .4), labeltype="custom", custom.labels=c("Food",
                                                                                                                            "Societal Values", "Art"))
axis(1, at=seq(14025,16222, by=365), labels=seq(2008,2014))
findThoughts(mod.out, data$Content,topics=4,n=7)

#"I am attempting to capture features of a neighbourhood which are typical of Singapore’s
#heritage and tradition. Besides Shop Houses, I have also included old buildings,
#famous food places, hawker centres and many more. I hope that after looking at it,
#Singaporeans can see our country differently and realize the importance of heritage sites
#as they define part of our Singaporean identity and core."


#Talk about immigration worries, increasing over time
#Foreigners & growth
#Immigration
#Riots
#Labor Strikes
#Xenophobia
#Citizenship
plot.estimateEffect(prep, "date", topics=c(11,13,18,8,16,27), method="continuous", xaxt="n", ylim=c(0, .35), labeltype="custom", custom.labels=rev(c("Singaporean Future", "Immigration Protests",
                                                                                                                                    "Labor Strikes", "Xenophobia",
                                                                                                                                   "Foreigners and Growth", "Citizenship")))
axis(1, at=seq(14025,16222, by=365), labels=seq(2008,2014))
findThoughts(mod.out, data$Content,topics=27,n=7)
#"From concerns about smaller living spaces and the possible erosion of the Singaporean identity,
#to whether the Republic will preserve its racial mix, the anxiety of some senior citizens
#on the influx of foreigners was apparent during discussions yesterday as part of the
#Our Singapore Conversation project"

#History cluster: decreasing over time
#Chinese Culture: 5
#PAP: 2
#Religion: 7
#Singaporean History:22

plot.estimateEffect(prep, "date", topics=c(2,5,7,22), method="continuous", xaxt="n", labeltype="custom", custom.labels=c("PAP", "Chinese Culture",
                    "Religion", "Singaporean History"), ylim=c(-.1, .3))
axis(1, at=seq(14025,16222, by=365), labels=seq(2008,2014))
findThoughts(mod.out, data$Content,topics=5,n=7)
#"Well, the government has started yet another push to elevate Chinese culture again over all'others'.
#My question is, as it has always been, where does it leave the Malays and Indians?"
findThoughts(mod.out, data$Content,topics=2,n=7)
labels <- c("Sociological", "PAP", "Southeast Asia", "Food", "Chinese Culture",
            "Slang", "Religion", "Singaporean Future", "Racially Mixed Identity",
            "Elections", "Protests over Immigration", "Societal Values",
            "Labor Strike", "Malaysia", "General Words", "Xenophobia", "Names",
            "Foreigners & Growth", "Language", "Population White Paper", "Population growth",
            "Singaporean History", "Riots", "Hodge-podge", "Education Project", "Art",
            "Citizenship", "Immigration", "National Day", "Sports")

corr <- topicCorr(mod.out, cutoff=0)
plot.topicCorr(corr, vlabels=labels, vertex.color="lightblue")

plot.estimateEffect(prep, "date", topics=c(21,28), method="continuous")

