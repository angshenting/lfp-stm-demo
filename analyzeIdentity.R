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
axis(1, at=seq(14025,16222, by=365), labels=seq(2008,2014))

# You can also plot over binary categories
plot.estimateEffect(prep, "facebook", topics=26, method="pointestimate")

# Displays the top 5 documents belonging to topic 8
findThoughts(mod.out, meta$Content, topics=8, n=5)


# You can also plot multiple topics and add custom labels
plot.estimateEffect(prep, "date", topics=c(26,4,12), method="continuous", xaxt="n", ylim=c(-.05, .4), labeltype="custom", custom.labels=c("Food",
                                                                                                                            "Societal Values", "Art"))

labels <- c("Sociological", "PAP", "Southeast Asia", "Food", "Chinese Culture",
            "Slang", "Religion", "Singaporean Future", "Racially Mixed Identity",
            "Elections", "Protests over Immigration", "Societal Values",
            "Labor Strike", "Malaysia", "General Words", "Xenophobia", "Names",
            "Foreigners & Growth", "Language", "Population White Paper", "Population growth",
            "Singaporean History", "Riots", "Hodge-podge", "Education Project", "Art",
            "Citizenship", "Immigration", "National Day", "Sports")

# Calculates Topic Correlations
corr <- topicCorr(mod.out, cutoff=0)
# Plots graph of correlations
plot.topicCorr(corr, vlabels=labels, vertex.color="lightblue")


