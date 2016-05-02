library (survey)
library(foreign)
library(RCurl)

data(api)
srs_design <- svydesign(id=~1, fpc=~fpc, data=apisrs)
svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)


srs_design <- svydesign(id=~1, weights = ~pw, data=apisrs)
svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)


svytotal(~stype, srs_design)
means <- svymean(~api00+api99, srs_design)
svycontrast(means, c(api00=1,api99=-1))

srs_design <- update(srs_design, apidiff=api00-api99)
srs_design <- update(srs_design, apipct = apidiff/api99)

svymean(~apidiff+apipct, srs_design)


strat_design <- svydesign(id=~1, strata=~stype, fpc = ~fpc, data=apistrat)
svytotal(~enroll, strat_design)
svymean(~enroll, strat_design)
svytotal(~stype, strat_design)


data <- read.dta("http://r-survey.r-forge.r-project.org/svybook/Adult.dta")
chris_data <- data

chris <- svrepdesign(variables = chris_data[,1:418],
                     repweights = chris_data[,420:499],
                     weights = chris_data[,419],
                     combined.weights = T,
                     type="other",
                     scale = 1,
                     rscale = 1)


p <- getURL("https://raw.githubusercontent.com/xcodevn/SADP/master/nwts/nwts-share.csv")
popn <- read.csv(text = p)

cases <- subset (popn, relaps==1)
cases$wt <- 1
ctrls <- subset(popn ,relaps==0)
ctrls$wt <- 10
samp <- rbind(cases, ctrls[sample(nrow(ctrls), 325) ,]) 

fivesurv <-function(time , status ,w){
    scurve <- survfit(Surv(time,status)~1, weights=w)
    scurve$surv[min(which(scurve$time > 5))]
}

des <- svydesign(id=~1, strata=~relaps, weights=~wt, data=samp)
jkdes <- as.svrepdesign(des)
withReplicates(jkdes, quote(fivesurv(tre1, relaps, .weights)))

svyquantile(~bmi_p, design=chris,quantiles=c(0.25,0.5,0.75))

svyquantile(~api00, strat_design, c(0.25, 0.5, 0.75) ,ci=TRUE)

tab<-svymean(~interaction(ins,smoking,drop=TRUE),chris)

row.names(t) <- 1:nrow(t)
