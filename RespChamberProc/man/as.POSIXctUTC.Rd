\name{as.POSIXctUTC}
\alias{as.POSIXctUTC}
\title{as POSIXctUTC}
\description{construct a time in timezone UTC and set tzone attribute to UTC for correct printing}
\usage{as.POSIXctUTC(x)}
\arguments{
  \item{x}{An object to be converted.}
}



\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#tmp1 <- as.POSIXct(c("2014-06-23 00:00:01","2014-06-23 05:00:01"))
#plot(tmp1)    
tmp <- as.POSIXctUTC(c("2014-06-23 00:00:01","2014-06-23 05:00:01"))
print(tmp)
# note that teh following it is not converted to local 
# time e.g. CET, but ggplot respects timezone attribute
plot(tmp)    
#ggplot(data.frame(date=tmp, y=seq_along(tmp)), aes(date,y)) + geom_point() 
}
