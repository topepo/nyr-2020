## Materials for the 2020 NYR Machine Learning workshop

The materials will be posted a day or some prior to the workshop (this page will be updated). 

For software, there are two options: 

 * Install software locally on your computer
 * Use an RStudio Server Pro instance that we will provide on the day of the first workshop session. 
 
 The software requirements are: 
 
 ```r
 pkgs <- c('discrim', 'earth', 'ggrepel', 'glmnet', 'klaR', 'lubridate', 
           'rpart', 'stringr', 'tidymodels', 'vip', 'xgboost')

install.packages(pkgs, repos = "http://cran.rstudio.com")

# to test: 
library(tidymodels)
library(glmnet)
library(xgboost)
```

It is important to have **version 0.1.1** of the `tidymodels` package. 
 
We will have some slack channels that will be used for general questions as well as technical issues. More information will be coming for those. In the meantime, email `max@rstudio.com` if you have questions or issues.  
 
If you are interested in reading materials prior to the course, I would suggest taking a look at the first three chapters of the [Feature Engineering](https://bookdown.org/max/FES) book. Specifically, Chapter 3 ["_A Review of the Predictive Modeling Process_"](https://bookdown.org/max/FES/review-predictive-modeling-process.html) is a good overview of many of the topics that will be covered. 

If you are unfamiliar with the tidyverse, I suggest reviewing [_R for Data Science_](https://r4ds.had.co.nz/). 
