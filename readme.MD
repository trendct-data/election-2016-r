
# Presidential Elections in Connecticut 2016

This repository contains data and methodlogy behind the following Trend CT stories:

* [Real-time results on election night](https://blogotron.ctmirror.org/election-2016/page/white-house)
* [Programatically created result graphics for social and live blogging](https://blogotron.ctmirror.org/election-2016/)
* [Clinton won Connecticut but Trump made gains in the state](http://trendct.org/2016/11/10/clinton-won-connecticut-but-trump-made-gains-in-the-state/)
* [CT voter participation rates increased in 2016 compared to 2012](http://trendct.org/2016/11/14/voter-participation-rates-increased-for-2016-election-compared-to-2012-in-ct/)
* [What the 2016 presidential election returns reveal about Connecticut voters](http://www.trendct.org)

# Walkthroughs

* 2016 CT post-election analysis
 - [[HTML](https://trendct-data.github.io/election-2016-r/)] [[RMarkdown](https://github.com/trendct-data/election-2016-r/blob/master/index.Rmd)] 
* Analysis II (Correlations between votes and selected Census and DataHaven data points)
 - [[HTML](https://trendct-data.github.io/election-2016-r/analysis2.html)] [[RMarkdown](https://github.com/trendct-data/election-2016-r/blob/master/analysis2.Rmd)] 
 * Analysis III (Correlations between votes and all DataHaven data points)
 - [[HTML](https://trendct-data.github.io/election-2016-r/analysis3.html)] [[RMarkdown](https://github.com/trendct-data/election-2016-r/blob/master/analysis3.Rmd)] 
 * Analysis IV (Correlations between diff in 2016 and 2016 votes and selected Census and DataHaven data points)
 - [[HTML](https://trendct-data.github.io/election-2016-r/analysis4.html)] [[RMarkdown](https://github.com/trendct-data/election-2016-r/blob/master/analysis4.Rmd)] 
 
### The Data

* 2012 and 2016 presidential election results by town from the Office of the Secretary of the State
* 2012 voter turnout stats
* DataHaven 2015 Wellness Survey data by town
* Transformed data indicating which towns switched parties by supporting Clinton or Trump
* List of polling places in Connecticut
* 2016 registered voters by town
* town population density by town from the Census
* 2016 voter turnout calculations by town
* Classification list of towns by urban, rural, or mixed
* Total presidential votes back to 2000

### What's in this repo

* `data` **folder** - Folder with raw election results and participation rates and transformed data
* `maps` **folder** - Folder of shapefiles to generate maps
* `output` **folder** - Charts and maps generated on election night to be used on social media or the live blog
* `pres_map_generator.R` - Pulls data from the SotS and creates a town by town map and chart graphic
* `turnout_estimate.R` - Calculates turnout by town based on SotS online results
* `usa_map.R` -  Pulls data from a Google Spreadsheet and generates a U.S. electoral votes results map

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. [Read more](http://www.trendct.org/data).

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct-data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.

This walkthrough explains the methodology behind adjusting income in the Trend CT story [Would your job make you better or worse off in another state?](http://occupation.trendct.org/)

