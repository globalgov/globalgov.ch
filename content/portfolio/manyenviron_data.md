---
title: Environmental treaties
output: html_document
category:
- Data Packages
image: images/clients/manyenviron_hexlogo.png
type: portfolio
---



[manyenviron](https://globalgov.github.io/manyenviron/) is a data package in the many universe of packages. It currently includes an ensemble of datasets on international environmental agreements, and states’ membership or other relationships to those agreements.

With the help of the [manydata](https://manydata.ch/) portal we have made it easy to install and work with global governance data in R. Simply install the core package, manydata, as follows, and then you can discover, install and update various “many” packages from the console.



```r
#utils::install.packages("manydata")
library(manydata)
#manydata::call_packages() # to see all available package
manydata::call_packages("manyenviron") # to download manyenviron
data(package = "manyenviron")
knitr::kable(manydata::call_sources("manyenviron", "agreements"))
```

To "compare" the data available in manyenviron, you can use the `compare_*` functions in manydata.


```r
data("agreements", package = "manyenviron")
manydata::compare_dimensions(agreements)
plot(manydata::compare_overlap(agreements))
plot(manydata::compare_categories(agreements))
```

We can also consolidate these datasets with the `consolidate()` function.


```r
consolidate(datacube = emperors, rows = "every", cols = "every", resolve = "random", key = "ID")
```
