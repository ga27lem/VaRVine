### Installation
The installation is best described through an example:
* Assuming your R script is located in `/dir/my-script.R`
* CLone the project into some directory `/another-dir/VaRVine`

* Now, in your `/dir/my-script.R`, run the following to install the package:
```
setwd(/another-dir)
library(devtools)
install("VaRVine")
library(VaRVine)
```
* Now, the package is loaded into your `/dir/my-script.R` and can be used.