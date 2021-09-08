

### Installation
```
library(devtools)
install_github("ga27lem/VaRVine")

library(VaRVine)
```

### Usage
#### List of methods
Create a `GarchSettings` object:
``` 
garch_settings <- function(train.size=500, refit.every=50, specs=list())
```

Create a `VineSettings` object:

```
vine_settings <- function(train.size=250, refit.every=25, family.set='all')
```

Perform rolling window VaR forecast:

```
garch_vine_roll <- function(data = NULL, garch.settings = NULL, vine.settings = NULL, alpha = c(0.01, 0.05), weights=NULL)
```

Plot the foreasted Value-at-Risk:

```
VaR_plot <- function(garch.vine.roll = NULL, alpha = 0.05)
```

Backtest the VaR forecast:

```
backtest <- function(garch.vine.roll = NULL, alpha = 0.05)
```

#### Example
```
library(VaRVine)
library(rugarch)

data("sample_returns")
garch.specs <- list(
  GOOG = ugarchspec (
    variance.model = list(garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)),
    distribution.model = "norm"),
  AMZN = ugarchspec (
    variance.model = list(garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)),
    distribution.model = "sged")
)
garch.settings <- garch_settings(train.size = 750, refit.every = 50)
vine.settings <- vine_settings(train.size = 250, refit.every = 25, family.set = 'all')
roll <- garch_vine_roll(sample_returns, garch.settings,
                      vine.settings, alpha=0.05, weights=c(0.5, 0.2, 0.3))
head(roll@VaR.forecast)
```

### Parameter visualization
![Alt text](images/params.png)
