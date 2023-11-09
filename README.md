### Modeled Habitat Areas

*This package is for estimating spawning and rearing (instream and floodplain) habitat within the Sacramento and San Joaquin river systems for use with the Reorienting to Recovery salmon life cycle model.*

#### Installation

``` r
# install.packages("remotes")
remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
```

#### Usage

This package provides habitat related datasets to the [`fallRunDSM,`](https://github.com/Reorienting-to-Recovery/fallRunDSM) [`springRunDSM,`](https://github.com/Reorienting-to-Recovery/springRunDSM) and [`winterRunDSM,`](https://github.com/Reorienting-to-Recovery/winterRunDSM).

``` r
# datasets within the package
data(package = 'DSMhabitat')
```

#### About the Models

This data package includes flow to suitable habitat area relationships for salmonid (Fall Run, Spring Run, Winter Run, Late Fall Run, and steelhead - pending) spawning, instream rearing, and floodplain rearing habitat.

Where available, results from Instream Flow Incremental Methodology (IFIM) studies were used to generate instream spawning and rearing flow to suitable area relationships. For watersheds without IFIM (or comparable) studies, suitable instream areas were scaled from nearby, geomorphically similar watersheds.

Similarly, where available, results from floodplain hydraulic modeling studies were used to generate floodplain flow to suitable area relationships. Where no modeling studies were available, suitable floodplain area were scaled from nearby, geomorphically similar watersheds. Specific methods and supporting documents for instream and floodplain habitat inputs in every watershed are provided on the reference tab.

Additional scaling is applied to existing habitat data to account for current and planned restoration projects and to produce theoretical maximum habitat data.  

### Dependencies

The `DSMhabitat` package provides data for several other packages within the [Reorienting to Recovery Project](https://github.com/Reorienting-to-Recovery. These relationships are visualized in the dependency graph below. <img src="man/figures/dependencyChain.svg" width="100%"/>


Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/>


