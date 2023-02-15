<img src="man/figures/cvpia_logo.jpg" align="right" width="40%"/>

### Modeled Habitat Areas

*This package is for estimating spawning and rearing (instream and floodplain) habitat within the Sacramento and San Joaquin river systems for use with the CVPIA salmon life cycle model.*

#### Installation

``` r
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/DSMhabitat")
```

#### Usage

This package provides habitat related datasets to the [`fallRunDSM,`](https://github.com/CVPIA-OSC/fallRunDSM) [`springRunDSM,`](https://github.com/CVPIA-OSC/springRunDSM) [`winterRunDSM,`](https://github.com/CVPIA-OSC/winterRunDSM) and [`latefallRunDSM`](https://github.com/CVPIA-OSC/latefallRunDSM) packages.

``` r
# datasets within the package
data(package = 'DSMhabitat')
```

#### About the Models

This data package includes flow to suitable habitat area relationships for salmonid (Fall Run, Spring Run, Winter Run, Late Fall Run, and steelhead - pending) spawning, instream rearing, and floodplain rearing habitat.

Where available, results from Instream Flow Incremental Methodology (IFIM) studies were used to generate instream spawning and rearing flow to suitable area relationships. For watersheds without IFIM (or comparable) studies, suitable instream areas were scaled from nearby, geomorphically similar watersheds.

Similarly, where available, results from floodplain hydraulic modeling studies were used to generate floodplain flow to suitable area relationships. Where no modeling studies were available, suitable floodplain area were scaled from nearby, geomorphically similar watersheds. Specific methods and supporting documents for instream and floodplain habitat inputs in every watershed are provided on the reference tab.

### Dependencies

The `DSMhabitat` package provides data for several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below. <img src="man/figures/dependencyChain.svg" width="100%"/>

### License

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

IP-117068


Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/>


