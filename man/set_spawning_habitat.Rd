% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set-spawning-habitat.R
\name{set_spawning_habitat}
\alias{set_spawning_habitat}
\title{Set Spawning Habitat Area}
\usage{
set_spawning_habitat(watershed, species, flow, ...)
}
\arguments{
\item{watershed}{one of the watersheds defined for the SIT model}

\item{species}{one of 'fr' (Fall Run), 'sr' (Spring Run), 'lfr' (Late Fall Run), or 'st' (Steelhead)}

\item{flow}{a flow value in cubic feet per second used to determine habitat area}
}
\value{
habitat area in square meters
}
\description{
This function returns an estimated spawning area based on watershed, species, and flow.
}
\details{
The function relies on a dataframe called
\code{\link{modeling_exist}} that contains data on whether the species is present in a watershed
and whether habitat modeling exists.
If a model for the watershed does exist, the function looks up the flow to weighted usable area (WUA) relationship
(e.g. \code{\link{battle_creek_instream}}) and selects the correct WUA for the
given flow and species. This WUA is then multiplied by the watershed's
typical spawning habitat extent length (stored in \code{\link{watershed_lengths}}),
to return an estimate of suitable spawning habitat within the watershed.
When additional species modeling is not available, the fall run WUA
values are used (lengths are modified if the habitat extent varies across species).


\strong{Regional Approximation:}
When a watershed has no associated flow to WUA reltionship, a regional approximation is made.
First, the mean WUA at the given flow vale from a set of similar modeled watersheds nearby is calculated.
Then the mean WUA is multiplied by the river length of the watershed of interest.

Below are the regions (defined by the downstream watershed) that contain
watersheds with unmodeled spawning relationships. The modeled watersheds
used to approximate spawning area for the unmodeled watersheds
are marked with an asterisk.



\strong{Upper-mid Sacramento River}
\itemize{
  \item Battle Creek*
  \item Bear Creek
  \item Big Chico Creek
  \item Butte Creek*
  \item Clear Creek*
  \item Cottonwood Creek*
  \item Cow Creek
  \item Deer Creek
  \item Elder Creek
  \item Mill Creek
  \item Paynes Creek
  \item Stony Creek
  \item Thomes Creek
}
\strong{South Delta}
\itemize{
  \item Calaveras River*
  \item Cosumnes River
  \item Mokelumne River*
}
}
\examples{
# determine the spawning habitat area at Cottonwood Creek for Fall Run Chinook at flow 1567
set_spawning_habitat("Cottonwood Creek", "fr", 1567)

# determine spawning habitat for a watershed with no modeling, uses region approximation
set_spawning_habitat("Antelope Creek", "fr", 1597)

}
