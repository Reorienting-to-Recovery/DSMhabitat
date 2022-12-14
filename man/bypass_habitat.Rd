% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{bypass_habitat}
\alias{bypass_habitat}
\alias{yolo_bypass_habitat}
\alias{sutter_bypass_habitat}
\title{Bypass Flow to Habitat Area Relationships}
\source{
\href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/bypasses.html}{Sutter and Yolo Bypass Modeling Details}
}
\description{
A dataset containing the suitable habitat area in square meters as a
function of flow in cubic feet per second.
}
\section{yolo_bypass_habitat}{

\describe{
  \item{flow_cfs}{integer flow value in cubic feet per second}
  \item{Yolo Bypass 1}{suitable rearing area in square meters in the Yolo Bypass, Fremont Weir to Sacramento Weir}
  \item{Yolo Bypass 2}{suitable rearing area in square meters in the Yolo Bypass below Sacramento Weir}
}
}

\section{sutter_bypass_habitat}{

\describe{
  \item{flow_cfs}{integer flow value in cubic feet per second}
  \item{Sutter Bypass 1}{suitable rearing area in square meters in the Sutter Bypass, to Moulton Weir}
  \item{Sutter Bypass 2}{suitable rearing area in square meters in the Sutter Bypass, to Colusa Weir}
  \item{Sutter Bypass 3}{suitable rearing area in square meters in the Sutter Bypass, to Tisdale Weir}
  \item{Sutter Bypass 4}{suitable rearing area in square meters in the Sutter Bypass below Tisdale Weir}
}
}

\examples{
yolo_bypass_habitat

}
\keyword{datasets}
