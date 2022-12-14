% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pad_structures}
\alias{pad_structures}
\alias{contact_points}
\alias{delta_contact_points}
\title{Number of Contact Points}
\format{
1 dimensional array [31 watersheds]
}
\source{
Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}

Compiled by: James T. Peterson \email{jt.peterson@oregonstate.edu}
}
\usage{
contact_points

delta_contact_points
}
\description{
Number of contact points, estimated using PAD
Contact points were derived from the Passage Assessment Database \href{https://nrm.dfg.ca.gov/PAD/view/query.aspx}{(PAD)}
maintained by California Department of Fish and Wildlife. Each location considered 
in the model (e.g., tributary, Sacramento reach, and delta subdivisions) was 
assessed for all structures identified in the PAD.
}
\details{
Delta data is length two (North, South)
Watershed data is length 31 (Mainstems, Bypasses, and Tributaries)
}
\keyword{datasets}
