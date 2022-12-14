#' Set Spawning Habitat Area
#' @description This function returns an estimated spawning area based on watershed, species, and flow.
#'
#' @param watershed one of the watersheds defined for the SIT model
#' @param species one of 'fr' (Fall Run), 'sr' (Spring Run), 'lfr' (Late Fall Run), or 'st' (Steelhead)
#' @param flow a flow value in cubic feet per second used to determine habitat area
#' @return habitat area in square meters
#' @export
#' @examples
#' # determine the spawning habitat area at Cottonwood Creek for Fall Run Chinook at flow 1567
#' set_spawning_habitat("Cottonwood Creek", "fr", 1567)
#'
#' # determine spawning habitat for a watershed with no modeling, uses region approximation
#' set_spawning_habitat("Antelope Creek", "fr", 1597)
#'
#' @details The function relies on a dataframe called
#' \code{\link{modeling_exist}} that contains data on whether the species is present in a watershed
#' and whether habitat modeling exists.
#' If a model for the watershed does exist, the function looks up the flow to weighted usable area (WUA) relationship
#' (e.g. \code{\link{battle_creek_instream}}) and selects the correct WUA for the
#' given flow and species. This WUA is then multiplied by the watershed's
#' typical spawning habitat extent length (stored in \code{\link{watershed_lengths}}),
#' to return an estimate of suitable spawning habitat within the watershed.
#' When additional species modeling is not available, the fall run WUA
#' values are used (lengths are modified if the habitat extent varies across species).
#'
#'
#' \strong{Regional Approximation:}
#' When a watershed has no associated flow to WUA reltionship, a regional approximation is made.
#' First, the mean WUA at the given flow vale from a set of similar modeled watersheds nearby is calculated.
#' Then the mean WUA is multiplied by the river length of the watershed of interest.
#'
#' Below are the regions (defined by the downstream watershed) that contain
#' watersheds with unmodeled spawning relationships. The modeled watersheds
#' used to approximate spawning area for the unmodeled watersheds
#' are marked with an asterisk.
#'
#'
#'
#' \strong{Upper-mid Sacramento River}
#' \itemize{
#'   \item Battle Creek*
#'   \item Bear Creek
#'   \item Big Chico Creek
#'   \item Butte Creek*
#'   \item Clear Creek*
#'   \item Cottonwood Creek*
#'   \item Cow Creek
#'   \item Deer Creek
#'   \item Elder Creek
#'   \item Mill Creek
#'   \item Paynes Creek
#'   \item Stony Creek
#'   \item Thomes Creek
#' }
#' \strong{South Delta}
#' \itemize{
#'   \item Calaveras River*
#'   \item Cosumnes River
#'   \item Mokelumne River*
#' }
set_spawning_habitat <- function(watershed, species, flow, ...) {
  
  species_present <- all(subset(DSMhabitat::watershed_species_present, 
                                watershed_name == watershed,
                                c(species, 'spawn'), drop = TRUE))
  
  if (!species_present) {
    return(NA)
  }
  
  quantification_mode <- subset(DSMhabitat::watershed_methods, 
                                watershed_name == watershed, 
                                spawning, drop = TRUE)

  if (watershed == 'Upper Sacramento River') {
    return(set_upper_sac_spawn_habitat(species, flow, ...))
  }
  
  # TDOO --- needs improvement!!
  if (DSMhabitat::watershed_species_present$use_mid_sac_spawn_proxy[DSMhabitat::watershed_species_present$watershed_name == watershed]) {
    if (watershed == "Deer Creek" & species != "sr") {
      w <- watershed
      s <- species  
    } else {
      w <- "Upper Mid Sac Region"
      s <- "fr"
      quantification_mode <- "wua"
    }
  } else {
    w <- watershed
    s <- species
  }
  watershed_name <- tolower(gsub(pattern = "-| ", replacement = "_", x = w))

  watershed_rda_name <- paste(watershed_name, "instream", sep = "_")
  df <- as.data.frame(do.call(`::`, list(pkg = "DSMhabitat", name = watershed_rda_name)))

  hab_column <- get_habitat_selector(names(df), s, "spawn", mode = quantification_mode)
  df_na_rm <- df[!is.na(df[, hab_column]), ]
  flows <- df_na_rm[ , "flow_cfs"]
  habs <- df_na_rm[ , hab_column]
  hab_func <- approxfun(flows, habs , rule = 2)


  if (quantification_mode == "wua") {
    wua <- hab_func(flow)
    habitat_area <- wua_to_area(wua = wua, watershed = watershed,
                                life_stage = "spawning", species_name = species)
    
  } else if (quantification_mode == "hsi") {
    habitat_area <- hab_func(flow)
  }

  return(habitat_area)

}

set_upper_sac_spawn_habitat <- function(species, flow, month) {
  # this is composed of two different curves, the first is the board in and second is boards out
  # board IN months 4-10
  # board OUT months 1-3, 11-12

  # for fall run fry modeling does not exist so we use fall run juv
  if (species == 'wr') {
    # winter run spawning
    upper_sac_IN_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_in$flow_cfs,
                                     DSMhabitat::upper_sac_ACID_boards_in$WR_spawn_WUA, rule = 2)

    upper_sac_OUT_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_out$flow_cfs,
                                      DSMhabitat::upper_sac_ACID_boards_out$WR_spawn_WUA, rule = 2)
  } else if (species == 'st') {
    # steelhead spawning
    upper_sac_IN_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_in$flow_cfs,
                                     DSMhabitat::upper_sac_ACID_boards_in$ST_spawn_WUA, rule = 2)

    upper_sac_OUT_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_out$flow_cfs,
                                      DSMhabitat::upper_sac_ACID_boards_out$ST_spawn_WUA, rule = 2)
    
  } else if (species == 'lfr') {
    # steelhead spawning
    upper_sac_IN_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_in$flow_cfs,
                                     DSMhabitat::upper_sac_ACID_boards_in$LFR_spawn_WUA, rule = 2)
    
    upper_sac_OUT_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_out$flow_cfs,
                                      DSMhabitat::upper_sac_ACID_boards_out$LFR_spawn_WUA, rule = 2)
  } else {
    # fall run and spring run spawning
    upper_sac_IN_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_in$flow_cfs,
                                     DSMhabitat::upper_sac_ACID_boards_in$FR_spawn_WUA, rule = 2)

    upper_sac_OUT_approx <- approxfun(DSMhabitat::upper_sac_ACID_boards_out$flow_cfs,
                                      DSMhabitat::upper_sac_ACID_boards_out$FR_spawn_WUA, rule = 2)
  }


  wua <- if (month < 4 | month > 10) {
    upper_sac_OUT_approx(flow)
  } else {
    upper_sac_IN_approx(flow)
  }

  habitat_area <- wua_to_area(wua = wua, watershed = 'Upper Sacramento River',
                              life_stage = "spawning", species_name = species)

  return(habitat_area)
}
