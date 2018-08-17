#' Detection of habitat association
#'
#' Correct and false detections of the results of habitat associations
#' @param input [\code{list(1)}]\cr List created with Results.Habitat.Associations
#' @param species_code [\code{numeric(1)}]\cr Species_id

#'
#' @return Tibble with correct and false detections

#' @export
detect_habitat_associations <- function(input, 
                                        species_type, species_code, 
                                        variable){
  
  habitat <- as.numeric(stringr::str_sub(species_type, -1))
  
  association <- stringr::str_extract(species_type, "(?<=_).+?(?=_)")
  
  result_summarised <- c(Species_code = species_code, Variable = variable,
                         Correct = sum(input$Significance[input$Habitat == habitat] == association, na.rm=T),
                         False = sum(input$Significance[input$Habitat != habitat] == association, na.rm=T))

  return(result_summarised)
}
