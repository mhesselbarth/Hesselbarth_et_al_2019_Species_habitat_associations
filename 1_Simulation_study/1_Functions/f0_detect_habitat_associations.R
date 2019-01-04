#' Detection of habitat association
#'
#' Correct and false detections of the results of habitat associations
#' @param input List created with results_habitat_associations
#' @param species_type Species name
#' @param species_code Species id
#' @param variable Level of analysed variable

#'
#' @return Tibble with correct and false detections

#' @export
detect_habitat_associations <- function(input, 
                                        species_type, species_code, 
                                        variable){
  
  habitat <- as.numeric(stringr::str_sub(species_type, -1))
  
  association <- stringr::str_extract(species_type, "(?<=_).+?(?=_)")
  
  result_summarised <- c(species_code = species_code, variable = variable,
                         correct = sum(input$significance[input$habitat == habitat] == association, na.rm = TRUE),
                         false = sum(input$significance[input$habitat != habitat] == association, na.rm = TRUE))

  return(result_summarised)
}
