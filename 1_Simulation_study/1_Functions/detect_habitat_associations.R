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
  
  # get class id of associated habitat
  habitat <- as.numeric(stringr::str_sub(species_type, -1))
  
  # determine if positive or negative association
  association <- stringr::str_extract(species_type, "(?<=_).+?(?=_)")
  
  opposite <- ifelse(test = association == "positive", 
                     yes = "negative", no = "positive")
  
  
  correct <- sum(input$significance[input$habitat == habitat] == association, 
                 na.rm = TRUE)
  
  false <- sum(sum(input$significance[input$habitat != habitat] == association, 
                   na.rm = TRUE),
               sum(input$significance[input$habitat == habitat] == opposite, 
                   na.rm = TRUE),
               sum(input$significance[input$habitat == habitat] == "n.s.", 
                   na.rm = TRUE), 
               na.rm = TRUE)
  
  # count correct/wrong detections of habitat associations
  result_summarised <- c(species_code = species_code, 
                         variable = variable,
                         correct = correct,
                         false = false)
  
  return(result_summarised)
}
