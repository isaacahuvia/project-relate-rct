####  Reversed Question Difference  ####
#If the reversed items you provide this function are already coded to be in the same direction as other items, then a larger difference between means indicates careless responding
#If the reversed items you provide this function are still reverse-coded relative to the other items, then small (near-zero) difference between means indicates careless responding 
reversedItemDifference <- function(df, scaleLookup, reversedItems, columns = NULL) {
  
  #If the user specifies the columns to use (as a numeric vector), limit the analysis to those columns
  if(!is.null(columns)) df <- df[, columns]
  
  #Initialize the ouput vector so that the loop can populate it
  out <- c()
  
  #Limit to only scales with reversed items
  scaleLookup <- scaleLookup[scaleLookup[[1]] %in% (scaleLookup[[1]][scaleLookup[[2]] %in% reversedItems]),]
  
  #List unique scales
  scales <- unique(scaleLookup[[1]])
  
  for(i in 1:nrow(df)) {
    
    diff <- c()
    
    for(k in 1:length(scales)) {
      
      scale
      
      scale.normal <- df[i, names(df) %in% scaleLookup[scaleLookup[[1]] == scales[k],][[2]] & !names(df) %in% reversedItems]
      scale.reversed <- df[i, names(df) %in% scaleLookup[scaleLookup[[1]] == scales[k],][[2]] & names(df) %in% reversedItems]
      
      scale.normal <- as.numeric(scale.normal)
      scale.reversed <- as.numeric(scale.reversed)
      
      normal <- mean(scale.normal, na.rm = T)
      reversed <- mean(scale.reversed, na.rm = T)
      
      diff[k] <- abs(normal - reversed)
      
    }
    
    out[i] <- mean(diff, na.rm = T)
    
  }
  
  return(out)
  
}


items <- clean_data %>%
  select(
    matches("^b_|^pi_|^f_"),
    -matches("_mean$"),
    -matches("pfs"),
    -c(b_finished, f_finished, b_relstat, f_relstat)
  ) %>%
  colnames()

scale_lookup <- tibble(
  item = items
) %>%
  mutate(
    scale = gsub("\\_[0-9].*$", "", item)
  ) %>%
  select(scale, item)
  
reversed_items <- c(
  "b_rdm_8", "b_rdm_12",
  "f_rdm_8", "f_rdm_12",
  "b_ecr_1", "b_ecr_2", "b_ecr_3", "b_ecr_4",
  "f_ecr_1", "f_ecr_2", "f_ecr_3", "f_ecr_4",
  "b_iri_empcon_2", "b_iri_empcon_6", "b_iri_empcon_7", "b_iri_empcon_8",
  "f_iri_empcon_2", "f_iri_empcon_6", "f_iri_empcon_7", "f_iri_empcon_8"
)


clean_data$RID <- reversedItemDifference(
  df = clean_data,
  scaleLookup = scale_lookup,
  reversedItems = reversed_items
)

clean_data$RID

mean(clean_data$RID > 0)
mean(clean_data$RID > 1)
mean(clean_data$RID > 2)
mean(clean_data$RID > 3)
mean(clean_data$RID > 4)

clean_data %>% filter(
  RID < 3
) %>%
  select("b_rdm_" %+% 1:12) %>%
  alpha() %>%
  pluck("total")
