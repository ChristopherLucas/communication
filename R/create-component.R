create_component <- function(config, input = 'frames', 
                             component_type, component_data,
                             n_features = 1,
                             output = NULL,
                             output_define = T) {
  
  if (is.null(output) ) output <- paste0(sample(letters), collapse="")
  
  
  # define component_type
  config[['componentInstances:cComponentManager']][[paste0('instance[', output, '].type')]] <- component_type

  
  config <- add_component_data(config, component_data,  component_type, input, output, n_features)

  
  if (output_define) {
    config <- update_output(config, output, n_features)
  }
  
  config
}




add_component_data <- function(config, component_data,  component_type, input, output, n_features) {
  columns <- attr(config, "columns")
  edges <- attr(config, "edges")
  

  list_output <- paste0(output, ":", component_type)
  list_component_data <- c(component_data,
                           list(`reader.dmLevel` = input,
                                `writer.dmLevel` = output)) %>% 
    list() %>%
    purrr::set_names(list_output)
  
  config <- c(config,list_component_data )
  
  class(config) <- "audio_config"
  attr(config, "columns") <- columns 
  attr(config, "edges") <- rbind(edges, data.frame(input = input, output = output,
                                                   nFeatures = n_features,
                                                   explicit_output = F,
                                                   stringsAsFactors = F))
  config
}


update_output <- function(config, output, n_features) {
  config$`lldrcppdatasink:cRcppDataSink`$`reader.dmLevel` <- sub("^;","", 
                                                                 paste0(
                        config$`lldrcppdatasink:cRcppDataSink`$`reader.dmLevel`,
                            ";", output))

  attr(config, "columns") <- paste0(c(attr(config, "columns"), 
                                      paste0(output, 1:n_features)), collapse = ":" )
  
  attr(config, "edges")$explicit_output[attr(config, "edges")$output == output] <- T
  config
}



fill_graph_matrix <- function(graph_matrix, input, output) {
  names <- colnames(graph_matrix)
  col_number <- which(names == input)
  row_number <- which(names == output)
  graph_matrix[row_number, col_number] <- 1
  graph_matrix
}
create_graph_matrix <- function(edges) {
  nodes <- unique(c(edges$input, edges$output))
  graph_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes))
  colnames(graph_matrix) <- nodes
  rownames(graph_matrix) <- nodes
  purrr::map2(edges$input, edges$output, ~ fill_graph_matrix(graph_matrix, .x, .y)) %>%
    purrr::reduce(`+`)
  
}


#' @export
plot.audio_config <- function(x, ...) {
  pos <- create_graph_position(attr(x, "edges"))
  attr(x, "edges") %>%
    create_graph_matrix() %>%
    diagram::plotmat(pos = pos,
                     box.size = 0.1,
                     box.type = "", curve = 0, cex.txt = 0)
}


create_graph_position <- function(edges, rest = 0) {
  if (nrow(edges) == 0)
    return(rest)
  only_input <- setdiff(edges$input, edges$output)
  edges_not_only_input <- edges[edges$input != only_input,]
  edges_only_input <- edges[edges$input == only_input,]
  rest2 <- length(setdiff(edges_only_input$output, edges_not_only_input$input))
  return(c(length(only_input) + rest,
    create_graph_position(edges_not_only_input, rest2)
  ))
   
}