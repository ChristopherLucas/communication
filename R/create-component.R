#' Add an individual component to an existing configuration object
#'
#' This function appends a (non-sink) component to an existing configuration object.
#' It is used internally by the pipeline function, and is unlikely to be used by
#' itself.
#'
#' @param config A configuration object
#' @param component An individual component as a List object, with the first element
#' being the 'type' of the component (for example 'cTransformFFT').
#' @param is_last Whether this component is the last one to be added, in which case
#' its output channel will be set to the sink component, and the sink component's
#' input channel updated accordingly.
#' @return A configuration object suitable for use with extractFeatures.
pipeline.append <- function(config, component, is_last = F) {
  type <- component[[1]]
  input <- component$.input
  output <- component$.output
  n_features <- component$.n_features
  
  edges <- attr(config, 'edges')
  if (is.null(input)) input <- ifelse(is.null(edges), 'frames', edges[nrow(edges),]$output)
  if (is.null(output)) output <- paste0(type, '_', ifelse(is.null(edges), 1, nrow(edges)+1))
  if (is.null(n_features)) n_features <- 1
  
  # Any attributes that are unnamed or not .input/.output/.n_features are passed on as component data
  component_data = component[names(component) %in% c('', '.input', '.output', '.n_features') == F]
  create_component(config, input, type, component_data, n_features, output, is_last)
}

#' Construct a processing pipeline
#'
#' This function creates a new Configuration composed of a pipeline of components.
#' Individual components of the pipeline are named automatically, but at a mininum
#' the type of each component (and any additional options needed for the component)
#' should be specified during construction.
#' 
#' Linear pipelines can be constructed easily by laying out the individual components
#' as individual List objects, where the first element of each List is the component
#' type (for example 'cWindower'). The input and output level names of each component
#' are determined automatically, but can be overridden, allowing for creation of 
#' more complex pipelines.
#'
#' @param ... Individual components specified as individual List objects.
#' @return A configuration object suitable for use with extractFeatures.
#' @export
pipeline <- function(...) {
  config <- createConfig()
  components = list(...)
  for (c in head(components, -1)) {
    config <- pipeline.append(config=config, component=c, is_last=F)
  }
  last <- tail(components, 1)[[1]]
  config <- pipeline.append(config=config, component=last, is_last=T)
}

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

  
  if (n_features == 1) {
    # If adding a single output, set the column name the same as the output name
    attr(config, "columns") <- paste0(c(attr(config, "columns"), output), collapse = ":" )      
  } else {
    # Otherwise set column names as output + a suffix (_1/_2/..)
    attr(config, "columns") <- paste0(c(attr(config, "columns"),
                                        paste0(output, '_', 1:n_features)), collapse = ":" )    
  }

  
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