#' @title Abstract Graph Reporter Class
#' @name AbstractGraphReporter
#' @description Defines the Abstract Class for all PackageGraphReporters defined in pkgnet.
#'              The class is not meant to be instantiated, but inherited from and its methods
#'              overloaded such that each Metric implements certain functionality.
#' @family AbstractReporters
#' @section Public Members:
#' \describe{
#'    \item{\code{edges}}{A data.table from SOURCE to TARGET nodes describing the connections}
#'    \item{\code{nodes}}{A data.table with node as an identifier, and augmenting information about each node}
#'    \item{\code{pkg_graph}}{An igraph object describing the package graph}
#'    \item{\code{network_measures}}{A list of network measures calculated by \code{calculate_network_features}}
#'    \item{\code{layout_type}}{Character string indicating currently active graph layout}
#'    \item{\code{graph_viz}}{\code{visNetwork} object of package graph}
#' }
#' @section Active Bindings:
#' \describe{
#'    \item{\code{pkg_graph}}{Returns the graph object}
#'    \item{\code{network_measures}}{Returns a table of network measures, one row per node}
#'    \item{\code{graph_viz}}{Returns the graph visualization object}
#'    \item{\code{orphan_nodes}}{Returns the list of orphan nodes}
#'    \item{\code{layout_type}}{If no value given, the current layout type for the graph visualization is returned.
#'        If a valid layout type is given, this function will update the layout_type field.}
#'    \item{\code{orphan_node_clustering_threshold}}{If no value given, the current orphan node clustering threshold is returned.
#'        If a valid orphan node clustering threshold is given, this function will update the orphan node clustering threshold.}
#' }
#' @importFrom data.table data.table copy uniqueN
#' @importFrom R6 R6Class
#' @importFrom igraph degree graph_from_edgelist graph.edgelist centralization.betweenness
#' @importFrom igraph centralization.closeness centralization.degree hub_score
#' @importFrom igraph layout_as_tree layout_in_circle neighborhood.size page_rank V vcount vertex
#' @importFrom magrittr %>%
#' @importFrom methods hasArg formalArgs
#' @importFrom visNetwork visNetwork visHierarchicalLayout visEdges visOptions
#' @export
AbstractGraphReporter <- R6::R6Class(
    "AbstractGraphReporter",
    inherit = AbstractPackageReporter,
    active = list(
        pkg_graph = function(){
            if (is.null(private$cache$pkg_graph)){
                private$cache$pkg_graph <- DirectedGraph$new(self$nodes, self$edges)
            }
            return(private$cache$pkg_graph)
        },
        network_measures = function(){
            if (is.null(private$cache$network_measures)){
                self$pkg_graph$calculate_all_measures()
            }
            return(private$cache$network_measures)
        },
        graph_viz = function(){
            if (is.null(private$cache$graph_viz)) {
                log_info('Creating graph visualization plot...')
                private$cache$graph_viz <- private$plot_network()
                log_info('Done creating graph visualization plot.')
            }
            return(private$cache$graph_viz)
        },
        layout_type = function(value) {

            # If the person isn't using <- assignment, return the cached value
            if (!missing(value)) {
                if (!value %in% names(private$graph_layout_functions)) {
                    log_fatal(paste("Unsupported layout_type:", value))
                }
                if (!is.null(private$cache$graph_viz)) {
                    private$reset_graph_viz()
                }
                private$private_layout_type <- value
            }
            return(private$private_layout_type)
        }
    ),

    private = list(
        plotNodeColorScheme = list(
            field = NULL
            , pallete = '#97C2FC'
        ),

        # Create a "cache" to be used when evaluating active bindings
        # There is a default cache to reset to
        cache = list(
            nodes = NULL,
            edges = NULL,
            pkg_graph = NULL,
            network_measures = NULL,
            graph_viz = NULL,
            orphan_nodes = NULL
        ),

        private_orphan_node_clustering_threshold = 10,
        private_layout_type = "tree",

        # Calculate graph-related measures for pkg_graph
        calculate_network_measures = function(){
            self$pkg_graph$calculate_all_measures()
            return(invisible(NULL))
        },

        # Variables for the plot
        set_plot_node_color_scheme = function(field
                                              , pallete){

            # Check field is length 1 string vector
            if (typeof(field) != "character" || length(field) != 1) {
                log_fatal(paste0("'field' in set_plot_node_color_scheme must be a string vector of length one. "
                                 , "Coloring by multiple fields not supported."))
            }

            # Confirm All Colors in pallete are Colors
            areColors <- function(x) {
                sapply(x, function(X) {
                    tryCatch({
                        is.matrix(col2rgb(X))
                    }, error = function(e){
                        FALSE
                    })
                })
            }

            if (!all(areColors(pallete))) {
                notColors <- names(areColors)[areColors == FALSE]
                notColorsTXT <- paste(notColors, collapse = ", ")
                log_fatal(sprintf("The following are invalid colors: %s"
                                  , notColorsTXT))
            }

            private$plotNodeColorScheme <- list(
                field = field
                , pallete = pallete
            )

            log_info(sprintf("Node color scheme updated: field [%s], pallete [%s]."
                             , private$plotNodeColorScheme[['field']]
                             , paste(private$plotNodeColorScheme[['pallete']], collapse = ",")
            ))

            return(invisible(NULL))
        },

        # Creates visNetwork graph viz object
        # Uses pkg_graph active binding
        plot_network = function(){

            log_info("Creating plot...")

            # TODO:
            # Open these up to users or remove all the active binding code
            self$layout_type <- "tree"

            # format for plot
            plotDTnodes <- data.table::copy(self$nodes) # Don't modify original
            plotDTnodes[, id := node]
            plotDTnodes[, label := id]

            log_info(paste("Plotting with layout:", self$layout_type))
            plotDTnodes <- private$calculate_graph_layout(
                plotDT = plotDTnodes
            )

            if (length(self$edges) > 0) {
                plotDTedges <- data.table::copy(self$edges) # Don't modify original
                plotDTedges[, from := SOURCE]
                plotDTedges[, to := TARGET]
                plotDTedges[, color := '#848484'] # TODO Make edge formatting flexible too
            } else {
                plotDTedges <- NULL
            }

            # Color By Field
            if (is.null(private$plotNodeColorScheme[['field']])) {

                # Default Color for all Nodes
                plotDTnodes[, color := private$plotNodeColorScheme[['pallete']]]

            } else {

                # Fetch Color Scheme Values
                colorFieldName <- private$plotNodeColorScheme[['field']]

                # Check that that column exists in nodes table
                if (!is.element(colorFieldName, names(self$nodes))) {
                    log_fatal(sprintf(paste0("'%s' is not a field in the nodes table",
                                             " and as such cannot be used in plot color scheme.")
                                      , private$plotNodeColorScheme[['field']])
                    )
                }

                colorFieldPallete <- private$plotNodeColorScheme[['pallete']]
                colorFieldValues <- plotDTnodes[[colorFieldName]]
                log_info(sprintf("Coloring plot nodes by %s..."
                                 , colorFieldName))

                # If colorFieldValues are character
                if (is.character(colorFieldValues) | is.factor(colorFieldValues)) {

                    # Create pallete by unique values
                    valCount <- data.table::uniqueN(colorFieldValues)
                    newPallete <- grDevices::colorRampPalette(colors = colorFieldPallete)(valCount)

                    # For each character value, update all nodes with that value
                    plotDTnodes[, color := newPallete[.GRP]
                                , by = list(get(colorFieldName))]

                } else if (is.numeric(colorFieldValues)) {
                    # If colorFieldValues are numeric, assume continuous

                    # Create Continuous Color Pallete
                    newPallete <- grDevices::colorRamp(colors = colorFieldPallete)

                    # Scale Values to be with range 0 - 1
                    plotDTnodes[!is.na(get(colorFieldName)), scaledColorValues := get(colorFieldName) / max(get(colorFieldName))]

                    # Assign Color Values From Pallete
                    plotDTnodes[!is.na(scaledColorValues), color := grDevices::rgb(newPallete(scaledColorValues), maxColorValue = 255)]

                    # NA Values get gray color
                    plotDTnodes[is.na(scaledColorValues), color := "gray"]

                } else {
                    # Error Out
                    log_fatal(sprintf(paste0("A character, factor, or numeric field can be used to color nodes. "
                                             , "Field %s is of type %s.")
                                      , colorFieldName
                                      , typeof(colorFieldValues)
                    )
                    )

                } # end non-default color field

            } # end color field creation

            # Create Plot
            g <- visNetwork::visNetwork(nodes = plotDTnodes
                                        , edges = plotDTedges) %>%
                visNetwork::visHierarchicalLayout(sortMethod = "directed"
                                                  , direction = "UD") %>%
                visNetwork::visEdges(arrows = 'to') %>%
                visNetwork::visOptions(highlightNearest = list(enabled = TRUE
                                                               , degree = nrow(plotDTnodes) # guarantee full path
                                                               , algorithm = "hierarchical"))

            log_info("Done creating plot.")

            # Save plot in the cache
            private$cache$graph_viz <- g

            return(g)
        },

        # Function to reset cached graph_viz
        reset_graph_viz = function() {
            log_info('Resetting cached graph_viz...')
            private$cache$graph_viz <- NULL
            return(invisible(NULL))
        },

        # Identify orphan nodes
        identify_orphan_nodes = function() {
            orphan_nodes <- base::setdiff(
                self$nodes[, node]
                , unique(c(self$edges[, SOURCE], self$edges[, TARGET]))
            )

            # If there are none, then will be character(0)
            return(orphan_nodes)
        },

        graph_layout_functions = list(
            "tree" = function(pkg_graph) {igraph::layout_as_tree(pkg_graph)},
            "circle" = function(pkg_graph) {igraph::layout_in_circle(pkg_graph)}
        ),

        calculate_graph_layout = function(plotDT) {

            log_info(paste("Calculating graph layout for type:", self$layout_type))

            # Calculate positions for specified layout_type
            graph_func <- private$graph_layout_functions[[self$layout_type]]
            plotMat <- graph_func(self$pkg_graph$igraph_connected)

            # It might be important to get the nodes from pkg_graph so that they
            # are in the same order as in plotMat?
            coordsDT <- data.table::data.table(
                node = names(igraph::V(self$pkg_graph$igraph_connected))
                , level = plotMat[, 2]
                , horizontal = plotMat[, 1]
            )

            # Merge coordinates with plotDT
            plotDT <- merge(
                x = plotDT
                , y = coordsDT
                , by = 'node'
                , all.x = TRUE
            )

            return(plotDT)
        }
    )
)

DirectedGraph <- R6::R6Class(
    classname = "DirectedGraph"

    , public = list(
        initialize = function(nodes, edges) {
            # Store pointers to node and edge data.tables
            private$protected$nodes <- nodes
            private$protected$edges <- edges

            # Generate igraph objects
            private$initialize_igraph()

            return(invisible(self))
        }

        , node_measure = function(measure){
            assertthat::assert_that(
                measure %in% self$available_node_measures()
                , msg = sprintf('%s not in $available_node_measures()', measure)
            )
            if (!measure %in% names(self$nodes)) {
                result <- private$node_measure_functions[[measure]](self)
                resultDT <- data.table::data.table(
                    node_name = names(result)
                    , result = result
                )
                setkeyv(resultDT, 'node_name')
                self$nodes[, eval(measure) := resultDT[node, result]]
            }
            return(self$nodes[, .SD, .SDcols = c('node', measure)])
        }

        , available_node_measures = function(){
            names(private$node_measure_functions)
        }

        , graph_measure = function(measure){
            assertthat::assert_that(
                measure %in% self$available_graph_measures()
                , msg = sprintf('%s not in $available_graph_measures()', measure)
            )
            if (!measure %in% names(private$protected$graph_measures)) {
                result <- private$graph_measure_functions[[measure]](self)
                private$protected$graph_measures[[measure]] <- result
            }
            return(private$protected$graph_measures[[measure]])
        }

        , available_graph_measures = function(){
            names(private$graph_measure_functions)
        }

        , calculate_all_node_measures = function(){
            lapply(
                X = self$available_node_measures()
                , FUN = function(x){
                    self$node_measure(x)
                    NULL
                }
            )
            return(self$nodes[, .SD, .SDcols = c('node', self$available_node_measures())])
        }

        , calculate_all_graph_measures = function(){
            lapply(
                X = self$available_graph_measures()
                , FUN = function(x){
                    self$graph_measure(x)
                    NULL
                }
            )
            return(self$graph_measures)
        }

        , calculate_all_measures = function(){
            self$calculate_all_node_measures
            self$calculate_all_graph_measures
            return(invisible(NULL))
        }

    ) # /public

    , active = list(
        # Read-only access to node and edge data.tables
        nodes = function(){return(private$protected$nodes)}
        , edges = function(){return(private$protected$edges)}

        # Read-only list storing graph measures
        , graph_measures = function(){return(private$protected$graph_measures)}

        # Read-only access to igraph objects
        , igraph_complete = function(){
            return(private$protected$igraph_complete)
        }
        , igraph_connected = function(){
            return(private$protected$igraph_connected)
        }
        , igraph_unconnected = function(){
            return(private$protected$igraph_unconnected)
        }
    ) # /active

    , private = list(
        protected = list(
            nodes = NULL
            , edges = NULL
            , igraph_complete = NULL
            , igraph_connected = NULL
            , igraph_unconnected = NULL
            , graph_measures = list()
        )

        , initialize_igraph = function(){

            # Connected graph
            if (nrow(self$edges) > 0) {
                # A graph with edges
                connectedGraph <- igraph::graph.edgelist(
                    as.matrix(self$edges[,list(SOURCE,TARGET)])
                    , directed = TRUE
                )
            } else {
                connectedGraph <- igraph::make_empty_graph()
            }

            # Unconnected graph
            orphanNodes <- base::setdiff(
                self$nodes[, node]
                , unique(c(self$edges[, SOURCE], self$edges[, TARGET]))
            )
            unconnectedGraph <- igraph::make_empty_graph() + igraph::vertex(orphanNodes)

            # Complete graph
            completeGraph <- connectedGraph + unconnectedGraph

            # Store in protected cache
            private$protected$igraph_connected <- connectedGraph
            private$protected$igraph_unconnected <- unconnectedGraph
            private$protected$igraph_complete <- completeGraph

            return(invisible(NULL))
        }

        # Functions for node measures
        # All functions should return a named vector of node measure values
        , node_measure_functions = list(

            # In Degree
            inDegree = function(self){
                igraph::degree(
                    graph = self$igraph_complete
                    , mode = "in"
                )
            }

            # Out Degree
            , outDegree = function(self){
                igraph::degree(
                    graph = self$igraph_complete
                    , mode = "out"
                )
            }

            # Betweenness
            , betweenness = function(self){
                igraph::betweenness(
                    graph = self$igraph_complete
                    , directed = TRUE
                )
            }

            # In Closeness
            # Closeness doesn't really work for directed graphs that are not
            # strongly connected.
            # igraph calculates a thing anyways and gives a warning
            , inCloseness = function(self){
                suppressWarnings(igraph::closeness(
                    graph = self$igraph_complete
                    , mode = "out"
                ))
            }

            # Out Closeness
            # Closeness doesn't really work for directed graphs that are not
            # strongly connected.
            # igraph calculates a thing anyways and gives a warning
            , outCloseness = function(self){
                suppressWarnings(igraph::closeness(
                    graph = self$igraph_complete
                    , mode = "out"
                ))
            }

            # Eigenvector Centrality
            , eigencentrality = function(self){
                igraph::eigen_centrality(
                    graph = self$igraph_complete
                    , directed = TRUE
                )$vector
            }

            # Number of Decendants
            , numDescendents = function(self){
                # Calculate using out-neighborhood or out-ego size with max order
                result <- igraph::neighborhood.size(
                    graph = self$igraph_complete
                    , order = igraph::vcount(self$igraph_complete)
                    , mode = "out"
                )
                names(result) <- igraph::V(self$igraph_complete)$name
                return(result)
            }

            # Number of Ancestors
            , numAncestors = function(self){
                # Calculate using in-neighborhood or out-ego size with max order
                result <- igraph::neighborhood.size(
                    graph = self$igraph_complete
                    , order = igraph::vcount(self$igraph_complete)
                    , mode = "in"
                )
                names(result) <- igraph::V(self$igraph_complete)$name
                return(result)
            }

            # Page Rank
            , pageRank = function(self){
                igraph::page_rank(
                    graph = self$igraph_complete
                    , directed = TRUE
                )$vector
            }

            # Hub Score
            , hubScore = function(self){
                igraph::hub_score(
                    graph = self$igraph_complete
                    , scale = TRUE
                )$vector
            }

            # Authority Score
            , authorityScore = function(self){
                igraph::authority_score(
                    graph = self$igraph_complete
                    , scale = TRUE
                )$vector
            }

        )

        , graph_measure_functions = list(

            graphInDegree = function(self){
                measure <- 'inDegree'
                igraph::centralize(
                    scores = self$node_measure(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_degree_tmax(
                        graph = self$igraph_complete
                        , mode = "in"
                        , loops = TRUE
                    )
                    , normalized = TRUE
                )
            }

            , graphOutDegree = function(self){
                measure <- 'outDegree'
                igraph::centralize(
                    scores = self$node_measure(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_degree_tmax(
                        graph = self$igraph_complete
                        , mode = "out"
                        , loops = TRUE
                    )
                    , normalized = TRUE
                )
            }

            , graphBetweenness = function(self){
                measure <- 'betweenness'
                igraph::centralize(
                    scores = self$node_measure(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_betw_tmax(
                        graph = self$igraph_complete
                        , directed = TRUE)
                    , normalized = TRUE
                )
            }

            , graphInCloseness = function(self){
                measure <- 'inCloseness'
                igraph::centralize(
                    scores = self$node_measure(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_clo_tmax(
                        graph = self$igraph_complete
                        , mode = "in")
                    , normalized = TRUE
                )
            }

            , graphOutCloseness = function(self){
                measure <- 'outCloseness'
                igraph::centralize(
                    scores = self$node_measure(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_clo_tmax(
                        graph = self$igraph_complete
                        , mode = "out")
                    , normalized = TRUE
                )
            }

            , graphEigencentrality = function(self){
                measure <- 'eigencentrality'
                igraph::centralize(
                    scores = self$node_measure(measure)[, get(measure)]
                    , theoretical.max = igraph::centr_eigen_tmax(
                        graph = self$igraph_complete
                        , directed = TRUE)
                    , normalized = TRUE
                )
            }
        )
    )  # /private
)
