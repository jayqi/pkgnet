AbstractGraphVizAdapter <- R6::R6Class(
    classname = "AbstractGraphVizAdapter"
    , public = list(
        initialize = function(nodes, edges, pkg_graph) {
            # Store pointers to node and edge data.tables
            private$protected$nodes <- nodes
            private$protected$edges <- edges
            private$protected$pkg_graph <- pkg_graph
            return(invisible(self))
        }
        , print = function() {
            print(self$viz_object)
            return(invisible(self))
        }
    )
    , active = list(
        # Read-only access to node and edge data.tables
        nodes = function(){return(private$protected$nodes)}
        , edges = function(){return(private$protected$edges)}
        , pkg_graph = function(){return(private$protected$pkg_graph)}

        , generate_viz_call = function(){return(private$protected$generate_viz_call)}

        # Read-only access to viz object
        , viz_object = function() {
            if (is.null(private$protected$viz_object)) {
                private$protected$viz_object <- eval(self$generate_viz_call)
            }
            return(private$protected$viz_object)
        }

    )
    , private = list(
        protected = list(
            viz_object = NULL
        )

        , cache = list()

        , reset_cache = function() {
            private$cache <- list()
        }
    )
)

VisNetworkAdapter <- R6::R6Class(
    classname = "VisNetworkAdapter"
    , inherit = AbstractGraphVizAdapter
    , public = list(
    )
    , active = list(
        plot_nodes = function() {
            if (is.null(private$cache$plot_nodes)) {
                plotDT[, .(
                    id = node
                    , label = node
                    , group = self$groupCol
                )]
            }
            return(private$cache$plot_nodes)
        }

        , plot_edges = function() {
            if (is.null(private$cache$plot_edges)) {
                plotDT[, .(
                    from = SOURCE
                    , to = TARGET
                    , color = '#848484'
                )]
            }
            return(private$cache$plot_edges)
        }
    )
    , private = list(

    )



)

NetworkD3ForceAdapter <- R6::R6Class(
    classname = "NetworkD3ForceAdapter"
    , inherit = AbstractGraphVizAdapter

    , public = list(
        initialize = function(...) {
            private$protected <- c(super$protected, private$protected)
            return(super$initialize(...))
        }
    )

    , active = list(
        plot_nodes = function(){
            if (is.null(private$cache$plot_nodes)) {
                private$convert_igraph_to_networkD3()
            }
            return(private$cache$plot_nodes)
        }
        , plot_links = function(){
            if (is.null(private$cache$plot_links)) {
                private$convert_igraph_to_networkD3()
            }
            return(private$cache$plot_links)
        }
    )

    , private = list(

        protected = list(
                generate_viz_call = quote(
                    networkD3::forceNetwork(
                        Links = self$plot_links
                        , Nodes = self$plot_nodes
                        , Source = "source"
                        , Target = "target"
                        , NodeID = "name"
                        , Group = "group"
                        , opacity = 0.8
                        , bounded = TRUE
                        , zoom = TRUE
                        , charge = -10
                        #, arrows = TRUE
                    )
                )
        )

        , cache = list(
            plot_nodes = NULL
            , plot_links = NULL
        )

        , convert_igraph_to_networkD3 = function() {
            result <- networkD3::igraph_to_networkD3(
                g = self$pkg_graph$igraph_complete
                , group = rep(1, length(igraph::V(self$pkg_graph$igraph_complete)))
                , what = 'both'
            )
            private$cache$plot_nodes <- result$nodes
            private$cache$plot_links <- result$links
            return(invisible(NULL))
        }

    ) # /private
)




NetworkD3SimpleAdapter <- R6::R6Class(
    classname = "NetworkD3SimpleAdapter"
    , inherit = AbstractGraphVizAdapter

    , public = list(
        initialize = function(...) {
            private$protected <- c(super$protected, private$protected)
            return(super$initialize(...))
        }
    )

    , active = list(
        plot_links = function(){
            if (is.null(private$cache$plot_links)) {
                private$cache$plot_links <- self$edges[, .(
                    source = SOURCE
                    , target = TARGET
                )]
            }
            return(private$cache$plot_links)
        }
    )

    , private = list(

        protected = list(
            generate_viz_call = quote(
                networkD3::simpleNetwork(
                    Data = self$plot_links
                    , Source = 1
                    , Target = 2
                    , zoom = TRUE
                )
            )
        )

        , cache = list(
            plot_links = NULL
        )

    ) # /private
)
