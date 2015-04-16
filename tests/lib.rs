#[macro_use] extern crate ductgraph;

#[macro_use]
pub mod edges;

#[macro_use]
pub mod vertices;

mod adjlist {
    mod abstr {
        mod directed {
            mod edges {
                use ::ductgraph::map::{
                    HashMap,
                    BTreeMap,
                };
                use ::ductgraph::graph::*;
                use ::ductgraph::graph::adjlist::{
                    AdjGraph,
                    AdjVertex,
                };
                use ::ductgraph::uuid::{
                    Factory,
                };
                adjgraph_type!(MyGraph, Abstract, Directed, Stable, Factory<usize>, usize, HashMap, BTreeMap);

                directed_edges_tests! {
                    {
                        let mut graph : MyGraph<u32, u32> = AdjGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        (graph, 0 , 1)
                    }
                }

                directed_neighbors_tests! {
                    {
                        let mut graph : MyGraph<u32, u32> = AdjGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        assert_eq!(graph.create(3), Ok((2, None)));
                        assert_eq!(graph.create(4), Ok((3, None)));
                        (graph, 0, 1, 2, 3)
                    }
                }

                generic_vertices_tests! {
                    {
                        let mut graph : MyGraph<&'static str, &'static str> = AdjGraph::new();
                        assert_eq!(graph.create("hey"), Ok((0, None)));
                        assert_eq!(graph.create("hey"), Ok((1, None)));
                        (graph, 0 , 1, 2)
                    }
                }

                create_vertex_tests! {
                    {
                        let mut graph : MyGraph<&'static str, &'static str> = AdjGraph::new();
                        assert_eq!(graph.create("hey"), Ok((0, None)));
                        assert_eq!(graph.create("hey"), Ok((1, None)));
                        (graph, 0 , 1, 2)
                    }
                }

                pub mod unstable {
                    use ::ductgraph::map::{
                        HashMap,
                        BTreeMap,
                    };
                    use ::ductgraph::graph::*;
                    use ::ductgraph::graph::adjlist::{
                        AdjGraph,
                        AdjVertex,
                    };
                    use ::ductgraph::uuid::{
                        Factory,
                    };
                    use ::ductgraph::interface::*;

                    adjgraph_type!(MyUnstableGraph, Abstract, Directed, Unstable, Factory<usize>, usize, HashMap, BTreeMap);

                    generic_vertices_tests! {
                        {
                            let mut graph : MyUnstableGraph<&'static str, &'static str> = AdjGraph::new();
                            assert_eq!(graph.create("hey"), Ok((0, None)));
                            assert_eq!(graph.create("hey"), Ok((1, None)));
                            (graph, 0 , 1, 2)
                        }
                    }

                    create_vertex_tests! {
                        {
                            let mut graph : MyUnstableGraph<&'static str, &'static str> = AdjGraph::new();
                            assert_eq!(graph.create("hey"), Ok((0, None)));
                            assert_eq!(graph.create("hey"), Ok((1, None)));
                            (graph, 0 , 1, 2)
                        }
                    }

                    unstable_vertices_tests! {
                        {
                            let mut graph : MyUnstableGraph<&'static str, &'static str> = AdjGraph::new();
                            assert_eq!(graph.create("hey"), Ok((0, None)));
                            assert_eq!(graph.create("hey"), Ok((1, None)));
                            (graph, 0 , 1, 2)
                        }
                    }
                }

            }
        }
        mod undirected {
            mod edges {
                use ::ductgraph::map::{
                    HashMap,
                    BTreeMap,
                };
                use ::ductgraph::graph::*;
                use ::ductgraph::graph::adjlist::{
                    AdjGraph,
                    AdjVertex,
                };
                use ::ductgraph::uuid::{
                    Factory,
                };
                adjgraph_type!(MyGraph, Abstract, Undirected, Stable, Factory<usize>, usize, HashMap, BTreeMap);

                undirected_edges_tests! {
                    {
                        let mut graph : MyGraph<u32, u32> = AdjGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        (graph, 0 , 1)
                    }
                }
                directed_neighbors_tests! {
                    {
                        let mut graph : MyGraph<u32, u32> = AdjGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        assert_eq!(graph.create(3), Ok((2, None)));
                        assert_eq!(graph.create(4), Ok((3, None)));
                        (graph, 0, 1, 2, 3)
                    }
                }
            }
        }
    }
    mod concrete {
        mod directed {
            mod edges {
                use ::ductgraph::map::{
                    HashMap,
                    BTreeMap,
                };
                use ::ductgraph::graph::*;
                use ::ductgraph::graph::adjlist::{
                    AdjGraph,
                    AdjVertex,
                };
                use ::ductgraph::uuid::{
                    UnitFactory,
                };
                adjgraph_type!(MyGraph, Concrete, Directed, usize, HashMap, BTreeMap);

                directed_edges_tests! {
                    {
                        let graph : MyGraph<u32> = AdjGraph::new();
                        (graph, 0 , 1)
                    }
                }
                directed_neighbors_tests! {
                    {
                        let graph : MyGraph<u32> = AdjGraph::new();
                        (graph, 0, 1, 2, 3)
                    }
                }
            }
        }
        mod undirected {
            mod edges {
                use ::ductgraph::map::{
                    HashMap,
                    BTreeMap,
                };
                use ::ductgraph::graph::*;
                use ::ductgraph::graph::adjlist::{
                    AdjGraph,
                    AdjVertex,
                };
                use ::ductgraph::uuid::{
                    UnitFactory,
                };

                adjgraph_type!(MyGraph, Concrete, Undirected, usize, HashMap, BTreeMap);

                undirected_edges_tests! {
                    {
                        let graph : MyGraph<u32> = AdjGraph::new();
                        (graph, 0 , 1)
                    }
                }
                directed_neighbors_tests! {
                    {
                        let graph : MyGraph<u32> = AdjGraph::new();
                        (graph, 0, 1, 2, 3)
                    }
                }
            }
        }

    }
}


mod edgemap {
    mod abstr {
        mod directed {
            mod edges {
                use ::ductgraph::map::{
                    HashMap,
                    BTreeMap,
                };
                use ::ductgraph::graph::*;
                use ::ductgraph::graph::edgemap::{
                    MapGraph,
                    EdgeTuple,
                };
                use ::ductgraph::uuid::{
                    UUIDFactory,
                    Factory,
                };

                mapgraph_type!(MyGraph, Abstract, Directed, Factory<usize>, Stable, BTreeMap, HashMap);

                directed_edges_tests! {
                    { let mut graph : MyGraph<u32, u32> = MapGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        (graph, 0 , 1)
                    }
                }
                directed_neighbors_tests! {
                    {
                        let mut graph : MyGraph<u32, u32> = MapGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        assert_eq!(graph.create(3), Ok((2, None)));
                        assert_eq!(graph.create(4), Ok((3, None)));
                        (graph, 0, 1, 2, 3)
                    }
                }
            }
        }
        mod undirected {
            mod edges {
                use ::ductgraph::map::{
                    HashMap,
                    BTreeMap,
                };
                use ::ductgraph::graph::*;
                use ::ductgraph::graph::edgemap::{
                    MapGraph,
                    EdgeTuple,
                };
                use ::ductgraph::uuid::{
                    UUIDFactory,
                    Factory,
                };

                mapgraph_type!(MyGraph, Abstract, Undirected, Factory<usize>, Stable, BTreeMap, HashMap);

                undirected_edges_tests! {
                    { 
                        let mut graph : MyGraph<u32, u32> = MapGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        (graph, 0 , 1)
                    }
                }
                directed_neighbors_tests! {
                    {
                        let mut graph : MyGraph<u32, u32> = MapGraph::new();
                        assert_eq!(graph.create(1), Ok((0, None)));
                        assert_eq!(graph.create(2), Ok((1, None)));
                        assert_eq!(graph.create(3), Ok((2, None)));
                        assert_eq!(graph.create(4), Ok((3, None)));
                        (graph, 0, 1, 2, 3)
                    }
                }
            }
        }
    }
    mod concrete {
        mod directed {
            use ::ductgraph::map::{
                BTreeMap,
            };
            use ::ductgraph::graph::*;
            use ::ductgraph::graph::edgemap::{
                MapGraph,
                EdgeTuple,
            };
            use ::ductgraph::uuid::{
                UnitFactory,
            };

            mapgraph_type!(MyGraph, Concrete, Directed, usize, BTreeMap);

            directed_edges_tests! {
                {
                    let graph : MyGraph<u32> = MapGraph::new();
                    (graph, 0 , 1)
                }
            }
            directed_neighbors_tests! {
                {
                    let graph : MyGraph<u32> = MapGraph::new();
                    (graph, 0, 1, 2, 3)
                }
            }
        }
    }
    mod undirected {
        mod edges {
            use ::ductgraph::map::{
                BTreeMap,
            };
            use ::ductgraph::graph::*;
            use ::ductgraph::graph::edgemap::{
                MapGraph,
                EdgeTuple,
            };
            use ::ductgraph::uuid::{
                UnitFactory,
            };

            mapgraph_type!(MyGraph, Concrete, Undirected, usize, BTreeMap);

            undirected_edges_tests! {
                {
                    let graph : MyGraph<u32> = MapGraph::new();
                    (graph, 0 , 1)
                }
            }
            directed_neighbors_tests! {
                {
                    let graph : MyGraph<u32> = MapGraph::new();
                    (graph, 0, 1, 2, 3)
                }
            }
        }
    }
}
