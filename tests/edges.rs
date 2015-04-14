
#[macro_export] macro_rules! directed_edges_tests {
    { $setup:block } =>
    {
        use ::ductgraph::interface::*;

        #[test]
        fn non_existant_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
        }

        #[test]
        fn link_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&42));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 42));
            assert_eq!(graph.vertex_entry(&v1).link(43, &v2), Some(42));
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&43));
        }

        #[test]
        fn unlink_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_entry(&v1).unlink(&v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&42));
            assert_eq!(graph.vertex_entry(&v1).unlink(&v2), Some(42));
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
        }

        #[test]
        fn link_self() {
            let (mut graph, v1, _) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v1), None);
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&42));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v1), Some(&mut 42));
            assert_eq!(graph.vertex_entry(&v1).link(43, &v1), Some(42));
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&43));
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
        }

        #[test]
        fn unlink_self() {
            let (mut graph, v1, _) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).unlink(&v1), None);
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v1), None);
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&42));
            assert_eq!(graph.vertex_entry(&v1).unlink(&v1), Some(42));
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
        }

        #[test]
        fn label_mut() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            {
                let mut vertex = graph.vertex_mut(&v1);
                let label_ref = vertex.edge_label_mut(&v2);
                assert!(label_ref.is_some());
                *label_ref.unwrap() = 43;
            }
            assert_eq!(graph.vertex(&v1).contains(&v2), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&43));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 43));
        }

        #[test]
        fn label_mut_self() {
            let (mut graph, v1, _) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v1), None);
            {
                let mut vertex = graph.vertex_mut(&v1);
                let label_ref = vertex.edge_label_mut(&v1);
                assert!(label_ref.is_some());
                *label_ref.unwrap() = 43;
            }
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&43));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v1), Some(&mut 43));
        }
    }
}

#[macro_export] macro_rules! undirected_edges_tests {
    { $setup:block } =>
    {
        use ::ductgraph::interface::*;

        #[test]
        fn non_existant_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);
            assert_eq!(graph.vertex_mut(&v2).edge_label_mut(&v1), None);
            assert_eq!(graph.vertex(&v2).contains(&v1), false);
        }

        #[test]
        fn link_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex(&v2).contains(&v1), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&42));
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&42));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 42));
            assert_eq!(graph.vertex_mut(&v2).edge_label_mut(&v1), Some(&mut 42));
            assert_eq!(graph.vertex_entry(&v1).link(43, &v2), Some(42));
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&43));
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&43));
        }

        #[test]
        fn relink_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex(&v2).contains(&v1), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&42));
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&42));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 42));
            assert_eq!(graph.vertex_mut(&v2).edge_label_mut(&v1), Some(&mut 42));
            assert_eq!(graph.vertex_entry(&v2).link(43, &v1), Some(42));
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&43));
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&43));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 43));
            assert_eq!(graph.vertex_mut(&v2).edge_label_mut(&v1), Some(&mut 43));
        }

        #[test]
        fn unlink_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_entry(&v1).unlink(&v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);

            assert_eq!(graph.vertex(&v2).contains(&v1), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v2).unlink(&v1), None);
            assert_eq!(graph.vertex(&v2).contains(&v1), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);

            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&42));
            assert_eq!(graph.vertex(&v2).contains(&v1), true);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&42));

            assert_eq!(graph.vertex_entry(&v1).unlink(&v2), Some(42));
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex(&v2).contains(&v1), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);

            assert_eq!(graph.vertex_entry(&v1).unlink(&v2), None);
        }

        #[test]
        fn reunlink_edge() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            assert_eq!(graph.vertex(&v1).contains(&v2), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&42));
            assert_eq!(graph.vertex(&v2).contains(&v1), true);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&42));

            assert_eq!(graph.vertex_entry(&v2).unlink(&v1), Some(42));
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex(&v2).contains(&v1), false);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), None);

            assert_eq!(graph.vertex_entry(&v2).unlink(&v1), None);
        }

        #[test]
        fn link_self() {
            let (mut graph, v1, _) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v1), None);
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&42));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v1), Some(&mut 42));
            assert_eq!(graph.vertex_entry(&v1).link(43, &v1), Some(42));
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&43));
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
        }

        #[test]
        fn unlink_self() {
            let (mut graph, v1, _) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).unlink(&v1), None);
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v1), None);
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&42));
            assert_eq!(graph.vertex_entry(&v1).unlink(&v1), Some(42));
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
        }

        #[test]
        fn label_mut() {
            let (mut graph, v1, v2) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v2), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v2), None);
            {
                let mut vertex = graph.vertex_mut(&v1);
                let label_ref = vertex.edge_label_mut(&v2);
                assert!(label_ref.is_some());
                *label_ref.unwrap() = 43;
            }
            assert_eq!(graph.vertex(&v1).contains(&v2), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&43));
            assert_eq!(graph.vertex(&v2).contains(&v1), true);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&43));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 43));

            {
                let mut vertex = graph.vertex_mut(&v2);
                let label_ref = vertex.edge_label_mut(&v1);
                assert!(label_ref.is_some());
                *label_ref.unwrap() = 44;
            }

            assert_eq!(graph.vertex(&v1).contains(&v2), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v2), Some(&44));
            assert_eq!(graph.vertex(&v2).contains(&v1), true);
            assert_eq!(graph.vertex(&v2).edge_label(&v1), Some(&44));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v2), Some(&mut 44));
        }

        #[test]
        fn label_mut_self() {
            let (mut graph, v1, _) = $setup;
            assert_eq!(graph.vertex(&v1).contains(&v1), false);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), None);
            assert_eq!(graph.vertex_entry(&v1).link(42, &v1), None);
            {
                let mut vertex = graph.vertex_mut(&v1);
                let label_ref = vertex.edge_label_mut(&v1);
                assert!(label_ref.is_some());
                *label_ref.unwrap() = 43;
            }
            assert_eq!(graph.vertex(&v1).contains(&v1), true);
            assert_eq!(graph.vertex(&v1).edge_label(&v1), Some(&43));
            assert_eq!(graph.vertex_mut(&v1).edge_label_mut(&v1), Some(&mut 43));
        }
    }
}

#[macro_export] macro_rules! directed_neighbors_tests {
    { $setup:block } =>
    {

        #[test]
        fn edges() {
            let (mut graph, v1, v2, v3, v4) = $setup;
            static ONE : u32 = 1;
            static TWO : u32 = 2;
            static THREE : u32 = 3;

            graph.vertex_entry(&v1).link(1, &v2);
            graph.vertex_entry(&v1).link(2, &v3);
            graph.vertex_entry(&v1).link(3, &v4);

            let vertex  = graph.vertex(&v1);
            let edges = vertex.edges().collect::<Vec<_>>();
            assert_eq!(edges.len(), 3usize);
            assert!(edges[0] == (&v2, &ONE));
            assert!(edges[1] ==(&v3, &TWO));
            assert!(edges[2] == (&v4, &THREE));
        }

        #[test]
        fn edges_mut() {
            let (mut graph, v1, v2, v3, v4) = $setup;

            graph.vertex_entry(&v1).link(1, &v2);
            graph.vertex_entry(&v1).link(2, &v3);
            graph.vertex_entry(&v1).link(3, &v4);

            {
                let mut vertex  = graph.vertex_mut(&v1);
                for (_, l) in vertex.edges_mut() {
                    *l = 42;
                }
            }
            let vertex = graph.vertex(&v1);
            for (_, l) in vertex.edges() {
                assert!(l == &42);
            }
        }
    }
}
