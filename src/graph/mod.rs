//! Various implementations of a graph datastructure
#[macro_use] pub mod edgemap;
#[macro_use] pub mod adjlist;
//pub mod rcnodes;

/// A marker for stable graphs
///
/// A stable graph is a kind of abstract graph allowing insertion of new vertices but not deletion
/// of existing vertices.
pub struct Stable;

/// A marker for unstable graphs
///
/// An unstable graph is a kind of abstract graph allowing both insertion and deletion of vertices.
pub struct Unstable;

/// A marker for concrete graphs
///
/// A concrete graph is a kind of graph with no notion of vertex set. Each element of the type of
/// vertices is an existing vertex in the graph. No vertex creation or deletion ever happens.
pub struct Concrete;

/// A marker for abstract graphs
///
/// An abstract graph is a kind of graph where vertices have to be created and deleted explicitly.
/// Initially, an abstract graph has no vertex and before adding edges to the graph, vertices have
/// to be created.
pub struct Abstract;
