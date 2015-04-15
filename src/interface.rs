//! Collection of traits providing operations on a generic graph data structure

use std::ops::{
    Deref,
    DerefMut,
};
use std::hash::Hash;

/// Marker for undirected edges : an edge between A and B is the same as an edge between B and A.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Undirected;
/// Marker for directed edges : an edge from A to B is not the same as an edge from B to A.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Directed;

/// Marker trait implemented on both edge direction markers : `Directed` and `Undirected`
pub trait Direction : 'static + PartialEq + Eq + PartialOrd + Hash + Ord {}

impl Direction for Undirected {}
impl Direction for Directed {}


/// Constructs an iterator on the edges starting from a vertex.
pub trait Edges<'a> {
    /// The type of edges label
    type Label;
    /// The type of vertex references
    type V;
    type VRef : Deref<Target = Self::V> + 'a;

    /// The type of the iterator on the edges starting from the vertex
    type Edges : Iterator<Item = (Self::VRef, &'a Self::Label)>;

    /// Returns an iterator which yields a tuple `(Self::VRef, &'a Self::Label)` for each edge
    /// starting from this vertex. The first element of the tuple is a reference to the destination
    /// vertex, and the second element of the tuple is a reference to the label of the edge.
    fn edges(self) -> Self::Edges;
}

/// Constructs an iterator on the edges starting from a vertex, yielding mutable references to the
/// edge label.
pub trait EdgesMut<'a> {
    /// The type of edges label
    type Label;
    /// The type of vertex references
    type V;
    type VRef : Deref<Target = Self::V> + 'a;

    /// The type of the iterator on the edges starting from the vertex
    type Edges : Iterator<Item = (Self::VRef, &'a mut Self::Label)>;

    /// Returns an iterator which yields a tuple `(Self::VRef, &'a mut Self::Label)` for each edge
    /// starting from this vertex. The first element of the tuple is a reference to the destination
    /// vertex, and the second element of the tuple is a mutable reference to  the label of the edge.
    fn edges_mut(self) -> Self::Edges;
}

/// Immutable operations on any edge starting from a vertex.
pub trait Edge<'a, Q> : Sized {
    type V;
    /// The type of the label of the edges.
    type Label;
    /// Potentially a RAII guard which deref to a reference to a label.
    type LabelGuard : Deref<Target = Self::Label> + 'a;

    /// Returns a reference to the label of the edge starting from the current vertex to the
    /// given destination vertex.
    ///
    /// If the edge doesn't exists, `None` is returned.
    fn edge_label(self, to : &'a Q) -> Option<Self::LabelGuard>;

    /// Returns true if the graph contains an edge from the current vertex to the specified
    /// destination.
    fn contains(self, to : &'a Q) -> bool { self.edge_label(to).is_some() }
}

/// Mutable operations on any edge starting from a vertex.
pub trait EdgeMut<'a, Q> {
    type V;
    /// The type of the label of the edges.
    type Label;
    /// Potentially a RAII guard which deref to a reference to a label.
    type LabelGuard : DerefMut<Target = Self::Label> + 'a;

    /// Returns a mutable reference to the label of the edge starting from the current vertex to
    /// the given destination vertex.
    fn edge_label_mut(self, to : &'a Q) -> Option<Self::LabelGuard>;
}

/// Unstable operations on any edge starting from a vertex.
pub trait EdgeEntry<'a, Q> {
    type V;
    /// The type of the label of the edges.
    type Label;

    /// Adds an edge from the current vertex to the specified destination vertex with the given
    /// label.
    ///
    /// If an edge between those two vertices already exists in the graph, the label is updated and
    /// the old label is returned. Else, `None` is returned.
    fn link(self, label : Self::Label, to : &'a Q) -> Option<Self::Label>;

    /// Removes an edge from the current vertex to the specified destination vertex.
    /// 
    /// If no such edge exists, the method does nothing and returns `None`. If an edge exists,
    /// it is removed and its label is returned.
    fn unlink(self, to : &'a Q) -> Option<Self::Label>;
}

/// A Graph structure with immutable operations on the vertices. 
pub trait GraphLike<'a, V> {
    /// The type of a vertex 
    type Vertex;

    /// Retrieves an immutable vertex entry in the graph from a vertex reference.
    /// 
    /// Their is no notion of existence, because this interface is intended to be used both with
    /// abstract and concrete graphs. 
    fn vertex(&'a self, v : &'a V) -> Self::Vertex;
}

/// Immutable operations on graphs.
pub trait ConcreteGraphLike<V> {
    /// Returns `true` if the graph contains the specified vertex
    /// 
    /// For concrete graphs, this method will always return `true`
    fn contains(&self, v : &V) -> bool;
}


/// A Graph structure with mutable operations on the vertices.
pub trait GraphLikeMut<'a, V> {
    /// The type of a mutable vertex.
    type VertexMut;

    /// Retrieves a mutable vertex entry in the graph from a vertex reference.
    ///
    /// Their is no notion of existence, because this interface is intended to be used both with
    /// abstract and concrete graphs. 
    fn vertex_mut(&'a mut self, v : &'a V) -> Self::VertexMut;
}


/// A Graph structure with unstable structural operations on the vertices.
pub trait GraphLikeEntry<'a, V> {
    /// The type of an unstable vertex.
    type VertexEntry;

    /// Retrieves an unstable vertex entry in the graph from a vertex reference.
    ///
    /// Unstable operations are operation which modify the structure of the graph, for instance
    /// adding or removing an edge, removing a vertex, ...
    ///
    /// Their is no notion of existence, because this interface is intended to be used both with
    /// abstract and concrete graphs. 
    fn vertex_entry(&'a mut self, v : &'a V) -> Self::VertexEntry;
}

/// Immutable operations on a vertex entry.
pub trait Vertex : Sized {
    /// The type of a vertex label.
    type Label;
    /// Potentially a RAII guard dereferencing to a vertex label reference.
    type LabelGuard : Deref<Target = Self::Label>;

    /// Returns a reference to the label of the vertex if the vertex entry exists.
    fn label(self) -> Option<Self::LabelGuard>;

    /// Returns true if the vertex exists.
    fn exists(self) -> bool { self.label().is_some() }
}

/// Mutable operations on a vertex entry.
pub trait VertexMut {
    /// The type of a vertex label.
    type Label;
    /// Potentially a RAII guard dereferencing to a vertex label reference.
    type LabelGuard : DerefMut<Target = Self::Label>;

    /// Returns a mutable reference to the label of the vertex if the vertex entry exists.
    fn label_mut(self) -> Option<Self::LabelGuard>;
}

/// Unstable operations on a vertex entry.
pub trait VertexEntry {
    /// The type of a vertex label.
    type Label;

    /// Removes the vertex from the graph.
    ///
    /// If the vertex entry exists in the graph, the vertex is removed, every edge starting from
    /// and pointing to this vertex are removed, and the vertex label is returned.
    ///
    /// If the vertex entry doesn't exist, `None` is returned.
    fn remove(self) -> Option<Self::Label>;
}

/// Vertex construction operation.
pub trait VertexCreate {
    /// The type of a vertex label.
    type Label;
    /// The type of a vertex reference.
    type V;

    /// Add a new vertex in the graph with the specified label.
    ///
    /// If the addition succeeded, a reference to the newly added vertex is returned.
    /// If this interface is used on a vertex entry, a vertex with this reference might already
    /// exists in the graph. In this case, the label is updated and the old label is returned.
    ///
    /// If the addition fails, the given label is returned back.
    fn create(self, label : Self::Label) ->  Result<(Self::V, Option<Self::Label>), Self::Label>;
}

/// A graph tructure.
pub trait Graph {
    type ELabel;
    type Label;
    type V;
}
