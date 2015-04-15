use std::hash::{
    Hash,
    Hasher,
};
use std::cmp::{
    self,
    Ord,
    Eq,
    Ordering,
};
use std::iter::IntoIterator;
use std::marker::PhantomData;
use std::borrow::{
    Borrow,
    ToOwned
};

use ::map::{
    self,
    ToUsize,
    Map,
    FixedSizedMap,
    FixedMap,
    MapOwned,
    StableMap,
    IntoOrder,
    ExternalOrd,
    ExtOrdering,
};

use ::interface::*;

use ::uuid::{
    StableUUIDFactory,
    UUIDFactory,
    UnitFactory,
    InternalFactory,
    Factory,
};

use super::{
    Stable,
    Unstable,
    Abstract,
    Concrete
};

pub struct EdgeTuple<V, Dir>{
    from : V,
    to : V,
    marker : PhantomData<Dir>
}

trait IsConnected<To> {
    fn connected(&self, to : &To) -> bool;
}


impl<V : Ord> ExternalOrd<V> for EdgeTuple<V, Directed> {
    #[inline]
    fn ext_cmp(&self, to : &V) -> ExtOrdering<&V> {
        match self.from.cmp(to) {
            Ordering::Equal => ExtOrdering::Equal(&self.to),
            Ordering::Less => ExtOrdering::Less,
            Ordering::Greater => ExtOrdering::Greater,
        }
    }
}

impl<V : Ord> ExternalOrd<V> for EdgeTuple<V, Undirected> {
    #[inline]
    fn ext_cmp(&self, to : &V) -> ExtOrdering<&V> {
        use std::cmp::Ordering::*;
        match (self.from.cmp(to), self.to.cmp(to)) {
            (Equal, _) => ExtOrdering::Equal(&self.to),
            (_, Equal) => ExtOrdering::Equal(&self.from),
            (Less, Less) => ExtOrdering::Less,
            (Greater, Greater) => ExtOrdering::Greater,
            _ => ExtOrdering::Partial,
        }
    }
}

trait Index {
    fn index(&self, base : usize) -> Option<usize>;
}

impl<T> Index for EdgeTuple<T, Directed> where T : ToUsize {
    fn index(&self, base : usize) -> Option<usize> {
        let (from, to) = (self.from.to_usize(), self.to.to_usize());

        if from > base || to > base {
            None
        } else {
            Some(from * base + to)
        }
    }
}

impl<T> Index for EdgeTuple<T, Undirected> where T : ToUsize {
    fn index(&self, base : usize) -> Option<usize> {
        let (from, to) = (self.from.to_usize(), self.to.to_usize());
        let (max, min) = (cmp::max(from, to), cmp::min(from, to));
        if max > base {
            None
        } else {
            Some(max * (max - 1) / 2 + min)
        }
    }
}

impl<V, Q> IsConnected<Q> for EdgeTuple<V, Directed> where V : Borrow<Q> + Eq, Q : Eq {
    fn connected(&self, to : &Q) -> bool {
        self.from.borrow() == to
    }
}

impl<V, Q> IsConnected<Q> for EdgeTuple<V, Undirected> where V : Borrow<Q> + Eq, Q : Eq {
    fn connected(&self, to : &Q) -> bool {
        self.from.borrow() == to || self.to.borrow() == to
    }
}

impl<V : Eq + Ord> Borrow<V> for EdgeTuple<V, Directed> {
    #[inline]
    fn borrow(&self) -> &V {
        &self.from
    }
}

impl<V : Eq + Ord> Borrow<V> for EdgeTuple<V, Undirected> {
    #[inline]
    fn borrow(&self) -> &V {
        cmp::max(&self.from, &self.to)
    }
}

impl<V : Clone, Dir> Clone for EdgeTuple<V, Dir> {
    fn clone(&self) -> EdgeTuple<V, Dir> {
        EdgeTuple { from : self.from.clone(), to : self.to.clone(), marker : PhantomData }
    }
}

impl<V, Dir> EdgeTuple<V, Dir> {
    fn new(from : V, to : V) -> EdgeTuple<V, Dir> {
        EdgeTuple { from : from, to : to, marker : PhantomData }
    }
}

impl<V : PartialEq> PartialEq for EdgeTuple<V, Directed> {
    fn eq(&self, rhs : &EdgeTuple<V, Directed>) -> bool {
        self.from == rhs.from && self.to == rhs.to
    }
}

impl<V : PartialEq> PartialEq for EdgeTuple<V, Undirected> {
    fn eq(&self, rhs : &EdgeTuple<V, Undirected>) -> bool {
        (self.from == rhs.from && self.to == rhs.to)
            || (self.from == rhs.to && self.to == rhs.from)
    }
}

impl<V : Eq> Eq for EdgeTuple<V, Undirected> {}
impl<V : Eq> Eq for EdgeTuple<V, Directed> {}

impl<V : PartialOrd> PartialOrd for EdgeTuple<V, Directed> {
    #[inline]
    fn partial_cmp(&self, rhs : &EdgeTuple<V, Directed>) -> Option<Ordering> {
                (&self.from, &self.to).partial_cmp(&(&rhs.from, &rhs.to))
    }
}

impl<V : Ord> Ord for EdgeTuple<V, Directed> {
    fn cmp(&self, rhs : &EdgeTuple<V, Directed>) -> Ordering {
        (&self.from, &self.to).cmp(&(&rhs.from, &rhs.to))
    }
}

impl<V : PartialOrd> PartialOrd for EdgeTuple<V, Undirected> {
    fn partial_cmp(&self, rhs : &EdgeTuple<V, Undirected>) -> Option<Ordering> {
            {
                let tuple2 = match rhs.from.partial_cmp(&rhs.to) {
                    None => return None,
                    Some(Ordering::Less) => (&rhs.to, &rhs.from),
                    _ => (&rhs.from, &rhs.to),
                };
                let tuple1 = match self.from.partial_cmp(&self.to) {
                    None => return None,
                    Some(Ordering::Less) => (&self.to, &self.from),
                    _ => (&self.from, &self.to),
                };
                tuple1.partial_cmp(&tuple2)
        }
    }
}

impl<V : Ord> Ord for EdgeTuple<V, Undirected> {
    fn cmp(&self, rhs : &EdgeTuple<V, Undirected>) -> Ordering {
                let t2 = (cmp::max(&rhs.from, &rhs.to), cmp::min(&rhs.from, &rhs.to));
                let t1 = (cmp::max(&self.from, &self.to), cmp::min(&self.from, &self.to));
                t1.cmp(&t2)
    }
}

impl<V : Hash> Hash for EdgeTuple<V, Directed> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.from.hash(state);
        self.to.hash(state);
    }
}

impl<V : Hash + Ord> Hash for EdgeTuple<V, Undirected> {
    fn hash<H>(&self, state : &mut H) where H: Hasher {
        let tuple = (cmp::max(&self.from, &self.to), cmp::min(&self.from, &self.to));
        tuple.hash(state);
    }
}

pub struct MapGraph<U, Map, VMap, S, A, D> {
    map : Map,
    vmap : VMap,
    uuids : U,
    marker : PhantomData<(S, A, D)>,
}

/// Declare the type of a MapGraph
///
/// # Overview
///
/// In a MapGraph, edges are kept in a single Map. The keys are tuples (see `EdgeTuple`) of vertex handles and the
/// value the label of the edges. For `Abstract` graphs, the mapping between vertex handles and
/// vertex labels is kept in a separate Map.
///
/// # Macro invocation
///
/// This macro declares a type for a particular MapGraph implementation.
///
/// ## Generic parameters 
///
/// - The first argument, `$name`, is the name of the declared type.
/// - The second argument is either `Concrete`, to declare a concrete graph, or `Abstract`, to
/// declare an abstract graph.
/// - The third argument is either `Directed` or `Undirected`, 
///
/// ## Concrete graphs 
///
/// For concrete graphs (the second parameter is `Concrete`), the macro takes two other parameters
/// :
///
/// - `$vertex`, the type of vertex handles,
/// - and `$map`, the type of the edges map.
///
/// ### Example
///
/// ```
/// # #[macro_use] extern crate ductgraph;
/// # fn main() {
/// use ductgraph::map::BTreeMap;
/// use ductgraph::interface::Directed;
/// use ductgraph::uuid::UnitFactory;
/// use ductgraph::graph::{Unstable, Concrete};
/// use ductgraph::graph::edgemap::{MapGraph, EdgeTuple};
///
/// mapgraph_type! { MyGraph, Concrete, Directed, usize, BTreeMap };
///
/// let graph : MyGraph<u32> = MapGraph::new();
///
/// # }
/// ```
///
/// This will declare a `type MyGraph<Label>` with `Directed` edges kept in a `BTreeMap` and using
/// `usize` as vertex handles, and create a new graph of this type.
///
/// The `Label` parameter of the new type is the type of edge labels.
///
/// ## Abstract graphs
///
/// For abstract graphs (the second parameter is `Abstract`, the macro takes five more parameters
/// :
///
/// - `$factory`, the handles factory, one of :
///     - `Factory<F>`, where `F` is any type implementing `UUIDFactory`. In this case, handles are
///     internally created.
///     - `UnitFactory`. In this case, you have to pass the handle and the label at the vertex
///     creation.
///     - `InternalFactory`. This is a particular kind of dummy factory to be used with the a
///     vertex map which implements `InternalStableMap` instead of `Map`. In this case, the handles
///     are internally created but you cannot control their generation.
/// - `$stable`, the stability of the graph, either :
///     - `Stable`, vertices can be added to the graph but not removed.
///     - `Unstable`, vertices can be added and removed to the graph. This is option is not avaible
///     when the factory parameter is `InternalFactory`.
/// - `$map`, the type of map used to keep the edges.
/// - `$vmap`, the type of map used to keep vertices.
/// -  If the `$factory` parameter is not `Factory<F>`, the macro invocation takes one more
/// paramater, `$vertex`, which is the type of the vertex handles to use.
///
/// ## Example 1
///
/// Declare an `Abstract` graph, with `Undirected` edges, which will autogenerate the handles using
/// an `usize` counter and store the edges in a `BTreeMap` and the vertices in an `HashMap` :
///
///
/// ```
/// # #[macro_use] extern crate ductgraph;
/// # fn main() {
/// use ductgraph::map::{BTreeMap, HashMap};
/// use ductgraph::uuid::{UUIDFactory, Factory};
/// use ductgraph::interface::Undirected;
/// use ductgraph::graph::edgemap::{MapGraph, EdgeTuple};
/// use ductgraph::graph::{Stable, Abstract};
///
/// mapgraph_type!{ MyGraph, Abstract, Undirected, Factory<usize>, Stable, BTreeMap, HashMap};
///
/// let graph : MyGraph<String, u32>  = MapGraph::new();
/// # }
/// ```
/// The last statement will create a new graph of this kind with vertices labeled with a `String`
/// and edges labeled with an `u32`.
///
/// ## Example 2
///
/// Declare an `Abstract` graph with `Directed` edges kept in a `BTreeMap` and vertices with
/// `String` handles :
///
/// ```
/// # #[macro_use] extern crate ductgraph;
/// # fn main() {
/// use ductgraph::map::{BTreeMap, HashMap};
/// use ductgraph::uuid::UnitFactory;
/// use ductgraph::graph::edgemap::{MapGraph, EdgeTuple};
/// use ductgraph::interface::Directed;
/// use ductgraph::graph::{Unstable, Abstract};
///
/// mapgraph_type!{ MyGraph, Abstract, Directed, UnitFactory, Unstable, BTreeMap, HashMap, String };
///
/// let graph : MyGraph<u8, u8> = MapGraph::new();
/// # }
/// ```
///
/// ## Example 3
///
/// Declare an `Abstract` graph with `Directed` edges kept in a adjacency matrix with internal
/// handles for vertex kept in a vector :
///
/// ```
/// # #[macro_use] extern crate ductgraph;
/// # fn main() {
/// use ductgraph::map::StableVecMap;
/// use ductgraph::uuid::InternalFactory;
/// use ductgraph::graph::edgemap::{MapGraph, EdgeTuple};
/// use ductgraph::interface::Directed;
/// use ductgraph::graph::{Stable, Abstract};
/// use ductgraph::graph::edgemap::edgematrix::VecMatrix;
///
/// mapgraph_type!{ MyGraph, Abstract, Directed, InternalFactory, Stable, VecMatrix, StableVecMap,
/// u32};
///
/// # }
/// ```
#[macro_export]
macro_rules! mapgraph_type {
    ($name:ident, Concrete, Directed, $vertex:ident, $map:ident) =>
        (type $name<Label> = 
            MapGraph<UnitFactory, $map<EdgeTuple<$vertex, Directed>, Label>, (), Unstable, Concrete, Directed>;);

    ($name:ident, Concrete, Undirected, $vertex:ident, $map:ident) =>
        (type $name<Label> = 
         MapGraph<UnitFactory, $map<EdgeTuple<$vertex, Undirected>, Label>, (), Unstable, Concrete, Undirected>;); 

    ($name:ident, Abstract, Directed, UnitFactory, $stable:ident, $map:ident, $vmap:ident, $vertex:ty) =>
        (type $name<Label, ELabel> =
         MapGraph<UnitFactory, $map<EdgeTuple<$vertex, Directed>, ELabel>, $vmap<$vertex, Label>, $stable, Abstract, Directed>;);

    ($name:ident, Abstract, Directed, Factory<$uuid:ty>, $stable:ident, $map:ident, $vmap:ident) =>
        (type $name<Label, ELabel> =
         MapGraph<Factory<$uuid>, $map<EdgeTuple<<$uuid as UUIDFactory>::UUID, Directed>, ELabel>, $vmap<<$uuid as UUIDFactory>::UUID,Label>, $stable, Abstract, Directed>;);

    ($name:ident, Abstract, Undirected, UnitFactory, $stable:ident, $map:ident, $vmap:ident, $vertex:ty) =>
        (type $name<Label, ELabel> 
         MapGraph<UnitFactory, $map<EdgeTuple<$vertex, Undirected>, ELabel>, $vmap<$vertex, Label>, $stable, Abstract, Undirected>;);

    ($name:ident, Abstract, Undirected, Factory<$uuid:ty>, $stable:ident, $map:ident, $vmap:ident) =>
        (type $name<Label, ELabel> =
         MapGraph<Factory<$uuid>, $map<EdgeTuple<<$uuid as UUIDFactory>::UUID, Undirected>, ELabel>, $vmap<<$uuid as UUIDFactory>::UUID, Label>, $stable, Abstract, Undirected>;);

    ($name:ident, Abstract, Directed, InternalFactory, Stable, VecMatrix, $vmap:ident, $vertex:ty) =>
        (type $name<Label, ELabel> =
         MapGraph<InternalFactory, VecMatrix<$vertex, ELabel, Directed>, $vmap<$vertex, Label>, Stable, Abstract, Directed>;);
    ($name:ident, Abstract, Undirected, InternalFactory, Stable, VecMatrix, $vmap:ident, $vertex:ty) =>
        (type $name<Label, ELabel> 
         MapGraph<InternalFactory, VecMatrix<$vertex, ELabel, Undirected>, $vmap<$vertex, Label>, Stable, Abstract, Undirected>;);
}


use std::default::Default;

impl<U : Default, Map : Default, VMap : Default, S, A, D> MapGraph<U, Map, VMap, S, A, D> {
    pub fn new() -> MapGraph<U, Map, VMap, S, A, D> {
        MapGraph {
            map : Map::default(),
            vmap : VMap::default(),
            uuids : U::default(),
            marker : PhantomData,
        }
    }
}

pub struct MapGraphEntry<Vert, Map, S, A, D, Q> {
    q : Q,
    map : Map,
    vertex : Vert,
    marker : PhantomData<(S, A, D)>,
}

impl<Vert, Map, S, A, D, Q> MapGraphEntry<Vert, Map, S, A, D, Q> {
    #[inline]
    fn new(q : Q, v : Vert, m : Map) -> MapGraphEntry<Vert, Map, S, A, D, Q> {
        MapGraphEntry { 
            q : q,
            map : m,
            vertex : v,
            marker : PhantomData,
        }
    }
}

impl<'a, U, Label, Map, VMap, V, Q, S, Dir> GraphLike<'a, Q>
for MapGraph<U, Map, VMap, S, Concrete, Dir>
where 
Map : map::MapOwned<Key = EdgeTuple<V, Dir>, Value = Label>,
Dir : Direction,
V : Borrow<Q>,
{
    type Vertex = MapGraphEntry<(), &'a Map, S, Concrete, Dir, &'a Q>;

    #[inline]
    fn vertex(&'a self, from : &'a Q) ->MapGraphEntry<(), &'a Map, S, Concrete, Dir, &'a Q> {
        MapGraphEntry::new(from, (), &self.map)
    }
}

impl<'a, U, Label, Map, VMap, V, Q, VLabel, S, Dir> GraphLike<'a, Q>
for MapGraph<U, Map, VMap, S, Abstract, Dir>
where 
Map : map::MapOwned<Key = EdgeTuple<V, Dir>, Value = Label>,
VMap : map::FixedMap<Q, Key = V, Value = VLabel>,
Dir : Direction,
V : Borrow<Q>,
{
    type Vertex = MapGraphEntry<Option<&'a VLabel>, &'a Map, S, Abstract, Dir, &'a Q>;

    #[inline]
    fn vertex(&'a self, from : &'a Q) -> MapGraphEntry<Option<&'a VLabel>, &'a Map, S, Abstract, Dir, &'a Q> {
        MapGraphEntry::new(from, self.vmap.get(from), &self.map)
    }
}

impl<'a, U, Label, Map, VMap, V, Q, S, Dir> GraphLikeMut<'a, Q>
for MapGraph<U, Map, VMap, S, Concrete, Dir>
where 
Map : map::MapOwned<Key = EdgeTuple<V, Dir>, Value = Label>,
Dir : Direction,
V : Borrow<Q>,
{
    type VertexMut = MapGraphEntry<(), &'a mut Map, S, Concrete, Dir, &'a Q>;

    #[inline]
    fn vertex_mut(&'a mut self, from : &'a Q) ->MapGraphEntry<(), &'a mut Map, S, Concrete, Dir, &'a Q> {
        MapGraphEntry::new(from, (), &mut self.map)
    }
}

impl<'a, U, Label, Map, VMap, V, Q, VLabel, S, Dir> GraphLikeMut<'a, Q>
for MapGraph<U, Map, VMap, S, Abstract, Dir>
where 
Map : map::MapOwned<Key = EdgeTuple<V, Dir>, Value = Label>,
VMap : map::FixedMap<Q, Key = V, Value = VLabel>,
Dir : Direction,
V : Borrow<Q>,
{
    type VertexMut = MapGraphEntry<Option<&'a mut VLabel>, &'a mut Map, S, Abstract, Dir, &'a Q>;

    #[inline]
    fn vertex_mut(&'a mut self, from : &'a Q) -> MapGraphEntry<Option<&'a mut VLabel>, &'a mut Map, S, Abstract, Dir, &'a Q> {
        MapGraphEntry::new(from, self.vmap.get_mut(from), &mut self.map)
    }
} 

impl<'a, U, Map, VMap, V, Q, S, A, Dir> GraphLikeEntry<'a, Q>
for MapGraph<U, Map, VMap, S, A, Dir>
where 
Map : map::MapOwned<Key = EdgeTuple<V, Dir>>,
Dir : Direction,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type VertexEntry = (&'a mut MapGraph<U, Map, VMap, S, A, Dir>, &'a Q);

    #[inline]
    fn vertex_entry(&'a mut self, from : &'a Q) ->(&'a mut MapGraph<U, Map, VMap, S, A, Dir>, &'a Q) {
        (self, from)
    } 
} 

impl<'b, 'a : 'b, VLabel, Map, Q, S, Dir> Vertex
for &'b MapGraphEntry<Option<&'a VLabel>, &'a Map, S, Abstract, Dir, &'a Q>
where
Dir : Direction,
{
    type Label = VLabel;
    type LabelGuard = &'b VLabel;

    fn label(self) -> Option<&'b VLabel> {
        self.vertex
    }
}

impl<'b, 'a : 'b, VLabel, Map, Q, S, Dir> Vertex
for &'b MapGraphEntry<Option<&'a mut VLabel>, &'a mut Map, S, Abstract, Dir, &'a Q>
where
Dir : Direction,
{
    type Label = VLabel;
    type LabelGuard = &'b VLabel;

    fn label(self) -> Option<&'b VLabel> {
        self.vertex.as_ref().map(|l| &**l)
    }
}

impl<'b, 'a : 'b, VLabel, Map, Q, S, Dir> VertexMut
for &'b mut MapGraphEntry<Option<&'a mut VLabel>, &'a mut Map, S, Abstract, Dir, &'a Q>
where
Dir : Direction,
{
    type Label = VLabel;
    type LabelGuard = &'b mut VLabel;

    fn label_mut(self) -> Option<&'b mut VLabel> {
        self.vertex.as_mut().map(|l| &mut**l)
    }
}

impl<'c, 'b : 'c, 'a : 'b, VEnt, Label : 'b, Map, Q1, Q2, S, A, Dir, V> Edge<'c, Q2>
for &'b MapGraphEntry<VEnt, &'a Map, S, A, Dir, &'a Q1>
where
Dir : Direction,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q1 : ToOwned<Owned = V>,
Q2 : ToOwned<Owned = V>,
Map : map::Map<EdgeTuple<V, Dir>, Value = Label, Key = EdgeTuple<V, Dir>>,
{
    type V = V;
    type Label = Label;
    type LabelGuard = &'b Label;

    fn edge_label(self, to : &'c  Q2) -> Option<&'b Label> {
        self.map.get(&EdgeTuple::new(self.q.to_owned(), to.to_owned()))
    }
}

impl<'c, 'b : 'c, 'a : 'b, VEnt, Label : 'b, Map, Q1, Q2, S, A, Dir, V> Edge<'c, Q2>
for &'b MapGraphEntry<VEnt, &'a mut Map, S, A, Dir, &'a Q1>
where
Dir : Direction,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q1 : ToOwned<Owned = V>,
Q2 : ToOwned<Owned = V>,
Map : map::FixedMap<EdgeTuple<V, Dir>, Value = Label, Key = EdgeTuple<V, Dir>>,
{
    type V = V;
    type Label = Label;
    type LabelGuard = &'b Label;

    fn edge_label(self, to : &'c Q2) -> Option<&'b Label> {
        self.map.get(&EdgeTuple::new(self.q.to_owned(), to.to_owned()))
    }
}

impl<'c, 'b : 'c, 'a : 'b, VEnt, Label : 'b, Map, Q1, Q2, S, A, Dir, V> EdgeMut<'c, Q2>
for &'b mut MapGraphEntry<VEnt, &'a mut Map, S, A, Dir, &'a Q1>
where
Dir : Direction,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q1 : ToOwned<Owned = V>,
Q2 : ToOwned<Owned = V>,
Map : map::FixedMap<EdgeTuple<V, Dir>, Value = Label, Key = EdgeTuple<V, Dir>>,
{
    type V = V;
    type Label = Label;
    type LabelGuard = &'b mut Label;

    fn edge_label_mut(self, to : &'c Q2) -> Option<&'b mut Label> {
        self.map.get_mut(&EdgeTuple::new(self.q.to_owned(), to.to_owned()))
    }
}
impl<'a, 'b, U, Label, Map, VMap, Q1, Q2, S, Dir, V> EdgeEntry<'b, Q2>
for (&'a mut MapGraph<U, Map, VMap, S, Concrete, Dir>, &'a Q1)
where
Dir : Direction,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q1 : ToOwned<Owned = V>,
Q2 : ToOwned<Owned = V>,
Map : map::Map<EdgeTuple<V, Dir>, Value = Label, Key = EdgeTuple<V, Dir>>,
{
    type V = V;
    type Label = Label;

    fn link(self, label : Label, to : &'b Q2) -> Option<Label> {
        let (graph, from) = self;
        graph.map.insert(EdgeTuple::new(from.to_owned(), to.to_owned()), label)
    }

    fn unlink(self, to : &Q2) -> Option<Label> {
        let (graph, from) = self;
        graph.map.remove(&EdgeTuple::new(from.to_owned(), to.to_owned()))
    }
}

impl<'a, 'b, U, Label, Map, VMap, Q1, Q2, S, Dir, V> EdgeEntry<'b, Q2>
for (&'a mut MapGraph<U, Map, VMap, S, Abstract, Dir>, &'a Q1)
where
Dir : Direction,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q1 : ToOwned<Owned = V>,
Q2 : ToOwned<Owned = V>,
Map : map::UnstableFixedSizedMap<EdgeTuple<V, Dir>, Value = Label, Key = EdgeTuple<V, Dir>>,
MapGraph<U, Map, VMap, S, Abstract, Dir> : ConcreteGraphLike<Q2>,
MapGraph<U, Map, VMap, S, Abstract, Dir> : ConcreteGraphLike<Q1>,
{
    type V = V;
    type Label = Label;

    fn link(self, label : Label, to : &'b Q2) -> Option<Label> {
        let (graph, from) = self;
        if graph.contains(to) && graph.contains(from) {
            graph.map.insert(EdgeTuple::new(from.to_owned(), to.to_owned()), label)
        } else {
            None
        }
    }

    fn unlink(self, to : &Q2) -> Option<Label> {
        let (graph, from) = self;
        graph.map.remove(&EdgeTuple::new(from.to_owned(), to.to_owned()))
    }

}

impl<'a, U, Label, VLabel, Map, VMap, Q, Dir, V> VertexEntry
for (&'a mut MapGraph<U, Map, VMap, Unstable, Abstract, Dir>, &'a Q)
where
Dir : Direction,
V : Borrow<Q> + Clone,
Q : ToOwned<Owned = V>,
Map : map::UnstableFixedSizedMap<EdgeTuple<V, Dir>, Value = Label, Key = EdgeTuple<V, Dir>>,
for<'c> &'c Map : IntoIterator<Item = (&'c EdgeTuple<V, Dir>, &'c Label)>,
VMap : map::Map<Q, Value = VLabel, Key = V>,
EdgeTuple<V, Dir> : IsConnected<Q> + Clone,
{
    type Label = VLabel;

    fn remove(self) -> Option<VLabel> {
        let (graph, from) = self;
        if let Some(label) = graph.vmap.remove(from) {
            let edges =
                (&graph.map)
                .into_iter()
                .filter(|&(e, _l)| e.connected(from))
                .map(|(k, _)| k.clone())
                .collect::<Vec<_>>();
            for v in edges {
                graph.map.remove(&v);
            }
            Some(label)
        } else {
            None
        }
    }
}

impl<'b, 'a : 'b, V, A, S, Dir, Vert, Q, Map, Label> Edges<'b> 
for &'b MapGraphEntry<Vert, &'a Map, S, A, Dir, &'a Q>
where
&'b Map : IntoOrder<'b, Q, Key = V, Value =Label>,
<&'b Map as IntoOrder<'b, Q>>::IntoOrder : Iterator<Item = (&'b V, &'b Label)>,
Dir : Direction,
V : Borrow<Q>,
Q : Eq,
{
    type V = V;
    type Label = Label;
    type Edges = <&'b Map as IntoOrder<'b, Q>>::IntoOrder;

    fn edges(self) -> <&'b Map as IntoOrder<'b, Q>>::IntoOrder {
        (& self.map).into_order(self.q)
    }
}

impl<'b, 'a : 'b, V, A, S, Dir, Vert, Q, Map, Label> EdgesMut<'b> 
for &'b mut MapGraphEntry<Vert, &'a mut Map, S, A, Dir, &'a Q>
where
&'b mut Map : IntoOrder<'b, Q, Key = V, Value = Label>,
<&'b mut Map as IntoOrder<'b, Q>>::IntoOrder : Iterator<Item = (&'b V, &'b mut Label)>,
V : Borrow<Q>,
Dir : Direction,
Q : Eq,
{
    type Label = Label;
    type V = V;
    type Edges = <&'b mut Map as IntoOrder<'b, Q>>::IntoOrder;

    fn edges_mut(self) -> <&'b mut Map as IntoOrder<'b, Q>>::IntoOrder {
        (&mut self.map).into_order(self.q)
    }
}

impl<U, Map, VMap, Q, S, Dir> ConcreteGraphLike<Q>
for MapGraph<U, Map, VMap, S, Concrete, Dir>
where
Dir : Direction
{
    #[inline(always)]
    fn contains(&self, _ : &Q) -> bool {
        true
    }
}

impl<'a, Label, Map, VMap, Q, Dir, V, S> VertexCreate
for (&'a mut MapGraph<UnitFactory, Map, VMap, S , Abstract, Dir>, &'a Q)
where
Dir : Direction,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
VMap : map::StableMap<V, Value = Label, Key = V>,
{
    type V = V;
    type Label = Label;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
        let (graph, vertex) = self;
        Ok((vertex.to_owned(), graph.vmap.insert(vertex.to_owned(), label)))
    }
}

impl<'a, Label, Map, VMap, Dir, V> VertexCreate
for &'a mut MapGraph<InternalFactory, Map, VMap, Stable , Abstract, Dir>
where
Dir : Direction,
VMap : map::InternalStableMap<V, Value = Label, Key = V>,
{
    type V = V;
    type Label = Label;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
        self.vmap.append(label).map(|v| (v, None))
    }
}
        
impl<'a, Label, Map, VMap, F , Dir, V, S> VertexCreate
for &'a mut MapGraph<Factory<F>, Map, VMap, S , Abstract, Dir>
where
Dir : Direction,
V : Clone,
F : UUIDFactory<UUID = V>,
VMap : map::Map<V, Value = Label, Key = V>,
{
    type V = V;
    type Label = Label;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
        if let Some(uuid) = self.uuids.f.fresh() {
            Ok((uuid.to_owned(), self.vmap.insert(uuid, label)))
        } else {
            Err(label)
        }
    }
}



impl<Map, VMap, V, Q, F, Dir> ConcreteGraphLike<Q>
for MapGraph<Factory<F>, Map, VMap, Stable, Abstract, Dir>
where
F : StableUUIDFactory<UUID = V>,
Dir : Direction,
Q : ToOwned<Owned = V> + Ord,
V : Borrow<Q>,
{
    #[inline]
    fn contains(&self, v : &Q) -> bool {
        self.uuids.f.exists(v)
    }
}

impl<Map, VMap, V, Q, F, Dir> ConcreteGraphLike<Q>
for MapGraph<F, Map, VMap, Unstable, Abstract, Dir>
where
Dir : Direction,
Q : ToOwned<Owned = V> + Ord,
V : Borrow<Q>,
for<'c> &'c MapGraph<F, Map, VMap, Unstable, Abstract, Dir> : GraphLike<'c, Q>,
for<'c> <&'c MapGraph<F, Map, VMap, Unstable, Abstract, Dir> as GraphLike<'c, Q>>::Vertex : Vertex,
{
    #[inline]
    fn contains(&self, v : &Q) -> bool {
        self.vertex(v).exists()
    }
}

pub use self::edgematrix::VecMatrix;

pub mod edgematrix {
    //! A fixed sized array of edges used to implement Abstract graph with a fixed number of
    //! vertices.
    use std::marker::PhantomData;
    use std::mem::replace;

    use super::{
        EdgeTuple,
        Index,
    };
    use ::interface::Direction;
    use ::map::{
        MapOwned,
        FixedMap,
        FixedSizedMap,
        UnstableFixedSizedMap,
        IntoOrder,
    };


    /// A two dimensions array of edges.
    ///
    /// The size of the Matrix is fixed and no new vertices can be added after creation. 
    pub struct VecMatrix<K, T, Dir : Direction> {
        matrix : Vec<Option<T>>,
        base : usize, // the number of vertices
        direction : PhantomData<(K, Dir)>,
    }

    /*
    pub struct Edges<D : Direction> {
        base : usize,
        pos : usize,
    }
    */


    impl<Label, Dir : Direction, K> MapOwned for VecMatrix<K, Label, Dir> where EdgeTuple<K, Dir> : Index + Eq {
        type Key = EdgeTuple<K, Dir>;
        type Value = Label;
    }

    impl<T, K, Dir : Direction> FixedSizedMap for VecMatrix<K, T, Dir> where EdgeTuple<K, Dir> : Index + Eq {
        fn is_empty(&self) -> bool {
            self.matrix.is_empty()
        }

        fn insert(&mut self, edge : EdgeTuple<K, Dir>, label : T) -> Option<T> {
            let index = edge.index(self.base).expect("Adjacency matrix : index out of range");
            replace(unsafe { self.matrix.get_unchecked_mut(index) }, Some(label))
        }
    }

    impl<Label, Dir : Direction, K> FixedMap<EdgeTuple<K, Dir>> for VecMatrix<K, Label, Dir> where
        EdgeTuple<K, Dir> : Index + Eq,
            {
                fn get(&self, edge : &EdgeTuple<K, Dir>) -> Option<&Label> {
                    let index = edge.index(self.base);
                    if let Some(idx) = index {
                        unsafe { self.matrix.get_unchecked(idx) }.as_ref()
                    } else {
                        None
                    }
                }

                fn get_mut(&mut self, edge : &EdgeTuple<K, Dir>) -> Option<&mut Label> {
                    let index = edge.index(self.base);
                    if let Some(idx) = index {
                        unsafe { self.matrix.get_unchecked_mut(idx) }.as_mut()
                    } else {
                        None
                    }
                }

                fn contains_key(&self, edge : &EdgeTuple<K, Dir>) -> bool {
                    let index = edge.index(self.base);
                    if let Some(idx) = index {
                        unsafe { self.matrix.get_unchecked(idx) }.is_some()
                    } else {
                        false
                    }
                }
            }

    impl<Label, Dir : Direction, K> UnstableFixedSizedMap<EdgeTuple<K, Dir>> for VecMatrix<K, Label, Dir> where EdgeTuple<K, Dir> : Index + Eq {

        fn remove(&mut self, edge : &EdgeTuple<K, Dir>) -> Option<Label> {
            let index = edge.index(self.base);
            if let Some(idx) = index {
                replace(unsafe { self.matrix.get_unchecked_mut(idx) }, None)
            } else {
                None
            }
        }
    }
}
