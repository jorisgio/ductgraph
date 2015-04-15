use std::cmp::Ord;
use std::default::Default;
use std::marker::PhantomData;
use std::boxed;
use std::borrow::{Borrow, ToOwned};
use std::mem;
use std::iter::IntoIterator;

use ::map::{
    self,
    Map,
    MapOwned,
    FixedMap,
    StableMap,
    MapEntry,
    Entry,
    Vacant,
    Occupied
};

use ::interface::*;
use ::uuid::{
    Factory,
    UnitFactory,
    UUIDFactory,
    StableUUIDFactory,
};

use super::{
    Stable,
    Unstable,
    Abstract,
    Concrete
};

/// G
#[macro_export]
macro_rules! adjgraph_type{
    ($name:ident, Concrete, Directed, $vertex:ident, $map:ident, $adjlist:ident) =>
        (type $name<Label> =
         AdjGraph<UnitFactory, $map<$vertex, AdjVertex<(), $adjlist<$vertex, Label>, Concrete, Directed>>, Unstable, Concrete, Directed>;
        );

    ($name:ident, Concrete, Undirected, $vertex:ident, $map:ident, $adjlist:ident) =>
        (type $name<Label> =
         AdjGraph<UnitFactory, $map<$vertex, AdjVertex<(), $adjlist<$vertex, *mut Label>, Concrete, Undirected>>, Unstable, Concrete, Undirected>;
        );

    ($name:ident, Abstract, Directed, $stable:ident, $uuid:ty, $vertex:ident, $map:ident, $adjlist:ident) =>
        (type $name<Label, ELabel> =
         AdjGraph<$uuid, $map<$vertex, AdjVertex<Label, $adjlist<$vertex, ELabel>, Abstract, Directed>>, $stable, Abstract, Directed>;
         );

    ($name:ident, Abstract, Undirected, $stable:ident, $uuid:ty, $vertex:ident, $map:ident, $adjlist:ident) =>
        (type $name<Label, ELabel> =
         AdjGraph<$uuid, $map<$vertex, AdjVertex<Label, $adjlist<$vertex, *mut ELabel>, Abstract, Undirected>>, $stable, Abstract, Undirected>;
        );
    }

pub struct AdjGraph<U, M, S, A, D> {
    map : M,
    uuids : U,
    stability : PhantomData<(S, A, D)>,
}

impl<U : Default, M : Default, S, A, D> AdjGraph<U, M, S, A, D> {
    pub fn new() -> AdjGraph<U, M, S, A, D> {
        AdjGraph {
            map : M::default(),
            uuids : U::default(),
            stability : PhantomData,
        }
    }
}

impl<U, M : Default, S, A, D> AdjGraph<U, M, S, A, D> {
    pub fn with_uuids(u : U) -> AdjGraph<Factory<U>, M, S, A, D> {
        AdjGraph {
            map : M::default(),
            uuids : Factory { f : u },
            stability : PhantomData,
        }
    }
}




impl<Label, VLabel, V, M, N, S, A, D, U> Graph for AdjGraph<U, M, S, A, D> where
M : map::MapOwned<Key = V, Value =  AdjVertex<VLabel, N, A, D>>,
N : map::MapOwned<Key = V, Value = Label>,
{
    type Label = VLabel;
    type ELabel = Label;
    type V = V;
}


impl<'a, U, MapFrom, MapTo, V, Q, VLabel, S, A, Dir> GraphLike<'a, Q>
for AdjGraph<U, MapFrom, S, A, Dir>
where 
MapFrom : map::FixedMap<Q, Key = V, Value = AdjVertex<VLabel, MapTo, A, Dir>>,
V : Borrow<Q>,
{
    type Vertex = Option<&'a AdjVertex<VLabel, MapTo, A, Dir>>;

    fn vertex(&'a self, from : &'a Q) -> Option<&'a AdjVertex<VLabel, MapTo, A, Dir>> {
        self.map.get(from)
    }
}

impl<'a, U, MapFrom, MapTo, Q, V, VLabel, S, A, Dir> GraphLikeMut<'a, Q>
for  AdjGraph<U, MapFrom, S, A, Dir>
where 
MapFrom : map::FixedMap<Q, Key = V, Value = AdjVertex<VLabel, MapTo, A, Dir>>,
V : Borrow<Q>,
{
    type VertexMut = Option<&'a mut AdjVertex<VLabel, MapTo, A, Dir>>;

    fn vertex_mut(&'a mut self, from : &'a Q) -> Option<&'a mut AdjVertex<VLabel, MapTo, A, Dir>> {
        self.map.get_mut(from)
    }
}

impl<'a, U, MapFrom, MapTo, V, Q, VLabel, S> GraphLikeEntry<'a, Q>
for AdjGraph<U, MapFrom, S, Concrete, Directed>
where 
MapFrom : for<'c> map::MapEntry<'c, Key = V, Value = AdjVertex<VLabel, MapTo, Concrete, Directed>>,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type VertexEntry = <MapFrom as map::MapEntry<'a>>::Entry;

    fn vertex_entry(&'a mut self, from : &'a Q) -> <MapFrom as map::MapEntry<'a>>::Entry {
        self.map.entry(from.to_owned())
    }
}

pub struct AdjGraphEntry<T, U> {
    graph : T,
    vertex :  U,
}

impl<'a, U, MapFrom, MapTo, V, Q,  VLabel, S> GraphLikeEntry<'a, Q>
for AdjGraph<U, MapFrom, S, Concrete, Undirected>
where 
MapFrom : map::FixedMap<Q, Key = V, Value = AdjVertex<VLabel, MapTo, Concrete, Undirected>>,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type VertexEntry = AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Concrete, Undirected>, &'a Q>;

    fn vertex_entry(&'a mut self, from : &'a Q) -> AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Concrete, Undirected>, &'a Q> {
        AdjGraphEntry { graph : self, vertex : from }
    }
}

impl<'a, U, MapFrom, MapTo, V, Q, VLabel, S, D> GraphLikeEntry<'a, Q>
for  AdjGraph<U, MapFrom, S, Abstract, D>
where 
MapFrom : map::FixedMap<Q, Key = V, Value = AdjVertex<VLabel, MapTo, Abstract, D>>,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type VertexEntry = AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Abstract, D>, &'a Q>;

    fn vertex_entry(&'a mut self, from : &'a Q) -> AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Abstract, D>, &'a Q> {
        AdjGraphEntry { graph : self, vertex : from }
    }
}
pub struct AdjVertex<Label, T, A, D> {
    map : T,
    label : Label,
    is_abstract : PhantomData<(A, D)>,
}

impl<T : Default, A, D> Default for AdjVertex<(), T, A, D> {
    fn default() -> AdjVertex<(), T, A, D> {
        AdjVertex { map : <T as Default>::default(), label : (), is_abstract : PhantomData }
    }
}

pub struct Neighbors<I, Dir> {
    iter : Option<I>,
    marker : PhantomData< Dir>,
}

pub struct NeighborsMut<I, Dir> {
    iter : Option<I>,
    marker : PhantomData<Dir>,
}

impl<'a, I, Label, V> Iterator for Neighbors<I, Undirected> where I : Iterator<Item = (&'a V, &'a *mut Label)> {
    type Item = (&'a V, &'a Label);

    fn next(&mut self) -> Option<(&'a V, &'a Label)> {
        self.iter
            .iter_mut()
            .flat_map(|mut it| it.next().into_iter())
            .map(|(v, label)| unsafe {(v, &**label) })
            .next()
    }
}

impl<'a, I, Label, V> Iterator for Neighbors<I, Directed> where I : Iterator<Item = (&'a V, &'a Label)> {
    type Item = (&'a V, &'a Label);

    #[inline]
    fn next(&mut self) -> Option<(&'a V, &'a Label)> {
        self.iter.as_mut().and_then(|it| it.next())
    }
}

impl<'a, I, Label, V> Iterator for NeighborsMut<I, Undirected> where I : Iterator<Item = (&'a V, &'a mut *mut Label)> {
    type Item = (&'a V, &'a mut Label);

    fn next(&mut self) -> Option<(&'a V, &'a mut Label)> {
        self.iter
            .iter_mut()
            .flat_map(|mut it| it.next().into_iter())
            .map(|(v, label)| unsafe {(v, &mut **label) })
            .next()
    }
}

impl<'a, I, Label, V> Iterator for NeighborsMut<I, Directed> where I : Iterator<Item = (&'a V, &'a mut Label)> {
    type Item = (&'a V, &'a mut Label);

    #[inline]
    fn next(&mut self) -> Option<(&'a V, &'a mut Label)> {
        self.iter.as_mut().and_then(|it| it.next())
    }
}

impl<'b, 'a : 'b, Label : 'b,  MapTo, V, VLabel, Dir, A> Edges<'b>
for &'b Option<&'a AdjVertex<VLabel,MapTo, A, Dir>> where
&'b MapTo : IntoIterator,
Neighbors<<&'b MapTo as IntoIterator>::IntoIter, Dir> : Iterator<Item = (&'b V, &'b Label)>,
{
    type Label = Label;
    type V = V;
    type Edges = Neighbors<<&'b MapTo as IntoIterator>::IntoIter, Dir>;

    fn edges(self) -> Neighbors<<&'b MapTo as IntoIterator>::IntoIter, Dir> {
        Neighbors {
            iter : self.map(|e| (&e.map).into_iter()),
            marker : PhantomData,
        }
    }
}

impl<'b, 'a : 'b, Label : 'b, MapTo, V, VLabel, Dir, A> EdgesMut<'b>
for &'b mut Option<&'a mut AdjVertex<VLabel,MapTo, A, Dir>> where
&'b mut MapTo : IntoIterator,
NeighborsMut<<&'b mut MapTo as IntoIterator>::IntoIter, Dir> : Iterator<Item = (&'b V, &'b mut Label)>,
{

    type Label = Label;
    type V = V;
    type Edges = NeighborsMut<<&'b mut MapTo as IntoIterator>::IntoIter, Dir>;

    fn edges_mut(self) -> NeighborsMut<<&'b mut MapTo as IntoIterator>::IntoIter, Dir> {
        NeighborsMut {
            iter : self.as_mut().map(|e| (&mut e.map).into_iter()),
            marker : PhantomData,
        }
    }
}

impl<'c, 'b : 'c, 'a : 'b, Label : 'b, MapTo, V, VLabel, Q, A> Edge<'c, Q> 
for &'b Option<&'a AdjVertex<VLabel,MapTo, A, Undirected>>
where
MapTo : map::FixedMap<Q, Key = V, Value = *mut Label>,
V : Borrow<Q>,
{
    type Label = Label;
    type LabelGuard = &'b Label;
    type V = V;

    fn edge_label(self, to : &'c Q) -> Option<&'b Label> {
            self.and_then(|e| e.map.get(to) )
                .map(|p| unsafe {&**p })
    }

}

impl<'c, 'b : 'c,  'a : 'b, Label : 'b, MapTo, V, VLabel, Q, A> EdgeMut<'c, Q> 
for &'b mut Option<&'a mut AdjVertex<VLabel,MapTo, A, Undirected>>
where
MapTo : map::FixedMap<Q, Key = V, Value = *mut Label>,
V : Borrow<Q>,
{
    type Label = Label;
    type LabelGuard = &'b mut Label;
    type V = V;

    fn edge_label_mut(self, to : &'c Q) -> Option<&'b mut Label> {
            self.as_mut().and_then(|e| e.map.get_mut(to) )
                .map(|p| unsafe {&mut **p })
    }

}

impl<'c, 'b : 'c, 'a : 'b, Label : 'b, MapTo, V, VLabel, Q, A> Edge<'c, Q> 
for &'b Option<&'a AdjVertex<VLabel,MapTo, A, Directed>>
where
MapTo : map::FixedMap<Q, Key = V, Value = Label>,
V : Borrow<Q>,
{
    type Label = Label;
    type LabelGuard = &'b Label;
    type V = V;

    fn edge_label(self, to : &'c Q) -> Option<&'b Label> {
            self.and_then(|e| e.map.get(to) )
    }

}

impl<'c, 'b : 'c, 'a : 'b,  Label : 'b, MapTo, V, VLabel, Q, A> EdgeMut<'c, Q> 
for &'b mut Option<&'a mut AdjVertex<VLabel,MapTo, A, Directed>>
where
MapTo : map::FixedMap<Q, Key = V, Value = Label>,
V : Borrow<Q>,
{
    type Label = Label;
    type LabelGuard = &'b mut Label;
    type V = V;

    fn edge_label_mut(self, to : &'c Q) -> Option<&'b mut Label> {
            self.as_mut().and_then(|e| e.map.get_mut(to) )
    }

}

impl<'b, 'a, Label : 'a, MapTo : 'a, V : 'a, Q, T> EdgeEntry<'b, Q> 
for T 
where
T : Entry<'a, Value = AdjVertex<(), MapTo, Concrete, Directed>>,
MapTo : map::Map<Q, Key = V, Value = Label>,
AdjVertex<(), MapTo, Concrete, Directed> : Default,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type Label = Label;
    type V = V;

    fn link(self, label : Label, to : &'b Q) -> Option<Label> {
        self
            .occupied()
            // if the entry doesn't exist, create a new map
            .map(|occ| occ.into_mut())
            .unwrap_or_else(|vacant| vacant.insert(<AdjVertex<(),MapTo, Concrete, Directed> as Default>::default()))
            .map
            .insert(to.to_owned(), label)
    }

    fn unlink(self, to : &'b Q) -> Option<Label> {
        self
            .occupied()
            .ok()
            .and_then(|mut occ| {
                let ret;
                if { 
                    let map = &mut occ.get().map;
                    ret = map.remove(to);
                    map.is_empty()
                } { 
                    occ.remove(); 
                }
                ret
            })
    }

}
// FIXME? It's not really an entry but we need to check the target exists
impl<'a, 'b, U, Label, MapTo, MapFrom, V, Q1, Q2, S> EdgeEntry<'b, Q2> 
for AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Concrete, Undirected>, &'a Q1>
where
AdjGraph<U, MapFrom, S, Concrete, Undirected> : ConcreteGraphLike<Q2>,
MapFrom : for<'c> map::MapEntry<'c, Key = V, Value = AdjVertex<(),MapTo, Concrete, Undirected>>,
MapTo : map::Map<Q2, Key =  V, Value = *mut Label>,
MapTo : map::Map<Q1>,
AdjVertex<(),MapTo, Concrete, Undirected> : Default,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q2 : ToOwned<Owned = V>,
Q1 : ToOwned<Owned = V>,
Q2 : Eq,
{
    type Label = Label;
    type V = V;

    fn link(self, label : Label, to : &'b Q2) -> Option<Label> {
        let (from, graph) = (self.vertex, self.graph);
        let raw = unsafe { boxed::into_raw(Box::new(label)) };

        let owned_from = from.to_owned();
        let should_insert_reverse = {
            let from_as_q2 : &Q2 = owned_from.borrow();
            from_as_q2 != to
        };

        let old = {
            let mut map = &mut graph.map.entry(owned_from)
                .occupied()
                // if the entry doesn't exist, create a new map
                .map(|occ| occ.into_mut())
                .unwrap_or_else(|vacant| vacant.insert(<AdjVertex<(),MapTo, Concrete, Undirected> as Default>::default()))
                .map;
            // Now insert the edge in the map
            map.insert(to.to_owned(), raw)
        };
        if should_insert_reverse {
            let mut map = &mut graph.map.entry(to.to_owned())
                .occupied()
                .map(|occ| occ.into_mut())
                .unwrap_or_else(|vacant| vacant.insert(<AdjVertex<(),MapTo, Concrete, Undirected> as Default>::default()))
                .map;
            map.insert(from.to_owned(), raw);
        }
        old.map(|p| unsafe { *Box::from_raw(p) })
    }

    fn unlink(self, to : &Q2) -> Option<Label> {
        let (from, graph) = (self.vertex, self.graph);

        let owned_from = from.to_owned();
        let should_remove_reverse = {
            let from_as_q2 : &Q2 = owned_from.borrow();
            from_as_q2 != to
        };

        let maybe_rawptr = graph.map
            .entry(owned_from)
            .occupied()
            .ok()
            .and_then(|mut occ| {
                let ret;
                if { 
                    let map = &mut occ.get().map;
                    ret = map.remove(to);
                    map.is_empty()
                } { 
                    occ.remove(); 
                }
                ret
            })
        ;
        let maybe_rawptr = 
            if should_remove_reverse {
                maybe_rawptr.map(
                    |rawptr| {
                        let mut occ = graph.map.entry(to.to_owned())
                            .occupied()
                            .ok()
                            .expect("Undirected graph has no reverse edge");
                        if { 
                            let map = &mut occ.get().map; 
                            map.remove(from);
                            map.is_empty()
                        } {
                            occ.remove();
                        }
                        rawptr
                    })
            } else {
                maybe_rawptr
            };
            maybe_rawptr.map(|ptr| unsafe { *Box::from_raw(ptr) })
    }

}


// FIXME? It's not really an entry but we need to check the target exists
impl<'b, 'a, U, Label, MapTo, MapFrom, V : 'a, VLabel, Q1, Q2, S> EdgeEntry<'b, Q2> 
for AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Abstract, Directed>, &'a Q1>
where
AdjGraph<U, MapFrom, S, Abstract, Directed> : ConcreteGraphLike<Q2>,
MapFrom : map::FixedMap<Q1, Key = V, Value = AdjVertex<VLabel,MapTo, Abstract, Directed>>,
MapTo : map::Map<Q2, Key = V, Value = Label>,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q2 : ToOwned<Owned = V>,
{
    type Label = Label;
    type V = V;

    fn link(self, label : Label, to : &'b Q2) -> Option<Label> {
        let (from, graph) = (self.vertex, self.graph);
        if graph.contains(to) {
            graph.map
                .get_mut(from)
                .and_then(|e| e.map.insert(to.to_owned(), label))
        } else {
            None
        }
    }

    fn unlink(self, to : &'b Q2) -> Option<Label> {
        let (from, graph) = (self.vertex, self.graph);
        graph.map
            .get_mut(from)
            .and_then(|mut v| v.map.remove(to))
    }
}

// FIXME? It's not really an entry but we need to check the target exists
impl<'a, 'b, U, Label : 'a, MapTo, MapFrom, V : 'a, VLabel, Q1, Q2, S> EdgeEntry<'b, Q2> 
for AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, S, Abstract, Undirected>, &'a Q1>
where
AdjGraph<U, MapFrom, S, Abstract, Undirected> : ConcreteGraphLike<Q2>,
MapFrom : map::FixedMap<Q1, Key = V, Value = AdjVertex<VLabel,MapTo, Abstract, Undirected>>,
MapFrom : map::FixedMap<Q2, Key = V, Value = AdjVertex<VLabel,MapTo, Abstract, Undirected>>,
MapTo : map::Map<Q1, Key = V, Value = *mut Label>,
MapTo : map::Map<Q2, Key = V, Value = *mut Label>,
V : Borrow<Q1>,
V : Borrow<Q2>,
Q2 : ToOwned<Owned = V>,
Q1 : ToOwned<Owned = V>,
{
    type Label = Label;
    type V = V;

    fn link(self, label : Label, to : &Q2) -> Option<Label> {
        let (from, graph) = (self.vertex, self.graph);
        if !graph.contains(to) {
            return None
        }
        let (old,raw) =   
        { 
            if let Some(mut map) = graph.map.get_mut(from) {
                let raw = unsafe { boxed::into_raw(Box::new(label)) };
                (map.map.insert(to.to_owned(), raw), raw)
            } else {
                return None
            }
        };
        let mut map = graph.map.get_mut(to).unwrap();
        map.map.insert(from.to_owned(), raw);
        old.map(|p| unsafe { *Box::from_raw(p) })
    }

    fn unlink(self, to : &Q2) -> Option<Label> {
        let (from, graph) = (self.vertex, self.graph);
        graph.map
            .get_mut(from)
            .and_then(|e| e.map.remove(to))
            .map(|rawptr| {
                graph.map
                    .get_mut(to)
                    .expect("Undirected graph has no reverse edge")
                    .map
                    .remove(from);
                unsafe { *Box::from_raw(rawptr) }
            })
    }
}

impl<'b, 'a : 'b, Label, MapTo, Dir> Vertex 
for  &'b Option<&'a AdjVertex<Label, MapTo, Abstract, Dir>> {
    type Label = Label;
    type LabelGuard = &'b Label;

    fn label(self) -> Option<&'b Label> {
        self.map(|v| &v.label)
    }

    fn exists(self) -> bool {
        self.label().is_some()
    }

}

impl<'b, 'a : 'b,  Label, MapTo, Dir> VertexMut 
for &'b mut Option<&'a mut AdjVertex<Label, MapTo, Abstract, Dir>> {
    type Label = Label;
    type LabelGuard = &'b mut Label;

    fn label_mut(self) -> Option<&'b mut Label> {
        self.as_mut().map(|v| &mut v.label)
    }

}

impl<MapFrom, U, Q, Dir> ConcreteGraphLike< Q>
for AdjGraph<U, MapFrom, Unstable, Abstract, Dir>
where 
for<'a> AdjGraph<U, MapFrom, Unstable, Abstract, Dir> : GraphLike<'a, Q>,
for<'a> <AdjGraph<U, MapFrom, Unstable, Abstract, Dir> as GraphLike<'a, Q>>::Vertex : Vertex,
{
    #[inline]
    fn contains(&self, v : &Q) -> bool {
        self.vertex(v).exists()
    }
}

impl<MapFrom, U, V, Q, Dir> ConcreteGraphLike<Q>
for AdjGraph<Factory<U>, MapFrom, Stable, Abstract, Dir>
where 
U : StableUUIDFactory<UUID = V>,
V : Ord,
V : Borrow<Q>,
Q : Ord,
{
    #[inline]
    fn contains(&self, v : &Q) -> bool {
        self.uuids.f.exists(v)
    }
}
impl<MapFrom, U, S, Q, Dir> ConcreteGraphLike<Q>
for AdjGraph<U, MapFrom, S, Concrete, Dir>
{
    #[inline]
    fn contains(&self, _ : &Q) -> bool { true }
}

impl<'a, MapFrom, Label : 'a, MapTo : 'a, V : 'a, Q, U> VertexEntry 
for AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, Unstable, Abstract, Directed>, &'a Q>
where
MapFrom : map::Map<Q, Value = AdjVertex<Label, MapTo, Abstract, Directed>, Key = V>,
V : Borrow<Q>,
{
    type Label = Label;

    fn remove(self) -> Option<Label> {
        self.graph.map
            .remove(self.vertex)
            .map(|e| e.label)
    }
}

impl<'a, MapFrom, ELabel : 'a, Label, MapTo : 'a, V : 'a, Q, U> VertexEntry 
for AdjGraphEntry<&'a mut AdjGraph<U, MapFrom, Unstable, Abstract, Undirected>, &'a Q>
where
MapFrom : map::Map<Q, Key = V, Value = AdjVertex<Label, MapTo, Abstract, Undirected>>,
MapFrom : map::FixedMap<V, Key = V, Value = AdjVertex<Label, MapTo, Abstract, Undirected>>,
MapTo : map::Map<Q, Key = V, Value = *mut ELabel>,
for<'c> &'c MapTo : IntoIterator<Item = (&'c V, &'c *mut Label)>,
V : Borrow<Q>,
{
    type Label = Label;

    fn remove(self) -> Option<Label> {
        let map = &mut self.graph.map;
        let vertex = &self.vertex;
        map
            .remove(vertex)
            .map(|e| {
                for (to, ptr) in (&e.map).into_iter() {
                    <MapFrom as map::FixedMap<V>>::get_mut(map,to)
                        .map(|e| e.map.remove(vertex));
                     unsafe { Box::from_raw(*ptr); }
                }
                e.label
            })
    }
}

impl<'a, MapFrom, Label, MapTo : 'a, V : 'a, Q, S, Dir> VertexCreate
for AdjGraphEntry<&'a mut AdjGraph<UnitFactory, MapFrom, S, Abstract, Dir>, &'a Q>
where
MapFrom : map::StableMap<Q, Key = V, Value = AdjVertex<Label, MapTo, Abstract, Dir>>,
MapTo : Default,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type Label = Label;
    type V = V;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
        let newentry = AdjVertex { 
            map : <MapTo as Default>::default(),
            label  : label,
            is_abstract : PhantomData,
        };
        let map = &mut self.graph.map;
        let vertex = self.vertex;
        Ok((vertex.to_owned(), map
            .insert(vertex.to_owned(), newentry)
            .map(|mut e| {
                 let new = map.get_mut(vertex).unwrap();
                 mem::swap(&mut new.map, &mut e.map);
                  e.label }))
            )
    }
}

impl<'a, MapFrom, Label, MapTo, V, Dir, U> VertexCreate
for &'a mut AdjGraph<Factory<U>, MapFrom, Unstable, Abstract, Dir>
where
MapFrom : map::StableMap<V, Key = V, Value = AdjVertex<Label, MapTo, Abstract, Dir>>,
MapTo : Default,
U : UUIDFactory<UUID = V>,
V : ToOwned<Owned = V> + Clone,
{
    type Label = Label;
    type V = V;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
        if let Some(uuid) = self.uuids.f.fresh() {
            let newentry = AdjVertex { 
                map : <MapTo as Default>::default(),
                label  : label,
                is_abstract : PhantomData,
            };
            let map = &mut self.map;
            Ok((uuid.to_owned(),
                    map.insert(uuid.to_owned(), newentry)
                    .map(|mut e| {
                        let new = map.get_mut(&uuid).unwrap();
                        mem::swap(&mut new.map, &mut e.map);
                        e.label })))
        } else {
            Err(label)
        }
    }
}

// Optimized version for Stable graph : we won't add twice the same vertex
impl<'a, MapFrom, Label, MapTo, V, Dir, U> VertexCreate
for &'a mut AdjGraph<Factory<U>, MapFrom, Stable, Abstract, Dir>
where
MapFrom : map::StableMap<V, Value = AdjVertex<Label, MapTo, Abstract, Dir>, Key = V>,
MapTo : Default,
U : UUIDFactory<UUID = V>,
V : ToOwned<Owned = V> + Clone,
{
    type Label = Label;
    type V = V;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
        if let Some(uuid) = self.uuids.f.fresh() {
            let newentry = AdjVertex { 
                map : <MapTo as Default>::default(),
                label  : label,
                is_abstract : PhantomData,
            };
            let map = &mut self.map;
            map.insert(uuid.to_owned(), newentry);
            Ok((uuid.to_owned(), None))
        } else {
            Err(label)
        }
    }
}

impl<'a, MapFrom, Label, MapTo, V, Dir> VertexCreate
for &'a mut AdjGraph<UnitFactory, MapFrom, Stable, Abstract, Dir>
where
MapFrom : map::InternalStableMap<V, Value = AdjVertex<Label, MapTo, Abstract, Dir>, Key = V>,
MapTo : Default,
{
    type Label = Label;
    type V = V;

    fn create(self, label : Label) -> Result<(V, Option<Label>), Label> {
            let newentry = AdjVertex { 
                map : <MapTo as Default>::default(),
                label  : label,
                is_abstract : PhantomData,
            };
            self.map.append(newentry).map(|k| (k, None)).map_err(|e| e.label)
    }
}

