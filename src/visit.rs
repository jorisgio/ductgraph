//! An iterator implementation on a generic graph data structure


use std::ops::Deref;
use std::default::Default;
use std::collections::{
    VecDeque,
};

use ::map::Map;
use ::interface::*;

pub trait Visitor<Label> {
    type V : Clone;

    fn delay<'a, I, Q : Deref<Target = Self::V>>(&mut self, v : &Self::V, i : I) where I : Iterator<Item = (Q, &'a Label)> ;
    fn visit(&mut self) -> Option<Self::V>;

    #[inline]
    fn filter<P>(self, predicate : P) -> Filter<Self, P> where
        P : for<'r, 's> FnMut(&'r Self::V, &'r (&'s Self::V, &'s Label)) -> bool,
        Self : Sized
    {
        Filter { visitor : self, pred : predicate }
    }
}

#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
pub struct Filter<Visit, P> { 
    visitor : Visit,
    pred : P,
}

impl<Visit, P, Label> Visitor<Label> for Filter<Visit, P> where
Visit : Visitor<Label>,
P : for<'s, 'r> FnMut(&'s Visit::V, &'s (&'r Visit::V, &'r Label)) -> bool,
{
    type V = Visit::V;

    #[inline]
    fn delay<'a, I, Q : Deref<Target = Visit::V>>(&mut self, v : &Visit::V, i : I) where I : Iterator<Item = (Q, &'a Label)> {
        let p = &mut self.pred;
        let mut it = i.filter(|&(ref t, ref l)| p(v, &(&**t, *l)));
        self.visitor.delay(&v, &mut it)
    }

    #[inline]
    fn visit(&mut self) -> Option<Visit::V> {
        self.visitor.visit()
    }
}


pub struct Dfs<V, M> {
    stack : Vec<V>,
    map : M,
}

impl<V, T>  Dfs<V, T> where T : Default + Map<V, Key = V, Value = ()> {
    pub fn new(v : V) -> Dfs<V, T> {
        Dfs { stack : vec![v], map : T::default() }
    }
}


impl<V, M, Label> Visitor<Label> for Dfs<V, M> where
    M : Map<V, Value = (), Key = V>,
    V : Clone,
{
    type V = V;

    fn delay<'a, I, Q : Deref<Target = V>>(&mut self, _ : &V, i : I ) where V : 'a, I : Iterator<Item = (Q, &'a Label)> {
        self.stack.extend(i.map(|(v, _)| v.clone()));
    }

    fn visit(&mut self) -> Option<V> {
        while let Some(v) = self.stack.pop() {
            if self.map.insert(v.clone(), ()).is_none() {
                return Some(v)
            } 
        }
        None
    }

}

pub struct Bfs<V, M> {
    queue : VecDeque<V>,
    map : M,
}

impl<V, T> Bfs<V, T> where T : Default + Map<V, Value = (), Key = V> {
    pub fn new(v : V) -> Bfs<V, T> {
        let mut deque = VecDeque::new();
        deque.push_back(v);
        Bfs { queue : deque, map : T::default() }
    }
}

impl<V, M, Label> Visitor<Label> for Bfs<V, M> where
    M : Map<V, Value = (), Key = V>,
    V : Clone,
{
    type V = V;

    fn delay<'a, I, Q : Deref<Target = V>>(&mut self, _ : &V, i : I ) where V : 'a, I : Iterator<Item = (Q, &'a Label)> {
        self.queue.extend(i.map(|(v, _)| v.clone()));
    }

    fn visit(&mut self) -> Option<V> {
        while let Some(v) = self.queue.pop_front() {
            if self.map.insert(v.clone(), ()).is_none() {
                return Some(v)
            } 
        }
        None
    }
}

/*
pub struct Path<P, I, V, Label> {
    iter_builder : P,
    vertex : Option<V>,
    marker : PhantomData<(Label, I)>,
}

impl<P, I, V, Label> Path<P, I, V, Label> where 
I : for<'r, 's>  Iterator<Item = (&'s V, &'s Label)>,
V : Clone,
{
    fn find(start : V) -> Path<, I, V, Label> {
        Path { iter_builder : EmptyPathIterator, vertex : Some(start), marker : PhantomData }
    }
}

impl

P : for<'r, 's> FnMut(&'r V, &'s mut I) -> Option<&'s V>,
impl<P, V, Label> Visitor<Label> for PathVisitor<P, V, Label>  where
P : for<'r, 's, 't> FnMut(&'t V, &'r (&'s V, &'s Label)) -> bool,
V : Clone,
{
    type V = V;

    fn visit(&mut self) -> Option<V> {
        self.vertex.take()
    }

    fn delay<'a, I>(&mut self, v : &V, mut i : I) where I : Iterator<Item = (&'a V, &'a Label)>, V : 'a, Label : 'a{
        let mut p = &mut self.pred;
        self.vertex = i.find(|t| p(v, t)).map(|(v, _)| v.clone());
    }
}
*/

pub struct IterVertices<Visit, Graph> {
    graph : Graph,
    visitor : Visit,
}


impl<'a, V, Visit, G, Label> Iterator for IterVertices<Visit, &'a G> where
    G : Graph<V = V, ELabel = Label>,
    Label : 'a,
    Visit : Visitor<Label, V = V>,
    for<'c> G : GraphLike<'c, V>,
    for<'c> <G as GraphLike<'c, V>>::Vertex : Edges<'c, V = V, Label = Label>,
    V : Clone,
{
    type Item = V;

    fn next(&mut self) -> Option<V> {
        self.visitor
            .visit()
            .map(|v| {
                let iter = self.graph.vertex(&v).edges();
                self.visitor.delay(&v, iter);
                v 
            }
            )
    }
}


pub trait VisitGraph<V : Clone> : Graph<V = V> {
     fn visit<'a, Visit>(&'a self, visitor : Visit) -> IterVertices<Visit, &'a Self> where
         IterVertices<Visit, &'a Self> : Iterator<Item = Self::V>,
             Visit : Visitor<Self::ELabel, V = Self::V>;

     fn foreach_mut<Visit, F>(&mut self, visitor : Visit, f : F) where 
         Visit : Visitor<Self::ELabel, V = Self::V>,
         for<'c> Self : GraphLikeMut<'c, V>,
         for<'c> <Self as GraphLikeMut<'c, V>>::VertexMut : EdgesMut<'c, Label = Self::ELabel, V = V>,
         F :  for<'r> FnMut(&'r mut <Self as GraphLikeMut<Self::V>>::VertexMut);

}

impl<G, V : Clone, Label> VisitGraph<V> for G where G : Graph<V = V, ELabel = Label> 
{

     fn visit<'a, Visit>(&'a self, visitor : Visit) -> IterVertices<Visit, &'a G> where
        IterVertices<Visit, &'a G> : Iterator<Item = V>,
        Visit : Visitor<Label, V = V>,
    {
        IterVertices { visitor : visitor, graph : self }
    }

     fn foreach_mut<Visit, F>(&mut self, mut visitor : Visit, mut f : F) where 
         Visit : Visitor<Label, V = V>,
         for<'c> Self : GraphLikeMut<'c, V>,
         for<'c> <Self as GraphLikeMut<'c, V>>::VertexMut : EdgesMut<'c, Label = Label, V = V>,
         F :  for<'r> FnMut(&'r mut <G as GraphLikeMut<V>>::VertexMut) {
            while let Some(v) = visitor.visit() {
                let mut vertex = self.vertex_mut(&v);
                f(&mut vertex);
                let iter = vertex.edges_mut();
                visitor.delay(&v, iter.map(|(v, l)| (v, &*l)));
            }
          }
}
