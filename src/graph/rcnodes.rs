use std::rc::Rc;
use std::mem;
use std::ptr;
use std::boxed;
use std::iter::IntoIterator;
use std::sync::{
    RwLock,
    RwLockReadGuard,
    RwLockWriteGuard,
    Arc,
};
use std::cell::{
    RefCell,
    Ref,
    RefMut,
};
use std::ops::{
    Deref,
    DerefMut,
};
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::hash::{
    Hash,
    Hasher,
};
use ::interface::*;
use ::map::{
    self,
    Map,
    Entry,
    Occupied,
    Vacant,
    MapEntry,
};
use std::borrow::{
    Borrow,
};

trait NodeLike {
    type Label;
    type Map;

    fn as_edges(&self) -> &Self::Map; 
    fn as_label(&self) -> &Self::Label;

    fn as_edges_mut(&mut self) -> &mut Self::Map;
    fn as_label_mut(&mut self) -> &mut Self::Label;
}

/// A generic wrapper for nodes protecting from concurrent accesses with Read/Write semantic
pub struct NodeCell<N, Dir : Direction> {
    cell : N,
    marker : PhantomData<Dir>,
}

impl<N, Dir : Direction> NodeCell<RefCell<N>, Dir> {
    /// Construct a new NodeCell wrapping the node n using a `RefCell`
    fn refcell(n : N) -> NodeCell<RefCell<N>, Dir> {
        NodeCell { cell : RefCell::new(n), marker : PhantomData }
    }
}

impl<N, Dir : Direction> NodeCell<RwLock<N>, Dir> {
    /// Construct a new NodeCell wrapping the node n using a `RwLock`
    fn rwlock(n : N) -> NodeCell<RwLock<N>, Dir> {
        NodeCell { cell : RwLock::new(n), marker : PhantomData }
    }
}

pub trait AsVertex<'a> {
    type Vertex : 'a; 

    fn as_vertex(&'a self) -> Self::Vertex;

}

pub trait AsVertexMut<'a> {
    type VertexMut : 'a;

    fn as_vertex_mut(&'a self) -> Self::VertexMut;
}

impl<'a, N, Dir : Direction> AsVertex<'a> for NodeCell<N, Dir> where NodeCell<N, Dir>  : NodeCellLock<'a>,  {
    type Vertex = <NodeCell<N, Dir> as NodeCellLock<'a>>::NodeGuard;

    #[inline]
    fn as_vertex(&'a self) -> <NodeCell<N, Dir> as NodeCellLock<'a>>::NodeGuard {
        self.lock_node()
    }
}

impl<'a, N> AsVertexMut<'a> for NodeCell<N, Directed> where NodeCell<N, Directed> : NodeCellLockMut<'a>  {
    type VertexMut = <NodeCell<N, Directed> as NodeCellLockMut<'a>>::NodeGuardMut;

    #[inline]
    fn as_vertex_mut(&'a self) -> <NodeCell<N, Directed> as NodeCellLockMut<'a>>::NodeGuardMut {
        self.lock_node_mut()
    }
}

trait NodeCellLock<'a> {
    type Node : NodeLike + 'a;
    #[must_use]
    type NodeGuard : Deref<Target = Self::Node> + 'a;

    fn lock_node(&'a self) -> Self::NodeGuard;

}

trait NodeCellLockMut<'a> : NodeCellLock<'a> {
    #[must_use]
    type NodeGuardMut : DerefMut<Target = Self::Node> + 'a;

    fn lock_node_mut(&'a self) -> Self::NodeGuardMut;
}

impl<'a, Dir : Direction, Node : NodeLike +'a> NodeCellLock<'a>
for NodeCell<RefCell<Node>, Dir> {
    type Node = Node;
    type NodeGuard = Ref<'a, Node>;

    fn lock_node(&'a self) -> Ref<'a, Node> {
        self.cell.borrow()
    }
}

impl<'a, Dir : Direction, Node : NodeLike + 'a> NodeCellLockMut<'a>
for NodeCell<RefCell<Node>, Dir> {
    type NodeGuardMut = RefMut<'a, Node>;

    fn lock_node_mut(&'a self) -> RefMut<'a, Node> {
        self.cell.borrow_mut()
    }
}

impl<'a, Dir : Direction, Node : NodeLike + 'a> NodeCellLock<'a>
for NodeCell<RwLock<Node>, Dir> {
    type Node = Node;
    type NodeGuard = RwLockReadGuard<'a, Node>;

    fn lock_node(&'a self) -> RwLockReadGuard<'a,Node > {
        self.cell.read().unwrap()
    }
}

impl<'a, Dir : Direction, Node : NodeLike + 'a > NodeCellLockMut<'a>
for NodeCell<RwLock<Node>, Dir> {
    type NodeGuardMut = RwLockWriteGuard<'a, Node>;

    fn lock_node_mut(&'a self) -> RwLockWriteGuard<'a, Node> {
        self.cell.write().unwrap()
    }
}

pub struct EdgeGuard<'a, G : 'a> {
    src : G,
    dst : Option<G>,
    marker : PhantomData<&'a G>,
}

impl<'a, G : 'a> EdgeGuard<'a, G> where G : DerefMut {
    #[inline]
    fn deref_endpoints_mut(&mut self) -> (&mut G::Target, Option<&mut G::Target>) {
        (&mut self.src, self.dst.as_mut().map(|s| s.deref_mut()))
    }
}

trait EdgeLockMut<'a> {
    type Node;
    type GuardMut : DerefMut<Target = Self::Node> + 'a;

    fn lock_mut(self) -> EdgeGuard<'a, Self::GuardMut>;
}

impl<'a, N : 'a + NodeLike> EdgeLockMut<'a> for (&'a NodeCell<RefCell<N>, Undirected>, &'a NodeCell<RefCell<N>, Undirected>)
where NodeCell<RefCell<N>, Undirected> : NodeCellLockMut<'a> {
    type Node = <NodeCell<RefCell<N>, Undirected> as NodeCellLock<'a>>::Node;
    type GuardMut = <NodeCell<RefCell<N>, Undirected> as NodeCellLockMut<'a>>::NodeGuardMut;

    fn lock_mut(self) -> EdgeGuard<'a, <NodeCell<RefCell<N>, Undirected> as NodeCellLockMut<'a>>::NodeGuardMut> {
        let (src, dst) = self;
        EdgeGuard {
            src : src.lock_node_mut(),
            dst : if src != dst { Some(dst.lock_node_mut()) } else { None },
            marker : PhantomData,
        }
    }
}


impl<'a, N : 'a + NodeLike> EdgeLockMut<'a> for (&'a NodeCell<RwLock<N>, Undirected>, &'a NodeCell<RwLock<N>, Undirected>)
where NodeCell<RwLock<N>, Undirected> : NodeCellLockMut<'a> {
    type Node = <NodeCell<RwLock<N>, Undirected> as NodeCellLock<'a>>::Node;
    type GuardMut = <NodeCell<RwLock<N>, Undirected> as NodeCellLockMut<'a>>::NodeGuardMut;

    fn lock_mut(self) -> EdgeGuard<'a, <NodeCell<RwLock<N>, Undirected> as NodeCellLockMut<'a>>::NodeGuardMut> {
        let (src, dst) = self;
        let (srclock, dstlock) = match src.cmp(dst) { 
            Ordering::Equal => (src.lock_node_mut(), None),
            Ordering::Less => (src.lock_node_mut(), Some(dst.lock_node_mut())),
            Ordering::Greater => { let lk1 = dst.lock_node_mut(); (src.lock_node_mut(), Some(lk1)) },
        };
        EdgeGuard {
            src : srclock,
            dst : dstlock,
            marker : PhantomData,
        }
    }
}

pub type RcNode<N, D> = Rc<NodeCell<RefCell<N>, D>>;
pub type ArcNode<N, D> = Arc<NodeCell<RwLock<N>, D>>;

fn rc_node<D : Direction, N>(n : N) -> RcNode<N, D> {
    Rc::new(NodeCell::refcell(n))
}

fn arc_node<D : Direction, N>(n : N) -> ArcNode<N, D> {
    Arc::new(NodeCell::rwlock(n))
}
macro_rules! node_decl {
    ($map:ident, $rc:ident, $new:ident) =>
        (
            pub mod directed {
                use ::interface::Directed;
                use ::graph::rcnodes::{$rc, $new, NodeLike};
                use ::map::$map;

                pub struct Node<Label, ELabel> {
                    edges : $map<$rc<Node<Label, ELabel>, Directed>, ELabel>,
                    label : Label,
                }

                impl<Label, ELabel> Node<Label, ELabel> {
                    pub fn new(label : Label) -> $rc<Node<Label, ELabel>, Directed> {
                        $new(Node { edges : $map::new(), label : label })
                    }
                }

                impl<Label, ELabel> NodeLike for Node<Label, ELabel> {
                    type Label = Label;
                    type Map = $map<$rc<Node<Label, ELabel>, Directed>, ELabel>;

                    fn as_label(&self) -> &Label {
                        &self.label
                    }

                    fn as_label_mut(&mut self) -> &mut Label {
                        &mut self.label
                    }

                    fn as_edges(&self) -> &$map<$rc<Node<Label, ELabel>, Directed>, ELabel> {
                        &self.edges
                    }

                    fn as_edges_mut(&mut self) -> &mut $map<$rc<Node<Label, ELabel>, Directed>, ELabel> {
                        &mut self.edges
                    }
                }
            }
            pub mod undirected {
                use ::interface::Undirected;
                use ::graph::rcnodes::{$rc, $new, NodeLike};
                use ::map::$map;

                pub struct Node<Label, ELabel> {
                    edges : $map<$rc<Node<Label, ELabel>, Undirected>, *mut ELabel>,
                    label : Label,
                }

                impl<Label, ELabel> Node<Label, ELabel> {
                    pub fn new(label : Label) -> $rc<Node<Label, ELabel>, Undirected> {
                        $new(Node { edges : $map::new(), label : label })
                    }
                }

                impl<Label, ELabel> NodeLike for Node<Label, ELabel> {
                    type Label = Label;
                    type Map = $map<$rc<Node<Label, ELabel>, Undirected>, *mut ELabel>;

                    fn as_label(&self) -> &Label {
                        &self.label
                    }

                    fn as_label_mut(&mut self) -> &mut Label {
                        &mut self.label
                    }

                    fn as_edges(&self) -> &$map<$rc<Node<Label, ELabel>, Undirected>, *mut ELabel> {
                        &self.edges
                    }

                    fn as_edges_mut(&mut self) -> &mut $map<$rc<Node<Label, ELabel>, Undirected>, *mut ELabel> {
                        &mut self.edges
                    }
                }
            }
            )
}


macro_rules! node {
    ($name:ident, $map:ident) =>
        (
            pub mod $name {
                pub mod rc {
                    node_decl!($map, RcNode, rc_node);
                }
                pub mod arc {
                    node_decl!($map, ArcNode, arc_node);
                }
            }
        )
}


node!(btree, BTreeMap);
node!(hash, HashMap);
node!(veclist, VecListMap);

impl<N, Dir : Direction> PartialEq for NodeCell<N, Dir> {
    #[inline]
    fn eq(&self, rhs : &NodeCell<N, Dir>) -> bool {
        self as *const NodeCell<N, Dir>
            == rhs as *const NodeCell<N, Dir>
    }
}

impl<N, D : Direction> Eq for NodeCell<N, D> {}

impl<N, D : Direction> PartialOrd for NodeCell<N, D> {
    #[inline]
    fn partial_cmp(&self, rhs : &NodeCell<N, D>) -> Option<Ordering> {
        (self as *const NodeCell<N, D>).partial_cmp(
            &(rhs as *const NodeCell<N, D>))
    }
}

impl<N, D : Direction> Ord for NodeCell<N, D> {
    #[inline]
    fn cmp(&self, rhs : &NodeCell<N, D>) -> Ordering {
        (self as *const NodeCell<N, D>).cmp(
            &(rhs as *const NodeCell<N, D>))
    }
}

impl<N, D : Direction> Hash for NodeCell<N, D> {
    #[inline]
    fn hash<H : Hasher>(&self, state : &mut H) {
        (self as *const NodeCell<N, D>).hash(state);
    }
}

#[must_use]
pub struct LabelGuard<'a, G : 'a> {
    g : G,
    marker : PhantomData<&'a G>,
}

impl<'a, Label : 'a,  G : 'a, Node : 'a > Deref for LabelGuard<'a, G> where
G : Deref<Target = Node>,
Node : NodeLike<Label = Label>,
{
    type Target = Label;

    #[inline]
    fn deref(&self) -> &Label {
        self.g.as_label()
    }
}

impl<'a,  Label : 'a, G : 'a, Node : 'a> DerefMut for LabelGuard<'a, G> where
G : DerefMut<Target = Node>,
Node : NodeLike<Label = Label>,
{
    #[inline]
    fn deref_mut<'b>(&'b mut self) -> &'b mut Label {
        self.g.as_label_mut()
    }
}

impl<'a, Label : 'a, T, Node : 'a> Vertex for &'a T where
T : NodeCellLock<'a, Node = Node>,
Node : NodeLike<Label = Label>,
{
    type Label = Label;
    type LabelGuard = LabelGuard<'a, T::NodeGuard>;

    #[inline]
    fn exists(self) -> bool { true }

    #[inline]
    fn label(self) -> Option<LabelGuard<'a, T::NodeGuard>> {
        Some(LabelGuard { g : self.lock_node(), marker : PhantomData })
    }
}

impl<'a, Label : 'a, T : 'a, Node : 'a > VertexMut for &'a T where
T : NodeCellLockMut<'a, Node = Node>,
Node : NodeLike<Label = Label>,
{
    type Label = Label;
    type LabelGuard = LabelGuard<'a, T::NodeGuardMut>;

    #[inline]
    fn label_mut(self) -> Option<LabelGuard<'a, T::NodeGuardMut>> {
        Some(LabelGuard { g : self.lock_node_mut(), marker : PhantomData })
    }
}

#[must_use]
pub struct EdgeLabelGuardMut<'a, G : 'a, Label : 'a> {
    _g : G,
    // Need to use a raw pointer because we can't have a backref to ourself
    // The guard is kept here and ensure the raw pointer is valid
    label : *mut Label,
    marker : PhantomData<&'a G>,
}

impl<'a, Label, G> Deref for EdgeLabelGuardMut<'a, G, Label>
{
    type Target = Label;

    #[inline]
    fn deref(&self) -> &Label {
        unsafe { mem::transmute(self.label) }
    }
}

impl<'a, Label : 'a, G> DerefMut for EdgeLabelGuardMut<'a, G, Label>
{
    #[inline]
    fn deref_mut(&mut self) -> &mut Label {
        unsafe { mem::transmute(self.label) }
    }
}

impl<'a, M,  ELabel : 'a, T, Q, Node> EdgeMut<'a, Q> for &'a NodeCell<T, Directed>  where
NodeCell<T, Directed> : NodeCellLockMut<'a, Node = Node>,
Node : NodeLike<Map = M>,
M : map::Map<Q, Value = ELabel>,
M::Key : Borrow<Q>,
{
    type V = M::Key;
    type Label = ELabel;
    type LabelGuard = EdgeLabelGuardMut<'a, <NodeCell<T, Directed> as NodeCellLockMut<'a>>::NodeGuardMut,  ELabel>;

    #[inline]
    fn edge_label_mut(self, to : &'a Q) -> Option<EdgeLabelGuardMut<'a, <NodeCell<T, Directed> as NodeCellLockMut<'a>>::NodeGuardMut, ELabel>> { 
        let mut guard = self.lock_node_mut();
        guard.as_edges_mut()
            .get_mut(to)
            .map(|l|  l as *mut ELabel )
            .map(|l| EdgeLabelGuardMut { _g : guard, label : l, marker : PhantomData })
    }
}

pub type ELock<'a, T> = (&'a NodeCell<T, Undirected>, &'a NodeCell<T, Undirected>);

impl<'a, M, Node, ELabel : 'a, T, Q> EdgeMut<'a, Q> for &'a NodeCell<T, Undirected> where
NodeCell<T, Undirected> : NodeCellLockMut<'a, Node = Node>,
ELock<'a, T> : EdgeLockMut<'a, Node = Node>,
Node : NodeLike<Map = M>,
M : map::Map<Q, Value = *mut ELabel>,
M::Key : Borrow<Q>,
Q : Deref<Target = NodeCell<T, Undirected>>,
{
    type V = M::Key;
    type Label = ELabel;
    type LabelGuard = EdgeLabelGuardMut<'a, EdgeGuard<'a, <ELock<'a, T> as EdgeLockMut<'a>>::GuardMut>, ELabel>;

    #[inline]
    fn edge_label_mut(self, to : &'a Q) -> Option<EdgeLabelGuardMut<'a, EdgeGuard<'a, <ELock<'a, T> as EdgeLockMut<'a>>::GuardMut>, ELabel>> { 
        let mut guard = (self, to.deref()).lock_mut();
        {
            let (src, _) = guard.deref_endpoints_mut();
            src.as_edges_mut()
                .get_mut(to)
                .map(|l| *l as *mut ELabel)
        }
        .map(|l| EdgeLabelGuardMut { _g :  guard,  label : l, marker : PhantomData })
    }
}

impl<'b, 'a, M, Node , ELabel : 'a, T : 'a, R> EdgeEntry<'a, R> for &'a R where
R : Deref<Target = NodeCell<T, Undirected>> + Clone,
NodeCell<T, Undirected> : NodeCellLockMut<'a, Node = Node>,
ELock<'a, T> : EdgeLockMut<'a, Node = Node>,
Node : NodeLike<Map = M>,
for<'d> M : map::MapEntry<'d, Value = *mut ELabel, Key = R>,
M : map::Map<R>,
{
    type V = M::Key;
    type Label = ELabel;

    fn link(self, label : ELabel, to : &'a R) -> Option<ELabel> { 
        let mut guard = (self.deref(), to.deref()).lock_mut();
        let (mut guard1, guard2) = guard.deref_endpoints_mut();
        let mut old_label = None;
        let ptr = guard1.as_edges_mut()
            .entry(to.clone())
            .occupied()
            .map(|mut occ|
                 unsafe { 
                     let ptr = *occ.get();
                     // Get back the old value
                     old_label = Some(ptr::read(ptr));
                     // Reuse the memory
                     ptr
                 })
            .unwrap_or_else(|vacant| 
                     *vacant.insert(unsafe { boxed::into_raw(Box::new({ mem::uninitialized::<ELabel>() }))})
                     );
        unsafe { *ptr = label };
        if let Some(g) = guard2 { g.as_edges_mut().insert(self.clone(), ptr); };
        old_label
    }

    fn unlink(self, to : &'a R) -> Option<ELabel> {
        let mut guard = (self.deref(), to.deref()).lock_mut();
        let (mut guard1, mut guard2) = guard.deref_endpoints_mut();
        
        guard1.as_edges_mut()
            .remove(to)
            .map(|ptr| { if let Some(ref mut g) = guard2 { g.as_edges_mut().remove(self); } unsafe { *Box::from_raw(ptr) }})
    }

}

pub struct EdgesGuard<'a, G : 'a, I : 'a, D : Direction> {
    _g : G,
    iter : I,
    marker : PhantomData<(&'a (), D)>,
}

impl<'a, G, I : Iterator> Iterator for EdgesGuard<'a, G, I, Directed> {
    type Item = <I as Iterator>::Item;
    
    #[inline]
    fn next(&mut self) -> Option<<I as Iterator>::Item> {
        self.iter.next()
    }
}

impl<'a, G, I, V, ELabel> Iterator for EdgesGuard<'a, G, I, Undirected>
where I : Iterator<Item = (&'a V, &'a *mut ELabel)>,
{
    type Item = (&'a V, &'a ELabel);
    
    #[inline]
    fn next(&mut self) -> Option<(&'a V, &'a ELabel)> {
        self.iter.next().map(|(v, ptr)| (v, unsafe {&**ptr }))
    }
}

impl<'a, M : 'a , ELabel : 'a, T, I : 'a, Node : 'a, V, Dir : Direction> Edges<'a> for &'a NodeCell<T, Dir> where 
NodeCell<T, Dir> : NodeCellLock<'a, Node = Node>,
Node : NodeLike<Map = M>,
&'a M : IntoIterator<IntoIter = I, Item = <I as Iterator>::Item>,
// FIXME ! Why is the I parameter needed ? Looks like <&'a M as IntoIterator>::IntoIter : 'a has no
// effect. rustc 1.0.0-nightly (be9bd7c93 2015-04-05) (built 2015-04-06)
I : Iterator,
EdgesGuard<'a, <NodeCell<T, Dir> as NodeCellLock<'a>>::NodeGuard, I, Dir> : Iterator<Item = (&'a V, &'a ELabel)>,
{
    type Label = ELabel;
    type V = V;

    type Edges = EdgesGuard<'a, <NodeCell<T, Dir> as NodeCellLock<'a>>::NodeGuard, I, Dir>;

    #[inline]
    fn edges(self) -> EdgesGuard<'a,<NodeCell<T, Dir> as NodeCellLock<'a>>::NodeGuard, I, Dir> {
        // FIXME ! Unsafe hack, is it really safe ? We should do a better job.
        let refguard = self.lock_node();
        let guard = EdgesGuard {
            // copy the RefGuard into the new guard
            _g : unsafe { ptr::read(&refguard as *const <NodeCell<T, Dir> as NodeCellLock<'a>>::NodeGuard) }, 
            // And then build an iterator from the old value and cheat about lifetime
            iter : unsafe { mem::transmute::<_, &'a M>(refguard.as_edges())}.into_iter(),
            marker : PhantomData,
        };
        // This is a copy, but we really want to keep a move semantic. forget about the old copy
        unsafe { mem::forget(refguard) };
        guard
    }
}

impl<'a, M : 'a , ELabel : 'a, T, I : 'a, Node, V> EdgesMut<'a> for &'a NodeCell<T, Directed> where 
NodeCell<T, Directed> : NodeCellLockMut<'a, Node = Node>,
Node : NodeLike<Map = M>,
&'a M : IntoIterator<IntoIter = I, Item = <I as Iterator>::Item>,
// FIXME ! Why is the I parameter needed ? Looks like <&'a M as IntoIterator>::IntoIter : 'a has no
// effect. rustc 1.0.0-nightly (be9bd7c93 2015-04-05) (built 2015-04-06)
I : Iterator,
EdgesGuard<'a, <NodeCell<T, Directed> as NodeCellLockMut<'a>>::NodeGuardMut, I, Directed> : Iterator<Item = (&'a V, &'a mut ELabel)>,
{
    type Edges = EdgesGuard<'a, <NodeCell<T, Directed> as NodeCellLockMut<'a>>::NodeGuardMut, I, Directed>;
    type Label = ELabel;
    type V = V;

    #[inline]
    fn edges_mut(self) -> EdgesGuard<'a,<NodeCell<T, Directed> as NodeCellLockMut<'a>>::NodeGuardMut, I, Directed> {
        // FIXME ! Unsafe hack, is it really safe ? We should do a better job.
        let mut refguard = self.lock_node_mut();
        let guard = EdgesGuard {
            // copy the RefGuard into the new guard
            _g : unsafe { ptr::read(&refguard as *const <NodeCell<T, Directed> as NodeCellLockMut<'a>>::NodeGuardMut) }, 
            // And then build an iterator from the old value and cheat about lifetime
            iter : unsafe { mem::transmute::<_, &'a mut M>(refguard.as_edges_mut())}.into_iter(),
            marker : PhantomData,
        };
        // This is a copy, but we really want to keep a move semantic. forget about the old copy
        unsafe { mem::forget(refguard) };
        guard
    }
}

#[test]
fn it_works() {
    use self::btree::rc::undirected::Node;

    let vertex = Node::new(1u32);
    let vertex1 = Node::new(2u32);
    let vertex2 = Node::new(3u32);
    let vertex3 = Node::new(4u32);
    println!("{:?}", &*vertex.label().unwrap());
    *vertex.label_mut().unwrap() = 54;
    println!("{:?}", &*vertex.label().unwrap());
    vertex.link(1, &vertex1);
    vertex.link(2, &vertex3);
    vertex.link(3, &vertex2);
    vertex2.link(3, &vertex1);
    vertex3.link(4, &vertex2);

    for (_,l) in vertex.edges() {
        println!("egdges {:?}", l);
    }

}

/*
/*

impl<'a, M, Label, ELabel, Dir> Vertex for &'a Node<M, Label, Dir> where
M : map::MapOwned<Value = ELabel>,
Dir : Direction,
{
    type Label = Label;
    type LabelGuard = &'a Label;

    #[inline]
    fn exists(self) -> bool { true }

    #[inline]
    fn label(self) -> Option<&'a Label> { Some(&self.label) }
}

impl<'a, M, Label, ELabel, Dir> VertexMut for &'a mut Node<M, Label, Dir> where
M : map::MapOwned<Value = ELabel>,
Dir : Direction,
{
    type Label = Label;
    type LabelGuard = &'a mut Label;

    #[inline]
    fn label_mut(self) -> Option<&'a mut Label> { Some(&mut self.label) }
}


impl<'a, M, Label, ELabel, V, Q, Directed> Edge<Q> for &'a Node<M, Label, Directed> where 
M : map::Map<Q, Key = V, Value = ELabel>,
V : Borrow<Q>,
{
    type Label = ELabel;
    type LabelGuard = &'a ELabel;
    type V = V;

    #[inline]
    fn edge_label(self, to : &Q) -> Option<&'a ELabel> { 
        self.edges
            .get(to)
    }
}

impl<'a, M, Label, ELabel, V, Q> EdgeMut<Q> for &'a mut Node<M, Label, Directed> where 
M : map::Map<Q, Key = V, Value = ELabel>,
V : Borrow<Q>,
{
    type Label = ELabel;
    type LabelGuard = &'a mut ELabel;
    type V = V;

    #[inline]
    fn edge_label_mut(self, to : &Q) -> Option<&'a mut ELabel> { 
        self.edges
            .get_mut(to)
    }

}

impl<'a, M : 'a , Label : 'a, ELabel : 'a, V, Dir> Edges for &'a Node<M, Label, Dir> where 
&'a M : IntoIterator<Item = (&'a V, &'a ELabel)>,
Dir : Direction,
{
    type Edges = <&'a M as IntoIterator>::IntoIter;
    type Item = (&'a V, &'a ELabel);

    #[inline]
    fn edges(self) -> <&'a M as IntoIterator>::IntoIter {
        (&self.edges).into_iter()
    }
}

impl<'a, M : 'a, Label : 'a, ELabel : 'a, V> EdgesMut for &'a mut Node<M, Label, Directed> where 
&'a mut M : IntoIterator<Item = (&'a V, &'a mut ELabel)>,
{
    type Edges = <&'a mut M as IntoIterator>::IntoIter;
    type Item = (&'a V, &'a mut ELabel);

    #[inline]
    fn edges_mut(self) -> <&'a mut M as IntoIterator>::IntoIter {
        (&mut self.edges).into_iter()
    }
}

impl<'a, M, Label, ELabel, Q, V> EdgeEntry<Q> for &'a mut Node<M, Label, Directed> where 
M : map::Map<Q, Key = V, Value = ELabel>,
V : Borrow<Q>,
Q : ToOwned<Owned = V>,
{
    type Label = ELabel;
    type V = V;

    #[inline]
    fn link(self, label : ELabel, to : &Q) -> Option<ELabel> { 
        self.edges
            .insert(to.to_owned(), label)
    }

    #[inline]
    fn unlink(self, to : &Q) -> Option<ELabel> {
        self.edges
            .remove(to)
    }
}

/*

macro_rules! test_basic {
    ($name:ident, $test:ident) => (
        #[test]
        fn $test() {
            let graph = $name::new(42u32);
            let graph2 = $name::new(43u32);

            graph.vertex_mut().link(4, &graph2);

            println!("{:?}", graph.vertex().label().unwrap());
            println!("{:?}", graph.vertex().edge_label(&graph2).unwrap());
        }
        )
}

test_basic!(BTreeRcNode, btree_rcnode_basic);
test_basic!(BTreeArcNode, btree_arcnode_basic);
test_basic!(HashRcNode, hash_rcnode_basic);
test_basic!(HashArcNode, hash_arcnode_basic);
test_basic!(VecListArcNode, veclist_arcnode_basic);
test_basic!(VecListRcNode, veclist_rcnode_basic);
*/
*/
*/
