//! A generic interface on Map datastructures. 

use std::cmp::{
    Ord,
    Eq,
};
use std::hash::Hash;
use std::collections::{
    hash_map,
    btree_map,
};
use std::borrow::Borrow;
pub use std::collections::{
    HashMap,
    BTreeMap,
};


#[derive(PartialEq, Eq)]
pub enum ExtOrdering {
    Equal,
    Less,
    Greater,
    Partial,
}

pub trait ExternalOrd<Q> {
    fn ext_cmp(&self, to : &Q) -> ExtOrdering;
}

/// Convert a mapping into an iterator on a orderalence class of the key-value pairs.
///
/// The iterator will only yield pairs such that the key is equal to the bound.
pub trait IntoOrder<'a, Q> {
    type Key : ExternalOrd<Q>;
    type Value;

    type IntoOrder : Iterator;

    /// Returns an iterator on the key value pairs with the specified bound
    fn into_order(self, eq : &'a Q) -> Self::IntoOrder;
}

/// An iterator on an orderalence class defined by external order between Q and the key
pub struct IterOrder<'a, Q : 'a, I> {
    iter : I,
    eq : &'a Q,
}

/// Any mapping from elements of type `Key` to elements of type `Value`
pub trait MapOwned {
    type Key : Eq;
    type Value; 
}

/// A mapping from Keys to Values with lookup functionnality for any type `Q` orderalent to the Key
/// type
pub trait FixedMap<Q> : MapOwned where Self::Key : Borrow<Q> {

    /// Returns a reference to the value corresponding to the key.
    /// The key may be any borrowed form of the map's key type, as long as the borrowed form
    /// properties (`Eq`, `Hash`, `Ord`, ...) match the properties on the keys type.
    fn get(&self, key : &Q) -> Option<&Self::Value>;

    /// Returns a mutable reference to the value corresponding to the key.
    /// The key may be any borrowed form of the map's key type, as long as the borrowed form
    /// properties (`Eq`, `Hash`, `Ord`, ...) match the properties on the keys type.
    fn get_mut(&mut self, key : &Q) -> Option<&mut Self::Value>;

    /// Returns true if the map contains a value for the specified key.
    fn contains_key(&self, key : &Q) -> bool;
}

/// A mapping from Keys to Values which can be extended with new mappings, but have a predefined
/// size.
pub trait FixedSizedMap: MapOwned {
    /// Inserts a key-value pair into the map. If the map already contains a value for this key, the
    /// present value is returned, otherwise, `None` is returned.
    /// In both cases, the map contains the new pair.
    fn insert(&mut self, key : Self::Key, value : Self::Value) -> Option<Self::Value>;

    /// Returns `true` if the map contains no key-value pair.
    fn is_empty(&self) -> bool;
}

/// A mapping from Keys to values which can be extended with new mappings but has no predefined
/// size.
pub trait GrowableMap : FixedSizedMap {}


/// A mapping from Keys to Values which can be extended with new mappings and from which values
/// associated to a key can be retrieved and modified.
/// The mapping is stable in the sense that no key-value pair can ever be removed.
pub trait StableMap<Q> : GrowableMap + FixedMap<Q> {}

/// A stable mapping is just a Fixed growable mapping.
impl<Q, T> StableMap<Q> for T where T : GrowableMap + FixedMap<Q> {}

/// A Mapping from Keys to Values which are append only
pub trait InternalMap : MapOwned {
    /// Appends a new value to the map which will be associated with the next avaible key,
    /// and returns the key.
    fn append(&mut self, value : Self::Value) -> Self::Key;
    /// Returns `true` if the map contains no key-value pair.
    fn is_empty(&self) -> bool;
}

/// A mapping from Keys to Values which can be extends with new mappings and from which values
/// associated to a key can be retrived and modified. But the mapping is only exenstible with
/// *values*. The key the new value is associated too is computed by the map itself.
pub trait InternalStableMap<Q> : InternalMap + FixedMap<Q> {}

impl<Q, T> InternalStableMap<Q> for T where T : InternalMap + FixedMap<Q> {}


/// An unstable mapping from Keys to Values. Supports removal of key-value pairs, but the map has a
/// fixed size.
pub trait UnstableFixedSizedMap<Q> : FixedMap<Q> + FixedSizedMap {

    /// Remove a key-value pair from the map.
    ///
    /// If a value is present in the map for the specified key, this value is removed from the map
    /// and returned. If no such value exists, `None` is returned.
    /// The key may be any borrowed form of the map's key type, as long as the borrowed form
    /// properties (`Eq`, `Hash`, `Ord`, ...) match the properties on the keys type.
    fn remove(&mut self, key : &Q) -> Option<Self::Value>;
}

/// An unstable mapping from Keys to Values.
pub trait Map<Q> : UnstableFixedSizedMap<Q> + GrowableMap {}

/// In place filtering operation on map.
pub trait InternalIterator<T> {
    /// Retains only the elements specified by the predicate.
    ///
    /// Remove the elements `e` such that `f(e)` returns false.
    fn retain<F>(&mut self, f: F) where F: FnMut(&T) -> bool;
}

/// A view into a single empty location in a Map.
pub trait Vacant<'a> {
    type Value;

    /// Sets the value of the key-value pair associated with this location, and returns a mutable
    /// reference to it.
    fn insert(self, value: Self::Value) -> &'a mut Self::Value;
}

/// A view into a single occupied location in a Map.
pub trait Occupied<'a> {
    type Value;

    /// Replace the value of the key-value pair associated with this location with the given value,
    /// and returns the old value associated with this location.
    fn insert(&mut self, value: Self::Value) -> Self::Value;

    /// Takes the value out of the entry and returns it. The key-value pair is removed from the
    /// Map.
    fn remove(self) -> Self::Value;

    /// Returns a mutable reference to the value associated with this location.
    fn get(&mut self) -> &mut Self::Value;

    /// Converts this entry into a mutable reference to the value associated with this location.
    fn into_mut(self) -> &'a mut Self::Value;
}

/// A view into a single key-value pair placeholder in the map, which might already exists or be
/// empty.
pub trait Entry<'a> {
    type Value;

    /// The type of an occupied location.
    type Occ : Occupied<'a, Value = Self::Value>;

    /// The type of a vacant location.
    type Vac : Vacant<'a, Value = Self::Value>;

    /// Converts this entry into either an existing location in the map or if this key-value pair
    /// doesn't exist in the map, a placeholder to this location.
    fn occupied(self) -> Result<Self::Occ, Self::Vac>;
}

/// A mapping from Keys to Value which supports access and modification through the use of view
/// into a single location in the map.
pub trait MapEntry<'a> : MapOwned {
    /// The type of a view into a single key-value pair placeholder
    type Entry : Entry<'a, Value = Self::Value>;

    /// Returns the placeholder in the map corresponding to the given key. The key doesn't need to
    /// exists in the map.
    fn entry(&'a mut self, k : Self::Key) -> Self::Entry;
}

impl<K, V> MapOwned for HashMap<K, V> where K : Hash + Eq, {
    type Key = K;
    type Value = V;
}

impl<K, V> FixedSizedMap for HashMap<K, V> where K : Hash + Eq {

    fn insert(&mut self, key : K, value : V) -> Option<V> {
        self.insert(key, value)
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }

}

impl<K, V> GrowableMap for HashMap<K, V> where K : Hash + Eq {}
impl<K, V, Q> Map<Q> for HashMap<K, V> where K : Hash + Eq, K : Borrow<Q>, Q : Eq + Hash {}

impl<K, V, Q> UnstableFixedSizedMap<Q> for HashMap<K, V> where K : Hash + Eq, Q : Eq + Hash, K : Borrow<Q> {
    fn remove(&mut self, key : &Q) -> Option<V> {
        self.remove(key)
    }
}

impl<K, V, Q> FixedMap<Q> for HashMap<K, V> where K : Hash + Eq, Q : Eq + Hash, K : Borrow<Q> {

    fn get(&self, key : &Q) -> Option<&V>  {
        self.get(key)
    }

    fn get_mut(&mut self, key : &Q) -> Option<&mut V> { 
        self.get_mut(key)
    }

    fn contains_key(&self, key : &Q) -> bool {
        self.contains_key(key)
    }
}


/// HashMaps have no order. All the map has to be visited
impl<'a, Q, K : ExternalOrd<Q>, V> Iterator for IterOrder<'a, Q, hash_map::Iter<'a, K, V>> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<(&'a K, &'a V)> {
        self.iter
            .next()
            .and_then(|(k, v)|
                match  k.ext_cmp(&self.eq) { 
                    ExtOrdering::Equal => Some((k, v)),
                    _ => self.next(),
                })
    }
}

/// HashMaps have no order. All the map has to be visited
impl<'a, Q, K : ExternalOrd<Q>, V> Iterator for IterOrder<'a, Q, hash_map::IterMut<'a, K, V>> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
        self.iter
            .next()
            .and_then(|(k, v)|
                match  k.ext_cmp(&self.eq) { 
                    ExtOrdering::Equal => Some((k, v)),
                    _ => self.next(),
                })
    }
}

/// The returned iterator is linear in the size of the Map
impl<'a, K, V, Q> IntoOrder<'a, Q> for &'a HashMap<K, V> where K : ExternalOrd<Q>, K : Eq + Hash {
    type Key = K;
    type Value = V;

    type IntoOrder = IterOrder<'a, Q, hash_map::Iter<'a, K, V>>;

    fn into_order(self, eq : &'a Q) -> IterOrder<'a, Q, hash_map::Iter<'a, K, V>> {
        IterOrder {
            iter : self.iter(),
            eq : eq,
        }
    }
}

/// The returned iterator is linear in the size of the Map
impl<'a, K, V, Q> IntoOrder<'a, Q> for &'a mut HashMap<K, V> where K : ExternalOrd<Q>, K : Eq + Hash {
    type Key = K;
    type Value = V;

    type IntoOrder = IterOrder<'a, Q, hash_map::IterMut<'a, K, V>>;

    fn into_order(self, eq : &'a Q) -> IterOrder<'a, Q, hash_map::IterMut<'a, K, V>> {
        IterOrder {
            iter : self.iter_mut(),
            eq : eq,
        }
    }
}

impl<'a, K, V> Occupied<'a> for hash_map::OccupiedEntry<'a, K, V> {
    type Value = V;

    fn insert(&mut self, value: V) -> V {
        self.insert(value)
    }

    fn remove(self) -> V {
        self.remove()
    }

    fn get(&mut self) -> &mut V {
        self.get_mut()
    }

    fn into_mut(self) -> &'a mut V {
        self.into_mut()
    }
}

impl<'a, K, V> Vacant<'a> for hash_map::VacantEntry<'a, K, V> {
    type Value = V;

    fn insert(self, value: V) -> &'a mut V {
        self.insert(value)
    }
}

impl<'a, K, V> Entry<'a> for hash_map::Entry<'a, K, V> {
    type Value = V;

    type Occ = hash_map::OccupiedEntry<'a, K, V>;
    type Vac = hash_map::VacantEntry<'a, K, V>;

    fn occupied(self) -> Result<hash_map::OccupiedEntry<'a, K, V>,  hash_map::VacantEntry<'a, K, V>> {
        match self {
            hash_map::Entry::Occupied(occ) => Ok(occ),
            hash_map::Entry::Vacant(vac) => Err(vac),
        }
    }
}

impl<'a, K, V> MapEntry<'a> for HashMap<K, V> where K : Hash + Eq {
    type Entry = hash_map::Entry<'a, K, V>;

    fn entry(&'a mut self, k : K) -> hash_map::Entry<'a, K, V> {
        self.entry(k)
    }
}


impl<K, V> MapOwned for BTreeMap<K, V> where K : Ord + Eq, {
    type Key = K;
    type Value = V;
}

impl<K, V> FixedSizedMap for BTreeMap<K, V> where K : Ord + Eq, {
    fn insert(&mut self, key : K, value : V) -> Option<V> {
        self.insert(key, value)
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<K, V> GrowableMap for BTreeMap<K, V> where K : Ord + Eq {}
impl<K, V, Q> Map<Q> for BTreeMap<K, V> where K : Ord + Eq, K : Borrow<Q>, Q : Ord + Eq {}

impl<K, V, Q> UnstableFixedSizedMap<Q> for BTreeMap<K, V> where K : Ord + Eq, Q : Ord + Eq, K : Borrow<Q> { 

    fn remove(&mut self, key : &Q) -> Option<V> {
        self.remove(key)
    }
}

impl<K, V, Q> FixedMap<Q> for BTreeMap<K, V> where K : Ord + Eq, Q : Ord + Eq, K : Borrow<Q> { 
    fn get(&self, key : &Q) -> Option<&V> {
        self.get(key)
    }

    fn get_mut(&mut self, key : &Q) -> Option<&mut V> {
        self.get_mut(key)
    }

    fn contains_key(&self, key : &Q) -> bool {
        self.contains_key(key)
    }
}

impl<'a, K : Ord, V> Occupied<'a> for btree_map::OccupiedEntry<'a, K, V> {
    type Value = V;

    fn insert(&mut self, value: V) -> V {
        self.insert(value)
    }

    fn remove(self) -> V {
        self.remove()
    }

    fn get(&mut self) -> &mut V {
        self.get_mut()
    }

    fn into_mut(self) -> &'a mut V {
        self.into_mut()
    }
}

impl<'a, K : Ord, V> Vacant<'a> for btree_map::VacantEntry<'a, K, V> {
    type Value = V;

    fn insert(self, value: V) -> &'a mut V {
        self.insert(value)
    }
}

impl<'a, K : Ord, V> Entry<'a> for btree_map::Entry<'a, K, V> {
    type Value = V;

    type Occ = btree_map::OccupiedEntry<'a, K, V>;
    type Vac = btree_map::VacantEntry<'a, K, V>;

    fn occupied(self) -> Result<btree_map::OccupiedEntry<'a, K, V>,  btree_map::VacantEntry<'a, K, V>> {
        match self {
            btree_map::Entry::Occupied(occ) => Ok(occ),
            btree_map::Entry::Vacant(vac) => Err(vac),
        }
    }
}


impl<'a, K,  V > MapEntry<'a> for BTreeMap<K, V> where K : Ord + Eq {

    type Entry = btree_map::Entry<'a, K, V>;

    fn entry(&'a mut self, k : K) -> btree_map::Entry<'a, K, V> {
        self.entry(k)
    }
}

/// Mostly linear in the size of the map. FIXME improve this once Range api is stabilized.
impl<'a, Q,  K : ExternalOrd<Q>, V> Iterator for IterOrder<'a, Q, btree_map::Iter<'a, K, V>> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<(&'a K, &'a V)> {
        self.iter
            .next()
            .and_then(|(k, v)| 
                      match k.ext_cmp(&self.eq) {
                          ExtOrdering::Equal => Some((k, v)),
                          ExtOrdering::Greater => None,
                          _ => self.next(),
                      })
                      
    }
}

/// HashMaps have no order. All the map has to be visited
impl<'a, Q, K : ExternalOrd<Q>, V> Iterator for IterOrder<'a, Q, btree_map::IterMut<'a, K, V>> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
        self.iter
            .next()
            .and_then(|(k, v)|
                      match k.ext_cmp(&self.eq) {
                          ExtOrdering::Equal => Some((k, v)),
                          ExtOrdering::Greater => None,
                          _ => self.next(),
                      })
    }
}

/// The returned iterator is linear in the size of the Map FIXME
impl<'a, K, V, Q> IntoOrder<'a, Q> for &'a BTreeMap<K, V> where
K : ExternalOrd<Q>,
K : Eq + Ord,
{
    type Key = K;
    type Value = V;

    type IntoOrder = IterOrder<'a, Q, btree_map::Iter<'a, K, V>>;

    fn into_order(self, eq : &'a Q) -> IterOrder<'a, Q, btree_map::Iter<'a, K, V>> {
        IterOrder {
            iter : self.iter(),
            eq : eq,
        }
    }
}

/// The returned iterator is linear in the size of the Map
impl<'a, K, V, Q> IntoOrder<'a, Q> for &'a mut BTreeMap<K, V> where 
K : ExternalOrd<Q>,
K : Eq + Ord,
{
    type Key = K;
    type Value = V;

    type IntoOrder = IterOrder<'a, Q, btree_map::IterMut<'a, K, V>>;

    fn into_order(self, eq : &'a Q) -> IterOrder<'a, Q, btree_map::IterMut<'a, K, V>> {
        IterOrder {
            iter : self.iter_mut(),
            eq : eq,
        }
    }
}

pub use self::vec_list::VecListMap; 
pub use self::stable_vec_map::StableVecMap;

pub mod vec_list {
    //! A simple Map implemented as a list of key-value tuples in a `Vec`
    //! Provide linear time search, constant time removal and inserstion
    use std::slice::{
        Iter,
        IterMut,
    };
    use std::borrow::Borrow;
    use std::mem;
    use super::{
        Map,
        FixedMap,
        MapOwned,
        GrowableMap,
        FixedSizedMap,
        InternalIterator,
        UnstableFixedSizedMap,
        IntoOrder,
        IterOrder,
        ExtOrdering,
        ExternalOrd,
    };


    /// A simple Map implemented as a list of key-value pair in a `Vec`
    /// Provide linear time search, constant time removal and inserstion
    pub struct VecListMap<K, V> {
        vec : Vec<VecMapTuple<K, V>>,
    }

    impl<K, V> VecListMap<K, V> {
        /// Construct a new empty map with default capacity
        pub fn new() -> VecListMap<K, V> {
            VecListMap { vec : Vec::new() }
        }

        /// Constructs a new, empty map with the specified capacity.
        ///
        /// The map will be able to hold exactly `capacity` elements without reallocating. If
        /// capacity is 0, the `map` will not allocate.
        pub fn with_capacity(cap : usize) -> VecListMap<K, V> {
            VecListMap { vec : Vec::with_capacity(cap) }
        }
    }

    /// A key-value pair entry
    pub struct VecMapTuple<K, V> {
        key : K,
        value : V,
    }

    impl<K, V> MapOwned for VecListMap<K, V> where K : Eq, {
        type Key = K;
        type Value = V;
    }

    impl<K, V> FixedSizedMap for VecListMap<K, V> where K : Eq, {
        fn insert(&mut self, key : K, value : V) -> Option<V> {
            {
                if let Some(tuple) = self.vec.iter_mut().find(|tuple| key == tuple.key) {
                    return Some(mem::replace(&mut tuple.value, value))
                }
            }
            self.vec.push(VecMapTuple { key : key, value : value });
            None
        }

        fn is_empty(&self) -> bool {
            self.vec.is_empty()
        }
    }

    impl<K, V> GrowableMap for VecListMap<K, V> where K : Eq {}

    impl<K, V, Q> Map<Q> for VecListMap<K, V> where K : Eq, Q : Eq, K : Borrow<Q> {}

    impl<K, V, Q> UnstableFixedSizedMap<Q> for VecListMap<K, V> where K : Eq, Q : Eq, K : Borrow<Q> {

        fn remove(&mut self, key : &Q) -> Option<V> {
            self.vec.iter()
                .position(|tuple| key == tuple.key.borrow())
                .map(|pos| self.vec.swap_remove(pos).value)
        }
    }

    impl<K, V, Q> FixedMap<Q> for VecListMap<K, V> where K : Eq, Q : Eq, K : Borrow<Q> {
        fn get(&self, k : &Q) -> Option<&V> {
            self.vec.iter()
                .find(|tuple| k == tuple.key.borrow())
                .map(|tuple| &tuple.value)
        }

        fn get_mut(&mut self, k : &Q) -> Option<&mut V> {
            self.vec.iter_mut()
                .find(|tuple| k == tuple.key.borrow())
                .map(|tuple| &mut tuple.value)
        }

        fn contains_key(&self, k : &Q) -> bool {
            self.vec.iter()
                .any(|tuple| tuple.key.borrow() == k)
        }

    }

    impl<K, V> InternalIterator<K> for VecListMap<K, V> where K : Eq {
        fn retain<F>(&mut self, mut f : F) where F : FnMut(&K) -> bool {
            self.vec.retain(|t| f(&t.key))
        }
    }

    impl<'a, Q, K : ExternalOrd<Q>, V> Iterator for IterOrder<'a, Q, Iter<'a, VecMapTuple<K, V>>> {
        type Item = (&'a K, &'a V);

        fn next(&mut self) -> Option<(&'a K, &'a V)> {
            self.iter
                .next()
                .and_then(|k| 
                          match  k.key.ext_cmp(&self.eq) { 
                              ExtOrdering::Equal => Some((&k.key, &k.value)),
                              _ => self.next(),
                          })
        }
    }

    impl<'a, Q, K : ExternalOrd<Q>, V> Iterator for IterOrder<'a, Q, IterMut<'a, VecMapTuple<K, V>>> {
        type Item = (&'a K, &'a mut V);

        fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
            self.iter
                .next()
                .and_then(|k| 
                          match  k.key.ext_cmp(&self.eq) { 
                              ExtOrdering::Equal => Some((&k.key, &mut k.value)),
                              _ => self.next(),
                          })
        }
    }

    /// The returned iterator is linear in the size of the Map
    impl<'a, K, V, Q> IntoOrder<'a, Q> for &'a VecListMap<K, V> where K : ExternalOrd<Q> + Eq {
        type Key = K;
        type Value = V;

        type IntoOrder = IterOrder<'a, Q,Iter<'a, VecMapTuple<K, V>>>;

        fn into_order(self, eq : &'a Q) -> IterOrder<'a, Q, Iter<'a, VecMapTuple<K, V>>> {
            IterOrder {
                iter : self.vec.iter(),
                eq : eq,
            }
        }
    }

    /// The returned iterator is linear in the size of the Map
    impl<'a, K, V, Q> IntoOrder<'a, Q> for &'a mut VecListMap<K, V> where K : ExternalOrd<Q> + Eq {
        type Key = K;
        type Value = V;

        type IntoOrder = IterOrder<'a, Q, IterMut<'a, VecMapTuple<K, V>>>;

        fn into_order(self, eq : &'a Q) -> IterOrder<'a, Q, IterMut<'a, VecMapTuple<K, V>>> {
            IterOrder {
                iter : self.vec.iter_mut(),
                eq : eq,
            }
        }

    }
}

pub mod stable_vec_map {
    //! A stable Map from unsigned integer keys to any value implemented as a vector.
    //! Search is in constant time


    use super::{
        MapOwned,
        FixedMap,
        InternalMap,
    };

    /// A stable map from unsigned integer keys to any value implemented as a vector
    pub struct StableVecMap<Value> {
        vec : Vec<Value>,
    }

    impl<Value> MapOwned for StableVecMap<Value> {
        type Key = usize;
        type Value = Value;
    }

    impl<Value> InternalMap for StableVecMap<Value> {
        #[inline]
        fn is_empty(&self) -> bool {
            self.vec.is_empty()
        }

        fn append(&mut self, v : Value) -> usize {
            self.vec.push(v);
            self.vec.len() - 1
        }
    }

    impl<Value> FixedMap<usize> for StableVecMap<Value> {
        #[inline]
        fn get(&self, key : &usize) -> Option<&Value> {
            self.vec.get(*key)
        }

        #[inline]
        fn get_mut(&mut self, key : &usize) -> Option<&mut Value> {
            self.vec.get_mut(*key)
        }

        #[inline]
        fn contains_key(&self, key : &usize) -> bool {
            *key < self.vec.len()
        }
    }
}
