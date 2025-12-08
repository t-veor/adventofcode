use std::{borrow::Borrow, collections::HashMap, hash::Hash};

#[derive(Debug, Clone)]
struct DisjointSetItem {
    parent: usize,
    // size and rank are only correct if this is a top-level node, i.e. if parent is itself.
    size: usize,
    rank: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DisjointSetKey(pub usize);

#[derive(Debug, Default, Clone)]
pub struct DisjointSet<T> {
    lookup: HashMap<T, usize>,
    items: Vec<DisjointSetItem>,
    sets: usize,
}

impl<T> FromIterator<T> for DisjointSet<T>
where
    T: Clone + Eq + Hash,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let size_hint = iter.size_hint().1.unwrap_or(0);

        let mut lookup = HashMap::with_capacity(size_hint);
        let mut items = Vec::with_capacity(size_hint);

        for (i, value) in iter.enumerate() {
            lookup.insert(value, i);
            items.push(DisjointSetItem {
                parent: i,
                size: 1,
                rank: 0,
            });
        }

        Self {
            sets: items.len(),
            lookup,
            items,
        }
    }
}

impl<T> DisjointSet<T> {
    fn _find(&mut self, mut x: usize) -> usize {
        debug_assert!(x < self.items.len());

        while self.items[x].parent != x {
            (x, self.items[x].parent) = (
                self.items[x].parent,
                self.items[self.items[x].parent].parent,
            );
        }
        x
    }

    pub fn find_by_key(&mut self, key: DisjointSetKey) -> Option<DisjointSetKey> {
        if key.0 >= self.items.len() {
            None
        } else {
            Some(DisjointSetKey(self._find(key.0)))
        }
    }

    fn _union(&mut self, x: usize, y: usize) -> bool {
        debug_assert!(x < self.items.len() && y < self.items.len());

        let x = self._find(x);
        let y = self._find(y);

        if x == y {
            return false;
        }

        // Ensure x.rank >= y.rank
        let (x, y) = if self.items[x].rank < self.items[y].rank {
            (y, x)
        } else {
            (x, y)
        };

        // Make x the new root
        self.items[y].parent = x;
        // Increment x's rank if necessary
        if self.items[x].rank == self.items[y].rank {
            self.items[x].rank += 1;
        }
        // Update x's size
        self.items[x].size += self.items[y].size;

        self.sets -= 1;
        true
    }

    pub fn union_by_keys(&mut self, x: DisjointSetKey, y: DisjointSetKey) -> Option<bool> {
        if x.0 >= self.items.len() || y.0 >= self.items.len() {
            None
        } else {
            Some(self._union(x.0, y.0))
        }
    }

    fn _rank(&mut self, x: usize) -> usize {
        debug_assert!(x < self.items.len());

        let x = self._find(x);
        self.items[x].rank
    }

    pub fn rank_by_key(&mut self, key: DisjointSetKey) -> Option<usize> {
        if key.0 >= self.items.len() {
            None
        } else {
            Some(self._rank(key.0))
        }
    }

    fn _size(&mut self, x: usize) -> usize {
        debug_assert!(x < self.items.len());

        let x = self._find(x);
        self.items[x].size
    }

    pub fn size_by_key(&mut self, key: DisjointSetKey) -> Option<usize> {
        if key.0 >= self.items.len() {
            None
        } else {
            Some(self._size(key.0))
        }
    }

    pub fn sets_with_sizes(&mut self) -> HashMap<DisjointSetKey, usize> {
        let mut sizes = HashMap::with_capacity(self.sets);

        for (i, item) in self.items.iter().enumerate() {
            if i == item.parent {
                sizes.insert(DisjointSetKey(i), item.size);
            }
        }

        sizes
    }

    pub fn sets(&self) -> usize {
        self.sets
    }
}

impl<T> DisjointSet<T>
where
    T: Clone + Eq + Hash,
{
    pub fn find<Q>(&mut self, x: &Q) -> Option<DisjointSetKey>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let idx = *self.lookup.get(x)?;
        Some(DisjointSetKey(self._find(idx)))
    }

    pub fn union<Q, Q2>(&mut self, x: &Q, y: &Q) -> Option<bool>
    where
        T: Borrow<Q> + Borrow<Q2>,
        Q: Hash + Eq + ?Sized,
        Q2: Hash + Eq + ?Sized,
    {
        let x = *self.lookup.get(x)?;
        let y = *self.lookup.get(y)?;

        Some(self._union(x, y))
    }

    pub fn rank<Q>(&mut self, x: &Q) -> Option<usize>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let x = *self.lookup.get(x)?;
        Some(self._rank(x))
    }

    pub fn size<Q>(&mut self, x: &Q) -> Option<usize>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let x = *self.lookup.get(x)?;
        Some(self._size(x))
    }
}
