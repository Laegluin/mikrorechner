use fnv::FnvHashMap;
use std::hash::Hash;

// the impl is very naive and could be made a lot more efficient

/// A map with hierarchical, scoped keys.
pub struct ScopeMap<K, V> {
    scopes: Vec<FnvHashMap<K, Level<K, V>>>,
}

impl<K, V> ScopeMap<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> ScopeMap<K, V> {
        ScopeMap {
            scopes: vec![FnvHashMap::default()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(FnvHashMap::default());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("cannot exit global scope");
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.current_scope_mut()
            .entry(key)
            .or_insert(Level {
                value: None,
                next: None,
            })
            .value
            .replace(value)
    }

    pub fn path_insert(&mut self, path: impl IntoIterator<Item = K>, value: V) -> Option<V> {
        let mut path: Vec<_> = path.into_iter().collect();
        let last_key = path.pop()?;

        let mut level = self;

        for key in path {
            level = level
                .current_scope_mut()
                .entry(key)
                .or_insert(Level {
                    value: None,
                    next: None,
                })
                .next
                .get_or_insert_with(|| ScopeMap::new());
        }

        level.insert(last_key, value)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.current_scope()
            .get(key)
            .and_then(|level| level.value.as_ref())
    }

    pub fn path_get<'a>(&self, path: impl IntoIterator<Item = &'a K>) -> Option<&V>
    where
        K: 'a,
    {
        let mut path: Vec<_> = path.into_iter().collect();
        let last_key = path.pop()?;

        let mut level = self;

        for key in path {
            level = level.current_scope().get(key)?.next.as_ref()?;
        }

        level.get(last_key)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.current_scope_mut()
            .get_mut(key)
            .and_then(|level| level.value.as_mut())
    }

    fn current_scope(&self) -> &FnvHashMap<K, Level<K, V>> {
        &self.scopes[self.scopes.len() - 1]
    }

    fn current_scope_mut(&mut self) -> &mut FnvHashMap<K, Level<K, V>> {
        let current_scope = self.scopes.len() - 1;
        &mut self.scopes[current_scope]
    }
}

struct Level<K, V> {
    value: Option<V>,
    next: Option<ScopeMap<K, V>>,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn path_insert_and_get() {
        // empty paths do nothing
        let mut map = ScopeMap::<&str, bool>::new();

        assert!(map.path_insert(vec![], true).is_none());
        assert!(map.path_get(vec![]).is_none());

        // one element only
        let mut map = ScopeMap::<&str, bool>::new();

        assert_eq!(map.path_get(&vec!["a"]), None);
        assert_eq!(map.path_insert(vec!["a"], true), None);
        assert_eq!(map.path_insert(vec!["a"], true), Some(true));
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));

        // long path
        let mut map = ScopeMap::<&str, bool>::new();

        assert_eq!(map.path_get(&vec!["a"]), None);
        assert_eq!(map.path_get(&vec!["a", "b"]), None);
        assert_eq!(map.path_insert(vec!["a"], true), None);
        assert_eq!(map.path_insert(vec!["a"], true), Some(true));
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));
        assert_eq!(map.path_get(&vec!["a", "b"]), None);

        assert_eq!(map.path_insert(vec!["a", "b"], false), None);
        assert_eq!(map.path_insert(vec!["a", "b"], false), Some(false));
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));
        assert_eq!(map.path_get(&vec!["a", "b"]), Some(&false));
    }
}
