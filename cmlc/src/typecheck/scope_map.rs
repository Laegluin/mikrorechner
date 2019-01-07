use fnv::FnvHashMap;
use std::hash::Hash;

// the impl is very naive and could be made a lot more efficient

/// A map with hierarchical, scoped keys.
#[derive(Debug)]
pub struct ScopeMap<K, V>
where
    K: Hash + Eq,
{
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
        let current_scope_idx = self.current_scope_idx();

        self.scopes[current_scope_idx]
            .entry(key)
            .or_insert(Level {
                value: None,
                next: None,
            })
            .value
            .replace(value)
    }

    pub fn path_insert(&mut self, path: impl IntoIterator<Item = K>, value: V) -> Option<V> {
        let current_scope_idx = self.current_scope_idx();

        let mut path: Vec<_> = path.into_iter().collect();
        let last_key = path.pop()?;

        let mut level = self;

        for key in path {
            level = level.scopes[current_scope_idx]
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
        for scope in self.scope_indices() {
            let value = self.scopes[scope]
                .get(key)
                .and_then(|level| level.value.as_ref());

            if let Some(value) = value {
                return Some(value);
            }
        }

        None
    }

    pub fn path_get<'a>(&self, path: impl IntoIterator<Item = &'a K>) -> Option<&V>
    where
        K: 'a,
    {
        let mut path: Vec<_> = path.into_iter().collect();
        let last_key = path.pop()?;

        for scope in self.scope_indices() {
            let mut level = self;

            for key in &path {
                let maybe_level = level.scopes[scope]
                    .get(key)
                    .and_then(|level| level.next.as_ref());

                level = match maybe_level {
                    Some(level) => level,
                    None => continue,
                }
            }

            if let Some(value) = level.get(last_key) {
                return Some(value);
            }
        }

        None
    }

    fn current_scope_idx(&self) -> usize {
        self.scopes.len() - 1
    }

    fn scope_indices(&self) -> impl Iterator<Item = usize> {
        (0..self.scopes.len()).rev()
    }
}

#[derive(Debug)]
struct Level<K, V>
where
    K: Hash + Eq,
{
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

        map.enter_scope();
        assert!(map.path_get(vec![]).is_none());

        // one element only
        let mut map = ScopeMap::<&str, bool>::new();

        assert_eq!(map.path_get(&vec!["a"]), None);
        assert_eq!(map.path_insert(vec!["a"], true), None);
        assert_eq!(map.path_insert(vec!["a"], true), Some(true));
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));

        map.enter_scope();
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));

        // long path
        let mut map = ScopeMap::<&str, bool>::new();

        assert_eq!(map.path_get(&vec!["a"]), None);
        assert_eq!(map.path_get(&vec!["a", "b"]), None);
        assert_eq!(map.path_insert(vec!["a"], true), None);
        assert_eq!(map.path_insert(vec!["a"], true), Some(true));
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));
        assert_eq!(map.path_get(&vec!["a", "b"]), None);

        map.enter_scope();
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));
        assert_eq!(map.path_get(&vec!["a", "b"]), None);

        assert_eq!(map.path_insert(vec!["a", "b"], false), None);
        assert_eq!(map.path_insert(vec!["a", "b"], false), Some(false));
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));
        assert_eq!(map.path_get(&vec!["a", "b"]), Some(&false));

        map.enter_scope();
        assert_eq!(map.path_get(&vec!["a"]), Some(&true));
        assert_eq!(map.path_get(&vec!["a", "b"]), Some(&false));
    }

    #[test]
    fn insert_and_get() {
        let mut map = ScopeMap::<&str, bool>::new();

        assert_eq!(map.get(&"key"), None);
        assert_eq!(map.insert("key", true), None);
        assert_eq!(map.get(&"key"), Some(&true));
        assert_eq!(map.insert("key", false), Some(true));
        assert_eq!(map.get(&"key"), Some(&false));

        map.enter_scope();
        assert_eq!(map.get(&"key"), Some(&false));
    }
}
