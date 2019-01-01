pub fn find_remove<T, F>(vec: &mut Vec<T>, mut pred: F) -> Option<T>
where
    F: FnMut(&T) -> bool,
{
    vec.iter()
        .enumerate()
        .find(|(_, elem)| pred(elem))
        .map(|(i, _)| i)
        .map(|i| vec.remove(i))
}
