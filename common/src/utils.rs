#[derive(Debug, Copy, Clone)]
pub struct NoCompare<T> {
    pub value: T,
}

impl<T> PartialEq for NoCompare<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for NoCompare<T> {}

impl<T> PartialOrd for NoCompare<T> {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}

impl<T> Ord for NoCompare<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<T> std::hash::Hash for NoCompare<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}
