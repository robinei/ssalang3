use std::collections::HashMap;
use std::num::NonZeroU32;
use std::sync::{LazyLock, RwLock};

/// A globally unique symbol identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(NonZeroU32);

impl Symbol {
    /// Create a new symbol with the given ID (for internal use)
    fn new(id: u32) -> Self {
        Self(NonZeroU32::new(id).unwrap())
    }

    /// Get the numeric ID of this symbol
    pub fn id(self) -> u32 {
        self.0.get()
    }

    /// Get the string representation of this symbol
    pub fn as_str(self) -> &'static str {
        SYMBOL_INTERNER.with_symbol(self)
    }

    /// Intern a string and return its symbol
    pub fn intern(s: &str) -> Symbol {
        SYMBOL_INTERNER.intern(s)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Thread-safe global symbol interner
struct SymbolInterner {
    /// Maps strings to their symbol IDs
    string_to_id: RwLock<HashMap<String, Symbol>>,
    /// Maps symbol IDs to their strings (stored as leaked static strings)
    id_to_string: RwLock<Vec<&'static str>>,
}

impl SymbolInterner {
    fn new() -> Self {
        Self {
            string_to_id: RwLock::new(HashMap::new()),
            id_to_string: RwLock::new(vec![""]),
        }
    }

    /// Intern a string and return its symbol
    fn intern(&self, s: &str) -> Symbol {
        assert!(s.len() > 0);

        // First try to find existing symbol (read lock)
        {
            let string_to_id = self.string_to_id.read().unwrap();
            if let Some(&symbol) = string_to_id.get(s) {
                return symbol;
            }
        }

        // Need to create new symbol (write lock)
        let mut string_to_id = self.string_to_id.write().unwrap();
        let mut id_to_string = self.id_to_string.write().unwrap();

        // Check again in case another thread added it while we were waiting
        if let Some(&symbol) = string_to_id.get(s) {
            return symbol;
        }

        // Create new symbol
        let id = id_to_string.len() as u32;
        let symbol = Symbol::new(id);

        // Leak the string to get a 'static reference
        // This is safe because symbols live for the entire program duration
        let static_str: &'static str = Box::leak(s.to_string().into_boxed_str());

        string_to_id.insert(s.to_string(), symbol);
        id_to_string.push(static_str);

        symbol
    }

    /// Get the string for a symbol (assumes symbol is valid)
    fn with_symbol(&self, symbol: Symbol) -> &'static str {
        let id_to_string = self.id_to_string.read().unwrap();
        id_to_string[symbol.0.get() as usize]
    }

    /// Get the number of interned symbols
    #[cfg(test)]
    #[allow(dead_code)]
    fn len(&self) -> usize {
        self.id_to_string.read().unwrap().len()
    }
}

/// Global symbol interner instance
static SYMBOL_INTERNER: LazyLock<SymbolInterner> = LazyLock::new(|| SymbolInterner::new());

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_interning() {
        let sym1 = Symbol::intern("test");
        let sym2 = Symbol::intern("test");
        let sym3 = Symbol::intern("different");

        // Same string should give same symbol
        assert_eq!(sym1, sym2);
        assert_ne!(sym1, sym3);

        // Should be able to get string back
        assert_eq!(sym1.as_str(), "test");
        assert_eq!(sym3.as_str(), "different");
    }

    #[test]
    fn test_symbol_ordering() {
        let sym1 = Symbol::intern("aaa");
        let sym2 = Symbol::intern("bbb");

        // Symbols should be ordered by creation order
        assert!(sym1 < sym2);
        assert!(sym1.id() < sym2.id());
    }

    #[test]
    fn test_symbol_display() {
        let sym = Symbol::intern("hello");
        assert_eq!(format!("{}", sym), "hello");
    }

    #[test]
    fn test_concurrent_interning() {
        use std::thread;

        let handles: Vec<_> = (0..10)
            .map(|i| {
                thread::spawn(move || {
                    let sym = Symbol::intern(&format!("test_{}", i));
                    assert_eq!(sym.as_str(), format!("test_{}", i));
                    sym
                })
            })
            .collect();

        let symbols: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();

        // All symbols should be different
        for i in 0..symbols.len() {
            for j in i + 1..symbols.len() {
                assert_ne!(symbols[i], symbols[j]);
            }
        }
    }
}
