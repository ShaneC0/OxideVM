use std::collections::HashMap;
use std::fmt;

pub trait IntoHeapType {
    fn into_heap_type(self) -> HeapType;
}

impl IntoHeapType for String {
    fn into_heap_type(self) -> HeapType {
        HeapType::String(self.to_string())
    }
}

impl IntoHeapType for Function {
    fn into_heap_type(self) -> HeapType {
        HeapType::Function(self)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub chunk: Vec<u8>,
    pub ip: usize,
    pub arity: u8,
}

impl Function {
    pub fn new() -> Self {
        Function {
            chunk: vec![],
            ip: 0,
            arity: 0,
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function(arity: {}, chunk: {:?})", self.arity, self.chunk)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum HeapType {
    String(String),
    Function(Function),
}

impl fmt::Display for HeapType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HeapType::String(s) => write!(f, "String(\"{}\")", s),
            HeapType::Function(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Clone)]
pub struct HeapInterner {
    map: HashMap<HeapType, usize>,
    vec: Vec<HeapType>,
}

impl HeapInterner {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: vec![]
        }
    }

    pub fn intern<T: IntoHeapType>(&mut self, value: T) -> usize {
        let heap_value = value.into_heap_type();
        if let Some(&index) = self.map.get(&heap_value) {
            index
        } else {
            let index = self.vec.len();
            self.vec.push(heap_value.clone());
            self.map.insert(heap_value, index);
            index
        }
    }

    pub fn get(&self, index: usize) -> Option<&HeapType> {
        self.vec.get(index)
    }
}

