use std::collections::{VecDeque, HashMap};

#[derive(Debug, Clone)]
pub struct StringInterner {
    strings: VecDeque<String>,
    map: HashMap<String, usize>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: VecDeque::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern_str(&mut self, s: &str) -> usize {
        if let Some(&index) = self.map.get(s) {
            return index;
        }
        self.strings.push_back(s.to_string());
        let index = self.strings.len() - 1;
        self.map.insert(s.to_string(), index);
        index
    }  

    pub fn get_interned_str(&self, index: usize) -> &str {
        &self.strings[index]
    }
}
