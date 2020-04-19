use std::fmt;

pub(crate) struct Stack<T>(Vec<T>);

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Stack(vec![])
    }
}

impl<T> Stack<T> {
    pub fn push_front(&mut self, val: T) {
        self.0.push(val)
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.iter().nth(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.iter_mut().nth(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter().rev()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut().rev()
    }
}

impl<T: fmt::Debug> fmt::Debug for Stack<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Stack(")?;
        fmt::Debug::fmt(&self.0, f)?;
        write!(f, ")")
    }
}

impl<T: PartialEq> PartialEq for Stack<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T: Clone> Clone for Stack<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> From<Vec<T>> for Stack<T> {
    fn from(other: Vec<T>) -> Self {
        Self(other)
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut stack = Stack::default();
        stack.push_front(1);
        stack.push_front(2);
        stack.push_front(3);
        assert_eq!(stack.pop_front(), Some(3));
        stack.push_front(4);
        assert_eq!(stack.pop_front(), Some(4));
        assert_eq!(stack.pop_front(), Some(2));
        assert_eq!(stack.pop_front(), Some(1));
        assert_eq!(stack.pop_front(), None);
    }

    #[test]
    fn test_get() {
        let mut stack = Stack::default();
        stack.push_front(1);
        stack.push_front(2);
        stack.push_front(3);
        assert_eq!(stack.get(0), Some(&3));
        assert_eq!(stack.get(1), Some(&2));
        assert_eq!(stack.get(2), Some(&1));
        assert_eq!(stack.get(3), None);
    }

    #[test]
    fn test_iter() {
        let mut stack = Stack::default();
        stack.push_front(1);
        stack.push_front(2);
        stack.push_front(3);
        let result: Vec<_> = stack.iter().copied().collect();
        assert_eq!(result, vec![3, 2, 1]);
    }
}
