use rnix::SyntaxNode;
use std::collections::VecDeque;

pub(crate) trait CollectFromTree<D> {
    type State;
    type Result;
    type Error;

    fn enter_node(&mut self, dependencies: D, node: &rnix::SyntaxNode);

    fn exit_node(&mut self, dependencies: D, node: &rnix::SyntaxNode);

    fn state(&self) -> &Self::State;

    fn result(self) -> (Self::Result, Vec<Self::Error>);
}

#[derive(Debug, PartialEq, Clone, Default)]
pub(crate) struct TrackParent {
    pub(crate) state: VecDeque<SyntaxNode>,
}

impl TrackParent {
    pub fn new() -> Self {
        Default::default()
    }
}

impl CollectFromTree<()> for TrackParent {
    type State = VecDeque<SyntaxNode>;
    type Result = ();
    type Error = ();

    fn enter_node(&mut self, _: (), node: &rnix::SyntaxNode) {
        self.state.push_front(node.clone());
    }

    fn exit_node(&mut self, _: (), _: &rnix::SyntaxNode) {
        self.state.pop_front();
    }

    fn state(&self) -> &Self::State {
        &self.state
    }

    fn result(self) -> (Self::Result, Vec<Self::Error>) {
        ((), vec![])
    }
}
