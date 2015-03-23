
module Consensus.Harness (
) where


run :: (Monad m, Protocol p, Node n) => p -> n -> ( some continuation ) -> m ()
run p n = do
  (n', req) <- receive n
  (p', res) <- step p req
  send res n' -- LOL
  run p' n'
