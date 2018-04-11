import Control.Monad
import Control.Monad.Trans.State hiding (get, put, modify)

get :: State s s
get = state $ \f -> (f, f)

put :: s -> State s ()
put s = state $ \f -> ((), s)

exec :: State s a -> s -> s
exec sa s = snd $ runState sa s

eval :: State s a -> s -> a
eval sa s = fst $ runState sa s

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)
