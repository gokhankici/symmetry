{-# Language RebindableSyntax #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language DataKinds #-}
module Main where

import Prelude hiding ((>>=), (>>), fail, return)
import Symmetry.Language
import Symmetry.Verify

pongServer :: (DSL repr) => repr (Process repr ())
pongServer = do myPid <- self
                (p :: repr (T "Ping" (Pid RSing))) <- recv
                -- yield G: PtrR[p] = 0
                --       R: PtrW[p] > 0
                send (forget p) (lift (TyName :: TyName "Pong") myPid)
                -- exit G: PtrW[p] > 0

master :: (DSL repr) => repr
          (RSing -> Process repr ())
master = lam $ \r -> do p     <- spawn r pongServer
                        myPid <- self
                        send p (lift (TyName :: TyName "Ping") myPid)
                        -- yield G: PtrW[p] > 0, PtrR[master] = 0
                        --       R: PtrW[master] > 0
                        (_ :: repr (T "Pong" (Pid RSing))) <- recv
                        return tt
                        -- exit G: PtrR[master] = 1...

mainProc :: (DSL repr) => repr ()
mainProc = exec $ do r <- newRSing
                     r |> master

main :: IO ()
main = checkerMain mainProc
