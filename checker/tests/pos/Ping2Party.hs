{-# LANGUAGE DataKinds #-}
{-# Language RebindableSyntax #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


module Main where

import Prelude hiding ((>>=), (>>), fail, return) 
import Symmetry.Language
import Symmetry.Verify
import SrcHelper

{-
| p           | Q         | r           |
|-------------+-----------+-------------|
| for q in Q: | r <- recv | for q in Q: |
|   send q r  | send r q  |   recv      |
-}

type R = T "R" (Pid RSing)
liftR :: (DSL repr) => repr (Pid RSing) -> repr R
liftR =  lift (TyName :: TyName "R")

type Q = T "Q" (Pid RSing)
liftQ :: (DSL repr) => repr (Pid RSing) -> repr Q
liftQ =  lift (TyName :: TyName "Q")

pProcess :: (DSL repr) => repr (Pid RMulti -> Pid RSing -> Process repr ())
pProcess = lam $ \qs -> lam $ \r ->
  do myPid <- self
     doMany "loop_p01" qs (lam $ \q -> send q (liftR r))
     ret tt

qProcess :: (DSL repr) => repr (Process repr ())
qProcess =
  do myPid <- self
     (r_pid :: repr (T "R" (Pid RSing))) <- recv
     send (forget r_pid) (liftQ myPid)
     ret tt
     

rProcess :: (DSL repr) => repr (Pid RMulti -> Process repr ())
rProcess = lam $ \qs ->
  do myPid <- self
     doMany "loop_r01" qs (lam $ \q -> do (q_pid :: repr (T "Q" (Pid RSing))) <- recv
                                          ret tt)
     ret tt

master :: (DSL repr) => repr (Int -> Process repr ())
master = lam $ \q_no -> -- no of q processes
  do q     <- newRMulti
     r     <- newRSing
     qs    <- spawnMany q q_no qProcess
     r_pid <- spawn r (app rProcess qs)
     app2 pProcess qs r_pid


main :: IO ()
main = checkerMain $ exec (app master arb)
