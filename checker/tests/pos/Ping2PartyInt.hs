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
| p            | Q         | r           |
|--------------+-----------+-------------|
| for q in Q:  | send p q  | for q in Q: |
|   id <- recv | send r q  |   x <- recv |
|   send id r  | id <-recv |             |
-}

-- type R = T "R" (Pid RSing)
-- liftR :: (DSL repr) => repr (Pid RSing) -> repr R
-- liftR =  lift (TyName :: TyName "R")

-- type Q = T "Q" (Pid RSing)
-- liftQ :: (DSL repr) => repr (Pid RSing) -> repr Q
-- liftQ =  lift (TyName :: TyName "Q")

pProcess :: (DSL repr) => repr (Pid RMulti -> Process repr ())
pProcess = lam $ \qs ->
  do myPid <- self
     doMany "loop_p01" qs (lam $ \_ -> do (id :: repr (Pid RSing)) <- recv
                                          send id myPid)
     ret tt

qProcess :: (DSL repr) => repr (Pid RSing -> Pid RSing -> Process repr ())
qProcess = lam $ \p_pid -> lam $ \r_pid ->
  do myPid <- self
     send p_pid myPid
     send r_pid myPid
     (id :: repr (Pid RSing)) <- recv
     ret tt
     
rProcess :: (DSL repr) => repr (Int -> Process repr ())
rProcess = lam $ \q_no ->
  do myPid <- self
     doN "loop_r01" q_no (lam $ \_ ->
                             do (x :: repr (Pid RSing)) <- recv
                                ret tt)
     ret tt

master :: (DSL repr) => repr (Int -> Process repr ())
master = lam $ \q_no -> -- no of q processes
  do q     <- newRMulti
     r     <- newRSing
     p_pid <- self
     r_pid <- spawn r (q_no |> rProcess)
     qs    <- spawnMany q q_no (app2 qProcess p_pid r_pid)
     qs |> pProcess


main :: IO ()
main = checkerMain $ exec (app master arb)
