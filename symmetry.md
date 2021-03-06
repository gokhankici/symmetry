# Communicating Symmetric Processes

## Process description language

Taken directly from the Coq [source](rewrite/ProcessRewrite.v):

~~~~{.coq}
Parameter T : Type.

Definition M := (T * PidClass).

Inductive Stmt : Type :=
(* ε *)
| s_skip  : Stmt

(* send(p, t) *)
| s_send  : PidClass -> M -> Stmt

(* recv(t) *)
| s_recv  : M -> Stmt

(* s1; s2 *)
| s_seq   : Stmt -> Stmt -> Stmt

(* { recv(t1); s1... , recv(t2); s2... } *)
| s_recv_l: list (M * Stmt) -> Stmt

(* ∀x. recv(t) *)
| s_recv_x: (T * Var) -> Stmt -> Stmt

(* foreach (x ∈ xs) *)
| s_iter  : Var -> Stmt -> Stmt

(* μX. s *)
| s_loop  : MuVar -> Stmt -> Stmt

(* X *)
| s_var   : MuVar -> Stmt

(* These statements below never appear in "original" programs *)
| s_loop_body : MuVar -> Stmt -> Stmt
| s_loop_end  : MuVar -> Stmt -> Stmt.

(* p ▹ s *)
Definition Process := (PidClass * Stmt).

(* p ▹ s | q ▹ r | ... *)
Definition Config := list Process.
~~~~

The three receive forms are:

- `s_recv`: normal message reception (value of type `T` + possible a Pid)

- `s_recv_l`: external choice

- `s_recv_x`: receive a Pid. This corresponds to "∀x. recv(x)", for example

## Examples

The programs we wish to consider live [here](examples), and are
mostly annotated with notes corresponding to plausible process
descriptions that should match up with the above language.

## Proposed basic API

~~~~{.haskell}

data Pid = ...
data Process a = ...

-- | Execute a process
runProcess :: Process a -> IO a

-- | Block until a value of type a is available, and return it
recv :: Process a

-- | External choice between two processes
offer :: Process a -> Process b -> Process ()

-- probably not plausible, but ideal:
match :: (a -> Process b) -> Match
recvWait :: [Match] -> Process b

-- | Send a message of type a to p
send :: Pid -> a -> Process ()

-- | Sequence two processes
seq :: Process a -> (a -> Process b) -> Process b

-- | Spawn a process
spawn :: Process a -> Process Pid

-- | Loop forever
forever :: Process a -> Process ()

-- | Iterate
for :: [a] -> (a -> Process b) -> Process [b]
~~~~
