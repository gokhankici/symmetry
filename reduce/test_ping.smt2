(push)
(declare-const p Pid)
(declare-const me Pid)
(declare-const q Pid)
(declare-const c Config)

(declare-fun ping (Pid) U)
(declare-fun pong (Pid) U)

(assert (forall ((p Pid) (q Pid))
                (=> (= (ping p) (ping q)) (= p q))))

(assert (forall ((p Pid) (q Pid))
                (=> (= (pong p) (pong q)) (= p q))))

(assert (forall ((p Pid) (q Pid))
                (not (= (ping p) (pong q)))))

;; Universe of PIDs:
(assert
 (forall ((r Pid))
         (or (= p r)
             (= q r)
             (= me r))))

;; Substitution "Instances"
(assert 
 (forall ((s Subst) (p Pid) (q Pid))
         (= (apply-subst-u s (ping p))
            (ping (select s p)))))

(assert 
 (forall ((s Subst) (p Pid) (q Pid))
         (= (apply-subst-u s (pong p))
            (pong (select s p)))))

(define-fun me_proc () Stmt 
  (seq (send p (ping me)) 
       (seq (recv (pong p)) 
            skip)))
  
(define-fun p_proc () Stmt
  (seq (bind q (seq (recv (ping q))
               (seq (send q (pong p)) skip))) skip))

(assert (not (= p q)))
(assert (not (= me p)))
(assert (not (= q me)))

(assert (= c (store (store empty (Sing me) me_proc)
                                 (Sing p)  p_proc)))

(assert
 (TC c))
(assert
 (TS me_proc))
(assert
 (TS p_proc))
(assert
 (TP (Sing p)))
(assert
 (TP (Sing me)))
(assert
 (TV me))
(assert
 (not (Rewrite c empty)))
(check-sat)
(pop)
