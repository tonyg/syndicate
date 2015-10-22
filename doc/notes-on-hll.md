## Syntax

Just a sketch, at the moment. The connection between Racket `expr`s
and `I` needs to be clarified, among many other things.

	Instantaneous actions, I := (actor Idef ...)
							    (state [O ...] [E I ...] ...)
								(background I ...)
								(assert! P)
								(retract! P)
								(send! P)
								(flush!)              ;; ???
								(begin I ...)         ;; and, generally, expr?
	  Actor-level defs, Idef := I
								(define id expr)
								(define (id id ...) expr ...)
		  Ongoing actions, O := (on E I ...)
								(once E I ...)
								(assert P)
								(track [x Agg] I ...)
								(begin O ...)         ;; ??? expr?
			Predicates, Pred := (not Pred)
								(exists P Pred)
								(forall P Pred)
								expr
				   Events, E := (asserted P)
								(retracted P)
								(message P)
								(rising-edge Pred)
								(falling-edge Pred)
             Aggregates, Agg := (count expr P)
			                    (set expr P)
								(hash k_expr v_expr P)
								(project P)
								(single-value expr P)
								(single-value expr P #:default def_expr)
								Pred
                 Patterns, P := ... ;; uses $var as binder

	(define-syntax-rule (until E O ...)
	  (state [O ...] [E]))

    (define-syntax-rule (forever O ...)
	  (until (rising-edge #f) O ...))
