:- op(1200, xfx, :+).

answer(negative(permitted((drink, drive)))).
answer(negative(obligatory(negative(pay_taxes)))).
answer(negative(obligatory(negative(stop_at_red_light)))).
answer(negative(obligatory((drink, drive)))).
answer(negative(permitted(steal))).
answer(negative(permitted(negative(pay_taxes)))).
answer(negative(permitted(negative(stop_at_red_light)))).
answer(negative(forbidden((drink, negative(drive))))).
answer(negative(forbidden((negative(drink), drive)))).
answer(obligatory(pay_taxes)).
answer(obligatory(stop_at_red_light)).
answer(obligatory(negative((drink, drive)))).
answer(permitted((drink, negative(drive)))).
answer(permitted((negative(drink), drive))).
answer(forbidden(steal)).
answer(forbidden(negative(pay_taxes))).
answer(forbidden(negative(stop_at_red_light))).
answer(forbidden((drink, drive))).
answer(negative(obligatory(steal))).
answer(negative(forbidden(pay_taxes))).
answer(negative(forbidden(stop_at_red_light))).
answer(negative(forbidden(negative((drink, drive))))).
answer(obligatory(negative(steal))).
answer(permitted(pay_taxes)).
answer(permitted(stop_at_red_light)).
answer(permitted(negative((drink, drive)))).
answer(negative(forbidden(negative(steal)))).
answer(permitted(negative(steal))).

step((obligatory(A):+negative(permitted(B)), negation(B, A)), (negative(permitted((drink, drive))), negation((drink, drive), negative((drink, drive)))), obligatory(negative((drink, drive)))).
step((forbidden(A):+obligatory(B), negation(B, A)), (obligatory(pay_taxes), negation(pay_taxes, negative(pay_taxes))), forbidden(negative(pay_taxes))).
step((forbidden(A):+obligatory(B), negation(B, A)), (obligatory(stop_at_red_light), negation(stop_at_red_light, negative(stop_at_red_light))), forbidden(negative(stop_at_red_light))).
step((forbidden(A):+obligatory(B), negation(B, A)), (obligatory(negative((drink, drive))), negation(negative((drink, drive)), (drink, drive))), forbidden((drink, drive))).
step((negative(obligatory(A)):+obligatory(B), negation(B, A)), (obligatory(pay_taxes), negation(pay_taxes, negative(pay_taxes))), negative(obligatory(negative(pay_taxes)))).
step((negative(obligatory(A)):+obligatory(B), negation(B, A)), (obligatory(stop_at_red_light), negation(stop_at_red_light, negative(stop_at_red_light))), negative(obligatory(negative(stop_at_red_light)))).
step((negative(obligatory(A)):+obligatory(B), negation(B, A)), (obligatory(negative((drink, drive))), negation(negative((drink, drive)), (drink, drive))), negative(obligatory((drink, drive)))).
step((negative(permitted(A)):+forbidden(A)), forbidden(steal), negative(permitted(steal))).
step((negative(permitted(A)):+forbidden(A)), forbidden(negative(pay_taxes)), negative(permitted(negative(pay_taxes)))).
step((negative(permitted(A)):+forbidden(A)), forbidden(negative(stop_at_red_light)), negative(permitted(negative(stop_at_red_light)))).
step((negative(forbidden(A)):+permitted(A)), permitted((drink, negative(drive))), negative(forbidden((drink, negative(drive))))).
step((negative(forbidden(A)):+permitted(A)), permitted((negative(drink), drive)), negative(forbidden((negative(drink), drive)))).
step((true:+negative(_)), negative(permitted((drink, drive))), true).
step((true:+negative(_)), negative(obligatory(negative(pay_taxes))), true).
step((true:+negative(_)), negative(obligatory(negative(stop_at_red_light))), true).
step((true:+negative(_)), negative(obligatory((drink, drive))), true).
step((true:+negative(_)), negative(permitted(steal)), true).
step((true:+negative(_)), negative(permitted(negative(pay_taxes))), true).
step((true:+negative(_)), negative(permitted(negative(stop_at_red_light))), true).
step((true:+negative(_)), negative(forbidden((drink, negative(drive)))), true).
step((true:+negative(_)), negative(forbidden((negative(drink), drive))), true).
step((true:+obligatory(_)), obligatory(pay_taxes), true).
step((true:+obligatory(_)), obligatory(stop_at_red_light), true).
step((true:+obligatory(_)), obligatory(negative((drink, drive))), true).
step((true:+permitted(_)), permitted((drink, negative(drive))), true).
step((true:+permitted(_)), permitted((negative(drink), drive)), true).
step((true:+forbidden(_)), forbidden(steal), true).
step((true:+forbidden(_)), forbidden(negative(pay_taxes)), true).
step((true:+forbidden(_)), forbidden(negative(stop_at_red_light)), true).
step((true:+forbidden(_)), forbidden((drink, drive)), true).
step((obligatory(A):+negative(permitted(B)), negation(B, A)), (negative(permitted(steal)), negation(steal, negative(steal))), obligatory(negative(steal))).
step((permitted(A):+negative(obligatory(B)), negation(B, A)), (negative(obligatory(negative(pay_taxes))), negation(negative(pay_taxes), pay_taxes)), permitted(pay_taxes)).
step((permitted(A):+negative(obligatory(B)), negation(B, A)), (negative(obligatory(negative(stop_at_red_light))), negation(negative(stop_at_red_light), stop_at_red_light)), permitted(stop_at_red_light)).
step((permitted(A):+negative(obligatory(B)), negation(B, A)), (negative(obligatory((drink, drive))), negation((drink, drive), negative((drink, drive)))), permitted(negative((drink, drive)))).
step((negative(obligatory(A)):+obligatory(B), negation(B, A)), (obligatory(negative(steal)), negation(negative(steal), steal)), negative(obligatory(steal))).
step((negative(forbidden(A)):+permitted(A)), permitted(pay_taxes), negative(forbidden(pay_taxes))).
step((negative(forbidden(A)):+permitted(A)), permitted(stop_at_red_light), negative(forbidden(stop_at_red_light))).
step((negative(forbidden(A)):+permitted(A)), permitted(negative((drink, drive))), negative(forbidden(negative((drink, drive))))).
step((true:+negative(_)), negative(obligatory(steal)), true).
step((true:+negative(_)), negative(forbidden(pay_taxes)), true).
step((true:+negative(_)), negative(forbidden(stop_at_red_light)), true).
step((true:+negative(_)), negative(forbidden(negative((drink, drive)))), true).
step((true:+obligatory(_)), obligatory(negative(steal)), true).
step((true:+permitted(_)), permitted(pay_taxes), true).
step((true:+permitted(_)), permitted(stop_at_red_light), true).
step((true:+permitted(_)), permitted(negative((drink, drive))), true).
step((permitted(A):+negative(obligatory(B)), negation(B, A)), (negative(obligatory(steal)), negation(steal, negative(steal))), permitted(negative(steal))).
step((negative(forbidden(A)):+permitted(A)), permitted(negative(steal)), negative(forbidden(negative(steal)))).
step((true:+negative(_)), negative(forbidden(negative(steal))), true).
step((true:+permitted(_)), permitted(negative(steal)), true).
