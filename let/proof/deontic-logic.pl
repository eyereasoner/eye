negative(permitted((drink,drive))).
obligatory(pay_taxes).
obligatory(stop_at_red_light).
permitted((drink,negative(drive))).
permitted((negative(drink),drive)).
forbidden(steal).
negative(obligatory(negative(pay_taxes))).
negative(obligatory(negative(stop_at_red_light))).
negative(obligatory((drink,drive))).
negative(permitted(steal)).
negative(permitted(negative(pay_taxes))).
negative(permitted(negative(stop_at_red_light))).
negative(forbidden((drink,negative(drive)))).
negative(forbidden((negative(drink),drive))).
obligatory(negative((drink,drive))).
forbidden(negative(pay_taxes)).
forbidden(negative(stop_at_red_light)).
forbidden((drink,drive)).
negative(obligatory(steal)).
negative(forbidden(pay_taxes)).
negative(forbidden(stop_at_red_light)).
negative(forbidden(negative((drink,drive)))).
obligatory(negative(steal)).
permitted(pay_taxes).
permitted(stop_at_red_light).
permitted(negative((drink,drive))).
negative(forbidden(negative(steal))).
permitted(negative(steal)).

step(rule(negative(A), answer(negative(A))), negative(permitted((drink, drive))), answer(negative(permitted((drink, drive))))).
step(rule(obligatory(A), answer(obligatory(A))), obligatory(pay_taxes), answer(obligatory(pay_taxes))).
step(rule(obligatory(A), answer(obligatory(A))), obligatory(stop_at_red_light), answer(obligatory(stop_at_red_light))).
step(rule(permitted(A), answer(permitted(A))), permitted((drink, negative(drive))), answer(permitted((drink, negative(drive))))).
step(rule(permitted(A), answer(permitted(A))), permitted((negative(drink), drive)), answer(permitted((negative(drink), drive)))).
step(rule(forbidden(A), answer(forbidden(A))), forbidden(steal), answer(forbidden(steal))).
step(rule((negative(permitted(A)), negation(A, B)), obligatory(B)), (negative(permitted((drink, drive))), negation((drink, drive), negative((drink, drive)))), obligatory(negative((drink, drive)))).
step(rule((obligatory(A), negation(A, B)), forbidden(B)), (obligatory(pay_taxes), negation(pay_taxes, negative(pay_taxes))), forbidden(negative(pay_taxes))).
step(rule((obligatory(A), negation(A, B)), forbidden(B)), (obligatory(stop_at_red_light), negation(stop_at_red_light, negative(stop_at_red_light))), forbidden(negative(stop_at_red_light))).
step(rule((obligatory(A), negation(A, B)), forbidden(B)), (obligatory(negative((drink, drive))), negation(negative((drink, drive)), (drink, drive))), forbidden((drink, drive))).
step(rule((obligatory(A), negation(A, B)), negative(obligatory(B))), (obligatory(pay_taxes), negation(pay_taxes, negative(pay_taxes))), negative(obligatory(negative(pay_taxes)))).
step(rule((obligatory(A), negation(A, B)), negative(obligatory(B))), (obligatory(stop_at_red_light), negation(stop_at_red_light, negative(stop_at_red_light))), negative(obligatory(negative(stop_at_red_light)))).
step(rule((obligatory(A), negation(A, B)), negative(obligatory(B))), (obligatory(negative((drink, drive))), negation(negative((drink, drive)), (drink, drive))), negative(obligatory((drink, drive)))).
step(rule(forbidden(A), negative(permitted(A))), forbidden(steal), negative(permitted(steal))).
step(rule(forbidden(A), negative(permitted(A))), forbidden(negative(pay_taxes)), negative(permitted(negative(pay_taxes)))).
step(rule(forbidden(A), negative(permitted(A))), forbidden(negative(stop_at_red_light)), negative(permitted(negative(stop_at_red_light)))).
step(rule(permitted(A), negative(forbidden(A))), permitted((drink, negative(drive))), negative(forbidden((drink, negative(drive))))).
step(rule(permitted(A), negative(forbidden(A))), permitted((negative(drink), drive)), negative(forbidden((negative(drink), drive)))).
step(rule(negative(A), answer(negative(A))), negative(obligatory(negative(pay_taxes))), answer(negative(obligatory(negative(pay_taxes))))).
step(rule(negative(A), answer(negative(A))), negative(obligatory(negative(stop_at_red_light))), answer(negative(obligatory(negative(stop_at_red_light))))).
step(rule(negative(A), answer(negative(A))), negative(obligatory((drink, drive))), answer(negative(obligatory((drink, drive))))).
step(rule(negative(A), answer(negative(A))), negative(permitted(steal)), answer(negative(permitted(steal)))).
step(rule(negative(A), answer(negative(A))), negative(permitted(negative(pay_taxes))), answer(negative(permitted(negative(pay_taxes))))).
step(rule(negative(A), answer(negative(A))), negative(permitted(negative(stop_at_red_light))), answer(negative(permitted(negative(stop_at_red_light))))).
step(rule(negative(A), answer(negative(A))), negative(forbidden((drink, negative(drive)))), answer(negative(forbidden((drink, negative(drive)))))).
step(rule(negative(A), answer(negative(A))), negative(forbidden((negative(drink), drive))), answer(negative(forbidden((negative(drink), drive))))).
step(rule(obligatory(A), answer(obligatory(A))), obligatory(negative((drink, drive))), answer(obligatory(negative((drink, drive))))).
step(rule(forbidden(A), answer(forbidden(A))), forbidden(negative(pay_taxes)), answer(forbidden(negative(pay_taxes)))).
step(rule(forbidden(A), answer(forbidden(A))), forbidden(negative(stop_at_red_light)), answer(forbidden(negative(stop_at_red_light)))).
step(rule(forbidden(A), answer(forbidden(A))), forbidden((drink, drive)), answer(forbidden((drink, drive)))).
step(rule((negative(permitted(A)), negation(A, B)), obligatory(B)), (negative(permitted(steal)), negation(steal, negative(steal))), obligatory(negative(steal))).
step(rule((negative(obligatory(A)), negation(A, B)), permitted(B)), (negative(obligatory(negative(pay_taxes))), negation(negative(pay_taxes), pay_taxes)), permitted(pay_taxes)).
step(rule((negative(obligatory(A)), negation(A, B)), permitted(B)), (negative(obligatory(negative(stop_at_red_light))), negation(negative(stop_at_red_light), stop_at_red_light)), permitted(stop_at_red_light)).
step(rule((negative(obligatory(A)), negation(A, B)), permitted(B)), (negative(obligatory((drink, drive))), negation((drink, drive), negative((drink, drive)))), permitted(negative((drink, drive)))).
step(rule((obligatory(A), negation(A, B)), negative(obligatory(B))), (obligatory(negative(steal)), negation(negative(steal), steal)), negative(obligatory(steal))).
step(rule(permitted(A), negative(forbidden(A))), permitted(pay_taxes), negative(forbidden(pay_taxes))).
step(rule(permitted(A), negative(forbidden(A))), permitted(stop_at_red_light), negative(forbidden(stop_at_red_light))).
step(rule(permitted(A), negative(forbidden(A))), permitted(negative((drink, drive))), negative(forbidden(negative((drink, drive))))).
step(rule(negative(A), answer(negative(A))), negative(obligatory(steal)), answer(negative(obligatory(steal)))).
step(rule(negative(A), answer(negative(A))), negative(forbidden(pay_taxes)), answer(negative(forbidden(pay_taxes)))).
step(rule(negative(A), answer(negative(A))), negative(forbidden(stop_at_red_light)), answer(negative(forbidden(stop_at_red_light)))).
step(rule(negative(A), answer(negative(A))), negative(forbidden(negative((drink, drive)))), answer(negative(forbidden(negative((drink, drive)))))).
step(rule(obligatory(A), answer(obligatory(A))), obligatory(negative(steal)), answer(obligatory(negative(steal)))).
step(rule(permitted(A), answer(permitted(A))), permitted(pay_taxes), answer(permitted(pay_taxes))).
step(rule(permitted(A), answer(permitted(A))), permitted(stop_at_red_light), answer(permitted(stop_at_red_light))).
step(rule(permitted(A), answer(permitted(A))), permitted(negative((drink, drive))), answer(permitted(negative((drink, drive))))).
step(rule((negative(obligatory(A)), negation(A, B)), permitted(B)), (negative(obligatory(steal)), negation(steal, negative(steal))), permitted(negative(steal))).
step(rule(permitted(A), negative(forbidden(A))), permitted(negative(steal)), negative(forbidden(negative(steal)))).
step(rule(negative(A), answer(negative(A))), negative(forbidden(negative(steal))), answer(negative(forbidden(negative(steal))))).
step(rule(permitted(A), answer(permitted(A))), permitted(negative(steal)), answer(permitted(negative(steal)))).
