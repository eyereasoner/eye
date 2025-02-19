:- op(1200, xfx, :+).

answer('<urn:knows:not>'('<urn:knows:permitted>'((drink, drive)))).
answer('<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(pay_taxes)))).
answer('<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(stop_at_red_light)))).
answer('<urn:knows:not>'('<urn:knows:obligatory>'((drink, drive)))).
answer('<urn:knows:not>'('<urn:knows:permitted>'(steal))).
answer('<urn:knows:not>'('<urn:knows:permitted>'('<urn:knows:not>'(pay_taxes)))).
answer('<urn:knows:not>'('<urn:knows:permitted>'('<urn:knows:not>'(stop_at_red_light)))).
answer('<urn:knows:not>'('<urn:knows:forbidden>'((drink, '<urn:knows:not>'(drive))))).
answer('<urn:knows:not>'('<urn:knows:forbidden>'(('<urn:knows:not>'(drink), drive)))).
answer('<urn:knows:obligatory>'(pay_taxes)).
answer('<urn:knows:obligatory>'(stop_at_red_light)).
answer('<urn:knows:obligatory>'('<urn:knows:not>'((drink, drive)))).
answer('<urn:knows:permitted>'((drink, '<urn:knows:not>'(drive)))).
answer('<urn:knows:permitted>'(('<urn:knows:not>'(drink), drive))).
answer('<urn:knows:forbidden>'(steal)).
answer('<urn:knows:forbidden>'('<urn:knows:not>'(pay_taxes))).
answer('<urn:knows:forbidden>'('<urn:knows:not>'(stop_at_red_light))).
answer('<urn:knows:forbidden>'((drink, drive))).
answer('<urn:knows:not>'('<urn:knows:obligatory>'(steal))).
answer('<urn:knows:not>'('<urn:knows:forbidden>'(pay_taxes))).
answer('<urn:knows:not>'('<urn:knows:forbidden>'(stop_at_red_light))).
answer('<urn:knows:not>'('<urn:knows:forbidden>'('<urn:knows:not>'((drink, drive))))).
answer('<urn:knows:obligatory>'('<urn:knows:not>'(steal))).
answer('<urn:knows:permitted>'(pay_taxes)).
answer('<urn:knows:permitted>'(stop_at_red_light)).
answer('<urn:knows:permitted>'('<urn:knows:not>'((drink, drive)))).
answer('<urn:knows:not>'('<urn:knows:forbidden>'('<urn:knows:not>'(steal)))).
answer('<urn:knows:permitted>'('<urn:knows:not>'(steal))).

% proof steps
step(('<urn:knows:obligatory>'(A):+'<urn:knows:not>'('<urn:knows:permitted>'(B)), negation(B, A)), ('<urn:knows:not>'('<urn:knows:permitted>'((drink, drive))), negation((drink, drive), '<urn:knows:not>'((drink, drive)))), '<urn:knows:obligatory>'('<urn:knows:not>'((drink, drive)))).
step(('<urn:knows:forbidden>'(A):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'(pay_taxes), negation(pay_taxes, '<urn:knows:not>'(pay_taxes))), '<urn:knows:forbidden>'('<urn:knows:not>'(pay_taxes))).
step(('<urn:knows:forbidden>'(A):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'(stop_at_red_light), negation(stop_at_red_light, '<urn:knows:not>'(stop_at_red_light))), '<urn:knows:forbidden>'('<urn:knows:not>'(stop_at_red_light))).
step(('<urn:knows:forbidden>'(A):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'('<urn:knows:not>'((drink, drive))), negation('<urn:knows:not>'((drink, drive)), (drink, drive))), '<urn:knows:forbidden>'((drink, drive))).
step(('<urn:knows:not>'('<urn:knows:obligatory>'(A)):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'(pay_taxes), negation(pay_taxes, '<urn:knows:not>'(pay_taxes))), '<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(pay_taxes)))).
step(('<urn:knows:not>'('<urn:knows:obligatory>'(A)):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'(stop_at_red_light), negation(stop_at_red_light, '<urn:knows:not>'(stop_at_red_light))), '<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(stop_at_red_light)))).
step(('<urn:knows:not>'('<urn:knows:obligatory>'(A)):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'('<urn:knows:not>'((drink, drive))), negation('<urn:knows:not>'((drink, drive)), (drink, drive))), '<urn:knows:not>'('<urn:knows:obligatory>'((drink, drive)))).
step(('<urn:knows:not>'('<urn:knows:permitted>'(A)):+'<urn:knows:forbidden>'(A)), '<urn:knows:forbidden>'(steal), '<urn:knows:not>'('<urn:knows:permitted>'(steal))).
step(('<urn:knows:not>'('<urn:knows:permitted>'(A)):+'<urn:knows:forbidden>'(A)), '<urn:knows:forbidden>'('<urn:knows:not>'(pay_taxes)), '<urn:knows:not>'('<urn:knows:permitted>'('<urn:knows:not>'(pay_taxes)))).
step(('<urn:knows:not>'('<urn:knows:permitted>'(A)):+'<urn:knows:forbidden>'(A)), '<urn:knows:forbidden>'('<urn:knows:not>'(stop_at_red_light)), '<urn:knows:not>'('<urn:knows:permitted>'('<urn:knows:not>'(stop_at_red_light)))).
step(('<urn:knows:not>'('<urn:knows:forbidden>'(A)):+'<urn:knows:permitted>'(A)), '<urn:knows:permitted>'((drink, '<urn:knows:not>'(drive))), '<urn:knows:not>'('<urn:knows:forbidden>'((drink, '<urn:knows:not>'(drive))))).
step(('<urn:knows:not>'('<urn:knows:forbidden>'(A)):+'<urn:knows:permitted>'(A)), '<urn:knows:permitted>'(('<urn:knows:not>'(drink), drive)), '<urn:knows:not>'('<urn:knows:forbidden>'(('<urn:knows:not>'(drink), drive)))).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:permitted>'((drink, drive))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(pay_taxes))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(stop_at_red_light))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:obligatory>'((drink, drive))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:permitted>'(steal)), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:permitted>'('<urn:knows:not>'(pay_taxes))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:permitted>'('<urn:knows:not>'(stop_at_red_light))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:forbidden>'((drink, '<urn:knows:not>'(drive)))), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:forbidden>'(('<urn:knows:not>'(drink), drive))), true).
step((true:+'<urn:knows:obligatory>'(_)), '<urn:knows:obligatory>'(pay_taxes), true).
step((true:+'<urn:knows:obligatory>'(_)), '<urn:knows:obligatory>'(stop_at_red_light), true).
step((true:+'<urn:knows:obligatory>'(_)), '<urn:knows:obligatory>'('<urn:knows:not>'((drink, drive))), true).
step((true:+'<urn:knows:permitted>'(_)), '<urn:knows:permitted>'((drink, '<urn:knows:not>'(drive))), true).
step((true:+'<urn:knows:permitted>'(_)), '<urn:knows:permitted>'(('<urn:knows:not>'(drink), drive)), true).
step((true:+'<urn:knows:forbidden>'(_)), '<urn:knows:forbidden>'(steal), true).
step((true:+'<urn:knows:forbidden>'(_)), '<urn:knows:forbidden>'('<urn:knows:not>'(pay_taxes)), true).
step((true:+'<urn:knows:forbidden>'(_)), '<urn:knows:forbidden>'('<urn:knows:not>'(stop_at_red_light)), true).
step((true:+'<urn:knows:forbidden>'(_)), '<urn:knows:forbidden>'((drink, drive)), true).
step(('<urn:knows:obligatory>'(A):+'<urn:knows:not>'('<urn:knows:permitted>'(B)), negation(B, A)), ('<urn:knows:not>'('<urn:knows:permitted>'(steal)), negation(steal, '<urn:knows:not>'(steal))), '<urn:knows:obligatory>'('<urn:knows:not>'(steal))).
step(('<urn:knows:permitted>'(A):+'<urn:knows:not>'('<urn:knows:obligatory>'(B)), negation(B, A)), ('<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(pay_taxes))), negation('<urn:knows:not>'(pay_taxes), pay_taxes)), '<urn:knows:permitted>'(pay_taxes)).
step(('<urn:knows:permitted>'(A):+'<urn:knows:not>'('<urn:knows:obligatory>'(B)), negation(B, A)), ('<urn:knows:not>'('<urn:knows:obligatory>'('<urn:knows:not>'(stop_at_red_light))), negation('<urn:knows:not>'(stop_at_red_light), stop_at_red_light)), '<urn:knows:permitted>'(stop_at_red_light)).
step(('<urn:knows:permitted>'(A):+'<urn:knows:not>'('<urn:knows:obligatory>'(B)), negation(B, A)), ('<urn:knows:not>'('<urn:knows:obligatory>'((drink, drive))), negation((drink, drive), '<urn:knows:not>'((drink, drive)))), '<urn:knows:permitted>'('<urn:knows:not>'((drink, drive)))).
step(('<urn:knows:not>'('<urn:knows:obligatory>'(A)):+'<urn:knows:obligatory>'(B), negation(B, A)), ('<urn:knows:obligatory>'('<urn:knows:not>'(steal)), negation('<urn:knows:not>'(steal), steal)), '<urn:knows:not>'('<urn:knows:obligatory>'(steal))).
step(('<urn:knows:not>'('<urn:knows:forbidden>'(A)):+'<urn:knows:permitted>'(A)), '<urn:knows:permitted>'(pay_taxes), '<urn:knows:not>'('<urn:knows:forbidden>'(pay_taxes))).
step(('<urn:knows:not>'('<urn:knows:forbidden>'(A)):+'<urn:knows:permitted>'(A)), '<urn:knows:permitted>'(stop_at_red_light), '<urn:knows:not>'('<urn:knows:forbidden>'(stop_at_red_light))).
step(('<urn:knows:not>'('<urn:knows:forbidden>'(A)):+'<urn:knows:permitted>'(A)), '<urn:knows:permitted>'('<urn:knows:not>'((drink, drive))), '<urn:knows:not>'('<urn:knows:forbidden>'('<urn:knows:not>'((drink, drive))))).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:obligatory>'(steal)), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:forbidden>'(pay_taxes)), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:forbidden>'(stop_at_red_light)), true).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:forbidden>'('<urn:knows:not>'((drink, drive)))), true).
step((true:+'<urn:knows:obligatory>'(_)), '<urn:knows:obligatory>'('<urn:knows:not>'(steal)), true).
step((true:+'<urn:knows:permitted>'(_)), '<urn:knows:permitted>'(pay_taxes), true).
step((true:+'<urn:knows:permitted>'(_)), '<urn:knows:permitted>'(stop_at_red_light), true).
step((true:+'<urn:knows:permitted>'(_)), '<urn:knows:permitted>'('<urn:knows:not>'((drink, drive))), true).
step(('<urn:knows:permitted>'(A):+'<urn:knows:not>'('<urn:knows:obligatory>'(B)), negation(B, A)), ('<urn:knows:not>'('<urn:knows:obligatory>'(steal)), negation(steal, '<urn:knows:not>'(steal))), '<urn:knows:permitted>'('<urn:knows:not>'(steal))).
step(('<urn:knows:not>'('<urn:knows:forbidden>'(A)):+'<urn:knows:permitted>'(A)), '<urn:knows:permitted>'('<urn:knows:not>'(steal)), '<urn:knows:not>'('<urn:knows:forbidden>'('<urn:knows:not>'(steal)))).
step((true:+'<urn:knows:not>'(_)), '<urn:knows:not>'('<urn:knows:forbidden>'('<urn:knows:not>'(steal))), true).
step((true:+'<urn:knows:permitted>'(_)), '<urn:knows:permitted>'('<urn:knows:not>'(steal)), true).
