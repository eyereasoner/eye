:- op(1200, xfx, :+).

answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a, reflection_b, reflection_c])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a, reflection_b])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a, reflection_c])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_b, reflection_c])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_b])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_c])).
answer('urn:example:validGroup'([identity, rotation_120, rotation_240])).
answer('urn:example:validGroup'([identity, reflection_a])).
answer('urn:example:validGroup'([identity, reflection_b])).
answer('urn:example:validGroup'([identity, reflection_c])).
answer('urn:example:validGroup'([identity])).

step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a, reflection_b, reflection_c]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a, reflection_b]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a, reflection_c]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_a]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_b, reflection_c]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_b]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240, reflection_c]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, rotation_120, rotation_240]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, reflection_a]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, reflection_b]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity, reflection_c]), true).
step((true:+'urn:example:validGroup'(_)), 'urn:example:validGroup'([identity]), true).
