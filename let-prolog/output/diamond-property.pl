:- op(1200, xfx, :+).

answer((re(b, sk_0), re(c, sk_0))).

step((re(a, b), re(a, c):+true), true, (re(a, b), re(a, c))).
step((e(A, A):+re(A, _)), re(a, b), e(a, a)).
step((e(A, A):+re(_, A)), re(a, b), e(b, b)).
step((e(A, A):+re(_, A)), re(a, c), e(c, c)).
step((re(A, B):+e(A, B)), e(a, a), re(a, a)).
step((re(A, B):+e(A, B)), e(b, b), re(b, b)).
step((re(A, B):+e(A, B)), e(c, c), re(c, c)).
step((not_re(c, A):+re(b, A)), re(b, b), not_re(c, b)).
step((not_re(b, A):+re(c, A)), re(c, c), not_re(b, c)).
step((not_e(A, B):+not_re(A, C), re(B, C)), (not_re(c, b), re(a, b)), not_e(c, a)).
step((not_e(A, B):+not_re(A, C), re(B, C)), (not_re(c, b), re(b, b)), not_e(c, b)).
step((not_e(A, B):+not_re(A, C), re(B, C)), (not_re(b, c), re(a, c)), not_e(b, a)).
step((not_e(A, B):+not_re(A, C), re(B, C)), (not_re(b, c), re(c, c)), not_e(b, c)).
step((not_e(A, B):+not_e(B, A)), not_e(c, a), not_e(a, c)).
step((not_e(A, B):+not_e(B, A)), not_e(b, a), not_e(a, b)).
step((r(A, B):+re(A, B), not_e(A, B)), (re(a, b), not_e(a, b)), r(a, b)).
step((r(A, B):+re(A, B), not_e(A, B)), (re(a, c), not_e(a, c)), r(a, c)).
step((r(A, B), r(C, B):+r(D, A), r(D, C)), (r(a, b), r(a, b)), (r(b, sk_0), r(b, sk_0))).
step((r(A, B), r(C, B):+r(D, A), r(D, C)), (r(a, b), r(a, c)), (r(b, sk_0), r(c, sk_0))).
step((re(A, B):+r(A, B)), r(b, sk_0), re(b, sk_0)).
step((re(A, B):+r(A, B)), r(c, sk_0), re(c, sk_0)).
step((r(A, B), r(C, B):+r(D, A), r(D, C)), (r(b, sk_0), r(b, sk_0)), (r(sk_0, sk_0), r(sk_0, sk_0))).
step((not_re(c, A):+re(b, A)), re(b, sk_0), not_re(c, sk_0)).
step((not_re(b, A):+re(c, A)), re(c, sk_0), not_re(b, sk_0)).
step((true:+re(b, A), re(c, A)), (re(b, sk_0), re(c, sk_0)), true).
step((e(A, A):+re(_, A)), re(b, sk_0), e(sk_0, sk_0)).
step((not_e(A, B):+not_re(A, C), re(B, C)), (not_re(c, sk_0), re(c, sk_0)), not_e(c, c)).
step((not_e(A, B):+not_re(A, C), re(B, C)), (not_re(b, sk_0), re(b, sk_0)), not_e(b, b)).
step((not_e(A, B):+e(C, A), not_re(C, B)), (e(b, b), not_re(b, sk_0)), not_e(b, sk_0)).
step((not_e(A, B):+e(C, A), not_re(C, B)), (e(c, c), not_re(c, sk_0)), not_e(c, sk_0)).
step((re(A, B):+e(A, B)), e(sk_0, sk_0), re(sk_0, sk_0)).
step((r(A, B):+re(A, B), not_e(A, B)), (re(b, b), not_e(b, b)), r(b, b)).
step((r(A, B):+re(A, B), not_e(A, B)), (re(c, c), not_e(c, c)), r(c, c)).
step((not_e(A, B):+not_e(B, A)), not_e(b, sk_0), not_e(sk_0, b)).
step((not_e(A, B):+not_e(B, A)), not_e(c, sk_0), not_e(sk_0, c)).
