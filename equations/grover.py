# See https://en.wikipedia.org/wiki/Grover%27s_algorithm

from sympy.physics.quantum.qapply import qapply
from sympy.physics.quantum.qubit import IntQubit
from sympy.physics.quantum.grover import OracleGate
from sympy.physics.quantum.grover import superposition_basis
from sympy.physics.quantum.grover import grover_iteration

if __name__ == "__main__":
    cases = [
        "qapply(grover_iteration(superposition_basis(4), OracleGate(4, lambda qubits: qubits == IntQubit(2))))"
    ]

    for lhs in cases:
        print("%s = %s" % (lhs, eval(lhs)))
    print("")
