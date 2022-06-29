# See https://en.wikipedia.org/wiki/Grover%27s_algorithm

from sympy.physics.quantum.qapply import qapply
from sympy.physics.quantum.qubit import IntQubit
from sympy.physics.quantum.grover import OracleGate, superposition_basis, grover_iteration

if __name__ == "__main__":
    cases = [
        "qapply(OracleGate(2, lambda qubits: qubits == IntQubit(2))*IntQubit(2))",
        "qapply(OracleGate(2, lambda qubits: qubits == IntQubit(2))*IntQubit(3))",
        "qapply(grover_iteration(superposition_basis(2), OracleGate(2, lambda qubits: qubits == IntQubit(2))))",
        "qapply(grover_iteration(superposition_basis(4), OracleGate(4, lambda qubits: qubits == IntQubit(2))))"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
