# Original code from https://github.com/ProjectQ-Framework/ProjectQ/blob/develop/examples/grover.py

import math

from projectq import MainEngine
from projectq.ops import H, Z, X, Measure, All
from projectq.meta import Loop, Compute, Uncompute, Control


def run_grover(eng, n, oracle):
    """
    Runs Grover's algorithm on n qubit using the provided quantum oracle.

    Args:
        eng (MainEngine): Main compiler engine to run Grover on.
        n (int): Number of bits in the solution.
        oracle (function): Function accepting the engine, an n-qubit register,
            and an output qubit which is flipped by the oracle for the correct
            bit string.

    Returns:
        solution (list<int>): Solution bit-string.
    """
    x = eng.allocate_qureg(n)

    # start in uniform superposition
    All(H) | x

    # number of iterations we have to run:
    num_it = int(math.pi/4.*math.sqrt(1 << n))

    # prepare the oracle output qubit (the one that is flipped to indicate the
    # solution. start in state 1/sqrt(2) * (|0> - |1>) s.t. a bit-flip turns
    # into a (-1)-phase.
    oracle_out = eng.allocate_qubit()
    X | oracle_out
    H | oracle_out

    # run num_it iterations
    with Loop(eng, num_it):
        # oracle adds a (-1)-phase to the solution
        oracle(eng, x, oracle_out)

        # reflection across uniform superposition
        with Compute(eng):
            All(H) | x
            All(X) | x

        with Control(eng, x[0:-1]):
            Z | x[-1]

        Uncompute(eng)

    All(Measure) | x
    Measure | oracle_out

    eng.flush()
    # return result
    return [int(qubit) for qubit in x]


def alternating_bits_oracle(eng, qubits, output):
    """
    Marks the solution string 1,0,1,0,...,0,1 by flipping the output qubit,
    conditioned on qubits being equal to the alternating bit-string.

    Args:
        eng (MainEngine): Main compiler engine the algorithm is being run on.
        qubits (Qureg): n-qubit quantum register Grover search is run on.
        output (Qubit): Output qubit to flip in order to mark the solution.
    """
    with Compute(eng):
        All(X) | qubits[1::2]
    with Control(eng, qubits):
        X | output
    Uncompute(eng)

def to_n3(item):
    if type(item) == list or type(item) == tuple:
        s = '('
        for e in item:
            if s != '(':
                s += ' '
            s += repr(e)
        s += ')'
        return s
    else:
        return item

if __name__ == "__main__":
    print("@prefix grover: <http://josd.github.io/eye/reasoning/grover#>.")
    print("")
    eng = MainEngine()  # use default compiler engine
    # run Grover search to find n-bit solutions
    for n in range(1, 10):
        bits = run_grover(eng, n, alternating_bits_oracle)
        n3_bits = to_n3(bits)
        print('(%d) grover:alternating_bits_oracle %s.' % (n, n3_bits))
