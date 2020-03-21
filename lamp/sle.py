# See https://en.wikipedia.org/wiki/System_of_linear_equations

import numpy as np

if __name__ == "__main__":
    print("# main of sle.py")

    A = np.array([[1, 1, 0, 0], [0, 0, 1, 1], [0.07, 0, -0.93, 0], [0, -0.89, 0, 0.11]])
    B = np.array([856, 1308, 0, 0])
    X = np.linalg.inv(A).dot(B)
    print('sle_solution_1 = %s' % [int(round(x)) for x in X])
    print("")
