# Original code from https://codereview.stackexchange.com/questions/37430/sudoku-solver-in-c

def fill(puzzle, row, col):
    if row < 9 and col < 9:
        if puzzle[row][col] != 0:
            if col+1 < 9:
                return fill(puzzle, row, col+1)
            elif row+1 < 9:
                return fill(puzzle, row+1, 0)
            else:
                return 1
        else:
            for i in range(9):
                if available(puzzle, row, col, i+1):
                    puzzle[row][col] = i+1
                    if col+1 < 9:
                        if fill(puzzle, row, col+1):
                            return 1
                        else:
                            puzzle[row][col] = 0
                    elif row+1 < 9:
                        if fill(puzzle, row+1, 0):
                            return 1
                        else:
                            puzzle[row][col] = 0
                    else:
                        return 1
        return 0
    else:
        return 1

def available(puzzle, row, col, num):
    for i in range(9):
        if puzzle[row][i] == num:
            return 0
        if puzzle[i][col] == num:
            return 0
        if puzzle[row//3*3+i%3][col//3*3+i//3] == num:
            return 0
    return 1

def to_n3_list(l):
    s = '('
    for i in l:
        if len(s) > 1:
            s += ' '
        if type(i) is list:
            s += to_n3_list(i)
        elif type(i) is tuple:
            s += to_n3_list(i)
        else:
            s += str(i)
    s += ')'
    return s

if __name__ == "__main__":
    print('@prefix : <http://josd.github.io/eye/reasoning#>.')
    print('')

    sudoku_201906 = [[8, 0, 0, 4, 0, 5, 0, 0, 0],
                     [6, 0, 4, 0, 0, 0, 0, 0, 1],
                     [0, 0, 0, 0, 1, 0, 0, 0, 2],
                     [0, 3, 7, 2, 0, 0, 0, 0, 0],
                     [0, 0, 0, 0, 5, 1, 0, 0, 9],
                     [0, 0, 0, 0, 0, 4, 0, 3, 0],
                     [9, 0, 2, 0, 0, 8, 1, 0, 7],
                     [7, 0, 0, 0, 0, 0, 0, 0, 0],
                     [0, 0, 1, 0, 0, 2, 0, 0, 0]]
    fill(sudoku_201906, 0, 0)
    print(':sudoku_201906 :sudoku %s.' % (to_n3_list(sudoku_201906)))

    sudoku_201907 = [[4, 0, 0, 0, 0, 8, 5, 0, 0],
                     [6, 0, 0, 0, 1, 5, 9, 0, 0],
                     [5, 0, 1, 0, 0, 0, 0, 0, 0],
                     [0, 7, 0, 0, 2, 4, 0, 0, 0],
                     [0, 0, 0, 0, 0, 0, 0, 9, 0],
                     [9, 0, 5, 3, 0, 6, 0, 0, 8],
                     [3, 0, 0, 0, 0, 0, 0, 8, 7],
                     [0, 0, 0, 0, 0, 0, 6, 0, 0],
                     [0, 9, 6, 0, 0, 0, 4, 0, 1]]
    fill(sudoku_201907, 0, 0)
    print(':sudoku_201907 :sudoku %s.' % (to_n3_list(sudoku_201907)))

    sudoku_201909 = [[0, 0, 0, 0, 0, 0, 0, 2, 5],
                     [5, 0, 0, 0, 7, 4, 0, 0, 0],
                     [0, 0, 0, 3, 5, 0, 0, 0, 8],
                     [0, 0, 0, 0, 0, 0, 0, 0, 0],
                     [0, 0, 9, 2, 0, 0, 1, 4, 0],
                     [0, 2, 0, 7, 0, 8, 0, 9, 0],
                     [0, 0, 0, 0, 3, 0, 0, 0, 0],
                     [0, 0, 4, 5, 0, 6, 9, 0, 0],
                     [9, 8, 0, 0, 0, 0, 0, 7, 0]]
    fill(sudoku_201909, 0, 0)
    print(':sudoku_201909 :sudoku %s.' % (to_n3_list(sudoku_201909)))

    sudoku_201910 = [[0, 0, 1, 8, 6, 9, 0, 2, 0],
                     [5, 0, 0, 0, 1, 0, 3, 0, 9],
                     [9, 0, 8, 2, 0, 5, 6, 4, 0],
                     [0, 8, 0, 6, 0, 0, 9, 3, 4],
                     [0, 0, 0, 0, 0, 3, 0, 7, 0],
                     [2, 0, 9, 0, 4, 0, 0, 0, 0],
                     [0, 9, 0, 0, 0, 0, 4, 0, 7],
                     [0, 0, 0, 0, 2, 0, 8, 0, 0],
                     [0, 0, 0, 0, 0, 0, 0, 0, 0]]
    fill(sudoku_201910, 0, 0)
    print(':sudoku_201910 :sudoku %s.' % (to_n3_list(sudoku_201910)))

    sudoku_201912 = [[0, 0, 0, 0, 5, 0, 8, 7, 0],
                     [4, 3, 0, 0, 0, 6, 0, 0, 0],
                     [1, 0, 0, 0, 3, 0, 0, 0, 4],
                     [5, 0, 0, 2, 0, 0, 3, 4, 0],
                     [0, 0, 0, 0, 0, 1, 0, 0, 0],
                     [3, 0, 7, 0, 0, 0, 0, 0, 0],
                     [7, 9, 0, 0, 0, 0, 6, 0, 0],
                     [0, 0, 0, 0, 0, 0, 0, 0, 2],
                     [0, 1, 3, 0, 6, 0, 7, 8, 0]]
    fill(sudoku_201912, 0, 0)
    print(':sudoku_201912 :sudoku %s.' % (to_n3_list(sudoku_201912)))

    sudoku_202001 = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
                     [0, 1, 0, 0, 6, 9, 7, 0, 0],
                     [6, 0, 7, 5, 0, 1, 0, 8, 3],
                     [0, 0, 0, 3, 0, 0, 0, 4, 0],
                     [0, 0, 0, 0, 8, 0, 9, 0, 0],
                     [4, 0, 0, 6, 0, 0, 0, 0, 8],
                     [1, 5, 0, 8, 0, 6, 0, 0, 0],
                     [0, 4, 0, 0, 0, 0, 1, 0, 0],
                     [2, 0, 0, 0, 7, 0, 0, 0, 0]]
    fill(sudoku_202001, 0, 0)
    print(':sudoku_202001 :sudoku %s.' % (to_n3_list(sudoku_202001)))
