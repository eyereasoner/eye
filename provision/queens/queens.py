# Original code from https://stackoverflow.com/questions/4818201/solving-the-n-queen-puzzle/4819050#4819050

def nqueen(B, N, row, col):
    if(row >= N):
        return 1
    if(col >= N):
        print("%d :solve %s." % (N, '(' + ' '.join([str(i) for i in B]) + ')'))
        return 0
    B[col] = row
    if(0==(Attacked_H[row] + Attacked_DU[row+col] + Attacked_DD[row-col])):
        [Attacked_H[row], Attacked_DU[row+col], Attacked_DD[row-col]] = [1, 1, 1]
        nqueen(B, N, 0, col + 1)
        [Attacked_H[row], Attacked_DU[row+col], Attacked_DD[row-col]] = [0, 0, 0]
    nqueen(B, N, row + 1, col)

if __name__ == "__main__":
    board_size = 11
    Attacked_H  = {i:0 for i in range(0, board_size)}
    Attacked_DU = {i:0 for i in range(0, board_size*2)}
    Attacked_DD = {i:0 for i in range(-board_size, board_size)}

    print('PREFIX : <http://josd.github.io/eye/provision/queens#>')
    print('')
    nqueen(list(range(0, board_size)), board_size, 0, 0)
