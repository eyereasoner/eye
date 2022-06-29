# See https://en.wikipedia.org/wiki/Lee_algorithm
# Original code from https://stackoverflow.com/questions/45969687/python-maze-route-finding

def gen_lee(start, size, traversable):
    neighbor_offsets = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    score = 0
    lee_map = [[None for _ in range(size)] for _ in range(size)]
    node_list = [start]
    lee_map[start[0]][start[1]] = 0
    for node in node_list:
        score = lee_map[node[0]][node[1]]
        for neighbor_offset in neighbor_offsets:
            neighbor_x = node[0] + neighbor_offset[0]
            neighbor_y = node[1] + neighbor_offset[1]
            if neighbor_x < 0 or \
               neighbor_y < 0 or \
               neighbor_x >= size or \
               neighbor_y >= size:
                continue  # Skip out of map neighbors
            if not traversable[neighbor_x][neighbor_y]:
                continue  # Skip untraversable neighbors
            if lee_map[neighbor_x][neighbor_y] is None:
                node_list.append((neighbor_x, neighbor_y))
                lee_map[neighbor_x][neighbor_y] = score + 1
    return lee_map

def find_path(start, destination, lee_map):
    neighbor_offsets = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    path = [destination]
    node = destination
    score = lee_map[destination[0]][destination[1]]
    for sc in range(score):
        for neighbor_offset in neighbor_offsets:
            neighbor_x = node[0] + neighbor_offset[0]
            neighbor_y = node[1] + neighbor_offset[1]
            if lee_map[neighbor_x][neighbor_y] == score - sc - 1:
                path.append((neighbor_x, neighbor_y))
                node = (neighbor_x, neighbor_y)
                break
    return path

if __name__ == "__main__":
    traversable = [[1 for _ in range(20) ] for _ in range(20)]
    traversable[2][2] = 0
    traversable[2][3] = 0
    traversable[2][4] = 0
    traversable[2][5] = 0
    traversable[2][6] = 0
    traversable[2][7] = 0
    traversable[2][8] = 0
    traversable[6][6] = 0
    traversable[6][7] = 0
    traversable[6][8] = 0
    traversable[7][6] = 0
    traversable[7][7] = 0
    traversable[7][8] = 0

    lee_map = gen_lee((1, 1), 20, traversable)
    print('[] :python-result "lee_map_1 = %s".' % (lee_map))

    lee_path = find_path((1, 1), (9, 6), lee_map)
    print('[] :python-result "lee_path_1 = %s".' % (lee_path))
