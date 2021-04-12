# See https://en.wikipedia.org/wiki/Polygon

def polygon_area(corners):
    n = len(corners)
    area = 0.0
    for i in range(n):
        j = (i + 1) % n
        area += corners[i][0] * corners[j][1]
        area -= corners[j][0] * corners[i][1]
    area = abs(area) / 2.0
    return area

if __name__ == "__main__":
    print("polygon_area(%s) = %s" % ([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]], polygon_area([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]])))
    print("")
