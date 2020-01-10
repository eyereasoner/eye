import random
random.seed(201908)

def map_coloring(map, colors):
    mc = {}
    for place in map:
        for color in colors:
            if place not in mc and not sum([1 if color == mc.get(neighbour) else 0 for neighbour in map[place]]):
                mc[place] = color
    if len(mc) == len(map):
        return mc
    else:
        l = list(map.items())
        random.shuffle(l)
        map = dict(l)
        return map_coloring(map, colors)

if __name__ == "__main__":
    europe = {
        'Czech_Republic': {'Germany', 'Poland', 'Slovakia', 'Austria'},
        'Denmark': {'Germany', 'Sweden'},
        'Slovakia': {'Czech Republic', 'Poland', 'Hungary', 'Austria'},
        'Slovenia': {'Austria', 'Italy', 'Hungary', 'Croatia'},
        'Spain': {'France', 'Portugal'},
        'Sweden': {'Finland', 'Denmark'},
        'United_Kingdom': {'Ireland', 'Netherlands', 'Belgium', 'France'},
        'Malta': {},
        'Netherlands': {'Belgium', 'Germany', 'United Kingdom'},
        'Poland': {'Germany', 'Czech Republic', 'Slovakia', 'Lithuania'},
        'Portugal': {'Spain'},
        'Romania': {'Hungary', 'Bulgaria'},
        'Estonia': {'Finland', 'Latvia', 'Lithuania'},
        'Greece': {'Bulgaria', 'Cyprus'},
        'Hungary': {'Austria', 'Slovakia', 'Romania', 'Croatia', 'Slovenia'},
        'Ireland': {'United Kingdom'},
        'Italy': {'France', 'Austria', 'Slovenia'},
        'Latvia': {'Estonia', 'Lithuania'},
        'Lithuania': {'Estonia', 'Latvia', 'Poland'},
        'Luxemburg': {'Belgium', 'France', 'Germany'},
        'Croatia': {'Slovenia', 'Hungary'},
        'Cyprus': {'Greece'},
        'Finland': {'Estonia', 'Sweden'},
        'Austria': {'Czech Republic', 'Germany', 'Hungary', 'Italy', 'Slovenia', 'Slovakia'},
        'Belgium': {'France', 'Netherlands', 'Luxemburg', 'Germany', 'United Kingdom'},
        'Bulgaria': {'Romania', 'Greece'},
        'France': {'Spain', 'Belgium', 'Luxemburg', 'Germany', 'Italy', 'United Kingdom'},
        'Germany': {'Netherlands', 'Belgium', 'Luxemburg', 'Denmark', 'France', 'Austria', 'Poland', 'Czech Republic'},
    }

    colors  = ('red', 'green', 'blue', 'yellow')

    map_coloring_europe = map_coloring(europe, colors)

    print('@prefix : <http://josd.github.io/eye/resources#>.')
    print('')
    for place, color in map_coloring_europe.items():
        print(':%s :color :%s.' % (place, color))
