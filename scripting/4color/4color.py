def map_coloring(map, colors):
    mc = {}
    for place in map:
        for color in colors:
            if place not in mc and not sum([1 if color == mc.get(neighbour) else 0 for neighbour in map[place]]):
                mc[place] = color
    return mc

if __name__ == "__main__":
    map = {
        'Austria': {'Czech Republic', 'Germany', 'Hungary', 'Italy', 'Slovenia', 'Slovakia'},
        'Belgium': {'France', 'Netherlands', 'Luxemburg', 'Germany', 'United Kingdom'},
        'Bulgaria': {'Romania', 'Greece'},
        'Croatia': {'Slovenia', 'Hungary'},
        'Cyprus': {'Greece'},
        'Czech Republic': {'Germany', 'Poland', 'Slovakia', 'Austria'},
        'Denmark': {'Germany', 'Sweden'},
        'Estonia': {'Finland', 'Latvia', 'Lithuania'},
        'Finland': {'Estonia', 'Sweden'},
        'France': {'Spain', 'Belgium', 'Luxemburg', 'Germany', 'Italy', 'United Kingdom'},
        'Germany': {'Netherlands', 'Belgium', 'Luxemburg', 'Denmark', 'France', 'Austria', 'Poland', 'Czech Republic'},
        'Greece': {'Bulgaria', 'Cyprus'},
        'Hungary': {'Austria', 'Slovakia', 'Romania', 'Croatia', 'Slovenia'},
        'Ireland': {'United Kingdom'},
        'Italy': {'France', 'Austria', 'Slovenia'},
        'Latvia': {'Estonia', 'Lithuania'},
        'Lithuania': {'Estonia', 'Latvia', 'Poland'},
        'Luxemburg': {'Belgium', 'France', 'Germany'},
        'Malta': {},
        'Netherlands': {'Belgium', 'Germany', 'United Kingdom'},
        'Poland': {'Germany', 'Czech Republic', 'Slovakia', 'Lithuania'},
        'Portugal': {'Spain'},
        'Romania': {'Hungary', 'Bulgaria'},
        'Slovakia': {'Czech Republic', 'Poland', 'Hungary', 'Austria'},
        'Slovenia': {'Austria', 'Italy', 'Hungary', 'Croatia'},
        'Spain': {'France', 'Portugal'},
        'Sweden': {'Finland', 'Denmark'},
        'United Kingdom': {'Ireland', 'Netherlands', 'Belgium', 'France'},
    }

    colors  = ('red', 'green', 'blue', 'yellow')

    print('PREFIX : <http://josd.github.io/eye/scripting/4color#>')
    print('')
    for place, color in map_coloring(map, colors).items():
        print(':%s :color :%s.' % (place.replace(' ', '_'), color))
