travel_table = {
    'road': {'hill': 'hill',
             'w': 'hill',
             'u': 'hill',
             'enter': 'building',
             'build': 'building',
             'in': 'building',
             'e': 'building',
             'downs': 'valley.',
             'gully': 'valley.',
             'strea': 'valley.',
             's': 'valley.',
             'd': 'valley.',
             'fores': 'forrest_valley',
             'n': 'forrest_valley',
             'depre': 'grate_outside'},
    'hill': {'road': 'road',
             'build': 'road',
             'forwa': 'road',
             'e': 'road',
             'd': 'road',
             'fores': 'forrest_valley',
             'n': 'forrest_valley',
             's': 'forrest_valley'},
    'building': {'enter': 'road',
                 'out': 'road',
                 'outdo': 'road',
                 'w': 'road',
                 'xyzzy': 'debris_room',
                 'plugh': 'y2',
                 'downs': 'stream_pipe',
                 'strea': 'stream_pipe'},
    'valley.': {'upstr': 'road',
                'build': 'road',
                'n': 'road',
                'fores': 'forrest_valley',
                'e': 'forrest_valley',
                'w': 'forrest_valley',
                'u': 'forrest_valley',
                'downs': 'slit',
                's': 'slit', 'd': 'slit',
                'depre': 'grate_outside'},
    'forrest_valley': {'valle': 'valley.',
                       'e': 'valley.',
                       'd': 'valley.',
                       'fores': ('forrest_valley', ['PROBABILITY', 50]),
                       'forwa': ('forrest_valley', ('PROBABILITY', 50)),
                       'n': ('forrest_valley', ('PROBABILITY', 50)),
                       'default': 'forrest_road',
                       'w': 'forrest_valley',
                       's': 'forrest_valley'},
    'forrest_road': {'hill': 'road',
                     'n': 'road',
                     'valle': 'valley.',
                     'e': 'valley.',
                     'w': 'valley.',
                     'd': 'valley.',
                     'fores': 'forrest_valley',
                     's': 'forrest_valley'},
    'slit': {'build': 'road',
             'upstr': 'valley.',
             'n': 'valley.',
             'fores': 'forrest_valley',
             'e': 'forrest_valley',
             'w': 'forrest_valley',
             'downs': 'grate_outside',
             'rock': 'grate_outside',
             'bed': 'grate_outside',
             's': 'grate_outside',
             'slit': (('MESSAGE', 95), None, None),
             'strea': (('MESSAGE', 95), None, None),
             'd': (('MESSAGE', 95), None, None)},
    'grate_outside': {'fores': 'forrest_valley',
                      'e': 'forrest_valley',
                      'w': 'forrest_valley',
                      's': 'forrest_valley',
                      'build': 'road',
                      'upstr': 'slit',
                      'gully': 'slit',
                      'n': 'slit',
                      'enter': ('grate_below', ('PROPERTY (must not be)', 'grate', 0)),
                      'in': ('grate_below', ('PROPERTY (must not be)', 'grate', 0)),
                      'd': ('grate_below', ('PROPERTY (must not be)', 'grate', 0)),
                      'default': (('MESSAGE', 93), None, None)},
    'grate_below': {'out': ('grate_outside', ('PROPERTY (must not be)', 'grate', 0)),
                    'u': ('grate_outside', ('PROPERTY (must not be)', 'grate', 0)),
                    'default': (('MESSAGE', 93), None, None),
                    'crawl': 'cobble_crawl',
                    'cobbl': 'cobble_crawl',
                    'in': 'cobble_crawl',
                    'w': 'cobble_crawl',
                    'pit': 'pit',
                    'debri': 'debris_room'},
    'cobble_crawl': {'out': 'grate_below',
                     'surfa': 'grate_below',
                     'null': 'grate_below',
                     'e': 'grate_below',
                     'in': 'debris_room',
                     'dark': 'debris_room',
                     'w': 'debris_room',
                     'debri': 'debris_room',
                     'pit': 'pit'},
    'debris_room': {'depre': ('grate_outside', ('PROPERTY (must not be)', 'grate', 0)),
                    'entra': 'grate_below',
                    'crawl': 'cobble_crawl',
                    'cobbl': 'cobble_crawl',
                    'passa': 'cobble_crawl',
                    'low': 'cobble_crawl',
                    'e': 'cobble_crawl',
                    'canyo': 'awkward_canyon',
                    'in': 'awkward_canyon',
                    'u': 'awkward_canyon',
                    'w': 'awkward_canyon',
                    'xyzzy': 'building',
                    'pit': 'pit'},
    'awkward_canyon': {'depre': ('grate_outside', ('PROPERTY (must not be)', 'grate', 0)),
                       'entra': 'grate_below',
                       'd': 'debris_room',
                       'e': 'debris_room',
                       'debri': 'debris_room',
                       'in': 'bird_chamber',
                       'u': 'bird_chamber',
                       'w': 'bird_chamber',
                       'pit': 'pit'},
    'bird_chamber': {'depre': ('grate_outside', ('PROPERTY (must not be)', 'grate', 0)),
                     'entra': 'grate_below',
                     'debri': 'debris_room',
                     'canyo': 'awkward_canyon',
                     'e': 'awkward_canyon',
                     'passa': 'pit',
                     'pit': 'pit',
                     'w': 'pit'},
    'pit': {'depre': ('grate_outside', ('PROPERTY (must not be)', 'grate', 0)),
            'entra': 'grate_below',
            'debri': 'debris_room',
            'passa': 'bird_chamber',
            'e': 'bird_chamber',
            'd': ('broken_neck', ('REQUIRED_OBJECT', 'nugget')),
            'pit': ('broken_neck', ('REQUIRED_OBJECT', 'nugget')),
            'steps': ('broken_neck', ('REQUIRED_OBJECT', 'nugget')),
            'default': 'hall_of_mists_east',
            'crack': 'crack',
            'w': 'crack'},
    'hall_of_mists_east': {'left': 'nugget_room',
                           's': 'nugget_room',
                           'forwa': 'fissure_east',
                           'hall': 'fissure_east',
                           'w': 'fissure_east',
                           'stair': 'hall_of_mountain_king',
                           'd': 'hall_of_mountain_king',
                           'n': 'hall_of_mountain_king',
                           'u': ('unclimable', ('REQUIRED_OBJECT', 'nugget')),
                           'pit': ('unclimable', ('REQUIRED_OBJECT', 'nugget')),
                           'steps': ('unclimable', ('REQUIRED_OBJECT', 'nugget')),
                           'dome': ('unclimable', ('REQUIRED_OBJECT', 'nugget')),
                           'passa': ('unclimable', ('REQUIRED_OBJECT', 'nugget')),
                           'e': ('unclimable', ('REQUIRED_OBJECT', 'nugget')),
                           'default': 'pit',
                           'y2': 'jumble_of_rocks'},
    'crack': {'e': 'pit'},
    'fissure_east': {'hall': 'hall_of_mists_east',
                     'e': 'hall_of_mists_east',
                     'jump': (('MESSAGE', 96), None,
                              ('PROPERTY (must not be)', 'fissure', 0)),
                     'forwa': ('didnt_makeit', ('PROPERTY (must not be)', 'fissure', 1)),
                     'over': (('MESSAGE', 97), None,
                              ('PROPERTY (must not be)', 'fissure', 1)),
                     'acros': (('MESSAGE', 97), None,
                               ('PROPERTY (must not be)', 'fissure', 1)),
                     'w': (('MESSAGE', 97), None,
                           ('PROPERTY (must not be)', 'fissure', 1)),
                     'cross': (('MESSAGE', 97), None,
                               ('PROPERTY (must not be)', 'fissure', 1)),
                     'default': 'fissure_west'},
    'nugget_room': {'hall': 'hall_of_mists_east',
                    'out': 'hall_of_mists_east',
                    'n': 'hall_of_mists_east'},
    'hall_of_mountain_king': {'stair': 'hall_of_mists_east',
                              'u': 'hall_of_mists_east',
                              'e': 'hall_of_mists_east',
                              'n': ('hole_in_floor', ('PROPERTY (must not be)', 'snake', 0)),
                              'left': ('hole_in_floor', ('PROPERTY (must not be)', 'snake', 0)),
                              's': ('south-side_chamber', ('PROPERTY (must not be)', 'snake', 0)),
                              'right': ('south-side_chamber', ('PROPERTY (must not be)', 'snake', 0)),
                              'w': ('west-side_chamber', ('PROPERTY (must not be)', 'snake', 0)),
                              'forwa': ('west-side_chamber', ('PROPERTY (must not be)', 'snake', 0)),
                              'default': ('snake', ('REQUIRED_OBJECT_IN_ROOM', 'snake')),
                              'sw': ('secret_canyon', ('PROBABILITY', 35)),
                              'secre': 'secret_canyon'},
    'broken_neck': {'road': (0, None)},
    'didnt_makeit': {'road': (0, None)},
    'unclimable': {'road': 'hall_of_mists_east'},
    'two_pit_west': {'e': 'twopit_east',
                     'acros': 'twopit_east',
                     'w': 'slab',
                     'slab': 'slab',
                     'd': 'pit_west',
                     'pit': 'pit_west',
                     'hole': (('MESSAGE', 148), None, None)},
    'pit_east': {'u': 'twopit_east',
                 'out': 'twopit_east'},
    'pit_west': {'u': 'two_pit_west',
                 'out': 'two_pit_west',
                 'climb': (31,),
                 'default': 'plant_top'},
    'plant_top': {'road': 'narrow_corridor'},
    'fissure_west': {
        'jump': (('MESSAGE', 96), None, ('PROPERTY (must not be)', 'fissure', 0)),
        'forwa': ('didnt_makeit', ('PROPERTY (must not be)', 'fissure', 1)),
        'over': (('MESSAGE', 97), None, ('PROPERTY (must not be)', 'fissure', 1)),
        'acros': (('MESSAGE', 97), None, ('PROPERTY (must not be)', 'fissure', 1)),
        'e': (('MESSAGE', 97), None, ('PROPERTY (must not be)', 'fissure', 1)),
        'cross': (('MESSAGE', 97), None, ('PROPERTY (must not be)', 'fissure', 1)),
        'default': 'fissure_east',
        'n': 'very_low_passage', 'w': 'hall_of_mists_west'},
    'hole_in_floor': {'hall': 'hall_of_mountain_king',
                      'out': 'hall_of_mountain_king',
                      's': 'hall_of_mountain_king',
                      'n': 'y2',
                      'y2': 'y2',
                      'd': 'dirty_passage',
                      'hole': 'dirty_passage'},
    'south-side_chamber': {'hall': 'hall_of_mountain_king',
                           'out': 'hall_of_mountain_king',
                           'n': 'hall_of_mountain_king'},
    'west-side_chamber': {'hall': 'hall_of_mountain_king',
                          'out': 'hall_of_mountain_king',
                          'e': 'hall_of_mountain_king',
                          'w': 'cross_over', 'u': 'cross_over'},
    '>$<': {'road': ('nothing_to_climb', ('PROPERTY (must not be)', 'plant', 2)),
            'default': 'climbed_up'},
    'snake': {'road': 'hall_of_mountain_king'},
    'y2': {'plugh': 'building',
           's': 'hole_in_floor',
           'e': 'jumble_of_rocks',
           'wall': 'jumble_of_rocks',
           'broke': 'jumble_of_rocks',
           'w': 'window_over_pit',
           'plove': (159302, 'goto', 2, ('REQUIRED_OBJECT', 'emerald')),
           'default': 'plover'},
    'jumble_of_rocks': {'d': 'y2',
                        'y2': 'y2',
                        'u': 'hall_of_mists_east'},
    'window_over_pit': {'e': 'y2',
                        'y2': 'y2',
                        'jump': 'broken_neck'},
    'dirty_passage': {'e': 'brink_of_pit',
                      'crawl': 'brink_of_pit',
                      'u': 'hole_in_floor',
                      'hole': 'hole_in_floor',
                      'w': 'dusty_rock',
                      'bedqu': 'bedquilt'},
    'brink_of_pit': {'w': 'dirty_passage',
                     'crawl': 'dirty_passage',
                     'd': 'bottom_of_pit',
                     'pit': 'bottom_of_pit',
                     'climb': 'bottom_of_pit'},
    'bottom_of_pit': {'climb': 'brink_of_pit',
                      'u': 'brink_of_pit',
                      'out': 'brink_of_pit',
                      'slit': (('MESSAGE', 95), None, None),
                      'strea': (('MESSAGE', 95), None, None),
                      'd': (('MESSAGE', 95), None, None),
                      'upstr': (('MESSAGE', 95), None, None),
                      'downs': (('MESSAGE', 95), None, None)},
    'dusty_rock': {'e': 'dirty_passage',
                   'passa': 'dirty_passage',
                   'd': 'complex_junction',
                   'hole': 'complex_junction',
                   'floor': 'complex_junction',
                   'bedqu': 'bedquilt'},
    'very_low_passage': {'road': 'hall_of_mists_west'},
    'hall_of_mists_west': {'s': 'twisty_maze_1',
                           'u': 'twisty_maze_1',
                           'passa': 'twisty_maze_1',
                           'climb': 'twisty_maze_1',
                           'e': 'fissure_west',
                           'n': 'crawled_through',
                           'w': 'long_hall_east',
                           'crawl': 'long_hall_east'},
    'twisty_maze_1': {'u': 'hall_of_mists_west',
                      'n': 'twisty_maze_1',
                      'e': 'twisty_maze_2',
                      's': 'twisty_maze_4',
                      'w': 'maze_1'},
    'twisty_maze_2': {'w': 'twisty_maze_1',
                      's': 'twisty_maze_3',
                      'e': 'twisty_maze_4'},
    'twisty_maze_3': {'e': 'twisty_maze_2',
                      'd': 'twisty_maze_7',
                      's': 'twisty_maze_9',
                      'n': 'maze_3'},
    'twisty_maze_4': {'w': 'twisty_maze_1',
                      'n': 'twisty_maze_2',
                      'e': 'twisty_maze_5',
                      's': 'twisty_maze_6',
                      'u': 'maze_8',
                      'd': 'maze_8'},
    'twisty_maze_5': {'w': 'twisty_maze_4', 'out': 'twisty_maze_4'},
    'twisty_maze_6': {'e': 'twisty_maze_4', 'out': 'twisty_maze_4'},
    'twisty_maze_7': {'u': 'twisty_maze_3', 'out': 'twisty_maze_3'},
    'twisty_maze_8': {'e': 'twisty_maze_9', 'w': 'twisty_maze_10'},
    'twisty_maze_9': {'e': 'twisty_maze_3', 'w': 'twisty_maze_8',
                      'd': 'twisty_maze_10', 's': 'twisty_maze_11'},
    'twisty_maze_10': {'w': 'twisty_maze_8', 'u': 'twisty_maze_9',
                       'e': 'twisty_maze_11', 's': 'twisty_maze_12'},
    'twisty_maze_11': {'w': 'twisty_maze_9', 'e': 'twisty_maze_10',
                       's': 'twisty_maze_11', 'u': 'twisty_maze_12',
                       'n': 'twisty_maze_14', 'd': 'maze_7'},
    'twisty_maze_12': {'w': 'twisty_maze_10', 'n': 'twisty_maze_11',
                       's': 'twisty_maze_13'},
    'twisty_maze_13': {'w': 'twisty_maze_12', 'out': 'twisty_maze_12'},
    'twisty_maze_14': {'w': 'twisty_maze_11', 'n': 'twisty_maze_14',
                       'd': 'twisty_maze_15', 'e': 'pit_brink'},
    'twisty_maze_15': {'u': 'twisty_maze_14', 'out': 'twisty_maze_14'},
    'pit_brink': {'d': 'bird_chamber',
                  'climb': 'bird_chamber',
                  'w': 'twisty_maze_14',
                  's': 'dead_end',
                  'n': 'maze_4',
                  'e': 'maze_5'},
    'dead_end': {'e': 'pit_brink', 'out': 'pit_brink'},
    'crawled_through': {'road': 'fissure_west'},
    'long_hall_east': {'e': 'hall_of_mists_west',
                       'u': 'hall_of_mists_west',
                       'crawl': 'hall_of_mists_west',
                       'w': 'long_hall_west',
                       'n': 'cross_over',
                       'd': 'cross_over',
                       'hole': 'cross_over'},
    'long_hall_west': {'e': 'long_hall_east',
                       'n': 'cross_over',
                       's': ('maze_different', 'no dwarves')},
    'cross_over': {'w': 'long_hall_east',
                   'n': 'dead_end_2',
                   'e': 'west-side_chamber',
                   's': 'long_hall_west'},
    'dead_end_2': {'s': 'cross_over', 'out': 'cross_over'},
    'complex_junction': {'u': 'dusty_rock',
                         'climb': 'dusty_rock',
                         'room': 'dusty_rock',
                         'w': 'bedquilt',
                         'bedqu': 'bedquilt',
                         'n': 'shell_room',
                         'shell': 'shell_room',
                         'e': 'anteroom'},
    'bedquilt': {'e': 'complex_junction',
                 'w': 'swiss_cheese',
                 'd': (('MESSAGE', 56), None, ('PROBABILITY', 80)),
                 'slab': 'slab',
                 'u': ('secret_canyon_above_passage', ('PROBABILITY', 50)),
                 'default': 'anteroom',
                 'n': (('MESSAGE', 56), None, ('PROBABILITY', 60))},
    'swiss_cheese': {'ne': 'bedquilt',
                     'w': 'twopit_east',
                     's': (('MESSAGE', 56), None, ('PROBABILITY', 80)),
                     'canyo': 'tall_canyon',
                     'e': 'soft',
                     'nw': (('MESSAGE', 56), None, ('PROBABILITY', 50)),
                     'orien': 'oriental'},
    'twopit_east': {'e': 'swiss_cheese',
                    'w': 'two_pit_west',
                    'acros': 'two_pit_west',
                    'd': 'pit_east',
                    'pit': 'pit_east'},
    'slab': {'s': 'two_pit_west',
             'u': 'secret_canyon_above_room',
             'climb': 'secret_canyon_above_room',
             'n': 'bedquilt'},
    'secret_canyon_above_room': {'d': 'slab',
                                 'slab': 'slab',
                                 's': ('canyon_2', ('PROPERTY (must not be)', 'dragon', 0)),
                                 'default': 'canyon_1',
                                 'n': 'mirror_canyon',
                                 'reser': 'reservoir'},
    'secret_canyon_above_passage': {'n': '3_canyons_junction',
                                    'd': 'bedquilt', 'passa': 'bedquilt',
                                    's': 'stalactite'},
    '3_canyons_junction': {'se': 'bedquilt',
                           's': 'secret_canyon_above_passage',
                           'n': 'window_pit'},
    'large_low_room': {'bedqu': 'bedquilt',
                       'sw': 'sloping_corridor',
                       'n': 'dead_end_crawl',
                       'se': 'oriental',
                       'orien': 'oriental'},
    'dead_end_crawl': {'s': 'large_low_room',
                       'crawl': 'large_low_room',
                       'out': 'large_low_room'},
    'secret_canyon': {'e': 'hall_of_mountain_king',
                      'w': ('canyon_2', ('PROPERTY (must not be)', 'dragon', 0)),
                      'default': 'canyon_3',
                      'd': 'wide_place'},
    'wide_place': {'s': 'tight_canyon', 'n': 'tall_canyon'},
    'tight_canyon': {'n': 'wide_place'},
    'tall_canyon': {'e': 'wide_place',
                    'w': 'boulders',
                    'n': 'swiss_cheese',
                    'crawl': 'swiss_cheese'},
    'boulders': {'s': 'tall_canyon'},
    'stream_pipe': {'build': 'building'},
    'maze_1': {'n': 'twisty_maze_1', 's': 'maze_1', 'e': 'maze_2'},
    'maze_2': {'w': 'maze_1', 'out': 'maze_1'},
    'maze_3': {'s': 'twisty_maze_3', 'out': 'twisty_maze_3'},
    'maze_4': {'s': 'pit_brink', 'e': 'maze_5', 'w': 'maze_6'},
    'maze_5': {'n': 'pit_brink', 'w': 'maze_4', 'nw': 'dead_end_3'},
    'maze_6': {'e': 'maze_4', 'out': 'maze_4'},
    'maze_7': {'u': 'twisty_maze_11', 'out': 'twisty_maze_11'},
    'maze_8': {'u': 'twisty_maze_4', 'd': 'twisty_maze_4'},
    'narrow_corridor': {'d': 'pit_west',
                        'climb': 'pit_west',
                        'e': 'pit_west',
                        'jump': 'broken_neck',
                        'w': 'giant_room',
                        'giant': 'giant_room'},
    'nothing_to_climb': {'road': 'pit_west'},
    'climbed_up': {'road': 'two_pit_west'},
    'steep_incline': {'n': 'cavern',
                      'caver': 'cavern',
                      'passa': 'cavern',
                      'd': 'large_low_room',
                      'climb': 'large_low_room'},
    'giant_room': {'s': 'narrow_corridor',
                   'e': 'cave-in',
                   'n': 'immense_passage'},
    'cave-in': {'s': 'giant_room',
                'giant': 'giant_room',
                'out': 'giant_room'},
    'immense_passage': {'s': 'giant_room',
                        'giant': 'giant_room',
                        'passa': 'giant_room',
                        'n': ('cavern', ('PROPERTY (must not be)', 'door', 0)),
                        'enter': ('cavern', ('PROPERTY (must not be)', 'door', 0)),
                        'caver': ('cavern', ('PROPERTY (must not be)', 'door', 0)),
                        'default': (('MESSAGE', 111), None, None)},
    'cavern': {'s': 'immense_passage',
               'out': 'immense_passage',
               'giant': 'giant_room',
               'w': 'steep_incline'},
    'soft': {'w': 'swiss_cheese', 'out': 'swiss_cheese'},
    'oriental': {'se': 'swiss_cheese',
                 'w': 'large_low_room',
                 'crawl': 'large_low_room',
                 'u': 'misty_cavern',
                 'n': 'misty_cavern',
                 'caver': 'misty_cavern'},
    'misty_cavern': {'s': 'oriental', 'orien': 'oriental', 'w': 'alcove'},
    'alcove': {'nw': 'misty_cavern',
               'caver': 'misty_cavern',
               'e': (301, 'goto', 1, None),
               'passa': (301, 'goto', 1, None),
               'default': 'plover'},
    'plover': {'w': (301, 'goto', 1, None),
               'passa': (301, 'goto', 1, None),
               'out': (301, 'goto', 1, None),
               'default': 'y2',
               'plove': (159302, 'goto', 2, ('REQUIRED_OBJECT', 'emerald')),
               'ne': 'dark_room',
               'dark': 'dark_room'},
    'dark_room': {'s': 'plover', 'plove': 'plover', 'out': 'plover'},
    'arched_hall': {'d': 'shell_room',
                    'shell': 'shell_room',
                    'out': 'shell_room'},
    'shell_room': {'u': 'arched_hall',
                   'hall': 'arched_hall',
                   'd': 'sloping_corridor_ragged',
                   's': (('MESSAGE', 118), None, ('REQUIRED_OBJECT', 'clam')),
                   'default': 'complex_junction'},
    'sloping_corridor_ragged': {'u': 'shell_room',
                                'shell': 'shell_room',
                                'd': 'cul-de-sac'},
    'cul-de-sac': {'u': 'sloping_corridor_ragged',
                   'out': 'sloping_corridor_ragged',
                   'shell': 'shell_room'},
    'anteroom': {'u': 'complex_junction',
                 'w': 'bedquilt',
                 'e': 'witts_end'},
    'maze_different': {'s': 'twisty_1',
                       'sw': 'twisty_2',
                       'ne': 'twisty_3',
                       'se': 'twisty_4',
                       'u': 'twisty_5',
                       'nw': 'twisty_6',
                       'e': 'twisty_7',
                       'w': 'twisty_8',
                       'n': 'twisty_9',
                       'd': 'long_hall_west'},
    'witts_end': {'e': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'n': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  's': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'ne': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'se': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'sw': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'nw': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'u': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'd': (('MESSAGE', 56), None, ('PROBABILITY', 95)),
                  'default': 'anteroom',
                  'w': (('MESSAGE', 126), None, None)},
    'mirror_canyon': {'s': 'secret_canyon_above_room',
                      'n': 'reservoir',
                      'reser': 'reservoir'},
    'window_pit': {'w': '3_canyons_junction', 'jump': 'broken_neck'},
    'stalactite': {'n': 'secret_canyon_above_passage',
                   'd': ('twisty_maze_9', ('PROBABILITY', 40)),
                   'jump': ('twisty_maze_9', ('PROBABILITY', 40)),
                   'climb': ('twisty_maze_9', ('PROBABILITY', 40)),
                   'default': 'twisty_maze_4'},
    'little_maze_different': {'sw': 'twisty_1',
                              'n': 'twisty_2',
                              'e': 'twisty_3',
                              'nw': 'twisty_4',
                              'se': 'twisty_5',
                              'ne': 'twisty_6',
                              'w': 'twisty_7',
                              'd': 'twisty_8',
                              'u': 'twisty_9',
                              's': 'twisty_10'},
    'reservoir': {'s': 'mirror_canyon',
                  'out': 'mirror_canyon',
                  'mirror_canyon': 'mirror_canyon'},
    'dead_end_3': {'se': 'maze_5'},
    'ne_end': {'sw': 'sw_end'},
    'sw_end': {'ne': 'ne_end', 'd': (('MESSAGE', 93), None, None)},
    'sw_side_of_chasm': {'sw': 'sloping_corridor', 'over': (
        ('MESSAGE', 160), None, ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'acros': (('MESSAGE', 160), None,
                                   ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'cross': (('MESSAGE', 160), None,
                                   ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'ne': (('MESSAGE', 160), None,
                                ('REQUIRED_OBJECT_IN_ROOM', 33)),
                         'default': (('MESSAGE', 96), None, None),
                         'jump': ('didnt_makeit', ('PROPERTY (must not be)', 'chasm', 0))},
    'sloping_corridor': {'d': 'large_low_room', 'u': 'sw_side_of_chasm'},
    'canyon_1': {'n': 'secret_canyon_above_room',
                 'out': 'secret_canyon_above_room',
                 'e': (('MESSAGE', 153), None, None),
                 'forwa': (('MESSAGE', 153), None, None)},
    'canyon_2': {'n': 'secret_canyon_above_room', 'e': 'secret_canyon'},
    'canyon_3': {'e': 'secret_canyon',
                 'out': 'secret_canyon',
                 'n': (('MESSAGE', 153), None, None),
                 'forwa': (('MESSAGE', 153), None, None)},
    'ne_side_of_chasm': {'ne': 'corridor', 'over': (
        ('MESSAGE', 160), None, ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'acros': (('MESSAGE', 160), None,
                                   ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'cross': (('MESSAGE', 160), None,
                                   ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'sw': (('MESSAGE', 160), None,
                                ('REQUIRED_OBJECT_IN_ROOM', 'troll')),
                         'default': (303, 'goto', 3, None),
                         'jump': (('MESSAGE', 96), None, None),
                         'fork': 'fork',
                         'view': 'breath-taking_view',
                         'barre': 'barren_room_front'},
    'corridor': {'w': 'ne_side_of_chasm',
                 'e': 'fork',
                 'fork': 'fork',
                 'view': 'breath-taking_view',
                 'barre': 'barren_room_front'},
    'fork': {'w': 'corridor',
             'ne': 'warm_walls',
             'left': 'warm_walls',
             'se': 'limestone_passage',
             'right': 'limestone_passage',
             'd': 'limestone_passage',
             'view': 'breath-taking_view',
             'barre': 'barren_room_front'},
    'warm_walls': {'s': 'fork',
                   'fork': 'fork',
                   'n': 'breath-taking_view',
                   'view': 'breath-taking_view',
                   'e': 'boulder_chamber',
                   'crawl': 'boulder_chamber'},
    'breath-taking_view': {'s': 'warm_walls',
                           'passa': 'warm_walls',
                           'out': 'warm_walls',
                           'fork': 'fork',
                           'd': (('MESSAGE', 110), None, None),
                           'jump': (('MESSAGE', 110), None, None)},
    'boulder_chamber': {'w': 'warm_walls',
                        'out': 'warm_walls',
                        'crawl': 'warm_walls',
                        'fork': 'fork',
                        'view': 'breath-taking_view'},
    'limestone_passage': {'n': 'fork',
                          'u': 'fork',
                          'fork': 'fork',
                          's': 'barren_room_front',
                          'd': 'barren_room_front',
                          'barre': 'barren_room_front',
                          'view': 'breath-taking_view'},
    'barren_room_front': {'w': 'limestone_passage',
                          'u': 'limestone_passage',
                          'fork': 'fork',
                          'e': 'barren_room',
                          'in': 'barren_room',
                          'barre': 'barren_room',
                          'enter': 'barren_room',
                          'view': 'breath-taking_view'},
    'barren_room': {'w': 'barren_room_front',
                    'out': 'barren_room_front',
                    'fork': 'fork',
                    'view': 'breath-taking_view'},
    'twisty_1': {'w': 'maze_different',
                 'se': 'twisty_2',
                 'nw': 'twisty_3',
                 'sw': 'twisty_4',
                 'ne': 'twisty_5',
                 'u': 'twisty_6',
                 'd': 'twisty_7',
                 'n': 'twisty_8',
                 's': 'twisty_9',
                 'e': 'little_maze_different'},
    'twisty_2': {'nw': 'maze_different',
                 'u': 'twisty_1',
                 'n': 'twisty_3',
                 's': 'twisty_4',
                 'w': 'twisty_5',
                 'sw': 'twisty_6',
                 'ne': 'twisty_7',
                 'e': 'twisty_8',
                 'd': 'twisty_9',
                 'se': 'little_maze_different'},
    'twisty_3': {'u': 'maze_different',
                 'd': 'twisty_1',
                 'w': 'twisty_2',
                 'ne': 'twisty_4',
                 'sw': 'twisty_5',
                 'e': 'twisty_6',
                 'n': 'twisty_7', 'nw': 'twisty_8',
                 'se': 'twisty_9',
                 's': 'little_maze_different'},
    'twisty_4': {'ne': 'maze_different',
                 'n': 'twisty_1',
                 'nw': 'twisty_2',
                 'se': 'twisty_3',
                 'e': 'twisty_5',
                 'd': 'twisty_6',
                 's': 'twisty_7', 'u': 'twisty_8',
                 'w': 'twisty_9',
                 'sw': 'little_maze_different'},
    'twisty_5': {'n': 'maze_different',
                 'se': 'twisty_1',
                 'd': 'twisty_2',
                 's': 'twisty_3',
                 'e': 'twisty_4',
                 'w': 'twisty_6',
                 'sw': 'twisty_7',
                 'ne': 'twisty_8',
                 'nw': 'twisty_9',
                 'u': 'little_maze_different'},
    'twisty_6': {'e': 'maze_different',
                 'w': 'twisty_1',
                 'u': 'twisty_2',
                 'sw': 'twisty_3',
                 'd': 'twisty_4',
                 's': 'twisty_5',
                 'nw': 'twisty_7',
                 'se': 'twisty_8',
                 'ne': 'twisty_9',
                 'n': 'little_maze_different'},
    'twisty_7': {'se': 'maze_different',
                 'ne': 'twisty_1',
                 's': 'twisty_2',
                 'd': 'twisty_3',
                 'u': 'twisty_4',
                 'nw': 'twisty_5',
                 'n': 'twisty_6',
                 'sw': 'twisty_8',
                 'e': 'twisty_9',
                 'w': 'little_maze_different'},
    'twisty_8': {'d': 'maze_different',
                 'e': 'twisty_1',
                 'ne': 'twisty_2',
                 'u': 'twisty_3',
                 'w': 'twisty_4',
                 'n': 'twisty_5',
                 's': 'twisty_6', 'se': 'twisty_7',
                 'sw': 'twisty_9',
                 'nw': 'little_maze_different'},
    'twisty_9': {'sw': 'maze_different',
                 'nw': 'twisty_1',
                 'e': 'twisty_2',
                 'w': 'twisty_3',
                 'n': 'twisty_4',
                 'd': 'twisty_5',
                 'se': 'twisty_6',
                 'u': 'twisty_7',
                 's': 'twisty_8',
                 'ne': 'little_maze_different'},
    'twisty_10': {'n': 'little_maze_different',
                  'out': 'little_maze_different'},
}