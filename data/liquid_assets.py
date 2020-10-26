"""
#  SECTION 9: LIQUID ASSETS, ETC.  EACH LINE CONTAINS A NUMBER (N) AND UP TO 20
#	LOCATION NUMBERS.  BIT N (WHERE 0 IS THE UNITS BIT) IS SET IN COND(LOC)
#	FOR EACH LOC GIVEN.  THE COND BITS CURRENTLY ASSIGNED ARE:
#		0	LIGHT
#		1	IF BIT 2 IS ON: ON FOR OIL, OFF FOR WATER
#		2	LIQUID ASSET, SEE BIT 1
#		3	PIRATE DOESN'T GO HERE UNLESS FOLLOWING PLAYER
#	OTHER BITS ARE USED TO INDICATE AREAS OF INTEREST TO "HINT" ROUTINES:
#		4	TRYING TO GET INTO CAVE
#		5	TRYING TO CATCH BIRD
#		6	TRYING TO DEAL WITH SNAKE
#		7	LOST IN MAZE
#		8	PONDERING DARK ROOM
#		9	AT WITT'S END
#	COND(LOC) IS SET TO 2, OVERRIDING ALL OTHER BITS, IF LOC HAS FORCED
#	MOTION.
"""
liquid_assets_ = {
    0: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    0: [100, 115, 116, 126],
    2: [1, 3, 4, 7, 38, 95, 113, 24],
    1: [24],
    3: [46, 47, 48, 54, 56, 58, 82, 85, 86],
    3: [122, 123, 124, 125, 126, 127, 128, 129, 130],
    4: [8],
    5: [13],
    6: [19],
    7: [42, 43, 44, 45, 46, 47, 48, 49, 50, 51],
    7: [52, 53, 54, 55, 56, 80, 81, 82, 86, 87],
    8: [99, 100, 101],
    9: [108],
}

liquid_assets = (
    ('road', 'hill', 'building', 'valley.', 'forrest_valley',
     'forrest_road', 'slit', 'grate_outside', 'grate_below',
     'cobble_crawl', 'plover', 'ne_end', 'sw_end', 'taking_view',),
    ('pit_east',),
    ('road', 'building', 'valley.', 'slit', 'bottom_of_pit', 'cavern', 'reservoir', 'pit_east',),
    ('twisty_maze_5', 'twisty_maze_6', 'twisty_maze_7', 'twisty_maze_13',
     'twisty_maze_15', 'dead_end', 'maze_3', 'maze_6', 'maze_7',
     'ne_side_of_chasm', 'corridor', 'fork', 'warm_walls', 'breath-taking_view',
     'boulder_chamber', 'limestone_passage', 'barren_room_front', 'barren_room',),
    ('grate_outside',),
    ('bird_chamber',),
    ('hall_of_mountain_king',),
    ('twisty_maze_1', 'twisty_maze_2', 'twisty_maze_3', 'twisty_maze_4',
     'twisty_maze_5', 'twisty_maze_6', 'twisty_maze_7', 'twisty_maze_8',
     'twisty_maze_9', 'twisty_maze_10', 'twisty_maze_11', 'twisty_maze_12',
     'twisty_maze_13', 'twisty_maze_14', 'twisty_maze_15', 'maze_1', 'maze_2',
     'maze_3', 'maze_7', 'maze_8',),
    ('alcove', 'plover', 'dark_room',),
    ('witts_end',),
)


def get_cond(location_name: str) -> int:
    bin_cond = 0
    for index, value in enumerate(liquid_assets):
        if location_name in value:
            bin_cond = bin_cond | 1 << index

    return bin_cond

