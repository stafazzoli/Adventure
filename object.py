from commands import Util

"""
#  SECTION 5: OBJECT DESCRIPTIONS.  EACH LINE CONTAINS A NUMBER (N), A TAB),
# AND A MESSAGE.  IF N IS FROM 1 TO 100, THE MESSAGE IS THE INVENTORY
# MESSAGE FOR OBJECT N.  OTHERWISE, N SHOULD BE [100, 200, ETC., AND
# THE MESSAGE SHOULD BE THE DESCRIPTION OF THE PRECEDING OBJECT WHEN ITS
# PROP VALUE IS N/100.  THE N/100 IS USED ONLY TO DISTINGUISH MULTIPLE
# MESSAGES FROM MULTI-LINE MESSAGES; THE PROP INFO ACTUALLY REQUIRES ALL
# MESSAGES FOR AN OBJECT TO BE PRESENT AND CONSECUTIVE.  PROPERTIES WHICH
# PRODUCE NO MESSAGE SHOULD BE GIVEN THE MESSAGE >$<.
"""


class Item:
    """
    Class to store "object" messages and properties.
    This class corresponds to "object" in the original documentation (but the name object can't be used in Python).

    The 000, 100, 200 etc index numbers aren't required.  Instead, we use normal list indexing.
    """

    def __init__(self, index, name, description, messages, locations):
        self.index = index
        self.name = name
        self.description = description
        self.messages = []
        for message in messages:
            self.messages.append(message)

        self.prop = 0
        if self.index > 50:
            self.prop = -1  # treasures have prop -1.

        self.initial_location = locations  # a tuple, see 7 below
        self.location = self.initial_location[0]  # location1
        self.locations = self.initial_location[:2]  # (location1, location2)
        self.immovable = self.initial_location[2]

    def set_location(self, loc_name: str, loc2=False) -> None:
        """
        Set the location of an item. If loc2 is true, change the second location of the item.
        location 'player': item is carried by the player
        location 'None': item is destroyed
        """
        self.location = loc_name
        if self.locations[1] == 0 or not loc2:
            self.locations = (loc_name, self.locations[1])
        elif loc2:
            self.locations = (self.locations[0], loc_name)

    def get_message(self, prop: int = None) -> str:
        prop_msg = prop if prop else self.prop
        Util.color_print('ITEM_INFO', self.messages[prop_msg])

    def __str__(self):
        return f"{self.index}-{self.name}:: {self.description}: {self.messages}: {self.prop}: " \
               f"{self.location}: {self.locations}: {self.immovable}"


objects_dict = {
    'keys': (1, '''SET OF KEYS''',
             ['''THERE ARE SOME KEYS ON THE GROUND HERE.''']),
    'lantern': (2, '''BRASS LANTERN''',
                ['''THERE IS A SHINY BRASS LAMP NEARBY.''',
                 '''THERE IS A LAMP SHINING NEARBY.''']),
    'grate': (3, '''*GRATE''',
              ['''THE GRATE IS LOCKED.''',
               '''THE GRATE IS OPEN.''']),
    'cage': (4, '''WICKER CAGE''',
             ['''THERE IS A SMALL WICKER CAGE DISCARDED NEARBY.''']),
    'rod': (5, '''BLACK ROD''',
            ['''A THREE FOOT BLACK ROD WITH A RUSTY STAR ON AN END LIES NEARBY.''']),
    'rod_6': (6, '''BLACK ROD''',
              ['''A THREE FOOT BLACK ROD WITH A RUSTY MARK ON AN END LIES NEARBY.''']),
    'steps': (7, '''*STEPS''',
              ['''ROUGH STONE STEPS LEAD DOWN THE PIT.''',
               '''ROUGH STONE STEPS LEAD UP THE DOME.''']),
    'bird': (8, '''LITTLE BIRD IN CAGE''',
             ['''A CHEERFUL LITTLE BIRD IS SITTING HERE SINGING.''',
              '''THERE IS A LITTLE BIRD IN THE CAGE.''']),
    'door': (9, '''*RUSTY DOOR''',
             ['''THE WAY NORTH IS BARRED BY A MASSIVE, RUSTY, IRON DOOR.''',
              '''THE WAY NORTH LEADS THROUGH A MASSIVE, RUSTY, IRON DOOR.''']),
    'pillow': (10, '''VELVET PILLOW''',
               ['''A SMALL VELVET PILLOW LIES ON THE FLOOR.''']),
    'snake': (11, '''*SNAKE''',
              ['''A HUGE GREEN FIERCE SNAKE BARS THE WAY!''',
               '''>$<  (CHASED AWAY)''']),
    'fissure': (12, '''*FISSURE''',
                ['''>$<''',
                 '''A CRYSTAL BRIDGE NOW SPANS THE FISSURE.''',
                 '''THE CRYSTAL BRIDGE HAS VANISHED!''']),
    'tablet': (13, '''*STONE TABLET''',
               ['''A MASSIVE STONE TABLET EMBEDDED IN THE WALL READS:
             "CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!"''']),
    'clam': (14, '''GIANT CLAM  >GRUNT!<''',
             ['''THERE IS AN ENORMOUS CLAM HERE WITH ITS SHELL TIGHTLY CLOSED.''']),
    'oyster': (15, '''GIANT OYSTER  >GROAN!<''',
               ['''THERE IS AN ENORMOUS OYSTER HERE WITH ITS SHELL TIGHTLY CLOSED.''',
                '''INTERESTING.  THERE SEEMS TO BE SOMETHING WRITTEN ON THE UNDERSIDE OF THE OYSTER.''']),
    'spelunker': (16, '''"SPELUNKER TODAY''',
                  ['''THERE ARE A FEW RECENT ISSUES OF "SPELUNKER TODAY" MAGAZINE HERE.''']),
    'dwarf': (17, '''Dwarf''',
              ['''There is a nasty little dwarf in the room with you.''']),
    'knife': (18, '''Knife''',
              ['''A dwarf's knife.''']),
    'food': (19, '''TASTY FOOD''',
             ['''THERE IS FOOD HERE.''']),
    'bottle': (20, '''SMALL BOTTLE''',
               ['''THERE IS A BOTTLE OF WATER HERE.''',
                '''THERE IS AN EMPTY BOTTLE HERE.''',
                '''THERE IS A BOTTLE OF OIL HERE.''']),
    'water': (21, '''WATER IN THE BOTTLE''', []),
    'oil': (22, '''OIL IN THE BOTTLE''', []),
    'mirror': (23, '''*MIRROR''',
               ['''>$<''']),
    'plant': (24, '''*PLANT''',
              ['''THERE IS A TINY LITTLE PLANT IN THE PIT, MURMURING "WATER, WATER, ..."''',
               '''THE PLANT SPURTS INTO FURIOUS GROWTH FOR A FEW SECONDS.''',
               '''THERE IS A 12-FOOT-TALL BEANSTALK STRETCHING UP OUT OF THE PIT, BELLOWING "WATER!! WATER!!"''',
               '''THE PLANT GROWS EXPLOSIVELY, ALMOST FILLING THE BOTTOM OF THE PIT.''',
               '''THERE IS A GIGANTIC BEANSTALK STRETCHING ALL THE WAY UP TO THE HOLE.''',
               '''YOU'VE OVER-WATERED THE PLANT! IT'S SHRIVELING UP!  IT'S, IT'S...''']),
    'phony plant': (25, '''*PHONY PLANT (SEEN IN TWOPIT ROOM ONLY WHEN TALL ENOUGH)''',
                    ['''>$<''',
                     '''THE TOP OF A 12-FOOT-TALL BEANSTALK IS POKING OUT OF THE WEST PIT.''',
                     '''THERE IS A HUGE BEANSTALK GROWING OUT OF THE WEST PIT UP TO THE HOLE.''']),
    'stalactite': (26, '''*STALACTITE''',
                   ['''>$<''']),
    'figure': (27, '''*SHADOWY FIGURE''',
               ['''THE SHADOWY FIGURE SEEMS TO BE TRYING TO ATTRACT YOUR ATTENTION.''']),
    'axe': (28, '''DWARF'S AXE''',
            ['''THERE IS A LITTLE AXE HERE.''',
             '''THERE IS A LITTLE AXE LYING BESIDE THE BEAR.''']),
    'drawings': (29, '''*CAVE DRAWINGS''',
                 ['''>$<''']),
    'pirate': (30, '''*PIRATE''',
               ['''>$<''']),
    'dragon': (31, '''*DRAGON''',
               ['''A HUGE GREEN FIERCE DRAGON BARS THE WAY!''',
                '''CONGRATULATIONS!  YOU HAVE JUST VANQUISHED A DRAGON WITH YOUR BARE HANDS! (UNBELIEVABLE, ISN'T IT?)''',
                '''THE BODY OF A HUGE GREEN DEAD DRAGON IS LYING OFF TO ONE SIDE.''']),
    'chasm': (32, '''*CHASM''',
              ['''A RICKETY WOODEN BRIDGE EXTENDS ACROSS THE CHASM, VANISHING INTO THE MIST.
             A SIGN POSTED ON THE BRIDGE READS, "STOP! PAY TROLL!"''',
               '''THE WRECKAGE OF A BRIDGE (AND A DEAD BEAR) CAN BE SEEN AT THE BOTTOM OF THE CHASM.''']),
    'troll': (33,
              '''*TROLL''',
              ['''A BURLY TROLL STANDS BY THE BRIDGE AND INSISTS YOU THROW HIM A TREASURE BEFORE YOU MAY CROSS.''',
               '''THE TROLL STEPS OUT FROM BENEATH THE BRIDGE AND BLOCKS YOUR WAY.''',
               '''>$<  (CHASED AWAY)''']),
    'phony troll': (34, '''*PHONY TROLL''', ['''THE TROLL IS NOWHERE TO BE SEEN.''']),
    'bear': (35, '''>$<  (BEAR USES RTEXT 141)''',
             ['''THERE IS A FEROCIOUS CAVE BEAR EYING YOU FROM THE FAR END OF THE ROOM!''',
              '''THERE IS A GENTLE CAVE BEAR SITTING PLACIDLY IN ONE CORNER.''',
              '''THERE IS A CONTENTED-LOOKING BEAR WANDERING ABOUT NEARBY.''',
              '''>$<  (DEAD)''']),
    'message': (36, '''*MESSAGE IN SECOND MAZE''',
                ['''THERE IS A MESSAGE SCRAWLED IN THE DUST IN A FLOWERY SCRIPT, READING:
                 "THIS IS NOT THE MAZE WHERE THE PIRATE LEAVES HIS TREASURE CHEST."''']),
    'volcano': (37, '''*VOLCANO AND/OR GEYSER''',
                ['''>$<''']),
    'vending': (38, '''*VENDING MACHINE''',
                ['''THERE IS A MASSIVE VENDING MACHINE HERE. THE INSTRUCTIONS ON IT READ:
             "DROP COINS HERE TO RECEIVE FRESH BATTERIES."''']),
    'batteries': (39, '''BATTERIES''',
                  ['''THERE ARE FRESH BATTERIES HERE.''',
                   '''SOME WORN-OUT BATTERIES HAVE BEEN DISCARDED NEARBY.''']),
    'carpet': (40, '''*CARPET AND/OR MOSS''',
               ['''>$<''']),
    'nugget': (50, '''LARGE GOLD NUGGET''',
               ['''THERE IS A LARGE SPARKLING NUGGET OF GOLD HERE!''']),
    'diamonds': (51, '''SEVERAL DIAMONDS''',
                 ['''THERE ARE DIAMONDS HERE!''']),
    'silver': (52, '''BARS OF SILVER''',
               ['''THERE ARE BARS OF SILVER HERE!''']),
    'jewelery': (53, '''PRECIOUS JEWELRY''',
                 ['''THERE IS PRECIOUS JEWELRY HERE!''']),
    'coins': (54, '''RARE COINS''',
              ['''THERE ARE MANY COINS HERE!''']),
    'chest': (55, '''TREASURE CHEST''', ['''THE PIRATE'S TREASURE CHEST IS HERE!''']),
    'egg': (56, '''GOLDEN EGGS''',
            ['''THERE IS A LARGE NEST HERE, FULL OF GOLDEN EGGS!''',
             '''THE NEST OF GOLDEN EGGS HAS VANISHED!''',
             '''DONE!''']),
    'trident': (57, '''JEWELED TRIDENT''',
                ['''THERE IS A JEWEL-ENCRUSTED TRIDENT HERE!''']),
    'vase': (58, '''MING VASE''',
             ['''THERE IS A DELICATE, PRECIOUS, MING VASE HERE!''',
              '''THE VASE IS NOW RESTING, DELICATELY, ON A VELVET PILLOW.''',
              '''THE FLOOR IS LITTERED WITH WORTHLESS SHARDS OF POTTERY.''',
              '''THE MING VASE DROPS WITH A DELICATE CRASH.''']),
    'emerald': (59, '''EGG-SIZED EMERALD''',
                ['''THERE IS AN EMERALD HERE THE SIZE OF A PLOVER'S EGG!''']),
    'pyramid': (60, '''PLATINUM PYRAMID''',
                ['''THERE IS A PLATINUM PYRAMID HERE, 8 INCHES ON A SIDE!''']),
    'pearl': (61, '''GLISTENING PEARL''',
              ['''OFF TO ONE SIDE LIES A GLISTENING PEARL!''']),
    'rug': (62, '''PERSIAN RUG''',
            ['''THERE IS A PERSIAN RUG SPREAD OUT ON THE FLOOR!''',
             '''THE DRAGON IS SPRAWLED OUT ON A PERSIAN RUG!!''']),
    'spice': (63, '''RARE SPICES''',
              ['''THERE ARE RARE SPICES HERE!''']),
    'chain': (64, '''GOLDEN CHAIN''',
              ['''THERE IS A GOLDEN CHAIN LYING IN A HEAP ON THE FLOOR!''',
               '''THE BEAR IS LOCKED TO THE WALL WITH A GOLDEN CHAIN!''',
               '''THERE IS A GOLDEN CHAIN LOCKED TO THE WALL!''']),
}

object_indices = {  # may be useful when converting any other data that still uses the numbers
    1: "keys",
    2: "lantern",
    3: "grate",
    4: "cage",
    5: "rod",
    6: "rod_6",
    7: "steps",
    8: "bird",
    9: "door",
    10: "pillow",
    11: "snake",
    12: "fissure",
    13: "tablet",
    14: "clam",
    15: "oyster",
    16: "spelunker",
    17: 'dwarf',
    18: 'knife',
    19: "food",
    20: "bottle",
    21: "water",
    22: "oil",
    23: "mirror",
    24: "plant",
    25: "phony plant",
    26: "stalactite",
    27: "figure",
    28: "axe",
    29: "drawings",
    30: "pirate",
    31: "dragon",
    32: "chasm",
    33: "troll",
    34: "phony troll",
    35: "bear",
    36: "message",
    37: "volcano",
    38: "vending",
    39: "batteries",
    40: "carpet",
    50: "nugget",
    51: "diamonds",
    52: "silver",
    53: "jewelery",
    54: "coins",
    55: "chest",
    56: "egg",
    57: "trident",
    58: "vase",
    59: "emerald",
    60: "pyramid",
    61: "pearl",
    62: "rug",
    63: "spice",
    64: "chain",
}

"""
# SECTION 7: OBJECT LOCATIONS.
# EACH LINE CONTAINS AN OBJECT NUMBER AND ITS INITIAL LOCATION (ZERO IF NONE),.
# THE LOCATION IS FOLLOWED BY A "True/False" to indicate whether it is immovable. IF IT HAS TWO LOCATIONS
# (E.G. THE GRATE), THE FIRST LOCATION IS FOLLOWED WITH THE SECOND, otherwise the 2nd loc is zero.
"""
initial_object_locations = {
    'keys': ('building', 0, False),
    'lantern': ('building', 0, False),
    'grate': ('grate_outside', 'grate_below', True),
    'cage': ('cobble_crawl', 0, False),
    'rod': ('debris_room', 0, False),
    'rod_6': (0, 0, False),
    'steps': ('pit', 'hall_of_mists_east', True),
    'bird': ('bird_chamber', 0, False),
    'door': ('immense_passage', 0, True),
    'pillow': ('soft', 0, False),
    'snake': ('hall_of_mountain_king', 0, True),
    'fissure': ('fissure_east', 'fissure_west', True),
    'tablet': ('dark_room', 0, True),
    'clam': ('shell_room', 0, False),
    'oyster': (0, 0, False),
    'spelunker': ('anteroom', 0, False),
    'dwarf': (0, 0, True),
    'knife': (0, 0, False),
    'food': ('building', 0, False),
    'bottle': ('building', 0, False),
    'water': (0, 0, False),
    'oil': (0, 0, False),
    'mirror': ('mirror_canyon', 0, True),
    'plant': ('pit_west', 0, True),
    'phony plant': ('two_pit_west', 'twopit_east', True),
    'stalactite': ('stalactite', 0, True),
    'figure': ('window_over_pit', 'window_pit', True),
    'axe': (0, 0, False),
    'drawings': ('oriental', 0, True),
    'pirate': (0, 0, True),
    'dragon': ('canyon_1', 'canyon_3', True),
    'chasm': ('sw_side_of_chasm', 'ne_side_of_chasm', True),
    'troll': ('sw_side_of_chasm', 'ne_side_of_chasm', True),
    'phony troll': (0, 0, True),
    'bear': ('barren_room', 0, True),
    'message': (0, 0, True),
    'volcano': ('breath-taking_view', 0, True),
    'vending': ('twisty_10', 0, True),
    'batteries': (0, 0, False),
    'carpet': ('soft', 0, True),
    'nugget': ('nugget_room', 0, False),
    'diamonds': ('fissure_west', 0, False),
    'silver': ('hole_in_floor', 0, False),
    'jewelery': ('south-side_chamber', 0, False),
    'coins': ('west-side_chamber', 0, False),
    'chest': (0, 0, False),
    'egg': ('giant_room', 0, False),
    'trident': ('cavern', 0, False),
    'vase': ('oriental', 0, False),
    'emerald': ('plover', 0, False),
    'pyramid': ('dark_room', 0, False),
    'pearl': (0, 0, False),
    'rug': ('canyon_1', 'canyon_3', True),
    'spice': ('boulder_chamber', 0, False),
    'chain': ('barren_room', 0, True),
}
