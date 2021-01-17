import re
import sys
import random

import location
from commands import Command, Util
from object import objects_dict, initial_object_locations, Item
from player import Player
from data.travel_table import travel_table
from data.vocabulary import TYPE_MOTION, TYPE_OBJECT, TYPE_ACTION
from dwarf import Dwarf, dwarves_info

objects = {}
locations = {}

# HOW OFTEN WE'VE SAID "NOT ALLOWED TO GIVE MORE DETAIL"
detail = 0
dwarf_activation_level = 0


class Adventure:
    """
    """
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super(Adventure, cls).__new__(cls)
        return cls._instance

    def setUp(self) -> None:
        """
        Sets up the game by creating locations dictionary, items dictionary, player
        :return:
        """

        # Create item objects and assign add the item to its location
        global objects
        _location_objects = {}

        # Create objects instances dictionary
        for key, value in objects_dict.items():
            initial_location = initial_object_locations[key]
            item = Item(value[0], key, value[1], value[2], initial_location)

            # Now update the location_objects. This is for performance,
            # so that locations can easily populate their initial objects.
            objects[key] = item
            _location_objects.setdefault(initial_location[0], {})[key] = item
            if initial_location[1] != 0:
                _location_objects.setdefault(initial_location[1], {})[key] = item

        # Create location objects
        global locations
        for name, desc in location.long_descriptions.items():
            short_desc = location.short_descriptions.get(name, None)

            index = next((k for k, v in location.location_lookup.items() if v == name), None)
            loc = location.Location(index, name, desc, short_desc, _location_objects.get(name, {}))
            locations[name] = loc

            # Build up a dictionary of destinations for each location
            for dir, dest in travel_table[name].items():
                locations[name].add_destination(dir, dest)

        del _location_objects

        self.dwarves = []
        for loc, is_pirate in dwarves_info:
            d = Dwarf(loc, is_pirate)
            self.dwarves.append(d)

        self.player = Player()

    def manage_dwarves(self):
        global dwarf_activation_level
        loc = self.player.location

        if not (loc.is_forced() or loc.bitset(3)):
            for d in self.dwarves:
                if d.old_loc == loc.name and d.seen:
                    Util.print_message(2)
                    break

        if loc.is_forced() or loc.bitset(3):
            loc.print_desc(self.player)
        elif dwarf_activation_level != 0:
            if dwarf_activation_level == 1:
                # WHEN WE ENCOUNTER THE FIRST DWARF, WE KILL 0, 1, OR 2 OF THE 5 DWARVES.
                # TODO replace `loc.index < 15` with `loc.index < 15 or Util.pct(95)` after test
                if loc.index < 15:
                    loc.print_desc(self.player)
                else:
                    dwarf_activation_level = 2
                    for d in random.sample(self.dwarves, 2):
                        if Util.pct(50):
                            d.loc = None
                    for d in self.dwarves:
                        if d.loc == loc.name:
                            d.old_loc = d.loc
                            d.loc = 'nugget_room'
                    Util.print_message(3)
                    loc.add_item(objects['axe'])
                    loc.print_desc(self.player)
            else:
                loc.print_desc(self.player)
                # print('#6010')
        elif dwarf_activation_level == 0 and loc.index >= 15:
            dwarf_activation_level = 1
            loc.print_desc(self.player)
        else:
            loc.print_desc(self.player)

    def run(self) -> None:
        while True:
            self.manage_dwarves()

            # available_exists = {dest for dir, dest in locations[self.player.location.name].destinations.items()}
            # direction = input(f"Available exits are:\n{available_exists}\n:").casefold()
            direction = input(":").casefold()
            from colorama import Fore, Style
            print(Fore.YELLOW, direction, Style.RESET_ALL)

            # remove non-alph from the import and interpret each word
            what_to_do = []
            for d in direction.split():
                word = re.sub(r'[^a-zA-Z]', '', d)[:6]
                if w := Util.get_word_info(word):
                    what_to_do.append(w)

            if what_to_do:
                vocab_type, value = what_to_do[0]

                # if the first word is a TYPE_MOTION, ignore the rest
                if vocab_type == TYPE_MOTION:
                    self.player.go(value)
                else:
                    action_vocab = ''
                    item_vocabs = []
                    for vocab_type, value in what_to_do:
                        if vocab_type == TYPE_ACTION:
                            action_vocab = value
                        elif vocab_type == TYPE_OBJECT:
                            item_vocabs.append(value)

                    if action_vocab and not item_vocabs:
                        getattr(Command, action_vocab)(player=self.player)
                    elif item_vocabs:
                        for item_name in item_vocabs:
                            if action_vocab:
                                getattr(Command, action_vocab)(player=self.player, item=objects[item_name])

                    # elif vocab_type == TYPE_SPECIAL_CASE:
                    #     # special case verb
                    #     print(arbitrary_messages[value])
            else:
                print("Please enter a valid input")

    def end(self):
        pass

    def save(self):
        import pickle
        with open('player_game.pickle', 'wb') as pickled_file:
            pickle.dump(self.objects, pickled_file)
            pickle.dump(self.locations, pickled_file)

    def run_test(self):
        with open('test_values.txt') as target:
            try:
                self.orig_stdin = sys.stdin
                sys.stdin = target
                self.run()
                sys.stdin = self.orig_stdin
            except EOFError:
                print(end='')
