import re
import sys

import location
from commands import Command, Util
from object import objects_dict, initial_object_locations, Item
from player import Player
from data.travel_table import travel_table
from data.vocabulary import TYPE_MOTION, TYPE_OBJECT, TYPE_ACTION

objects = {}
locations = {}

# HOW OFTEN WE'VE SAID "NOT ALLOWED TO GIVE MORE DETAIL"
detail = 0


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

        self.player = Player()

    def run(self) -> None:
        while True:
            locations[self.player.location.name].print_info(objects['lantern'], self.player)

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
