import game
from data.class_messages import classMessages
from commands import Util
from location import Location
from object import Item


class Player:
    """
    Class to store player's state.
    There will only be one instance of this class in the game - unless it's made multi-player.

    The class attributes include the player's current location, and the items that the player has collected.
    """

    def __init__(self):
        self.items = []
        self.location = game.locations['road']  # start at the road
        self.trajectory = [self.location.name]
        self.score = 0

    def take_item(self, item: Item) -> None:
        if item not in self.items:
            if not item.immovable:
                print(f'{item.description}: Taken.')
                self.items.append(item)
                item.move("player", True if self.location.name == item.locations[1] else False)
            else:
                print(f'The {item.name} is fixed in place.')
        else:
            print(f'You are already carrying the {item.name}.')

    def drop_item(self, item: Item) -> None:
        print(f'{item.name}: Dropped/ Released.')
        self.items.remove(item)
        item.move(self.location.name)

    def destroy_item(self, item_name: str) -> None:
        """
        Permanently eliminate an item by moving to a non-existent location (None).
        """
        if self.has_item(item_name):
            item = game.objects[item_name]
            self.drop_item(item)
        elif self.location.is_item_present(item_name):
            item = self.location.objects[item_name]
        else:
            raise ValueError(f'Can not destroy the {item_name}.')

        item.move(None)

    def has_item(self, item_name) -> bool:
        """
        Check if the player carries an item
        """
        return any(i.name == item_name for i in self.items)

    def item_count(self):
        """
        Returns total number of items the player carries
        """
        return len(self.items)

    def get_level(self):
        player_level = ''
        for score, level in classMessages:
            if self.score < score:
                player_level = level
                break

        if player_level:
            print(player_level)
        else:
            print('No level found for the player!')

    def go(self, direction: str) -> Location:

        dest_info = self.location.destinations.get(direction, None)
        dest = None
        print('***', self.location.name, direction, dest_info)
        while not dest:
            if isinstance(dest_info, str):
                dest = game.locations.get(dest_info)
            elif isinstance(dest_info, tuple):
                if isinstance(dest_info[0], str):
                    dest = game.locations.get(dest_info[0])
                    default_dest_info = self.location.destinations.get('default', None)
                    if dest_info[0] == 'MESSAGE':
                        Util.print_message(dest_info[1])
                        if dest_info[2]:
                            if dest_info[2][0] == 'PROPERTY (must not be)':
                                item = game.objects[dest_info[2][1]]
                                if item.prop == dest_info[2][2]:
                                    dest_info, dest = default_dest_info, None
                            elif dest_info[2][0] == 'REQUIRED_OBJECT_IN_ROOM':
                                if self.location.is_item_present(dest_info[2][1]) or self.has_item(dest_info[2][1]):
                                    return
                                else:
                                    dest_info, dest = default_dest_info, None
                        else:
                            return
                    else:
                        if dest_info[1][0] == 'PROPERTY (must not be)':
                            item = game.objects[dest_info[1][1]]
                            if item.prop == dest_info[1][2]:
                                dest_info, dest = default_dest_info, None
                        elif dest_info[1][0] == 'REQUIRED_OBJECT':
                            item = game.objects[dest_info[1][1]]
                            if not self.has_item(item.name):
                                dest_info, dest = default_dest_info, None
                        elif dest_info[1][0] == 'PROBABILITY':
                            # TODO replace None with Util.pct(dest_info[1][1]) after test
                            if None:
                                dest_info, dest = default_dest_info, None
                        elif dest_info[1][0] == 'REQUIRED_OBJECT_IN_ROOM':
                            if not (self.location.is_item_present(dest_info[1][1]) or self.has_item(dest_info[1][1])):
                                dest_info, dest = default_dest_info, None
                        else:
                            raise NotImplemented('The move is not implemented!')
            else:
                break

        if not dest:
            Util.color_print('WARN', "You can't go that way.")
            return

        if self.location != dest:
            if dest.is_forced():
                self.trajectory.append(dest)
                dest.print_desc(self)
                dest = game.locations.get(dest.destinations['road'], None)
            self.location = dest
            self.trajectory.append(self.location.name)
            return self.location

        # 'hole': ('MESSAGE', 148, None)
        # 'road': (0, None)
        # 'over': ('MESSAGE', 160, ('REQUIRED_OBJECT_IN_ROOM', 'troll')),

    def move(self, loc: Location):
        if self.location != loc:
            self.location = loc
            self.trajectory.append(self.location.name)
            return self.location

    def __str__(self):
        return f'Player is at location {self.location.number} and has a list of items {self.items}'
