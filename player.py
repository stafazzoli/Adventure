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
        if item.name in self.location.objects:
            if not item.immovable:
                print(f"{item.description}: Taken.")
                self.items.append(item)
                self.location.remove_item(item)
                item.set_location("player")
            else:
                print(f"The {item.description} is fixed in place.")
        else:
            print(f"'{item.name}' does not exist here.")

    def drop_item(self, item: Item) -> None:
        print(f"{item.description}: Dropped.")
        self.items.pop(item)
        self.location.add_item(item)
        item.set_location(self.location.name)

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
            raise ValueError(f"Can not destroy '{item_name}'")

        item.set_location(None)

    def has_item(self, item_name) -> bool:
        """
        Check if the player carries an item
        """
        return any(i.name == item_name for i in self.items)

    def items_count(self):
        """
        Returns the number of items the player carries
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
                        if not (self.location.is_item_present('snake') or self.has_item('snake')):
                            dest_info, dest = default_dest_info, None
                    else:
                        raise NotImplemented('The move is not implemented!')
            else:
                break

        if not dest:
            Util.color_print('WARN', "You can't go that way.")
            return

        if self.location != dest:
            self.location = dest
            self.trajectory.append(self.location.name)
            return self.location

        # 'road': (0, None)
        # 'slit': (('MESSAGE', 95), None, None),
        # 'jump': (('MESSAGE', 96), None, ('PROPERTY (must not be)', 12, 0)),

    def __str__(self):
        return f"Player is at location {self.location.number} and has a list of items {self.items}"
