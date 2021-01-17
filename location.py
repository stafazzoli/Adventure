import game
from data.liquid_assets import get_cond
from commands import Util
from configs import ABBNUM
from object import Item


class Location:
    """ Location class
    abb_desc_no: CONTROLS WHETHER THE ABBREVIATED DESCRIPTION IS PRINTED
    """

    def __init__(self, index, name, description, short_description, objects):
        self.index = index
        self.name = name
        self.description = description
        self.short_description = short_description
        self.destinations = {}  # dic{direction: destination}
        self.objects = objects  # dic{item_name: item_object}
        self.abb_desc_no = 0
        self.cond = get_cond(self.name)

    def add_destination(self, direction: str, destination: any) -> None:
        self.destinations[direction] = destination

    def add_item(self, item: Item) -> None:
        self.objects[item.name] = item

    def remove_item(self, item: Item) -> None:
        del self.objects[item.name]

    def print_info(self, lamp, player):
        # print('----- ', self.name, self.abb_desc_no)
        if not self.short_description or (self.abb_desc_no % ABBNUM == 0):
            desc = self.description
        else:
            desc = self.short_description

        if self.is_forced() or not self.is_dark(lamp, player):
            if player.has_item('bear'):
                Util.print_message(141)
        elif game.locations[player.trajectory[-1]].is_dark(lamp, player) and Util.pct(35):
            Util.print_message(23)
            # 90 HE'S DEAD.  LET'S GET ON WITH IT.
            return
        else:
            desc = 16
            if player.has_item('bear'):
                Util.print_message(141)
            Util.print_message(desc)
            desc = None

        # IF(FORCED(LOC))GOTO 8
        # IF(LOC.EQ.33.AND.PCT(25).AND..NOT.CLOSNG)CALL RSPEAK(8)

        if desc:
            self.abb_desc_no += 1
            Util.color_print('LOC_INFO', desc)

    def print_desc(self, player) -> None:
        lamp = game.objects['lantern']
        self.print_info(lamp, player)
        if not self.is_dark(lamp, player):
            for item in self.objects.values():
                item.print_info()

    def is_item_present(self, item_name: str) -> bool:
        return item_name in self.objects

    def is_dark(self, lamp: Item, player) -> bool:
        return self.cond % 2 == 0 and (
                lamp.prop == 0 or not (player.has_item('lantern') or self.is_item_present('lantern')))

    def is_forced(self):
        return self.cond == 2

    def get_liquid_no(self) -> int:
        """
        OBJECT NUMBER OF LIQUID (IF ANY) AT LOC
        :return:
        """
        water = game.objects['water'].index
        oil = game.objects['oil'].index
        liq2 = lambda x: (1 - x) * water + (x // 2) * (water + oil)
        return liq2(((self.cond // 2 * 2) % 8 - 5) * (self.cond // 4 % 2 + 1))

    def bitset(self, n: int) -> bool:
        """
        TRUE IF COND(L) HAS BIT N SET (BIT 0 IS UNITS BIT)
        :return:
        """
        return bool(self.cond & (1 << n))

    def __str__(self):
        return f"{self.name}: {self.description}: {self.short_description}: {self.objects}: {self.cond}"


long_descriptions = {
    'road': '''You are standing at the end of a road before a small brick building.
Around you is a forest.
A small stream flows out of the building and down a gully.''',
    'hill': '''You have walked up a hill, still in the forest.
The road slopes back down the other side of the hill.
There is a building in the distance.''',
    'building': '''You are inside a building, a well house for a large spring.''',
    'valley.': '''You are in a valley in the forest beside a stream tumbling along a rocky bed.''',
    'forrest_valley': '''You are in open forest, with a deep valley to one side.''',
    'forrest_road': '''You are in open forest near both a valley and a road.''',
    'slit': '''At your feet all the water of the stream splashes into a 2-inch slit in the rock.
Downstream the streambed is bare rock.''',
    'grate_outside': '''You are in a 20-foot depression floored with bare dirt.
Set into the dirt is a strong steel grate mounted in concrete.
A dry streambed leads into the depression.''',
    'grate_below': '''You are in a small chamber beneath a 3x3 steel grate to the surface.
A low crawl over cobbles leads inward to the west.''',
    'cobble_crawl': '''You are crawling over cobbles in a low passage.
There is a dim light at the east end of the passage.''',
    'debris_room': '''You are in a debris room filled with stuff washed in from the surface.
A low wide passage with cobbles becomes plugged with mud and debris here, but an awkward canyon leads upward and west.
A note on the wall says "magic word xyzzy".''',
    'awkward_canyon': '''You are in an awkward sloping east/west canyon.''',
    'bird_chamber': '''You are in a splendid chamber thirty feet high.
The walls are frozen rivers of orange stone.
An awkward canyon and a good passage exit from east and west sides of the chamber.''',
    'pit': '''At your feet is a small pit breathing traces of white mist.
An east passage ends here except for a small crack leading on.''',
    'hall_of_mists_east': '''You are at one end of a vast hall stretching forward out of sight to the west.
There are openings to either side.
Nearby, a wide stone staircase leads downward.
The hall is filled with wisps of white mist swaying to and fro almost as if alive.
A cold wind blows up the staircase.
There is a passage at the top of a dome behind you.''',
    'crack': '''The crack is far too small for you to follow.''',
    'fissure_east': '''You are on the east bank of a fissure slicing clear across the hall.
The mist is quite thick here, and the fissure is too wide to jump.''',
    'nugget_room': '''This is a low room with a crude note on the wall.
The note says, "you won't get it up the steps".''',
    'hall_of_mountain_king': '''You are in the hall of the mountain king, with passages off in all directions.''',
    'broken_neck': '''You are at the bottom of the pit with a broken neck.''',
    'didnt_makeit': '''You didn't make it.''',
    'unclimable': '''The dome is unclimbable.''',
    'two_pit_west': '''You are at the west end of the twopit room.
There is a large hole in the wall above the pit at this end of the room.''',
    'pit_east': '''You are at the bottom of the eastern pit in the twopit room.
There is a small pool of oil in one corner of the pit.''',
    'pit_west': '''You are at the bottom of the western pit in the twopit room.
There is a large hole in the wall about 25 feet above you.''',
    'plant_top': '''You clamber up the plant and scurry through the hole at the top.''',
    'fissure_west': '''You are on the west side of the fissure in the hall of mists.''',
    'hole_in_floor': '''You are in a low n/s passage at a hole in the floor.
The hole goes down to an e/w passage.''',
    'south-side_chamber': '''You are in the south side chamber.''',
    'west-side_chamber': '''You are in the west side chamber of the hall of the mountain king.
A passage continues west and up here.''',
    '>$<': '''>$<''',
    'snake': '''You can't get by the snake.''',
    'y2': '''You are in a large room, with a passage to the south, a passage to the west, and a wall of broken rock to the east.
There is a large 'y2' on a rock in the room's center.''',
    'jumble_of_rocks': '''You are in a jumble of rock, with cracks everywhere.''',
    'window_over_pit': '''You're at a low window overlooking a huge pit, which extends up out of sight.
A floor is indistinctly visible over 50 feet below.
Traces of white mist cover the floor of the pit, becoming thicker to the right.
Marks in the dust around the window would seem to indicate that someone has been here recently.
Directly across the pit from you and 25 feet away there is a similar window looking into a lighted room.
A shadowy figure can be seen there peering back at you.''',
    'dirty_passage': '''You are in a dirty broken passage.
To the east is a crawl.
To the west is a large passage.
Above you is a hole to another passage.''',
    'brink_of_pit': '''You are on the brink of a small clean climbable pit.
A crawl leads west.''',
    'bottom_of_pit': '''You are in the bottom of a small pit with a little stream, which enters and exits through tiny slits.''',
    'dusty_rock': '''You are in a large room full of dusty rocks.
There is a big hole in the floor.
There are cracks everywhere, and a passage leading east.''',
    'very_low_passage': '''You have crawled through a very low wide passage parallel to and north of the hall of mists.''',
    'hall_of_mists_west': '''You are at the west end of hall of mists.
A low wide crawl continues west and another goes north.
To the south is a little passage 6 feet off the floor.''',
    'twisty_maze_1': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_2': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_3': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_4': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_5': '''Dead end''',
    'twisty_maze_6': '''Dead end''',
    'twisty_maze_7': '''Dead end''',
    'twisty_maze_8': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_9': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_10': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_11': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_12': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_13': '''Dead end''',
    'twisty_maze_14': '''You are in a maze of twisty little passages, all alike.''',
    'twisty_maze_15': '''Dead end''',
    'pit_brink': '''You are on the brink of a thirty foot pit with a massive orange column down one wall.
You could climb down here but you could not get back up.
The maze continues at this level.''',
    'dead_end': '''Dead end''',
    'crawled_through': '''You have crawled through a very low wide passage parallel to and north of the hall of mists.''',
    'long_hall_east': '''You are at the east end of a very long hall apparently without side chambers.
To the east a low wide crawl slants up.
To the north a round two foot hole slants down.''',
    'long_hall_west': '''You are at the west end of a very long featureless hall.
The hall joins up with a narrow north/south passage.''',
    'cross_over': '''You are at a crossover of a high n/s passage and a low e/w one.''',
    'dead_end_2': '''Dead end''',
    'complex_junction': '''You are at a complex junction.
A low hands and knees passage from the north joins a higher crawl from the east to make a walking passage going west.
There is also a large room above.
The air is damp here.''',
    'bedquilt': '''You are in bedquilt, a long east/west passage with holes everywhere.
To explore at random select north, south, up, or down.''',
    'swiss_cheese': '''You are in a room whose walls resemble swiss cheese.
Obvious passages go west, east, ne, and nw.
Part of the room is occupied by a large bedrock block.''',
    'twopit_east': '''You are at the east end of the twopit room.
The floor here is littered with thin rock slabs, which make it easy to descend the pits.
There is a path here bypassing the pits to connect passages from east and west.
There are holes all over, but the only big one is on the wall directly over the west pit where you can't get to it.''',
    'slab': '''You are in a large low circular chamber whose floor is an immense slab fallen from the ceiling (slab room).
East and west there once were large passages, but they are now filled with boulders.
Low small passages go north and south, and the south one quickly bends west around the boulders.''',
    'secret_canyon_above_room': '''You are in a secret n/s canyon above a large room.''',
    'secret_canyon_above_passage': '''You are in a secret n/s canyon above a sizable passage.''',
    '3_canyons_junction': '''You are in a secret canyon at a junction of three canyons, bearing north, south, and se.
The north one is as tall as the other two combined.''',
    'large_low_room': '''You are in a large low room.
Crawls lead north, se, and sw.''',
    'dead_end_crawl': '''Dead end crawl.''',
    'secret_canyon': '''You are in a secret canyon which here runs e/w.
It crosses over a very tight canyon 15 feet below.
If you go down you may not be able to get back up.''',
    'wide_place': '''You are at a wide place in a very tight n/s canyon.''',
    'tight_canyon': '''The canyon here becomes too tight to go further south.''',
    'tall_canyon': '''You are in a tall e/w canyon.
A low tight crawl goes 3 feet north and seems to open up.''',
    'boulders': '''The canyon runs into a mass of boulders -- dead end.''',
    'stream_pipe': '''The stream flows out through a pair of 1 foot diameter sewer pipes.
It would be advisable to use the exit.''',
    'maze_1': '''You are in a maze of twisty little passages, all alike.''',
    'maze_2': '''Dead end''',
    'maze_3': '''Dead end''',
    'maze_4': '''You are in a maze of twisty little passages, all alike.''',
    'maze_5': '''You are in a maze of twisty little passages, all alike.''',
    'maze_6': '''Dead end''',
    'maze_7': '''Dead end''',
    'maze_8': '''You are in a maze of twisty little passages, all alike.''',
    'narrow_corridor': '''You are in a long, narrow corridor stretching out of sight to the west.
At the eastern end is a hole through which you can see a profusion of leaves.''',
    'nothing_to_climb': '''There is nothing here to climb.
Use "up" or "out" to leave the pit.''',
    'climbed_up': '''You have climbed up the plant and out of the pit.''',
    'steep_incline': '''You are at the top of a steep incline above a large room.
You could climb down here, but you would not be able to climb up.
There is a passage leading back to the north.''',
    'giant_room': '''You are in the giant room.
The ceiling here is too high up for your lamp to show it.
Cavernous passages lead east, north, and south.
On the west wall is scrawled the inscription, "fee fie foe foo" [sic].''',
    'cave-in': '''The passage here is blocked by a recent cave-in.''',
    'immense_passage': '''You are at one end of an immense north/south passage.''',
    'cavern': '''You are in a magnificent cavern with a rushing stream, which cascades over a sparkling waterfall into a roaring whirlpool which disappears through a hole in the floor.
Passages exit to the south and west.''',
    'soft': '''You are in the soft room.
The walls are covered with heavy curtains, the floor with a thick pile carpet.
Moss covers the ceiling.''',
    'oriental': '''This is the oriental room.
Ancient oriental cave drawings cover the walls.
A gently sloping passage leads upward to the north, another passage leads se, and a hands and knees crawl leads west.''',
    'misty_cavern': '''You are following a wide path around the outer edge of a large cavern.
Far below, through a heavy white mist, strange splashing noises can be heard.
The mist rises up through a fissure in the ceiling.
The path exits to the south and west.''',
    'alcove': '''You are in an alcove.
A small nw path seems to widen after a short distance.
An extremely tight tunnel leads east.
It looks like a very tight squeeze.
An eerie light can be seen at the other end.''',
    'plover': '''You are in a small chamber lit by an eerie green light.
An extremely narrow tunnel exits to the west.
A dark corridor leads ne.''',
    'dark_room': '''You are in the dark-room.
A corridor leading south is the only exit.''',
    'arched_hall': '''You are in an arched hall.
A coral passage once continued up and east from here, but is now blocked by debris.
The air smells of sea water.''',
    'shell_room': '''You are in a large room carved out of sedimentary rock.
The floor and walls are littered with bits of shells imbedded in the stone.
A shallow passage proceeds downward, and a somewhat steeper one leadsup.
A low hands and knees passage enters from the south.''',
    'sloping_corridor_ragged': '''You are in a long sloping corridor with ragged sharp walls.''',
    'cul-de-sac': '''You are in a cul-de-sac about eight feet across.''',
    'anteroom': '''You are in an anteroom leading to a large passage to the east.
Small passages go west and up.
The remnants of recent digging are evident.
A sign in midair here says "cave under construction beyond this point.
Proceed at own risk.
[witt construction company]"''',
    'maze_different': '''You are in a maze of twisty little passages, all different.''',
    'witts_end': '''You are at witt's end.
Passages lead off in *all* directions.''',
    'mirror_canyon': '''You are in a north/south canyon about 25 feet across.
The floor is covered by white mist seeping in from the north.
The walls extend upward for well over 100 feet.
Suspended from some unseen point far above you, an enormous two-sided mirror is hanging parallel to and midway between the canyon walls.
(the mirror is obviously provided for the use of the dwarves, who as you know, are extremely vain.) a small window can be seen in either wall, some fifty feet up.''',
    'window_pit': '''You are at a low window overlooking a huge pit, which extends up out of sight.
A floor is indistinctly visible over 50 feet below.
Traces of white mist cover the floor of the pit, becoming thicker to the left.
Marks in the dust around the window would seem to indicate that someone has been here recently.
Directly across the pit from you and 25 feet away there is a similar window looking into a lighted room.
A shadowy figure can be seen there peering back at you.''',
    'stalactite': '''A large stalactite extends from the roof and almost reaches the floor below.
You could climb down it, and jump from it to the floor, but having done so you would be unable to reach it to climb back up.''',
    'little_maze_different': '''You are in a little maze of twisting passages, all different.''',
    'reservoir': '''You are at the edge of a large underground reservoir.
An opaque cloud of white mist fills the room and rises rapidly upward.
The lake is fed by a stream, which tumbles out of a hole in the wall about 10 feet overhead and splashes noisily into the water somewhere within the mist.
The only passage goes back toward the south.''',
    'dead_end_3': '''Dead end''',
    'ne_end': '''You are at the northeast end of an immense room, even larger than the giant room.
It appears to be a repository for the "adventure" program.
Massive torches far overhead bathe the room with smoky yellow light.
Scattered about you can be seen a pile of bottles (all of them empty), a nursery of young beanstalks murmuring quietly, a bed of oysters, a bundle of black rods with rusty stars on their ends, and a collection of brass lanterns.
Off to one side a great many dwarves are sleeping on the floor, snoring loudly.
A sign nearby reads: "do not disturb the dwarves!" an immense mirror is hanging against one wall, and stretches to the other end of the room, where various other sundry objects can be glimpsed dimly in the distance.''',
    'sw_end': '''You are at the southwest end of the repository.
To one side is a pit full of fierce green snakes.
On the other side is a row of small wicker cages, each of which contains a little sulking bird.
In one corner is a bundle of black rods with rusty marks on their ends.
A large number of velvet pillows are scattered about on the floor.
A vast mirror stretches off to the northeast.
At your feet is a large steel grate, next to which is a sign which reads, "treasure vault.
Keys in main office."''',
    'sw_side_of_chasm': '''You are on one side of a large, deep chasm.
A heavy white mist rising up from below obscures all view of the far side.
A sw path leads away from the chasm into a winding corridor.''',
    'sloping_corridor': '''You are in a long winding corridor sloping out of sight in both directions.''',
    'canyon_1': '''You are in a secret canyon which exits to the north and east.''',
    'canyon_2': '''You are in a secret canyon which exits to the north and east.''',
    'canyon_3': '''You are in a secret canyon which exits to the north and east.''',
    'ne_side_of_chasm': '''You are on the far side of the chasm.
A ne path leads away from the chasm on this side.''',
    'corridor': '''You are in a long east/west corridor.
A faint rumbling noise can be heard in the distance.''',
    'fork': '''The path forks here.
The left fork leads northeast.
A dull rumbling seems to get louder in that direction.
The right fork leads southeast down a gentle slope.
The main corridor enters from the west.''',
    'warm_walls': '''The walls are quite warm here.
From the north can be heard a steady roar, so loud that the entire cave seems to be trembling.
Another passage leads south, and a low crawl goes east.''',
    'breath-taking_view': '''You are on the edge of a breath-taking view.
Far below you is an active volcano, from which great gouts of molten lava come surging out, cascading back down into the depths.
The glowing rock fills the farthest reaches of the cavern with a blood-red glare, giving every- thing an eerie, macabre appearance.
The air is filled with flickering sparks of ash and a heavy smell of brimstone.
The walls are hot to the touch, and the thundering of the volcano drowns out all other sounds.
Embedded in the jagged roof far overhead are myriad twisted formations composed of pure white alabaster, which scatter the murky light into sinister apparitions upon the walls.
To one side is a deep gorge, filled with a bizarre chaos of tortured rock which seems to have been crafted by the devil himself.
An immense river of fire crashes out from the depths of the volcano, burns its way through the gorge, and plummets into a bottomless pit far off to your left.
To the right, an immense geyser of blistering steam erupts continuously from a barren island in the center of a sulfurous lake, which bubbles ominously.
The far right wall is aflame with an incandescence of its own, which lends an additional infernal splendor to the already hellish scene.
A dark, foreboding passage exits to the south.''',
    'boulder_chamber': '''You are in a small chamber filled with large boulders.
The walls are very warm, causing the air in the room to be almost stifling from the heat.
The only exit is a crawl heading west, through which is coming a low rumbling.''',
    'limestone_passage': '''You are walking along a gently sloping north/south passage lined with oddly shaped limestone formations.''',
    'barren_room_front': '''You are standing at the entrance to a large, barren room.
A sign posted above the entrance reads: "caution! bear in room!"''',
    'barren_room': '''You are inside a barren room.
The center of the room is completely empty except for some dust.
Marks in the dust lead away toward the far end of the room.
The only exit is the way you came in.''',
    'twisty_1': '''You are in a maze of twisting little passages, all different.''',
    'twisty_2': '''You are in a little maze of twisty passages, all different.''',
    'twisty_3': '''You are in a twisting maze of little passages, all different.''',
    'twisty_4': '''You are in a twisting little maze of passages, all different.''',
    'twisty_5': '''You are in a twisty little maze of passages, all different.''',
    'twisty_6': '''You are in a twisty maze of little passages, all different.''',
    'twisty_7': '''You are in a little twisty maze of passages, all different.''',
    'twisty_8': '''You are in a maze of little twisting passages, all different.''',
    'twisty_9': '''You are in a maze of little twisty passages, all different.''',
    'twisty_10': '''Dead end''',
}

short_descriptions = {
    'road': '''You are at end of road again.''',
    'hill': '''You are at hill in road.''',
    'building': '''You are inside building.''',
    'valley.': '''You are in valley.''',
    'forrest_valley': '''You are in forest.''',
    'forrest_road': '''You are in forest.''',
    'slit': '''You are at slit in streambed.''',
    'grate_outside': '''You are outside grate.''',
    'grate_below': '''You are below the grate.''',
    'cobble_crawl': '''You are in cobble crawl.''',
    'debris_room': '''You are in debris room.''',
    'bird_chamber': '''You are in bird chamber.''',
    'pit': '''You are at top of small pit.''',
    'hall_of_mists_east': '''You are in hall of mists.''',
    'fissure_east': '''You are on east bank of fissure.''',
    'nugget_room': '''You are in nugget of gold room.''',
    'hall_of_mountain_king': '''You are in hall of mt king''',
    'two_pit_west': '''You are at west end of twopit room.''',
    'pit_east': '''You are in east pit.''',
    'pit_west': '''You are in west pit.''',
    'y2': '''You are at "y2".''',
    'window_over_pit': '''You are at window on pit.''',
    'dirty_passage': '''You are in dirty passage.''',
    'dusty_rock': '''You are in dusty rock room.''',
    'hall_of_mists_west': '''You are at west end of hall of mists.''',
    'pit_brink': '''You are at brink of pit.''',
    'long_hall_east': '''You are at east end of long hall.''',
    'long_hall_west': '''You are at west end of long hall.''',
    'complex_junction': '''You are at complex junction.''',
    'swiss_cheese': '''You are in swiss cheese room.''',
    'twopit_east': '''You are at east end of twopit room.''',
    'slab': '''You are in slab room.''',
    '3_canyons_junction': '''You are at junction of three secret canyons.''',
    'secret_canyon': '''You are in secret e/w canyon above tight canyon.''',
    'narrow_corridor': '''You are in narrow corridor.''',
    'steep_incline': '''You are at steep incline above large room.''',
    'giant_room': '''You are in giant room.''',
    'cavern': '''You are in cavern with waterfall.''',
    'soft': '''You are in soft room.''',
    'oriental': '''You are in oriental room.''',
    'misty_cavern': '''You are in misty cavern.''',
    'alcove': '''You are in alcove.''',
    'plover': '''You are in plover room.''',
    'dark_room': '''You are in dark-room.''',
    'arched_hall': '''You are in arched hall.''',
    'shell_room': '''You are in shell room.''',
    'anteroom': '''You are in anteroom.''',
    'witts_end': '''You are at witt's end.''',
    'mirror_canyon': '''You are in mirror canyon.''',
    'window_pit': '''You are at window on pit.''',
    'stalactite': '''You are at top of stalactite.''',
    'reservoir': '''You are at reservoir.''',
    'ne_end': '''You are at ne end.''',
    'sw_end': '''You are at sw end.''',
    'sw_side_of_chasm': '''You are on sw side of chasm.''',
    'sloping_corridor': '''You are in sloping corridor.''',
    'ne_side_of_chasm': '''You are on ne side of chasm.''',
    'corridor': '''You are in corridor.''',
    'fork': '''You are at fork in path.''',
    'warm_walls': '''You are at junction with warm walls.''',
    'breath-taking_view': '''You are at breath-taking view.''',
    'boulder_chamber': '''You are in chamber of boulders.''',
    'limestone_passage': '''You are in limestone passage.''',
    'barren_room_front': '''You are in front of barren room.''',
    'barren_room': '''You are in barren room.''',
}

location_lookup = {  # May be useful for converting any other data that still uses numeric keys.
    1: 'road',
    2: 'hill',
    3: 'building',
    4: 'valley.',
    5: 'forrest_valley',
    6: 'forrest_road',
    7: 'slit',
    8: 'grate_outside',
    9: 'grate_below',
    10: 'cobble_crawl',
    11: 'debris_room',
    12: 'awkward_canyon',
    13: 'bird_chamber',
    14: 'pit',
    15: 'hall_of_mists_east',
    16: 'crack',
    17: 'fissure_east',
    18: 'nugget_room',
    19: 'hall_of_mountain_king',
    20: 'broken_neck',
    21: 'didnt_makeit',
    22: 'unclimable',
    23: 'two_pit_west',
    24: 'pit_east',
    25: 'pit_west',
    26: 'plant_top',
    27: 'fissure_west',
    28: 'hole_in_floor',
    29: 'south-side_chamber',
    30: 'west-side_chamber',
    31: '>$<',
    32: 'snake',
    33: 'y2',
    34: 'jumble_of_rocks',
    35: 'window_over_pit',
    36: 'dirty_passage',
    37: 'brink_of_pit',
    38: 'bottom_of_pit',
    39: 'dusty_rock',
    40: 'very_low_passage',
    41: 'hall_of_mists_west',
    42: 'twisty_maze_1',
    43: 'twisty_maze_2',
    44: 'twisty_maze_3',
    45: 'twisty_maze_4',
    46: 'twisty_maze_5',
    47: 'twisty_maze_6',
    48: 'twisty_maze_7',
    49: 'twisty_maze_8',
    50: 'twisty_maze_9',
    51: 'twisty_maze_10',
    52: 'twisty_maze_11',
    53: 'twisty_maze_12',
    54: 'twisty_maze_13',
    55: 'twisty_maze_14',
    56: 'twisty_maze_15',
    57: 'pit_brink',
    58: 'dead_end',
    59: 'crawled_through',
    60: 'long_hall_east',
    61: 'long_hall_west',
    62: 'cross_over',
    63: 'dead_end_2',
    64: 'complex_junction',
    65: 'bedquilt',
    66: 'swiss_cheese',
    67: 'twopit_east',
    68: 'slab',
    69: 'secret_canyon_above_room',
    70: 'secret_canyon_above_passage',
    71: '3_canyons_junction',
    72: 'large_low_room',
    73: 'dead_end_crawl',
    74: 'secret_canyon',
    75: 'wide_place',
    76: 'tight_canyon',
    77: 'tall_canyon',
    78: 'boulders',
    79: 'stream_pipe',
    80: 'maze_1',
    81: 'maze_2',
    82: 'maze_3',
    83: 'maze_4',
    84: 'maze_5',
    85: 'maze_6',
    86: 'maze_7',
    87: 'maze_8',
    88: 'narrow_corridor',
    89: 'nothing_to_climb',
    90: 'climbed_up',
    91: 'steep_incline',
    92: 'giant_room',
    93: 'cave-in',
    94: 'immense_passage',
    95: 'cavern',
    96: 'soft',
    97: 'oriental',
    98: 'misty_cavern',
    99: 'alcove',
    100: 'plover',
    101: 'dark_room',
    102: 'arched_hall',
    103: 'shell_room',
    104: 'sloping_corridor_ragged',
    105: 'cul-de-sac',
    106: 'anteroom',
    107: 'maze_different',
    108: 'witts_end',
    109: 'mirror_canyon',
    110: 'window_pit',
    111: 'stalactite',
    112: 'little_maze_different',
    113: 'reservoir',
    114: 'dead_end_3',
    115: 'ne_end',
    116: 'sw_end',
    117: 'sw_side_of_chasm',
    118: 'sloping_corridor',
    119: 'canyon_1',
    120: 'canyon_2',
    121: 'canyon_3',
    122: 'ne_side_of_chasm',
    123: 'corridor',
    124: 'fork',
    125: 'warm_walls',
    126: 'breath-taking_view',
    127: 'boulder_chamber',
    128: 'limestone_passage',
    129: 'barren_room_front',
    130: 'barren_room',
    131: 'twisty_1',
    132: 'twisty_2',
    133: 'twisty_3',
    134: 'twisty_4',
    135: 'twisty_5',
    136: 'twisty_6',
    137: 'twisty_7',
    138: 'twisty_8',
    139: 'twisty_9',
    140: 'twisty_10',
}
