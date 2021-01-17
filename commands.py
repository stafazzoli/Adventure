from random import randint
from typing import Tuple

from colorama import Fore, Style

import game
from data.messages import arbitrary_messages
from data.action_defaults import action_defaults
from configs import LIMIT
from data.vocabulary import vocabulary, synonyms, TYPE_ACTION


class Util:
    @staticmethod
    def print_message(msg_no: int) -> None:
        if (msg_text := arbitrary_messages.get(msg_no, None)) not in [None, '>$<']:
            Util.color_print('ARBIT', str(msg_no) + ' ' + msg_text)

    @staticmethod
    def pct(n: int) -> bool:
        return randint(0, 100) <= n

    @staticmethod
    def color_print(msg_type: str, msg: str) -> None:
        color = Fore.LIGHTRED_EX
        if msg_type == 'LOC_INFO':
            color = Fore.MAGENTA
        elif msg_type == 'ITEM_INFO':
            color = Fore.CYAN
        elif msg_type == 'ARBIT':
            color = Fore.BLUE
        elif msg_type == 'WARN':
            color = Fore.RED

        print(color, msg, Style.RESET_ALL, sep='')

    @staticmethod
    def get_default_msg_no(verb: str) -> int:
        vocabulary.get(verb, None)
        if v := vocabulary.get(verb, None):
            return action_defaults[v[1]]

    @staticmethod
    def get_liquid_no() -> int:
        water = game.objects['water'].index
        oil = game.objects['oil'].index
        bottle = game.objects['bottle']
        liq2 = lambda x: (1 - x) * water + (x // 2) * (water + oil)
        liq = liq2(max(bottle.prop, -1 - bottle.prop))
        return liq

    @staticmethod
    def check_synonyms(word: str) -> str:
        """
        Returns the base word for a synonym.
        The return value will be casefolded.
        """
        lookup = word.casefold()
        result = synonyms.get(lookup, lookup)
        return result

    @staticmethod
    def get_word_info(word: str) -> Tuple[str, str]:
        syn_word = Util.check_synonyms(word)
        word_info = vocabulary.get(syn_word, None)
        if not word_info:
            return None
        word_type, main_word = word_info
        if word_type == TYPE_ACTION:
            return word_type, syn_word
        else:
            return word_type, main_word

    @staticmethod
    def get_response() -> str:
        return input('').casefold()

    @staticmethod
    def get_item_by_index(index: int):
        return next((v for k, v in game.objects.items() if v.index == index), None)


class Command:
    @classmethod  # 9170
    def throw(cls, **kwargs):
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        # IF(TOTING(ROD2).AND.OBJ.EQ.ROD.AND..NOT.TOTING(ROD))OBJ=ROD2
        spk = Util.get_default_msg_no('throw')
        if not item:
            raise NotImplementedError('#8000')
        elif not player.has_item(item_name):
            Util.print_message(spk)
            return

        if item.is_treasure and location.is_item_present('troll'):
            spk = 159
            player.drop_item(item)
            item.move(None)
            item.move(None, True)
            phony_troll = game.object['phony troll']
            phony_troll.move(item.initial_location[0])
            phony_troll.move(item.initial_location[1])
            Util.print_message(spk)

    #   CALL DROP(OBJ,0)
    # 	CALL MOVE(TROLL,0)
    # 	CALL MOVE(TROLL+100,0)
    # 	CALL DROP(TROLL2,PLAC(TROLL))
    # 	CALL DROP(TROLL2+100,FIXD(TROLL))
    # 	CALL JUGGLE(CHASM)
    # 	GOTO 2011

    @classmethod  # 9090
    def wave(cls, **kwargs):
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        spk = Util.get_default_msg_no('wave')
        if not player.has_item(item_name):
            spk = 29
        if item_name != 'rod' or not location.is_item_present('fissure') or not player.has_item('rod'): # .OR.CLOSNG
            Util.print_message(spk)
            return
        fissure = game.objects['fissure']
        fissure.prop = 1 - fissure.prop
        fissure.print_message(2 - fissure.prop)

    @classmethod  # 9130
    def pour(cls, **kwargs):
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        spk = Util.get_default_msg_no('pour')
        if not item or item_name == 'bottle':
            item = Util.get_item_by_index(Util.get_liquid_no())

        if not item:
            raise NotImplementedError('#8000')
        elif not player.has_item(item_name):
            Util.print_message(spk)
            return

        spk = 78
        if item_name != 'water' and item_name != 'oil':
            Util.print_message(spk)
            return
        game.objects['bottle'].prop = 1
        player.drop_item(item)
        item.move(None)
        spk = 77
        if not (location.is_item_present('plant') or location.is_item_present('door')):
            Util.print_message(spk)
            return
        elif location.is_item_present('door'):
            door = game.objects['door']
            door.prop = 0
            if item_name == 'oil':
                door.prop = 1
            spk = 113 + door.prop
            Util.print_message(spk)
            return
        elif item_name != 'water':
            Util.print_message(112)
            return
        else:
            plant, phony_plant = game.objects['plant'], game.objects['phony plant']
            plant.print_message(plant.prop + 1)
            plant.prop = plant.prop + 2 % 6
            phony_plant.prop = plant.prop // 2
        # 	K=NULL
        # 	GOTO 8

    @classmethod  # 9120
    def kill(cls, **kwargs):
        player = kwargs['player']
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        spk = Util.get_default_msg_no('kill')
        if item:
            if item.name == 'bird':
                raise NotImplementedError('#9124')
            else:
                if item_name == 'clam' or item_name == 'oyster':
                    spk = 150
                elif item_name == 'snake':
                    spk = 46
                elif item_name == 'dwarf':
                    spk = 49
                elif item_name == 'dwarf':
                    # AND.CLOSED)GOTO 19000
                    pass
                elif item_name == 'dragon':
                    spk = 167
                elif item_name == 'troll':
                    spk = 157
                elif item_name == 'bear':
                    spk = 165 + (item.prop + 1) / 2

                if item_name != 'dragon' or (item_name == 'dragon' and item.prop != 0):
                    Util.print_message(spk)
                    return
                else:
                    Util.print_message(49)
                    resp = Util.get_response()
                    if resp not in ('y', 'yes'):
                        return
                    item.print_message(1)
                    item.prop = 2
                    game.objects['rug'].prop = 0
                    k = sum(game.locations[loc].index for loc in item.locations) // 2
                    # CALL MOVE(DRAGON+100,-1)
                    # CALL MOVE(RUG+100,0)
                    # item.move(None, True) # destroy dragon
                    game.objects['rug'].immovable = False
                    loc = next(iter(val for key, val in game.locations.items() if val.index == k))
                    item.move(loc.name, True)
                    game.objects['rug'].move(loc.name, True)
                    player.move(loc)

    @classmethod  # 9070
    def on(cls, **kwargs):
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)

        spk = Util.get_default_msg_no('on')
        if not (player.has_item('lantern') or location.is_item_present('lantern')):
            Util.print_message(spk)
            return

        if LIMIT < 0:
            Util.print_message(184)
            return

        if not item or item.name == 'lantern':
            item.prop = 1
            Util.print_message(39)

        # IF(WZDARK)GOTO 2000

    @classmethod  # 9080
    def off(cls, **kwargs):
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)

        spk = Util.get_default_msg_no('off')
        if not (player.has_item('lantern') or location.is_item_present('lantern')):
            Util.print_message(spk)
            return

        if not item or item.name == 'lantern':
            item.prop = 0
            Util.print_message(40)

    @classmethod  # 9220
    def fill(cls, **kwargs) -> None:
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        spk = Util.get_default_msg_no('fill')
        if item_name == 'vase':  # 9222
            spk = 29
            if location.get_liquid_no() == 0:
                spk = 144
            if location.get_liquid_no() == 0 or not player.has_item(item_name):
                Util.print_message(spk)
                return
            Util.print_message(spk)
            item.prop = 2
            item.immovable = True
            raise NotImplementedError('GOTO #9024')
        elif item and item.name != 'bottle':
            Util.print_message(spk)
            return
        elif not item and not (player.has_item('bottle') or location.is_item_present('bottle')):
            raise NotImplementedError('GOTO #8000')

        spk = 107
        if location.get_liquid_no() == 0:
            spk = 106

        if Util.get_liquid_no() != 0:
            spk = 105
        if spk != 107:
            Util.print_message(spk)
            return
        bottle = game.objects['bottle']
        bottle.prop = (location.cond % 4) // 2 * 2
        liq_item = Util.get_item_by_index(Util.get_liquid_no())
        if player.has_item('bottle'):
            player.take_item(liq_item)
        if liq_item.name == 'oil':
            spk = 108
        Util.print_message(spk)
        return

    @classmethod  # 8010, 9010
    def get(cls, **kwargs) -> None:
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        if not item and location.objects:
            item = next(iter(v for k, v in location.objects.items()))  # get the first item available in location

        if not item:
            raise NotImplementedError('#8010')
        elif item and player.has_item(item_name):
            Util.print_message(Util.get_default_msg_no('get'))
            return
        else:
            if item_name == 'plant' and item.prop < 0:
                spk = 115
            elif item_name == 'bear' and item.prop == 1:
                spk = 169
            elif item_name == 'chain' and game.objects['bear'].prop == 0:
                spk = 170
            elif item.immovable:
                print(f'The {item_name} is fixed in place.')
                return
            elif item_name == 'water' or item_name == 'oil':
                raise NotImplementedError('Not implemented in open()')
                # elif player.has_item('bottle') or location.is_item_present('bottle') and Util.get_liquid_no() == item.index:
                #     pass
            else:
                if player.item_count() < 7:
                    if item_name != 'bird' or (item_name == 'bird' and item.prop != 0):
                        if (item_name == 'bird' or item_name == 'cage') and game.objects['bird'].prop != 0:
                            bird_index, cage_index = game.objects['bird'].index, game.objects['cage'].index
                            item_take = Util.get_item_by_index(bird_index + cage_index - item.index)
                            player.take_item(item_take)
                        player.take_item(item)
                        if item_name == 'bottle' and (liq_no := Util.get_liquid_no()) != 0:
                            liquid_to_take = Util.get_item_by_index(liq_no)
                            player.take_item(liquid_to_take)
                        Util.print_message(54)
                        return

                    elif not (player.has_item('rod')):
                        if player.has_item('cage'):
                            item.prop = 1
                            if (item_name == 'bird' or item_name == 'cage') and game.objects['bird'].prop != 0:
                                bird_index, cage_index = game.objects['bird'].index, game.objects['cage'].index
                                item_take_index = bird_index + cage_index - item.index
                                item_take = Util.get_item_by_index(item_take_index)
                                player.take_item(item_take)
                            player.take_item(item)
                            if item_name == 'bottle' and (liq_no := Util.get_liquid_no()) != 0:
                                liquid_to_take = Util.get_item_by_index(liq_no)
                                player.take_item(liquid_to_take)
                            Util.print_message(54)
                            return
                        else:
                            Util.print_message(27)
                            return
                    else:
                        Util.print_message(26)
                        return
                else:
                    Util.print_message(92)
                    return
        Util.print_message(spk)

    @classmethod  # 9020 with/ 8000 no without item
    def drop(cls, **kwargs) -> None:
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        # IF(TOTING(ROD2).AND.OBJ.EQ.ROD.AND..NOT.TOTING(ROD))OBJ=ROD2
        spk = Util.get_default_msg_no('drop')
        if not player.has_item(item_name):
            Util.print_message(spk)
            return

        if not (item_name != 'bird' or not (location.is_item_present('snake'))):
            Util.print_message(30)
            player.destroy_item('snake')
            game.objects['snake'].prop = 1
            k = Util.get_liquid_no()
            if k == item.index:
                item_name = 'bottle'
            if item_name == 'bottle' and k != 0:
                pass
                # PLACE(K) = 0
            if item_name == 'cage' and game.objects['bird'].prop != 0:
                player.drop_item(game.objects['bird'])

            if item_name == 'bird':
                game.objects['bird'].prop = 0

            player.drop_item(item)
            return

        else:
            if not (item_name != 'coins' or not (player.has_item('vend') or location.is_item_present('vend'))):
                player.destroy_item('coins')
                player.drop_item(game.objects['batter'])
                # CALL PSPEAK(BATTER,0)
            else:
                if not (item_name != 'bear' or not location.is_item_present('troll')):
                    raise NotImplementedError('#9026')
                    # 9026	IF(OBJ.NE.BEAR.OR..NOT.AT(TROLL))GOTO 9027
                    # 	CALL RSPEAK(163)
                    # 	CALL MOVE(TROLL,0)
                    # 	CALL MOVE(TROLL+100,0)
                    # 	CALL MOVE(TROLL2,PLAC(TROLL))
                    # 	CALL MOVE(TROLL2+100,FIXD(TROLL))
                    # 	CALL JUGGLE(CHASM)
                    # 	PROP(TROLL)=2
                    # 	GOTO 9021
                else:
                    if not(item_name == 'vase' and not location.is_item_present('pillow')):
                        Util.print_message(54)
                        k = Util.get_liquid_no()
                        if k == item.index:
                            item_name = 'bottle'
                        if item_name == 'bottle' and k != 0:
                            pass
                            # PLACE(K) = 0
                        if item_name == 'cage' and game.objects['bird'].prop != 0:
                            player.drop_item(game.objects['bird'])

                        if item_name == 'bird':
                            game.objects['bird'].prop = 0

                        player.drop_item(item)
                        return
                    else:
                        raise NotImplementedError('#9028')

    @classmethod # 8040, 9040
    def open(cls, opposite=False, **kwargs):
        player = kwargs['player']
        location = player.location
        item = kwargs.get('item', None)
        item_name = item.name if item else None

        if not item_name:  # 8040
            if player.has_item('clam') or location.is_item_present('clam'):
                item_name = 'clam'
            elif player.has_item('oyste') or location.is_item_present('oyste'):
                item_name = 'oyste'
            elif location.is_item_present('door'):
                item_name = 'door'
            elif location.is_item_present('grate'):
                item_name = 'grate'
            # IF(OBJ.NE.0.AND.HERE(CHAIN))GOTO 8000
            elif player.has_item('chain') or location.is_item_present('chain'):
                item_name = 'chain'
            else:
                Util.print_message(28)
                return
        item = player.location.objects[item_name]

        if item_name in ['clam', 'oyste']:  # 9046
            spk = 124  # A GLISTENING PEARL FALLS OUT OF THE CLAM AND ROLLS AWAY. GOODNESS, THIS MUST REALLY BE AN OYSTER. (I NEVER WAS VERY GOOD AT IDENTIFYING BIVALVES.) WHATEVER IT IS, IT HAS NOW SNAPPED SHUT AGAIN.
            k = 0
            if item_name == 'oyste':
                k = 1
                spk = 125  # THE OYSTER CREAKS OPEN, REVEALING NOTHING BUT OYSTER INSIDE. IT PROMPTLY SNAPS SHUT AGAIN.
            if player.has_item(item_name):
                spk = 120 + k
            if not player.has_item('trident'):
                spk = 122 + k  # YOU DON'T HAVE ANYTHING STRONG ENOUGH TO OPEN THE CLAM.
            if opposite:
                spk = 61  # WHAT?

            if spk != 124:
                Util.print_message(spk)
                return
            else:
                player.destroy_item('clam')
                # CALL DROP(OYSTER, LOC)
                # CALL DROP(PEARL, 105)
                Util.print_message(spk)
                return
        elif item_name == 'door' and item.prop != 1:
            spk = 111
        elif item_name == 'door' and item.prop == 1:
            spk = 54
        elif item_name == 'cage':
            spk = 32
        elif item_name == 'keys':
            spk = 55
        elif item_name == 'grate' or item_name == 'chain':
            spk = 31

        if spk != 31 or not (player.has_item('keys') or location.is_item_present('keys')):
            Util.print_message(spk)
            return

        if item_name == 'chain':
            if not opposite:  # 9048
                spk = 171
                bear = location.get('bear', None)
                if bear and bear.prop == 0:
                    spk = 41
                if item.prop == 0:
                    spk = 31
                if spk != 171:
                    Util.print_message(spk)
                    return
                item.prop = 0
                # FIXED(CHAIN) = 0
                if bear.prop != 3:
                    bear.prop = 2
                # FIXED(BEAR) = 2 - PROP(BEAR)
                Util.print_message(spk)
                return
            else:  # 9049
                spk = 172
                if item.prop != 0:
                    spk = 34
                # IF(LOC.NE.PLAC(CHAIN))SPK=173
                if spk != 172:
                    Util.print_message(spk)
                    return
                item.prop = 2
                if player.has_item('chain'):
                    player.drop_item(item)
                # FIXED(CHAIN) = -1
                Util.print_message(spk)
                return

        # IF(.NOT.CLOSNG)GOTO 9043
        if item_name == 'grate':
            spk = 34 + item.prop
            item.prop = 0 if opposite else 1
            spk += 2 * item.prop
            Util.print_message(spk)
            return

        # 	GOTO 2010

        # K = 130
        # IF(.NOT.PANIC)CLOCK2 = 15
        # PANIC =.TRUE.
        Util.print_message(spk)
        return

    @classmethod  # 8040, 9040
    def lock(cls, **kwargs):
        cls.open(opposite=True, **kwargs)

    @classmethod
    def look(cls, **kwargs) -> None:
        location = kwargs['player'].location
        if game.detail < 3:
            Util.print_message(15)
        game.detail += 1
        location.abb_desc_no = 0
        #WZDARK=.FALSE.
