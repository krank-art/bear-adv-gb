# Bear Adv GB

Game written in [TIC-80](https://tic80.com), an 8-bit fantasy console.

To build this project, you need the pro version of TIC-80 (available at [itch.io](https://nesbox.itch.io/tic80))
and then run `load bear-adv.lua`.

## Ideas

* Hidden areas (like a cave you get into with a pipe, will remove neutral cover once inside).
* Usage of the background as a second stage (enemies throw objects from background to foreground).
  * Platform blocks that can be switched from background to foreground
* One way platforms can be passed through by pressing down while on them
  * Foreground will change colors to a dark grey-black silhouette
* Carrying objects (like keys can be carried to doors)
* Boss fights with a healthbar
* Robot that can easily destroy stronger blocks in the game
  * Upgrades:
    * Blaster (allows shooting 3 blocks far like a shoot-em-up)
    * Chainsaw (easily slashes all enemies and destroyable blocks in front)
    * Magnet (coins and magnetic objects are pulled or pushed away)
    * Grapplehook (build up momentum and swing around corners; see Teeworlds)
    * Jet (allows dashing forward and fly over sections; or hovering for a while like Kirby; gotta repeatedly press A to gain altitude again)
    * Tank (has treads and can shoot grenades)
  * Ammunition as pickupable items; puts a limit on shooting
    * INspired from Banjo Kazooie where flying a bit upwards costs you a feather
    * Or maybe invulnerability pickups that are capped at 10 and make you invincible for 10 secs?
* Moves
  * Moving
  * Jumping
  * Ground pound
  * Dashing
  * Walljump?
* Player has a healthbar
  * Allows for more flexible types of damage
  * Maybe also upgradeable
* Collectable star coins
  * 3 per normal level
  * Spend to unlock story pages (kinda just a cosmetic reward but it adds dimension to the game)
* Destroyable environment
  * E.g. cutting a cable that holds a platform that then comes crashing down and stays there
* Collecting ship parts (30 in total for 30 courses)
  * Story is that bear crash landed on the planet and needs his spaceship to work again
  * Inspired by Pikmin 1
  * Defeating the boss at each world's end makes the radar better so a new world can be reached
* Levels should be very classical with worlds that each have levels in them with a simple overworld map
* No lives system; should the player die, lose 10 coins instead
  * Coins are saved in a total bank and are used to buy passages for new levels or useable powerups
* Enemies should have a friendly and round design
  * Enemies can drop coins or powerups upon death


## Tile index mapping in 2 BPP

* setting game to 2 bits per pixel doubles the amount of sprites
* but the map still can only represent 0 to 255 different tiles
* so when we want to draw a tile as sprite, we need to map between the index.
* Example with tile count of 4 instead of 16:
  ```
  .-- 4bpp --.  .-------- 2bpp --------.
   0  1  2  3    0  1  2  3  4  5  6  7
   4  5  6  7    8  9 10 11 12 13 14 15
   8  9 10 11   16 17 18 19 20 21 22 23
  12 13 14 15   24 25 26 27 28 29 30 31
  ```
* tid:15 = sid:27
  * `(15//4)*8 + 15%4 <=> 3*8 + 3 <=> 24 + 3 <=> 27`


## Interesting  visual glitches

`if sid~=227 then spr(sid,x*8-cam.x,y*9-cam.y,ck) end`
accidentally using 9 instead of 8 for row offset creates lines where bg shines through


## Input mapping

```
.----------------------.
| .------------------. |
| |                  | |
| |                  | |
| |                  | |
| |                  | |
| |                  | |
| '------------------' |
|    .--.              |
|  __|Up|__     (Y)    |
| |Lf    Rt|  (X)  (B) |
|  ''|Dn|''      (A)   |
|     ''               |
'----------------------'
```


| Action |  Key  | Normal        | On ladder  | In water    | In Robot          |
| :----: | :---: | ------------- | ---------- | ----------- | ----------------- |
|   Up   |   ^   | -             | Walk Up    | -           | Aim up            |
|  Down  |   v   | Duck/Fall     | Walk Down  | Submerge    | Duck/Fall/GrndPnd |
|  Left  |  <--  | Walk left     | Walk Left  | Swim Left   | Walk left         |
| Right  |  -->  | Walk right    | Walk Right | Swim Right  | Walk right        |
|   A    |   Z   | Jump          | Jump off   | Swim up     | Jump              |
|   B    |   X   | Use abl/Throw | -          | Use ability | Use ability       |
|   X    |   A   | Enter         | Enter      | Enter       | Eject (Hold)      |
|   Y    |   S   | Menu          | Menu       | Menu        | Menu              |

https://github.com/nesbox/TIC-80/wiki/key-map

We assume QWERTZ as keyboard layout.
I think TIC-80 automatically adapts to keyboard layout bc on QWERTY keyboard, Y works as Z.

Android only uses Gamepad 1 as input.
So we will map start and select as Y and X since we have no other inputs.


## First level design

Movement left and right


Krank (c) 2026
