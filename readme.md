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
    * Jet (allows dashing forward and fly over sections)
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

Krank (c) 2026
