-- title:   game title
-- author:  game developer, email, etc.
-- desc:    short description
-- site:    website link
-- license: MIT License (change this to your license of choice)
-- version: 0.1
-- script:  lua

-- set special collission rules for tiles where
--  the dir(ection) shows which edge of tile is solid
--  e.g. N (north) means the top edge is solid
DIR = {N=1,E=2,S=4,W=8}

-- direction collision tiles flags
--  only NORTH implemented so far
dirColTls={
 [136]=4,[137]=4,[138]=4,[142]=4,[143]=4,
 [187]=8,[188]=2,
}

-- sound lookup, jumping, bumping head
snd={
 jmp=32,
 bmp=33,
 coin={34,60,40},
 gear={34,55,40},
}

-- solid tiles index min, solid tiles index max, 
--  max jump timer, jump velocity, gravity
cfg={solidMin=1,solidMax=95,maxJmpTmr=28,jmpVel=2,G=2}

-- movement type: Running, Ladder (Climbing), Swimming
MOV={R=1,L=2,S=4}

--[[
   v head
.-----.
|     | < collider
|     |
'--o--'
   ^(x,y) player origin (feet)
]]--
plr={
 x=96,y=24, -- pos x,y
 lx=nil,ly=nil, -- last pos x,y
 vx=0,vy=0, -- velocity x,y
 w=10,h=20, -- width, height
 cx=-5,cy=-20, -- collider pos x,y
 hr=20,hc=14, -- height running, height crouching
 cyr=-20,cyc=-14, -- collider offset y running, crouching
 onGrd=false,onCeil=false, -- onGround, onCeiling
 onLdr=false,onLdrTop=false, -- onLadder, onLadderTop
 mov=MOV.R,crch=false, -- movementMode, crouching
 jmpTmr=0,jmpRls=true,jmpSnd=false, -- jump timer, released, sound
 flp=false, -- flipped
}

lvl={x=0,y=0,w=90,h=34,coins=0,gears=0}

-- global camera
-- pos x, pos y, offset x, offset y, target x, target y, 
--  cam region, velocity
cam={x=0,y=0,dx=0,dy=0,tx=0,ty=0,box={x=0,y=0,w=1920,h=136},v=5}

-- entity behaviour
bhv={"sblk"}

-- {tile id, [duration], [tile x], [tile y], [tile width], [tile height]},
-- {collider x, collider y, collider width, collider height},
--  entity behaviour
entDefs={
 -- surprise block
 --sblk={{0,0,12,0,0,2,2},{0,0,16,16},"sblk"}
 coin={
  ani={{18,16,0,0,2,2},{20,8},{22},{24},{26},{28}},
  tml={}, -- timeline, generated on load
  cani={}, -- constant animation, generated on load
  def=nil, -- shorthand for entity definition
  aniFlg=0, -- animation flags, flagged enum; see drwEnt()
  col={0,0,16,16}, -- collider
  bhv={"coin"}, -- behaviour
  dead=false, -- marked so at the end of frame it can be removed
 },
 gear={
  ani={{82,90,0,0,2,2},{80,6},{82,16},{84,8},{86},{88}},
  col={0,0,16,16},
  aniFlg=1,
  bhv={"gear"}, -- behaviour
 }
}

-- collissions, updates each frame
cols={}

-- entities in scene
-- during build, entities get a unique id ("n$")
-- where $ is the index. remember to use entCnt
-- upon inserting new entries
ents={
 --set entities via map flags
 --{id="coin",x=16,y=32,ast={}},
 --{id="coin",x=32,y=32},
 --{id="gear",x=200,y=48},
 -- during runtime only append entities at the end bc
 --  collision resolution is dependent on index here
}
-- entity counter
entCnt=-1

t=0

cookie=nil -- Turbine's cookie

-- transparency mask; bit mask from 0 to 1023 for 
--  tiles where the black color should be drawn as transparent
trnsMask=nil

-- ladder mask
ldrMsk=nil

-- intro step
intStp=0

function BOOT() 
 trnsMask=maskNew(16,{227,320,321,322,323,352,353,354,355})
 ldrMsk=maskNew(4,{139,140,141,142,143,155,156,157,158,159,171,172,173})
 bldCnstAniAll(entDefs)
 loadMap() -- loading map can add new entities
 loadEnts(ents)
 -- https://github.com/nesbox/TIC-80/wiki/blit-segment
 -- to access page two in tileset, we need to set to 5
 poke4(2*0x3ffc,4) -- set to 2 bits per pixel
 music(0)
end

function TIC()
 cls(13)
 updPlr()
 updCol()
 updEnts()
 updCam()
 --plyInt()
 --map(0,17,30,17,cam.x//10,cam.y//10) --background
 drwMapRpt(0,17,30,17,cam.x//10,cam.y//10) --background
 drwMap() --foreground
 --print("HELLO WORLD!",84,84)
 print(tableToString(plr),2,10,15,false,1,true)
 print(frmt("coins: %d / gears: %d",lvl.coins,lvl.gears),2,2,15)
 --print(tableToString(cam))
 drwEnts()
 drwPlr()
 lateUpd()
 t=t+1
end

-- late update
function lateUpd()
 for key,e in pairs(ents) do
  -- mark dead entities ready for GC
  if e.dead then ents[key]=nil end
 end
end

-- play sound
function plySnd(id)
 local snd=snd[id]
 if not snd then return end
 local sid,n,d=table.unpack(snd) -- sound id, note, duration
 sfx(sid,n,d)
end

function drwPlr()
 local flp=plr.flp and 1 or 0
 local sx,sy=plr.x-cam.x,plr.y-cam.y -- sprite x and y
 if plr.crch then
  spr(806,sx-8,sy-16,1,1,flp,0,2,2)
 else
  spr(770,sx-8,sy-24,1,1,flp,0,2,3)
 end
 --rectb(plr.x+plr.cx-cam.x, plr.y+plr.cy-cam.y, plr.w, plr.h, 4)
end

function updCam()
 -- cam controls is special in that in x axis,
 --  camera scrolls a little forward so the player
 --  can see where they are running to. In y axis,
 --  the camera follows a safety margin (safe action),
 --  so when falling down, the player is at the bottom
 --  of the action area and can jump without cam moving.
 -- TODO: drag camera to smooth motion and give sudden
 --  stops more punch (overshoot for a few frames)

 local px=plr.x-120 -- x basis is centered player pos
 local dx=cam.dx+plr.x-plr.lx -- x offset based on player movement

 local q,dy=24,0 --y padding, offset (delta) y
 local cy1,cy2=cam.ty+plr.h+q,cam.ty+136-q
 if plr.y<cy1 then dy=plr.y-cy1
 elseif plr.y>cy2 then dy=plr.y-cy2 end

 -- clamp offset
 cam.dx=clmp(dx,-40,40) 
 cam.dy=clmp(dy,-16,16) --hard cap

 -- y: apply coords
 cam.ty=cam.ty+cam.dy
 cam.tx=px+cam.dx 

 -- clamp coords to region
 local cbx=cam.box
 if cbx then
  local bx1, by1, bx2, by2=
   cbx.x, cbx.y, cbx.x+cbx.w, cbx.y+cbx.h
  if (plr.x+plr.cx)>=bx1 and (plr.x+plr.cx+plr.w)<bx2 and 
    (plr.y+plr.cy)>=by1 and (plr.y+plr.cy+plr.h)<by2 then
   cam.tx=clmp(cam.tx,bx1,bx2-240)
   cam.ty=clmp(cam.ty,by1,by2-136)
  end
 end

 -- move towards cam target
 local bx,by=cam.tx-cam.x,cam.ty-cam.y -- target delta
 local d,v=sqrt(bx*bx+by*by),cam.v --distance, velocity
 if d>0 then
  if d<=v then
   cam.x=cam.tx
   cam.y=cam.ty
  else
   cam.x=cam.x+bx/d*v
   cam.y=cam.y+by/d*v
  end
 end
 --print(frmt("cx:%d,cy:%d",cam.x,cam.y),56,2)
end

-- update entitites
function updEnts()
 -- collision detection is setup only for the player
 --  so far, so entities will not collide with each
 for i,key in pairs(cols) do
  local ent=ents[key]
  local def=ent.def
  local bhv=def.bhv
  for j,trait in pairs(bhv) do
   if trait=="coin" then 
    sfx(table.unpack(snd.coin))
    lvl.coins=lvl.coins+1
    ent.dead=true
   end
   if trait=="gear" then 
    plySnd("gear")
    --sfx(table.unpack(snd.gear))
    lvl.gears=lvl.gears+1
    ent.dead=true
   end
  end
 end
end

function drwEnts()
 for i,e in pairs(ents) do drwEnt(e) end
end

function drwEnt(e)
 local id, ast, x, y = e.id, e.ast, e.x, e.y
 local def=e.def
 local flg=def.aniFlg or 0
 if flg > 0 then
  -- if flag 1 is set, move up and down visually
  if flg&1~=0 then y=y-(sin(t*0.15)*2) end
 end
 local tml,cani=def.tml,def.cani,def
 plyAni(cani,tml,ast,x-cam.x,y-cam.y)
end

function loadEnts(ents)
 entCnt=#ents+1
 for i,e in ipairs(ents) do
  -- change index to absolute ones so we can
  --  remove items without order breaking
  ents["n"..i]=e
  ents[i]=nil

  -- add definition shorthand
  e.def=entDefs[e.id]

  -- prepare for animation
  e.ast={ -- ast = animation state
   el=0 -- elapsed ticks
  }
 end
end

-- build constant animation for all entity definitions
function bldCnstAniAll(ent)
 for entName,data in pairs(ent) do
  local tml,cani=bldCnstAni(data.ani)
  local cur=entDefs[entName]
  cur.tml=tml
  cur.cani=cani
 end
end


-- build constant animation
function bldCnstAni(ani)
 -- timeline
 local tml,t,dur = {0},0,0
 for i,frm in ipairs(ani) do
  -- if duration present in this frame, update it
  if frm[2]~=nil then dur=frm[2] end
  t=t+dur
  tml[#tml+1]=t
 end

 -- constant frames (have full data):
 --  the regular frames are written a compressed way
 --  to save space by only specifying changes in
 --  e.g.: ani={{18,4,0,0,2,2},{20},{22},{20}}
 local cani={} -- constant animation
 local f=ani[1] -- first frame
 -- tile id, duration, x, y, tile width, tile height
 local t,d,x,y,w,h=f[1],f[2],f[3],f[4],f[5],f[6]
 for i,c in ipairs(ani) do
  -- c is current frame
  t,d,x=c[1] or t, c[2] or d, c[3] or x
  y,w,h=c[4] or y, c[5] or w, c[6] or h
  cani[i]={t,d,x,y,w,h}
 end

 return tml,cani
end

-- loadMap()
function loadMap()
 --mapTrns=getMapDrwChks(0,0,30,17)
 local function addEnt(eid,i,j)
  table.insert(ents, {id=eid,x=i*8,y=j*8})
  entCnt=#ents+1
  local n=mget(i+1,j) -- neighboring tile
  mset(i,j,n)
 end
 local onTile = {
  [204] = function(tid,i,j) addEnt("coin",i,j) end,
  [205] = function(tid,i,j) addEnt("gear",i,j) end,
 }
 local x1,x2,y1,y2=lvl.x,lvl.x+lvl.w-1,lvl.y,lvl.y+lvl.h-1
 for i=x2,x1,-1 do -- we iterate in reverse so you can stack entities on top
  for j=y2,y1,-1 do -- upon replacing the entity marker, the right tile gets copied over
   local tid=mget(i,j)
   local fn=onTile[tid]
   if fn then fn(tid,i,j) end
  end
 end
end

-- get map draw chunks; scans the map in 6x6 chunks
-- returns a 2D dictionary: chunks[cx][cy] = true
-- when true, draw each tile in chunk as sprite; otherwise as map
function getMapDrwChks(mx,my,w,h)
 local chks = {} -- chunks
 local cs=6 -- chunk size
 for cy=my,h-1,cs do
  for cx=mx,w-1,cs do
   local fnd=false -- found
   for y=0,cs-1 do -- scan inside chunk
    for x=0,cs-1 do
     local tx,ty = cx+x,cy+y
     if tx<w and ty<h then
      if mget(tx,ty)==86 then fnd=true break end
     end
    end
    if fnd then break end
   end
   if fnd then -- store chunk coords if tile found
    local i,j = cx//cs,cy//cs
    chks[i] = chks[i] or {}
    chks[i][j] = true
   end
  end
 end
 return chks
end

-- draw map
function drwMap()
 local q=8 -- padding
 -- render area absolute coords
 local rx1,ry1,rx2,ry2=cam.x-q,cam.y-q,cam.x+240+q,cam.y+136+q
 -- render area tile coords
 local mx1,my1,mx2,my2=flr(rx1/8),flr(ry1/8),cil(rx2/8),cil(ry2/8)
 for y=my1,my2-1 do
  for x=mx1,mx2-1 do
   local dx,dy=rx1*8,ry1*8
   local tid=mget(x,y)
   local sid=(tid//16*32)+tid%16
   local ck=maskHas(trnsMask,sid) and 0 or -1 -- color key
   if sid~=227 then spr(sid,x*8-cam.x,y*8-cam.y,ck) end
   --error(frmt("tx=%d, ty=%d, tid=%d, sid=%d",tx,ty,tid,sid))
  end
 end
end

-- draw map repeat
-- map x, map y, map width, map height, draw x, draw y, [color key=-1]
function drwMapRpt(mx,my,mw,mh,x,y,ck)
 local ck=ck or -1
 local ox,oy=mw*8,mh*8 -- offset
 -- modulo coords so only offset from repeating map needs to be drawn
 local x,y=x%ox,y%oy
 local mi,mj=cil(30/mw),cil(30/mh) -- map draws per x axis, per y axis
 for i=0,mi do -- we do one extra draw repeat bc offset
  for j=0,mj do
   -- TODO: we could optimize by limiting draw in first and last 
   --  iteration to really only draw visible tiles, but for now 
   --  we will keep this dumb implementation
   map(mx,my,mw,mh,x+i*mw*8-ox,y+j*mh*8-oy,ck)
  end
 end
end

-- bitmask create
function maskNew(size,args)
  -- integers in TIC-80 Lua are 64bit, so 16 ints
  --  can represent 0...1023. in 2 bits per pixel,
  --  this covers the entire spritesheet
 local size,mask=size or 16,{}
 for i=1,size do mask[i]=0 end
 for _, v in ipairs(args) do maskAdd(mask,v) end
 return mask
end

function maskAdd(mask, v) --add value from bitmask
 local idx=flr(v/64)+1
 local bit=v%64
 mask[idx]=mask[idx]|(1<<bit)
end

function maskDel(mask, v) --remove value from bitmask
 local idx=flr(v/64)+1
 local bit=v%64
 mask[idx]=mask[idx]&~(1<<bit)
end

function maskHas(mask, v) --check if a value is in mask
 local idx=flr(v/64)+1
 local bit=v%64
 return (mask[idx]&(1<<bit))~=0
end

-- play intro
function plyInt()
 music(-1)
 sync(1|2,1) -- switch tiles and sprites to bank 1
 cls(0)
 local sprIdx={0,160,320,512,672,832}
 spr(sprIdx[intStp+1],40,20,-1,2,0,0,10,5)
 local lines={
  {"Bear was happy to go to space!",""},
  {"Off you go little bear!","Adventure is out there!"},
  {"He reaches space and just... WOW","The view is breathtaking."},
  {"Do you see that out the window?","Oh no, it's an-- ASTEROID!!!."},
  {"ARGHHHH!!",""},
  {"This wasn't supposed to happen.","Poor bear, I hope he survived."},
 }
 local ln=lines[intStp+1]
 for i=1,2 do
  local w=print(ln[i],0,-60)
  print(ln[i],(240-w)//2,100+i*10,3,false,1)
 end
 if btnp(4,16,16) then intStp=(intStp+1)%6 end
end

-- play animation (constant animation, timeline,
--  animation state, x, y)
function plyAni(cani,tml,state,x,y)
 local el=state.el -- elapsed ticks
 local dur=tml[#tml] -- total animation duration
  
 -- get current frame to play
 local curFrmIdx
 for i=#tml,1,-1 do
  --error(tableToString(tml[i]))
  if el>=tml[i] then curFrmIdx=i break end
 end

 -- draw sprite
 local f=cani[curFrmIdx]
 local t,d,fx,fy,w,h=f[1],f[2],f[3],f[4],f[5],f[6]
 spr(t,x+fx,y+fy,1,1,0,0,w,h)
 --error(tableToString(cani[2]))
 --print(curFrmIdx)
 
 -- increase animation counter
 el=(el+1)%dur
 state.el=el
 return el
end

-- check if player is anywhere on a ladder
function chkPlrLdr()
 return chkPlrTil(function (tile,x,y) 
  return maskHas(ldrMsk,tile)
 end)
end

-- check if player is on top of ladder
function chkPlrLdrTop()
 if not plr.onGrd then return false end
 local px,py,pw,ph,pcx,pcy=plr.x,plr.y,plr.w,plr.h,plr.cx,plr.cy
 local x1,x2,y=px+pcx,px+pcx+pw-1,py+pcy+ph
 local i1,i2,j=x1//8,x2//8,y//8
 for i=i1,i2 do
  -- triggers also on grid that has no top collission
  --  is probably fine that player could move one pxl down
  if maskHas(ldrMsk,mget(i,j)) then return true end
 end
 return false
end

function chkPlrGrd()
 local px,py,w,h,cx,cy=plr.x,plr.y,plr.w,plr.h,plr.cx,plr.cy
 local x1,x2,y=px+cx,px+cx+w-1,py+cy+h
 for i=x1,x2 do
  if isSld(i,y+1,DIR.S) 
  then return true end
 end
 return false
end

function chkPlrCeil()
 local px,py,w,cx,cy=plr.x,plr.y,plr.w,plr.cx,plr.cy
 local x1,x2,t=px+cx,px+cx+w-1,py+cy --t for top
 for i=x1,x2 do
  if isSld(i,t-1) then return true end
 end
 return false
end

function jmpPlr()
 local jmpDwn = btn(4)
 plr.vy=0

 -- update release flag
 if not jmpDwn then
  plr.jmpRls = true  -- button released, next jump allowed
 end
 
 -- start jump if jump button was released and on ground or on ladder
 if jmpDwn and plr.jmpRls and (plr.onGrd or plr.mov==MOV.L) then
  plr.mov=MOV.R -- set to running after jumping off ladder
  plr.jmpTmr = 1
  plr.vy = -cfg.jmpVel
  plr.jmpRls = false  -- prevents immediate auto-repeat
  plr.jmpSnd=true
  sfx(32,40,-1)
 end

 -- handle ladder climbing
 if plr.mov==MOV.L then
  plr.vy=0
  if btn(0) then return -1 end
  if btn(1) then return 1 end
 end
 
 if plr.onCeil and plr.jmpTmr > 0 then
  plr.jmpSnd=false
  sfx(snd.bmp,26,24)
 end
 
 -- continue jump while button held and under max jump time
 if jmpDwn and plr.jmpTmr > 0 and not plr.onCeil then
  plr.vy = -cfg.jmpVel
  plr.jmpTmr = plr.jmpTmr + 1
 end
 
 -- reset jump timer if button released or hit ceiling
 if not jmpDwn or plr.onCeil or plr.jmpTmr > cfg.maxJmpTmr then
  plr.jmpTmr = 0
  --if plr.jmpSnd then sfx(-1) end
  --TODO: the longer the player holds jump, the higher the note goes.
  -- Problem is the sfx(-1) interferes with other sounds, I need
  -- a more complex sound manager to remember which sound got 
  -- interrupted by what. Otherwise sfx(-1) will cut off the wrong
  -- thing or the jump sound go up infinitely. So for now we will
  -- simply have a fixed length jump sound.
 end

 -- apply gravity if not on ground, jump timer finished
 --  and currently not in climb mode
 if not plr.onGrd and plr.jmpTmr == 0 and plr.mov~=MOV.L then
  plr.vy = cfg.G
 end

 -- vertical delta for movePlayer
 return plr.vy
end

-- check player tiles
function chkPlrTil(checker)
 local px,py,w,h,cx,cy=plr.x,plr.y,plr.w,plr.h,plr.cx,plr.cy
 local x1, x2, y1, y2 = 
  (px+cx)//8, (px+cx+w-1)//8, (py+cy)//8, (py+cy+h-1)//8
 for i=x1,x2 do
  for j=y1,y2 do
   if checker(mget(i,j),i,j)==true then return true end
  end
 end
 return false
end

-- update player
function updPlr()
 -- handle ladders
 plr.onLdr=chkPlrLdr()
 -- set back to running mode when not on ladder anymore
 if plr.mov==MOV.L and not plr.onLdr then plr.mov=MOV.R end
 -- enter ladder mode when pressing up and ladder in reach
 if (btn(0) or btn(1)) and plr.onLdr then plr.mov=MOV.L end
 
 -- handle crouching; WIP
 local dh=plr.hr-plr.hc -- delta height
 plr.crch=false
 plr.h=plr.hr
 plr.cy=plr.cyr
 if btn(1) and plr.mov==MOV.R and plr.onGrd then
  plr.crch=true
  plr.h=plr.hc
  plr.cy=plr.cyc
  --plr.y=plr.y+dh-1
 end

 -- walk left and right
 local dvx,dvy=0,0 -- delta velocity
 local s=plr.crch and 0.5 or 1 -- speed
 if btn(2) then 
  dvx=dvx-s
  plr.flp=true
 end
 if btn(3) then
  dvx=dvx+s
  plr.flp=false
 end
 dvy=jmpPlr()
 plr.onGrd=chkPlrGrd()
 plr.onLdrTop=chkPlrLdrTop()
 plr.onCeil=chkPlrCeil()
 -- if standing on top of ladder and no y velocity, allow to
 --  squeeze down by pressing the down button
 if btn(1) and dvy==0 and plr.onLdrTop then dvy=2 end
 plr.vx=dvx
 plr.vy=dvy
 movePlayer()
end

-- tests if player collides with entities
function updCol()
 local px,py,pw,ph,pcx,pcy=plr.x,plr.y,plr.w,plr.h,plr.cx,plr.cy
 cols={} -- refresh each frame
 for k, ent in pairs(ents) do
  local def=ent.def
  local col=def.col -- collider in ent def
  local ex,ey=ent.x,ent.y
  local cx,cy,cw,ch = col[1],col[2],col[3],col[4]
  if aabb(px+pcx,py+pcy,pw,ph,ex+cx,ey+cy,cw,ch) then
   cols[#cols+1]=k -- store entity index
  end
 end
end

-- returns true if two boxes overlap
function aabb(px, py, pw, ph, ex, ey, ew, eh)
 return px < ex + ew and px + pw > ex and
  py < ey + eh and py + ph > ey
end

-- isSolid; return if tile at position is solid
function isSldMap(i,j,dir)
 return isSld(i*8,j*8,dir)
end

function isSld(x, y, dir)
 local tile = mget(x//8,y//8)
 if dir then
  local flgs=dirColTls[tile]
  if flgs then 
   -- collission margin is one pixel
   if (flgs & dir & DIR.S)~=0 and (y%8==0 or y%8==1) then return true end
   if (flgs & dir & DIR.N)~=0 and (y%8==0 or y%8==7) then return true end
   if (flgs & dir & DIR.W)~=0 and (x%8==0 or x%8==1) then return true end
   if (flgs & dir & DIR.E)~=0 and (x%8==0 or x%8==7) then return true end
  end
 end
 return tile >= cfg.solidMin and tile <= cfg.solidMax
end

-- solidSweep; return first solid tile pos
--  or if direction is supplied, get pos after solid tile
function sldSwp(x,y,w,h,dir)
 -- We need to round input since we step through the input
 --  in discrete steps (1px). See note below on discrete raycast.
 local x1,y1,x2,y2=rnd(x),rnd(y),rnd(x+w-1),rnd(y+h-1)
 local i1,i2,j1,j2=x1//8,x2//8,y1//8,y2//8
 local dir=dir or nil

 --[[
  Essentially the cleanest solution would be to make multiple
  parallel raycasts with a distance of 8 pixel inbetween.
  Right now we have a discrete raycast since we step through it
  for every pixel. This is a tradeoff since we need to get our
  position in relation to the fixed grid. Maybe I could change
  this to intersection math in the future.
 ]]--

 -- i,j = grid, precision of 8 for map
 -- x,y = absolute position, precision of 1 (or even smaller)
 
 if dir==DIR.E then -- East
  for i=x1,x2 do for j=j1,j2 do --cols first
    --trace(frmt("x1:%d y1:%d x2:%d y2:%d",x1,y1,x2,y2))
    if isSld(i,j*8,DIR.E) then return i end
  end end
 elseif dir==DIR.W then -- West
  for i=x2,x1,-1 do for j=j1,j2 do --cols first
    if isSld(i,j*8,DIR.W) then return i+1 end
  end end
 elseif dir==DIR.S then -- South
  for j=y1,y2 do for i=i1,i2 do --rows first
    if isSld(i*8,j,DIR.S) then return j end
  end end
 elseif dir==DIR.N then -- North
  for j=y2,y1,-1 do for i=i1,i2 do --rows first
    if isSld(i*8,j,DIR.N) then return j+1 end
  end end
 else --default: is any solid tile in the sweep
  for i=i1,i2 do for j=j1,j2 do
    if isSld(i*8,j*8) then return true end
  end end
 end

 return nil -- default: returns nothing
end

function printTileAt(x,y,px,py)
 local tx = flr(x/8)
 local ty = flr(y/8)
 local tile = mget(tx,ty)
 print(frmt("tile:%d,x:%d,y:%d",
  tile,tx,ty),px or 0,py or 0)
end

-- sweep player rectangle from old position to new, clamped to tiles
function movePlayer()
 local px, py = plr.x, plr.y
 local vx, vy = plr.vx, plr.vy
 local w, h = plr.w, plr.h
 local cx, cy = plr.cx, plr.cy

 if vx>0 then --EAST
  -- we move East, so we need to check tiles with **WEST** edge
  local txe=sldSwp(px+cx+w,py+cy,vx,h,DIR.E) 
  if txe then vx=txe-(px+cx+w) end -- txe: tile x East
 elseif vx<0 then --WEST
  local txw=sldSwp(px+cx+vx,py+cy,abs(vx),h,DIR.W) 
  if txw then vx=txw-(px+cx) end -- txw: tile x West
 end

 if vy>0 then --SOUTH
  local tys=sldSwp(px+cx,py+cy+h,w,vy,DIR.S) 
  if tys then vy=tys-(py+cy+h) end -- tys: tile y South
 elseif vy<0 then --NORTH
  local tyn=sldSwp(px+cx,py+cy+vy,w,abs(vy),DIR.N) 
  if tyn then vy=tyn-(py+cy) end -- tyn: tile y North
 end

 --[[
 -- first, horizontal movement
 if vx ~= 0 then
  local sign = vx > 0 and 1 or -1
  -- try moving pixel by pixel
  for i = 1, abs(vx) do
   if sign > 0 then
    -- moving right: check right edge
    -- TODO: Rewrite so its not 2 discrete tests (top edge and bottom edge), 
    --  but getting all tiles in the range and testing them
    if isSld(px+cx+w, py+cy) or isSld(px+cx+w, py+cy+h-1) then
     vx = i - 1
     break
    end
   else
    -- moving left: check left edge
    if isSld(px+cx-1, py+cy) or isSld(px+cx-1, py+cy+h-1) then
     vx = -(i - 1)
     break
    end
   end
   px = px + sign
  end
 end
 ]]--

 local ndir=DIR.N -- north direction
 -- if on ladder top and trying to pass down, ignore solid north flag
 if (btn(1) and plr.onLdrTop and vy>0) then ndir=nil end
 -- TODO: FIX

 --[[
 -- then, vertical movement
 if vy ~= 0 then
  local sign = vy > 0 and 1 or -1
  for i = 1, abs(vy) do
   if sign > 0 then
    -- moving down: check bottom edge
    if isSld(px+cx,py+cy+h,ndir) or isSld(px+cx+w-1,py+cy+h,ndir) then
     vy = i - 1
     break
    end
   else
    -- moving up: check top edge
    if isSld(px+cx, py+cy-1) or isSld(px+cx+w-1, py+cy-1) then
     vy = -(i - 1)
     break
    end
   end
   py = py + sign
  end
 end
 ]]--

 -- remember last position
 plr.lx=plr.x
 plr.ly=plr.y

 -- apply movement
 plr.x = plr.x + vx
 plr.y = plr.y + vy
end

flr=math.floor
cil=math.ceil
min=math.min
max=math.max
abs=math.abs
sin=math.sin
cos=math.cos
sqrt=math.sqrt
frmt=string.format
function clmp(v, low, hi) return math.max(low, math.min(hi, v)) end
function tern(cond, t, f) -- ternary operation
 if cond then return t else return f end
end
function rnd(n) return math.floor(n + 0.5) end
function tableToString(t)
 local parts = {}
 for k, v in pairs(t) do
  table.insert(parts, tostring(k) .. "=" .. tostring(v))
 end
 local str= "{" .. table.concat(parts, ", ") .. "}"
 return str:gsub("(".. string.rep(".", 60) ..")", "%1\n")
end

-- <TILES>
-- 000:00005100000050df000014ff00001dff00000fff00004fff00004f7500004555
-- 001:00000000fffffffffffffffffff5dff5ff555555755555555555555555555555
-- 002:00450000f7050000ff1400005f74000055f0000055d100005551000055510000
-- 004:655555599fbffbf2db6fa6f3da6aa6a29a6aa9a29a6559a29adfb6a29adba6a2
-- 005:6555555997affbf2d75fa6f3da5aa6a1590a5100514000829adba0a29a9a6251
-- 006:655555599fbfbff2dfeffef3dffffff3dfaeaaf3dbfbffe3de5b7db3de5b7db3
-- 007:655555599ffffff2daaaaaa3928ff282900eb002900800029208008299080862
-- 008:55555555559aa55559effa555effff655effff659fffffb59fffffb59fffffb5
-- 009:5555555555100555518fb05558fffb455cfbff451efeef251ffeef351aeeea25
-- 010:5555555555100555558fa45551eff35551feb25558feff455cfeff4558eeaa45
-- 011:5555555555504555551b255555cff45555cba45551fff35551fff35551ffa255
-- 012:5555555555504555551a2555551f3555551a2555551f3555551f3555551a2555
-- 013:5555555555504555551a355555cff455558af45551fff35551fff35551aef355
-- 014:5555555555100555558eb45551fff25551afe3555cffeb455cffef4558aeea45
-- 016:0000455500004555000045550000455500004555000045550000455500004555
-- 017:5555555155555551555555515555555155555551555555515555555155555551
-- 018:100000004ff70000cfff0000cff500004f550000475500004555000045550000
-- 020:559aa651df9aa6f3db9aa6f29a6559a29aaa9aa29aaa9aa25555555120000008
-- 021:559a1210db9200f29a9055a29a2459a2969a9692949686525455451120000008
-- 022:deabbeb3de5b7db3defbffb3dbaeaae3daffffa39ffffff29aaaaaa220000008
-- 023:9d062c729d073c729df7ff72965955929faaaaf25aaaaaa14555555120000008
-- 024:9fffffb59fffffb59fffffb55effff655effff6559effa55559aa55555555555
-- 025:1aeeea251aeeea251efeef2558ebfb4558fffb45518fb0555510055555555555
-- 026:58eeaa4558eeaa4558fefa4551eea25551efb255558fa4555510055555555555
-- 027:51ffa25551ffa25551ffa25555cba45555cba455551b25555550455555555555
-- 028:551a2555551a2555551a2555551a2555551a2555551a25555550455555555555
-- 029:51aef35551aef35551aef355558af455558af455551a35555550455555555555
-- 030:58aeea4558aeea4558efeb4551aee25551aff255558eb4555510055555555555
-- 032:0000455500004555000045550000055500001555000014550000505500005100
-- 033:5555555155555551555555515555555055555554555555145555550500000045
-- 034:100000004aa600008aaa00008aa500004a550000465500004555000045550000
-- 036:65555559dffffff3dffffff39ffffff19effff719affff519aeff7519aae7551
-- 038:200000088ffffff2cffffff3cffffff3cffffff3cffffff3cffffff3cffffff3
-- 039:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
-- 040:559aa5559a9fb9a5abafbab6efeffef6effffff6effaeff6afb59fb69fb59fb5
-- 041:55100555101f3105020f30248a8fb8a48fbaafb40fb08f341c351f0558251a45
-- 042:55100555101f3105030f3024cfcff8a4cfffffb40ff08f341c351f0558251a45
-- 043:55100555101f3105020f30348a8ffcf48ffffff40ff0cf341c351f055c351f45
-- 044:55100555101f3105020f30248a8fb8a48fbaafb40fb08f341c351f0558251a45
-- 048:000051000000509a000014aa000019aa00000aaa00004aaa00004a6500004555
-- 049:00000000aaaaaaaaaaaaaaaaaaa59aa5aa555555655555555555555555555555
-- 050:00450000a6050000aa1400005a64000055a00000559100005551000055510000
-- 052:9aa655519aa655519aa005519a2004519a000051920000419000000120000008
-- 054:cffffff3cffffff3cffffff3cffffff3cffffff3cffffff38ffffff220000008
-- 055:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
-- 056:9fb59fb6effaeff6effffff6efeffef6abafbab69a9fb9a5559aa55555555555
-- 057:1c351f050eb08f248aafbaa48a8aa8a4020a2024101a21055510055555555555
-- 058:1c351f050eb08f248aafbaa48a8aa8a4020a2024101a21055510055555555555
-- 059:1c351f050ff0cf24cfffbaa4cb8aa8a4020a2024101a21055510055555555555
-- 060:1c351f050eb08f348aeffff48acffcf4020f3034101f31055510055555555555
-- 064:6000000019aaaaaa4effffff4fffffff4fffffff4feffeff4b9fbafb475996aa
-- 065:00000000aaaaaaaaffffffffffffffffffffffffeffbeffbaffaaffaaaaaaaaa
-- 066:00000009aaaaaa64ffffffb1fffffff1fffffff1ffbffbf1efaef6e1aa9665d1
-- 067:186655551866aa65186965a6186a555954965555146555551866555518665555
-- 068:4010040140105911401414524055040590600401852004014010040140100401
-- 069:5555992455559924555599146555561595559a245a55692455aa992455559924
-- 073:55555d7555514fb6555c3fa6001eb400ca8eb2a3cbebebe3cbffffe31efc3fb4
-- 074:55555f555555df755551cba5555c3a55100ff0041beaabe41bffffe41efc3fb4
-- 080:47599aaa465996aa465995a60659559505555555005550551405140014504555
-- 081:9aa6aaaa9aa6aaaa9aa59aaa5a655aa555555555455055510000451000000000
-- 082:aaa665d1aa9665919a5665919655659055555550550555000014501455510514
-- 083:18665a5518669aa51869659a1866555914955555546655551866555518665555
-- 084:4010090140104951401014465015040591610401452004014010040140100401
-- 085:555599245555551455559a15659a69249aa59924555599245555992455559924
-- 089:1efc3fb45cfc3f351efc3fb41bbebee48beffbe2cfaffaf3cb0000e300555500
-- 090:1efc3fb458fc3f251efc3fb41bbebee41beffbe41faffaf41000000455555555
-- 099:186655551866aa65186965a6186a555954965555146555551866555518665555
-- 100:555555555555aa65555965a6559a5559a5a555559a6555555555555555555555
-- 101:5555992455559924555599146555561595559a245a55692455aa992455559924
-- 112:ffffaaaaffffaaaaffffaaaaffffaaaaffffaaaaffffaaaaffffaaaaffffaaaa
-- 113:55550000555500005555033055550c0055550330555500005555000055550000
-- 115:18665a5518669aa51869659a1866555914955555546655551866555518665555
-- 116:55555a5555559aa55555659a655a5559a6a655555a6555555555555555555555
-- 117:555599245555551455559a15659a69249aa59924555599245555992455559924
-- 128:fffbeffffffaafffffbaaeffffaaaafffbaaaaeffaaaaaafbaaaaaaeaaaaaaaa
-- 129:ffbaaefffbaaaaeffaaaaaafbaaaaaaebaaaaaaeaaaaaaaaaaaaaaaaaaaaaaaa
-- 130:aaeffbaaaeffffbaaffffffaeffffffbeffffffbffffffffffffffffffffffff
-- 131:fff7dffffff55fffff755dffff5555fff75555dff555555f7555555d55555555
-- 132:200000004fffffffdaaaaaaa9aaaaaaa9659aaaa96b9aaaa96a9aaaa9659aaaa
-- 133:00086555fff15faaaaa79faaaaa69a55659696af6b9696e76a9696f96596967a
-- 134:55555559aaaaaaf5aaaaaaf6555555a6faaffa96dbe7db966ff96f96ad7aad96
-- 135:659aa65955555555dffffff7d7ffffd7dffffff7555555556a5555a96f9aa6f9
-- 144:aaaaaaaabaaaaaaefaaaaaaffbaaaaefffaaaaffffbaaefffffaaffffffbefff
-- 145:aaaaaaaaaaaaaaaaaaaaaaaabaaaaaaebaaaaaaefaaaaaaffbaaaaefffbaaeff
-- 146:ffffffffffffffffffffffffeffffffbeffffffbaffffffaaeffffbaaaeffbaa
-- 147:555555557555555df555555ff75555dfff5555ffff755dfffff55ffffff7dfff
-- 148:9aaa00009aaa00009aaa00009aaa00009aaa00009aaa00009aaa00009aaa0000
-- 149:aaa696baaaa696faaaa696dbaaa6966faaa696afaaa696e7aaa696f9aaa6967a
-- 150:aebaae96affaaf96e7dbe796f96ff996faaffa96dbe7db966ff96f96ad7aad96
-- 151:6f9aa6f9655555596ffffff967ffffd96ffffff9655555596a5555a96f9aa6f9
-- 160:00082000000aa000008aa20000aaaa0008aaaa200aaaaaa08aaaaaa2aaaaaaaa
-- 161:008aa20008aaaa200aaaaaa08aaaaaa28aaaaaa2aaaaaaaaaaaaaaaaaaaaaaaa
-- 164:9aaaaaaa9659aaaa96b9aaaa96a9aaaa9659aaaa9aaaaaaa5aaaaaaa65555555
-- 165:aaa696ba659696fa6e9696db6a96966f65969a55aaa69faaaaa55faa55596555
-- 166:aebaae96affaaf96e7dbe796f96ff996555555e7aaaaaae7aaaaaaa555555559
-- 176:aaaaaaaa8aaaaaa20aaaaaa008aaaa2000aaaa00008aa200000aa00000082000
-- 177:aaaaaaaaaaaaaaaaaaaaaaaa8aaaaaa28aaaaaa20aaaaaa008aaaa20008aa200
-- 180:aebaaebaaffbeffbebaffaaffaaaaaaafffffffffbbaaaaaffffffffffffffff
-- 181:aebafbfbeffaebfbfaebabfbaaafaafbffffabfbaeefebfbfffffbfbfffffbfb
-- 182:efef0000efeb0000efea0000efaa0000efea0000efeb0000efef0000efef0000
-- 192:aaa65555aa65ffffaa5faaaaa6daaaaaa5baeffba9aeffff69afffff6aefffff
-- 193:9aaaaaa259aaaa20f5aaaa00a79aa200ae5aa000ba6aa000fa692000fba92000
-- 194:00008aaa000008aa000000aa0000008a0000000a0000000a0000000800000008
-- 195:aaa65555aa65ffffaa5f5555a6d55555a5759696a55967996556579565955795
-- 196:9aaaffff59aafffff5aaaaaa579affff5d5aaaaa655affff9559aaaa5759aaaa
-- 197:efef0000fefe0000efef0000fefe0000efef0000fefe0000efef0000fefe0000
-- 198:0f300f30cff0fcc3c3f0fff3c3f083b0c3f0fff3cff0fcc30f300f3000000000
-- 199:cff00000fff300003c030000bc230000bc230000fff30000cff0000000000000
-- 200:aaa65555aa65ffffaa5faaaaa6daaaaaa5baeffba9aeffff69afffff6aefffff
-- 201:9aaaaaa259aaaa20f5aaaa00a79aa204ae5aa005ba6aa009fa692049fba9204a
-- 202:04559aaa45ff59aa5faaf5aadaaaa79abaefae5aaaffba6aaefffa69affffba9
-- 203:aaa20000aa200004aa000004a2000004a0000005a00000092000000920000009
-- 204:55aa5100d79ad700be5ad700aa6a9a10af699a10afa99a10afa99a10afb99a10
-- 205:aaa65555aa65ffffaa5faaaaa6daaaaaa5baebeba9aefbef69affbef6aeffbef
-- 206:9aaa000059aa0000f5aa0000a79a0000ae5a0000ba6a0000fa690000fba90000
-- 208:6aefffff5affffff9affffff9affffff9affffff9affffff9aaaaaaa6aaaaaaa
-- 209:fba92000ffa50000ffa60000ffa60000ffa60000ffa60000aaa60000aaa82000
-- 210:0000000800000000000000000000000000000000000000000000000000000008
-- 211:659557955565579555655795556557955565579555eff79f5555555565555555
-- 212:5759ffff5d55aaaa5d55aaaa5d55aaaa5d55ffffff55aaaa5555aaaa5559aaaa
-- 213:eeee0000bbbb0000eeee0000bbbb0000eeee0000bbbb0000eeee0000bbbb0000
-- 214:0f300f30cff0c0c0cff0c0c0c0f0c0c0ccf0c0c0cff0c0c0cff0cae000000000
-- 215:0f3000008fb00000cee00000cbf00000cee000008fb00000cff0000000000000
-- 216:6aefffff5affffff9affffff9affffff9affffff9affffff9aaaaaaa6aaaaaaa
-- 217:fba9205affa5009affa6009affa6009affa6009affa6009aaaa6009aaaa8209a
-- 218:affffba5efffffa6efffffa6efffffa6efffffa6efffffa6aaaaaaa6aaaaaaa8
-- 219:200000490000004a0000004a0000004a0000004a0000004a0000004a2000004a
-- 220:afb55910efb65910efb65910efb65910efb65910efb65910aaa69a10aaa69a10
-- 221:6aeffbef5afffbef9afffbef9afffbef9afffbef9afffbef9aaaaaaa6aaaaaaa
-- 222:fba90000ffa50000ffa60000ffa60000ffa60000ffa60000aaa60000aaa90000
-- 224:6aefffff6affffff6affffff6a75dfff6ad75fffa8d75effa8596effa8596eff
-- 225:fba82000ffa82000ffa82000ffa82000fba82000fb2aa000fb2aa000fb2aa000
-- 226:00000008000000080000000800000008000000080000000a0000000a0000000a
-- 227:659aa69a65655795656557956565579565955795a5955795a59ff79fa5555555
-- 228:a65900005d5900005d5900005d59000057590000575a0000f75a0000555a0000
-- 229:baba0000abab0000baba0000efef0000ffff0000abab0000baba0000abab0000
-- 232:6aefffff6affffff6affffff6a75dfff6ad75fffa8d75effa8596effa8596eff
-- 233:fba8204affa8204affa8204affa8204afba8204afb2aa04afb2aa04afb2aa009
-- 234:efffffa8efffffa8efffffa865dfffa8d75ffba8d75ffb2a596ffb2a596ffb2a
-- 235:2000004a2000004a200000492000000920000009a0000009a0000009a0000009
-- 236:efb69a10efb69a10efb69a10e55696516f519d756f519d7565a1959665a19596
-- 237:6aeffbef6afffbef6afffbef6afffbef6aeffbefa9effbefa9effbefa9aaaaaa
-- 238:fba90000ffa90000ffa90000ffa90000fba90000fb6a0000fb6a0000aa6a0000
-- 240:a8659effe8aaafffe3affffff3affffff3aaaaaaff8aaaaaff8aaaaaff8aaaaa
-- 241:fa2aa000fa4be000facbe300facff300aacff300a2ffff00a2ffff00a2ffff00
-- 242:0000000a000000cb000000cb000000cf000000cf000000ff000000ff000000ff
-- 243:a5555555a55aa79aa6565795a65ff7dfa6555555aa555555aa555555aa555555
-- 244:555a0000e59a0000d59a0000f59a0000559a000055aa000055aa000055aa0000
-- 246:0f300c000f300f30fcc3cff0fff3fff3fcc3fff30c00ccc00f300f3000000000
-- 247:00000c00c3f00f30fff3cff0fff3fff3cff0cff00f300f300c000c0000000000
-- 248:a8659effe8aaafffe3affffff3affffff3aaaaaaff8aaaaaff8aaaaaff8aaaaa
-- 249:fa2aa009fa4be009facbe309facff309aacff309a2ffff04a2ffff04a2ffff04
-- 250:659ffa2aaaeffa4baffffacbaffffacfaaaaaacfaaaaa2ffaaaaa2ffaaaaa2ff
-- 251:a0000009e0000009e3000009f3000009f3000009ff000005ff000004ff000004
-- 252:a5589651aaa89a10afa85510afac9a10aaac5510aa2c5500aa2f9600aa2f5100
-- 253:a9aaaaaaa9affbefa6affbefa6affbefa6aaaaaaaa9aaaaaaa9aaaaaaa9aaaaa
-- 254:aa6a0000fa9a0000fa9a0000fa9a0000aa9a0000a6aa0000a6aa0000a6aa0000
-- </TILES>

-- <TILES1>
-- 000:000000000000000000000000000000000000cfff000fffff00ff75550cf55555
-- 001:00000000000000000000000000000000f3000004fff0000510cf0045100c0055
-- 002:aaa6559aaaa6559aaaa6559aaaa655aaaaa655aaaaa655aaaaaa55aaaaaa5555
-- 003:a2000000a2000000a2000000a2000000a2000000aa000000aa00000055000000
-- 004:0008aa8a0008aa8a0008aa8a0008aa8a0008a28a0008a2aa000002a00008a00a
-- 016:0f755555cf555555c759aa55f55a5a617596596175965920559659aa559aaaaa
-- 017:1000f5550000c75500000f558aa00c55aaa200755aa60075a5aef375a5aef3c5
-- 018:aaaa5555aaaa55a6aaa655aaaaa655aaaaa655aaaaa655aaaaa655aaaaa655aa
-- 019:550000009a000000aa000000aa000000aa000000a2000000a2000000a2000000
-- 020:000028aa00002aaa00008aaa00008aaa00000aaa00000aaa000008aa000008aa
-- 032:55aaa59655aa6fbe59aad3be59aad0b20aaad0b20aaadfbe0aa5aaaa0aaaaa20
-- 033:5aaef3c5796cf3c5c96000c5c9200007c9a00007f9a20f07aa520f07a9a6cfc5
-- 034:aaa6550aaaa6550aaaa6550a5aa5550555555505555555555555555555555555
-- 035:a200000082000000820000004100000051000000510000005100000051000000
-- 036:000000aa000000aa0000008a0000004500000045000000450004555500045555
-- 048:8aaa6aa88aaa2aa88aaaa000baaaaa08aaaaaaaa5dbaaaaa55fbaaaa555dfaaa
-- 049:a8a6f3c5a8aef3f50aaf3075aaef3c75aaef0c55aaa10755aaa2d75daaaaf65f
-- 050:df555555df7555555f7555555ff5f755df7ff755ff7ff755ff5fdf55f755ff55
-- 051:5100000050000000500000001000100400001004000400000004000000000000
-- 052:0004550400005100000051000004510000005555100045555000455541004555
-- 064:55555fba755555dfff755555ffff5555fffff555ffffffffffffffffffffffff
-- 065:aef0aadff5558aef55558aaf55550aae55ff3aaefffff8aafffff0aafffff08a
-- 066:ff7fff55ff7ffd55ff755f55fff5f755fffff555ff755555ff755555ff555555
-- 067:cf3000005550000055500000555cffff555cffff555555555555555555555555
-- 068:4dff05550455555504555555f4555555f5555555555555555555555555555555
-- 080:0000000000000000000000000000000000000551045aaaa1459aaaa155aaaaa1
-- 081:00000000000000000000000000000000000000000c0000000000000000000000
-- 082:000000000000000000cff30000c5530000755d00007555f00c7555c00c5ff5c0
-- 083:00000000000000000000000000000000000f0000000f00000000000000000000
-- 084:00000000004555550055555508aaa65509aaaaa509aaaaa509aaaaa60559aaa6
-- 096:59aaaaae59aaa8ae59aa6aef59aa5adf59a69aff59a6aaff5555aaff55559aff
-- 097:fff30000fff30000fff3c00cfffb200cf75a2000f59afff075aefff35aaffff3
-- 098:0c5ff5c00c5ff5f03c5ff5303c5555300c5555300c5555300c9aaa750c9aaa75
-- 099:000000000c30000a0c3cf30a000cfffa000cfffafffcfffbfffffffbffffffff
-- 100:08659aa6a8a65aa6aaaa5aa6aaaa5555aaaa5555aaaa5555aaaa5555aaaa0055
-- 112:55555aff55555aff55555aff55555aff45555a0000000000000000c00055dff0
-- 113:5aaffff39aeffff3baef7ff1aaefbeffa0cfaeff00cfaeff000faeffcf00aeff
-- 114:4d9aaa7f459aaaff459faafff59ffbfff59ffbdff5affb5df5affb5dffaffb55
-- 115:fffffffffffffffffffffffffffffffffffff79afffff7aaff7dfbffaffb6aff
-- 116:baa00000f3004551f3aa55515dfbaaa5aefaaaa5afbaaaa5efaaaaa5efaaaa55
-- 128:55555ff0555aaeff555aaaff555aaafa555aaaaa5555aaaa5555aaaa55559aaa
-- 129:ba2082c3baa20000baaaff00aaaaff30aaaafff0aaaaffffeffaaeffeffbaaff
-- 130:0cf5555d00c5aaaf00cfaeffcfffaeffcfffaeffffffaeffffff38aeffff300a
-- 131:fffb6efffffbaffffffbefffffbaeffbff55dffbff55dffaff55dfbaaa55dfba
-- 132:efaaaa55ffaaa655ffaaa555ffbaaa65ffbaaa65fbaaaa65f9aaaa6569aaa555
-- 144:55555aaa55555aaa55555aaa55555aaa55555aaa55555aaa5555555a5555555a
-- 145:efffaaffefffaaffafffaaffaeffbaefaefffaafaafffaaeaafffaaaaaeffaaa
-- 146:ffff3000ffff3000fffff000fffff0c0fbfff000fbfff000aaff0000aa200000
-- 147:aa55ffbaafbaaaaa0cbaaaaa0008aaa60008aaa50008aaa50008aaa500008aa5
-- 148:69aa65556aa655555aa655555aa655555aa655555aa555555555555555555555
-- 160:0000000000000000000000000000000000040000000000000000000000000000
-- 161:0000000000000000000000000000010000000000000000000800000000000000
-- 162:0000000000000000000000000000000800040000000000000000000000000000
-- 176:000000000000000000000000559aa000559aaaa05555aaaa5555559a0004555a
-- 177:000000000001000800000000000000000000000000000005a2000005aa200005
-- 178:000000000000000000000000000000450000045f555145df555555df555555df
-- 179:000000000ff00000455f000055553000f5553000f7553000f75d0000f75d0000
-- 180:8000000000000010000000000000000000000000000000000000000000100000
-- 192:0000455500000455000000450000000400000000000000000000000000000000
-- 193:9aa000055aa2000055aa0000559a2000455a20000559a0000455a20000559a00
-- 194:5555555f05555555045555550455555f045555f304555f300555d30005551000
-- 195:f553000055d3000055f00000ff30000000000000000000000000000000000000
-- 196:0000000000000000800000000000000000000000000000000000000000000000
-- 209:00055a0000045920000055a0000045a000000590000005900000049200000452
-- 210:0df3000000000000000000000000000000000000000000000000000000000000
-- 211:0000000000000000000000400000000000000000000000000000000010000000
-- 212:0000100000000000000000000000000000000000000000000000000000000000
-- 225:0000005a00000059000000590000005900000055000000450000004500000045
-- 226:0000040000000000000000000000000000000000200000001000000010000000
-- 228:0010000000000000000000400000000000000000020000000000001000000000
-- </TILES1>

-- <SPRITES>
-- 000:000000000000cff30000cff30000c3000000c3000000c3000000c30000000000
-- 001:00000000cff3cff3cff3cff3000000c3000000c3000000c3000000c300000000
-- 002:0f300c000f300f30fcc3cff0fff3fff3fcc3fff30c00ccc00f300f3000000000
-- 003:04000400c3fff3ffc3fff3ffc30fffcfcf0000cfcfffbacfcfffaa0f1ffbaa2c
-- 004:00100010ffcfffc3ffcffcc3ffcf30f3f3000cf4f38afff4f0aaff3438aaef05
-- 005:aa6559aaa610049aa145004a60450009600000091030003410c00c0413030307
-- 006:555555555100500050ba0eff5cfa2fff5cfbaffa5cff20825cff0f3c50f3c3fc
-- 007:55550000100500000aa00000fbe30000eaf300000ef30000fcf30000c3f00000
-- 008:0000000000000000000000cf00000cff00000cff00000ff000000f3000000f30
-- 009:0000000000000000fffff300ffffff30ffffff3000000ff000000cf000000cf0
-- 010:00000000000000000000ffff0000ffff0000ffff00000c300000000000000000
-- 013:000000cf000000cf000000ff00000cff00000fff0000cff30000fff0000cff30
-- 014:00000000f0000000f3000000ff300000fff00000cff000000ff300000cff0000
-- 016:000000000000c3000000c3000000c3000000c3000000c3000000c30000000000
-- 017:00000000000000c3000000c3000000c3000000c3000000c3000000c300000000
-- 018:00000c00c3f00f30fff3cff0fff3fff3cff0cff00f300f300c000c0000000000
-- 019:50fbaaa0008aaaa2cfaaaaa2cfaaaaa20f2000081fa0f0381ca2ff381faa0008
-- 020:8aaaaa452aaaaa002aaaa2f32aaaaef420000c342c0f0e352cf38e052000ae04
-- 021:1d0000c5173000771cfcfcf46033333960cfcf09a104504aa610049aaa6559aa
-- 022:5100c3fc5510cffc55130f3c550f000355cf3ef355cf88ff51f0a2005138aaaa
-- 023:c3040000f3550000f055000004550000f4550000355500002045000000350000
-- 024:00000f3000000f3000000f3000000f3000000f3000000f3000000f3000000f30
-- 025:00000cf000000cf000000cf000000cf000000cf000000cf000000cf000000cf0
-- 026:0f3000000f3000000f3000000ff000000ff000000f3000000f3000000f300000
-- 027:0cf000000cf000000cf000000ff000000ff000000cf000000cf000000cf00000
-- 029:000cff00000ff300000ff00000ff300000ff30000cff00000ff300000ff30000
-- 030:00ff300000cff000000ff300000fff000000ff300000cff000000ff000000ff3
-- 032:000000000000c3000000c3000000c3000000c3000000cff30000cff300000000
-- 033:00000000000000c3000000c3000000c3000000c3cff3cff3cff3cff300000000
-- 035:cfaaaaaacfaaaaaacf000aaa1caa2aaa50aa28aa0cbaa800cffaa8cfcfffa800
-- 036:aaaaaef3aaaaaef3aaaaacf0aaaa2f04aaa082450028ae05f32aaff4302aaff3
-- 037:aaeffbaaae755dbaa7df55dae5df555be555555b7575557d75d55d5d7757575f
-- 038:518aaaaa11ba08a011b2f32011aac002158a20c815ca220045cb0a0045c30008
-- 039:8005000002050000c005000000450000045500000455000002550000aa450000
-- 040:00000f3000000f3000000ff000000cff00000cff000000cf0000000000000000
-- 041:00000cf000000cf000000ff0ffffff30ffffff30fffff3000000000000000000
-- 042:000000000000000000000c300000ffff0000ffff0000ffff0000000000000000
-- 044:00000000000000000000000c0000000f0000000f000000cf000000cf000000cf
-- 045:cff00000ff300000ff000000ff000000f3000000f00000003000000000000000
-- 046:00000cff000000ff000000ff000000cf0000000f0000000f0000000cff300000
-- 047:000000000000000030000000f0000000f0000000f3000000f3000000ff000000
-- 051:c0ffa8a200ffb8a21cffbaaa1fffb8aa0f300000cf0cff0f0051c35c05550450
-- 052:3a2aef3f0a2eff30aa2ffff4aaab00c000000ff3f0c340f3340051c305145500
-- 053:7d5555dd7775557f7dfdfdfde577777be5dfdf5ba75555daae755dbaaaeffbaa
-- 054:45c3c00b45c38aff41f3aaae4cf0aaa20f33aaa80000aaa84541aaa845458aa8
-- 055:aa450000fa250000ff340000af330000aac30000aac30000a2c30000a2f30000
-- 060:00000cff00000cff00000ff300000ff00000cf300000cf300000ff000000ff00
-- 061:000000cf00000cff0000cff00000ff30000cf300000ff000000f3000000f0000
-- 062:ff3000003ff0000000f3000000cf0000000f3000000ff0000000f0000000f300
-- 063:ff300000cf300000cff000000ff000000ff300000cf300000cff000000ff0000
-- 067:50fbaaa0008aaaa2cfa20002cf20ff300f20f0381fa0f0381ca2ff381faa0008
-- 068:8aaaaa452aaaaa00200082f32cff0cf42c0f0c342c0f0e352cff8e052000ae04
-- 069:aa6559aaa610049aa145004a6045000960c30f0910f30f3410f00c3414100054
-- 070:41558a2a14558a2a55558a2a55558a8a55558a8a55558a8a5551ff3f5551feec
-- 071:a0f40000240400002455000005550000455500004555000034550000e2550000
-- 076:000cf300000cf300000cf000000ff000000ff00000cf300000cf300000cf0000
-- 077:000f0000000f0000000f0000000f0000000f0000000f0000000f0000000f3000
-- 078:0000c3000000c3000000c3000000cf000000cf000000cf000000c3000000c300
-- 079:00cf300000cf3000000ff000000ff300000cf300000cff000000ff000000ff30
-- 083:50fbaaa0008aaaa2cfaaaaa2cfaa00820fa2000a1fa20a0a1cefbaaa1faaaaaa
-- 084:8aaaaa452aaaaa002aaaa2f32200aef4a0008e34a0828e35aaaefe05aaaaae04
-- 085:100cf004103333341ccfcfc46030003960000009a100004aa610049aaa6559aa
-- 092:00ff000000ff00000cf300000cf300000ff000000ff000000f3000000f300000
-- 093:000c3000000cf0000000f3000000f3000000cf0000000fff00000cff00000000
-- 094:0000f3000000f300000cf000000f300000ff3000ffff0000ff30000000000000
-- 095:0000cf3000000ff000000ff000000ff000000cf000000cf300000cf3000000f3
-- 108:0f300000cf300000cf300000cf000000cf000000ff000000ff000000ff000000
-- 111:000000ff000000ff000000ff000000cf000000cf000000cf000000cf000000cf
-- 124:f3000000f3000000f3000000f3000000f3000000f3000000f3000000f3000000
-- 127:000000cf000000cf000000cf000000cf000000cf000000cf000000cf000000cf
-- 129:55555555105550450f451a25ca300a25ca3fff25cfffff35cffcfc35cffcfcf4
-- 130:5555555555555555105550450f451a25ca300a25ca3fff25cfffff35cffcfc35
-- 132:0000105500001f450000cf450000c3550000c300000013ff00000fff0000cfff
-- 133:504551055c3418255cf00a255cfffb250cffff353c3cfc340fc333f0cbeffbe3
-- 134:505555051a4550a48f2008f28faaaaf28a0820a282a2aa828223ac821220a084
-- 135:505555051a0551a48f0008f283ff00c28cffcf328c3fc3328cffcf321cf3cf34
-- 136:510550455cf41f351be41be41be00be41ffffff30ffcfcf3cffcfcf3cffcfcf3
-- 140:f3000000f30000003000000000000000f3000000f3000000f3000000f3000000
-- 143:000000cf000000cf000000cf000000cf000000cf000000cf000000ff000000ff
-- 145:cffcfcf3cfaf0be3cffc33f3cff3ccf3cffffff4cffffff0cfffff33cfcffffc
-- 146:cffcfcf4cffcfcf3cfaf0be3cffc33f3cff3ccf4cffffff0cf3fff33cffcfffc
-- 147:5500005510ffff04cfffffb2cffcfcb2cffcfcf40cfcfcf30faf0be3cffc33f3
-- 148:0000cfff0000cfff0000cffc0000cff300000ff3000010f3000051c3000055cf
-- 149:cff30ff33f3fc3f4fcf03c35f0ffff450c30a0554c31a4550c30a2550cf4cf45
-- 150:1aaefba48a2a0a828a2a2a820aa080a418aaaa25508aaa4551aaaa4551aaaa25
-- 151:120820848afa8fa28afaafa21aa28aa058a28a2451a2820551aaaa4551aaaa45
-- 152:cfaf0be30ffc33f3cff3ccf3cfcffff310fffff41ffffff45cf30c3551041045
-- 156:f3000000f3000000f3000000f3000000f3000000f3000000ff000000fff00000
-- 157:000000000000000000000000000000000000cfff0cffffff0cffffff0cfff30f
-- 158:00000000000000000000000000000000fffffff0fffffffffffffffff300ffff
-- 159:000000f300000cf300000ff300000ff000000ff030000f303000000030000f30
-- 161:cfcffff30fcffff3c0fffff3cffffff3cffffff41ffffff45cf30c3551041045
-- 162:cffcfff3c3fcfff3cf0ffff3cffffff30ffffff4cffffff3cf300cf310055004
-- 163:cff3ccf3cf3fff34cffc30c0ccfcff33c30ffff4cffffff41ff00c3550051045
-- 164:000055550000555500005510000051cf00005cff00001fff00001fff0000cfff
-- 165:555555550404504082f32f328cffcff08c3fc3f03cffcff400f32f30ca0cf082
-- 166:51aaa82551a8a00551a0280450a20a845882aa845882aa845820aa0458aaaa45
-- 167:10aa8a458aaa2a048aa2a8a20aa208a21882aaa05820aa2458aaaa2551aaaa45
-- 172:cff000000ff000000f3000000f3000000ff000000ff000000cf000000cf00000
-- 173:00ff000f00ff000f00f3000f00f3000f00f3000f00f3000f00f3000f00ff000f
-- 174:f3000ff3f300cff3f300cff0f300cf30f300ff00f300ff00f30cf300f30cf300
-- 175:00000f3000000f3000000f3000000f3000000f3000000f3000000f3000000f30
-- 178:555555555555105555550f455555ca305555ca3f5555cfff55551ffc5555cffc
-- 179:55555555504555551a2555550a255555ff255555ff355555fc355555fcf45555
-- 180:0000cfff0000c3ff0000c3ff0000c3f30000c3f30000c3f4000010f400005105
-- 181:cff30ff33ffb8ff4fc302035f0ffff4500f000250cf31aa40cf45cf410055105
-- 182:18aaaa45c8aaaa458aaaaa458aaaaa4518aaae4558aaaf4558a0cf455825c345
-- 183:51aaaa4550aaaa4558aaaa2518aaaa348aaaaef38aa00ff38a0410f410455105
-- 188:0cf000000cf300000cff000000ff000000cf300000cff000000ff300000fff30
-- 189:00ff000f00ff300f00cf300f000ff00f000ff30c000cf30c0000f30c0000ff00
-- 190:ff0cf300ff0ff000ff0fff00ffcfff00ffcfff00fffff300fffff0000cff0000
-- 191:00000ff30000cff30000cff30000cff00000ff30000cff00000ff30000cff300
-- 192:5555555555100555550ff45551cff31051ffff8a5cffff0a1cffff001fffff30
-- 193:5555555555100455550aa045008aaa05aa08aa24aaa0aaa40000aaa400008aa4
-- 194:5555cffc55100faf550ffcfc51cff3f351ffffcf5cffff031cffff0c1fffff3c
-- 195:fcf355550be3045533f3a045ccf3aa05fff3aa240f08aaa4f0f3aaa4f0f38aa4
-- 204:0000fff00000ffff00000cff000000ff000000cf000000000000000000000000
-- 205:0000ff000000ff00f000ff00fffff300fffff300fffff0000000000000000000
-- 206:0cf300000cfff0000cfff0000cfff0000cffffff0cffffff000fffff00000cff
-- 207:00fff3000ffff000cfff3000ffff3000ffff0000fff30000ff30000030000000
-- 208:0fffff30cfffff0fcfffffcfcffff3cf000f30ffcff0cffbfff3afaecff3baef
-- 209:000c8aa4ffff3aa4fffff8a4ffbae004baafa2a4affef382fffeff82fffeff82
-- 210:0fffff30cfffff0fcfffffcfcffff3cf000f30ffcff0cffbcff3afaecff3baef
-- 211:000c8aa4ffff3aa4fffff8a4ffbae004baafa2a4affef382fffeff82fffeff82
-- 224:cff3ffefcff3aaefcff3fbff1300fbff10ff8aff0fffceffcfff8fffcfffcfff
-- 225:fffeff82fffeff82ffbeff04ffbfff82ffbfff82ffbff382ffaff3a2ffeff0a2
-- 226:cff3ffefcff3aaefcff3fbff1300fbff10ff8aff0fffceffcfff8fffcfffcfff
-- 227:fffeff82fffeff82ffbeff04ffbfff82ffbfff82ffbff382ffaff3a2ffeff0a2
-- 240:cfff0cffcff3c30c0f00fff05041fff35551fff05551aaa45551ff3555550045
-- 241:faff0cf0ff308004008aa45558aae45558ef345550aa255551ff055551004555
-- 242:cfff0cffcff3c30c0f00fff05041fff35551fff05551aaa45551ff3555550045
-- 243:faff0cf0ff308004008aa45558aae45558ef345550aa255551ff055551004555
-- </SPRITES>

-- <SPRITES1>
-- 000:0000000000000000000000000000000000000000000000000080000000004000
-- 001:000000000080000000000000000000000000000400000ff700cff55500755555
-- 002:007555000c5555000d5555000f55550055555500555555005555550055555000
-- 003:0000000000000000000000000000000000455000047555550d55550445510504
-- 004:0000000000000000000000000000000000000000000000005514000055504500
-- 016:01000000000000000000000c0000000d0000000f0000004f000000ff000000ff
-- 017:cf555555f7555555f5555555f5ff755575fff55555bee5555dbae5555dbbb555
-- 018:5555104d55510c755550cf505510f500551c7500550f55005507510051070000
-- 019:7550050400100055000000050000000400000004000000040000000400000000
-- 020:5550451055514550555145505551055055510000555104005551001005551010
-- 032:000000ff000000ff000000ff000000df0000004f0000000d0000000000000000
-- 033:5dbbb55555ba6555555555557555555575555555f75555500ff7100000000000
-- 034:55d7000055d50000551404005504f51050047510000c500000041000000d1000
-- 035:0000000500000005000000450005104500551045005555550455551005555555
-- 036:5555104055555550545555505055550450455500004545100455055005550451
-- 048:0000000000100000000000000000000000000000000000000002000000000000
-- 049:0000000000000000000000000000000000000100000000000000000000001000
-- 050:000f10000007100000071000000c1000000c1000000c55000000755500003000
-- 051:0555555505555555055555550555504505555104450555005104510000455100
-- 052:5551004555500004551000005150045051410450550504514511104505545004
-- 065:0000000200000000100000000000000000000000000000000000000000000000
-- 066:0000000500000105000000540400005000000000000000000000005000000000
-- 067:1141410050004510500045140050451000410000000400000000500000004000
-- 068:0450110000014100000504000004041000505010000540004004100000000000
-- 080:00000fff000fffff00ffffffcffffffffdfffffff5fffffff7ffffffffffffff
-- 081:fffffffffffffffffffffffffffffffffffffffffaffff55faeff755fbeff555
-- 082:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 083:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 084:ff000000fff00000fffff000ffffff30fffffff0ffffffffffffffffffffff5f
-- 096:ffff55dfffff555dffff555affff555afdff555affff759affff75aaffff7596
-- 097:fbeff555fffff555effff555e7ff7555efff7555ef5f7a55ef7d7a55aff57a65
-- 098:f55fffffff7dffffdfffffffdfffffffdfff7dffdfd77555dff55555d7555551
-- 099:fffffffffffffffffffff755ffff5555755555df5d55dfff5fffffff555dff7f
-- 100:ffff55ff7555dfff5fffffffffffffffffffffffffffffffffff7555ff755555
-- 112:ffffffffffffffffff75dffffff7affffff7afffffffff7ffffffffffff5ffff
-- 113:fffb59a5fff9a5aaff7596aafd7a6aa6ff559aa6f7755aa6f759aaa5ffaaaa65
-- 114:e7555555f5550555750005555100045551000000510000001000000000000000
-- 115:555dffff555d7df71045df5510514555004155550045555d04555dff04555fff
-- 116:f7555dff55555dff555dffff5fffffffdfffffffffff75fffff5dffff75dffff
-- 128:faa575ffba8655f7b2a65df7baa65df5b2a655f5fa855575fff7dff5fffffff5
-- 129:779aaa655a5aaa655a55aa655a104a65551049e555104ae555104ae555104ae5
-- 130:000000000000000000000005100000551000045550000555510045555510455d
-- 131:0555fff70555f75d555d7dff5555dfff555fffff555fffff5fffffffffffffff
-- 132:55fffffffffff0ffffff30ffffff0cfffff30ffffff300cffff000cfff300cff
-- 144:fffffff5fffffff7fffffffffffffff50ffffff50cffffff000fffff0000ffff
-- 145:551045d555104df775555ffff555df30d755ff00fffff300fffff300ffffffff
-- 146:5510455f5510455f555555fff7555dff0f3cff300f3cff30ffffffffffffffff
-- 147:ffffff30fff30c00f3000c0000000000000000cf00000fffffffffffffffffff
-- 148:ff300fffff00cff0f300ff30f30cf300f0c0f300f0f0f300ffff3000ffff3000
-- 160:0554100005540000555500000054000000540055005005510055555500405550
-- 161:510000005100000055555555500000005005550050055500500555045965a5aa
-- 162:0000000000000000555555550000000000000000000000005555555520000000
-- 163:0000000000000000555555550000000000000000000000005555555500000000
-- 164:0000000000000000555555550000000000000000000000005555555500000000
-- 176:0000550000055555000004a6100104a600000455555104555555555555555555
-- 177:4969a5aa5559a5aa555555aa5555555551a0000055a555555a6555555a69a555
-- 178:20000000aa655555aaaaa2009aaaaa658aaaaa2055aaaff755aaaff765aaaff7
-- 179:0000000055555555000000005555555500000000555555555555555555555555
-- 180:0000000055555555000000005555555500000000555555555555555555555555
-- 192:5555555555555555555555555555555555555555555555555555555555555555
-- 193:5a6aa555559aa555559aa555559aa9a5555559a655559aa655559aa655559aa6
-- 194:55555ff75955555555655555555a5555555af555555555555555555555555555
-- 195:5555555555555555555555555555555555555555555555001045510000455000
-- 196:5555555555500455555004555550045555100055551000554500004545000005
-- 208:5555555555555555555555505105555050155510051055400000400055510455
-- 209:55555aa655555af555555df555555df545555555455555550555555501141500
-- 210:5555555555555551555555515555555155555001555510005555000045550000
-- 211:0045500000455000004550000045500000051000000410000004100000040000
-- 212:0100000501000004010000000000000000000000000000000000000000000000
-- 224:5555110055555451510551455555551455555554555555555555555555555555
-- 225:0000005504555555555510055555000014550000504500005555000055550010
-- 226:0000000055510000555000004500000000000000000000000000000000000000
-- 227:0004000000000000000000000000000000000000000000000000000000000000
-- </SPRITES1>

-- <MAP>
-- 000:373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 001:373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 002:373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 003:373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 004:3737cc37cc3737373737373737bb3737cb37cc37cc37dc373737373737373737b8c8c8c8c8c8c8d83737373737373737cccccccccccccc3737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 005:37373737373737373737373737bb3737cb373737373737373737373737373737b9c9c9c9c9c9c9d93737373737373737cccccccccccccc3737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 006:8090889898a8373737373737c0d0c0d0e0f0a0b0a0b0a0b03737e8f837373737b9c9c9c9c9c9c9d93737373737373737cccccccccccccc3737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 007:8191891717a9373737373737c1d1c1d1e1f1a1b1a1b1a1b13737e9f937373737b9c9c9c9c9c9c9d93737373737373737cccccccccccccc3737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 008:3737891717a9373737378090370a171717178090373737373737e9f937373737b9c9c9c9c9c9c9d93737373737373737373737373737373737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 009:3737891717a93737373781910a17171717178191372a173a3737e9f937373737bacacacacacacada3737373737373737373737373737373737373737373737373737373737373737373700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 010:37378917889898a83737370a1717cdcf171780902aedcf173a37e9f937373737373737373737373737373737373704142434243424344454e8f80414243444543737373737373737829282928090829282920000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 011:37378917891717a937370a171717171717178191171717171737e9f937373737373737373737373737373737373705152535253525352555e9f90535253545553737373737373737839383938191839383930000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 012:37378917891717a9370a17171717171717178090171717171717e9f937373737373737373737373737373737373764748400000000000094e9f984000094a4b4373737373737373782920000e8f8000082920000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 013:37378917891717a90a1717171717171717178191171717171717e9f937373737373737373737373737373737373765758500000000000095e9f985000095a5b5373737373737373783930000e9f9000083930000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 014:1020304041411040102030304041829282928292829282928292829282920414243424342434243424342434243444548400000000000094e9f9840000940414243424342434243444540000e9f9000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 015:1121213142421131112121213142839383938393839383938393839383930515253525352535253525352535253545558500000000000095e9f9850000950515253525352535253545550000e9f9132343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 016:12222232114311132333432131000000000082920000000000000000000064748400000000000000000000000094a4b48400000000000094e9f98400009464748400000000000094a4b40000e9f9112131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 017:0707070707070707070707070707070707070707070707070707070707070000000000000000000000000000000000000000000000132343e9f9692727272727272727272727272727272727e9f9122232134300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 018:0707070707070707072817380707070707070707070707070707070707070000000000000000000000000000000000000000000000112131e9f9076927272727272727272727272727272727e9f9134342123200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 019:0707070707070707281717173807070707070707070707283807070707070000000000000000000000000000000000000000000000122232e9f9070769272727cc27dc27cc27272727272727e9f9123213233343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 020:0707070707070707171717485838070707070707070728171738070707070000000000000000000000000000000000000000132333434242e9f9070707692727272727272727272727272727e9f9424211212131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 021:0707283807070728171748070729173807070707070729171717173807070000000000000000000000000000000000000000112121311343e9f9070707076927809080908090272727272727e9f9134312222232000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 022:0728171738070729174807070707581749070707072838581717171738070000000000000000000000000000000000000000122222321232e9f9070707070769819181918191272727272727e9f9113142000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 023:0729171717490707070707070707070707070707281717380758173907070000000000000000000000000000000000000000004242132343e9f9070707070707692727272727272727272727e9f9123242000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 024:aeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeae2917171749aeaeae28380000000000000000000000000000000000000000000042112131e9f9070707070707076927272727272727272727e9f9421323334300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 025:3807070707070707070707070707070707070707070707070707072817170000000000000000000000000000000000000000000000122232e9f907070707cc07cc076927cc27cc2727272727e9f9421121213100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 026:1717173807070707070707070707070707070707070707070728171717170000000000000000000000000000000000000000000013233343e9f9070707070707070707692727272727272727e9f9421222223200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 027:9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c0000000000000000000000000000000000000000000012222232e9f9070707078090809080908090809027272727e9f9132343424200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 028:9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d9d0000000000000000000000000000000000000000000000421343e9f9070707078191819181918191819127272727e9f9112131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 029:1717171717171717171717171717171717171717171717171717171717170000000000000000000000000000000000000000000000421131e9f9070707076927272727272727272727272727e9f9122232000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 030:1717171717171717171717171717171717171717171717171717171717170000000000000000000000000000000000000000000000421232e9f9070707070769272727272727272727272727e9f9424200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 031:171717171717171717171717171717171717171717171717171717171717000000000000000000000000000000000000000000001323334310203040102030401020304013233343132333431323334313233343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 032:171717171717171717171717171717171717171717171717171717171717000000000000000000000000000000000000000000001121213111212131112121311121213111212131112121311121213111212131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 033:171717171717171717171717171717171717171717171717171717171717000000000000000000000000000000000000000000001222223212222232122222321222223212222232122222321222223212222232000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 034:00000000000000000000000000000000000000eafa000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </MAP>

-- <MAP1>
-- 000:102020202020202020202020202020202020202020202020202020202030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 001:115000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 002:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 003:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 004:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 005:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 006:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 007:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 008:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 009:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 010:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 011:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 012:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 013:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 014:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 015:110000000000000000000000000000000000000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 016:122222222222222222222222222222222222222222222222222222222232000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </MAP1>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000206000000000
-- 001:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000104000000000
-- 002:02000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020000b000000000
-- 003:03000300030033004300630073008300b300d300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300300000000000
-- 032:000000000010001000200030004000400050005000600060007000800080009000a000b000b000c000d000d000e000e000e000f000f010f080f0f0f0324000000000
-- 033:00000000000000000000000010001000100010001000200020002000300040004000500050006000600080009000a000b000c000d000e000f000f000430000000000
-- 034:000000000030003000304030b030f030f030f030f030f030f020f020f020f020f020f020f020f020f020f020f020f020f020f020f020f020f020f020470000000000
-- </SFX>

-- <PATTERNS>
-- 000:855108000000700008100000800008100000900008000000100000000000800008000000100000000000800008100000600008100000500008100000600008000000100000000000000000000000000000000000000000000000000000000000600008000000500008100000600008100000800008000000100000000000600008000000100000000000600008100000400008100000f00006100000400008000000000000000000100000000000000000000000000000000000000000000000
-- 001:88f122000000100000000000800022100000f00020000000100000000000f00020100000000000000000800022000000100000000000800022100000000000000000800022000000f00020000000100000000000f00020000000100000000000600022000000100000000000600022100000d00020000000100000000000d00020000000100000000000600022000000100000000000600022100000600022000000100000000000d00020000000100000000000d00020000000100000000000
-- 002:90003400000000000000000090003e90003e90003e00000090003e00000000000000000090003e00000090003e00000090003e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 003:4a213600000000000000000040003600000040003c00000000000000000040003c00000040003c00000040003600000000000000000040003600000040003600000040003c00000000000000000040003c00000040003c00000000000000000040003600000000000000000040003600000040003c00000000000000000040003c00000040003c00000040003600000000000000000040003600000000000000000040003c000000400036600036800036900036b00036d00036f00036400038
-- </PATTERNS>

-- <TRACKS>
-- 000:0800000800010800010000000000000000000000000000000000000000000000000000000000000000000000000000006f0000
-- </TRACKS>

-- <PALETTE>
-- 000:182c4825717938b764a7f070ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- 001:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

-- <PALETTE1>
-- 000:182c4825717938b764a7f070ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- 001:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE1>

