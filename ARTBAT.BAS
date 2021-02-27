DECLARE SUB noise (points%(), i1%, i2%, amp AS SINGLE, recdepth%)
DECLARE FUNCTION posofchar% (s$, c$, charnum%)
DECLARE SUB closeapp ()
DECLARE SUB cleartext (startrow%, numrows%)
DECLARE FUNCTION rand# ()
DECLARE SUB ingamekeys (a$)
DECLARE SUB calctargetpos (target AS ANY, i%)
DECLARE FUNCTION calccompaim% (tank() AS ANY, t%)
DECLARE SUB tankfall (tank AS ANY, i%)
DECLARE SUB puttank (tank AS ANY, i%)
DECLARE SUB landfall (x1%, y1%, x2%, y2%)
DECLARE SUB scanland (lx%, rx%, a%, hitland%, tank AS ANY)
DECLARE FUNCTION hitsomething% (tank AS ANY, c%)
DECLARE SUB groundexplode (tank() AS ANY, t%, xc%, yc%, size%)
DECLARE SUB displaystats (tank() AS ANY, row%, final%)
DECLARE SUB endofturn (tank() AS ANY)
DECLARE SUB sortscore (tank() AS ANY)
DECLARE SUB turnover (tank() AS ANY)
DECLARE SUB setTarget (tank() AS ANY, i%)
DECLARE FUNCTION yninput$ ()
DECLARE FUNCTION intinput% (min%, max%, default%)
DECLARE SUB drawframe ()
DECLARE SUB quit ()
DECLARE SUB hltank (tank() AS ANY, t%)
DECLARE SUB menu (tank() AS ANY)
DECLARE SUB printwins (tank() AS ANY, t%)
DECLARE SUB win (tank() AS ANY)
DECLARE SUB explode (tank AS ANY)
DECLARE SUB compadjust (tank AS ANY, tank2 AS ANY)
DECLARE SUB bounds (v#, min#, max#)
DECLARE SUB compscan (tank AS ANY, tank2 AS ANY)
DECLARE FUNCTION isOut% (x AS SINGLE, y AS SINGLE, xs AS SINGLE, ys AS SINGLE, tank AS ANY)
DECLARE FUNCTION control% (tank AS ANY)
DECLARE SUB updatetank (tank AS ANY)
DECLARE SUB computerAim (tank() AS ANY, t%)
DECLARE FUNCTION shoot% (tank() AS ANY, t%, computer%, countdig%)
DECLARE SUB drawtank (tankpic%(), c%, i%)
DECLARE FUNCTION calcy% (x%)
DECLARE SUB drawlandscape ()
DEFINT A-Z
SCREEN 12
startapp:
CLEAR
DIM SHARED randseed AS DOUBLE
RANDOMIZE TIMER
randseed = TIMER
'KEY(11) ON
'KEY 15, CHR$(128) + CHR$(72)
'KEY(15) ON
'DO
'ON KEY(11) GOSUB test
'ON KEY(15) GOSUB test
'a$ = INKEY$
'k = INP(&H60)
'IF a$ <> "" THEN PRINT ASC(a$); LEN(a$), ASC(LEFT$(a$, 1)); ASC(MID$(a$, LEN(a$), 1))
'IF a$ = CHR$(27) THEN END
'PRINT k
'LOOP
'test:
'PRINT "test"
'RETURN
DIM SHARED tankpic(505)
'DIM SHARED temp(10000)
DIM SHARED replayfmtname AS STRING
replayfmtname = "abrep"
DIM SHARED replayfmtver
replayfmtver = 1
DIM SHARED replay
DIM SHARED restart
DIM SHARED walls
DIM SHARED ceiling
DIM SHARED ground
DIM SHARED numtanks
DIM SHARED onehuman
DIM SHARED turns
DIM SHARED tankid(10)
DIM SHARED maxturns
DIM SHARED humanleft
DIM SHARED curnamnmax
DIM SHARED gdelay AS SINGLE
DIM SHARED contdelay AS SINGLE
maxturns = 10

'Adapt frame delay for Dosbox with 20000 cycles per ms
compile = 1
compileScaleGdelay = 1
compileScaleContDelay = 1
IF compile = 1 THEN
    compileScaleGdelay = 2.5
    compileScaleContDelay = .8
END IF
gdelay = .05 * compileScaleGdelay
contdelay = .9 * compileScaleContDelay

CONST gspeed = .03
CONST maxgspeed = .03
CONST sw = 639
CONST sh = 479
CONST l = 15
CONST h = 10
CONST slength = l * 3 / 5
CONST slength2 = slength * slength
CONST lcol = 3
CONST bcol = 0
CONST framecol = 9
CONST powspeed = 1
CONST aspeed = 6
CONST powmax = 100
CONST powfactor = 1
CONST powdisplayfactor = 10
CONST powdisplaydecimals = 0
CONST powreduction = powmax / 100 'caused by damage
CONST maxpowrnd = powmax / 4
CONST maxarnd = 90 / 4
CONST cy = 33
CONST grav = 1.8
CONST lheight = 150
CONST namnmax = 20
CONST borderoffset = 2

CONST glue = lcol
CONST none = 0
CONST bounce = 13

TYPE shotdata
  up AS INTEGER
  dx AS INTEGER
  dy AS INTEGER
  ceiling AS SINGLE
  shot AS INTEGER
  time AS INTEGER
  arclen AS SINGLE
  weight AS INTEGER
  dig AS INTEGER
  pathisclear AS INTEGER
  powoffset AS SINGLE
  aoffset AS SINGLE
END TYPE

TYPE pointtype
  x AS INTEGER
  y AS INTEGER
END TYPE

TYPE tanktype
  putx AS INTEGER
  puty AS INTEGER
  x AS INTEGER
  y AS INTEGER
  xr AS INTEGER
  yr AS INTEGER
  shootx AS INTEGER
  shooty AS INTEGER
  a AS DOUBLE
  pow AS DOUBLE
  damage AS INTEGER
  cont AS INTEGER
  col AS INTEGER
  ai AS shotdata
  compdif AS INTEGER
  wins AS INTEGER
  destroyed AS INTEGER
  kills AS INTEGER
  targetborder AS INTEGER
  targetindex AS INTEGER
  score AS INTEGER
END TYPE

TYPE particle
  x AS SINGLE
  y AS SINGLE
  xr AS SINGLE
  yr AS SINGLE
  xs AS SINGLE
  ys AS SINGLE
  hit AS INTEGER
END TYPE

TYPE targetinfo
  left AS INTEGER
  right AS INTEGER
  middle AS INTEGER
END TYPE


DIM tank(10) AS tanktype
DIM SHARED pln$(10)
tank(0).col = 14
tank(1).col = 4
tank(2).col = 5
tank(3).col = 6
tank(4).col = 7
tank(5).col = 8
tank(6).col = 2
tank(7).col = 11
tank(8).col = 12


CLS
menu tank()
FOR i = 0 TO numtanks
  drawtank tankpic(), tank(i).col, i
  tank(i).wins = 0
  tank(i).kills = 0
  tank(i).score = 0
  CLS
NEXT
'tank(2).wins = 20
cycle = INT(RND * (numtanks + 1))
IF replay THEN
  GET #1, , start
  PUT #2, , start
ELSE
  start = INT(RND * (numtanks + 1))
  PUT #1, , start
END IF

'win tank(), 0
DO
 GOSUB init
 start = start + 1
 cycle = cycle + 1
 IF start > numtanks THEN start = 0
 IF cycle > numtanks THEN cycle = 0
 t = start
DO
FOR i = 0 TO numtanks
 IF NOT tank(i).destroyed THEN
   PUT (tank(i).putx, tank(i).puty), tankpic(50 * i), PSET
   updatetank tank(i)
 END IF
NEXT

aim = -1
IF replay THEN LOCATE (2), (74): PRINT "Replay"
hltank tank(), t
drawframe
WHILE aim
IF replay THEN
  GET #1, , tank(t).a
  GET #1, , tank(t).pow
  PUT #2, , tank(t).a
  PUT #2, , tank(t).pow
  aim = 0
ELSE
  IF tank(t).cont THEN
    aim = control(tank(t))
  ELSE
    IF tank(t).targetindex < 0 THEN setTarget tank(), t
    IF tank(tank(t).targetindex).destroyed THEN setTarget tank(), t
    'computerAim tank(), t
    hit = calccompaim(tank(), t)
    'IF INKEY$ = CHR$(27) THEN quit
    updatetank tank(t)
    aim = 0
  END IF
END IF
IF restart THEN GOTO startapp
IF tank(t).a > 180 THEN a = 180 - tank(t).a ELSE a = tank(t).a
IF a > 90 THEN a = 180 - a
LOCATE (2), (4): PRINT USING "##"; a;
PRINT "  "
usingstr$ = "####"
IF powdisplaydecimals > 0 THEN usingstr$ = usingstr$ + "." + STRING$(powdisplaydecimals, "#")
LOCATE (2), (12): PRINT USING usingstr$; tank(t).pow * powdisplayfactor;
PRINT "/"; MID$(STR$((powmax - tank(t).damage * powreduction) * powdisplayfactor), 2); "    "
WEND
IF replay THEN
  updatetank tank(t)
ELSE
  PUT #1, , tank(t).a
  PUT #1, , tank(t).pow
END IF
IF shoot(tank(), t, 1, 0) THEN
   IF tank(t).destroyed = 0 THEN tank(t).kills = tank(t).kills + 1
   'printwins tank(), t
END IF

IF restart THEN GOTO startapp
t = t + 1
IF t > numtanks THEN t = 0
winningtank = -1
numdest = 0
FOR i = 0 TO numtanks
IF tank(i).destroyed THEN numdest = numdest + 1 ELSE winningtank = i
NEXT
IF numdest >= numtanks THEN
  turns = turns + 1
  IF winningtank >= 0 THEN tank(winningtank).wins = tank(winningtank).wins + 1
  FOR i = 0 TO numtanks
    tank(i).score = tank(i).kills + tank(i).wins '*2
  NEXT
  IF turns >= maxturns THEN win tank(): GOTO startapp
  endofturn tank()
  IF restart THEN GOTO startapp
  EXIT DO
ELSE
  WHILE tank(t).destroyed
    t = t + 1
    IF t > numtanks THEN t = 0
  WEND
END IF
LOOP
LOOP

DIM takentankpos(10)
init:
CLS
drawlandscape
FOR i = 0 TO numtanks
  takentankpos(i) = -1
NEXT
FOR i = 0 TO numtanks
  'tankpos = i + cycle
  'IF tankpos > numtanks THEN tankpos = tankpos - numtanks - 1
   tankpos = INT(RND * (numtanks + 1))
checkiftankposistaken:
  FOR j = 0 TO i - 1
    IF takentankpos(j) = tankpos THEN
      tankpos = tankpos + 1
      IF tankpos > numtanks THEN tankpos = 0
      GOTO checkiftankposistaken
    END IF
  NEXT
  takentankpos(i) = tankpos

  IF replay THEN
    GET #1, , tank(i).putx
    PUT #2, , tank(i).putx
    tank(i).puty = calcy(tank(i).putx + l / 2) - h / 2
  ELSE
    tank(i).putx = RND * ((sw - l) / (numtanks + 1) - ((sw - l) / (3 * numtanks))) + tankpos * (sw - l) / (numtanks + 1) + ((sw - l) / (6 * numtanks))
    tank(i).puty = calcy(tank(i).putx + l / 2) - h / 2
    tankfall tank(i), i
    tank(i).damage = 0
    PUT #1, , tank(i).putx
  END IF
  'LINE (tank(i).putx + l / 2 - slength - 1, tank(i).puty - slength)-(tank(i).putx + l / 2 + slength + 1, tank(i).puty), bcol, BF
  circlecx = tank(i).putx + l / 2
  FOR circley = -slength TO 0
    xd = SQR(slength2 - circley * circley)
    LINE (circlecx - xd, tank(i).puty + circley)-(circlecx + xd, tank(i).puty + circley), bcol
  NEXT
  'tank(i).puty = cy + 1
  puttank tank(i), i
  tank(i).a = 0
  tank(i).damage = 0
  tank(i).pow = 25
  tank(i).ai.shot = 0
  'printwins tank(), i
  tank(i).destroyed = 0
  tank(i).ai.time = 0
  tank(i).ai.dig = 0
  tank(i).targetindex = -1
NEXT
IF replay THEN
  GET #1, , walls
  GET #1, , ceiling
  GET #1, , ground
  PUT #2, , walls
  PUT #2, , ceiling
  PUT #2, , ground
ELSE
  walls = INT(RND * 3)
  IF walls = 1 THEN walls = glue
  IF walls = 2 THEN walls = bounce
  ceiling = INT(RND * 3)
  IF ceiling = 1 THEN ceiling = glue
  IF ceiling = 2 THEN ceiling = bounce
  'ground = INT(RND * 2)
  'IF ground = 0 THEN ground = glue
  'IF ground = 1 THEN ground = bounce
  ground = glue
  PUT #1, , walls
  PUT #1, , ceiling
  PUT #1, , ground
END IF
drawframe
FOR i = 0 TO numtanks
'setTarget tank(), i
NEXT
RETURN

DEFDBL A-Z
SUB bounds (v, min, max)
IF v > max THEN v = max
IF v < min THEN v = min
END SUB

DEFINT A-Z
FUNCTION calccompaim (tank() AS tanktype, t)
DIM target AS pointtype
DIM x AS DOUBLE, y AS DOUBLE, p AS DOUBLE, x2 AS DOUBLE
DIM a AS DOUBLE, b AS DOUBLE, c AS DOUBLE, bdiv2a AS DOUBLE, px AS DOUBLE, py AS DOUBLE, pxadjust AS DOUBLE, root AS DOUBLE, minpx AS DOUBLE, maxpx AS DOUBLE, time AS DOUBLE, sqrparam AS DOUBLE

target.x = tank(tank(t).targetindex).x
target.y = tank(tank(t).targetindex).y
calctargetpos target, tank(t).targetborder
x = ABS(target.x - tank(t).x)
y = (tank(t).y - target.y)
p = (powmax - tank(t).damage * powreduction) * powfactor
DIM g AS DOUBLE
g = grav
x2 = x * x
a = x2 + y * y
'b = x2 * (2 * y * g - p * p)
'c = g * g * x2 * x2

bdiv2a = x2 * ((2 * y * g - p * p) / (2 * a))
sqrparam = bdiv2a * bdiv2a - x2 * (x2 * g * g / a)
IF sqrparam > 0 THEN root = SQR(sqrparam) 'ELSE PRINT "Too little power to hit target": EXIT FUNCTION
minpx = (SQR(-bdiv2a - root)) * 1.01
IF ceiling > 0 THEN
    DIM minpx2 AS DOUBLE
   DIM pydiv2g AS DOUBLE
   ceilingy = ABS(tank(t).y - cy - borderoffset - 2)
   py = SQR(4 * g * ceilingy)
   pydiv2g = py / (2 * g)
   minpx2 = x / (pydiv2g + SQR(pydiv2g * pydiv2g - y / g + .00001))
   'minpx2 = g * x / SQR(g * ceilingy)
   IF minpx2 > minpx THEN minpx = minpx2
END IF
maxpx = (SQR(-bdiv2a + root)) * .99
'IF maxpx > p THEN LOCATE 3, 1: PRINT "maxpx:"; maxpx

pxadjust = (maxpx - minpx) * .5
    'px = maxpx
    'time = x / px
    'py = (y + g * time * time) / time
  
    'PRINT "maxpx total p:"; SQR(px * px + py * py)
    'px = minpx
    'time = x / px
    'py = (y + g * time * time) / time
    'PRINT "minpx total p:"; SQR(px * px + py * py)
px = maxpx
DO WHILE pxadjust > .01
    tank(t).ai.ceiling = 0
    time = x / px
    py = (y + g * time * time) / time
    IF py > 0 THEN
        tank(t).pow = (SQR(px * px + py * py))
        tank(t).a = (180 * ATN(py / px) / 3.141)
        IF target.x - tank(t).x < 0 THEN tank(t).a = 180 - tank(t).a
        hit = shoot(tank(), t, 0, 0)
        IF hit = tank(tank(t).targetindex).col THEN EXIT DO
    END IF
    IF tank(t).ai.ceiling THEN
        px = px + pxadjust
    ELSE
        px = px - pxadjust
    END IF
    pxadjust = pxadjust / 2
    'IF px < minpx OR px > maxpx THEN
    'PRINT "Invalid px:"; px
    'PRINT "minpx:"; minpx
    'PRINT "maxpx:"; maxpx
    'END IF
LOOP

tank(t).ai.arclen = 0
IF hit = 0 THEN
    dummy = shoot(tank(), t, 0, 1) 'count dig
    tank(t).ai.pathisclear = 0
ELSE
    IF NOT tank(t).ai.pathisclear THEN tank(t).ai.powoffset = maxpowrnd / 2: tank(t).ai.aoffset = maxarnd / 2
    tank(t).ai.pathisclear = -1
END IF
IF restart THEN EXIT FUNCTION
FOR tid = 0 TO tank(t).ai.time
  yspeed# = py - 2 * grav * tid
  speed# = SQR(px * px + yspeed# * yspeed#)
  tank(t).ai.arclen = tank(t).ai.arclen + speed#
NEXT
tank(t).ai.weight = tank(t).ai.time * 5 + tank(t).ai.arclen
'LOCATE 5 + tank(t).targetborder, 5: PRINT "dig: "; tank(t).ai.dig; "        "
'LOCATE 5 + tank(t).targetborder, 15: PRINT "time: "; tank(t).ai.time; "        "
'LOCATE 5 + tank(t).targetborder, 30: PRINT "arclen: "; tank(t).ai.arclen; "         "
'LOCATE 5 + tank(t).targetborder, 45: PRINT "weight: "; tank(t).ai.weight; "          "

tank(t).pow = tank(t).pow / powfactor
calccompaim = hit

'scale difficulty
IF tank(t).compdif = 9 THEN EXIT FUNCTION
DIM rndbottom AS SINGLE, rndtop AS SINGLE, rndrange AS SINGLE
'rndtop = 1 - (tank(t).compdif - 4) / 5
rndtop = 1 - (tank(t).compdif) / 10
IF rndtop > 1 THEN rndtop = 1
rndbottom = 1 * RND
'rndbottom = rndbottom - rndbottom * tank(t).compdif / 4
rndbottom = rndbottom - rndbottom * tank(t).compdif / 9
IF rndbottom < 0 THEN rndbottom = 0
rndrange = rndtop - rndbottom

DIM weight AS SINGLE
weight = tank(t).ai.weight / 500 + .3
IF weight > 1 THEN weight = 1

tank(t).ai.powoffset = tank(t).ai.powoffset * (RND * rndrange + rndbottom) * weight
tank(t).ai.aoffset = tank(t).ai.aoffset * (RND * rndrange + rndbottom) * weight

'IF hit = 0 THEN
    'DIM minPowRnd AS SINGLE, minARnd AS SINGLE
    'minPowRnd = maxpowrnd * (1 - (tank(t).compdif + 20) / 29)
    'IF tank(t).ai.powoffset < minPowRnd * 1.2 THEN tank(t).ai.powoffset = minPowRnd * 1.2
    'minARnd = maxarnd * (1 - (tank(t).compdif + 20) / 29)
    'IF tank(t).ai.aoffset < minARnd THEN tank(t).ai.aoffset = minARnd
'END IF
'LOCATE 5, 1: PRINT tank(t).ai.powoffset, tank(t).ai.aoffset
'LOCATE 6, 1: PRINT "rndrange:"; rndrange
'LOCATE 7, 1: PRINT "hit:"; hit
'LOCATE 8, 1: PRINT "time:"; INT(time)

IF hit THEN
    pm = INT(RND * 2)
    IF pm = 0 THEN pm = -1
ELSE
    pm = -1
END IF
tank(t).pow = tank(t).pow + tank(t).ai.powoffset * pm

IF tank(t).a > 90 THEN left = 1: tank(t).a = 180 - tank(t).a
'pm = INT(RND * 2)
'IF pm = 0 THEN pm = -1
tank(t).a = tank(t).a + tank(t).ai.aoffset * pm

bounds tank(t).pow, 0, 1000
bounds tank(t).a, 0, 89

IF left THEN tank(t).a = 180 - tank(t).a
END FUNCTION

SUB calctargetpos (target AS pointtype, i)
IF walls = glue OR i = 0 THEN EXIT SUB

IF walls = 0 THEN  'wrap
 target.x = target.x + (sw - 1) * i
ELSEIF walls = bounce THEN
  IF i = -1 THEN 'bounce left
    target.x = target.x - ((target.x - borderoffset) * 2 - 1)
  ELSEIF i = 1 THEN 'bounce right
    target.x = target.x + ((sw - target.x - borderoffset) * 2 - 1)
  END IF
END IF
END SUB

FUNCTION calcy (x)
y = sh
WHILE POINT(x, y) <> bcol OR y > 479 - h
  y = y - 1
WEND
calcy = y
END FUNCTION

SUB cleartext (startrow, numrows)
LOCATE startrow, 1
FOR i = 1 TO numrows
  PRINT SPACE$(80)
NEXT
END SUB

SUB closeapp
CLOSE
KILL "temprec"
END
END SUB

SUB compadjust (tank AS tanktype, tank2 AS tanktype)
IF tank.a > 90 THEN a = 180 - tank.a ELSE a = tank.a
dxs = SGN(tank.x - tank2.x)
IF tank.ai.ceiling > 0 THEN  'ceiling=glue
  ar! = 3.14 * a / 180
  tank.pow = tank.pow - ABS(COS(ar!)) * 200
  a = a - ABS(SIN(ar!)) * 5
ELSEIF tank.ai.up AND tank.ai.dx * dxs > 0 AND tank.ai.ceiling = 0 THEN  'v„gg=glue, ys>0
  a = a + RND * 10
  IF a >= 90 THEN a = 89
ELSEIF tank.pow > 700 AND tank.ai.dx * dxs > 0 THEN  'minska vinkeln
  a = a - RND * 10
  IF a < 0 THEN a = 0
ELSE    'inga specialfall
  tank.pow = tank.pow + RND * (tank.ai.dx * dxs / 2) + (tank.ai.dx * dxs / 2)  '* (ABS(a - 45) ^ 1.1 / 3)
 
   IF tank.ai.dy < 0 THEN
     IF ABS(tank.ai.dx) < 20 AND a < 30 THEN
       tank.pow = tank.pow + tank.ai.dy
     ELSE
       IF tank.ai.dx * dxs < 0 THEN
         IF tank.ai.up THEN tank.pow = tank.pow + tank.ai.dy
       ELSE
         tank.pow = tank.pow + tank.ai.dy * .2
       END IF
     END IF
  END IF
  IF tank.ai.dy > 0 AND tank.ai.dx * dxs < 20 AND tank.ai.dx * dxs > 0 THEN tank.pow = tank.pow + tank.ai.dy / (ABS(tank.ai.dx / 3) + .001)
END IF

IF dxs > 0 THEN tank.a = 180 - a ELSE tank.a = a
END SUB

SUB compscan (tank AS tanktype, tank2 AS tanktype)
d = tank.x - tank2.x
dys = SGN(tank.y - tank2.y)
IF d < 0 THEN
lx = tank.x: rx = tank2.x
ELSE
lx = tank2.x: rx = tank.x
END IF
ainit = 180 * (ATN(ABS(tank.y - tank2.y) / (ABS(tank.x - tank2.x) + .001)) * dys) / 3.141
scanland lx, rx, a, hitland, tank
scanland rx, lx, a, hitland, tank2

IF ainit < 0 THEN a = a + ABS(a - ainit) / 2: IF a >= 90 THEN a = 89
tank.pow = ABS(d) * ((ABS(a - 45) / 40!) + .5) ^ 2 + SIN(a) * 200
dy = tank.y - tank2.y
IF a = 0 AND dy < 0 OR dy > 0 THEN tank.pow = tank.pow + dy
IF a = ainit OR NOT hitland AND dys > 0 THEN tank.pow = powmax: a = ainit + 5: IF a >= 90 THEN a = 89
IF d < 0 THEN tank.a = a ELSE tank.a = 180 - a
END SUB

SUB computerAim (tank() AS tanktype, t)
dif! = 1
IF tank(t).ai.shot = 0 THEN

compscan tank(t), tank(tank(t).targetindex)
bounds tank(t).pow, 0, powmax - tank(t).damage * powreduction
'dif! = .3
END IF

FOR i = 0 TO tank(t).compdif '* dif!
 compadjust tank(t), tank(tank(t).targetindex)
 bounds tank(t).pow, 0, powmax - tank(t).damage * powreduction
 IF shoot(tank(), t, 0, 0) THEN GOTO hit
 q$ = INKEY$
 IF q$ <> "" THEN IF ASC(q$) = 27 THEN quit
NEXT
hit:

END SUB

FUNCTION control (tank AS tanktype)
STATIC length
wide$ = INKEY$
IF wide$ <> "" THEN length = LEN(wide$)
a$ = RIGHT$(wide$, 1)
IF a$ <> "" THEN k = ASC(a$) ELSE k = 128
k2 = INP(&H60)
'LOCATE 4, 1: PRINT k2; LEN(wide$)
scale = 1

IF length < 2 THEN     'numpad
FOR i! = 0 TO 2000 * contdelay * gdelay
'loopinp = INP(&H60)
'IF loopinp < 128 THEN loop$ = INKEY$: ingamekeys loop$
loop$ = INKEY$
IF loop$ <> "" THEN a$ = RIGHT$(loop$, 1): k = ASC(a$)
IF a$ = CHR$(27) OR a$ = " " OR a$ = CHR$(13) THEN
  GOTO breakcontdelay
ELSEIF LEN(loop$) = 2 THEN
  length = 2
  GOTO breakcontdelay
END IF
NEXT
breakcontdelay:

scale2 = 6
IF k2 = 30 THEN tank.a = tank.a + aspeed / scale2
IF k2 = 32 THEN tank.a = tank.a - aspeed / scale2
IF k2 = 17 THEN tank.pow = tank.pow + powspeed / scale2
IF k2 = 31 THEN tank.pow = tank.pow - powspeed / scale2
END IF

IF k = 75 THEN tank.a = tank.a + 1
IF k = 77 THEN tank.a = tank.a - 1

IF tank.a < 0 THEN tank.a = 180
IF tank.a > 180 THEN tank.a = 0

IF k = 72 THEN tank.pow = tank.pow + powspeed * scale
IF k = 80 THEN tank.pow = tank.pow - powspeed * scale
IF k = 73 THEN tank.pow = tank.pow + powspeed * 10 * scale
IF k = 81 THEN tank.pow = tank.pow - powspeed * 10 * scale
bounds tank.pow, 0, powmax - tank.damage * powreduction
IF k < 128 OR k2 < 128 THEN updatetank tank

IF a$ = CHR$(13) OR a$ = " " THEN control = 0 ELSE control = -1
ingamekeys a$
END FUNCTION

SUB cursorblink
  IF TIMER * 2 MOD 2 = 0 THEN PRINT "_";  ELSE PRINT " ";
  LOCATE (CSRLIN), (POS(0) - 1)
END SUB

SUB displaystats (tank() AS tanktype, row, final)
c = 15
COLOR c
IF final THEN
  r = 33
  header$ = "FINAL "
  us$ = "----------"
  F = 2
ELSE
  r = 36
END IF
header$ = header$ + "STANDINGS"
us$ = us$ + "-----------"
LOCATE (row), (r)
PRINT header$
LOCATE (CSRLIN), (r - (1 + F))
PRINT us$
column = 41 - (25 + curnamnmax) \ 2
LOCATE CSRLIN + 1, column
PRINT SPACE$(2 + curnamnmax) + "| Kills   Wins | Score"
LOCATE CSRLIN, column
PRINT STRING$(2 + curnamnmax, "-") + "+--------------+-------"

sortscore tank()
column2 = column
FOR i = 0 TO numtanks
  column = column2 + 1
  LOCATE (CSRLIN), (column)
  COLOR tank(tankid(i)).col
  PRINT pln$(tankid(i));
 
  'LOCATE CSRLIN, column
  'PRINT "-";
  'IF tank(tankid(i)).cont THEN PRINT "(Human)";  ELSE PRINT " (L"; STR$(tank(tankid(i)).compdif); ")";
 
  COLOR c
  column = column + curnamnmax + 1
  LOCATE CSRLIN, column
  PRINT "|  ";
  column = column + 10
  PRINT USING "##"; tank(tankid(i)).kills; : LOCATE CSRLIN, column: PRINT tank(tankid(i)).wins;
  column = column + 5
  LOCATE CSRLIN, column: PRINT "|  ";
  PRINT USING "##"; tank(tankid(i)).score
NEXT
END SUB

SUB drawframe
LOCATE (1), (3): PRINT "Angle:"
LOCATE (1), (15): PRINT "Power:"

IF walls THEN
    LINE (borderoffset, cy + 1)-(borderoffset, sh), walls
    LINE (sw - borderoffset, cy + 1)-(sw - borderoffset, sh - 1), walls
END IF
'LINE (borderoffset, sh - borderoffset)-(sw - borderoffset, sh - borderoffset), ground
IF ceiling <> 0 THEN LINE (1, cy + borderoffset)-(sw - 1, cy + borderoffset), ceiling

IF walls AND ceiling THEN
  LINE (1, cy + 1)-(sw - 1, sh), 0, B
ELSEIF walls THEN
  LINE (1, cy + 1)-(1, sh), 0
  LINE (sw - 1, cy + 1)-(sw - 1, sh), 0
END IF
  

LINE (0, 0)-(sw, cy), framecol, B
LINE -(0, sh), framecol, B

END SUB

DEFSNG A-Z
SUB drawlandscape
INC = 1.01
DEC = .99

   lut = RND + .5      'slumpar ut om landskapet ska bli bergigt eller plant
   intervall = INT(RND * 100) + 50 'slumpar ut om det ska vara m†nga eller
                                    'f† tv„ra h”jd„ndringar
   xl = 0
   yl = 250
   intyl% = CINT(yl)
   PSET (xl, intyl%)
   IF replay% = 0 THEN PUT #1, , intyl%

IF replay% THEN
   FOR xl = 0 TO sw
     GET #1, , intyl%
     PUT #2, , intyl%
     LINE -(xl, intyl%), lcol
   NEXT
ELSE

 DIM points(640) AS INTEGER
 points(0) = 480 / 2
 points(sw - 1) = 480 / 2
 noise points(), 0, sw - 1, 480 / 2, 0

 PSET (0, points(0))
 FOR x = 1 TO sw - 1
   IF points(x) = 0 THEN
       x1 = x
       y1 = points(x - 1)
       WHILE points(x) = 0: x = x + 1: WEND
       x2 = x
       y2 = points(x)
       add = (y2 - y1) / (x2 - x1 + 1)
       FOR i = x1 TO x2 - 1
         points(i) = points(i - 1) + add
       NEXT
   END IF
   'LINE -(x, points(x)), col
 NEXT
 
 DO
   land = INT(RND * 90) + intervall 'l„ngden p† f”ljande intervall
   ylp2 = 1
   ylp3 = INT(RND * 2)
   IF ylp3 = 0 THEN ylp3 = INC  'positiv lutning
   IF ylp3 = 1 THEN ylp3 = DEC  'negativ    "
   ylp = (RND * 6 - 3) * lut 'brantheten f”r f”ljande intervall
  
   FOR j = 1 TO land 'avb”jningar i intervallet
      ylp2 = ylp2 * ylp3
      IF ABS(ylp2) < .01 THEN ylp3 = INC: ylp = ylp * -1
      IF ABS(ylp2) > 1.99 THEN ylp3 = DEC
      yl = yl + ylp * ylp2
      'IF yl < lheight THEN ylp = ylp * -1
      'IF yl > sh - 10 THEN ylp = ylp * -1
     
      'noise
      scale = .5
      'yl2 = 480 / 2 + (points(xl) - 480 / 2) * scale
      yl2 = yl + (points(xl) - 480 / 2) * scale
      IF yl2 > sh - 10 THEN yl2 = (sh - 10) * 2 - yl2: IF ylp > 0 THEN ylp = ylp * -1
      IF yl2 < lheight THEN yl2 = lheight * 2 - yl2: IF ylp < 0 THEN ylp = ylp * -1
     
      xl = xl + 1

      intyl% = CINT(yl2)
      LINE -(xl, intyl%), lcol
      PUT #1, , intyl%
      IF xl >= sw THEN EXIT DO
   NEXT
 LOOP

END IF
PAINT (1, sh), lcol
END SUB

DEFINT A-Z
SUB drawtank (tankpic(), c, i)
x = 70: y = 70
LINE (x, y)-(x + l, y + h / 2), c, BF
LINE (x, y)-(x + l / 3, y - h / 2), c
LINE -STEP(l / 3, 0), c
LINE -STEP(l / 3, h / 2), c
LINE -STEP(-l, 0), c
PAINT (x + l / 2, y - 1), c
GET (x, y - h / 2)-(x + l, y + h / 2), tankpic(50 * i)
END SUB

SUB endofturn (tank() AS tanktype)
'have extra width of 3 chars
xbox = (25 + 3) * 8 + curnamnmax * 8: ybox = 147
xshift = 3: ysdhift = 0

x1 = sw / 2 - xbox / 2 + xshift
x2 = x1 + xbox + xshift2
y1 = sh / 2 - ybox / 2 - 40 + yshift
y2 = y1 + ybox + 16 * (numtanks - 1) + yshift + 21

LINE (x1, y1)-(x2, y2), bcol, BF

'drawframe'''''''''''''''''''''''''''''''
dp = 1
LINE (x1 + dp, y1 + dp)-(x2 + dp, y2 + dp), 8, B
dp = -1
LINE (x1 + dp, y1 + dp)-(x2 + dp, y2 + dp), 15, B
LINE (x1, y1)-(x2, y2), 7, B

''''''''''''''''''''''''''''''''''''''''''
displaystats tank(), 10, 0
LOCATE (CSRLIN + 1), (33)
IF maxturns - turns > 1 THEN s$ = "s"
PRINT maxturns - turns; "round"; s$; " left"
WHILE INKEY$ <> "": WEND
a$ = ""
WHILE a$ = ""
a$ = INKEY$
WEND
IF a$ = CHR$(27) THEN quit
END SUB

SUB explode (tank AS tanktype)
LINE (tank.x, tank.y)-(tank.shootx, tank.shooty), bcol
LINE (tank.putx - 1, tank.puty)-(tank.putx + l + 1, tank.puty + h), bcol, BF
'LINE (tank.putx, tank.puty)-(tank.putx + l, tank.puty + h), 15, B
FOR i! = 0 TO 6.2 STEP .1
  LINE (tank.x, tank.y + h / 2)-STEP(COS(i!) * 14, SIN(i!) * 14), bcol
NEXT
numpart = 10
DIM p(numpart) AS particle
DIM buf(800, numpart) AS pointtype

FOR i = 0 TO numpart
  p(i).x = tank.x
  p(i).y = tank.y
  p(i).xs = (RND * 22 - 22 * .5) * gspeed
  p(i).ys = -RND * 13
NEXT
WHILE hit < numpart
'FOR aetrth! = 0 TO 30000: NEXT
FOR i = 0 TO numpart - 1
  IF NOT p(i).hit THEN
    p(i).x = p(i).x + p(i).xs
    p(i).y = p(i).y + gspeed * (p(i).ys + grav * gspeed)
    p(i).ys = p(i).ys + 2 * grav * gspeed
    buf(b, i).x = p(i).x
    buf(b, i).y = p(i).y
    c = POINT(p(i).x, p(i).y)
    IF c = lcol OR c = -1 THEN hit = hit + 1: p(i).hit = -1
    'PSET (p(i).x, p(i).y), 15
  END IF
NEXT
b = b + 1
WEND
b = b - 1
sudda = INT(RND * 2) - 1
sudda = -1
particlelength = INT(RND * 5) + 2
FOR i = 0 TO b + particlelength
  FOR aetrth! = 0 TO 1 + 435 * gdelay
  a$ = INKEY$
  IF a$ = "d" OR a$ = "c" OR a$ = CHR$(27) THEN GOTO breakexplodedelay
  NEXT
breakexplodedelay:
  ingamekeys a$
  IF restart THEN EXIT SUB
  FOR j = 0 TO numpart - 1
    IF i > particlelength THEN
        x = buf(i - particlelength, j).x
        y = buf(i - particlelength, j).y
        IF sudda AND POINT(x, y) = tank.col THEN PSET (x, y), bcol
    END IF
    IF i < b THEN IF POINT(buf(i, j).x, buf(i, j).y) = 0 THEN PSET (buf(i, j).x, buf(i, j).y), tank.col
  NEXT
NEXT
END SUB

SUB groundexplode (tank() AS tanktype, t, xc, yc, size)
 size2 = size ^ 2
 FOR y = -size TO size
   xd = SQR(size2 - y ^ 2)
   x1 = xc - xd
   x2 = xc + xd
   FOR x = x1 TO x2
     FOR i = 0 TO numtanks
       IF POINT(x, y + yc) = tank(i).col AND i <> t THEN tank(i).damage = tank(i).damage + 1
     NEXT
   NEXT
   LINE (x1, y + yc)-(x2, y + yc), bcol
 NEXT
                  
 y2 = yc + size
 x1 = xc - size
 x2 = xc + size
 landfall x1, lheight, x2, y2
 FOR i = 0 TO numtanks
   tankfall tank(i), i
 NEXT
END SUB

FUNCTION hitsomething (tank AS tanktype, c)
hitsomething = c <> bcol AND c <> tank.col AND c <> 15 AND c <> -1 AND c <> framecol AND c <> bounce 'AND c <> glue
END FUNCTION

SUB hltank (tank() AS tanktype, t)
'LOCATE (1), (47 + t * 10): PRINT "*"
'LOCATE (1), (54 + t * 10): PRINT "*"
'FOR i = 0 TO numtanks
'  IF i <> t THEN
'    LOCATE (1), (47 + i * 10): PRINT " "
'    LOCATE (1), (54 + i * 10): PRINT " "
'  END IF
'NEXT
LOCATE (1), (47)
PRINT SPACE$(namnmax + 8)
LOCATE (1), (47)
COLOR tank(t).col
PRINT pln$(t); "'s turn"
COLOR 15
END SUB

SUB ingamekeys (a$)
    IF a$ = CHR$(27) THEN
       quit
    END IF
END SUB

FUNCTION intinput (min, max, default)
DO
  'cursorblink
  d$ = INKEY$
  IF d$ <> "" THEN
    IF ASC(d$) >= ASC(RIGHT$(STR$(min), 1)) AND ASC(d$) <= ASC(RIGHT$(STR$(max), 1)) THEN
      intinput = ASC(d$) - 48
      PRINT " "; d$
      EXIT DO
    ELSEIF d$ = CHR$(27) THEN
      intinput = ASC(d$)
      EXIT DO
    'ELSEIF d$ = CHR$(13) THEN
     ' intinput = default
     ' PRINT default
     ' EXIT DO
    END IF
  END IF
LOOP
END FUNCTION

FUNCTION isOut (x AS SINGLE, y AS SINGLE, xs AS SINGLE, ys AS SINGLE, tank AS tanktype)
isOut = 0

IF INT(y) <= cy + borderoffset THEN
  IF ceiling = glue THEN isOut = -1: tank.ai.ceiling = -ys
  IF ceiling = bounce THEN ys = ys * -1: y = cy + borderoffset + 1
  IF ceiling > 0 THEN tank.ai.ceiling = 1 'NEW AI
ELSEIF y >= sh THEN
  IF ground = glue THEN isOut = -1
  IF ground = bounce THEN ys = ys * -1: y = sh - 1
END IF

IF INT(x) <= borderoffset THEN
  IF walls = glue THEN isOut = -1
  IF walls = bounce THEN xs = xs * -1: x = 1 + borderoffset
  IF x <= 0 THEN IF walls = none THEN x = sw - 1
ELSEIF x >= sw - borderoffset THEN
  IF walls = glue THEN isOut = -1
  IF walls = bounce THEN xs = xs * -1: x = sw - borderoffset - 1
  IF x >= sw THEN IF walls = none THEN x = 1
END IF
END FUNCTION

SUB landfall (x1, y1, x2, y2)
FOR x = x1 TO x2
    y = y2
    WHILE POINT(x, y) <> bcol
      y = y - 1
      IF y < y1 THEN EXIT SUB
    WEND
    yh2 = y
    WHILE POINT(x, y) <> lcol
      y = y - 1
      IF y = y1 THEN GOTO lfcontinue
      IF y < y1 THEN EXIT SUB
    WEND
    yh1 = y
    WHILE POINT(x, y) = lcol
      y = y - 1
    WEND
    yt = y
   
    LINE (x, yt)-STEP(0, yh1 - yt), bcol '+ 15
    'LINE (x, yt)-(x, yh1), bcol
    'LINE (x, yh1)-(x, yh2), lcol + 2
    LINE (x, yh2 - (yh1 - yt))-STEP(0, yh1 - yt), lcol '+ 2
lfcontinue:
NEXT
END SUB

SUB menu (tank() AS tanktype)
redo:
CLOSE #1
CLOSE #2
OPEN "temprec" FOR OUTPUT AS #1: CLOSE #1
OPEN "temprec" FOR BINARY AS #1

curnamnmax = 0
CLS
PRINT "                              Artillery Battle"
PRINT "                           ----------------------"
PRINT
'redo:
'LOCATE 7, 1
'LINE (0, 90)-(640, 480), 0, BF

PRINT "Choose number of players(2-9) to start a new game,"
PRINT "or press r to play a replay file."
PRINT "> ";
a$ = ""
DO
DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF ASC(a$) >= ASC(RIGHT$(STR$(2), 1)) AND ASC(a$) <= ASC(RIGHT$(STR$(9), 1)) THEN
    p = ASC(a$) - 48
    PRINT " "; a$
    EXIT DO
ELSEIF a$ = CHR$(27) THEN
  closeapp
ELSEIF a$ = "r" THEN
    PRINT "r"
    'PRINT
    INPUT "Enter filename:", file$
    periodpos = posofchar(file$, ".", 1)
    IF file$ = "" THEN GOTO redo
    IF periodpos = 0 AND LEN(file$) > 8 OR periodpos > 9 THEN PRINT "Filename can't have more than 8 characters.": SLEEP: GOTO redo
    IF periodpos = 0 THEN file$ = file$ + ".rep"
    CLOSE #1
    OPEN file$ FOR BINARY AS #1
    IF LOF(1) = 0 THEN PRINT "File not found.": CLOSE #1: KILL (file$): SLEEP: GOTO redo
    OPEN "temprec" FOR BINARY AS #2
    rfname$ = replayfmtname
    GET #1, , rfname$
    PUT #2, , rfname$
    IF rfname$ <> replayfmtname THEN PRINT "Unknown fileformat.": SLEEP: GOTO redo
    GET #1, , rfver
    PUT #2, , rfver
    IF rfver > replayfmtver THEN PRINT "Wrong version number.": SLEEP: GOTO redo
    replay = -1
    GET #1, , numtanks
    PUT #2, , numtanks
    FOR i = 0 TO numtanks
     GET #1, , namelength
     PUT #2, , namelength
     pln$(i) = ""
     FOR n = 1 TO namelength
       GET #1, , char&
       PUT #2, , char&
       pln$(i) = pln$(i) + CHR$(SQR((char& \ 5) - 1104))
     NEXT
     GET #1, , tank(i).cont
     PUT #2, , tank(i).cont
     IF tank(i).cont THEN onehuman = -1
    NEXT
    GET #1, , curnamnmax
    PUT #2, , curnamnmax
    EXIT SUB
END IF
LOOP
PUT #1, , replayfmtname
PUT #1, , replayfmtver
numtanks = p - 1
PUT #1, , numtanks
FOR i = 0 TO numtanks
  PRINT
  PRINT "Is player"; i + 1; "a human player? ";
yn:
  a$ = yninput$
  IF a$ = "y" THEN
    GOSUB human
  ELSEIF a$ = CHR$(27) THEN
    GOTO redo
  ELSEIF a$ = "n" THEN GOSUB intel
  ELSE GOTO yn
  END IF
  namnlen = LEN(pln$(i))
  IF namnlen > namnmax THEN namnlen = namnmax
  IF curnamnmax < namnlen THEN curnamnmax = namnlen
  pln$(i) = LEFT$(pln$(i), namnmax)
  namelength = LEN(pln$(i))
  PUT #1, , namelength
  FOR n = 1 TO namelength
    char& = ((ASC(MID$(pln$(i), n, 1)) ^ 2) + 1104) * 5
    PUT #1, , char&
  NEXT
  PUT #1, , tank(i).cont
NEXT
PUT #1, , curnamnmax
EXIT SUB

intel:
PRINT "Choose computer intelligence level:(0-9)";
dif = intinput(0, 9, 3)
IF dif = 27 THEN GOTO redo
tank(i).compdif = dif
pln$(i) = "Player" + STR$(i + 1) + "-L" + RIGHT$(STR$(dif), 1)
tank(i).cont = 0
RETURN

human:
tank(i).cont = -1
onehuman = -1
'PRINT
INPUT "Enter your name:", pln$(i)
IF pln$(i) = "" THEN pln$(i) = "Player" + STR$(i + 1)
RETURN
END SUB

SUB noise (points(), i1, i2, amp AS SINGLE, recdepth)
CONST maxrecdepth = 12
rd = recdepth + 1

i3 = (i1 + i2) \ 2
points(i3) = (points(i1) + points(i2)) \ 2
IF rd > 1 THEN points(i3) = points(i3) + (RND - .5) * amp

IF rd < maxrecdepth THEN
  noise points(), i1, i3, amp / 2, rd
  noise points(), i3, i2, amp / 2, rd
END IF

END SUB

FUNCTION posofchar (s$, c$, charnum)
slen = LEN(s$)
FOR i = 1 TO slen
  IF MID$(s$, i, 1) = c$ THEN cn = cn + 1: IF cn = charnum THEN posofchar = i: EXIT FUNCTION
NEXT
posofchar = 0     'char not found
END FUNCTION

SUB printwins (tank() AS tanktype, t)
LOCATE (1), (48 + t * 10): PRINT pln$(t)
LOCATE (2), (50 + t * 10): PRINT tank(t).wins
END SUB

SUB puttank (tank AS tanktype, i)
  tank.x = tank.putx + l / 2
  tank.y = tank.puty
  PUT (tank.putx, tank.puty), tankpic(50 * i), PSET
  tank.shootx = tank.x
  tank.shooty = tank.y
  updatetank tank
END SUB

SUB quit
LOCATE 4, 27
x1 = 150: x2 = x1 + 355
y1 = 142: y2 = y1 + 18

'GET (x1, y1)-(x2, y2), temp
PRINT "Do you really want to quit? ";
IF yninput$ = "y" THEN restart = -1 ELSE LOCATE 4, 27: PRINT SPACE$(41)
'PUT (x1, y1), temp, PSET
'LINE (x1, y1)-(x2, y2), , BF
END SUB

DEFDBL A-Z
FUNCTION rand
CONST a# = 4 * 4000# + 1
CONST b# = 395045
STATIC m#
m# = 2 ^ 24
WHILE randseed > 64000
randseed = randseed - 64000
WEND
randseed = (a# * randseed + b#) MOD m#
WHILE randseed > 64000
randseed = randseed - 64000
WEND
rand = randseed / 64000#
'rand = .5
END FUNCTION

DEFINT A-Z
SUB scanland (lx, rx, a, hitland, tank AS tanktype)
y = tank.y
IF rx < lx THEN s = -1 ELSE s = 1
FOR x = lx TO rx STEP s
  IF POINT(x, y) = lcol THEN
    hitland = -1
    DO
      y = y - 1
    LOOP WHILE POINT(x, y) = lcol
 
    IF y < tank.y THEN
      a2 = 180 * ATN((tank.y - y + 100) / (ABS(x - tank.x) + .1)) / 3.141
      IF a2 > a THEN a = a2
    END IF
  END IF
NEXT
END SUB

SUB setTarget (tank() AS tanktype, i)
DIM maxweight AS SINGLE, weight AS SINGLE
DIM target AS pointtype

IF tank(i).cont THEN EXIT SUB
FOR j = 0 TO numtanks
   IF j <> i AND NOT tank(j).destroyed THEN
      'dx = ABS(tank(i).x - tank(j).x)
      'dy = ABS(tank(i).y - tank(j).y)
      'd = SQR(dx ^ 2 + dy ^ 2)
       
      FOR j2 = -1 TO 1
         IF walls <> glue OR j2 = 0 THEN
           target.x = tank(j).x
           target.y = tank(j).y
           calctargetpos target, j2
           tank(i).targetindex = j
           tank(i).targetborder = j2
           hit = calccompaim(tank(), i)
           IF restart THEN EXIT SUB
           weight = tank(i).ai.weight
           IF hit = 0 THEN weight = weight + tank(i).ai.dig * 500! + 1000
           IF hit = tank(j).col OR hit = 0 THEN
              weight = (1 + tank(j).score / 30) / weight
           ELSE          'fel tank tr„ffad
             weight = 0
           END IF
           IF weight > maxweight THEN maxweight = weight: targetindex = j: targetborder = j2
         END IF
      NEXT
   END IF
NEXT
tank(i).targetindex = targetindex
tank(i).targetborder = targetborder
tank(i).ai.pathisclear = -1
tank(i).ai.shot = 0
tank(i).ai.powoffset = maxpowrnd
tank(i).ai.aoffset = maxarnd
END SUB

FUNCTION shoot (tank() AS tanktype, t, visible, countdig)
  tank(t).ai.shot = 1
  tank(t).ai.ceiling = 0
  tank(t).ai.up = 0
  tank(t).ai.time = 0
  tank(t).ai.dig = 0
  DIM xs AS SINGLE, ys  AS SINGLE, a AS SINGLE
  DIM x AS SINGLE, y AS SINGLE, pow AS SINGLE
  bufmax = 1000
  DIM buf(bufmax) AS pointtype
  humanleft = 0
  FOR i = 0 TO numtanks
    IF tank(i).cont AND NOT tank(i).destroyed THEN humanleft = 1
  NEXT
                         
  a = 3.141 * tank(t).a / 180
  IF tank(t).cont THEN
    x = tank(t).shootx
    y = tank(t).shooty
  ELSE
    x = tank(t).x
    y = tank(t).y
  END IF
  DIM gspeed2 AS SINGLE
  IF visible THEN
    pow = tank(t).pow * powfactor
    gspeed2 = gspeed
  ELSE
     pow = tank(t).pow
     gspeed2 = maxgspeed
  END IF
 
  xs = COS(a) * pow * gspeed2
  ys = -SIN(a) * pow
 
  xr = 1: yr = 1
  shotlength = 5
 ' gs2! = gspeed2 ^ 2
 tid# = TIMER
  'intx = INT(x)
  DO
    'IF visible THEN PSET (x, y), 15
    intx = INT(x)
    tank(t).ai.time = tank(t).ai.time + 1
   
    'LOCATE 4, 50: PRINT tank(t).ai.time; "                  "
    FOR aetg& = 0 TO 1 + 610 * gdelay * eobr * visible * humanleft * (stopdrawing + 1)
      loop$ = INKEY$
      IF loop$ = "d" OR loop$ = "c" OR loop$ = CHR$(27) THEN GOTO breakshootdelay
    NEXT
breakshootdelay:
    ingamekeys loop$
    IF restart THEN EXIT FUNCTION
    startdrawing = eobr OR hit
    IF startdrawing AND visible = 1 AND humanleft = 1 THEN
      IF r >= shotlength OR bufwrap OR hit THEN
        bufindex = r - shotlength
        IF bufindex < 0 AND hit AND bufwrap = 0 THEN
            bufindex = 0
        ELSEIF bufindex < 0 AND bufwrap THEN
            bufindex = bufindex + bufmax
        END IF
        xr = buf(bufindex).x
        yr = buf(bufindex).y
        IF POINT(xr, yr) = 15 THEN PSET (xr, yr), bcol
      END IF
      xtemp = buf(r).x: ytemp = buf(r).y
      IF stopdrawing = 0 AND xtemp > 1 AND xtemp < sw AND ytemp > cy AND ytemp < sh AND POINT(xtemp, ytemp) <> tank(t).col THEN PSET (buf(r).x, buf(r).y), 15
      'IF POINT(buf(r).x, buf(r).y) = 0 THEN PSET (buf(r).x, buf(r).y)
      'xr = buf(r).x: yr = buf(r).y
      r = r + 1
      IF r = bufmax THEN r = 0: bufwrap = -1
      IF hit AND r = w THEN stopdrawing = -1
      IF stopdrawing THEN cleanupcount = cleanupcount + 1
      IF cleanupcount > shotlength THEN EXIT DO
    END IF
    IF NOT hit THEN
      'xs = xs - .5 * .003 'vind
      x = x + xs
      y = y + gspeed2 * (ys + grav * gspeed2)
      ys = ys + 2 * grav * gspeed2
      c = POINT(x, y)
      IF hitsomething(tank(t), c) OR isOut(x, y, xs, ys, tank(t)) THEN
        IF countdig = 0 OR c <> lcol THEN
          hit = -1: eobr = 1: w = w + 1
          IF visible = 0 THEN GOTO compjump
          IF humanleft = 0 THEN EXIT DO 'GOTO onlycpus
        ELSE
          bert = 0
          WHILE intx <> INT(x)
            'intx = INT(x)
            bert = bert + 1
            countdigy = y
            WHILE POINT(intx, countdigy) = lcol
              countdigy = countdigy - 1
              tank(t).ai.dig = tank(t).ai.dig + 1
            WEND
            IF tank(t).ai.dig > 10000 THEN countdig = 0
            'PSET (intx, countdigy), 15
            'FOR saergyu! = 0 TO 100000: NEXT
            'LINE (intx, y)-(intx, countdigy), 15
            'LOCATE 6, 40: PRINT tank(t).ai.dig; "          "
            intx = intx + SGN(INT(x) - intx)
          WEND
          'IF bert > 1 THEN
           ' LOCATE 5, 20: PRINT bert; "     "
          'END IF
        END IF
      END IF
      IF r - w > shotlength OR eobr = 0 THEN
        IF NOT hit THEN
            buf(w).x = INT(x)
            buf(w).y = INT(y)
            w = w + 1
        END IF
      END IF
      IF w = bufmax THEN w = 0: eobr = 1
    END IF
  LOOP
'tid# = TIMER - tid#
'LOCATE 10, 1
'IF tid# THEN PRINT tank(t).ai.time / (tid#)
  iy = y: ix = x
  groundexplode tank(), t, ix, iy, 10
compjump:
tank(t).ai.time = tank(t).ai.time * gspeed2
IF tank(t).ai.time = 0 THEN
    tank(t).ai.time = 1
END IF
'tank(t).ai.dx = x - tank(t).target.x
'tank(t).ai.dy = y - tank(t).target.y
IF ys < 0 THEN tank(t).ai.up = -1

FOR i = 0 TO numtanks
  IF c = tank(i).col OR tank(i).damage > 50 AND NOT tank(i).destroyed THEN
    IF visible THEN explode tank(i): tank(i).destroyed = -1
    shoot = tank(i).col
    EXIT FUNCTION
  ELSE
    shoot = 0
  END IF
NEXT
END FUNCTION

SUB sortscore (tank() AS tanktype)
max = 32000
FOR i = 0 TO numtanks
  max2 = -32000
  FOR j = 0 TO numtanks
    IF max2 <= tank(j).score THEN
      FOR k = 0 TO i - 1
        IF tankid(k) = j THEN GOTO continue
      NEXT
      IF max2 = tank(j).score AND tank(j).kills < maxkills THEN GOTO continue
      max2 = tank(j).score: tankid(i) = j
      maxkills = tank(j).kills
    END IF
continue:
  NEXT
max = max2
NEXT
END SUB

SUB tankfall (tank AS tanktype, i)
   y = tank.puty + h + 1
   y0 = y
   x1 = tank.putx
   x2 = x1 + l
   DO WHILE POINT(x1 + 2, y + 1) = bcol AND POINT(x2 - 2, y + 1) = bcol
     IF tank.destroyed THEN EXIT DO 'continue
     changex = 0
     IF POINT(x1, y) = lcol THEN x1 = x1 + 2: x2 = x1 + l: changex = 1
     IF POINT(x2, y) = lcol THEN x1 = x1 - 2: x2 = x1 + l: changex = changex + 1
     IF changex = 2 THEN EXIT DO
   WHILE POINT(x1, y) = bcol AND POINT(x2, y) = bcol
     LINE (tank.putx + l / 2 - slength - 1, tank.puty - slength)-(tank.putx + l / 2 + slength + 1, tank.puty + h), bcol, BF
     tank.puty = y - h - 1
     tank.putx = x1
     puttank tank, i
     y = y + 1
   WEND
   IF POINT(x1, y) <> lcol AND POINT(x1, y) <> bcol OR POINT(x2, y) <> lcol AND POINT(x2, y) <> bcol THEN EXIT DO
   LOOP
 tank.damage = tank.damage + (y - y0) / 2

END SUB

SUB updatetank (tank AS tanktype)
DIM a AS SINGLE
a = tank.a * 3.141 / 180
LINE (tank.x, tank.y)-(tank.shootx, tank.shooty), bcol
tank.shootx = COS(a) * slength + tank.x
tank.shooty = -SIN(a) * slength + tank.y
LINE (tank.x, tank.y)-(tank.shootx, tank.shooty), tank.col
END SUB

SUB win (tank() AS tanktype)
'tank(0).kills = 10
'tank(0).wins = 5
'tank(0).score = 15

'tank(1).kills = 10
'tank(1).wins = 5
'tank(1).score = 15

'tank(2).kills = 9
'tank(2).wins = 6
'tank(2).score = 15

CLS
displaystats tank(), 3, -1
PRINT
winscore = tank(tankid(0)).score
winwins = tank(tankid(0)).wins
IF tank(tankid(0)).cont THEN humanwin = 1
FOR i = 1 TO numtanks
  'tank(tankid(i)).score = winscore: tank(tankid(i)).wins = winwins
   IF tank(tankid(i)).score = winscore AND tank(tankid(i)).wins = winwins THEN
     tie = tie + 1
     IF tank(tankid(i)).cont THEN humanwin = 1
   END IF
NEXT
IF tie THEN
  PRINT "It's a tie between "; pln$(tankid(0));
  FOR i = 1 TO numtanks
     IF tank(tankid(i)).score = winscore AND tank(tankid(i)).wins = winwins THEN
       tie2 = tie2 + 1
       IF tie2 = tie THEN PRINT " and ";  ELSE PRINT ", ";
       PRINT pln$(tankid(i));
     END IF
  NEXT
  PRINT "."
ELSE
  PRINT ; pln$(tankid(0)); " wins!"
  IF tank(tankid(0)).cont THEN
    PRINT "Good job defeating the enemy artiller";
    IF numtanks > 1 THEN PRINT "ies." ELSE PRINT "y."
    PRINT "The world is a better place now."
  ELSEIF onehuman = 0 THEN
    'PRINT "Was it the one you were cheering for?"
  END IF
END IF
IF humanwin = 0 AND onehuman THEN PRINT "Better luck next time."
PRINT
CLOSE #1
CLOSE #2
PRINT STRING$(80, "-")
PRINT "Press p to play again."
IF replay = 0 THEN PRINT "Press s to save a replay file."
PRINT "Press <Esc> to quit."
PRINT
'IF yninput$ <> "y" THEN PRINT : PRINT "Bye!": END
saverow = CSRLIN
DO
winopt:
a$ = INKEY$
IF a$ = "p" THEN EXIT DO
IF a$ = "s" AND replay = 0 THEN
saverf:
  LOCATE saverow, 1
  INPUT "Enter filename:", file$
  IF file$ <> "" THEN
     periodpos = posofchar(file$, ".", 1)
     IF periodpos = 0 AND LEN(file$) > 8 OR periodpos > 9 THEN cleartext saverow, 2: LOCATE saverow + 1, 1: PRINT "Filename can't have more than 8 characters.": GOTO saverf
     IF periodpos = 0 THEN file$ = file$ + ".rep"
     row = CSRLIN
     OPEN file$ FOR BINARY AS #2
     IF LOF(2) > 0 THEN
        CLOSE #2
        PRINT SPACE$(80)
        LOCATE saverow + 1, 1
        PRINT "File already exists. Do you want to replace it? ";
        a$ = yninput
        IF a$ = "n" OR a$ = CHR$(27) THEN cleartext saverow, 2: GOTO winopt
        KILL file$
        OPEN file$ FOR BINARY AS #2
     END IF
     'SHELL "copy temprec " + file$
      OPEN "temprec" FOR BINARY AS #1
      WHILE EOF(1) = 0
        GET #1, , tempvar
        PUT #2, , tempvar
      WEND
      CLOSE #1
      CLOSE #2
      cleartext saverow, 2
      LOCATE saverow, 1: PRINT "File saved."
  ELSE
      cleartext saverow, 2
  END IF
END IF
IF a$ = CHR$(27) THEN closeapp
LOOP
END SUB

FUNCTION yninput$
DO
  a$ = LCASE$(INKEY$)
  IF a$ = "y" OR a$ = "n" THEN PRINT a$: EXIT DO
  IF a$ = CHR$(27) THEN EXIT DO
LOOP
yninput$ = a$
END FUNCTION

