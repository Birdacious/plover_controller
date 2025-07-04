# Defines for my KK3 Max controller, plus some hard-coded chords
defines <- c(
  'lstick stick has segments (dr,d,dl,l,ul,u,ur,r) on axes 0 and 1 offset by 22.5 degrees',
  'rstick stick has segments (dr,d,dl,l,ul,u,ur,r) on axes 2 and 3 offset by 22.5 degrees',

  'trigger on axis 4 is ltrig',
  'trigger on axis 5 is rtrig',

  'button 0 is a',
  'button 1 is b',
  'button 2 is x',
  'button 3 is y',
  'button 4 is minus',
  'button 5 is home',
  'button 6 is plus',
  'button 7 is lstickclick',
  'button 8 is rstickclick',
  'button 9 is leftshoulder',
  'button 10 is rightshoulder',
  'button 11 is dpup',
  'button 12 is dpdown',
  'button 13 is dpleft',
  'button 14 is dpright',

  'a -> PHRO-LG // Toggle Plover',
  'b -> -E',
  'x -> O-',
  'y -> -U',
  #'minus ->',
  'home -> A-',
  #'plus ->',
  'lstickclick -> PHO-UPL // Activate mouse mode', # "moum"
  'rstickclick -> *',
  'leftshoulder -> S-',
  'rightshoulder -> -F',
  #'dpup ->',
  #'dpdown ->',
  #'dpleft ->',
  #'dpright ->',
  ''
)

maps_ltrig <- list(
  R=c('l'),
  L=c('l','h','l'),
  L=c('l','h'    ), # ! misstroke, if trigger is raised too fast
  STKPWHR=c('l','l'), # Activate modifiers
  SHREUFRPL=c('l','h','l','h','l'), # Activate shrimple
  SHREUFRPL=c(    'h','l','h','l'),
  SHREUFRPL=c('l','h','l','h'    ),
  SHREUFRPL=c(    'h','l','h'    )
)

maps_rtrig <- list(
  R=c('l'),
  L=c('l','h','l'),
  L=c('l','h'    ),
  RL=c('l','l'),
  RL=c('l','l','h','l'),
  RL=c('l','l','h'    ),
  RL=c('l','h','l','l'),
  RL=c('l','h','l','l','h','l'),
  RL=c('l','h',  'l',  'h','l'),
  RL=c('l','h',        'h','l')
)

# TODO: My stick mappings are outdated.
#  They were created under the constraints of a more limited input space before I implemented combos.
#  There is more room to make things more comfortable now.
#  Maybe make -m/-n into a double-tap.
maps_lstick <- list( # Shoulder: S-    Trigger: R- & HR-
  #`#`='ur', # no other motions from ur, too uncomfy
  #  `#`=c('ur','u'), # misstroke
  G='dr',
    J=c('rl','r'), Y=c('rl','r','ur'),
      J=c('rl','r','dr'), # misstroke
  M='r',
    G=c('r','dr'), # misstroke
    N=c('r','ur'),
    W=c('r','r'),
      WH=c('r','r','ur'), KW=c('r','r','ur','u'),
    SPW=c('r','l'), #ent-/int-
      SPW=c('r','ul'), SPW=c('r','dl'), # misstrokes
  P='ul',
    P=c('ul','u'), # misstroke
  K='u',
    KH=c('u','ul'),
    X=c('u','ur'), KM=c('u','ur','r'),
  T='l',
    T=c('l','dl'), # misstroke
    TH=c('l','ul'),
    F=c('l','l'),
      V=c('l','l','ul'),
    TW=c('l','r'), # No need to double-tap for TW, since TM- doesn't exist.
      TW=c('l','ur'), TW=c('l','dr'), # misstrokes
  D='dl',
    B=c('dl','l'),
    D=c('dl','d'), # misstroke
  H='d',
    Z=c('d','dl')
)

maps_rstick <- list( # Shoulder: -F    Trigger: -R & -L
  `#`='ul', # no other motions from ul, too uncomfy
    `#`=c('ul','u'), # misstoke
  M='l',
    N=c('l','ul'), `*NG`=c('l','ul','u'), # -nk
    G=c('l','dl'), # misstroke
    MT=c('l','r'),
    PSDZ=c('l','l'), PBGSDZ=c('l','l','ul','u'), # for 'w' & 'q' fingerspelling
  G='dl',
    J=c('dl','l'), Y=c('dl','l','ul'),
    NG=c('dl','ur'),
      NG=c('dl','u'), NG=c('dl','r'), # misstrokes
  P='ur',
    `*PL`=c('ur','dl'), 
      `*PL`=c('ur','d'), `*PL`=c('ur','l'),  # misstrokes
    `*PL`=c('ur','u'), # alternative, easier motion
    PD=c('ur','r','dr'), # tepid, vapid, etc.
  T='r',
    NT=c('r','l'), `*PBT`=c('r','r','l'), # -nt, ^n't / -nth
      NT=c('r','ul'), NT=c('r','dl'), # misstrokes
      `*PBT`=c('r','r','ul'),`*PBT`=c('r','r','dl'), # misstrokes
    `*T`=c('r','ur'),
    F=c('r','r'),
      V=c('r','r','ur'),
  D='dr',
    B=c('dr','r'),
    ND=c('dr','ul'),
      ND=c('dr','l'), ND=c('rl','u'), # misstrokes
  K='u',
    FP=c('u','ur'), KT=c('u','ur','r'), # -ch, -ct
      FRPB=c('u','ur','dl'), # TODO: this motion sucks. Do some rearranging so that K is not so overloaded and you have room to make this easier. OR, maybe -n/-m double taps instead of cross-deadzone now. That would also fix this.
        FRPB=c('u','ur','d'), FRPB=c('u','ur','l'), # misstrokes
    `*NG`=c('u','d'), # -nk
      `*NG`=c('u','dl'), `*NG`=c('u','dr'), # misstrokes
    X=c('u','ul'), GS=c('u','ul','l'), # -shun, -kshun
  S='d',
    Z=c('d','dl'),
    RB=c('d','dr')
)
# TODO: easy motion for -ngth words

#STKPWHR AO*EU FRPBLKCGTSDZ

sub2  <- \(x,pattern,replacement)  sub(pattern,replacement,x) # More convenient for piping
gsub2 <- \(x,pattern,replacement) gsub(pattern,replacement,x) # More convenient for piping
translate_to_proper_steno_grammar <- \(strokes, side) {
  if(!(side %in% c('lhs','rhs'))) stop('Invalid side')
  if(side=='lhs') {
    strokes <- strokes |>
      sub2('D','TK'   ) |>
      sub2('B','PW'   ) |>
      sub2('L','HR'   ) |>
      sub2('M','PH'   ) |>
      sub2('F','TP'   ) |>
      sub2('Q','KW'   ) |>
      sub2('N','TPH'  ) |>
      sub2('V','SR'   ) |>
      sub2('G','TKPW' ) |>
      sub2('J','SKWR' ) |>
      sub2('Y','KWR'  ) |>
      sub2('X','KP'   ) |>
      sub2('Z','STKPW')
    steno_order <- c('#','S','T','K','P','W','H','R','*')
    strokes <- steno_order[sort(match(strsplit(strokes,'')[[1]],steno_order))]
    strokes <- paste(c(strokes,'-'),collapse='')
  }
  if(side=='rhs') {
    strokes <- strokes |>
      sub2('K' ,'BG'   ) |>
      sub2('M' ,'PL'   ) |>
      sub2('N' ,'PB'   ) |>
      sub2('J' ,'PBLG' ) |>
      sub2('SH','RB'   ) |>
      sub2('TH','*T'   ) |>
      sub2('V' ,'*F'   ) |>
      sub2('X' ,'BGS'  ) |>
      sub2('Y' ,'RPBLG') # Y = KWR on LHS, so Y = RMK (R)(PL)(BG) on RHS
    steno_order <- c('#','*','F','R','P','B','L','G','T','S','D','Z')
    strokes <- steno_order[sort(match(strsplit(strokes,'')[[1]],steno_order))]
    strokes <- paste(c('-',strokes),collapse='')
  }

  if(grepl('\\*',strokes)) strokes <- strokes |> gsub2('\\*','') |> sub2('-','*-')
  if(grepl( '#', strokes)) strokes <- strokes |> gsub2( '#' ,'') |> {\(x)paste0('#',x)}()
  strokes <- strsplit(strokes,'')[[1]]
  strokes <- strokes[!duplicated(strokes)]
  strokes <- paste(strokes,collapse='')
}

names(maps_lstick) <- sapply(names(maps_lstick), translate_to_proper_steno_grammar, 'lhs')
names(maps_rstick) <- sapply(names(maps_rstick), translate_to_proper_steno_grammar, 'rhs')
names(maps_ltrig)  <- sapply(names(maps_ltrig ), translate_to_proper_steno_grammar, 'lhs')
names(maps_rtrig)  <- sapply(names(maps_rtrig ), translate_to_proper_steno_grammar, 'rhs')

# Manual addition of one-handed motions mapping to other-hand or two-hand chords.
maps_rstick <- c(maps_rstick, list(
  # Symbol base for use with Shrimple
  `HA*ERB`   =c('ul','ul'), # #
  OEU        =c('u', 'u' ), # /
  `STPH-FPT` =c('ur','ur'), # : normally STPH-FP_L_T
  `KR*GS`    =c('l', 'l' ), # "
  `TKPWR*PB` =c('r', 'r' ), # >
  `PHR*US`   =c('dl','dl'), # +
  `KW*`      =c('d', 'd' ), # = normally KW*_L_
  `STPH*FPT` =c('dr','dr')  # ; normally STPH*FP_L_T
))

# Generate the plover_controller file
map2string <- \(map,nm,stick_or_trig) {
  paste0(stick_or_trig,'(',paste(map,collapse=','),') -> ',nm)
}

tmp <- c(mapply(maps_lstick, names(maps_lstick), FUN = \(map,nm) map2string(map,nm,'lstick')),
         mapply(maps_rstick, names(maps_rstick), FUN = \(map,nm) map2string(map,nm,'rstick')),
         mapply(maps_ltrig,  names(maps_ltrig ), FUN = \(map,nm) map2string(map,nm,'ltrig')),
         mapply(maps_rtrig,  names(maps_rtrig ), FUN = \(map,nm) map2string(map,nm,'rtrig')))

extract_motion <- \(x) sub(' ->.*', '', tmp[names(tmp)==x])
all_possible_combos <- \(...) apply(expand.grid(...), 1, paste, collapse=' + ')
make_combos <- \(..., output) combos2add <- lapply(list(...),extract_motion) |> all_possible_combos() |> paste('->',output)

# Combos to work around steno rules:
#   -rv = FRB,             but -v = *F
#   -rch (& -nch) = -FRPB, but -ch = FP
#   -lch = also -LG,       but -ch = FP
#   -lj = -LG,             but J = PBLG
#   -lk = *LG,             but -k = BG
# S/t for -mp?
tmp <- c(tmp,
  make_combos('-R','-FP',   output='-FRPB'),
  make_combos('-R','-PB',   output='-FRB' ),
  make_combos('-L','-PBLG', output='-LG'  ),
  make_combos('-L','-FP',   output='-LG'  ),
  make_combos('-L','-BG',   output='*-LG' ))
# Combos for some RHS fingerspellings
tmp <- c(tmp,
  make_combos('-L','-P',     output='-PLZ'   ), # B/c I want L + P to be '3'
  make_combos('-L','-PL',    output='-PLSZ'  ), # B/c I want L + M to be '4'
  make_combos('-R','-RPBLG', output='-RPBLGZ')) # B/c I want R + Y to be 'i'

writeLines(c(defines,tmp), 'booger.txt')

# ----------

library(jsonlite)
# Need num dictionary.
# To do that, make list where
#   names are strokes w/ plover-auto-transl'd #s,
#   items are what you actually finna get typed
# 1. Get w/e is ul, u, ur, l, etc. on RHS, add '#' to it, set transl to 1234 etc.
#   a. Also {&0.1} {&0.2} etc.
#   b. Also LHS S to names and then {super(1)} {super(2)} etc.
# 2. Transl the nms from #S.. #T... -> 1... 2... as plover would
ns   <- c(`-U`='0',ul='1',u='2',ur='3',l='4',`-E`='5',r='6',dl='7',d='8',dr='9')
m <- sapply(names(ns),'==',maps_rstick)
names(ns)[col(m)[m]] <- rownames(m)[row(m)[m]]
names(ns) <- sub('##','#',paste0('#',names(ns)))
ns

wss <- sapply(ns, \(n) paste0('{#super(',n,')}'))
names(wss) <- sub('#','#S',names(wss))
wss

decs <- sapply(ns, \(n) paste0('{&0.',n,'}'))
names(decs) <- sub('#','#PH',names(decs))
decs

# Could be more generalized but idc
simulate_plover_num_conversion <- \(stroke) if(!grepl('#',stroke)) {stroke} else {

  lhs <- strsplit(stroke,'-|\\*')[[1]][1]
  rhs <- strsplit(stroke,'-|\\*')[[1]][2]
  if(grepl('\\*',stroke)) lhs <- lhs |> paste0('*') # sloppy fix to not lose *

  if(!grepl('O|S|T|P|H|A',lhs) & !grepl('F|P|L|T',rhs)) {stroke} else {
    paste0(
      lhs |> sub2('#','') |> sub2('O',0) |> sub2('S',1) |> sub2('T',2) |> sub2('P',3) |> sub2('H',4) |> sub2('A',5),
      rhs |> sub2('F',6 ) |> sub2('P',7) |> sub2('L',8) |> sub2('T',9)
    ) |> sub2('NA$','') # sloppy fix 4 when rhs is empty
  }
}

ns2 <- c(ns,wss,decs)
names(ns2) <- sapply(names(ns2), simulate_plover_num_conversion)
ns2

write_json(as.list(ns2), 'numbers.json', auto_unbox=T,pretty=T)
