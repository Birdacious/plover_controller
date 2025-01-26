# V should NOT be double tap, b/c it's never used in non-R/L clusters. (ever, evil sure, but evp, evt, evm, evn
# Although LHS V never combos w/ a/t, RHS v is full of combos (cavern, govt, covet, cavity, scavenge, aversion...). So it could deserve the double-tap spot for RHS's sake.
#
# Since L is taken care of by double taps, the next most-combos guy is M & N. Now THEY can take a nice spot on the stick, l or r.
# Let's try to make this maximally forgving. So, cross-deadzone motions should have 3 sectors of forgiveness. Only 1 motion to the clockwise/counterclockwise.
#   That way I can just FLY w/o worrying about finger accuracy.
#   And for speed, no double-tap into cross-deadzone, only double-tap into motion.
#   This is enough restrictions that I think I could list out e/ motion now:
#     u d l r ur dr ul dl
#     double taps of each
#     cross-deadzones of each
#     clockwise/counter-clockwise of each
#     double tap + clockwise/counter-clockwise of each.
#     Total: 8 * 2 * 2 * 2 * 2 = 2^7 = 128, minus some uncomfortable exceptions like LHS ur ur u or ur ur r. That'll do!
#   Aren't I wasting some potential of the # of possibilities the sticks have though?
#     No, it's fine. You start with this simple base, and then you can always make exceptions later for the longer motions that are easy to do accurately.
#
# LK LG are same. But KR/KL and GR/GL are not.
#   So, G can be in a place difficult to double-tap-ish on RHS. That's RHS dr.
#   But LJ is still used. So 
#   Also need NG. MG doesn't exist. Cross-deadzone seems good for this. And it's uncomfy to do this cdz on LHS too so even perfecter spot.
#   LHS has gl- but no jl-. RHS has -lj but no -lg. This means I can replace double-tap + motion to be s/t else.

mapsl <- list( # Trigger: S-    Shoulder: R-
  `#`='ur', # NO other motions from ur, too uncomfy  
    `#`=c('ur','u'), # misstroke
  G='dl', GL=c('dl','dl'),
    J=c('dl','l'), Y=c('dl','l','ul'),
      J=c('dl','l','dl'), # misstroke
    G=c('dl','d'), # misstroke
  M='r', W=c('r','r'), WH=c('r','r','ur'),
      D=c('r','dr'), # misstroke
      N=c('r','ur'),
    SPW=c('r','l'), #ent-/int-
      SPW=c('r','ul'), SPW=c('r','dl'), # misstrokes
    KW=c('r','r','ur','u'),
  P='ul', PL=c('ul','ul'),
    P=c('ul','u'), # misstroke
  K='u', KL=c('u','u'),
    X=c('u','ul'),
    CH=c('u','ur'), KM=c('u','ur','r'),
  T='l', F=c('l','l'),
    T=c('l','dl'), # misstroke
    TW=c('l','r'), FL=c('l','l','r'), # No need to double-tap for TW, since TM- doesn't exist. What to do about -lmt on RHS? RHS has F on shoulder button so this spot frees up.
      TW=c('l','ur'), TW=c('l','dr'), # misstrokes
      FL=c('l','l','ur'), FL=c('l','l','dr'), # misstrokes
    TH=c('l','ul'), V=c('l','l','ul'),
  D='dr', DL=c('dr','dr'),
    B=c('dr','r'), BL=c('dr','dr','r'),
  H='d', L=c('d','d'),
    DL=c('d','dr'), BL=c('d','dr','r'), # Easier to reach DL & BL this way
    Z=c('d','dl')
)

mapsr <- list( # Trigger: -F    Shoulder: -R
  `#`='ul', # and NO other motions from ul, too uncomfy
    `#`=c('ul','u'), # misstoke
  M='l', #`*NG`=c('u','u'), # M=-PL so LM doesn't actually exist oh RHS either!
    N=c('l','ul'),
      NL=c('u','u','ul'), # vuln, anlysis
    MT=c('l','r'),
    D=c('l','dl'), # misstroke
  G='dr', LG=c('dr','dr'), # -gl-
    J=c('dr','r'),
      LG=c('dr','dr','r'), # -lj
    #Y=c('dr','d'),
    NG=c('dr','ul'),
      NG=c('dr','u'), NG=c('dr','l'), # misstrokes
  P='ur', #PL=c('u','u'),
    `*PL`=c('ur','u'), # -mp # Or, 'ur','l' accurate enough?
  T='r', LT=c('r','r'),
    NT=c('r','l'), `*PBT`=c('r','r','l'), # -nt, ^n't / -nth / -ngth
      NT=c('r','ul'), NT=c('r','dl'), # misstrokes
      `*PBT`=c('r','r','ul'),`*PBT`=c('r','r','dl'), # misstrokes
    `*T`=c('r','ur'), V=c('r','r','ur'),
  D='dl', LD=c('dl','dl'),
    LD=c('dl','dl','d'), LD=c('d','dl','dl'), # misstrokes
    B=c('dl','l'), LB=c('dl','dl','l'),
    ND=c('dl','ur'),
      ND=c('dl','u'), ND=c('dl','r'), # misstrokes
  K='u', `*LG`=c('u','u'), # Note: *LG is -lk, LG is -lch/-lj. *PBG is -nk, PBG is -ng. GT is very common -ct.
    FP=c('u','ur'), KT=c('u','ur','r'), # -ch, -ct
      LG=c('u','u','ur'), # -lch/lj or -gl-
      FRPB=c('u','ur','dl'), # -rch/-nch
        FRPB=c('u','ur','d'), FRPB=c('u','ur','l'), # misstrokes
    `*NG`=c('u','d'), # -nk
      `*NG`=c('u','dl'), `*NG`=c('u','dr'), # misstrokes
    GS=c('u','ul'), X=c('u','ul','l'), # -shun, -kshun
  S='d', L=c('d','d'),
    L=c('d','d','dr'), # misstroke
    Z=c('d','dl'),
      LS=c('d','d','dl'), LZ=c('d','d','dl','l'),
    RB=c('d','dr')
)

#STKPWHR AO*EU FRPBLGTSDZ

# What if -K  & -rch/-nch, and I want to add D/S/Z to it?
#   Doesn't matter. Just add the D/S/Z in the next stroke. No workd ending w/ -K or -rch/-nch needs an S or D in it as part of the base word's spelling that can't just be added as a postfix in a separate stroke.

# TODO: because -R is not on the stick, I can't deal with -rve (-RV -> -FRB) nor -rch (-FRP -> FRPB) here. I'll need to either:
#   1. Edit the dictionary s.t. *FR = -rve, and fix all conflicts that causes
#   2. Make my own steno system so I have more control over e/t (probably better)
#   However, for now I can simply type v (*F) as a second stroke for -rve, and always use -nch for -rch.
#   Doesn't cost me that much speed, and it will be easy to relearn if/when I fix this later.

sub2  <- \(x,pattern,replacement)  sub(pattern,replacement,x) # More convenient for piping
gsub2 <- \(x,pattern,replacement) gsub(pattern,replacement,x) # More convenient for piping
translate_to_proper_steno_grammar <- \(strokes, stick) {
  if(!(stick %in% c('left','right'))) stop('Invalid stick name.')
  if(stick=='left') {
    strokes <- strokes |>
      sub2('D' ,'TK'   ) |>
      sub2('B' ,'PW'   ) |>
      sub2('L' ,'HR'   ) |>
      sub2('CH','KH'   ) |>
      sub2('M' ,'PH'   ) |>
      sub2('F' ,'TP'   ) |>
      sub2('Q' ,'KW'   ) |>
      sub2('N' ,'TPH'  ) |>
      sub2('V' ,'SR'   ) |>
      sub2('G' ,'TKPW' ) |>
      sub2('J' ,'SKWR' ) |>
      sub2('Y' ,'KWR'  ) |>
      sub2('X' ,'KP'   ) |>
      sub2('Z' ,'STKPW')
    steno_order <- c('#','S','T','K','P','W','H','R','*')
    strokes <- steno_order[sort(match(strsplit(strokes,'')[[1]],steno_order))]
    strokes <- paste(c(strokes,'-'),collapse='')
  }
  if(stick=='right') {
    strokes <- strokes |>
      sub2('K' ,'BG'  ) |>
      sub2('CH','FP'  ) |>
      sub2('M' ,'PL'  ) |>
      sub2('N' ,'PB'  ) |>
      sub2('J' ,'PBLG') |>
      sub2('SH','RB'  ) |>
      sub2('TH','*T'  ) |>
      sub2('V' ,'*F'  ) |>
      sub2('X' ,'BGS' )
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

names(mapsl) <- sapply(names(mapsl), translate_to_proper_steno_grammar,  'left')
names(mapsr) <- sapply(names(mapsr), translate_to_proper_steno_grammar, 'right')

# Generate the plover_controller file
map2string <- \(map,nm,stick) {
  if(!(stick %in% c('left','right'))) stop('Invalid stick name.')
  paste0(stick,'(',paste(map,collapse=','),') -> ',nm)
}

c(mapply(mapsl,names(mapsl), FUN = \(map,nm) map2string(map,nm,'left')),
  mapply(mapsr,names(mapsr), FUN = \(map,nm) map2string(map,nm,'right'))) |>
writeLines('booger.txt')


library(jsonlite)
# Need num dictionary.
# To do that, make list where
#   nms are strokes w/ plover-auto-transl'd #s,
#   els are what you actually finna get typed
# 1. Get w/e is ul, u, ur, l, etc. on RHS, add '#' to it, set transl to 1234 etc.
#   a. Also {&0.1} {&0.2} etc.
#   b. Also LHS S to names and then {super(1)} {super(2)} etc.
# 2. Transl the nms from #S.. #T... -> 1... 2... as plover would
ns   <- c(`-U`='0',ul='1',u='2',ur='3',l='4',`*`='5',r='6',dl='7',d='8',dr='9')
m <- sapply(names(ns),'==',mapsr)
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

