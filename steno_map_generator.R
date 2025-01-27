mapsl <- list( # Shoulder: S-    Trigger: R- & HR-
  `#`='ur', # no other motions from ur, too uncomfy  
    `#`=c('ur','u'), # misstroke
  G='dl',
    G=c('dl','d'), # misstroke
    J=c('dl','l'), Y=c('dl','l','ul'),
      J=c('dl','l','dl'), # misstroke
  M='r', W=c('r','r'), WH=c('r','r','dr'),
      N=c('r','ur'),
      D=c('r','dr'), # misstroke
    SPW=c('r','l'), #ent-/int-
      SPW=c('r','ul'), SPW=c('r','dl'), # misstrokes
    KW=c('r','r','ur','u'),
  P='ul',
    P=c('ul','u'), # misstroke
  K='u',
    X=c('u','ul'),
    C=c('u','ur'), KM=c('u','ur','r'),
  T='l', F=c('l','l'),
    T=c('l','dl'), # misstroke
    TW=c('l','r'), # No need to double-tap for TW, since TM- doesn't exist.
      TW=c('l','ur'), TW=c('l','dr'), # misstrokes
    TH=c('l','ul'),
      V=c('l','l','ul'),
  D='dr',
    B=c('dr','r'),
  H='d',
    Z=c('d','dl')
)

mapsr <- list( # Shoulder: -F    Trigger: -R & -L
  `#`='ul', # no other motions from ul, too uncomfy
    `#`=c('ul','u'), # misstoke
  M='l', #`*NG`=c('u','u'),
    N=c('l','ul'),
    MT=c('l','r'),
    D=c('l','dl'), # misstroke
  G='dr',
    J=c('dr','r'),
    NG=c('dr','ul'),
      NG=c('dr','u'), NG=c('dr','l'), # misstrokes
  P='ur',
    `*PL`=c('ur','u'), # -mp
    `*PL`=c('ur','dl'), # Another option, slower, but more consistent with how other N/M combos work. 
      `*PL`=c('ur','d'), `*PL`=c('ur','l'), # misstrokes
  T='r',
    NT=c('r','l'), # -nt
      NT=c('r','ul'), NT=c('r','dl'), # misstrokes
    `*NT`=c('r','r','l'), # ^n't / -nth / -ngth 
      `*NT`=c('r','r','ul'),`*NT`=c('r','r','dl'), # misstrokes
    `*T`=c('r','ur'), V=c('r','r','ur'),
  D='dl',
    B=c('dl','l'),
    ND=c('dl','ur'),
      ND=c('dl','u'), ND=c('dl','r'), # misstrokes
  K='u',
    C=c('u','ur'), KT=c('u','ur','r'), # -ch, -ct
      NC=c('u','ur','dl'), # TODO: this motion sucks. Do some rearranging so that K is not so overloaded and you have room to make this easier.
        NC=c('u','ur','d'), NC=c('u','ur','l'), # misstrokes
    NK=c('u','d'),
      NK=c('u','dl'), NK=c('u','dr'), # misstrokes
    GS=c('u','ul'), X=c('u','ul','l'), # -shun, -kshun
  S='d',
    Z=c('d','dl'),
    RB=c('d','dr') # -sh
)

#STKPWHR AO*EU FRPBLKCGTSDZ

# TODO: because -R is not on the stick, I can't deal with -rve (-RV -> -FRB). I'll need to either:
#   1. Edit the dictionary s.t. *FR = -rve, and fix all conflicts that causes
#   2. Deal with it and just memorize that -F + -RB (-sh) = -rve
#   2. Edit custom steno system
#   However, for now I can simply type v (*F) as a second stroke for -rve
#   Doesn't cost me that much speed, and it will be easy to relearn if/when I fix this later.

sub2  <- \(x,pattern,replacement)  sub(pattern,replacement,x) # More convenient for piping
gsub2 <- \(x,pattern,replacement) gsub(pattern,replacement,x) # More convenient for piping
translate_to_proper_steno_grammar <- \(strokes, stick) {
  if(!(stick %in% c('left','right'))) stop('Invalid stick name.')
  if(stick=='left') {
    strokes <- strokes |>
      sub2('D','TK'   ) |>
      sub2('B','PW'   ) |>
      sub2('L','HR'   ) |>
      sub2('C','KH'   ) |>
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
  if(stick=='right') {
    strokes <- strokes |>
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


# FIXME this broke since I changed how numbers work in the new steno system
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

