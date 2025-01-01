dirs <- c(r=1,ur=2,u=3,ul=4,l=5,dl=6,d=7,dr=8)

# JUSTIFICATIONS
# MISC.
# R -> P -> F looks like you're removing a line from the letter each time.
# R -> B going the opposite way b/c looks like you're adding a line, and B ending on a similar hard consonant sound K makes sense.
#     B -> V related sounds (V pronounced B in Spanish). Also good to put V in an uncomfortable spot for countermotions, since it doesn't need counter motions. Also B for "Bottom" in the bottom position
# ^ Also cool to have F & V at the end of these two chains, since they are related sounds.
# G -> J -> Y related sounds
# R is tappable on both sides b/c it is 1) very common 2) has no use for countermotions. 2nd most comfortable spot.
# W -> M b/c upside-down. 
# ^    M -> N -> ent/int- / -n't related sounds, plus the latter ending at the T sector makes sense.
# D is tappable b/c it's a pretty common word starter, and SUPER common on RHS for adding -ed to words.
#   I put it in the spot where it is easy to make motions but unfomfortable to make countermotions. It is good to put R-and-L-already-included things (DR+DL/RD+LD) in these motion spots, b/c they do not need countermotions.
# RHS LG countermotion lands on K which makes sense b/c LG sounds like -lk
# J is in a position on left stick where it is easy to make mistaken countermotions, but doesn't matter b/c J=SKWR
# J is in the same place on right stick, but there it is easier to avoid mistaken countermotions, which is greate, b/c -rj is actually somewhat used

# LEFT STICK
# S = Extremely common, deserves the most comfortable spot.
# T Also very common, deserves a comfortable spot, but doesn't have many consonant combos so doesn't need to have comrortable motions more than a couple sectors away.
# W tappable but in uncomfortable spot to make motoins
# W being opposite of R is nice; WR combo often used for orthographic disambiguation (e.g. right vs write)
# Y is in a spot where it is easy to make mistaken countermotions. But it already includes R (Y=KWR), so that solves that!
# K
#   KH is in a place where countermotions are very uncomfortable, perfect b/c CH+R CH+L sounds don't exist. More comfortable on right stick tho which is good b/c -rch & -lch do exist.
# DL ends in the 'dl' direction!

# RIGHT STICK
# F - F fills in for S often, so same reasoning as for S on left stick.

# TODO?
# Plenty of space to add common prefix/suffixes or other briefs
# I am not too attached to the position of the lone L
    
MAPS_l <- list(
  S='ul',
    SH=c('ul','u'), SW=c('ul','u','ur'), SM=c('ul','u','ur','r'), SN=c('ul','u','ur','r','dr'),
    ST=c('ul','l'), SP=c('ul','l','dl'), SK=c('ul','l','dl','d'), SN=c('ul','l','dl','d','dr'),
  T='l', #↓HR
     L=c('l','ul'), TH=c('l','ul','u'), TW=c('l','ul','u','ur'), TW=c('l','ul','u','ur','r'),
    TR=c('l','dl'), TR=c('l','dl','d'),
  R='dl',           # ^ misstroke, but easy to avoid so could replace. # v misttoke
    P=c('dl','l'), F=c('dl','l','ul'), F=c('dl','l','ul','u'), TW=c('dl','l','ul','u','ur'),
    R=c('dl','d'), B=c('dl','d','dr'), V=c('dl','d','dr','r'),
  K='d', #↑SR
    KR=c('d','dl'), KL=c('d','dl','l'),
    KH=c('d','dr'),  X=c('d','dr','r'), X=c('d','dr','r','ur'),
  D='dr', #TK       #↑KP
    DR=c('dr','d'), DL=c('dr','d','dl'), DL=c('dr','d','dl','l'),
     G=c('dr','r'),  J=c('dr','r','ur'),  Y=c('dr','r','ur','u'),
  #M='r', #↑TPKW    #↑SKWR               #↑KWR
    D=c('r','dr'), DR=c('r','dr','d'), DL=c('r','dr','d','dl'), DL=c('r','dr','d','dl','l'), # misstrokes
    #=c('r','ur'),
  W='ur',
     #=c('ur','r'),
     M=c('ur','u'), N=c('ur','u','ul'), SPW=c('ur','u','ul','l'),
  H='u'                                 #↑ent-/int-
)
MAPS_r <- list(
  F='ur', #↓SH                                                  
    RB=c('ur','u'),  S=c('ur','u','ul'),  Z=c('ur','u','ul','l'),
    FT=c('ur','r'), FP=c('ur','r','dr'), FK=c('ur','r','dr','d'),
  T='r',            #↓TH
     L=c('r','ur'), `*T`=c('r','ur','u'), TS=c('r','ur','u','ul'), TZ=c('r','ur','u','ul','l'),
    RT=c('r','dr'),   LT=c('r','dr','d'),
  R='dr',
    P=c('dr','r'), F=c('dr','r','ur'),
    B=c('dr','d'), V=c('dr','d','dl'), V=c('dr','d','dl','l'),
  K='d', # BG
    RK=c('d','dr'), LG=c('d','dr','r'), NG=c('d','dr','r','ur'), #NG=-nk. Using G plus countermotion more comfy tho.
    FP=c('d','dl'), GS=c('d','dl','l'),  X=c('d','dl','l','ul'),
  D='dl', #↑CH      #↑-shun             #↑BGS. X/-kshun
    RD=c('dl','d'), LD=c('dl','d','dr'), LD=c('dl','d','dr','r'),
     G=c('dl','l'),  J=c('dl','l','ul'),
  #='l',
    #D=c('l','dl'), RD=c('l','dl','d'), LD=c('l','dl','d','dr'), LD=c('l','dl','d','dr','r'), # misstrokes
    #=c('l','ul'),
  #='ul',
    #=c('ul','u'),
    M=c('ul','u'), N=c('ul','u','ur'), `*PBT`=c('ul','u','ur','l'),
  `#`='u',
    S=c('u','ul'), Z=c('u','ul','l') # misstrokes
    #=c('u','ur'),
)
#STKPWHR AO*EU FRPBLGTSDZ

iMAPS_l <- lapply(MAPS_l, \(map) dirs[map])
iMAPS_r <- lapply(MAPS_r, \(map) dirs[map])

generate_countermotions <- \(imap, nm, stick=c('l','r')) {
  if(length(imap)==1) { # Tap
    if(stick=='l') return( list(c(imap,imap)) |> setNames(paste0(nm,'*')) ) 
    if(stick=='r') return( list(c(imap,imap)) |> setNames(paste0('*',nm)) ) 
  }

       if(stick=='r' && nm=='J' ) { nm <- ''; letters2append <- c('RJ', 'LG','*J', 'NG', 'NG') } # J = -PBLG, so how to add N (-PB) and L (-L) to make '-nge' & '-lge'?               Convert J to just -G. And '-mg' is never used so might as well also be '-nge'.
  else if(stick=='r' && nm=='K' ) { nm <- ''; letters2append <- c('RK','*LG','*K','*NG','*NG') } # K = -BG,   so how to add N (-PB)            to make '-nk' (-PBG is already '-ng')? Convert K to *G.
  else if(stick=='r' && nm=='FP') letters2append <- c('FRPB','L','*','FRPB','M'  ) # -FRPB = -rch/-nch both
  else if(stick=='r' && nm=='*F') letters2append <- c('*FRB','L','*','N',   'M'  ) # -*FRB or -FRB = -rve
  else if(stick=='r' && nm=='P' ) letters2append <- c('R',   'L','*','N',   'FRP') # -FRP = -mp
  else if(stick=='r') letters2append <- c('R','L','*','N', 'M' )
  else if(stick=='l') letters2append <- c('R','L','*','R*','L*')
  else stop()
  # Left stick has no countermotions for N/M because o/w it'd be impossible to get a * for words like "cLeRk" or "gLiMpse" where a countermotion is required on both sticks.
  # Does it matter if I can't also get an asterisk for such words in one stroke? Maybe not, but N & M countermotions aren't really needed on left stick a/ws.

  i1 <- imap[1]
  i2 <- imap[2]
  iN <- tail(imap,1)

  clockwise <- i1 == i2-1 || i1==8 && i2==1 

  if(clockwise)
    return(list(c(     imap, iN-1             ),    # 1 (R-)
                c(     imap, iN-1, iN         ),    # 1 misstroke
                c(     imap, iN-1, iN-2       ),    # 2 (L-)
                c(     imap, iN-1, iN-2, iN-1 ),    # 2 misstroke
                c(     imap, iN-1, iN-2, iN-3 ),    # 2 misstroke
                c( i1, imap                   ),    # 3 (*)
                c( i1, imap, iN-1             ),    # 4 (R* or -N)
                c( i1, imap, iN-1, iN         ),    # 4 misstroke
                c( i1, imap, iN-1, iN-2       ),    # 5 (L* or -M)
                c( i1, imap, iN-1, iN-2, iN-1 ),    # 5 misstroke
                c( i1, imap, iN-1, iN-2, iN-3 )) |> # 5 misstroke
           setNames(paste0(nm, letters2append[c(1,1,2,2,2,3,4,4,5,5,5)])))
  else                                        # R R L L L * N N M M M
    return(list(c(     imap, iN+1             ),
                c(     imap, iN+1, iN         ),
                c(     imap, iN+1, iN+2       ),
                c(     imap, iN+1, iN+2, iN+1 ),
                c(     imap, iN+1, iN+2, iN+3 ),
                c( i1, imap                   ),
                c( i1, imap, iN+1             ),
                c( i1, imap, iN+1, iN         ),
                c( i1, imap, iN+1, iN+2       ),
                c( i1, imap, iN+1, iN+2, iN+1 ),
                c( i1, imap, iN+1, iN+2, iN+3 )) |>
           setNames(paste0(nm, letters2append[c(1,1,2,2,2,3,4,4,5,5,5)])))
  stopifnot(F)
}

generate_2taps <- \(imap,nm,stick) {
  if(stick=='l') nm <- paste0(nm,'*')
  if(stick=='r') nm <- paste0('*',nm)
  if(length(imap)==1) {
    l <- list(c(imap[1],imap))
    names(l) <- nm
    l
  }
}

# Walk a map with a window of 3. If are continuous, e.g. 1 2 3 or 7 6 5 etc, then create a misstroke version of the map, with the middle number in that window skipped.
generate_skip_misstrokes <- \(imap, nm) {
  generate_skip1_misstrokes <- \(imap, nm) { 
    n <- length(imap)
    if(n<3) return(NA)
  
    lapply(2:(n-1), \(i) {
      window_of_3 <- imap[(i-1):(i+1)]
      if(all(diff(window_of_3)==1) | all(diff(window_of_3)==-1))
        imap[c(1:(i-1),(i+1):n)]
    }) |>
    `names<-`(rep(nm,n-2)) |>
    Filter(f=Negate(is.null))
  }
  
  skip1 <- generate_skip1_misstrokes(imap,nm)
  skip2 <- unique(unlist(recursive=F, Map(generate_skip1_misstrokes, skip1,names(skip1))))
  c(skip1,skip2)
} #generate_skip_misstrokes(c(2,1,2,3,4,3,2,1,2),'hi') # Test

# For code simplicity, generate_countermotions was allowed to go past the 1-8 range. Need to readjust the numbers.
reframe_numbers <- \(imap) {
  imap[imap<1] <- imap[imap<1]+8
  imap[imap>8] <- imap[imap>8]-8
  imap
}

# TODO: instead make a super-function of \(imap) generate_2taps() |> generate_countermotions() |> etc. and pass iMAPs to THAT?
# Augment the base iMAPS
iMAPS_l <- c(iMAPS_l, unlist(recursive=F,mapply( iMAPS_l, names(iMAPS_l), FUN=generate_countermotions, MoreArgs=list(stick='l') ))) |> Filter(f=is.numeric)
iMAPS_l <- c(iMAPS_l, unlist(recursive=F,mapply( iMAPS_l, names(iMAPS_l), FUN=generate_2taps,          MoreArgs=list(stick='l') ))) |> Filter(f=is.numeric)
iMAPS_l <- c(iMAPS_l, unlist(recursive=F,mapply( iMAPS_l, names(iMAPS_l), FUN=generate_skip_misstrokes                         ))) |> Filter(f=is.numeric)
iMAPS_l <- lapply(iMAPS_l,reframe_numbers)
names(iMAPS_l) <- sub('.*\\.','',names(iMAPS_l)) # NOTE: when dup names, R.nvim can only print the first item w/ that name. To get all entries of a name, need to do iMAPS[names(iMAPS)==nm].
MAPS_l <- lapply(iMAPS_l, \(imap) setNames(names(dirs),dirs)[imap])

iMAPS_r <- c(iMAPS_r, unlist(recursive=F,mapply( iMAPS_r, names(iMAPS_r), FUN=generate_countermotions, MoreArgs=list(stick='r') ))) |> Filter(f=is.numeric)
iMAPS_r <- c(iMAPS_r, unlist(recursive=F,mapply( iMAPS_r, names(iMAPS_r), FUN=generate_skip_misstrokes                         ))) |> Filter(f=is.numeric)
iMAPS_r <- lapply(iMAPS_r,reframe_numbers)
names(iMAPS_r) <- sub('.*\\.','',names(iMAPS_r))
MAPS_r <- lapply(iMAPS_r, \(imap) setNames(names(dirs),dirs)[imap])

# Note ul & dr are most comfy positions on left stick, and ur & dl are most comfy on right.
#hist(unlist(iMAPS_l))
#hist(unlist(iMAPS_r))

translate_to_proper_steno_grammar <- \(strokes, stick) {
  if(!(stick %in% c('left','right'))) stop('Invalid stick name.')
  sub2  <- \(x,pattern,replacement)  sub(pattern,replacement,x) # More convenient for piping
  gsub2 <- \(x,pattern,replacement) gsub(pattern,replacement,x) # More convenient for piping

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

names(MAPS_l) <- sapply(names(MAPS_l), translate_to_proper_steno_grammar, "left")
names(MAPS_r) <- sapply(names(MAPS_r), translate_to_proper_steno_grammar, "right")
#tmp <- sapply(unique(names(MAPS_r)), translate_to_proper_steno_grammar, "right")
#tmp <- data.frame(b4=names(tmp),aft=tmp)

# Generate the plover_controller file
map2string <- \(map,nm,stick) {
  if(!(stick %in% c("left","right"))) stop("Invalid stick name.")
  paste0(stick,"(",paste(map,collapse=','),") -> ",nm)
}

c(mapply(MAPS_l,names(MAPS_l), FUN = \(map,nm) map2string(map,nm,"left")),
  mapply(MAPS_r,names(MAPS_r), FUN = \(map,nm) map2string(map,nm,"right"))) |>
writeLines('booger.txt')

