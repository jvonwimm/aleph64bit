      PARAMETER (LWORDB = 65536)
      PARAMETER (MAXMCX = 2000, MAXMES = 500)
      COMMON /FYRELA/ IHTYPE,ICALTO
     &               ,ITRCAL(MAXMES),ICALTR(2,MAXMES)
     &               ,LTKNUM,LVXNUM,IMEATR,KINTRK,KINVER
     &               ,FSHDRO,FTKDRO,CUTRAC,BFLCLT,ENMINC
     &               ,INDKIN,INDVER,JDKEKS,JDVNFO,JDVOUT
     &               ,JDKOFN,JDKNFO
      LOGICAL FSHDRO,FTKDRO
      PARAMETER (LFXWBK = 7)
      INTEGER  JDFXWB(LFXWBK)
      EQUIVALENCE (JDFXWB(1),INDKIN)
      COMMON /FYCHAR/ ELISAD
      CHARACTER*48 ELISAD
#if defined(DOC)
C!           FXXX necessary constants or pointers
  IHTYPE : history type (=0 means LUND or no history code)
  ICALTO : Total no. of CAL objects
  ITRCAL : (n) CAL object no. track n is assoc.'d with
  ICALTR : (1,j) counts no. of meas'd tracks assoc.'d to CAL obj.# j
  ICALTR : (2,j) has pointer to first meas'd track assoc.'d to CAL obj.# j
  LTKNUM : # of tracks kept in FKIN (tracks not dropped)
  LVXNUM : # of vertices kept in FVER (vertices not dropped)
  KINTRK : No. of tracks produced by event generator (if known)
  KINVER : No. of vertices produced by event generator (if known)
  FSHDRO : IF true THEN drop history of interactions in calo.
  FTKDRO : IF true THEN drop history of interactions in central det.
  CUTRAC : IF not 0. THEN drop track with momentum < CUTRAC
  ENMINC : minimum energy in ECAL for a CAL object to have good coord.
  BFLCLT : mag. field constant used often in helix routines
  INDKIN : Index of work bank with KINE indices obtained from BWIND
  INDVER : Index of work bank with VERT indices obtained from BWIND
  JDKEKS : index of wkbk giving new track# or 0
           no. of words (=1) / KINE bank
           ===========================================
            1  : = 0 means the KINE bank is dropped
                 > 0 means new track# after dropping
           ============================================
  JDVNFO : index of wkbk giving new vertex# or 0
           no. of words (=1) / VERT bank
           ============================================
            1  : = 0 means the VERT bank is dropped
                 > 0 means new vertex# after dropping
           ============================================
  JDVOUT : work bank giving for each vertex# (FVER numbering)
           the # of daughters and offset to the 1st daugther
           no. of words (=2) / final vertex
           ========================================================
            1  : offset to 1st daughter
            2  : # of daughters
           ========================================================
  JDKNFO : work bank giving new track # for each old KINE #
           no. of words (=1) / KINE track
           ============================================
            1  : = 0 means no new track#
                 > 0 means new track# (FKINE numbering)
           ============================================
  JDKOFN : work bank giving old KINE # for each new track #
           no. of words (=3) / new track (FKINE numbering)
           ==============================================
            1  : KINE track#  of new track# i
            2  : # of daughters of mother track# i-1
            3  : offset to the 1st daughter of mother track# i-1
           ==============================================
  LFXWBK : # of FXXX work banks
  JDFXWB : FXXX work bank indices
#endif
