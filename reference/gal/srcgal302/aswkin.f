      SUBROUTINE ASWKIN
C ---------------------------------------------------------
C - F.RANJARD - 860308
C! Interface Geant3/BOS kine banks
C - Translate GEANT banks : 'VERT' and 'KINE' into similar
C   BOS banks.
C - Called by    ASEVST                       from this .HLB
C - Calls        NBANK, BLIST, NAMIND, BKFMT  from BOS77.hlb
C                ALBOS                        from this .HLB
C ----------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
      COMMON/GCNUM/NGMATE,NGVOLU,NGROTM,NGTMED,NGTMUL,NGTRAC,NGPART
     +     ,NGSTMA,NGVERT,NGHEAD,NGBIT
      COMMON/GCNUMX/ NGALIV,NGTMST
C
      SAVE AVER, IPROG
C - def. of 'VERT' parameters : header length, # of parameters
      PARAMETER (LHVE=3, LPVE=5, LEVE=LHVE+LPVE)
C - def. of 'KINE' parameters : header length, # of parameters
      PARAMETER (LHKI=3, LPKI=5, LEKI=LHKI+LPKI)
      DATA IFI/0/
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C - index of the next vertex/track to be stored in KINE/VERT
C   bank known by its index JVK
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)
C - # of vertices/tracks which could be stored in KINE/VERT
C   bank known by its index JVK
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))
C - index of the 1st parameter of KINE/VERT bank known by its
C   index JVK
      KPARVK(JVK) = JVK + IW(JVK+1)
C - index of 1st vertex/track # contained into the list of
C   bank KINE/VERT known by its index JVK
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)
C - charge of ALEPH particle# JPA
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)
C - # of vertices on a track known by its BOS index /
C   # of outgoing tracks of a vertex known by its BOS index
      NOFVK(JVK)  = IW(JVK+3)
C - Particle type of a track known by its BOS index
      KINTYP(JVK) = IW(KPARVK(JVK)+5)
C - incoming track # of a vertex known by its BOS index
      INPTRK(JVK) = IW(KPARVK(JVK)+5)
C - origin vertex # of a track known by its BOS index
      INPVRT(JVK) = IW(KLISVK(JVK)+1)
C - momentum of a track known by its BOS index
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2
     &                   +RW(KPARVK(JVK)+3)**2)
C - mass of a track known by its BOS index
      PMASVK(JVK) = RW(KPARVK(JVK)+4)
C - energy of a track known by its BOS index
      ENERVK(JVK) = SQRT (PMODVK(JVK)**2 + PMASVK(JVK)**2)
C - time of flight of the icoming particle to the vertex known by its
C   BOS index
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)
C - radius of the vertex known by its BOS index
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)
C - mother track # of a track known by its BOS index
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))
C - alephlib version# used in KINGAL (NKJ is the name-index of KJOB)
      ALEKIN(NKJ) = ITABL(IW(NKJ),1,JKJOAV)
C
      NUMGP(JD) = INT (GB(JD+5))
C ----------------------------------------------------------
C
C - get the alephlib version # used in KINGAL or GALEPH
      IF (IFI .EQ. 0) THEN
         IFI = 1
         CALL KIAVER (AVER,IPROG)
      ENDIF
C
C - Translate GEANT banks 'VERT' and 'KINE' into BOS banks
C
C                                    'VERT' banks
      DO 1 I=1,NGVERT
         JV = LGB(JGVERT-I)
         IF (JV.EQ.0) GOTO 1
         NOUT = GB(JV+7)
         CALL ALBOS ('VERT',I,LEVE+NOUT,KVE,IGARB)
         IW(KVE+1) = LHVE
         IW(KVE+2) = LPVE
         IW(KVE+3) = NOUT
         KV1 = KVE + LHVE
         RW(KV1+1) = GB(JV+1)
         RW(KV1+2) = GB(JV+2)
         RW(KV1+3) = GB(JV+3)
         RW(KV1+4) = GB(JV+4)
         IW(KV1+LPVE) = GB(JV+5)
         DO 11 N=1,NOUT
            IW(KV1+LPVE+N) = GB(JV+7+N)
 11      CONTINUE
 1    CONTINUE
C                                      'KINE'
      DO 2 I=1,NGTRAC
         JK = LGB(JGKINE-I)
C        IF the Geant# .EQ. the NOtracking marker word THEN skip it
         IF (NUMGP(JK) .EQ. NOTRKI) GOTO 2
C
         IF (JK.LE.0) GOTO 2
         NVX = GB(JK+7)+1
         CALL ALBOS ('KINE',I,LEKI+NVX,KKI,IGARB)
         IW(KKI+1) = LHKI
         IW(KKI+2) = LPKI
         IW(KKI+3) = NVX
         KK1 = KKI+LHKI
         RW(KK1+1) = GB(JK+1)
         RW(KK1+2) = GB(JK+2)
         RW(KK1+3) = GB(JK+3)
         IW(KK1+5) = GB(JK+5)
C        In the ALEPHLIB 9.0 the KINE and FKIN banks have been modified:
C        word(4) has been changed to contain mass instead of energy.
C        to be backward compatible the following test is necessary.
C        when files created with an ALEPHLIB earlier than 9.0 will have
C        disappeared only the statment IF(I.GT.NITRKI.. will be necessar
         IF (AVER .LT. 9.0) THEN
            RW(KK1+4) = GB(JK+4)
         ELSE
            IF(I .GT. NITRKI) THEN
               RW(KK1+4) = PARMAS(IW(KK1+5))
            ENDIF
         ENDIF
         IW(KK1+6) = GB(JK+6)
         DO 21 N=2,NVX
            IW(KK1+LPKI+N) = GB(JK+6+N)
 21      CONTINUE
 2    CONTINUE
C
 999  CONTINUE
      RETURN
      END
