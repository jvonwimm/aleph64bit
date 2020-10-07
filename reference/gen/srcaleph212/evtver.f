      SUBROUTINE EVTVER (X,Y,Z)
C----------------------------------------------------------------------
C!  - Get Event Vertex (X,Y,Z) from YTOPOL (x,y) and mean z0
C!
C!   Author   :- E. Lancon              8-FEB-1993
C!
C!   Inputs:
C!        -   None
C!
C!   Outputs:
C!        -   X,Y       /R    x,y position of event vertex from
C!                            YTOPOL PYER bank
C!            Z         /R    from average z0 of tracks with UFITQL
C!                            quality flag = 1 or 2
C!
C?
C!======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JPFRIR=1,JPFRTL=2,JPFRP0=3,JPFRD0=4,JPFRZ0=5,JPFRAL=6,
     +          JPFREO=7,JPFREM=13,JPFRC2=28,JPFRNO=29,LPFRFA=29)
      PARAMETER(JFRIBP=1,JFRIDZ=2,JFRIBC=3,JFRIDC=4,JFRIPE=5,JFRIPM=6,
     +          JFRIPI=7,JFRIPK=8,JFRIPP=9,JFRINK=10,JFRIQF=11,
     +          LFRIDA=11)
C
      DATA KRUNO, KEVTO / 0,0 /
      DATA XO,YO,ZO / 3*0. /
      DATA NAPYER,NAFRFT,NAPFRF,NAFRID /4*0/
C!    set of intrinsic functions to handle BOS banks
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
C----------------------------------------------------------------------
      IF (NAPYER.EQ.0) THEN
        NAPYER = NAMIND('PYER')
        NAFRFT = NAMIND('FRFT')
        NAPFRF = NAMIND('PFRF')
        NAFRID = NAMIND('FRID')
      ENDIF
C
C?   New Run, Evt ???
C
      CALL ABRUEV (KRUN, KEVT)
C
      IF ( KRUN.EQ.KRUNO .AND. KEVT.EQ.KEVTO ) THEN
C
C?   Same (run,evt) get previous values
C
        X = XO
        Y = YO
        Z = ZO
      ELSE
C
C?   Get (x,y) from PYER , First Vertex should be of type 1 or 3
C?            Type of vertex:
C?            =1 if it is the main vertex
C?            =3 if main vtx from 2 trks + beam spot (Bhabha)
C
        X = 0.
        Y = 0.
        KPYER = IW(NAPYER)
        IF ( KPYER.GT.0 ) THEN
          NPYER = LROWS(KPYER)
          IF ( NPYER.GE.1 ) THEN
            IVTP = ITABL(KPYER,1,JPYETY)
            IF ( IVTP.EQ.1 .OR. IVTP.EQ.3 ) THEN
              X = RTABL(KPYER,1,JPYEVX)
              Y = RTABL(KPYER,1,JPYEVY)
            ENDIF
          ENDIF
        ENDIF
C
C?   Get Z from average Z0 of tracks with quality flag = 1 or 2
C?   see UFITQL, only for evts with at least 2 good tracks
C?          Track quality flag
C?            1 = Good track from the origen
C?            2 = Good track but momentum > Ebeam
C
        KFRID = IW(NAFRID)
        KFRFT = IW(NAFRFT)
        JATZ0 = JFRFZ0
        IF (KFRFT.LE.0) THEN
          KFRFT = IW(NAPFRF)
          JATZ0 = JPFRZ0
        ENDIF
        NMEAN = 0
        ZMEAN = 0.
        IF (KFRFT.GT.0) THEN
          NFRFT = LROWS(KFRFT)
          DO IFRFT =  1, NFRFT
            IQUAL = ITABL(KFRID,IFRFT,JFRIQF)
            IF ( IQUAL.EQ.1 .OR. IQUAL.EQ.2 ) THEN
              NMEAN = NMEAN + 1
              ZMEAN = ZMEAN + RTABL(KFRFT,IFRFT,JATZ0)
            ENDIF
          ENDDO
        ENDIF
        IF ( NMEAN.GE.2 ) THEN
          Z = ZMEAN / FLOAT(NMEAN)
        ELSE
          Z = ZO
        ENDIF
C
        XO = X
        YO = Y
        ZO = Z
      ENDIF
C
  999 RETURN
      END
