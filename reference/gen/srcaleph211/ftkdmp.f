      SUBROUTINE FTKDMP(IUNIT,ICTRL)
C
C----------------------------------------------------------------------
C! Dump all fitted tracks from FRFT
CKEY PRINT TRACKS TPC ITC VDET
C!
C!   Author:    R. Johnson  29-06-87
C!
C!   Called by TPCREC
C!
C!   Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!         - ICTRL   /I    Control parameter
C!                         0:  print only track information
C!                         1:  include coordinates
C!                         2:  include wire hits
C!                         3:  include coordinates and wire hits
C!                         NOTE that wire hits generally are not
C!                              available from the POT
C!
C!---------------------------------------------------------------------
      SAVE
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C
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
C
      KFRFT=IW(NAMIND('FRFT'))
      IF (KFRFT.EQ.0) RETURN
C
      KEVEH=IW(NAMIND('EVEH'))
      IF (KEVEH.NE.0) THEN
        IRUN=IW(KEVEH+JEVERN)
        IEVT=IW(KEVEH+JEVEEV)
      ELSE
        IRUN=0
        IEVT=0
      ENDIF
      NTRK=LROWS(KFRFT)
      WRITE(IUNIT,100) NTRK,IRUN,IEVT
  100 FORMAT(/' Dump of ',I3,' fitted tracks for Run ',I5,
     &            '  Event ',I6,':')
C
      DO 50 ITK=1,NTRK
        CALL FRFTDP(ITK,IUNIT,ICTRL)
   50 CONTINUE
C
      WRITE(IUNIT,101) IRUNRC,IEVTRC
  101 FORMAT('------ End of TRACK dump for Run ',I5,
     &            '  Event ',I6,'------'//)
C
      RETURN
      END
