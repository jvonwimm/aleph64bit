      SUBROUTINE VDHITE
C-----------------------------------------------------------------------
C! updates end segment flag in VDSS when track leaves the active volume
CKEY VDET DIGITIZE
C!
C!
C!  Author         G. Taylor       1/8/95
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER JVDSTN, JVDSLN, JVDSPN, JVDSXE, JVDSYE, JVDSZE,
     $   JVDSXL, JVDSYL, JVDSZL, JVDSER, JVDSRN, JVDSES, LVDSSA
      PARAMETER(JVDSTN=1,JVDSLN=2,JVDSPN=3,JVDSXE=4,JVDSYE=5,JVDSZE=6,
     $   JVDSXL=7,JVDSYL=8,JVDSZL=9,JVDSER=10,JVDSRN=11,JVDSES=12,
     $   LVDSSA=12)
      DATA NAVDSS /0/
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
      IF (NAVDSS.EQ.0) NAVDSS = NAMIND('VDSS')
      KVDSS=IW(NAVDSS)
      IF(KVDSS.GT.0) THEN
       ILAST=LROWS(KVDSS)
       IF(ILAST.GT.0) IW(KROW(KVDSS,ILAST)+JVDSES)=1
      ENDIF
      RETURN
      END
