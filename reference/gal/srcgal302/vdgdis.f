      SUBROUTINE VDGDIS(NFROM,NTO,DIST)
C!----------------------------------------------------------------------
C! Find the distance between two elements
C!  of the Geant geometry description
CKEY VDET GEOM
C!
C!
C!  Author         A. Bonissent 15-Feb-1994
C!
C!  Description
C!  ===========
C!  Navigate into the tree, in bank VPOS, until the mother of the mother
C!                of ... the mother of NFROM be NTO;
C!           during this process, the translations are accumulated.
C!           Rotations are not taken into account
C!
C! Input :   NFROM - Character*4, starting volume                     I
C!           NTO   - Character*4, ending                              I
C!           DIST  - array of distances in the 3 dimansions           I
C!
C! Input : VPOS bank
C!
C-----------------------------------------------------------------------
      DIMENSION DIST(3)
      CHARACTER*4 NFROM,NTO,NLOC,NCURR,CHAINT
      PARAMETER(JVPODA=1,JVPOMO=2,JVPOFL=3,JVPOVR=4,JVPOCN=5,JVPOPO=6,
     +          LVPOSA=8)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      KVPOS = IW(NAMIND('VPOS'))
      IF(KVPOS.EQ.0)THEN
          CALL ALTELL ('VDGDIS : VPOS bank missing',0,'STOP')
      ENDIF
      NVPOS = LROWS(KVPOS)
      NCURR = NFROM
      DO 11 IDIM=1,3
         DIST(IDIM)=0.
   11 CONTINUE
   13 CONTINUE
      IF(NCURR.NE.NTO)THEN
         DO 10 IVPOS = 1, NVPOS
         NLOC = CHAINT(ITABL(KVPOS,IVPOS,JVPODA))
         IF(NLOC.EQ.NCURR)THEN
            NCURR = CHAINT(ITABL(KVPOS,IVPOS,JVPOMO))
            DO 12 IDIM=1,3
            DIST(IDIM)=DIST(IDIM)+RTABL(KVPOS,IVPOS,JVPOPO+IDIM-1)
   12       CONTINUE
            GO TO 13
         ENDIF
   10    CONTINUE
C
C  This statement should never be executed, except in case of an error
C
         CALL ALTELL ('VDGDIS : Could not find volumes',0,'STOP')
      ENDIF
  999 CONTINUE
      RETURN
      END
