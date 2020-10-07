      SUBROUTINE VINSTS(ISTRI,IVDHT,CHGE,IROL,NHITS,IRNW)
C----------------------------------------------------------------------
C! Insert truth relation between strip and monte carlo track
CKEY VDET DIGITIZE TRUTH
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  search if a relation already exists fro this track and this strip
C!            if yes, add the charge to this relation
C!            if no, add a new row into the relation bank, and
C!                      update the pointers for the chain link
C!                      of each relation to the next relation for the
C!                      same strip
C!
C! Input :   ISTRI - strip number                                     I
C!           IVDHT - monte carlo track index                          I
C!           CHGE  - charge deposited by this track on this strip     I
C!           IROL  - (previous) first relation for this strip         I
C!           NHITS - total number of hits in the wafer                I
C!           IRNW  - (new) first relation for this strip              O
C!
C! Output : VTSK bank (created or updated)
C!
C-----------------------------------------------------------------------
C
C  NSPH = average number of strips per hit
C  RTPS = average number of tracks which deposit energy in 1 strip
C         assume 20% strips with 2 tracks
C NADDRL = number of relations to add to the bank
C in case of too little space left
      PARAMETER (NSPH=12,RTPS=1.2,NADRL=10)
C
      SAVE NAVTSK
C
      PARAMETER(JVTSCH=1,JVTSHT=2,JVTSVT=3,LVTSKA=3)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVTSK = NAMIND('VTSK')
      ENDIF
C
C
C First, let us see if the relation bank is there. If not, create it.
C
      KVTSK = IW(NAVTSK)
      IF(KVTSK.EQ.0)THEN
C
C Compute max. approximate number of fired strips,
C 2 is because we have 2 views
C At this stage, we assume ntps tracks per strip on average
C
        NROW = NHITS*NSPH*2*RTPS
        NDATA = NROW*LVTSKA+LMHLEN
        CALL ALBOS('VTSK',0,NDATA,KVTSK,IGARB)
        IW(KVTSK+LMHROW)=0
        IW(KVTSK+LMHCOL)=LVTSKA
      ENDIF
C
C See if the relation already exists with this track
C
      IRST = IROL
 20   CONTINUE
      IF(IRST.EQ.0)GO TO 10
      IVD = ITABL(KVTSK,IRST,JVTSHT)
      IF(IVD.EQ.IVDHT)THEN
C
C Here, the relation exists, we add the charge and exit the routine
C
        CHO = RTABL(KVTSK,IRST,JVTSCH)
        RW(KROW(KVTSK,IRST)+JVTSCH) = CHO + CHGE
C
C The first acces to the chain is unchanged in this case
C
        IRNW = IROL
        GO TO 999
      ENDIF
      IRST = ITABL(KVTSK,IRST,JVTSVT)
      GO TO 20
 10   CONTINUE
C
C If the relation does not exist, we have to create it
C
      IF(LFRROW(KVTSK).LT.1)THEN
C If there is not enough space left in the bank, we extend it for
C NADRL more rows
        NDATA = IW(KVTSK) + NADRL*LVTSKA
        CALL ALBOS('VTSK',0,NDATA,KVTSK,IGARB)
      ENDIF
C
C Now, fill the last relation
C
      NREL=LROWS(KVTSK)+1
      IW(KVTSK+LMHROW)=NREL
      KADDR = KROW(KVTSK,NREL)
      RW(KADDR+JVTSCH) = CHGE
      IW(KADDR+JVTSHT) = IVDHT
C
C And update the link to next relation for same strip
C
      IW(KADDR+JVTSVT) = IROL
      IRNW = NREL
 999  CONTINUE
      RETURN
      END
