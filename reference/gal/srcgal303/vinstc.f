      SUBROUTINE VINSTC(IROC,ISTRI,CC,IMOD,IV,NHITT,IROLD,IRNEW)
C----------------------------------------------------------------------
C! Insert truth relation between R/O channel and monte carlo track
CKEY VDET DIGITIZE TRUTH
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  For each track which deposited charge in this given strip :
C!  find the charge;
C!  search if a relation already exists for this channel and this track
C!            if yes, add the charge to this relation
C!            if no, add a new row into the relation bank, and
C!                      update the pointers for the chain link
C!                      of each relation to the next relation for the
C!                      same channel
C!
C! Input :   IROC  -  readout channel number                          I
C!           ISTRI - strip number                                     I
C!           IMOD  - module number                                    I
C!           IV   - view                                              I
C!           NHITT - total number of hits in the event                I
C!           IROLD - (previous) first relation for this channel       I
C! Output:   IRNEW - (new) first relation for this channel            O
C!
C! Intput : VTSK bank
C!
C! Output : VTRS bank (created or updated)
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
      SAVE NAVTSK,NAVTRS,NAVWSX
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION NAVWSX(2)
      PARAMETER(JVWSSC=1,JVWSVT=2,LVWS1A=2)
      PARAMETER(JVTSCH=1,JVTSHT=2,JVTSVT=3,LVTSKA=3)
      PARAMETER(JVTRAD=1,JVTRCH=2,JVTRHT=3,JVTRVT=4,LVTRSA=4)
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
         NAVTRS = NAMIND('VTRS')
         NAVWSX(1) = NAMIND('VWS1')
         NAVWSX(2) = NAMIND('VWS2')
C
      ENDIF
      CALL VFNDEL
     $    (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $     MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
C
C
C First, let us see if the strip-to-track relation bank is there.
C If not, exit
C
      KVTSK = IW(NAVTSK)
      IF(KVTSK.EQ.0)GO TO 999
C
C Now, let us see if the readout channels bank exists. If not, create it
C
      KVTRS = IW(NAVTRS)
      IF(KVTRS.EQ.0)THEN
C Compute max. approximate number of fired strips,
C 2 is because we have 2 views
C At this stage, we assume ntps tracks per strip on average
C
        NROW = NHITT*NSPH*2*RTPS
        NDATA = NROW*LVTRSA+LMHLEN
        CALL ALBOS('VTRS',0,NDATA,KVTRS,IGARB)
        IW(KVTRS+LMHROW)=0
        IW(KVTRS+LMHCOL)=LVTRSA
      ENDIF
C
      KVTSK = IW(NAVTSK)
      KVWSX = IW(NAVWSX(IV))
      IRST = ITABL(KVWSX,ISTRI,JVWSVT)
 20   CONTINUE
C
C Here, we explore all the relations to tracks for this one strip,
C and we update the channels to tracks accordingly
C
      IF(IRST.EQ.0)GO TO 10
      CHGE = RTABL(KVTSK,IRST,JVTSCH)*CC
      PULSH = NBITSH(IV)*CHGE/(VELGV(IV)*VDLCO(IV))
      IVDHT = ITABL(KVTSK,IRST,JVTSHT)

C
C See if the relation already exists with this track
C
      IRCT = IROLD
 30   CONTINUE
      IF(IRCT.EQ.0)GO TO 40
      IVD = ITABL(KVTRS,IRCT,JVTRHT)
      IF(IVD.EQ.IVDHT)THEN
C
C Here, the relation exists, we add the charge and exit the loop
C
        PULSO = RTABL(KVTRS,IRCT,JVTRCH)
        RW(KROW(KVTRS,IRCT)+JVTRCH) = PULSO + PULSH
        IRNEW = IROLD
        GO TO 11
      ENDIF
      IRCT = ITABL(KVTRS,IRCT,JVTRVT)
      GO TO 30
 40   CONTINUE
C
C If the relation does not exist, we have to create it
C
      IF(LFRROW(KVTRS).LT.1)THEN
C
C If there is not enough space left in the bank, we extend it for
C NADDRL more rows
C
        NDATA = IW(KVTRS) + NADRL*LVTRSA
        CALL ALBOS('VTRS',0,NDATA,KVTRS,IGARB)
      ENDIF
C
C
C Now, fill the last relation
C
      NREL=LROWS(KVTRS)+1
      IW(KVTRS+LMHROW)=NREL
      KADDR = KROW(KVTRS,NREL)
      RW(KADDR+JVTRCH) = PULSH
      IW(KADDR+JVTRHT) = IVDHT
C
C Make strip address and fill it in bank
C
      CALL VAENCL(IWRD,IMOD,IV,IROC,0,IB1,IB2)
      IW(KADDR+JVTRAD) = IWRD
C
C And update the link to next relation for same strip
C
      IW(KADDR+JVTRVT) = IROLD
      IRNEW = NREL
      IROLD = NREL
 11   IRST = ITABL(KVTSK,IRST,JVTSVT)
      GO TO 20
 10   CONTINUE
 999  CONTINUE
      RETURN
      END
