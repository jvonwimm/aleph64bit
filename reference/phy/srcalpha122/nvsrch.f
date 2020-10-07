      SUBROUTINE NVSRCH(BPOS,BSIZ,PVTX,EPVTX,DJET,BTAG,SVTX,ESVTX,AVTX,
     &  CAVTX,JERR)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : auxiliary routine to NVSRCH. Gets primary and
C!                  secondary vertex coordinates from bank NEHE
C!
C!   Author   :- Gerrit Graefe         25-MAR-1995
C!
C!   Inputs:
C!        - none            all input arguments ar dummy
C!
C!
C!   Outputs:
C!        - PVTX(3)         primary vertex in ALEPH coordinates
C!        - BTAG(2)         b-tagging variable (see description in QVSRC
C!        - AVTX(3,2)       secondary vertices in ALEPH coordinates
C!        - CAVTX(1,1,x)    the first components (CAVTX(1,1,1)andCAVTX(1
C!                          contain the error on the decay length. NOTE
C!                          that this is different to input from MINI or
C!                          DST.
C!        - JERR            Error occured while running QVSRCH
C!                          JERR =  0  error code from QVSRCH .EQ.  0
C!                          JERR =  1  error code from QVSRCH .lt. 12
C!                          JERR = 12  error code from QVSRCH .ge. 12
C!
C!   Libraries required: BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine takes the precalculated information from the NEHE
C?   bank and returns it to the user. The calculation is done during the
C?   NanoDST production step, for that all input arguments are dummy.
C?
C!======================================================================
      IMPLICIT NONE
      SAVE NERR
      REAL      BPOS,BSIZ,PVTX,EPVTX,DJET,BTAG,SVTX,ESVTX,AVTX,CAVTX
      DIMENSION BPOS(3),BSIZ(3)
      DIMENSION PVTX(3),EPVTX(3),DJET(3,2)
      DIMENSION BTAG(2),SVTX(3,2),ESVTX(3,2)
      DIMENSION AVTX(3,2),CAVTX(3,3,2)
      LOGICAL BTEST
      INTEGER NVERS,NAMIND,NERR
      INTEGER INEHE,KCNEHE,INDX,KRNEHE,KONEHE,JERR
      DATA    NERR/0/
      INTEGER JNEHKR,JNEHKE,JNEHTR,JNEHTP,JNEHPF,JNEHE1,JNEHE2,JNEHDS,
     &  JNEHP2,JNEHVX,JNEHVY,JNEHVZ,JNEHX1,JNEHY1,JNEHZ1,JNEHX2,JNEHY2,
     &  JNEHZ2,JNEHB1,JNEHB2,LNEHEA
      PARAMETER(JNEHKR=1,JNEHKE=2,JNEHTR=3,JNEHTP=4,JNEHPF=5,JNEHE1=6,
     +          JNEHE2=7,JNEHDS=8,JNEHP2=9,JNEHVX=10,JNEHVY=11,
     +          JNEHVZ=12,JNEHX1=13,JNEHY1=14,JNEHZ1=15,JNEHX2=16,
     +          JNEHY2=17,JNEHZ2=18,JNEHB1=19,JNEHB2=20,LNEHEA=20)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
C----------------------------------------------------------------------
C
C!..CHECK NANODST VERSION
C
      CALL GETNAV(NVERS)
      IF(NVERS.LT.116.AND.NERR.LT.10)THEN
        NERR=NERR+1
        CALL QWMESE
     &    ('+++ NVSRCH +++ No QVSRCH information for NANO<116 !')
        GOTO 999
      ENDIF
C
C!..LINK TO BANK NEHE
C
      INEHE=IW(NAMIND('NEHE'))
      IF(INEHE.EQ.0)THEN
        RETURN
      ENDIF
      KONEHE=INEHE
      KCNEHE=IW(KONEHE+1)
      KRNEHE=IW(KONEHE+2)
C
C!..ZERO ALL MATRICES
C
      CALL VZERO(BTAG,2)
      CALL VZERO(BSIZ,3)
      CALL VZERO(BPOS,3)
      CALL VZERO(PVTX,3)
      CALL VZERO(EPVTX,3)
      CALL VZERO(DJET,6)
      CALL VZERO(PVTX,3)
      CALL VZERO(SVTX,6)
      CALL VZERO(ESVTX,6)
      CALL VZERO(AVTX,6)
      CALL VZERO(CAVTX,18)
C
C!..NOW FILL MATRICES
C
      INDX=INEHE+LMHLEN
C..ERROR CODE
      JERR=0
      IF(BTEST(IW(INDX+JNEHPF),30))JERR=1
      IF(BTEST(IW(INDX+JNEHPF),31))JERR=12
C..BTAG
      BTAG(1)=FLOAT(IW(INDX+JNEHB1))/10000.0
      BTAG(2)=FLOAT(IW(INDX+JNEHB2))/10000.0
C..SECONDARY VERTICES
      AVTX(1,1)=FLOAT(IW(INDX+JNEHX1))/10000.0
      AVTX(2,1)=FLOAT(IW(INDX+JNEHY1))/10000.0
      AVTX(3,1)=FLOAT(IW(INDX+JNEHZ1))/10000.0
      AVTX(1,2)=FLOAT(IW(INDX+JNEHX2))/10000.0
      AVTX(2,2)=FLOAT(IW(INDX+JNEHY2))/10000.0
      AVTX(3,2)=FLOAT(IW(INDX+JNEHZ2))/10000.0
C..PRIMARY VERTEX
      PVTX(1)=FLOAT(IW(INDX+JNEHVX))/10000.0
      PVTX(2)=FLOAT(IW(INDX+JNEHVY))/10000.0
      PVTX(3)=FLOAT(IW(INDX+JNEHVZ))/10000.0
C..ERROR ON DECAY LENGTH
      CAVTX(1,1,1)=FLOAT(IW(INDX+JNEHE1))/10000.0
      CAVTX(1,1,2)=FLOAT(IW(INDX+JNEHE1))/10000.0
  999 RETURN
      END
