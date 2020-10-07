      SUBROUTINE X2LUTS (IRET)
C
C -----------------------------------------------
C! Level 2 trigger Look Up Table calculations
C
C   Author : T.Medcalf    10/9/87
C
C? Creates look up tables used by the electronics
C? of the level2 trigger, for converting time to
C? theta-bin, etc.
C
C - Output:   IRET / I   = return flag
C                        0 means OK
C                        2 means FATAL error (due to default bin
C                                             values)
C ----------------------------------------------------------------
C
C  BOS bank X2TB - Theta bin table for use by level 3.
C  1  :  words / theta bin ( = 16 )
C  2  :  # of theta bins
C  WORD(I)    :  # of first time bin in the Ith theta bin, on row 1
C  WORD(I+1)  :  # of last time bin in the Ith theta bin, on row 1
C  WORD(I+2)  :  # of first time bin in the Ith theta bin, on row 2
C  WORD(I+3)  :  # of last time bin in the Ith theta bin, on row 2
C  .............
C  WORD(I+15) :  # of last time bin in the Ith theta bin, on row 8
C -----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "x2cons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "x2tbjj.h"
C
C    Radian measures of the theta-angle of the first and last
C   trigger pad rows. ( Respectively 26. and 39. degrees )
C
      PARAMETER ( THFIRS = 0.453785, THLAST = 0.680679 )
C
C    Number of data-words/row in the theta-bin bank is
C  2 per pad-row = start and finish time-bin numbers.
C    NPADS is number of pads in 1 row in a Phi-sector of 30 degrees.
C   Calculation uses NPEF1 as the lowest padrow for which a hit can
C  lead to enough points to form a allowable track.
C
      PARAMETER ( NPEF1 = 4, NPADS = 4 )
      PARAMETER ( NDSOS = 3, NWRTB = 2 * NPUSE )
C
C   The largest bin-number is used as code for an incomplete bin.
C   There are NSEGS segments in the theta-range, for the mask.
C
      PARAMETER ( NBMXP = NBMAX + 1, NSEGS = 5 )
C
C
      DIMENSION BST(NBMAX,NPUSE),IPADT(IOS,NPADS),ITHTP(NPUSE)
      DIMENSION SEGTH(NSEGS)
C   Macro for BOS-bank pointer
      ITPTR(IO,IT,IR) = IO + ( IT - 1 ) * LX2TBA + 2 * IR - 2
C ----------------------------------------------------------------
C
C Calculate pad conversion tables using ignored padrows
C
C Flag ignored padrows as -1 in IPDCON
C
      DO 1003 IP = 1, NIGN
        IPDCON(IGNPAD(IP)-NPUSE) = -1
 1003 CONTINUE
C
C Now place numbers 1 to NPUSE into sequence in remaining
C places in IPDCON
C
      IPP = 1
      DO 1013 IP = 1,NPUSE
 1020   IF (IPDCON(IPP).EQ.-1) THEN
          IPP = IPP+1
          GOTO 1020
        ELSE
          IPDCON(IPP) = IP
          IPP = IPP+1
        END IF
 1013 CONTINUE
C
C Put radii of surviving padrows into RADPAD(NPUSE,IOS)
C where the second index refers to inner/outer sector
C
C Inner padrows correspond - i.e. no pads ignored.
C
      DO 1030 IP = 1,NPUSE
        RADPAD(IP,1) = TPTROW(IP)
 1030 CONTINUE
C
C Now only put non-ignored padrow radii into RADPAD(IP,IOS)
C
      DO 1040 IP = 1,NPEXI
        IF (IPDCON(IP).NE.-1) RADPAD(IPDCON(IP),IOS) = TPTROW(IP+NPUSE)
 1040 CONTINUE
C
C
C Pad radii are in RADPAD(row,iosec)
C Number of th bins is in NTBINS(iosec)
C Z-acc in mm is in ZACPMM
C Clock rate is in CLOCKR
C Working drift velocity is ADVELO
C
C Theta bin patterns are put into
C   ITHETA(iosec,timebin,row)     Theta bin number
C   ITHSUB(iosec,timebin,row)     Sub-theta
C   ITHOVR(iosec,timebin,row)     1 = overlap
C   ITHOSB(iosec,timebin,row)     Overlap sub-theta
C
C (101 is restart point on error)
  101 CONTINUE
C
C --- set overlap flags to zero
C
      CALL VZERO (ITHOVR(1,0,1),2*257*8)
C
C   Convert Z-acc in mm to Z-bins
      ZACM = (ZACPMM*CLOCKR/ADVELO)
C   Calc Max timebin
      ITBMX = INT((ZTPCMX*CLOCKR*10.)/ADVELO)
C
C   * * * *   L o o p   o v e r  i n n e r / o u t e r   s e c t o r s
C
      DO 500 IOSEC = 1,IOS
C
C ---      First calculate non-overlapped theta bins
C
        DO 1000 IPROW = 1,NPUSE
          ZBNS = (REAL(ITBMX)*RADPAD(IPROW,IOSEC))
     +       /(REAL(NTBINS(IOSEC))*RADPAD(NPEF1,IOSEC))
          DO 1010 ITH = 1,NTBINS(IOSEC)
            BST(ITH,IPROW) = REAL(ITH-1)*ZBNS
 1010     CONTINUE
          LSBIN = 0
          ISUBN = 0
C
          DO 2000 IZ = 0,ITBMX
            ITHZ = INT(REAL(IZ)/ZBNS)+1
            IF (ITHZ.GT.NTBINS(IOSEC)) ITHZ = 0
            IF (ITHZ.NE.LSBIN) THEN
              ISUBN = 0
            END IF
            ISUBN = ISUBN+1
            IF (ITHZ.EQ.0) ISUBN = 0
            ITHETA(IOSEC,ITBMX-IZ,IPROW) = ITHZ
            ITHSUB(IOSEC,ITBMX-IZ,IPROW) = ISUBN
            LSBIN = ITHZ
 2000     CONTINUE
C
C        If next bin would start outside TPC, then not all of last
C        bin was inside. so set its number to NBMXP.
          ITHN = INT(REAL(ITBMX+1)/ZBNS)+1
          IF (ITHN.EQ.ITHETA(IOSEC,0,IPROW)) THEN
            DO 2500 IT = 0,ITBMX
              IF (ITHETA(IOSEC,IT,IPROW).NE.ITHN) GOTO 2600
              ITHETA(IOSEC,IT,IPROW) = NBMXP
              ITHSUB(IOSEC,IT,IPROW) = 0
 2500       CONTINUE
          END IF
 2600     CONTINUE
C
 1000   CONTINUE
C ---
C
C       Use non-overlap theta bin data to calculate conversion
C       from theta bin to theta segment in the mask
C
C       Table is MASCON(theta,2,iosec) giving up to two segment numbers
C       for bin theta, in iosec.
C
C       Array SEGTH holds theta segment boundaries for the mask.
        SEGTH(1) = PIBY2
        SEGTH(2) = PIBY3
        SEGTH(3) = THLAST
        SEGTH(4) = THFIRS
        SEGTH(5) = PIBY12
C
        ISCN = 0
C ++++
        DO 7000 ITH = 1,NTBINS(IOSEC)
          ITST = ISCN
 7010     ISCN = ISCN+1
          IF(ITHETA(IOSEC,ITBMX-ISCN,NPEF1).EQ.ITH) GOTO 7010
          ITFN = ISCN-1
C        THST and THFN are start and finish angle of theta bin.
          THST = ATAN( RADPAD(NPEF1,IOSEC) / (REAL(ITST)+0.5) )
          THFN = ATAN( RADPAD(NPEF1,IOSEC) / (REAL(ITFN)+0.5) )
C        Now find which segment they are in.
          ISGST = NPEF1
 7015     IF(THST.GE.SEGTH(ISGST)) THEN
            ISGST = ISGST-1
            GOTO 7015
          END IF
          ISGFN = NPEF1
 7025     IF(THFN.GE.SEGTH(ISGFN)) THEN
            ISGFN = ISGFN-1
            GOTO 7025
          END IF
          MASCON(ITH,1,IOSEC) = ISGST
          MASCON(ITH,2,IOSEC) = ISGFN
 7000   CONTINUE
C ++++
C      Now calculate overlaps.
C      If the bin/overlap process fails, the flag IERBN is set
C      to a value which identifies the reason for failure.
C
        IERBN = 0
C ====
        DO 3000 ITH = NTBINS(IOSEC)-1,1,-1
          DO 3500 IPROW = 1,NPUSE
            IF (BST(ITH,IPROW).GT.ITBMX) GOTO 3500
C           calculate overlap from upper and lower pts. take largest.
            OVRA = (RADPAD(IPROW,IOSEC)*(BST(ITH+1,NPUSE)-ZACM))
     +         /RADPAD(NPUSE,IOSEC)+ZACM
            OVRB = (RADPAD(IPROW,IOSEC)*(BST(ITH+1,1)+ZACM))
     +         /RADPAD(1,IOSEC)-ZACM
            IOVR = INT(MAX(OVRA,OVRB))
            IF (IOVR.GT.ITBMX) THEN
              DO 3600 IZ = ITBMX,0,-1
                JTH = ITHETA(IOSEC,ITBMX-IZ,IPROW)
                IF (JTH.LT.ITH) GOTO 3610
                ITHETA(IOSEC,ITBMX-IZ,IPROW) = NBMXP
                ITHSUB(IOSEC,ITBMX-IZ,IPROW) = 0
                ITHOVR(IOSEC,ITBMX-IZ,IPROW) = 0
                ITHOSB(IOSEC,ITBMX-IZ,IPROW) = 0
 3600         CONTINUE
 3610         CONTINUE
            ELSE
C            overlap ends inside TPC
              JTH = ITHETA(IOSEC,ITBMX-IOVR,IPROW)
              IF (JTH.EQ.ITH+1) THEN
C              overlap is into adjacent bin
                DO 3700 IZ = IOVR,0,-1
                  IF (ITHETA(IOSEC,ITBMX-IZ,IPROW).EQ.ITH) GOTO 3710
 3700           CONTINUE
 3710           CONTINUE
                ISUBT = ITHSUB(IOSEC,ITBMX-IZ,IPROW)
                DO 3800 IZO = 1,IOVR-IZ
                  ITHOSB(IOSEC,ITBMX-(IZ+IZO),IPROW) =
     +                   ITHSUB(IOSEC,ITBMX-(IZ+IZO),IPROW)
                  ITHSUB(IOSEC,ITBMX-(IZ+IZO),IPROW) = ISUBT+IZO
                  ITHETA(IOSEC,ITBMX-(IZ+IZO),IPROW) = ITH
                  ITHOVR(IOSEC,ITBMX-(IZ+IZO),IPROW) = 1
 3800           CONTINUE
              ELSE IF (JTH.EQ.NBMXP) THEN
C              overlap is into end piece of a bin
                DO 3900 IZ = IOVR,0,-1
                  KTH = ITHETA(IOSEC,ITBMX-IZ,IPROW)
                  IF (KTH.EQ.ITH) GOTO 3910
                  IF (KTH.EQ.NBMXP) GOTO 3920
                  IERBN = 1
                  GOTO 5999
 3920             ITHOVR(IOSEC,ITBMX-IZ,IPROW) = 0
                  ITHOSB(IOSEC,ITBMX-IZ,IPROW) = 0
 3900           CONTINUE
 3910           CONTINUE
              ELSE IF ((JTH.EQ.0).OR.(JTH.GT.(ITH+1))) THEN
C              overlap is into bin zero space or more than 1 bin
                IERBN = 1
                GOTO 5999
              ELSE IF(ITH.EQ.JTH) THEN
              ELSE
                IERBN = 2 + 10 * ITH + 1000 * JTH
                GOTO 5999
              END IF
            END IF
 3500     CONTINUE
 3000   CONTINUE
C ====
C
C      Now check that padrow NPEF1 does not exceed ILM time bins in
C      any theta bin.
C
        DO 4000 ITH = 0,ITBMX
          IF(ITHSUB(IOSEC,ITH,NPEF1).GT.ILM) THEN
            IERBN = 3 + 10 * ITH
            GOTO 5999
          END IF
 4000   CONTINUE
C
C      now calculate the max number of pads in each th-bin
C      and store in IHTMAX(IOSEC,THBIN)
C
        DO 5000 IT = 0,ITBMX
          IBNLS = ITHETA(IOSEC,IT,NPUSE)
          IF((IBNLS.NE.NBMXP).AND.(IBNLS.NE.0)) GOTO 5010
 5000   CONTINUE
 5010   CONTINUE
C
        DO 5100 ITBIN = 1,IBNLS
          IHTMAX(IOSEC,ITBIN) = NPUSE
 5100   CONTINUE
C
        DO 5200 IROW = NPUSE-1,NPEF1,-1
          DO 5300 IT = 0,ITBMX
            IBNNX = ITHETA(IOSEC,IT,IROW)
            IF((IBNNX.NE.0).AND.(IBNNX.NE.NBMXP)) THEN
              IF (IBNNX.EQ.IBNLS) GOTO 5200
              DO 5400 IIT = IBNLS+1,IBNNX
                IHTMAX(IOSEC,IIT) = IROW
 5400         CONTINUE
              IBNLS = IBNNX
              GOTO 5200
            END IF
 5300     CONTINUE
 5200   CONTINUE
C
  500 CONTINUE
C +++++++++++++++++
      GOTO 6000
C
C =============
C  Default values produced error
C  The type of error and associated bin-values are decoded from IERBN.
C
 5999 JTH = MOD( IERBN, 1000 )
      IERBN = IERBN - 1000 * JTH
      ITH = MOD( IERBN, 10 )
      IERBN = IERBN - 10 * ITH
      IF( IERBN .EQ. 1 ) THEN
        WRITE(IW(6),4010)
      ELSEIF( IERBN .EQ. 2 ) THEN
        WRITE(IW(6),4030) ITH,JTH
      ELSEIF( IERBN .EQ. 3 ) THEN
        WRITE(IW(6),4040) ITH
      ENDIF
C
 4010 FORMAT( ' X2LUTS - Overlap exceeds 1 bin' )
 4020 FORMAT( ' ZERO OVERLAP FOR BIN ', I4 )
 4030 FORMAT( ' Anomalous overlap.',/, ' From bin ',I4, ' to ', I4 )
 4040 FORMAT( ' Bin 4 exceeds 15 sub theta bins in pad', I4 )
C
      CALL ALTELL(' X2LUTS: Default bin values gave error - STOP',
     +  0,'RETURN')
C     IRET = 2
      RETURN
C ===========
C
 6000 CONTINUE
C
C    Now calculate the theta bin bos bank for level 3
C
C    The bank contains rows of LX2TBA words, namely start and finish
C    time bin numbers for each of NPUSE rows. Zero means that the
C    theta bin does not exist in this row (for low theta theta-bins)
C    Bank length will be (NTBINS(1)+NTBINS(2))*LX2TBA + LMHLEN
C
C    first allocate bank
      ITBLN = LMHLEN+(NTBINS(1)+NTBINS(2))*LX2TBA
      CALL AUBOS('X2TB',0,ITBLN,IX2TB,IGARB)
      IF (IX2TB.EQ.0) GOTO 900
      IW(IX2TB+LMHCOL) = LX2TBA
      IW(IX2TB+LMHROW) = NTBINS(1)+NTBINS(2)
C
C ...Now enter the start and end time bin for each bin
C    in each row in each of inner/outer sector.
      DO 8000 IOSEC = 1,IOS
        IOFF = IX2TB+3
        IF(IOSEC.EQ.2) IOFF = IX2TB+NIGN+LX2TBA*NTBINS(1)
        DO 8010 IROW = 1,NPUSE
          ICBIN = 1
          DO 8020 ITIME = ITBMX,0,-1
            ITHB = ITHETA(IOSEC,ITIME,IROW)
            IF (ITHSUB(IOSEC,ITIME,IROW).EQ.1) THEN
              IW( ITPTR(IOFF,ITHB,IROW) ) = ITIME
            END IF
            IF (ITHOSB(IOSEC,ITIME,IROW).EQ.1) THEN
              IW( ITPTR(IOFF,ITHB+1,IROW) ) = ITIME
            END IF
            IF (ITHB.NE.ICBIN) THEN
              IW( ITPTR(IOFF+1,ICBIN,IROW) ) = ITIME+1
              ICBIN = ITHB
              IF((ICBIN.EQ.NBMXP).OR.(ICBIN.EQ.0)) THEN
                ISABS = ITHETA(IOSEC,ITIME+1,IROW)+1
                IF(ITHOVR(IOSEC,ITIME+1,IROW).EQ.1) THEN
                  ISABS = ISABS+1
                  ITEM = ITHETA(IOSEC,ITIME+1,IROW)+1
                  IW( ITPTR(IOFF+1,ITEM,IROW) ) = ITIME+1
                END IF
                GOTO 8099
              END IF
            END IF
 8020     CONTINUE
          ISABS = ICBIN+1
          IW( ITPTR(IOFF+1,ICBIN,IROW) ) = 0
 8099     CONTINUE
          DO 8025 IBIN = ISABS,NTBINS(IOSEC)
            IW( ITPTR(IOFF,IBIN,IROW) ) = - 1
            IW( ITPTR(IOFF+1,IBIN,IROW) ) = - 1
 8025     CONTINUE
 8010   CONTINUE

 8000 CONTINUE
C ...
C
C    Calculate look ups for theta bin and z-road analysis.
C
C    Thresholds and pad pairs are in array ITHRSH and IPADPR
C    in common X2CONS
C
C    Now use road width to calc look up that gives
C    the vote (1 or 0) for comparing measured with
C    calculated sub-theta bins
C
      DO 7030 IM = 1,NVMAX
        DO 7040 IC = 0,NVMAX
          IF (ABS(IM-IC).LE.IRWDTH) ITVOTE(IM,IC) = 1
 7040   CONTINUE
 7030 CONTINUE
C
C    Now for the table to give calculated straight line from
C    two sub-thetas for each of the pad pair choices for both
C    inner and outer sectors.
C
C ++++
      DO 7050 IOSEC = 1,IOS
        DO 7060 IPCHX = 1,ICHX
          RU = RADPAD(IPADPR(2,IPCHX),IOSEC)
          RL = RADPAD(IPADPR(1,IPCHX),IOSEC)
          DO 7070 IL = 1,ILM
            DO 7080 IU = 1,IUM
              SU = REAL(IU)+0.5
              SL = REAL(IL)+0.5
              Z = (((SU*RL)/RU)-SL)/((RL/RU)-1.)
              IZB = INT(Z)
              IF (ABS(IZB).GT.IZBMAX) THEN
                DO 7090 IROW = 1,NPUSE
                  IZRDLK(IL,IU,IROW,IPCHX,IOSEC) = IUM
 7090           CONTINUE
                IZRDLK(IL,IU,ISMX,IPCHX,IOSEC) = NBMXP
              ELSE
                DO 7100 IROW = 1,NPUSE
                  SIROW = ((RADPAD(IROW,IOSEC)-RL)/(RU-RL))
     +                   *(SU-SL)+SL
                  IZRDLK(IL,IU,IROW,IPCHX,IOSEC) = INT(SIROW)
 7100           CONTINUE
                IZRDLK(IL,IU,ISMX,IPCHX,IOSEC) = IZB
              END IF
 7080       CONTINUE
 7070     CONTINUE
 7060   CONTINUE
 7050 CONTINUE
C ++++
      IRET = 0
      RETURN
C
C - not enough space for BOS bank
C
 900  CONTINUE
      CALL ALTELL ('X2LUTS: not enough space for X2TB - STOP',
     &             0,'RETURN')
      IRET = 2
C
      END
#endif
