      SUBROUTINE BTAG_FIT(FILE)
CKEY   QIPBTAG / USER
C ----------------------------------------------------------------------
C! Fit the negative dmin/sig spectrum to determine the track resolution,
C  pack the parameters into the appropriate banks, and write them out.
C  May be called by the user at the end of his job
C  Author  Dave Brown  29-1-93
C
C     Input;  FILE   File name of the card file to be created.  If
C                    this is blank or equal to "NONE", the card file
C                    will appear on unit 32
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
C
C  Track probabilty function koeffizients
C
      INTEGER         NFIT,MAXTYPE,NTYPE,INTYPE
      PARAMETER      (NFIT=5,MAXTYPE=10)
      INTEGER NVIEW(MAXTYPE)
      REAL            FITP(NFIT,MAXTYPE)
      LOGICAL CALIB,PARS
      CHARACTER*80 CALFILE
      CHARACTER*8 USRNAME(MAXTYPE)
      COMMON/FITPAR/FITP,CALIB,PARS,CALFILE,NTYPE,INTYPE,USRNAME,NVIEW
C
C  Track cuts
C
      INTEGER MINN(3),IDB,NUMJET
      REAL D0_CUT,Z0_CUT,MINMOM,MAXMOM
      REAL MAX_ERR,CHI_CUT,DOTCUT
      REAL LCUT(MAXTYPE),LSIGCUT(MAXTYPE),SCUT(MAXTYPE)
      REAL MXDMIN(MAXTYPE),MXSDMIN(MAXTYPE)
      COMMON/TRCUT/D0_CUT,Z0_CUT,MINMOM,MAXMOM,
     & MAX_ERR,CHI_CUT,MINN,IDB,
     & MXDMIN,MXSDMIN,NUMJET,DOTCUT,LCUT,LSIGCUT,SCUT
C
C  Jet momentum cut
C
      REAL PCUT
      COMMON / JTCUT / PCUT
C
C  Generic cuts
C
      LOGICAL NEGPROB
      COMMON/BTGEN/NEGPROB
      INTEGER IND,IPAR
      INTEGER ILOW,IHIGH
      INTEGER ITYPE
      CHARACTER*(*) FILE
      REAL ERR(NFIT),CHI
      INTEGER NBANK
      REAL FITPAR(NFIT,MAXTYPE)
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C ----------------------------------------------------------------------
C  Copy the existing parameters as initial values
C
      DO ITYPE=1,NTYPE
        DO IPAR=1,NFIT
          FITPAR(IPAR,ITYPE) = FITP(IPAR,ITYPE)
        END DO
      END DO
C
C     Limit the fit range, so as to model the tail better
C
      WRITE (IW(6),*)'BTAG_FIT: Calibration fit results'
      DO ITYPE=1,NTYPE
        CALL FIT_DMIN(ITYPE,NVIEW(ITYPE),FITPAR(1,ITYPE),ERR,CHI)
        WRITE (IW(6),*)'chisq/dof = ',CHI,' For tracks OF type ',ITYPE
      END DO
C
C  Save the values in a card file
C
      IND = NBANK('FITP',1,2+NTYPE*NFIT)
      IF(IND .LE. 0 )THEN
        WRITE (IW(6),*)'BTAG_FIT: Not enough space to create FITP bank'
        RETURN
      END IF
      IW(IND+1) = NFIT
      IW(IND+2) = NTYPE
      DO ITYPE=1,NTYPE
        DO IPAR=1,NFIT
          RW(KROW(IND,ITYPE)+IPAR) = FITPAR(IPAR,ITYPE)
        END DO
      END DO
      CALL BKFMT('FITP','2I,(5F)')
C
C  Write out the bank as a cards
C
      IF(FILE(1:4) .NE. 'NONE' .AND. FILE(1:4) .NE. 'none')THEN
        WRITE (IW(6),*)' Saving BTAG calibration on file ',FILE
        OPEN(UNIT=32,STATUS='UNKNOWN',FORM='FORMATTED',FILE=FILE)
      ELSE
        WRITE (IW(6),*)' Saving BTAG calibration on unit 32'
      END IF
      CALL BUNIT(32,'TEXT',72)
      CALL BLIST(IW,'E=','FITP')
      CALL BWRITE(IW,32,'E')
      CALL BWRITE(IW,32,'0')
      CLOSE(32)
      RETURN
      END
