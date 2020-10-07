      SUBROUTINE TFITKF(IER)
C!
C!  Refit TGFT tracks using Kalman filter
C!  Remove hits flagged as outliers from coordinate list
C!
C!      Created: 23-OCT-1995    D.Casper
C!
#ifndef DOC
#include "tgftjj.h"
#include "tgcljj.h"
#include "tgtljj.h"
#include "tpcojj.h"
#include "bcs.h"
C
      LOGICAL FIRST/.TRUE./
#include "rconds.h"

      PARAMETER (MPT = 40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      DIMENSION TRACK(6), COV(21)
      INTEGER SORTARR(MPT)
      INTEGER LISTRM(MPT)
      INTEGER  UFTTRA, UFDALI, NAMIND
      EXTERNAL UFTTRA, UFDALI, NAMIND
#include "bmacro.h"
C
      IF (FIRST)THEN
        FIRST = .FALSE.
        NTGFT = NAMIND('TGFT')
        NTPCO = NAMIND('TPCO')
        NTGTL = NAMIND('TGTL')
        NTGCL = NAMIND('TGCL')
      ENDIF
      KTGFT = IW(NTGFT)
      KTPCO = IW(NTPCO)
      KTGTL = IW(NTGTL)
      KTGCL = IW(NTGCL)
      IF( KTGFT.LE.0 .OR. KTPCO.LE.0 .OR. KTGTL.LE.0
     &  .OR. KTGCL.LE.0)THEN
        IER = 1
        CALL RERROR('TFITKF',IER,'Missing input banks')
        RETURN
      ENDIF

      NTRK = LROWS(KTGFT)
      DO ITRK = 1, NTRK
C
C++   Fit TPC hits using Kalman filter
C
        NTPC = ITABL(KTGTL,ITRK,JTGTN1)
        IOTGCL = ITABL(KTGTL,ITRK,JTGTIO)
        ITGFT = KROW(KTGFT,ITRK)
        ICODE = UFTTRA(ITRK,FIELRC,RW(ITGFT+JTGFIR),
     +              RW(ITGFT+JTGFCD),NTPC,0,0,
     +              IW(KTGCL+LMHLEN+IOTGCL+1),IDUM,IDUM,
     +              TRACK,COV,CHI2,NDEG)
C
C++  Only overwrite TGFT and update hit banks if fit succeeded
C
        IF (CHI2 .LT. 1.E29 .AND. ICODE.EQ.0) THEN
            CALL UCOPY(TRACK,RW(ITGFT+JTGFIR),5)
            CALL UCOPY(COV,RW(ITGFT+JTGFEM),15)
            RW(ITGFT+JTGFCD) = CHI2
            IW(ITGFT+JTGFDF) = NDEG
C
C++   Get information which we could get much more easily from a common block
C
            IRET = UFDALI(NLOW,NHIGH,XSTS,XME,RF,IUSED,VMEAS)
C           CALL UFSORT(SORTARR)
C
C++   Disassociate filtered hits from track
C
            NRM = 0
            DO I = 1, NTPC
                IF(IUSED(I).EQ.0 .OR. IUSED(I).EQ.-1)THEN
                    CALL FDETCO('TG',SNGL(RF(I)),ITRK,ITPCO)
                    IF (ITPCO .NE. 0)THEN
                        NRM = NRM+1
                        LISTRM(NRM) = ITPCO
                    ELSE
                        IER = 2
                        CALL RERROR('TFITKF',IER,
     &                      'Coordinate not found on list')
                    ENDIF
                ENDIF
            ENDDO
            IF (NRM.GT.0) THEN
                CALL FRMHIT('TG',ITRK,NRM,LISTRM)
            ENDIF
        ELSE
            CALL RERROR('UFTKAL',ICODE,'Kalman Filter error')
        ENDIF
      ENDDO

      RETURN
      END

#endif
