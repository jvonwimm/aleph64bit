      SUBROUTINE RDETEC
C-----------------------------------------------------------------------
C!  - Set selected detectors
C!
C!  Author S. Orteu              Date :  13-JAN-1987
C!  Modified J.Knobloch          Date :  31-JUL-1989
C!  Modified M.Cattaneo          Date :  15-MAY-1996
C!    Switch off tracking upgrade refinements in PASS0,
C!    by forcing YNSV,YNLV,YNKF,TNPX,ENKF,VNSL
C!  Modified I.Tomalin           Date :  16-MAY-1996
C!    Switch on use of TPC wires in PASS0.
C!
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "bosext.h"
#include "rflags.h"
#include "rlunit.h"
      CHARACTER*4 TCHR,TOPT
#include "ittime.h"
#include "bmacro.h"
C
C detector names order !
C--------------------------------------------------------------------
C
      TNAMRD(JULVD) = 'VDET'
      TNAMRD(JULIT) = 'ITC '
      TNAMRD(JULTP) = 'TPC '
      TNAMRD(JULEC) = 'ECAL'
      TNAMRD(JULLC) = 'LCAL'
      TNAMRD(JULSA) = 'SATR'
      TNAMRD(JULHC) = 'HCAL'
      TNAMRD(JULMU) = 'MUON'
      TNAMRD(JULCA) = 'CALO'
      TNAMRD(JULSK) = 'SKEL'
      TNAMRD(JULYR) = 'YREC'
      TNAMRD(JULYT) = 'YTOP'
      TNAMRD(JULEF) = 'EFLO'
      TNAMRD(JULBC) = 'BCAL'
      TNAMRD(JULFA) = 'FALC'
      TNAMRD(JULBO) = 'BOM '
      TNAMRD(JULSI) = 'SICA'
C--------------------------------------------------------------------
C
C?   Debug Flag
C
      FDEBRF = .TRUE.
C
C decode the requested detectors for reconstruction.
C
      KPROC = NLINK('PROC',0)
      IF (KPROC.EQ.0) THEN
C No control card; take defaults
        DO 5 J=1,MDET
          FDETRF(J) = .TRUE.
          JDBDRF(J) = 0
          JHISRF(J) = 0
          JPRSRF(J) = 1
          JPRERF(J) = 1
    5   CONTINUE
        JDBDRF (JULSK) = 2
        FDETRF(JULFA) = .FALSE.
      ELSE
C Set according to PROC card
        DO 10 I=1,MDET
          FDETRF(I) = .FALSE.
          JDBDRF(I) = 0
          JHISRF(I) = 0
          JPRSRF(I) = 1
          JPRERF(I) = 1
   10   CONTINUE
        FDETRF (JULSK) = .TRUE.
        JDBDRF (JULSK) = 2
C Scan PROC bank
        NDETR = IW(KPROC)
        DO 15 IDETR=1,NDETR
          DO 20 IDET=1,MDET
            TCHR = CHAINT(IW(IDETR+KPROC))
            IF (TNAMRD(IDET)(:2).EQ.TCHR(:2)) THEN
              FDETRF(IDET) = .TRUE.
              GOTO 15
            ENDIF
   20     CONTINUE
C This detector doesn't exist.
          WRITE(LOUTRL,'(3A)') '0RDETEC- Unrecognized detector ',TCHR,
     $                           ' in PROC card.'
   15   CONTINUE
      ENDIF
C
C decode detector options
C
      DO 30 IDET = 1, MDET
        KNDTC = NLINK(TNAMRD(IDET),0)
        IF (KNDTC.NE.0.AND.FDETRF(IDET)) THEN
          FDETRF(IDET) = .TRUE.
          DO 35 IOPT = 1, IW (KNDTC), 2
            TOPT = CHAINT(IW(KNDTC+IOPT))
            IF     (TOPT.EQ.'PRIN') THEN
              JDBDRF(IDET) = IW(KNDTC+IOPT+1)
              GOTO 35
            ELSEIF (TOPT.EQ.'HIST') THEN
              JHISRF(IDET) = IW(KNDTC+IOPT+1)
              GOTO 35
            ELSEIF (TOPT.EQ.'SPRI') THEN
              JPRSRF(IDET) = IW(KNDTC+IOPT+1)
              GOTO 35
            ELSEIF (TOPT.EQ.'EPRI') THEN
              JPRERF(IDET) = IW(KNDTC+IOPT+1)
              GOTO 35
            ELSE
C Error
              WRITE(LOUTRL,'(3A)') '0RDETEC- Option ', TOPT,
     +                     ' doesn''t exist; option ignored'
            ENDIF
   35     CONTINUE
        ENDIF
        KNDTC = NDROP(TNAMRD(IDET),0)
   30 CONTINUE
C
C?   Special case for PASS0
C
      KPASS0 = IW ( NAMIND('PAS0') )
      FPASS0 = KPASS0.GT.0
      IF ( FPASS0 ) THEN
        IF ( KPROC.EQ.0 ) THEN
          DO 40  I =  1,  MDET
            FDETRF(I) = .FALSE.
   40     CONTINUE
          FDETRF(JULTP) = .TRUE.
          FDETRF(JULVD) = .TRUE.
        ENDIF
        FDETRF(JULSK) = .TRUE.
C Skip tracking upgrade refinements not necessary for PASS0
        CALL AUBOS ('YNSV', 0, 0, KYNSV, IYNSV)
        CALL AUBOS ('YNLV', 0, 0, KYNLV, IYNLV)
        CALL AUBOS ('YNKF', 0, 0, KYNKF, IYNKF)
        CALL AUBOS ('TNPX', 0, 0, KTNPX, ITNPX)
        CALL AUBOS ('ENKF', 0, 0, KENKF, IENKF)
        CALL AUBOS ('VNSL', 0, 0, KVNSL, IVNSL)
      ENDIF
C
C?   Special case for REPR -> No LCAL
C
      KREPR = IW(NAMIND('REPR'))
      FREPRO = KREPR.GT.0
      IF ( FREPRO ) THEN
        FDETRF(JULLC) = .FALSE.
      ENDIF
C
C?   Set maximum error print out
C
      IF ( IW(NAMIND('ERRP')).EQ.0 ) THEN
        CALL AUBOS ('ERRP', 0, 1, KERRP, IGARB)
        IW(KERRP+1) = 1
      ENDIF
C
C?   Set these options as default from now on
C
C         GAMP default use NGAM to switch off
C         VFIT             NVFI
C         EWTH             NEWT
C         ESCA             NESC
C         GARB             NGAR
C
      IF ( IW(NAMIND('NGAM')).EQ.0 ) THEN
        CALL AUBOS ('GAMP', 0, 0, KGAMP, IGAMP)
      ENDIF
      IF ( IW(NAMIND('NVFI')).EQ.0 ) THEN
        CALL AUBOS ('VFIT', 0, 0, KVFIT, IVFIT)
      ENDIF
      IF ( IW(NAMIND('NEWT')).EQ.0 ) THEN
        CALL AUBOS ('EWTH', 0, 0, KEWTH, IEWTH)
      ENDIF
      IF ( IW(NAMIND('NESC')).EQ.0 ) THEN
        CALL AUBOS ('ESCA', 0, 0, KESCA, IESCA)
      ENDIF
      IF ( IW(NAMIND('NGAR')).EQ.0 ) THEN
        CALL AUBOS ('GARB', 0, 0, KGARB, IGARB)
      ENDIF
C
C?   COMP 'INTE' is the default
C
      IF ( IW(NAMIND('COMP')).EQ.0 ) THEN
        CALL AUBOS ('COMP', 0, 1, KCOMP, IGARB)
        IW(KCOMP+1) = INTCHA ('INTE')
      ENDIF
C
C?   Set time measurement flags : For ITC, presence of ITTI
C
      IFITTI = IW(NAMIND('ITTI')) .NE. 0
C
C
C Set special option  CALO. CALO = ECAL.or.HCAL
      FDETRF(JULCA) = FDETRF(JULEC).OR.FDETRF(JULHC)
C--------------------------------------------------------------------
      IF (FDEBRF.AND.JDBDRF (JULSK).GE.1) WRITE(LOUTRL,500)
  500 FORMAT('0The detectors scheduled for reconstruction are :' /,
     +  1X,'Detector    Debug level    Histogram level' /,1X,
     +  '--------    ----------     --------------')
      DO 70 IDET=1,MDET
        IF (FDETRF(IDET)) WRITE(LOUTRL,'(3X,A,8X,I3,15X,I3)')
     1                     TNAMRD(IDET),JDBDRF(IDET),JHISRF(IDET)
   70 CONTINUE
      WRITE(LOUTRL,520)
  520 FORMAT(1X,80('+'))
      GOTO 999
  999 CONTINUE
      END
#endif
