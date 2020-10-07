      SUBROUTINE CREFOL(KEEP)
C--------------------------------------------------------------------
C! Create and fill the EFOL bank for the mini
C
C  Patrick Janot -- 14 Jun 1992
C--------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
#include "intval.h"
#include "eauxjj.h"
      INTEGER ALTRIG
      LOGICAL first, Xhvok
      DATA first/.TRUE./
#include "qcde.h"
#include "qmacro.h"
C--------------------------------------------------------------------
      DO ipartk = 1 , 7
        nfwvc = IW(ifwvc(ipartk) + lmhlen)
        IFWVC(IPARTK) = NBANK ('FWVC', ipartk, lmhlen + kfwvc*nfwvc)
        IF ( IFWVC(IPARTK) .EQ. 0 ) THEN
          WRITE (IW(6),*)
     .    ' **** CREFOL **** Unable to create temporary banks.'
          GOTO 997
        ENDIF
      ENDDO
      IF ( keep .EQ. 0 ) GOTO 999
C
      keep = 0
C
C  Enlarge EFOL
C
      nefolp = 0
      DO ipartk = 1 , 6
        nefolp = nefolp + IW(ifwvc(ipartk)+lmhlen) - 1
      ENDDO
      IEFOL = NBANK ('EFOL', 3, lmhlen + lefola*nefolp)
      IF ( IEFOL .EQ. 0 ) THEN
        WRITE (IW(6),*) ' **** CREFOL **** Unable to enlarge EFOL #3'
        GOTO 997
      ENDIF
C
C Fill EFOL
C
      nefol = 0
C
C 1.- Good traks (good tracks from V0s are omitted)
C
      DO 1 itk = 2, IW(ifwvc(1)+lmhlen)
        jfwvc = ifwvc(1) + lmhlen + kfwvc*(itk-1)
        ich = IW(jfwvc + 6)
        DO 2 jtk = 2, IW(ifwvc(2)+lmhlen)
          jfwv2 = ifwvc(2) + lmhlen + kfwvc*(jtk-1)
          jch = IW(jfwv2 + 6)
          iv0 = IW(jfwv2 + 7)
          IF ( ich .EQ. KCHT(iv0) ) GOTO 1
    2   CONTINUE
        jtyp = 0
        DO 11 ifl = 1, 200
          IF ( itkflg(ifl) .EQ. 0 ) GOTO 12
          IF ( itkflg(ifl) .EQ. ich ) THEN
            IF ( idflag(ifl) .EQ. 1 ) jtyp = 1
            IF ( idflag(ifl) .EQ. 3 ) jtyp = 2
            GOTO 12
          ENDIF
   11   CONTINUE
   12   nefol = nefol+1
        IF ( nefol .GT. nefolp ) THEN
          WRITE (IW(6),*) ' +++ ENFLW Warning +++ Run/Evt ',krun,kevt
          WRITE (IW(6),*) ' The size of EFOL is smaller than expected'
          WRITE (IW(6),*) ' Call an expert '
          GOTO 1
        ENDIF
        jefol = iefol + lmhlen + lefola*(nefol-1)
        RW(jefol + jefopx) = RW(jfwvc + 1)
        RW(jefol + jefopy) = RW(jfwvc + 2)
        RW(jefol + jefopz) = RW(jfwvc + 3)
        RW(jefol + jefoew) = RW(jfwvc + 4)
        RW(jefol + jefowe) = RW(jfwvc + 5)
        IW(jefol + jefoty) = jtyp
        IW(jefol + jefole) = 0
        IW(jefol + jefolt) = ktn(IW(jfwvc+6))
        IW(jefol + jefolh) = 0
        IW(jefol + jefolc) = 0
        IW(jefol + jefolj) = 0
    1 CONTINUE
C
C 2.- Tracks from good V0s
C
      DO 3 itk = 2, IW(ifwvc(2)+lmhlen)
        jfwvc = ifwvc(2) + lmhlen + kfwvc*(itk-1)
        IF ( RW(jfwvc + 5) .EQ. 0. ) GOTO 3
        ich = IW(jfwvc + 6)
        ist = IW(jfwvc + 7)
        jtypsi = 0
        DO 4 jtk = 2, IW(ifwvc(1)+lmhlen)
          jfwv1 = ifwvc(1) + lmhlen + kfwvc*(jtk-1)
          IF ( RW(jfwv1 + 5) .EQ. 0. ) GOTO 4
          jch = IW(jfwv1 + 6)
          IF ( jch .EQ. KCHT(ist) ) THEN
            RW(jfwv1 + 1) = qx(ist)
            RW(jfwv1 + 2) = qy(ist)
            RW(jfwv1 + 3) = qz(ist)
            RW(jfwv1 + 4) = qe(ist)
            jtypsi = 0
            DO 41 ifl = 1, 200
              IF ( itkflg(ifl) .EQ. 0 ) GOTO 6
              IF ( itkflg(ifl) .EQ. ich ) THEN
                IF ( idflag(ifl) .EQ. 1 ) jtypsi = 1
                IF ( idflag(ifl) .EQ. 3 ) jtypsi = 2
                GOTO 6
              ENDIF
   41       CONTINUE
            GOTO 6
          ENDIF
    4   CONTINUE
        jtypsi = 3
        DO 5 jtk = 2, IW(ifwvc(2)+lmhlen)
          jfwv1 = ifwvc(2) + lmhlen + kfwvc*(jtk-1)
          IF ( RW(jfwv1 + 5) .EQ. 0. ) GOTO 5
          jch = IW(jfwv1 + 6)
          IF ( jch .EQ. KCHT(ist) ) GOTO 6
    5   CONTINUE
        WRITE (IW(6),*) '**** ENFLW **** No sister to track ',itk,
     .             ' from FWVC (NR=2)'
        GOTO 3
    6   RW(jfwv1 + 5) = 0.
        nefol = nefol+1
        IF ( nefol .GT. nefolp ) THEN
          WRITE (IW(6),*) ' +++ ENFLW Warning +++ Run/Evt ',krun,kevt
          WRITE (IW(6),*) ' The size of EFOL is smaller than expected'
          WRITE (IW(6),*) ' Call an expert! '
          GOTO 3
        ENDIF
        jefol = iefol + lmhlen + lefola*(nefol-1)
        RW(jefol + jefopx) = RW(jfwvc + 1)
        RW(jefol + jefopy) = RW(jfwvc + 2)
        RW(jefol + jefopz) = RW(jfwvc + 3)
        RW(jefol + jefoew) = RW(jfwvc + 4)
        RW(jefol + jefowe) = 1.
        IW(jefol + jefoty) = 3
        IW(jefol + jefole) = 0
        IW(jefol + jefolt) = ktn(IW(jfwvc+6))
        IW(jefol + jefolh) = 0
        IW(jefol + jefolc) = 0
        IW(jefol + jefolj) = 0
        nefol = nefol+1
        IF ( nefol .GT. nefolp ) THEN
          WRITE (IW(6),*) ' +++ ENFLW Warning +++ Run/Evt ',krun,kevt
          WRITE (IW(6),*) ' The size of EFOL is smaller than expected'
          WRITE (IW(6),*) ' Call an expert! '
          GOTO 3
        ENDIF
        jefol = iefol + lmhlen + lefola*(nefol-1)
        RW(jefol + jefopx) = RW(jfwv1 + 1)
        RW(jefol + jefopy) = RW(jfwv1 + 2)
        RW(jefol + jefopz) = RW(jfwv1 + 3)
        RW(jefol + jefoew) = RW(jfwv1 + 4)
        RW(jefol + jefowe) = 1.
        IW(jefol + jefoty) = jtypsi
        IW(jefol + jefole) = 0
        IW(jefol + jefolt) = ktn(IW(jfwv1+6))
        IW(jefol + jefolh) = 0
        IW(jefol + jefolc) = 0
        IW(jefol + jefolj) = 0
    3 CONTINUE
C
C 3.- Neutral objects
C
      DO 7 ipartk = 3, 6
        DO 8 itk = 2, IW(ifwvc(ipartk)+lmhlen)
          jfwvc = ifwvc(ipartk) + lmhlen + kfwvc*(itk-1)
          ich = IW(jfwvc + 6)
          nefol = nefol+1
          IF ( nefol .GT. nefolp ) THEN
            WRITE (IW(6),*) ' +++ ENFLW Warning +++ Run/Evt ',krun,kevt
            WRITE (IW(6),*) ' The size of EFOL is smaller than expected'
            WRITE (IW(6),*) ' Call an expert! '
            GOTO 8
          ENDIF
          jefol = iefol + lmhlen + lefola*(nefol-1)
          RW(jefol + jefopx) = RW(jfwvc + 1)
          RW(jefol + jefopy) = RW(jfwvc + 2)
          RW(jefol + jefopz) = RW(jfwvc + 3)
          RW(jefol + jefoew) = RW(jfwvc + 4)
          RW(jefol + jefowe) = RW(jfwvc + 5)
          IF ( ipartk .EQ. 3 ) THEN
            IW(jefol + jefoty) = 4
            IW(jefol + jefole) = IW(jfwvc + 6)
            IW(jefol + jefolc) = 0
          ELSEIF ( ipartk .EQ. 4 ) THEN
            IW(jefol + jefoty) = 5
            IW(jefol + jefole) = 0
            IW(jefol + jefolc) = IW(jfwvc + 6)
          ELSEIF ( ipartk .EQ. 5 ) THEN
            IW(jefol + jefoty) = 7
            IW(jefol + jefole) = IW(jfwvc + 6)
            IW(jefol + jefolc) = 0
          ELSEIF ( ipartk .EQ. 6 ) THEN
            IW(jefol + jefoty) = 8
            IW(jefol + jefole) = IW(jfwvc + 6)
            IW(jefol + jefolc) = 0
          ENDIF
          IW(jefol + jefolt) = 0
          IW(jefol + jefolh) = 0
          IW(jefol + jefolj) = 0
    8   CONTINUE
    7 CONTINUE
C
      IW (iefol + 2) = nefol
      IF ( nefol .LT. nefolp ) THEN
        WRITE (IW(6),*) ' +++ ENFLW Warning +++ Run/Evt ',krun,kevt
        WRITE (IW(6),*) ' The size of EFOL is larger than expected'
        WRITE (IW(6),*) ' Call an expert! '
      ENDIF
C
C 4.- : Now save the identification information for ALL the muons
C
      DO 9 mefol = 1, nefol
        jefol = iefol + lmhlen + lefola*(mefol-1)
        IF ( IW(jefol+jefoty) .GT. 3 ) GOTO 90
        itk = IW(jefol+jefolt)
        DO 91 ich = kfcht, klcht
          IF ( ktn(ich) .EQ. itk ) GOTO 92
   91   CONTINUE
        GOTO 9
   92   CONTINUE
        iid = 0
        ifl = 0
        pseudow = 0.
        DO 93 nch = 1 , 200
          IF ( ( iid .EQ. 1 .OR. itkmue(nch) .EQ. 0 ) .AND.
     .         ( ifl .EQ. 1 .OR. itkflg(nch) .EQ. 0 ) ) GOTO 94
          IF ( itkmue(nch) .EQ. ich ) THEN
            nexp = muflag(nch)/20
            IF ( nexp .LT. 0 ) nexp = 0
            nfir = muword(nch)/2000
            hcal = AMIN1(umhcal(nch),99.)
            pseudow = pseudow + hcal/100.
     .              + 5.*FLOAT(nexp) + 200.*FLOAT(nfir)
            iid = 1
          ENDIF
          IF ( itkflg(nch) .EQ. ich ) THEN
            pseudow = pseudow + FLOAT(idflag(nch))
            ifl = 1
          ENDIF
   93   CONTINUE
   94 CONTINUE
C
      RW(jefol+jefowe) = pseudow
C
    9 CONTINUE
   90 CONTINUE
C
C  Build also a small bank (EAUX) to be written onto the mini
C
      CALL bdrop(iw,'EAUX')
      ieaux = NBANK('EAUX', 0, lmhlen+leauxa)
      IF ( ieaux .EQ. 0 ) THEN
        WRITE (IW(6),*) ' **** CREFOL **** Unable to create bank EAUX'
        GOTO 997
      ENDIF
      CALL blist(iw,'S+','EAUX')
      IF ( krun .LE. 4000 ) THEN
        IF ( NLINK('X1RG',0) .GT. 0 ) THEN
          CALL x1redo
        ELSE
          IF ( first ) THEN
            CALL x1ini(-1)
            first = .FALSE.
          ENDIF
          CALL x1trg
        ENDIF
      ENDIF
      itrgfl = ALTRIG(itrgw,itrg2,itrg3)
      iw(ieaux + lmhcol) = leauxa
      iw(ieaux + lmhlen) = 1
      iw(ieaux + lmhlen + jeautr) = itrgw
      iw(ieaux + lmhlen + jeauca) =  1000*nphco + npeco
      xhvok = xvlcal.AND.xvecal.and.xvhcal
      IF ( xhvok ) THEN
        iw(ieaux + lmhlen + jeauhv) = 1
      ELSE
        iw(ieaux + lmhlen + jeauhv) = 0
      ENDIF
      rw(ieaux + lmhlen + jeauhe ) = hadtot
      rw(ieaux + lmhlen + jeauhk ) = hadkil
      rw(ieaux + lmhlen + jeaue2 ) = ene12
      keep = 1
      GO TO 999
C No room for banks :
 997  CALL QWMESE(
     +'0CREFOL_ : Unable to create ENFLW banks :--> Enlarge BOS IW !!')
  999 RETURN
      END
#endif
