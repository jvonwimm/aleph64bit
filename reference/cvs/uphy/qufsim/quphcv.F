      SUBROUTINE QUPHCV
C-----------------------------------------------------------------------
C! Simulation of photon conversions in the material prior to the TPC
C
C  Input      :  none
C  Output     :  none
C
C  Patrick Janot -- 01 Nov 1993
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "parcut.h"
#include "resolu.h"
#include "lundcom.h"
      REAL par(6),parex(6)
      REAL pele(4), ppos(4)
#include "coddat.h"
#include "qmacro.h"
C
      j1(j) = koqvec + isave * kcqvec + j
      jv(j) = koqvrt + ivert * kcqvrt + j
C
C  Loop over MC particles
C
      DO 1 itk = kfmct, klmct
C
C  Keep only the "stable" photons
C
        IF ( kstabc(itk) .LT. 1 ) GOTO 1
        IF ( ktpcod(itk) .NE. 1 ) GOTO 1
C
C  Does it convert into e+e- ?
C
        test = RNDM(itk)
        sinth = SQRT(1.-qct(itk)**2)
C       WRITE(6,*) 'cos/sin : ',qct(itk),sinth
        IF ( ABS(qct(itk)) .LE. 1E-6 ) THEN
          WRITE(6,*) itk,qe(itk),qp(itk),qz(itk),ktn(itk)
          CALL prfkin
          GOTO 1
        ENDIF
        IF ( sinth .LE. 1E-6 ) GOTO 1
        tanth = ABS(sinth/qct(itk))
        prob = 0.
        DO 2 idime = 1, 7
          IF     (idime .EQ. 7 ) THEN
            rcyl = rdime(6) + RNDM(itk)*(rdime(7)-rdime(6))
          ELSEIF (idime .EQ. 4 ) THEN
            rcyl = rdime(4) + RNDM(itk)*(rdime(5)-rdime(4))
          ELSE
            rcyl = rdime(idime)
          ENDIF
          zcyl = zdime(idime)
C         WRITE(6,*) 'rcyl/zcyl : ',rcyl,zcyl,sinth
          IF ( tanth .GT. rcyl/zcyl )
     .    prob = prob + 1. - exp(-7./9.*x0rad(idime)/sinth)
          IF ( prob .GT. test ) GOTO 3
    2   CONTINUE
    3   IF ( idime .GT. 7 ) GOTO 1
        zato = atome(idime)
C
C  Extrapolate up to the conversion point
C
        ivr = KORIV(itk)
        par(1) = qvx(ivr)
        par(2) = qvy(ivr)
        par(3) = qvz(ivr)
C       WRITE(6,*) 'qp(itk) : ',qp(itk)
        par(4) = qx(itk)/qp(itk)
        par(5) = qy(itk)/qp(itk)
        par(6) = qz(itk)/qp(itk)
        CALL aulcyl (rcyl, zcyl, par, parex, icode)
        IF ( icode .NE. 1 ) GOTO 1
C
C Don't consider the conversion if it occurs outside the TPC
C
        IF ( ABS(parex(3)) .GT. zdime(7) ) GOTO 1
C
C
C  Create an e+e- pair
C
        CALL qupair(itk,zato,pele,ppos)
        IF ( ppos(4) .EQ. 0. .AND. pele(4) .EQ. 0. ) GOTO 1
C
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '+++ QUPHCV +++ Le photon ',
     .               itk,qx(itk),qy(itk),qz(itk),qe(itk),
     .               's''est converti'
          WRITE(6,*) 'e+    : ',ppos(1),ppos(2),ppos(3),ppos(4)
          WRITE(6,*) 'e-    : ',pele(1),pele(2),pele(3),pele(4)
        ENDIF
C
C  Add a new vertex to the ALPHA record
C
        ivert = KVERSV(dummy)
C
        RW(jv(JQVRVX)) = parex(1)
        RW(jv(JQVRVY)) = parex(2)
        RW(jv(JQVRVZ)) = parex(3)
        IW(jv(JQVRIP)) = itk
        IW(koqvec+itk*kcqvec+jqveev) = ivert
        IW(koqvec+itk*kcqvec+jqvesc) = -1
C
C And add the e+e- to the ALPHA MCT section
C
        posmom = SQRT(ppos(1)**2 + ppos(2)**2 + ppos(3)**2 )
        ppos(4) = SQRT(posmom**2+0.511E-3**2)
        CALL qvset4(inew,ppos)
        isave= KVSAVC(inew,'e+',kmonte)
        RW(j1(jqvech)) = 1.
        RW(j1(jqveqp)) = posmom
        IW(j1(jqvesc)) = 1
        IW(j1(jqvepa)) = 2
        IW(j1(jqveov)) = ivert
        IW(j1(jqveev)) = 0
        CALL qadau(itk,1,isave)
        CALL qamoth(itk,1,isave)
        CALL vzero(IW(j1(jqvebm)) , 10)
        CALL qsmctm(isave)
        IF ( idbg .GE. 10 ) WRITE(6,*) 'Index du positon     : ',isave
C
        elemom = SQRT(pele(1)**2 + pele(2)**2 + pele(3)**2 )
        pele(4) = SQRT(elemom**2+0.511E-3**2)
        CALL qvset4(inew,pele)
        isave= KVSAVC(inew,'e-',kmonte)
        RW(j1(jqvech)) =-1.
        RW(j1(jqveqp)) = elemom
        IW(j1(jqvesc)) = 1
        IW(j1(jqvepa)) = 3
        IW(j1(jqveov)) = ivert
        IW(j1(jqveev)) = 0
        CALL qadau(itk,1,isave)
        CALL qamoth(itk,1,isave)
        CALL vzero(IW(j1(jqvebm)) , 10)
        CALL qsmctm(isave)
        iv = KORIV(isave)
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) 'Index de l''electron : ',isave
          WRITE(6,*) 'Vertex position      : ',
     .               iv,qvx(iv),qvy(iv),qvz(iv)
        ENDIF
C
    1 CONTINUE
C
  999 RETURN
      END
