      SUBROUTINE quclal

C-----------------------------------------------------------------------
C     Clustering of hadrons and photons taking into account photons 
C     from hadronic interactions (satellites).
C
C     Patrick Janot -- 17 Nov 1993 (previously in quphst)
C     Modified: M. Kado  October 1997 
C-----------------------------------------------------------------------

#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "parcut.h"
#include "fsimid.h"
#include "psfrjj.h"
#include "trexjj.h"
#include "hadpho.h"
#include "ftrig.h"
#include "coddat.h"
#include "psfrfu.h"
#include "qmacro.h"

C
C Clustering of the hadrons
C

      CALL quclus(2)
      DO 30 jclu = 1, nclu
        CALL ucopy(vclu(1,jclu),vec(1),4)
        IF ( vec(4) .LE. .001 ) GOTO 30
        theta = ACOS ( ABS(vec(3)) /
     .          SQRT ( vec(1)**2+vec(2)**2+vec(3)**2 ) )
        IF ( theta*180./pi .GT. hcaacc ) THEN
          CALL addve1(vec,4)
          nhatot = nhatot + 1
          iefoha(nhatot) = isave
          nmotha(nhatot) = nmoclu(jclu)
          do i = 1, nmotha(nhatot)
            imotha(i,nhatot) = moclu(i,jclu)
            emotha(i,nhatot) = fclu(i,jclu)
          enddo
        ELSE
          CALL addve1(vec,5)
          nlutot = nlutot + 1
          iefolu(nlutot) = isave
          nmotlu(nlutot) = nmoclu(jclu)
          do i = 1, nmotlu(nlutot)
            imotlu(i,nlutot) = moclu(i,jclu)
            emotlu(i,nlutot) = fclu(i,jclu)
          enddo
        ENDIF
        IW(koqvec+isave*kcqvec+jqvepa)= 10
30    CONTINUE


C
C Clustering of the photons
C
      CALL quclus(1)
      DO 20 jclu = 1, nclu
        CALL ucopy(vclu(1,jclu),vec(1),4)
        theta = ACOS ( ABS(vec(3)) /
     .          SQRT ( vec(1)**2+vec(2)**2+vec(3)**2 ) )
        IF ( theta*180./pi .GT. polcor(3) ) THEN
          CALL addve1(vec,3)
          nphtot = nphtot + 1
          iefoph(nphtot) = isave
          nmotph(nphtot) = nmoclup(jclu)
          do i = 1, nmotph(nphtot)
            imotph(i,nphtot) = moclup(i,jclu)
            emotph(i,nphtot) = fclup(i,jclu)
          enddo
        ELSE
          CALL addve1(vec,5)
          nlutot = nlutot + 1
          iefolu(nlutot) = isave
          nmotlu(nlutot) = nmoclup(jclu)
          do i = 1, nmotlu(nlutot)
            imotlu(i,nlutot) = moclup(i,jclu)
            emotlu(i,nlutot) = fclup(i,jclu)
          enddo
        ENDIF
        IW(koqvec+isave*kcqvec+jqvepa)= 1
 20   CONTINUE

      END




