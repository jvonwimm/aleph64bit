      SUBROUTINE ENFWLIST(ICLASS)
C--------------------------------------------------------------------
C! Give a listing of the ENFLW objects with their lock status
C
C  P. Janot   02 october 1990
C--------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "parcut.h"
      CHARACTER*50 namls(6)
      CHARACTER*3 namp(6)
      DATA namp/'  ?','!e!','?e?','mu!','mu?','   '/
      DATA namls/'          Charged particles:',
     .           '          Particles coming from vzero:',
     .           '          Photons:',
     .           '          Neutral hadrons:',
     .           '          Low angle particles (LCAL-ECAL-HCAL):',
     .           '          Low angle particles (SICAL):'/
#include "qmacro.h"
C--------------------------------------------------------------------
      IF ( iclass .EQ. kreco ) THEN
        WRITE (IW(6),100) krun,kevt
        DO 1000 ikpar = 1 , 6
          IF ( kfrtot(ikpar) .GT. klstot(ikpar) ) GOTO 1000
          IF ( ikpar .LE. 2 ) THEN
            WRITE (IW(6),200) namls(ikpar)
          ELSEIF ( ikpar .EQ. 3 ) THEN
            WRITE (IW(6),201) namls(ikpar)
          ELSEIF ( ikpar .EQ. 4 ) THEN
            WRITE (IW(6),202) namls(ikpar)
          ELSE
            WRITE (IW(6),203) namls(ikpar)
          ENDIF
          DO 1500 itk = kfrtot(ikpar), klstot(ikpar)
C           kkk = kalpha(itk)
            kkk = itk
 1501       kkk = ksame(kkk)
            IF ( kkk .EQ. itk ) THEN
               kkk = kalpha(itk)
               GOTO 1502
            ENDIF
            IF ( kkk .GT. kleft .OR. kkk .LT. kfeft ) GOTO 1501
 1502       kis = ksister(itk)
            CALL idmuon(itk,nexp,nfir,n10,n03,nmu,idf,ehc,idt,
     .                  xmult,rapp,ang,ishad,sudnt,imcf)
            CALL idelec(itk,idip,idif,rt,rl,idt)
            CALL infoga(itk,iclu,nchflg,ncrflg,nga)
            IF ( ikpar .LE. 2 ) THEN
              WRITE (IW(6),300) itk,kkk,
     .        qx(itk),qy(itk),qz(itk),
     .        qe(itk),qch(itk),xlock(itk),kis,
     .        namp(idt+1),idf,nexp,nfir,n10,n03,nmu,ehc,
     .        idip,idif,rt,rl,dzzz(itk)
            ELSEIF (ikpar .EQ. 3 ) THEN
              WRITE (IW(6),301) itk,kkk,
     .        qx(itk),qy(itk),qz(itk),
     .        qe(itk),qch(itk),xlock(itk),
     .        nchflg,ncrflg,nga
            ELSEIF ( ikpar .EQ. 4 ) THEN
              WRITE (IW(6),302) itk,kkk,
     .        qx(itk),qy(itk),qz(itk),
     .        qe(itk),qch(itk),xlock(itk),
     .        100.*propec(itk-kfrtot(4)+1)
            ELSE
              WRITE (IW(6),303) itk,kkk,
     .        qx(itk),qy(itk),qz(itk),
     .        qe(itk),qch(itk),xlock(itk)
            ENDIF
 1500     CONTINUE
 1000   CONTINUE
        WRITE (IW(6),400)
      ELSE
        ikpar0 = 0
        WRITE (IW(6),500) krun,kevt,iclass
        itk = KPDIR('ENFW',iclass)
 998    IF ( itk .EQ. 0 ) GOTO 999
        itksam = itk
 996    itksam = KSAME(itk)
        IF ( itksam .EQ. itk ) GOTO 997
        IF ( itksam .LT. kfrtot(1) .OR.
     .       itksam .GT. klstot(6) ) GOTO 996
        DO 995 ikpar = 1 , 6
          IF ( itksam .GE. kfrtot(ikpar) .AND.
     .         itksam .LE. klstot(ikpar) ) GOTO 994
 995    CONTINUE
 994    IF ( ikpar .NE. ikpar0 ) THEN
          ikpar0 = ikpar
          WRITE (IW(6),550) namls(ikpar)
        ENDIF
        CALL idmuon(itksam,nexp,nfir,n10,n03,nmu,idf,ehc,idt,
     .              xmult,rapp,ang,ishad,sudnt,imcf)
        CALL idelec(itksam,idip,idif,rt,rl,idt)
        WRITE (IW(6),600) itk,itksam,
     .               qx(itk),qy(itk),qz(itk),
     .               qe(itk),qch(itk),xlock(itk),
     .               namp(idt+1)
        GOTO 997
 997    itk = KFOLLO(itk)
        GOTO 998
 999    CONTINUE
        WRITE (IW(6),700)
      ENDIF
      RETURN
C----------------------------------------------------------------
 100  FORMAT(/
     . ' ==========================================================='/
     . '   Listing of ENFW objects - Run ',i5,' Evt ',i5)
 200  FORMAT(
     . ' ==========================================================='/
     .   1X,A50/
     . ' -----------------------------------------------------------'/
     . ' itk itk0      Px        Py        Pz',
     . '        E     ch  lck iv0 idt idf nx nf 10 03 mu  hc ',
     . ' eid kf     rt       rl       d0 '/
     . ' -----------------------------------------------------------')
 201  FORMAT(
     . ' ==========================================================='/
     .   1X,A50/
     . ' -----------------------------------------------------------'/
     . ' itk itk0      Px        Py        Pz',
     . '        E     ch  lck nt cr ng '/
     . ' -----------------------------------------------------------')
 202  FORMAT(
     . ' ==========================================================='/
     .   1X,A50/
     . ' -----------------------------------------------------------'/
     . ' itk itk0      Px        Py        Pz',
     . '        E     ch  lck   %(ECAL)'/
     . ' -----------------------------------------------------------')
 203  FORMAT(
     . ' ==========================================================='/
     .   1X,A50/
     . ' -----------------------------------------------------------'/
     . ' itk itk0      Px        Py        Pz',
     . '        E     ch  lck '/
     . ' -----------------------------------------------------------')
 300  FORMAT(1X,I3,2X,I3,2X,F8.4,2X,F8.4,
     .       2X,F8.4,2X,F8.4,2X,F3.0,3X,L1,1X,I3,1X,A3,1X,6(I3),1X,F5.2,
     .       2(1X,I2),3(1X,F8.2))
 301  FORMAT(1X,I3,2X,I3,2X,F8.4,2X,F8.4,
     .       2X,F8.4,2X,F8.4,2X,F3.0,3X,L1,3(1X,I2))
 302  FORMAT(1X,I3,2X,I3,2X,F8.4,2X,F8.4,
     .       2X,F8.4,2X,F8.4,2X,F3.0,3X,L1,3X,F6.2)
 303  FORMAT(1X,I3,2X,I3,2X,F8.4,2X,F8.4,
     .       2X,F8.4,2X,F8.4,2X,F3.0,3X,L1)
 400  FORMAT(' ============== E n d    o f    L i s t i n g ',
     .       '=============='/)
 500  FORMAT(/' ==================================================',
     .   '===================='/
     .   '   Listing of ENFW objects - Run ',i5,' Evt ',i5,
     .   ' Class ',I3)
 550  FORMAT(' ==================================================',
     .   '===================='/
     .   8X,A50/
     .   ' --------------------------------------------------',
     .   '--------------------'/
     .   ' itk itk0     px        py        pz   ',
     .   '     e     ch  lck idt'/
     .   ' --------------------------------------------------',
     .   '--------------------')
 600  FORMAT(1X,I3,1X,I3,2X,F8.4,2X,F8.4,2X,F8.4,
     .       2X,F8.4,2X,F3.0,3X,L1,1X,A3)
 700  FORMAT(' =================== ',
     . 'E n d    o f    L i s t i n g ',
     . '===================='/)
      END
#endif
