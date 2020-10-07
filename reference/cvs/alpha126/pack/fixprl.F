      SUBROUTINE FIXPRL
C =========================================================================
C! Computes corrected Rl for all Monte Carlo 
C Execution activated by the data card UPRL in the users cards
C called from QFILL
C      For Monte-Carlo event :
C                - recalculate Rl Galeph version > 304 and < 400
C                                 Database > 206 and < 400
C                                 Author: P. Perret   31/01/97
c                                 modified I. Debonis 24/07/01 
c                                 modified P. Azzurri 08/02/02 
C
C     Input  ITK   Track number
C     Output RLnew new Rl electron estimator
C =========================================================================
#ifndef DOC
      implicit none
#include "qdecl.h"
#include "qcde.h"
#include "rhahjj.h"
      character*4 chaint
      character*8 PRNAM
      integer namind
      external chaint, namind
      integer naasim,kasim,imc,igeom,krhah,nrhah,irhah
     &     ,igver,idver
      integer ideb1,ideb2,ideb3
      data ideb1, ideb2, ideb3 / 0 , 0 , 0 /
      integer itk,iecl
      real tetifl,rlo,rlnew,rlmin,rlmax
      DATA RLMIN, RLMAX / -10.3 , 999. /
      integer igvermin,igvermax,igdbmin,igdbmax
      data igvermin,igvermax,igdbmin,igdbmax / 304, 400, 206, 400 /
#include "qmacro.h"
C -------------------------------------------------------------------------
C Initialisations and checks to know if the correction has to be done:
      if(ideb1.eq.0) then
        naasim = namind('ASIM')
        ideb1 = 1
      endif
c
c Check if update Rl needed
c
      if(iw(namind('UPRL')).eq.0) return
c
c For data : do nothing
c
      if(.not.xmcev) then
         if(ideb3.eq.0) then
           write(iw(6),'(///,4(/,10x,a),///)')
     > '+----------------------------------------------------------+',
     > '|                    WARNING!                              |',
     > '| Data NOT processed with RL electron estimator correction |',
     > '+----------------------------------------------------------+'
           ideb3 = 1
         endif
         return
      endif	
c 
c Get version of Galeph/db used for processing this event
c
      krhah = iw(narhah)
      if(krhah.eq.0) return
      igver = 0
      idver = 0
      nrhah = lrows(krhah)
      do irhah = 1,nrhah
        PRNAM(1:4) = chaint(itabl(krhah,irhah,jrhapn))
        PRNAM(5:8) = chaint(itabl(krhah,irhah,jrhapn+1))
        if(PRNAM.EQ.'GALEPH') THEN
          igver = itabl(krhah,irhah,jrhapv)
          idver = itabl(krhah,irhah,jrhadv)
        endif
      enddo

      if(igver.lt.igvermin.or.igver.gt.igvermax) return 
      if(idver.lt.igdbmin.or.idver.gt.igdbmax) return 
c
c
c  Rl re-computed
c
      if(ideb2.eq.0) then
        write(iw(6),'(///,5(/,10x,a),///)')
     > '+----------------------------------------------------------+',
     > '|    Data processed with RL electron estimator correction  |',
     > '|              FIXPRL called to fix RL value               |',
     > '|      !!!! Output banks are equal to input banks !!!      |',
     > '+----------------------------------------------------------+'
        ideb2 = 1
      endif
C -------------------------------------------------------------------------
C-    Extract RL
C
      DO ITK = KFCHT,KLCHT
C
      TETIFL  = 399.
C
C-    Keep old RL value
C
      RLO = QEIDRI(ITK,3)
      RLnew = RLO
C
C-    Check electron extrapolation
C
        IF (XEID(ITK)) THEN
C
C
C-      Calculate the row number in theta
C
          CALL QCPTETI(ITK,TETIFL,IECL)
C
C
          IF ( IECL.NE.0.AND.TETIFL.NE.399.) THEN
C
C-       Calculate longitudinal profile
C
            IF ((RLO.GT.RLMIN).and.(RLO.LT.RLMAX))
     >          CALL QCPRLN(RLO,TETIFL,QP(ITK),RLnew)
C
            rw(iw(kjqdet(itk)+jqdeae) + 3 +jeidr1-1) = RLnew
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END
#endif
