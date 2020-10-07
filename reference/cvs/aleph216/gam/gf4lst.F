      SUBROUTINE GF4LST(ESTO1,ESTO2,ESTO3,INDSTO,KGRID,KF4GRI)
C-----------------------------------------------------------------
C  M. Verderi                                            2-10-94
C  Adapted from GXTRAN
C! Returns in KF4GRI(2,2,3) the list of PEST storey numbers in the
C  four central towers of the input grid KGRID(3,3,3)
C
C  Called by GAMPEX
C-----------------------------------------------------------------
#ifndef DOC
      REAL        ESTO1(9,*)
      REAL        ESTO2(9,*)
      REAL        ESTO3(9,*)
      INTEGER     INDSTO(2,*),KGRID(3,3,3),KF4GRI(2,2,3)
      REAL        ESTO (9),ES33(3,3)
      INTEGER     ISTO(9),ISTORE,KSTACK
      EQUIVALENCE (ESTO,ISTO)
      INTEGER     KI,KJ,KS,ITCR,JFCR,L,IV,JV,ISHIFT,JSHIFT
      REAL        EIP,EIC,EIM,EJP,EJM,ETT,BARI,BARJ,PTIT
      PARAMETER ( PTIT = .0001 )
C-------------------------------------------------------
      DO KI = 1,2
         DO KJ = 1,2
           DO KS = 1,3
              KF4GRI(KI,KJ,KS) = 0
           ENDDO
        ENDDO
      ENDDO
Calcul de la map d'energie 3*3 et memorisation de itcr jfcr:
      DO KI= 1 , 3
         DO KJ = 1 , 3
            ES33(KI,KJ)=0.
            DO KS = 1 , 3
               ISTORE = KGRID(KI,KJ,KS)
               IF (ISTORE.LT.0) RETURN
               IF (ISTORE.GT.0) THEN
               CALL GIVSTO(ESTO1,ESTO2,ESTO3,INDSTO,
     &         ISTORE,ESTO,KSTACK)
               ES33(KI,KJ)=ES33(KI,KJ)+ESTO(1)
               IF (KI.EQ.2.AND.KJ.EQ.2.AND.ISTORE.GT.0) THEN
                  ITCR = ISTO(5)
                  JFCR = ISTO(6)
               ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
C   Barycenter over 3 towers in pads units.
C
      EIP = 0.
      EIC = 0.
      EIM = 0.
      EJP = 0.
      EJM = 0.
      DO 2 L = 1 , 3
         EIP = EIP + ES33( 3 , L )
         EIC = EIC + ES33( 2 , L )
         EIM = EIM + ES33( 1 , L )
         EJP = EJP + ES33( L , 3 )
         EJM = EJM + ES33( L , 1 )
    2 CONTINUE
      ETT = EIP + EIC + EIM
      IF( ETT .LT. PTIT ) ETT = PTIT
      BARJ = ( EJP - EJM) / ETT
      BARI = ( EIP - EIM) / ETT
C   Analysis of 2 X 2 towers around barycenter.
      IV = 1
      JV = 1
      ISHIFT = 0
      JSHIFT = 0
      IF( BARI .GT. 0. ) THEN
         IV = 3
         ISHIFT = 1
      ENDIF
      IF( BARJ .GT. 0. ) THEN
         JV = 3
         JSHIFT = 1
      ENDIF
      DO KS = 1,3
         KF4GRI( 2-ISHIFT, 2-JSHIFT, KS) = KGRID( 2, 2, KS)
         KF4GRI(IV-ISHIFT, 2-JSHIFT, KS) = KGRID(IV, 2, KS)
         KF4GRI( 2-ISHIFT,JV-JSHIFT, KS) = KGRID( 2,JV, KS)
         KF4GRI(IV-ISHIFT,JV-JSHIFT, KS) = KGRID(IV,JV, KS)
      ENDDO
      RETURN
      END
#endif
