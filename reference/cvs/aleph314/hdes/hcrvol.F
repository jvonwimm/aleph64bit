              SUBROUTINE HCRVOL(NROT,VIN,VOUT,NPOINT)
C----------------------------------------------------
CKEY HCALDES HCAL TRANSFORM  CORNERS VOLUME / USER
C
C!  Trasforms the coorners coordinates of the volume
C!        from Module R.S. to Aleph R.S.
C!
C!                                    Author G.Catanesi 880710
C!
C!
#ifndef DOC
              PARAMETER (LDIM=3)
              REAL VIN(NPOINT,LDIM),VOUT(NPOINT,LDIM)
              DIMENSION XIN(LDIM),XOUT(LDIM)
C
                 DO 30 J=1,NPOINT
                    DO 10 K=1,LDIM
                       XIN(K) = VIN(J,K)
   10             CONTINUE
                    CALL HCRINV(NROT,XIN,XOUT)
                    DO 20 K=1,LDIM
                       VOUT(J,K) = XOUT(K)
   20             CONTINUE
C
   30          CONTINUE
C
                 RETURN
                 END
#endif
