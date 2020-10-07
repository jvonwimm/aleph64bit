      SUBROUTINE UINCOV (IR,IOPT,NTPC,NITC,NVDET,FIELRC,VV0,COVIN,
     +                   COVOU,IERR)
C --------------------------------------------------------------
C! INCREASE THE ERRORS ON THE COVARIANCE MATRIX DUE TO M.S
C  This routine is obsolete.  Using UMSERR instead.
C
C     AUTHOR: LL. GARRIDO
C     MODIFIED: I. TOMALIN & LL. GARRIDO 89/06/05
C     MODIFIED: R. JOHNSON   22-01-91 to call UMSERR
C
C     INPUT::
C           IR : =0 MEANS YOU WANT THE ERROS ON THE INTERACTION
C                   REGION
C                =1 MEANS YOU WANT THE ERRORS ON THE ITC REGION
C                =2 MEANS YOU WANT THE ERRORS ON THE TPC REGION
C          IOPT: Not used
C          NTPC: Not used
C          NITC : NUMBER OF POINTS IN THE ITC USED IN  THIS
C                TRACK (CAN BE OBTAINED IN THE FRTL BANK)
C          NVDET: NUMBER OF POINTS IN THE VDET USED IN  THIS
C                TRACK (CAN BE OBTAINED IN THE FRTL BANK)
C          FIELRC: Not used
C          VV0:    THE 5 PARAMETERS OF THE HELIX
C          COVIN:  COVARIANCE MATRIX IN  TRIANGULAR FORM
C    OUTPUT::
C          COVOU: OUTPUT COVARIANCE MATRIX IN  TRIANGULAR FORM
C                   (15 ELEMENTS)
C          IERR: Error flag,
C                2= EVEH bank not found (for run number)
C                3= IR out of range
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
C
      DIMENSION VV0(*),COVIN(*),COVOU(*)
C
      CALL UMSERR(IR,NITC,NVDET,VV0,COVIN,COVOU,IERR)
      END
#endif
