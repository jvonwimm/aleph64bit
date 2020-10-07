      PARAMETER (MXHTCH=21,NPARCH=23)
      COMMON /TCHAIN/NHCHAI,IRCHAI(MXHTCH),IHCHAI(MXHTCH),
     &              MCHAIN(MXHTCH),D2CHAI(MXHTCH),PCHAIN(NPARCH),ISTCHA
#if defined(DOC)
C
C!  Information on current chain.
C  Used internally only in the TRKFND track finding module.
C
C      PCHAIN = rho, tanl, phi0, d0, z0, length (x-y), chisqr (x-y),
C               chisqr (z), diag. errors; all for the current chain
C      IRCHAI = row number of each hit in current chain
C      IHCHAI = pointer into the INDBIN workbank (see *CD TCBINS) for
C               each hit of the current chain
C      MCHAIN = rejection flag for each hit on current chain
C      ISTCHA = padrow index of current chain
C      NHCHAI = number of hits on current chain
C----------------------------------------------------------------------
#endif
