      SUBROUTINE RDEFMI
C----------------------------------------------------------------------
C!  - Module to steer production of MINI-DST DHEA Bank
C!
C!    Author:    S. Haywood 17/05/88
C!    Modified:  S. Haywood 07/06/89
C!   Modified :- E. Lancon              7-MAR-1991
C!              Mini banks are now written by the MINI package,
C!              keep only DHEA.
C!
C!    Called by RECONS.
C!
C!    All subroutines in this module use the BOS library and
C!    the ALEPHLIB.
C!
C! Description
C! -----------
C!    The routines to create DHEA bank (MINHEA and FIDHEA)
C!    are called in turn
C!
C?
C!======================================================================
#ifndef DOC
C
C++   Fill HEADER with summary information.
C
      CALL MINHEA
C
C++   Fill HEADER with E-Flow information
C
      CALL FIDHEA
C
      RETURN
      END
#endif
