      PARAMETER (NTIMEC=14)
      COMMON/TPTIME/ TPTSUM(NTIMEC)
C
C---------------------------------------------------------
#if defined(DOC)
C
C!  NTIMEC =  Number of TPC modules for which to keep time statistics
C  TPTSUM =  CPU times used by TPC reconstruction routines
C         1  TPREDA
C         2  TPCREC
C         3  TPADS
C         4  TWIRES
C         5  TCODRV
C         6  PATREC
C         7  TFITTK
C         8  TRKWRA
C         9  TRKELS
C        10  TFLNKS + TFCAND
C        11  TOVRLP
C        12  TCOORT
C        13  TFLKN2
C        14  TPADDX + TPDELS
C-----------------------------------------------------------
#endif
