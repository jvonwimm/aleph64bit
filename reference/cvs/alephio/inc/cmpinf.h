      PARAMETER   (MAXBNK=1000)
      INTEGER         TOLOWER
      LOGICAL CHECKS,NOFLO,FMTFL
      COMMON/CMPINF/NUMBNK,INDEXW(MAXBNK),INFSIZ(4),MINGAI,RATMAX,
     +              IRANKB(MAXBNK),CHECKS,NOFLO,TOLOWER,FMTFL,JNONE
      COMMON/CMPCHA/LBNKS,NBNKS
C - len is 4*MAXBNK
      CHARACTER*4000  LBNKS, NBNKS
#if defined(DOC)
C!    Information needed for the compression.
C!
C!    NUMBNK : Number of banks for which information is availiable.
C!             If 0 no compression is done on any bank.
C!    INDEXW : Array of indices of work banks. For every bank there is a
C!             coresponding work bank with the same name that stores
C!             the type of each column and a precision factor (for real
C!             columns). These work banks are droped by the CMPDRO
C!             subroutine.
C!    INFSIZ : Array containing the number of information words stored
C!             inside the compressed bank for every column, depending on
C!             its type.
C!             Column types are numbered as following :
C!                   1 -> character
C!                   2 -> non compressible real (precision not known)
C!                   3 -> compressible real
C!                   4 -> integer
C!                   5 -> integer which doesn't need to be converted
C!                        before compression.
C!             Type 5 integers are separated from type 4 only in the
C!             CMPBAN subroutine. Type 5 does not exist before this
C!             level.
C!    MINGAI : Minimum gain required from every bank (in 32-bit words).
C!             If the estimated gain is less than MINGAI the bank is not
C!             compressed.
C!    RATMAX : Factor used for the above estimation.
C!    IRANKB : Array containing pointers to the INDEXW array. This
C!             array is sorted by the name of the bank.
C!    CHECKS : Flag indicating whether a checksum is added or not.
C!    NOFLO  : Flag indicating whether REAL are compress or not.
C!    FMTFL  : Flag indicating whether format are written or not.
C!    JNONE  : index of the 'NONE' work bank which contains the list of
C!             banks which have not to be compressed (PIDI, XTRB'
C!    TOLOWER: integer to be added to the bank name in upper case to get
C!             the 1st letter in lower case.
C!    LBNKS  : list of banks to be decompressed.
C!    NBNKS  : list of banks whose format is read from CFMT  card.
C!
C!    This common block is initialized by subroutine CMPINI and used
C!    in CMPIN2, CMPLIS, CMPBAN and CMPDRO.
#endif
C-----------------------------------------------------------------------
