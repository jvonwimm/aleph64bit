C! beam crossing position
      COMMON/YBCRTO/BCROSS(3),VBCROS(6)
#if defined(DOC)
C     BCROSS    ..... current beam crossing position
C     VBCROS   ..... triangular covariance matrix
C                  stored in the order 1 2 4
C                                        3 5
C                                          6
C                  the correlation terms are set to 0.
#endif
