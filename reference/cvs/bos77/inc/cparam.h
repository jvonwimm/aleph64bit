C     ------CPARAM
C     COMMENT FOR MACRO PARAM
C
C     JW( )    DFLT VALUE    CONTENT
C .........    ..........    ...........................................
C        1          0        ADDITIONAL (INPUT) ARGUMENT
C
C        2                   RETURN CODE (OUTPUT ARGUMENT)
C
C        3          0        INDEX OF READ BANK
C        4          1        PRINT FLAG FOR MREADC ...
C LUC    5          5        CARD INPUT UNIT
C LUP    6          6        PRINT OUTPUT UNIT
C LUW    7        100        NUMBER OF MESSAGES TO BE PRINTED
C LUB    8        100        NUMBER OF BANKS TO BE PRINTED
C IAR+1  9      '    '       ) NAME (2*A4)
C IAR+2 10      '    '       )    OF ARRAY
C
C NJA   11                   NUMBER OF ARRAY
C NJW   12                   LENGTH OF ARRAY
C LJW   13                   ADDRESS OF ARRAY
C INM   14                   FIRST INDEX OF NAMED BANK AREA
C IGP   15                   FIRST INDEX OF GAP
C IWK   16                   FIRST INDEX OF WORK BANK AREA
C NDN   17                   NUMBER OF DROPPED WORDS OF NAMED BANKS
C NDW   18                   NUMBER OF DROPPED WORDS OF WORK BANKS
C IGN   19                   INDEX OF LOWEST DELETED NAMED BANK
C IGW   20                   INDEX OF HIGHEST DELETED WORK BANK
C
C IDL   21                   INDEX OF LINK BANK
C ILT+1 22                   INDEX OF WORK BANK FOR LIST 'C'
C    +2 23                   INDEX OF WORK BANK FOR LIST 'E'
C    +3 24                   INDEX OF WORK BANK FOR LIST 'R'
C    +4 25                   INDEX OF WORK BANK FOR LIST 'S'
C    +5 26                   INDEX OF WORK BANK FOR LIST 'T'
C    +6 27                   INDEX OF WORK BANK FOR LIST 'U'
C       28
C IOS   29                   INDEX OF IO-STATISTIC BANK
C ICK   30                   CHECK WORD (=12345)
C
C IRC+1 31                   STATISTIC: RETURN CODE 1
C    +2 32                              RETURN CODE 2
C    +3 33                              RETURN CODE 3
C    +4 34                              RETURN CODE 4
C    +5 35                              RETURN CODE 5
C    +6 36                              RETURN CODE 6
C    +7 37
C    +8 38
C NGN   39                   NUMBER OF N GARBAGE COLLECTIONS
C NGW   40                   NUMBER OF W GARBAGE COLLECTIONS
C
C NFH+1 41                   HISTOGRAM OF USED SPACE BEFORE GARBAGE
C NFH+2 42                   ...
C       ...                  ...
C NFH+10                     COLLECTION IN 10 PERCENT BINS
C     ------
