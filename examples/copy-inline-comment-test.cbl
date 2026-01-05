       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPY-INLINE-COMMENT-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Single-line COPY with trailing inline comment after the period
       COPY HOGEHOGE.   *> COPYBOOK of HOGEHOGE

      * COPY with trailing inline comment but no DISJOINING/JOINING
       COPY FUGAFUGA.   *> simple copybook

       PROCEDURE DIVISION.
       STOP RUN.
