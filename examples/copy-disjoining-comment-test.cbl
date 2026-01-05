       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPY-DISJOINING-COMMENT-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * The following COPY spans multiple lines with trailing inline comments.
       COPY HOGE
            DISJOINING OLD
            JOINING NEW AS PREFIX. *> PREFIX OLD TO NEW, should close here

      * Another COPY to ensure the next one is detected after the comment.
       COPY FOO
            DISJOINING AAA
            JOINING BBB AS PREFIX.

       PROCEDURE DIVISION.
       STOP RUN.
