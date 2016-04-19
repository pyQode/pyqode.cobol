150604*******************************************************************
150604** Virtual printer subprogram
150604*******************************************************************
150604 IDENTIFICATION DIVISION.
150604**************************************
150604 PROGRAM-ID. VIRTUAL-PRINTER.
150604**
150604 ENVIRONMENT DIVISION.
150604***************************************
150604**
150604 INPUT-OUTPUT SECTION.
150604**-*-*-*-*-*-*-*-*-*-*-*-*-*
150604 FILE-CONTROL.
150604     SELECT FPRINTER ASSIGN to "./printer.dat"
150604     ORGANIZATION LINE SEQUENTIAL
150604 ACCESS SEQUENTIAL.
150604**
150604 DATA DIVISION.
150604**************************************
150604 FILE SECTION.
150604**-*-*-*-*-*-*-*-*-*-*-*-*-*
150604 FD FPRINTER.
150604 01 ENREG-PRINTER PIC X(80).
150604**
150604 WORKING-STORAGE SECTION.
150604**-*-*-*-*-*-*-*-*-*-*-*-*-*
150604 LINKAGE SECTION.
150604**-*-*-*-*-*-*-*-*-*-*-*-*-*
150604 01 RECEIVED-PARAM.
150604     02 PA-RESET         PIC X       .
150604     02 PA-BUFFER        PIC X(80)   .
150604     02 PA-WHEN          PIC X(6)    .
150604     02 PA-WHAT          PIC X(5)    .
150604     02 PA-HOWMANY       PIC 99      .
150604 PROCEDURE DIVISION USING RECEIVED-PARAM.
150604**************************************
150604 MAIN-PRINTER.
150604
150604     IF(PA-RESET = "O")
150604         OPEN OUTPUT FPRINTER
150604         if(PA-WHEN = "AFTER")
150604             if(PA-WHEN = "AFTER")
150604                 WRITE ENREG-PRINTER
150604             END-IF
150604         END-IF
150604     ELSE
150604         OPEN EXTEND FPRINTER
150604         IF(PA-WHEN = "AFTER")
150604             IF(PA-WHAT = "PAGE")
150604                 MOVE '>------------------------------------------'
150604-'------------------------------------<' TO ENREG-PRINTER
150604                 WRITE ENREG-PRINTER
150604             ELSE
150604                 SUBTRACT 1 FROM PA-HOWMANY
150604                 PERFORM PA-HOWMANY TIMES
150604                     MOVE SPACES TO ENREG-PRINTER
150604                     WRITE ENREG-PRINTER
150604                 END-PERFORM
150604             END-IF
150604         END-IF
150604         WRITE ENREG-PRINTER FROM PA-BUFFER
150604         IF(PA-WHEN = "BEFORE")
150604            IF(PA-WHAT = "PAGE")
150604                MOVE '>------------------------------------------'
150604-'------------------------------------<' TO ENREG-PRINTER
150604                WRITE ENREG-PRINTER
150604            ELSE
150604                SUBTRACT 1 FROM PA-HOWMANY
150604                PERFORM PA-HOWMANY TIMES
150604                    MOVE SPACES TO ENREG-PRINTER
150604                    WRITE ENREG-PRINTER
150604                END-PERFORM
150604            END-IF
150604         END-IF
150604     END-IF
150604     CLOSE FPRINTER
150604     MOVE "N"        TO PA-RESET
150604     MOVE SPACES     TO PA-BUFFER
150604     MOVE "AFTER"    TO PA-WHEN
150604     MOVE "LINES"    TO PA-WHAT
150604     MOVE 1          TO PA-HOWMANY
150604     EXIT PROGRAM.
150604 END PROGRAM VIRTUAL-PRINTER.
