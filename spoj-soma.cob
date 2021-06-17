        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAIN.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 i        PIC 9(2).
        01 n        PIC 9(2).
        01 linha    PIC 9(4).
        01 result   PIC 9(6) VALUE 0.
        01 indx     PIC 9(1).
        01 casas    PIC 9(1).
        PROCEDURE DIVISION.
        processar.
            ACCEPT n.
            IF n = 0 THEN
                DISPLAY 0
                STOP RUN
            END-IF.
            PERFORM somar VARYING i FROM 0 BY 1 UNTIL i = n.
            PERFORM show-result.
            STOP RUN.
        somar.
            ACCEPT linha.
            COMPUTE result = result + linha.
        show-result.
            PERFORM VARYING indx FROM 1 BY 1 
                UNTIL result(indx:1) <> 0 END-PERFORM.
            COMPUTE casas = 7 - indx.
            DISPLAY result(indx:casas).
