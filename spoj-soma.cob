        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAIN.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 i        PIC 9(2).
        01 n        PIC 9(2).
        01 linha    PIC S9(5).
        01 result   PIC S9(7) VALUE 0.
        01 indx     PIC 9(1).
        01 casas    PIC 9(1).
        01 negativo PIC 9(1) VALUE 0.
        01 minus-one PIC S9(1) VALUE -1.
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
            IF result < 0 THEN
                SET negativo TO 1
                MULTIPLY result BY minus-one GIVING result
            END-IF.
            PERFORM VARYING indx FROM 1 BY 1 
                UNTIL result(indx:1) <> 0 END-PERFORM.
            COMPUTE casas = 8 - indx.
            IF negativo = 1 THEN
                DISPLAY '-'result(indx:casas)
            ELSE 
                DISPLAY result(indx:casas)
            END-IF.
    
