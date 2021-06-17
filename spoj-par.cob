        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAIN.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 teste    PIC 9(4) VALUE 1.
        01 i        PIC 9(2).
        01 n        PIC 9(4).
        01 linha    PIC 9(3).
        01 nome1    PIC A(10).
        01 nome2    PIC A(10).
        01 soma     PIC 9(2).
        01 indx-zro PIC 9(2).
        01 indxA    PIC 9(2).
        01 indxB    PIC 9(2).
        01 casas    PIC 9(1).
        PROCEDURE DIVISION.
        processar.
            ACCEPT n.
            IF n = 0 THEN
                STOP RUN
            END-IF.
            
            ACCEPT nome1.
            ACCEPT nome2.
            
            PERFORM calc-zros.
            DISPLAY 'Teste ' teste(indx-zro:casas).
            ADD 1 TO teste.
            
            PERFORM calcular VARYING i FROM 0 BY 1 UNTIL i = n.
            DISPLAY X'04'.
            PERFORM processar.
            
        calcular.
            ACCEPT linha.
            COMPUTE soma = FUNCTION NUMVAL(linha(2:1)) + 
                FUNCTION NUMVAL(linha(3:1)).
            
            IF FUNCTION MOD(soma 2) = 0 THEN
                PERFORM calc-nm1
                DISPLAY nome1(1:indxA)
            ELSE
                PERFORM calc-nm2
                DISPLAY nome2(1:indxB)
            END-IF.
            
        calc-zros.
            PERFORM VARYING indx-zro FROM 1 BY 1 
                UNTIL teste(indx-zro:1) <> 0 END-PERFORM.
            COMPUTE casas = 5 - indx-zro.
            
        calc-nm1.
            PERFORM VARYING indxA FROM 1 BY 1 
                UNTIL nome1(indxA:1) = SPACE OR < 10 END-PERFORM.
            SUBTRACT 1 FROM indxA.
            
        calc-nm2.
            PERFORM VARYING indxB FROM 1 BY 1 
                UNTIL nome2(indxB:1) = SPACE OR < 10 END-PERFORM.
            SUBTRACT 1 FROM indxB.
    
