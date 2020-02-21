      * ===============================================================         
      * The user invokes this transaction (called MB02) via:                    
      *   MB02 <VERB>                                                           
      *                                                                         
      * Where:                                                                  
      *   <VERB> = CRE|UPD|DEL                                                  
      *                                                                         
      * Verb Functions:                                                         
      *                                                                         
      * CREate                                                                  
      *  Invoked via MB02 CRE <COLOR> <INVENTORY> <COST>                        
      *                                                                         
      *  Where:                                                                 
      *   <COLOR>     A new marble color not defined in EVENT.MARBLE            
      *   <INVENTORY> An integer representing the inventory count               
      *   <COST>      An integer representing the cost per unit                 
      *                                                                         
      *  Errors:                                                                
      *   MARB002E (color already exists in inventory)                          
      *   MARB004E (color is not available - not found in EVENT.COLOR)          
      *   MARB005E (color is reserved for another user                          
      *                                                                         
      *  Example:                                                               
      *   MB02 CRE ORANGE 10 4                                                  
      *                                                                         
      * UPDate                                                                  
      *  Invoked via MB02 UPD <COLOR> <INVENTORY> <COST>                        
      *                                                                         
      *  Where:                                                                 
      *   <COLOR>     A marble color defined in EVENT.MARBLE                    
      *   <INVENTORY> An integer representing the new inventory count           
      *   <COST>      An integer representing the new cost per unit             
      *                                                                         
      *  Errors:                                                                
      *   MARB001E (color not found in inventory)                               
      *   MARB005E (color is reserved for another user                          
      *                                                                         
      *  Example:                                                               
      *   MB02 UPD ORANGE 1 5                                                   
      *                                                                         
      * DELete                                                                  
      *  Invoked via MB02 DEL <COLOR>                                           
      *                                                                         
      *  Where:                                                                 
      *   <COLOR>     A marble color defined in EVENT.MARBLE                    
      *                                                                         
      *  Errors:                                                                
      *   MARB001E (color not found in inventory)                               
      *   MARB005E (color is reserved for another user                          
      *                                                                         
      *  Example:                                                               
      *   MB02 DEL ORANGE                                                       
      *                                                                         
      * Other Error Conditions:                                                 
      *   MARB003E (invalid operation was requested)                            
      *                                                                         
      * Build via                                                               
      *  gulp build --source marbles.cbl && gulp refresh --source marbles.cbl   
      *                                                                         
      * ===============================================================         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. MARBLE02.                                                    
       ENVIRONMENT DIVISION.                                                    
       DATA DIVISION.                                                           
      * ===============================================================         
      * Map input / output areas                                                
      * ===============================================================         
       WORKING-STORAGE SECTION.                                                 
       01 WS-WORK.                                                              
          02 WS-WORK-COLOR               PIC X(10) VALUE SPACE.                 
          02 WS-WORK-INV                 PIC S9(4) COMP VALUE 0.                
          02 WS-WORK-COST                PIC S9(4) COMP VALUE 0.                
          02 WS-WORK-ROW-COUNT           PIC S9(4) COMP-3 VALUE 0.              
          02 WS-WORK-TRAN                PIC X(4)  VALUE SPACE.                 
          02 WS-WORK-RED                 PIC S9(4) COMP VALUE 0.                
          02 WS-WORK-GREEN               PIC S9(4) COMP VALUE 0.                
          02 WS-WORK-BLUE                PIC S9(4) COMP VALUE 0.                
       01 WS-RESULT.                                                            
          02 WS-RESULT-COLOR             PIC 9(1)  VALUE 0.                     
             88 WS-RESULT-COLOR-NOTFOUND     VALUE 0.                           
             88 WS-RESULT-COLOR-FOUND        VALUE 1.                           
          02 WS-RESULT-OPERATION         PIC 9(1)  VALUE 0.                     
             88 WS-RESULT-OPERATION-FAIL     VALUE 0.                           
             88 WS-RESULT-OPERATION-SUCCESS  VALUE 1.                           
          02 WS-RESULT-VERB              PIC 9(1)  VALUE 0.                     
             88 WS-RESULT-VERB-INVALID       VALUE 0.                           
             88 WS-RESULT-VERB-CREATE        VALUE 1.                           
             88 WS-RESULT-VERB-UPDATE        VALUE 2.                           
             88 WS-RESULT-VERB-DELETE        VALUE 3.                           
       01 WS-CONST.                                                             
          02 WS-CONST-CREATE             PIC X(3)  VALUE 'CRE'.                 
          02 WS-CONST-UPDATE             PIC X(3)  VALUE 'UPD'.                 
          02 WS-CONST-DELETE             PIC X(3)  VALUE 'DEL'.                 
          02 WS-CONST-SUCCESS            PIC X(7)  VALUE 'SUCCESS'.             
       01 WS-ERROR-CODES.                                                       
          02 WS-ERROR-MARBLE-DNE         PIC X(8)  VALUE 'MARB001E'.            
          02 WS-ERROR-MARBLE-EXISTS      PIC X(8)  VALUE 'MARB002E'.            
          02 WS-ERROR-VERB-DNE           PIC X(8)  VALUE 'MARB003E'.            
          02 WS-ERROR-COLOR-DNE          PIC X(8)  VALUE 'MARB004E'.            
          02 WS-ERROR-COLOR-INVALID      PIC X(8)  VALUE 'MARB005E'.            
       01 WS-CICS-INPUT                  PIC X(74) VALUE SPACES.                
       01 WS-INPUT.                                                             
          05 WS-INPUT-TRAN-ID            PIC X(4).                              
          05 WS-INPUT-VERB               PIC X(3)  VALUE SPACES.                
          05 WS-INPUT-COLOR              PIC X(10) VALUE SPACES.                
          05 WS-INPUT-INV                PIC 9(4)  VALUE 0.                     
          05 WS-INPUT-COST               PIC 9(4)  VALUE 0.                     
          05 WS-INPUT-TRAILER            PIC X(20) VALUE SPACE.                 
       01 WS-OUTPUT                      PIC X(78) VALUE SPACE.                 
       01 WS-OUTPUT-SUCCESS              REDEFINES WS-OUTPUT.                   
          05 WS-OUTPUT-SUCCESS-TEXT      PIC X(7).                              
          05 FILLER                      PIC X(1).                              
          05 WS-OUTPUT-SUCCESS-WARNING   PIC X(70).                             
       01 WS-OUTPUT-ERROR                REDEFINES WS-OUTPUT.                   
          05 WS-OUTPUT-ERROR-CODE        PIC X(8).                              
          05 FILLER                      PIC X(1).                              
          05 WS-OUTPUT-ERROR-MESSAGE     PIC X(69).                             
       01 WS-MSG-LENGTH                  PIC S9(4) COMP.                        
      * ===============================================================         
      * Map SQL table for this transaction                                      
      * ===============================================================         
           EXEC SQL DECLARE EVENT.MARBLE TABLE                                  
           ( COLOR                          VARCHAR(10) NOT NULL,               
             INVENTORY                      INTEGER     NOT NULL,               
             COST                           INTEGER     NOT NULL                
           ) END-EXEC.                                                          
           EXEC SQL DECLARE EVENT.COLOR TABLE                                   
           ( COLOR                          VARCHAR(10) NOT NULL,               
             TRAN                           CHAR   (4)  NOT NULL,               
             RED                            INTEGER     NOT NULL,               
             GREEN                          INTEGER     NOT NULL,               
             BLUE                           INTEGER     NOT NULL                
           ) END-EXEC.                                                          
           EXEC SQL INCLUDE SQLCA END-EXEC.                                     
      * ===============================================================         
      * MB02 transaction                                                        
      * ===============================================================         
       PROCEDURE DIVISION.                                                      
      *                                                                         
      *     Initial working storage to known values                             
      *                                                                         
            PERFORM INIT-WORK-AREAS.                                            
      *                                                                         
      *     Receive user input (e.g. CRE BLUE)                                  
      *                                                                         
            PERFORM GET-TRANS-INPUT.                                            
      *                                                                         
      *     Parse the user input into corresponding fields                      
      *                                                                         
            PERFORM PARSE-CICS-INPUT.                                           
      *                                                                         
      *     Verify known input verb                                             
      *                                                                         
            PERFORM VERIFY-VERB.                                                
      *                                                                         
      *     Verify correct color                                                
      *                                                                         
            IF WS-OUTPUT-ERROR-CODE = SPACE THEN                                
                PERFORM VERIFY-COLOR.                                           
      *                                                                         
      *     Route to specific verb processing routine                           
      *                                                                         
            IF WS-OUTPUT-ERROR-CODE = SPACE THEN                                
                EVALUATE TRUE                                                   
      *                                                                         
      *     Process a CREate request                                            
      *                                                                         
                WHEN WS-RESULT-VERB-CREATE                                      
                    PERFORM CHECK-IF-COLOR-FOUND                                
                    IF WS-RESULT-COLOR-FOUND THEN                               
                        PERFORM OUTPUT-MARBLE-ALREADY-EXISTS                    
                    ELSE                                                        
                        PERFORM INSERT-COLOR                                    
                        IF WS-RESULT-OPERATION-SUCCESS THEN                     
                            PERFORM OUTPUT-SUCCESS                              
                        END-IF                                                  
                    END-IF                                                      
      *                                                                         
      *     Process an UPDate request                                           
      *                                                                         
                WHEN WS-RESULT-VERB-UPDATE                                      
                    PERFORM CHECK-IF-COLOR-FOUND                                
                    IF WS-RESULT-COLOR-FOUND THEN                               
                        PERFORM UPDATE-COLOR                                    
                        IF WS-RESULT-OPERATION-SUCCESS THEN                     
                            PERFORM OUTPUT-SUCCESS                              
                        END-IF                                                  
                    ELSE                                                        
                        PERFORM OUTPUT-MARBLE-DOES-NOT-EXIST                    
                    END-IF                                                      
      *                                                                         
      *     Process a DELete request                                            
      *                                                                         
                WHEN WS-RESULT-VERB-DELETE                                      
                    PERFORM CHECK-IF-COLOR-FOUND                                
                    IF WS-RESULT-COLOR-FOUND THEN                               
                        PERFORM DELETE-COLOR                                    
                        IF WS-RESULT-OPERATION-SUCCESS THEN                     
                            PERFORM OUTPUT-SUCCESS                              
                        END-IF                                                  
                    ELSE                                                        
                        PERFORM OUTPUT-MARBLE-DOES-NOT-EXIST                    
                    END-IF                                                      
                END-EVALUATE                                                    
            END-IF.                                                             
      *                                                                         
      *     Tell the user the result and exit.                                  
      *                                                                         
            PERFORM WRITE-OUTPUT                                                
            GOBACK.                                                             
      * ===============================================================         
      * Initialize working areas                                                
      * ===============================================================         
       INIT-WORK-AREAS.                                                         
      *                                                                         
      *     Set work areas to known values                                      
      *                                                                         
            INITIALIZE SQLCA.                                                   
            MOVE 74 TO WS-MSG-LENGTH.                                           
            MOVE SPACES TO WS-INPUT.                                            
            MOVE SPACES TO WS-OUTPUT.                                           
      * ===============================================================         
      * Get transaction input                                                   
      * ===============================================================         
       GET-TRANS-INPUT.                                                         
      *                                                                         
      *     Receive input from user                                             
      *                                                                         
            EXEC CICS RECEIVE                                                   
                        INTO(WS-CICS-INPUT)                                     
                        LENGTH(WS-MSG-LENGTH)                                   
            END-EXEC.                                                           
      * ===============================================================         
      * Parse the transaction input                                             
      * ===============================================================         
       PARSE-CICS-INPUT.                                                        
            UNSTRING WS-CICS-INPUT DELIMITED BY SPACE                           
                INTO WS-INPUT-TRAN-ID,                                          
                     WS-INPUT-VERB,                                             
                     WS-INPUT-COLOR,                                            
                     WS-INPUT-INV,                                              
      * <-- remove   WS-INPUT-COST,                                             
                     WS-INPUT-TRAILER                                           
            END-UNSTRING.                                                       
      * ===============================================================         
      * Set indicator if verb is invalid                                        
      * ===============================================================         
       VERIFY-VERB.                                                             
      *                                                                         
      *     Validate the request type                                           
      *                                                                         
            IF WS-CONST-CREATE = WS-INPUT-VERB THEN                             
                SET WS-RESULT-VERB-CREATE TO TRUE                               
            ELSE IF WS-CONST-UPDATE = WS-INPUT-VERB THEN                        
                SET WS-RESULT-VERB-UPDATE TO TRUE                               
            ELSE IF WS-CONST-DELETE = WS-INPUT-VERB THEN                        
                SET WS-RESULT-VERB-DELETE TO TRUE                               
            ELSE                                                                
                SET WS-RESULT-VERB-INVALID TO TRUE                              
                PERFORM VERB-DOES-NOT-EXIST                                     
            END-IF.                                                             
      * ===============================================================         
      * Set indicator if color is invalid                                       
      * ===============================================================         
       VERIFY-COLOR.                                                            
      *                                                                         
      *     Validate the color being requested                                  
      *                                                                         
      *                                                                         
            IF EIBTRNID NOT = 'MARB' THEN                                       
                EXEC SQL                                                        
                    SELECT COUNT(*) INTO :WS-WORK-ROW-COUNT                     
                    FROM EVENT.COLOR                                            
                    WHERE COLOR = :WS-INPUT-COLOR                               
                END-EXEC                                                        
                IF WS-WORK-ROW-COUNT = 0 THEN                                   
                    PERFORM COLOR-DOES-NOT-EXIST                                
                ELSE                                                            
                    EXEC SQL                                                    
                        SELECT TRAN INTO :WS-WORK-TRAN                          
                        FROM EVENT.COLOR                                        
                        WHERE COLOR = :WS-INPUT-COLOR                           
                    END-EXEC                                                    
                    IF WS-WORK-TRAN NOT = EIBTRNID                              
                        PERFORM COLOR-IS-RESERVED                               
                    END-IF                                                      
                END-IF                                                          
            END-IF.                                                             
      * ===============================================================         
      * Write transaction response to user                                      
      * ===============================================================         
       WRITE-OUTPUT.                                                            
      *                                                                         
      *     Send response to terminal                                           
      *                                                                         
            EXEC CICS SEND                                                      
                        FROM(WS-OUTPUT)                                         
                        LENGTH(WS-MSG-LENGTH)                                   
                        ERASE                                                   
            END-EXEC.                                                           
      * ===============================================================         
      * Set indicator if input color is found                                   
      * ===============================================================         
       CHECK-IF-COLOR-FOUND.                                                    
      *                                                                         
      *     Get count of rows on input color                                    
      *                                                                         
            EXEC SQL                                                            
                SELECT COUNT(*) INTO :WS-WORK-ROW-COUNT                         
                FROM EVENT.MARBLE                                               
                WHERE COLOR = :WS-INPUT-COLOR                                   
            END-EXEC.                                                           
      *                                                                         
      *     If positive row count, mark "found" indicator                       
      *                                                                         
            IF WS-WORK-ROW-COUNT > 0 THEN                                       
                SET WS-RESULT-COLOR-FOUND TO TRUE                               
            ELSE                                                                
                SET WS-RESULT-COLOR-NOTFOUND TO TRUE                            
            END-IF.                                                             
      * ===============================================================         
      * Move the marble doesn't exist message into the buffer                   
      * ===============================================================         
       VERB-DOES-NOT-EXIST.                                                     
            MOVE 67                       TO WS-MSG-LENGTH.                     
            MOVE WS-ERROR-VERB-DNE        TO WS-OUTPUT-ERROR-CODE.              
            MOVE 'USE CRE|UPD|DEL'        TO WS-OUTPUT-ERROR-MESSAGE.           
            STRING 'Invalid request ('                                          
               DELIMITED BY SIZE                                                
               WS-INPUT-VERB                                                    
               DELIMITED BY SPACE                                               
               ').  Use CREate, UPDate or DELete.'                              
               DELIMITED BY SIZE                                                
               INTO WS-OUTPUT-ERROR-MESSAGE.                                    
      * ===============================================================         
      * Move the marble doesn't exist message into the buffer                   
      * ===============================================================         
       OUTPUT-MARBLE-DOES-NOT-EXIST.                                            
            MOVE 62                       TO WS-MSG-LENGTH.                     
            MOVE WS-ERROR-MARBLE-DNE      TO WS-OUTPUT-ERROR-CODE.              
            STRING 'Color ('                                                    
               DELIMITED BY SIZE                                                
               WS-INPUT-COLOR                                                   
               DELIMITED BY SPACE                                               
               ') not found in inventory, CREate it.'                           
               DELIMITED BY SIZE                                                
               INTO WS-OUTPUT-ERROR-MESSAGE.                                    
      * ===============================================================         
      * Move the marble already exists message into the buffer                  
      * ===============================================================         
       OUTPUT-MARBLE-ALREADY-EXISTS.                                            
            MOVE 64                       TO WS-MSG-LENGTH.                     
            MOVE WS-ERROR-MARBLE-EXISTS   TO WS-OUTPUT-ERROR-CODE.              
            STRING 'Color ('                                                    
               DELIMITED BY SIZE                                                
               WS-INPUT-COLOR                                                   
               DELIMITED BY SPACE                                               
               ') already exists, UPDate or DELete it.'                         
               DELIMITED BY SIZE                                                
               INTO WS-OUTPUT-ERROR-MESSAGE.                                    
      * ===============================================================         
      * Move the color doesn't exist message into the buffer                    
      * ===============================================================         
       COLOR-DOES-NOT-EXIST.                                                    
            SET  WS-RESULT-VERB-INVALID   TO TRUE.                              
            MOVE 45                       TO WS-MSG-LENGTH.                     
            MOVE WS-ERROR-COLOR-DNE       TO WS-OUTPUT-ERROR-CODE.              
            STRING 'Color ('                                                    
               DELIMITED BY SIZE                                                
               WS-INPUT-COLOR                                                   
               DELIMITED BY SPACE                                               
               ') is not available.'                                            
               DELIMITED BY SIZE                                                
               INTO WS-OUTPUT-ERROR-MESSAGE.                                    
      * ===============================================================         
      * Move the color invalid message into the buffer                          
      * ===============================================================         
       COLOR-IS-RESERVED.                                                       
            SET  WS-RESULT-VERB-INVALID   TO TRUE.                              
            MOVE WS-ERROR-COLOR-INVALID TO WS-OUTPUT-ERROR-CODE                 
            IF WS-WORK-TRAN = SPACE                                             
               MOVE 58 TO WS-MSG-LENGTH                                         
               STRING 'Color ('                                                 
                   DELIMITED BY SIZE                                            
                   WS-INPUT-COLOR                                               
                   DELIMITED BY SPACE                                           
                   ') is reserved for the faciliator'                           
                   DELIMITED BY SIZE                                            
                   INTO WS-OUTPUT-ERROR-MESSAGE                                 
            ELSE                                                                
               MOVE 66 TO WS-MSG-LENGTH                                         
               STRING 'Color ('                                                 
                   DELIMITED BY SIZE                                            
                   WS-INPUT-COLOR                                               
                   DELIMITED BY SPACE                                           
                   ') is reserved for another user ('                           
                   'CUST0'                                                      
                   WS-WORK-TRAN (3:2)                                           
                   ')'                                                          
                   DELIMITED BY SIZE                                            
                   INTO WS-OUTPUT-ERROR-MESSAGE.                                
      * ===============================================================         
      * Move the success message into the buffer                                
      * ===============================================================         
       OUTPUT-SUCCESS.                                                          
            MOVE WS-CONST-SUCCESS TO WS-OUTPUT-SUCCESS-TEXT.                    
            IF WS-INPUT-TRAILER = SPACE THEN                                    
                MOVE 7 TO WS-MSG-LENGTH                                         
            ELSE                                                                
                MOVE 74 TO WS-MSG-LENGTH                                        
                STRING                                                          
                    '(EXTRANEOUS ARGUMENT "'                                    
                    DELIMITED BY SIZE                                           
                    WS-INPUT-TRAILER                                            
                    DELIMITED BY SPACE                                          
                    '" WAS IGNORED)'                                            
                    DELIMITED BY SIZE                                           
                    INTO WS-OUTPUT-SUCCESS-WARNING                              
            END-IF.                                                             
      * ===============================================================         
      * Insert color                                                            
      * ===============================================================         
       INSERT-COLOR.                                                            
      *                                                                         
      *    Set current inventory into WS-WORK-INV                               
      *                                                                         
           MOVE WS-INPUT-INV TO WS-WORK-INV                                     
           MOVE WS-INPUT-COST TO WS-WORK-COST                                   
           EXEC SQL                                                             
               INSERT INTO EVENT.MARBLE                                         
               (COLOR,INVENTORY,COST)                                           
               VALUES (                                                         
                     :WS-INPUT-COLOR,                                           
                     :WS-WORK-INV,                                              
                     :WS-WORK-COST)                                             
           END-EXEC                                                             
           SET WS-RESULT-OPERATION-SUCCESS TO TRUE.                             
      * ===============================================================         
      * Update current color                                                    
      * ===============================================================         
       UPDATE-COLOR.                                                            
      *                                                                         
      *    Set current inventory into WS-WORK-INV                               
      *                                                                         
           MOVE WS-INPUT-INV TO WS-WORK-INV                                     
           MOVE WS-INPUT-COST TO WS-WORK-COST                                   
           EXEC SQL                                                             
               UPDATE EVENT.MARBLE                                              
               SET INVENTORY = :WS-WORK-INV,                                    
                   COST = :WS-WORK-COST                                         
               WHERE COLOR = :WS-INPUT-COLOR                                    
           END-EXEC                                                             
           SET WS-RESULT-OPERATION-SUCCESS TO TRUE.                             
      * ===============================================================         
      * Delete color                                                            
      * ===============================================================         
       DELETE-COLOR.                                                            
           EXEC SQL                                                             
               DELETE FROM EVENT.MARBLE                                         
               WHERE COLOR = :WS-INPUT-COLOR                                    
           END-EXEC                                                             
           SET WS-RESULT-OPERATION-SUCCESS TO TRUE.                             
