; FUNCTION TO DETERMINE THE SLOPE OF GRADES.
; ASKS USER TO INPUT FIRST ELEVATION AND SECOND ELEVATION
; AND SELECT TWO POINTS FOR THE DISTANCE BETWEEN THE ELEVATIONS.
; IT WILL AUTOMATICALLY CALCULATE THE SLOPE FOR THE USER
; AND SHOW IT ON THE COMMAND LINE

;; ============================================================================================================================================

; MAIN METHOD FUNCTION SLOPE()

;; ============================================================================================================================================

(defun c:Slope()
;LOAD VLISP EXTENSIONS
(vl-load-com) 

  (terpri)

;; ============================================================================================================================================

; SLOPE METHOD THAT CALCULATES SLOPE 

;; ============================================================================================================================================

  ; GETS FIRST ELEVATION FROM USER BY ASKING THEM TO SELECT ELEVATION BLOCK.
  (setq elevationBlock1 (ssget ":S:E" '((0 . "INSERT") (2 . "AZ-SPOT") (66 . 1))))
  (setq firstElevation (LM:GetAttributeValue (ssname elevationBlock1 0) "SPOT"))
  (setq firstElevation (atof firstElevation))
  
  ; GETS SECOND ELEVATION FROM USER ASKING THEM TO SELECT ELEVATION BLOCK.
  (setq elevationBlock2 (ssget ":S:E" '((0 . "INSERT") (2 . "AZ-SPOT") (66 . 1))))
  (setq secondElevation (LM:GetAttributeValue (ssname elevationBlock2 0) "SPOT"))
  (setq secondElevation (atof secondElevation))

  ; SUBTRACTS FIRST ELEVATION FROM SECOND ELEVATION
  ; TAKES THE ABSOLUTE VALUE SO MAKE HIGH OR LOW IRRELEVANT
  (setq totalElevation (abs(- firstElevation secondElevation)))  

  ; GETS DISTANCE BETWEEN TWO POINTS
  (setq dist (getdist "Pick distance"))
 
  ; ALGORITHM TO DETERMINE SLOPE PERCENTAGE AND PRINT IT TO SCREEN
  (setq slope (* 100 (/ totalElevation dist)))
  
  ; IF SLOPE % IS GREATER THAN OR EQUAL TO 10, ROUND SLOPE TO NEAREST WHOLE NUMBER
  ; IF SLOPE % IS LESS THAN 10, ROUND SLOPE TO NEAREST TENTH
  (if (>= slope 9.9)
	(setq slope (rtos slope 2 0))
	(setq slope (rtos slope 2 1)))
	
;; ============================================================================================================================================

; INSERT BLOCK METHOD AND CHANGE TO SPECIFIED SLOPE

;; ============================================================================================================================================

	; INSERT 'AZ-SLOPE' BLOCK AND USER PLACES BLOCK WHERE THEY WANT. SCALE IS SET TO 0.8
	(command "_INSERT" "AZ-SLOPE-TEST" pause 1 "" "")
	; CREATS NEW LAYER 'H - SPOTS' IF IT DOESN'T ALREADY EXISTS
	(command "_.-Layer" "_m" "H - SPOTS" "_Color" "1" "" "")
	; CHANGES LAYER OF 'AZ-SLOPE' TO 'H - SPOTS'
	(command "CHPROP" "last" "" "LA" "H - SPOTS" "")
	
	(princ "The Slope is: ")
	(princ slope)
	(princ "%\n")
	
  ; IF SELECT ALL BLOCKS WITH ATTRIBUTES AND NAMED 'AZ-SLOPE'
  (if (setq s1 (ssget "L" '((0 . "INSERT") (2 . "AZ-SLOPE-TEST") (66 . 1))))
    ; SET THE INDEX VARIABLE 'I' WITH THE SELECTION SET LENGTH,
    ; AND REPEAT THE NUMBER OF PREVIOUS SELECTED BLOCKS
    (repeat (setq i (sslength s1))
      ; GET THE 'I' ENAME FROM THE SELECTION SET,
      ; SETS THE VARIABLE 'I' REDUCED BY 1 IN EACH LOOP
      ; AND TRANSFORM THE ENTITY TO A VLA-OBJECT
      (setq obj    (vlax-ename->vla-object (ssname s1 (setq i (1- i))))
            ; SETS THE 'ATTLST' VARIABLE TO NIL, TO ENSURE THAT IN
            ; THE NEXT LOOP, IS SETTED WITH THE CORRECT ATTRIBUTES ONLY
            attlst nil
            ; SETS THE 'ATTLST' VARIABLE WITH THE ATTRIBUTES
            ; IN THE OBJ BLOCK, AS A LIST WITH VLA-OBJECTS
            attlst (vlax-invoke obj 'GetAttributes)
      )
      ; STEP THRU ALL ATTRIBUTES FROM THE LIST
      (foreach att attlst
        ; TEST IF TAG IS "SLOPE"
        (if (and (wcmatch (strcase (vla-get-TagString att)) "SLOPE")
                 ; AND, TEST IF CAN BE MODIFIED
                 (vlax-write-enabled-p att)
            )
          ; IF TRUE, PUT AN EMPTY STRING
          (vla-put-TextString att (strcat slope "%"))		  
        )
      )
    )
  )	
  (princ)
); END OF SLOPE() FUNCTION

(defun c:Slope2()
	;LOAD VLISP EXTENSIONS
	(vl-load-com) 
	(terpri)

;; ============================================================================================================================================

; SLOPE METHOD THAT CALCULATES SLOPE 

;; ============================================================================================================================================

  ; GETS FIRST ELEVATION FROM USER BY ASKING THEM TO SELECT ELEVATION BLOCK.
  (setq elevationBlock1 (ssget ":S:E" '((0 . "INSERT") (2 . "AZ-SPOT") (66 . 1))))
  (setq firstElevation (LM:GetAttributeValue (ssname elevationBlock1 0) "SPOT"))
  (setq firstElevation (atof firstElevation))
  
  ; GETS SECOND ELEVATION FROM USER ASKING THEM TO SELECT ELEVATION BLOCK.
  (setq elevationBlock2 (ssget ":S:E" '((0 . "INSERT") (2 . "AZ-SPOT") (66 . 1))))
  (setq secondElevation (LM:GetAttributeValue (ssname elevationBlock2 0) "SPOT"))
  (setq secondElevation (atof secondElevation))

  ; SUBTRACTS FIRST ELEVATION FROM SECOND ELEVATION
  ; TAKES THE ABSOLUTE VALUE SO MAKE HIGH OR LOW IRRELEVANT
  (setq totalElevation (abs(- firstElevation secondElevation)))  

  ; GETS DISTANCE BETWEEN TWO POINTS
  (setq dist (getdist "Pick distance"))
 
  ; ALGORITHM TO DETERMINE SLOPE PERCENTAGE AND PRINT IT TO SCREEN
  (setq slope (* 100 (/ totalElevation dist)))
  
  ; IF SLOPE % IS GREATER THAN OR EQUAL TO 10, ROUND SLOPE TO NEAREST WHOLE NUMBER
  ; IF SLOPE % IS LESS THAN 10, ROUND SLOPE TO NEAREST TENTH
  (if (>= slope 9.9)
	(setq slope (rtos slope 2 0))
	(setq slope (rtos slope 2 1)))
	
;; ============================================================================================================================================

; INSERT BLOCK METHOD AND CHANGE TO SPECIFIED SLOPE

;; ============================================================================================================================================

	; INSERT 'AZ-SLOPE' BLOCK AND USER PLACES BLOCK WHERE THEY WANT. SCALE IS SET TO 0.8
	;(command "_INSERT" "AZ-SLOPE-TEST" pause 1 "" "")
	; CREATS NEW LAYER 'H - SPOTS' IF IT DOESN'T ALREADY EXISTS
	;(command "_.-Layer" "_m" "H - SPOTS" "_Color" "1" "" "")
	; CHANGES LAYER OF 'AZ-SLOPE' TO 'H - SPOTS'
	;(command "CHPROP" "last" "" "LA" "H - SPOTS" "")
	
	(princ "The Slope is: ")
	(princ slope)
	(princ "%\n")
	
  ; IF SELECT ALL BLOCKS WITH ATTRIBUTES AND NAMED 'AZ-SLOPE'
  (if (setq s1 (ssget ":S:E" '((0 . "INSERT") (2 . "AZ-SLOPE-TEST") (66 . 1))))
    ; SET THE INDEX VARIABLE 'I' WITH THE SELECTION SET LENGTH,
    ; AND REPEAT THE NUMBER OF PREVIOUS SELECTED BLOCKS
    (repeat (setq i (sslength s1))
      ; GET THE 'I' ENAME FROM THE SELECTION SET,
      ; SETS THE VARIABLE 'I' REDUCED BY 1 IN EACH LOOP
      ; AND TRANSFORM THE ENTITY TO A VLA-OBJECT
      (setq obj    (vlax-ename->vla-object (ssname s1 (setq i (1- i))))
            ; SETS THE 'ATTLST' VARIABLE TO NIL, TO ENSURE THAT IN
            ; THE NEXT LOOP, IS SETTED WITH THE CORRECT ATTRIBUTES ONLY
            attlst nil
            ; SETS THE 'ATTLST' VARIABLE WITH THE ATTRIBUTES
            ; IN THE OBJ BLOCK, AS A LIST WITH VLA-OBJECTS
            attlst (vlax-invoke obj 'GetAttributes)
      )
      ; STEP THRU ALL ATTRIBUTES FROM THE LIST
      (foreach att attlst
        ; TEST IF TAG IS "SLOPE"
        (if (and (wcmatch (strcase (vla-get-TagString att)) "SLOPE")
                 ; AND, TEST IF CAN BE MODIFIED
                 (vlax-write-enabled-p att)
            )
          ; IF TRUE, PUT AN EMPTY STRING
          (vla-put-TextString att (strcat slope "%"))  
        )
      )
    )
  )	
  (princ)
); END OF SLOPE() FUNCTION


(defun c:Slope3()
	;LOAD VLISP EXTENSIONS
	(vl-load-com) 
	(terpri)

;; ============================================================================================================================================

; SLOPE METHOD THAT CALCULATES SLOPE 

;; ============================================================================================================================================

  ; GETS FIRST ELEVATION FROM USER BY ASKING THEM TO SELECT ELEVATION BLOCK.
  (setq firstElevation (getdist "\nEnter First Elevation"))
  
  ; GETS SECOND ELEVATION FROM USER ASKING THEM TO SELECT ELEVATION BLOCK.
  (setq secondElevation (getdist "\nEnter Second Elevation"))

  ; SUBTRACTS FIRST ELEVATION FROM SECOND ELEVATION
  ; TAKES THE ABSOLUTE VALUE SO MAKE HIGH OR LOW IRRELEVANT
  (setq totalElevation (abs(- firstElevation secondElevation)))  

  ; GETS DISTANCE BETWEEN TWO POINTS
  (setq dist (getdist "Pick distance"))
 
  ; ALGORITHM TO DETERMINE SLOPE PERCENTAGE AND PRINT IT TO SCREEN
  (setq slope (* 100 (/ totalElevation dist)))
  
  ; IF SLOPE % IS GREATER THAN OR EQUAL TO 10, ROUND SLOPE TO NEAREST WHOLE NUMBER
  ; IF SLOPE % IS LESS THAN 10, ROUND SLOPE TO NEAREST TENTH
  (if (>= slope 9.9)
	(setq slope (rtos slope 2 0))
	(setq slope (rtos slope 2 1)))
	
;; ============================================================================================================================================

; INSERT BLOCK METHOD AND CHANGE TO SPECIFIED SLOPE

;; ============================================================================================================================================

	; INSERT 'AZ-SLOPE' BLOCK AND USER PLACES BLOCK WHERE THEY WANT. SCALE IS SET TO 0.8
	;(command "_INSERT" "AZ-SLOPE-TEST" pause 1 "" "")
	; CREATS NEW LAYER 'H - SPOTS' IF IT DOESN'T ALREADY EXISTS
	;(command "_.-Layer" "_m" "H - SPOTS" "_Color" "1" "" "")
	; CHANGES LAYER OF 'AZ-SLOPE' TO 'H - SPOTS'
	;(command "CHPROP" "last" "" "LA" "H - SPOTS" "")
	
	(princ "The Slope is: ")
	(princ slope)
	(princ "%\n")
	
  ; IF SELECT ALL BLOCKS WITH ATTRIBUTES AND NAMED 'AZ-SLOPE'
  (if (setq s1 (ssget ":S:E" '((0 . "INSERT") (2 . "AZ-SLOPE-TEST") (66 . 1))))
    ; SET THE INDEX VARIABLE 'I' WITH THE SELECTION SET LENGTH,
    ; AND REPEAT THE NUMBER OF PREVIOUS SELECTED BLOCKS
    (repeat (setq i (sslength s1))
      ; GET THE 'I' ENAME FROM THE SELECTION SET,
      ; SETS THE VARIABLE 'I' REDUCED BY 1 IN EACH LOOP
      ; AND TRANSFORM THE ENTITY TO A VLA-OBJECT
      (setq obj    (vlax-ename->vla-object (ssname s1 (setq i (1- i))))
            ; SETS THE 'ATTLST' VARIABLE TO NIL, TO ENSURE THAT IN
            ; THE NEXT LOOP, IS SETTED WITH THE CORRECT ATTRIBUTES ONLY
            attlst nil
            ; SETS THE 'ATTLST' VARIABLE WITH THE ATTRIBUTES
            ; IN THE OBJ BLOCK, AS A LIST WITH VLA-OBJECTS
            attlst (vlax-invoke obj 'GetAttributes)
      )
      ; STEP THRU ALL ATTRIBUTES FROM THE LIST
      (foreach att attlst
        ; TEST IF TAG IS "SLOPE"
        (if (and (wcmatch (strcase (vla-get-TagString att)) "SLOPE")
                 ; AND, TEST IF CAN BE MODIFIED
                 (vlax-write-enabled-p att)
            )
          ; IF TRUE, PUT AN EMPTY STRING
          (vla-put-TextString att (strcat slope "%"))  
        )
      )
    )
  )	
  (princ)
); END OF SLOPE() FUNCTION







;; ============================================================================================================================================

; GET AND SET ATTRIBUTE VALUES 

;; ============================================================================================================================================

;; GET ATTRIBUTE VALUE  -  LEE MAC
;; RETURNS THE VALUE HELD BY THE SPECIFIED TAG WITHIN THE SUPPLIED BLOCK, IF PRESENT.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; TAG - [STR] ATTRIBUTE TAGSTRING
;; RETURNS: [STR] ATTRIBUTE VALUE, ELSE NIL IF TAG IS NOT FOUND.

(defun LM:getattributevalue ( blk tag / val enx )
    (while
        (and
            (null val)
            (setq blk (entnext blk))
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk)))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (setq val (cdr (assoc 1 (reverse enx))))
        )
    )
)


