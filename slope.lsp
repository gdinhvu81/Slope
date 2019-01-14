; FUNCTION TO DETERMINE THE SLOPE OF GRADES.
; ASKS USER TO INPUT FIRST ELEVATION AND SECOND ELEVATION
; AND SELECT TWO POINTS FOR THE DISTANCE BETWEEN THE ELEVATIONS.
; IT WILL AUTOMATICALLY CALCULATE THE SLOPE FOR THE USER
; AND SHOW IT ON THE COMMAND LINE

;LOAD VLISP EXTENSIONS
(vl-load-com) 

;; ============================================================================================================================================

; MAIN METHOD FUNCTION SLOPE()

;; ============================================================================================================================================
(defun c:Slope()
  (terpri)
  
  ; SETS DIMENSION PRECISION TO GET RID OF ENDING 0'S
  ;(setvar 'dimzin 12)
 
  ; Sets dimension precision back to 0
  ;(setvar 'dimzin 0)
	
  ; CALLS THE INSERT BLOCK METHOD TO INSERT BLOCK WITH CORRECT LAYER
  (insBlock)
 
  (princ)
); END OF SLOPE FUNCTION

;; ============================================================================================================================================

; SLOPE METHOD THAT CALCULATES SLOPE 

;; ============================================================================================================================================
(defun slope()
  ; GETS FIRST ELEVATION FROM USER.
  ; CAN'T BE NEGATIVE 
  (initget (+ 1 4))
  (setq firstElevation (getreal "\nEnter First Elevation: "))

  ; GETS SECOND ELEVATION FROM USER.
  ; CAN'T BE NEGATIVE 
  (initget (+ 1 4))
  (setq secondElevation (getreal "\nEnter Second Elevation: "))

  ; SUBTRACTS FIRST ELEVATION FROM SECOND ELEVATION
  ; TAKES THE ABSOLUTE VALUE SO MAKE HIGH OR LOW IRRELEVANT
  (setq totalElevation (abs(- firstElevation secondElevation)))  

  ; GETS DISTANCE BETWEEN TWO POINTS
  (setq startingPoint (getpoint "\nPick Starting Point:"))
  (setq endingPoint (getpoint "\nPick Ending Point:"))
  (setq d (distance startingPoint endingPoint))

  (terpri)
  
  ; ALGORITHM TO DETERMINE SLOPE PERCENTAGE AND PRINT IT TO SCREEN
  (setq slope (LM:roundto (* 100 (/ totalElevation d)) 1))
)

;; ============================================================================================================================================

; INSERT BLOCK METHOD AND CHANGE TO SPECIFIED SLOPE

;; ============================================================================================================================================
(defun insBlock ()
	; CONVERTS REAL NUMBER TO STRING IN ORDER TO PRINT IT TO THE BLOCK
	(setq slopes (rtos (slope)))
	(princ "The Slope is: ")
	(princ slopes)
	(princ "%\n")
	; INSERTS BLOCK AND PLACED WHERE USER WANTS WITH A SCALE OF 0.8
	(command "INSERT" "AZ-SLOPE" pause 0.8 "" "")
	; CREATES A LAYER H - SPOTS IF IT DOESN'T EXIST 
	(command "_.-Layer" "_m" "H - SPOTS" "_Color" "1" "" "")
	; CHANGES BLOCK TO H - SPOTS LAYER 
	(command "CHPROP" "last" "" "LA" "H - SPOTS" "")
	
  ; IF SELECT ALL BLOCKS WITH ATTRIBUTES AND NAMED 'AZ-SLOPE'
  (if (setq s1 (ssget "L" '((0 . "INSERT") (2 . "AZ-SLOPE") (66 . 1))))
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
        ; TEST IF TAG IS "DESCRIPTION_*"
        (if (and (wcmatch (strcase (vla-get-TagString att)) "SLOPE")
                 ; AND, TEST IF CAN BE MODIFIED
                 (vlax-write-enabled-p att)
            )
          ; IF TRUE, PUT AN EMPTY STRING
          (vla-put-TextString att (strcat slopes "%"))		  
        )
      )
    )
  )
)

;; ============================================================================================================================================

; ROUNDS REAL NUMBERS TO CERTAIN DECIMAL POINT

;; ============================================================================================================================================
;; ROUND MULTIPLE  -  LEE MAC
;; ROUNDS 'N' TO THE NEAREST MULTIPLE OF 'M'
(defun LM:roundm ( n m )
    (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
)

;; ROUND  -  LEE MAC
;; ROUNDS 'N' TO THE NEAREST INTEGER
(defun LM:roundto ( n p )
    (LM:roundm n (expt 10.0 (- p)))
)

;; ============================================================================================================================================

; GET AND SET ATTRIBUTE VALUES 

;; ============================================================================================================================================

;; GET ATTRIBUTE VALUE  -  LEE MAC
;; RETURNS THE VALUE HELD BY THE SPECIFIED TAG WITHIN THE SUPPLIED BLOCK, IF PRESENT.
;; BLK - [VLA] VLA BLOCK REFERENCE OBJECT
;; TAG - [STR] ATTRIBUTE TAGSTRING
;; RETURNS: [STR] ATTRIBUTE VALUE, ELSE NIL IF TAG IS NOT FOUND.

(defun LM:vl-getattributevalue ( blk tag )
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
)

;; SET ATTRIBUTE VALUE  -  LEE MAC
;; SETS THE VALUE OF THE FIRST ATTRIBUTE WITH THE GIVEN TAG FOUND WITHIN THE BLOCK, IF PRESENT.
;; BLK - [VLA] VLA BLOCK REFERENCE OBJECT
;; TAG - [STR] ATTRIBUTE TAGSTRING
;; VAL - [STR] ATTRIBUTE VALUE
;; RETURNS: [STR] ATTRIBUTE VALUE IF SUCCESSFUL, ELSE NIL.

(defun LM:vl-setattributevalue ( blk tag val )
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            )
        )
        (vlax-invoke blk 'getattributes)
    )
)

;; GET ATTRIBUTE VALUES  -  LEE MAC
;; RETURNS AN ASSOCIATION LIST OF ATTRIBUTES PRESENT IN THE SUPPLIED BLOCK.
;; BLK - [VLA] VLA BLOCK REFERENCE OBJECT
;; RETURNS: [LST] ASSOCIATION LIST OF ((<TAG> . <VALUE>) ... )

(defun LM:vl-getattributevalues ( blk )
    (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
)

;; SET ATTRIBUTE VALUES  -  LEE MAC
;; SETS ATTRIBUTES WITH TAGS FOUND IN THE ASSOCIATION LIST TO THEIR ASSOCIATED VALUES.
;; BLK - [VLA] VLA BLOCK REFERENCE OBJECT
;; LST - [LST] ASSOCIATION LIST OF ((<TAG> . <VALUE>) ... )
;; RETURNS: NIL

(defun LM:vl-setattributevalues ( blk lst / itm )
    (foreach att (vlax-invoke blk 'getattributes)
        (if (setq itm (assoc (vla-get-tagstring att) lst))
            (vla-put-textstring att (cdr itm))
        )
    )
)

;; GET ATTRIBUTE VALUE  -  LEE MAC
;; RETURNS THE VALUE HELD BY THE SPECIFIED TAG WITHIN THE SUPPLIED BLOCK, IF PRESENT.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; TAG - [STR] ATTRIBUTE TAGSTRING
;; RETURNS: [STR] ATTRIBUTE VALUE, ELSE NIL IF TAG IS NOT FOUND.

(defun LM:getattributevalue ( blk tag / enx )
    (if (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (cdr (assoc 1 (reverse enx)))
            (LM:getattributevalue blk tag)
        )
    )
)

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

;; SET ATTRIBUTE VALUE  -  LEE MAC
;; SETS THE VALUE OF THE FIRST ATTRIBUTE WITH THE GIVEN TAG FOUND WITHIN THE BLOCK, IF PRESENT.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; TAG - [STR] ATTRIBUTE TAGSTRING
;; VAL - [STR] ATTRIBUTE VALUE
;; RETURNS: [STR] ATTRIBUTE VALUE IF SUCCESSFUL, ELSE NIL

(defun LM:setattributevalue ( blk tag val / enx )
    (if (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (if (entmod (subst (cons 1 val) (assoc 1 (reverse enx)) enx))
                (progn
                    (entupd blk)
                    val
                )
            )
            (LM:setattributevalue blk tag val)
        )
    )
)

;; SET ATTRIBUTE VALUE  -  LEE MAC
;; SETS THE VALUE OF THE FIRST ATTRIBUTE WITH THE GIVEN TAG FOUND WITHIN THE BLOCK, IF PRESENT.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; TAG - [STR] ATTRIBUTE TAGSTRING
;; VAL - [STR] ATTRIBUTE VALUE
;; RETURNS: [STR] ATTRIBUTE VALUE IF SUCCESSFUL, ELSE NIL.

(defun LM:setattributevalue ( blk tag val / end enx )
    (while
        (and
            (null end)
            (setq blk (entnext blk))
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk)))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (if (entmod (subst (cons 1 val) (assoc 1 (reverse enx)) enx))
                (progn
                    (entupd blk)
                    (setq end val)
                )
            )
        )
    )
)

;; GET ATTRIBUTE VALUES  -  LEE MAC
;; RETURNS AN ASSOCIATION LIST OF ATTRIBUTES PRESENT IN THE SUPPLIED BLOCK.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; RETURNS: [LST] ASSOCIATION LIST OF ((<TAG> . <VALUE>) ... )

(defun LM:getattributevalues ( blk / enx )
    (if (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (cons
            (cons
                (cdr (assoc 2 enx))
                (cdr (assoc 1 (reverse enx)))
            )
            (LM:getattributevalues blk)
        )
    )
)

;; GET ATTRIBUTE VALUES  -  LEE MAC
;; RETURNS AN ASSOCIATION LIST OF ATTRIBUTES PRESENT IN THE SUPPLIED BLOCK.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; RETURNS: [LST] ASSOCIATION LIST OF ((<TAG> . <VALUE>) ... )

(defun LM:getattributevalues ( blk / enx lst )
    (while (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (setq lst
            (cons
                (cons
                    (cdr (assoc 2 enx))
                    (cdr (assoc 1 (reverse enx)))
                )
                lst
            )
        )
    )
    (reverse lst)
)

;; SET ATTRIBUTE VALUES  -  LEE MAC
;; SETS ATTRIBUTES WITH TAGS FOUND IN THE ASSOCIATION LIST TO THEIR ASSOCIATED VALUES.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; LST - [LST] ASSOCIATION LIST OF ((<TAG> . <VALUE>) ... )
;; RETURNS: NIL

(defun LM:setattributevalues ( blk lst / enx itm )
    (if (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (if (setq itm (assoc (cdr (assoc 2 enx)) lst))
            (progn
                (if (entmod (subst (cons 1 (cdr itm)) (assoc 1 (reverse enx)) enx))
                    (entupd blk)
                )
                (LM:setattributevalues blk lst)
            )
            (LM:setattributevalues blk lst)
        )
    )
)

;; SET ATTRIBUTE VALUES  -  LEE MAC
;; SETS ATTRIBUTES WITH TAGS FOUND IN THE ASSOCIATION LIST TO THEIR ASSOCIATED VALUES.
;; BLK - [ENT] BLOCK (INSERT) ENTITY NAME
;; LST - [LST] ASSOCIATION LIST OF ((<TAG> . <VALUE>) ... )
;; RETURNS: NIL

(defun LM:setattributevalues ( blk lst / enx itm )
    (while (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (if
            (and
                (setq itm (assoc (cdr (assoc 2 enx)) lst))
                (entmod (subst (cons 1 (cdr itm)) (assoc 1 (reverse enx)) enx))
            )
            (entupd blk)
        )
    )
    nil
)


