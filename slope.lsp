; FUNCTION TO DETERMINE THE SLOPE OF GRADES.
; ASKS USER TO INPUT FIRST ELEVATION AND SECOND ELEVATION
; AND SELECT TWO POINTS FOR THE DISTANCE BETWEEN THE ELEVATIONS.
; IT WILL AUTOMATICALLY CALCULATE THE SLOPE FOR THE USER
; AND SHOW IT ON THE COMMAND LINE

;Load VLisp extensions
(vl-load-com) 

(defun c:Slope()
  (terpri)

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
  
  ; sets dimension precision to get rid of ending 0's
  ;(setvar 'dimzin 12)
  
  ; Converts real number to string in order to print it to the block
  (setq slopes (rtos slope))
  (princ slope)
  (princ "%")
  (princ "\n")
 
  ; INSERTS SLOPE BLOCK INTO DRAWING WITH CORRECT LAYER 
  	; Sets dimension precision back to 0
	(setvar 'dimzin 0)
	
	(insBlock)
	

  
  (princ)
); END OF SLOPE FUNCTION

;; ============================================================================================================================================

; Rounds slope to 1 decimal

;; ============================================================================================================================================
;; Round Multiple  -  Lee Mac
;; Rounds 'n' to the nearest multiple of 'm'

(defun LM:roundm ( n m )
    (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
)

;; Round  -  Lee Mac
;; Rounds 'n' to the nearest integer

(defun LM:roundto ( n p )
    (LM:roundm n (expt 10.0 (- p)))
)

;; ============================================================================================================================================

; Insert Block Method and Change to specified slope

;; ============================================================================================================================================

(defun insBlock ()
	(command "INSERT" "AZ-SLOPE" pause "" "" "")
	(command "CHPROP" "last" "" "LA" "H - SPOTS" "")
	
	  ; if select all blocks with attributes and named 'AZ-SLOPE'
  (if (setq s1 (ssget "L" '((0 . "INSERT") (2 . "AZ-SLOPE") (66 . 1))))
    ; set the index variable 'i' with the selection set length,
    ; and repeat the number of previous selected blocks
    (repeat (setq i (sslength s1))
      ; get the 'i' ename from the selection set,
      ; sets the variable 'i' reduced by 1 in each loop
      ; and transform the entity to a VLA-object
      (setq obj    (vlax-ename->vla-object (ssname s1 (setq i (1- i))))
            ; sets the 'attlst' variable to nil, to ensure that in
            ; the next loop, is setted with the correct attributes only
            attlst nil
            ; sets the 'attlst' variable with the attributes
            ; in the obj block, as a list with VLA-objects
            attlst (vlax-invoke obj 'GetAttributes)
      )
      ; step thru all attributes from the list
      (foreach att attlst
        ; test if TAG is "DESCRIPTION_*"
        (if (and (wcmatch (strcase (vla-get-TagString att)) "SLOPE")
                 ; and, test if can be modified
                 (vlax-write-enabled-p att)
            )
          ; if true, put an empty string
          (vla-put-TextString att (strcat slopes "%"))		  
        )
      )
    )
  )
)

;; ============================================================================================================================================

; Get and Set Attribute Values 

;; ============================================================================================================================================

;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.

(defun LM:vl-getattributevalue ( blk tag )
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
)

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

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

;; Get Attribute Values  -  Lee Mac
;; Returns an association list of attributes present in the supplied block.
;; blk - [vla] VLA Block Reference Object
;; Returns: [lst] Association list of ((<tag> . <value>) ... )

(defun LM:vl-getattributevalues ( blk )
    (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
)

;; Set Attribute Values  -  Lee Mac
;; Sets attributes with tags found in the association list to their associated values.
;; blk - [vla] VLA Block Reference Object
;; lst - [lst] Association list of ((<tag> . <value>) ... )
;; Returns: nil

(defun LM:vl-setattributevalues ( blk lst / itm )
    (foreach att (vlax-invoke blk 'getattributes)
        (if (setq itm (assoc (vla-get-tagstring att) lst))
            (vla-put-textstring att (cdr itm))
        )
    )
)

;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.

(defun LM:getattributevalue ( blk tag / enx )
    (if (and (setq blk (entnext blk)) (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk))))))
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (cdr (assoc 1 (reverse enx)))
            (LM:getattributevalue blk tag)
        )
    )
)

;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.

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

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil

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

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

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

;; Get Attribute Values  -  Lee Mac
;; Returns an association list of attributes present in the supplied block.
;; blk - [ent] Block (Insert) Entity Name
;; Returns: [lst] Association list of ((<tag> . <value>) ... )

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

;; Get Attribute Values  -  Lee Mac
;; Returns an association list of attributes present in the supplied block.
;; blk - [ent] Block (Insert) Entity Name
;; Returns: [lst] Association list of ((<tag> . <value>) ... )

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

;; Set Attribute Values  -  Lee Mac
;; Sets attributes with tags found in the association list to their associated values.
;; blk - [ent] Block (Insert) Entity Name
;; lst - [lst] Association list of ((<tag> . <value>) ... )
;; Returns: nil

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

;; Set Attribute Values  -  Lee Mac
;; Sets attributes with tags found in the association list to their associated values.
;; blk - [ent] Block (Insert) Entity Name
;; lst - [lst] Association list of ((<tag> . <value>) ... )
;; Returns: nil

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


