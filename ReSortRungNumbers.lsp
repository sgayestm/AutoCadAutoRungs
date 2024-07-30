;;--------------=={ ReDoRungs.lsp - RungIndexerForPages }==-------------;;
;;                                                                      ;;  
;;  This program lets you redo the rung numbers for each page by        ;;
;;  Checking the dwg file and only getting all control pages, then      ;;
;;  updating the current rung number when it is on the current page     ;;
;;                                                                      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Matthew Ayestaran										                    	;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2024-07-21                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;


(defun c:ReSortRungNumbers (/ aline file dwg Page obj atts OLD_RUNG cnt s)
  (setq Page 0)
  (setq dwg (getvar "dwgname"))

  ;; Code to read the WDP and extract the control pages numbers
  (setq file (open (ace_getactiveproject) "r"))
  
  (while (setq aline (read-line file))
    ;; Check if it is a DWG title
    (if (wcmatch aline "02*")
      ;; Check for control drawing
      (if (wcmatch aline "*CONTROL*")
        (progn
          (setq Page (+ 1 Page))
          
          ;; Check for current page in count
          (if (wcmatch aline dwg)
            (progn
              ;; Find rung
              (setq s (ssget "_X" '((0 . "INSERT") (66 . 1) (2 . "WD_MLRH"))))
              
              ;; Cycle through selection
              (repeat (setq cnt (sslength s))
                (setq cnt (1- cnt))
                (setq obj (vlax-ename->vla-object (ssname s cnt)))
                (setq atts (vlax-invoke obj 'GetAttributes))
                
                (princ (strcat "current page: " aline "\n"))
                
                ;; Get current rung value
                (setq OLD_RUNG (vla-get-textstring (nth 9 atts)))

                (cond
                  ;; Check if page > 10 and OLD_RUNG ends with "-00"
                  ((and (> Page 10) (wcmatch OLD_RUNG "*-00"))
                    (progn
                      (princ "/n running")
                      (c:wd_renum_ladders (list (list OLD_RUNG (strcat "0" (rtos Page 2 0) "-00"))))
                      (princ "/n ran")
                    )
                  )
                  ;; Check if OLD_RUNG ends with "-39"
                  ((wcmatch OLD_RUNG "*-39")
                    (progn
                      (princ "/n running")
                      (c:wd_renum_ladders (list (list OLD_RUNG (strcat "0" (rtos Page 2 0) "-39"))))
                      (princ "/n ran")
                    )
                  )
                  ;; Default case for OLD_RUNG ending with "-00"
                  ((wcmatch OLD_RUNG "*-00")
                    (progn
                      (princ "/n running")
                      (c:wd_renum_ladders (list (list OLD_RUNG (strcat (rtos Page 2 0) "-00"))))
                      (princ "/n ran")
                    )
                  )
                  ;; Default case for OLD_RUNG ending with "-39"
                  ((wcmatch OLD_RUNG "*-39")
                    (progn
                      (princ "/n running")
                      (c:wd_renum_ladders (list (list OLD_RUNG (strcat (rtos Page 2 0) "-39"))))
                      (princ "/n ran")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (close file)
)