(defun clear-regapps ()
  (vl-load-com)
  (setq cnt 0)
  (setq regapp nil)
  (setq first T)
  (while
    (progn
      (setq regapp (vl-catch-all-apply 'tblnext (list "regapp" first)))
      (setq first nil)
      (and regapp (not (vl-catch-all-error-p regapp)))
    )
    (setq name (cdr (assoc 2 regapp)))
    (if (and name
             (/= name "ACAD")
             (/= name "ACADANNOTATIONSCALE"))
      (progn
        (setq regent (vl-catch-all-apply 'tblobjname (list "regapp" name)))
        (if (and regent (not (vl-catch-all-error-p regent)))
          (entdel regent)
        )
        (setq cnt (1+ cnt))
      )
    )
  )
  cnt
)

(defun c:SUPERCLEAN (/ acadObj doc blocks xrefCnt imgDict imgCnt ss oleCnt imgEntCnt regCnt
                          layTbl layName layData layStates layObj)

  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; 1. 记录并解锁/解冻/打开所有图层
  (setq layTbl (vla-get-layers doc))
  (setq layStates '())
  (vlax-for lay layTbl
    (setq layName (vla-get-name lay))
    (setq layStates (cons (list layName
                                (vla-get-lock lay)
                                (vla-get-freeze lay)
                                (vla-get-layeron lay)) layStates))
    (vl-catch-all-apply 'vla-put-lock (list lay :vlax-false))
    (vl-catch-all-apply 'vla-put-freeze (list lay :vlax-false))
    (vl-catch-all-apply 'vla-put-layeron (list lay :vlax-true))
  )

  ;; 2. 清理所有外部参照（XREF），包括未绑定的
  (setq blocks (vla-get-blocks doc))
  (setq xrefCnt 0)
  (vlax-for blk blocks
    (if (and
          (= (vla-get-IsXref blk) :vlax-true)
          (not (vla-get-IsLayout blk))
        )
      (progn
        (vl-catch-all-apply 'vla-delete (list blk))
        (setq xrefCnt (1+ xrefCnt))
      )
    )
  )

  ;; 3. 清理所有光栅图像实体（IMAGE），包括块内递归
  (defun del-image-in-blocks (blk)
    (vlax-for ent blk
      (cond
        ((= (vla-get-objectname ent) "AcDbRasterImage")
         (vl-catch-all-apply 'vla-delete (list ent))
         (setq imgEntCnt (1+ imgEntCnt)))
        ((= (vla-get-objectname ent) "AcDbBlockReference")
         (setq blkname (vla-get-name ent))
         (setq try-ref (vl-catch-all-apply 'vla-item (list blocks blkname)))
         (if (and try-ref (not (vl-catch-all-error-p try-ref)))
           (del-image-in-blocks try-ref)
         )
        )
      )
    )
  )
  (setq imgEntCnt 0)
  (vlax-for blk blocks
    (del-image-in-blocks blk)
  )

  ;; 4. 清理所有图片参照（IMAGE字典）
  (setq imgCnt 0)
  (setq imgDict nil)
  (setq imgDictRes (vl-catch-all-apply '(lambda () (vla-GetDictionary doc "ACAD_IMAGE_DICT")) nil))
  (if (and imgDictRes (not (vl-catch-all-error-p imgDictRes)))
    (progn
      (setq imgDict imgDictRes)
      (vlax-for item imgDict
        (vl-catch-all-apply 'vla-remove (list imgDict (vla-get-name item)))
        (setq imgCnt (1+ imgCnt))
      )
    )
  )

  ;; 5. 清理所有OLE对象，包括块内递归
  (defun del-ole-in-blocks (blk)
    (vlax-for ent blk
      (cond
        ((= (vla-get-objectname ent) "AcDbOle2Frame")
         (vl-catch-all-apply 'vla-delete (list ent))
         (setq oleCnt (1+ oleCnt)))
        ((= (vla-get-objectname ent) "AcDbBlockReference")
         (setq blkname (vla-get-name ent))
         (setq try-ref (vl-catch-all-apply 'vla-item (list blocks blkname)))
         (if (and try-ref (not (vl-catch-all-error-p try-ref)))
           (del-ole-in-blocks try-ref)
         )
        )
      )
    )
  )
  (setq oleCnt 0)
  (vlax-for blk blocks
    (del-ole-in-blocks blk)
  )

  ;; 6. 清理注册应用程序（Regapps）
  (setq regCnt (clear-regapps))

  ;; 7. 恢复原有图层状态（跳过不存在图层，并全程异常捕获）
  (foreach lay layStates
    (setq layName (car lay))
    (setq layObj (vl-catch-all-apply (function (lambda () (vla-item layTbl layName))) nil))
    (if (and layObj (not (vl-catch-all-error-p layObj)))
      (progn
        (vl-catch-all-apply 'vla-put-lock (list layObj (cadr lay)))
        (vl-catch-all-apply 'vla-put-freeze (list layObj (caddr lay)))
        (vl-catch-all-apply 'vla-put-layeron (list layObj (cadddr lay)))
      )
    )
  )

  (princ (strcat "\n清理完成：XREF=" (itoa xrefCnt)
                 "，光栅实体=" (itoa imgEntCnt)
                 "，图片字典=" (itoa imgCnt)
                 "，OLE=" (itoa oleCnt)
                 "，Regapps=" (itoa regCnt)))
  (princ)
)
