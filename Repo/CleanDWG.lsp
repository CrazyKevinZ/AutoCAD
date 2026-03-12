;;; ==========================================================================
;;; 插件名称: CLEANDWG (深度图纸清理工具 - 强制 Overkill 版)
;;; 功能描述: 自动清理外部参照、光栅图像、OLE对象、PDF底图，清理无用块参照并消除重复线条
;;; 快捷命令: CLEANDWG 
;;; ==========================================================================

(defun c:CLEANDWG ( / old_echo old_mutt image_ss ole_ss pdf_ss ss ss_after 
                    img_cnt ole_cnt pdf_cnt xref_cnt purge_cnt overkill_cnt 
                    blk tbl item purge_before purge_after ent_before ent_after overkill_str err)
  (vl-load-com)
  
  ;; 备份并设置系统变量（静默模式）
  (setq old_echo (getvar "CMDECHO"))
  (setq old_mutt (getvar "NOMUTT"))
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)

  ;; 初始化所有计数器
  (setq img_cnt 0  ole_cnt 0  pdf_cnt 0  xref_cnt 0  purge_cnt 0  overkill_cnt 0)

  (princ "\n=============================================")
  (princ "\n  正在后台深度清理图纸，请稍候...")

  ;; 1. 解锁所有图层
  (vl-catch-all-apply '(lambda () (vl-cmdf "_.-layer" "_unlock" "*" "")))

  ;; 2. 统计并删除光栅图像和照片
  (setq image_ss (ssget "X" '((0 . "IMAGE"))))
  (if image_ss
    (progn
      (setq img_cnt (sslength image_ss))
      (vl-catch-all-apply '(lambda () (vl-cmdf "_.ERASE" image_ss "")))
    )
  )
  (vl-catch-all-apply '(lambda () (vl-cmdf "_.-image" "_detach" "*")))

  ;; 3. 统计并删除 OLE 对象
  (setq ole_ss (ssget "X" '((0 . "OLE2FRAME"))))
  (if ole_ss
    (progn
      (setq ole_cnt (sslength ole_ss))
      (vl-catch-all-apply '(lambda () (vl-cmdf "_.ERASE" ole_ss "")))
    )
  )

  ;; 4. 统计并删除 PDF/DWF/DGN 参考底图
  (setq pdf_ss (ssget "X" '((0 . "PDFREFERENCE,DWFREFERENCE,DGNREFERENCE"))))
  (if pdf_ss
    (progn
      (setq pdf_cnt (sslength pdf_ss))
      (vl-catch-all-apply '(lambda () (vl-cmdf "_.ERASE" pdf_ss "")))
    )
  )

  ;; 5. 统计并处理外部参照 (XREF)
  (setq blk (tblnext "BLOCK" T))
  (while blk
    (if (= (logand (cdr (assoc 70 blk)) 4) 4)
      (setq xref_cnt (1+ xref_cnt))
    )
    (setq blk (tblnext "BLOCK"))
  )
  (vl-catch-all-apply '(lambda () (vl-cmdf "_.-xref" "_detach" "*")))

  ;; 6. 统计并清理无用块参照及冗余数据
  (setq purge_before 0)
  (foreach tbl '("LAYER" "LTYPE" "APPID" "DIMSTYLE" "STYLE" "BLOCK")
    (setq item (tblnext tbl T))
    (while item
      (setq purge_before (1+ purge_before))
      (setq item (tblnext tbl))
    )
  )
  
  (repeat 3
    (vl-catch-all-apply '(lambda () (vl-cmdf "_.-PURGE" "_A" "*" "_N")))
    (vl-catch-all-apply '(lambda () (vl-cmdf "_.-PURGE" "_R" "*" "_N")))
  )
  
  (setq purge_after 0)
  (foreach tbl '("LAYER" "LTYPE" "APPID" "DIMSTYLE" "STYLE" "BLOCK")
    (setq item (tblnext tbl T))
    (while item
      (setq purge_after (1+ purge_after))
      (setq item (tblnext tbl))
    )
  )
  (setq purge_cnt (- purge_before purge_after))
  (if (< purge_cnt 0) (setq purge_cnt 0))

  ;; 7. 统计并清理重复线条 (强制 Overkill 版)
  (setq ss (ssget "X"))
  (setq ent_before (if ss (sslength ss) 0))
  
  (if ss
    (progn
      ;; 去掉检测，直接强制尝试执行 OVERKILL，用错误捕捉机制来兜底
      (setq err (vl-catch-all-apply '(lambda () (vl-cmdf "_.-OVERKILL" ss "" ""))))
      
      (if (vl-catch-all-error-p err)
        ;; 如果强行执行报错，说明真的没这个命令
        (setq overkill_str "已跳过 (当前CAD不支持此功能)")
        (progn
          ;; 如果没有报错，说明执行成功，计算删除了多少条线
          (setq ss_after (ssget "X"))
          (setq ent_after (if ss_after (sslength ss_after) 0))
          (setq overkill_cnt (- ent_before ent_after))
          (if (< overkill_cnt 0) (setq overkill_cnt 0))
          (setq overkill_str (strcat "成功消除 " (itoa overkill_cnt) " 个"))
        )
      )
    )
    (setq overkill_str "成功消除 0 个")
  )

  ;; 恢复系统环境变量
  (setvar "NOMUTT" old_mutt)
  (setvar "CMDECHO" old_echo)

  ;; ==========================================
  ;; 输出最终排版统计结果
  ;; ==========================================
  (princ "\n")
  (princ "\n=============================================")
  (princ "\n            图纸深度清理已完成！             ")
  (princ "\n---------------------------------------------")
  (princ "\n [清理汇总清单]                              ")
  (princ (strcat "\n  - 光栅图像/照片  : 成功删除 " (itoa img_cnt) " 个"))
  (princ (strcat "\n  - OLE 嵌入对象   : 成功删除 " (itoa ole_cnt) " 个"))
  (princ (strcat "\n  - PDF等参考底图  : 成功删除 " (itoa pdf_cnt) " 个"))
  (princ (strcat "\n  - XREF 外部参照  : 成功拆离 " (itoa xref_cnt) " 个"))
  (princ (strcat "\n  - 冗余块/空图层  : 成功清理 " (itoa purge_cnt) " 个"))
  (princ (strcat "\n  - 重复与重叠线条 : " overkill_str))
  (princ "\n=============================================")
  (princ)
)

(princ "\n>>>[图纸清理插件] 加载成功！ 在命令行输入 CLEANDWG 即可运行 <<<")
(princ)
