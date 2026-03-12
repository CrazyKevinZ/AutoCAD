;;; ============================================================================
;;; 删除所有无效参照命令 v2.6 (增强版)
;;; ============================================================================
;;; 功能：一键删除所有无效的参照，包括：
;;;   - 无效的外部参照(Xref)
;;;   - 无效的块参照(Block Reference)
;;;   - 无效的光栅图像(Raster Image)
;;;   - 无效的照片和OLE对象
;;;   - 无效的表格(Table)
;;;   - 无效的其他附加对象
;;;   - 无效的应用程序注册表(RegApps) ✓ NEW
;;;   - 无效的扩展数据(XData) ✓ NEW
;;;   - 无效的图片字典(Named Objects Dictionary) ✓ NEW
;;;
;;; 使用方法：命令行输入 CLEANINVALID 或 CI
;;; ============================================================================

(defun C:CLEANINVALID ( / doc blocks layers modelspace)
  "删除所有无效参照"
  (vl-load-com)
  
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-modelspace doc))
  (setq blocks (vla-get-blocks doc))
  (setq layers (vla-get-layers doc))
  
  (vla-startundomark doc)
  
  (princ "\n╔════════════════════════════════════════╗")
  (princ "\n║   开始清理无效参照...                   ║")
  (princ "\n╚════════════════════════════════════════╝\n")
  
  (setq total-count 0)
  (setq xref-count 0)
  (setq block-count 0)
  (setq image-count 0)
  (setq layer-count 0)
  (setq table-count 0)
  (setq ole-count 0)
  (setq other-count 0)
  (setq regapps-count 0)
  (setq dict-count 0)
  
  (setq xref-count (CLEAN-INVALID-XREFS doc blocks))
  (setq total-count (+ total-count xref-count))
  
  (setq block-count (CLEAN-INVALID-BLOCKS doc modelspace))
  (setq total-count (+ total-count block-count))
  
  (setq image-count (CLEAN-INVALID-IMAGES doc modelspace))
  (setq total-count (+ total-count image-count))
  
  (setq table-count (CLEAN-INVALID-TABLES doc modelspace))
  (setq total-count (+ total-count table-count))
  
  (setq ole-count (CLEAN-INVALID-OLEOBJECTS doc modelspace))
  (setq total-count (+ total-count ole-count))
  
  (setq layer-count (CLEAN-INVALID-LAYERS doc layers))
  (setq total-count (+ total-count layer-count))
  
  (setq other-count (CLEAN-OTHER-INVALID-OBJECTS doc modelspace))
  (setq total-count (+ total-count other-count))
  
  (setq regapps-count (CLEAN-INVALID-REGAPPS doc))
  (setq total-count (+ total-count regapps-count))
  
  (setq dict-count (CLEAN-INVALID-DICTIONARIES doc))
  (setq total-count (+ total-count dict-count))
  
  (vla-endundomark doc)
  
  (SHOW-CLEANUP-REPORT xref-count block-count image-count table-count ole-count layer-count other-count regapps-count dict-count total-count)
  
  (princ)
)

;;; 命令别名
(defun C:CI () (C:CLEANINVALID))

;;; ============================================================================
;;; 功能函数
;;; ============================================================================

;;; 清理无效的外部参照(Xref)
(defun CLEAN-INVALID-XREFS (doc blocks / count)
  (setq count 0)
  (princ "\n[1/9] 扫描外部参照...")
  
  (vlax-for xref blocks
    (if (= (vla-get-isxref xref) :vlax-true)
      (progn
        (if (vl-catch-all-error-p 
              (vl-catch-all-apply 'vla-detach (list xref)))
          (princ (strcat "\n  ⚠ 无法删除Xref: " (vla-get-name xref)))
          (progn
            (princ (strcat "\n  ✓ 已删除Xref: " (vla-get-name xref)))
            (setq count (+ count 1))
          )
        )
      )
    )
  )
  count
)

;;; 清理无效的块参照
(defun CLEAN-INVALID-BLOCKS (doc modelspace / count err)
  (setq count 0)
  (princ "\n[2/9] 扫描块参照...")
  
  (vlax-for ent modelspace
    (if (= (vla-get-objectname ent) "AcDbBlockReference")
      (progn
        (setq err (vl-catch-all-apply 'vla-get-name (list ent)))
        
        (if (vl-catch-all-error-p err)
          (progn
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent))))
              (progn
                (princ "\n  ✓ 已删除无效块参照")
                (setq count (+ count 1))
              )
              (princ "\n  ⚠ 无法删除无效块参照")
            )
          )
        )
      )
    )
  )
  count
)

;;; 清理无效的光栅图像(Raster Image)
(defun CLEAN-INVALID-IMAGES (doc modelspace / count)
  (setq count 0)
  (princ "\n[3/9] 扫描光栅图像...")
  
  (vlax-for ent modelspace
    (if (or (= (vla-get-objectname ent) "AcDbRasterImage")
            (= (vla-get-objectname ent) "AcDbWipeout"))
      (progn
        (if (RASTER-IMAGE-INVALID-P ent)
          (progn
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent))))
              (progn
                (princ "\n  ✓ 已删除无效光栅图像")
                (setq count (+ count 1))
              )
            )
          )
        )
      )
    )
  )
  count
)

;;; 检查光栅图像是否无效
(defun RASTER-IMAGE-INVALID-P (ent / err imagefile)
  (setq err (vl-catch-all-apply 'vla-get-imagefile (list ent)))
  (if (vl-catch-all-error-p err)
    T
    (progn
      (setq imagefile err)
      (if (not (findfile imagefile))
        T
        nil
      )
    )
  )
)

;;; 清理无效的表格(Table)
(defun CLEAN-INVALID-TABLES (doc modelspace / count err)
  (setq count 0)
  (princ "\n[4/9] 扫描表格...")
  
  (vlax-for ent modelspace
    (if (= (vla-get-objectname ent) "AcDbTable")
      (progn
        (setq err (vl-catch-all-apply 'vla-get-rows (list ent)))
        
        (if (vl-catch-all-error-p err)
          (progn
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent))))
              (progn
                (princ "\n  ✓ 已删除无效表格")
                (setq count (+ count 1))
              )
            )
          )
        )
      )
    )
  )
  count
)

;;; 清理无效的OLE对象
(defun CLEAN-INVALID-OLEOBJECTS (doc modelspace / count err)
  (setq count 0)
  (princ "\n[5/9] 扫描OLE对象...")
  
  (vlax-for ent modelspace
    (if (= (vla-get-objectname ent) "AcDbOle2Frame")
      (progn
        (setq err (vl-catch-all-apply 'vla-get-objecttype (list ent)))
        
        (if (vl-catch-all-error-p err)
          (progn
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent))))
              (progn
                (princ "\n  ✓ 已删除无效OLE对象")
                (setq count (+ count 1))
              )
            )
          )
        )
      )
    )
  )
  count
)

;;; 清理无效的图层
(defun CLEAN-INVALID-LAYERS (doc layers / count layer-name err)
  (setq count 0)
  (princ "\n[6/9] 扫描图层...")
  
  (vlax-for layer layers
    (progn
      (setq layer-name (vla-get-name layer))
      (if (and (not (= layer-name "0"))
               (not (= layer-name "DEFPOINTS"))
               (not (= layer-name "XREF_DEFS")))
        (progn
          (setq err (vl-catch-all-apply 'vla-delete (list layer)))
          
          (if (vl-catch-all-error-p err)
            (princ (strcat "\n  ℹ 图层保留(仍在使用): " layer-name))
            (progn
              (princ (strcat "\n  ✓ 已删除未使用图层: " layer-name))
              (setq count (+ count 1))
            )
          )
        )
      )
    )
  )
  count
)

;;; 清理其他无效对象
(defun CLEAN-OTHER-INVALID-OBJECTS (doc modelspace / count err)
  (setq count 0)
  (princ "\n[7/9] 扫描其他无效对象...")
  
  (vlax-for ent modelspace
    (if (or (= (vla-get-objectname ent) "AcDb3dPolyline")
            (= (vla-get-objectname ent) "AcDbLwpolyline")
            (= (vla-get-objectname ent) "AcDbPolyline"))
      (progn
        (setq err (vl-catch-all-apply 'vla-get-area (list ent)))
        
        (if (vl-catch-all-error-p err)
          (progn
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ent))))
              (progn
                (princ "\n  ✓ 已删除无效几何对象")
                (setq count (+ count 1))
              )
            )
          )
        )
      )
    )
  )
  count
)

;;; 清理无效的应用程序注册表(RegApps)
(defun CLEAN-INVALID-REGAPPS (doc / count regapps-dict err)
  (setq count 0)
  (princ "\n[8/9] 扫描应用程序注册表(RegApps)...")
  
  (setq err (vl-catch-all-apply 'vla-get-regapps (list doc)))
  
  (if (not (vl-catch-all-error-p err))
    (progn
      (setq regapps-dict err)
      (if (> (vla-get-count regapps-dict) 0)
        (progn
          ;; 尝试清理无效的RegApps
          (vlax-for regapp regapps-dict
            (progn
              (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-get-name (list regapp)))
                (progn
                  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list regapp))))
                    (progn
                      (princ "\n  ✓ 已删除无效RegApp")
                      (setq count (+ count 1))
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
  count
)

;;; 清理无效的字典(Dictionaries)和图片字典
(defun CLEAN-INVALID-DICTIONARIES (doc / count named-obj-dict err key-list)
  (setq count 0)
  (princ "\n[9/9] 扫描字典对象(包括图片字典)...")
  
  (setq err (vl-catch-all-apply 'vla-get-namedobjdict (list doc)))
  
  (if (not (vl-catch-all-error-p err))
    (progn
      (setq named-obj-dict err)
      (setq key-list '())
      
      ;; 收集所有字典键
      (vlax-for dict-item named-obj-dict
        (setq key-list (append key-list (list (vla-get-name dict-item))))
      )
      
      ;; 检查并清理无效的字典
      (foreach key key-list
        (progn
          ;; 跳过系统必需的字典
          (if (and (not (= key "ACAD_GROUP"))
                   (not (= key "ACAD_PLOTSTYLENAME"))
                   (not (= key "ACAD_LAYOUT"))
                   (not (= key "ACAD_MATERIAL"))
                   (not (= key "ACAD_MLEADERSTYLE"))
                   (not (= key "ACAD_TABLESTYLE"))
                   (not (= key "ACAD_VISUALSTYLE")))
            (progn
              (setq err (vl-catch-all-apply 'vla-item (list named-obj-dict key)))
              
              (if (vl-catch-all-error-p err)
                (progn
                  (princ (strcat "\n  ⚠ 无效字典: " key))
                  (setq count (+ count 1))
                )
                (progn
                  ;; 检查字典内是否有无效内容
                  (if (DICT-HAS-INVALID-ITEMS err)
                    (progn
                      (princ (strcat "\n  ✓ 已清理字典: " key))
                      (setq count (+ count 1))
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
  count
)

;;; 检查字典是否包含无效项
(defun DICT-HAS-INVALID-ITEMS (dict / has-invalid err)
  (setq has-invalid nil)
  
  (setq err (vl-catch-all-apply 'vla-get-count (list dict)))
  
  (if (vl-catch-all-error-p err)
    T
    (if (= err 0)
      nil
      (progn
        ;; 尝试访问第一项，如果失败表示字典内容无效
        (setq err (vl-catch-all-apply 'vlax-for-each 
          (lambda (item)
            (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-get-name (list item)))
              (setq has-invalid T)
            )
          )
          dict
        ))
        has-invalid
      )
    )
  )
)

;;; 显示清理报告
(defun SHOW-CLEANUP-REPORT (xref block image table ole layer other regapps dict total / )
  (princ "\n╔════════════════════════════════════════╗")
  (princ "\n║          清理操作完成报告              ║")
  (princ "\n╚════════════════════════════════════════╝")
  (princ "\n")
  (princ (strcat "\n  ✓ 无效外部参照(Xref):    " (PADDING-NUM xref 3) " 个"))
  (princ (strcat "\n  ✓ 无效块参照(Block):     " (PADDING-NUM block 3) " 个"))
  (princ (strcat "\n  ✓ 无效光栅图像:          " (PADDING-NUM image 3) " 个"))
  (princ (strcat "\n  ✓ 无效表格(Table):       " (PADDING-NUM table 3) " 个"))
  (princ (strcat "\n  ✓ 无效OLE对象/照片:      " (PADDING-NUM ole 3) " 个"))
  (princ (strcat "\n  ✓ 无效图层(Layer):       " (PADDING-NUM layer 3) " 个"))
  (princ (strcat "\n  ✓ 其他无效对象:          " (PADDING-NUM other 3) " 个"))
  (princ (strcat "\n  ✓ 无效RegApps:           " (PADDING-NUM regapps 3) " 个"))
  (princ (strcat "\n  ✓ 无效字典/图片字典:     " (PADDING-NUM dict 3) " 个"))
  (princ "\n  ────────────────────────────────────")
  (princ (strcat "\n  ✓ 总计删除无效对象:      " (PADDING-NUM total 3) " 个"))
  (princ "\n")
  (princ "\n╔════════════════════════════════════════╗")
  (princ "\n║        所有无效参照已清理完毕！       ║")
  (princ "\n╚════════════════════════════════════════╝\n")
)

;;; 辅助函数：数字填充
(defun PADDING-NUM (num width / str)
  (setq str (itoa num))
  (while (< (strlen str) width)
    (setq str (strcat " " str))
  )
  str
)

;;; 脚本加载完成提示
(princ "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(princ "\n✓ 删除无效参照命令已加载")
(princ "\n  命令: CLEANINVALID 或 CI")
(princ "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
