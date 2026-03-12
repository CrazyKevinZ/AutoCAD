(defun c:ImportPoly ( / fn f line data mode elev pt_list p x y z pt old_cmdecho old_osmode)
  (vl-load-com)

  ;; 内部函数：按逗号分割字符串（改进版）
  (defun split-str (str / pos lst)
    (while (setq pos (vl-string-search "," str))
      (setq lst (cons (substr str 1 pos) lst)
            str (substr str (+ pos 2))
      )
    )
    (reverse (cons str lst))
  )

  ;; 1. 选择模式
  (initget "1 2 3")
  (setq mode (getkword "\n选择导入方式: [1-保持原始3D | 2-压平至Z=0 | 3-统一高度] <1>: "))
  (if (not mode) (setq mode "1"))

  (setq elev 0.0)
  (if (= mode "3")
    (progn
      (setq elev (getdist "\n输入统一的Z高度值: "))
      (if (not elev) (setq elev 0.0))
    )
  )

  ;; 2. 选择文件
  (setq fn (getfiled "选择坐标文件" "" "csv" 0))
  
  (if (and fn (setq f (open fn "r")))
    (progn
      (read-line f) ; 跳过表头
      (setq pt_list nil)
      
      (while (setq line (read-line f))
        (setq data (split-str line))
        
        ;; 验证数据有效性
        (if (>= (length data) 4)
          (progn
            ;; 安全的坐标转换
            (setq x (atof (nth 1 data))
                  y (atof (nth 2 data))
                  z (atof (nth 3 data)))
            
            ;; 检查坐标是否有效
            (if (and (numberp x) (numberp y) (numberp z))
              (progn
                ;; 根据模式处理坐标
                (cond
                  ((= mode "1") (setq pt (list x y z))) ; 保持3D
                  ((= mode "2") (setq pt (list x y 0.0))) ; 强制0
                  ((= mode "3") (setq pt (list x y elev))) ; 统一高度
                )
                (setq pt_list (cons pt pt_list))
              )
            )
          )
        )
      )
      
      (close f)

      ;; 3. 绘图
      (if pt_list
        (progn
          (setq pt_list (reverse pt_list))
          
          ;; 保存原始系统变量
          (setq old_cmdecho (getvar "CMDECHO")
                old_osmode (getvar "OSMODE"))
          
          (setvar "CMDECHO" 0)
          (setvar "OSMODE" 0)
          
          ;; 如果选 2 或 3，生成的是二维多段线
          (if (or (= mode "2") (= mode "3"))
            (command "._pline")
            (command "._3dpoly")
          )
          
          (foreach p pt_list (command p))
          (command "")
          
          ;; 恢复系统变量
          (setvar "CMDECHO" old_cmdecho)
          (setvar "OSMODE" old_osmode)
          
          (princ (strcat "\n成功绘制多段线，共 " (itoa (length pt_list)) " 个节点。"))
        )
        (princ "\n错误：没有有效的坐标数据。")
      )
    )
    (if fn
      (princ "\n错误：无法打开文件。")
      (princ "\n已取消。")
    )
  )
  (princ)
)
