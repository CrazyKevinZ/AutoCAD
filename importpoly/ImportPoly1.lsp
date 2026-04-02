(defun c:ImportPoly ( / fn f line data mode elev pt_list split-str p x y z pt 
                       orig_cmdecho orig_osmode orig_blipmode)
  (vl-load-com)

  ;; 内部函数：按逗号分割字符串
  (defun split-str (str / pos lst)
    (while (setq pos (vl-string-search "," str))
      (setq lst (cons (substr str 1 pos) lst)
            str (substr str (+ pos 2))
      )
    )
    (reverse (cons str lst))
  )

  ;; 【关键改进】保存原有的绘图环境
  (setq orig_cmdecho (getvar "CMDECHO")
        orig_osmode  (getvar "OSMODE")
        orig_blipmode (getvar "BLIPMODE")
  )

  ;; 1. 选择模式
  (initget "1 2 3")
  (setq mode (getkword "\n选择导入方式: [1-保持原始3D | 2-压平至Z=0 | 3-统一高度] <1>: "))
  (if (not mode) (setq mode "1"))

  (setq elev 0.0)
  (if (= mode "3")
    (setq elev (getdist "\n输入统一的Z高度值: "))
  )

  ;; 2. 选择文件
  (setq fn (getfiled "选择坐标文件" "" "csv" 0))
  (if (and fn (setq f (open fn "r")))
    (progn
      (read-line f) ; 跳过表头
      (setq pt_list nil)
      
      (while (setq line (read-line f))
        (setq data (split-str line))
        (if (>= (length data) 4)
          (progn
            (setq x (atof (nth 1 data))
                  y (atof (nth 2 data))
                  z (atof (nth 3 data)))
            
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
      (close f)

      ;; 3. 绘图
      (if pt_list
        (progn
          (setq pt_list (reverse pt_list))
          
          ;; 【改进】临时关闭运行环境
          (setvar "CMDECHO" 0)
          (setvar "OSMODE" 0) ; 关闭对象捕捉
          (setvar "BLIPMODE" 0) ; 关闭标记点
          
          ;; 如果选 2 或 3，生成的是二维多段线，使用 PLINE 效率更高
          (if (or (= mode "2") (= mode "3"))
            (command "._pline")
            (command "._3dpoly")
          )
          
          (foreach p pt_list (command p))
          (command "")
          
          (princ (strcat "\n成功绘制多段线，共 " (itoa (length pt_list)) " 个节点。"))
        )
      )
    )
  )

  ;; 【关键改进】恢复原有的绘图环境
  (setvar "CMDECHO" orig_cmdecho)
  (setvar "OSMODE" orig_osmode)
  (setvar "BLIPMODE" orig_blipmode)

  (princ)
)
