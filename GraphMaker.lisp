;;; GraphMaker.lisp
;;;
;;; 下記のデータ構造をgraphvizでpng画像として出力する
;;; ((node1 (description-of-node1))
;;;  (node2 (description-of-node2))
;;;  (node3 (description-of-node3))
;;;  ...)
;;; 
;;; ((source-node1 (destination-node1 direction way)
;;;                (destination-node2 direction way))
;;;  (source-node2 (destination-node1 direction way))
;;;  (source-node3 (destination-node1 direction way)
;;;                (destination-node2 direction way))
;;;  ...)


(defun dot-name (exp)
  "ノードの識別子を変換する
   exp: symbol
    'living-room > LIVING_ROOM
    'foo! > FOO_"
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

; ラベル名の最大文字数
(defparameter *max-label-length* 30)

(defun dot-label (exp)
  "グラフのノードにラベルをつける
   exp: symbol
    *max-label-length*より長い文字列 > 文末に...がついた文字列"
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  "ノードのDOT情報を生成する
   nodes: list
    (nodes->dot '((1 (x1-a)) (a2 (x2-a)) (a3 (x3-a)) (a4 (4-a))))
    > 1[label=\"(1 (X1-A))\"];
    > A2[label=\"(A2 (X2-A))\"];
    > A3[label=\"(A3 (X3-A))\"];
    > A4[label=\"(A4 (|4-A|))\"];"
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  "エッジをDOTフォーマットに変換する"
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun uedges->dot (edges)
  "無向グラフのエッジをDOTフォーマットに変換する"
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun graph->dot (nodes edges)
  "DOTデータを完成させる"
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun ugraph->dot (nodes edges)
  "無向グラフのDOTデータを完成させる"
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  "DOTファイルを画像にする
   fname: 出力先ファイル名
   thunk: ファイルに書き出す文字列を出力する関数"
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  "グラフを画像にする
   fname: 出力先ファイル名
   nodes: 出力対象のノード
   edges: 出力対象のエッジ"
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(defun ugraph->png (fname nodes edges)
  "無向グラフを画像にする 
   fname: 出力先ファイル名
   nodes: 出力対象のノード
   edges: 出力対象のエッジ"
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

