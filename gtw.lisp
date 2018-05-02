;; 7章で作成したグラフ操作系のユーティリティを読み込む
(load "graph-util")

;; congestion city の情報
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)    ; congestion city の場所の数
(defparameter *edge-num* 45)    ; congestion city の道路の本数
(defparameter *worm-num* 3)     ; ギャング Gluesome Glowworm のチーム数
(defparameter *cop-odds* 15)    ; 検問のある確率(1/*cop-odds*の確率)

(defun random-node ()
  "ランダムなノード番号を返す
   ノード番号は1始まり"
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "ノードaとノードb(a!=b)を dotted pair として両方の向きでリストとする
   a: ノード番号
   b: ノード番号
   > ((a . b) (b . a))"
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  "ランダムなエッジのリストを生成する
   ノードが重複するとエッジを作成しないため、
   エッジ数は*edge-num*と等しくならない場合がある"
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

