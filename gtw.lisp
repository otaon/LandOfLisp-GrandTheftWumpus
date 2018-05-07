;; congestion city の情報
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)    ; congestion city の場所の数
(defparameter *edge-num* 45)    ; congestion city の道路の本数
(defparameter *worm-num* 3)     ; ギャング Gluesome Glowworm のチーム数
(defparameter *cop-odds* 15)    ; 検問のある確率(1/*cop-odds*の確率)

;; ランダムなエッジの生成
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
   始点と終点が同じノードだとエッジを作成しないため、
   エッジ数は*edge-num*と等しくならない場合がある
   さらに、完全一致するエッジが生成される可能性もある"
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

;; 孤島を作らない
(defun direct-edges (node edge-list)
  "nodeが起点となるエッジをedge-listから抽出する
   node: 起点となるノード
   edge-list: 全エッジのリスト"
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  "繋がっているノードを探す
   node: 出発点となるノード
   edge-list: 全エッジのリスト"
  (let ((visited nil))  ; 探索済みのノードリスト
    (labels ((traverse (node)
               "接続されたノードを横断しつつ訪問済みノードをvisitedに追加する
                node: 訪れたノード"
               ;; nodeが未訪問ならvisitedに追加して、訪問を続ける
               (unless (member node visited)
                 (push node visited)
                 ;; 接続先の全てのノードを訪問する
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  "街に存在する島を列挙する
   nodes: 全てのノードのリスト
   edge-list: 全てのエッジのリスト
   return islands: 街に存在する島のリスト"
  (let ((islands nil))  ; 島ノードリスト
    (labels ((find-island (nodes)
               "ノードを島リスト(islands)に追加する
                nodes: 出発点となるノードのリスト"
               ;; connected: 対象ノードとそれに繋がっているノードのリスト
               ;; unconnected: 対象ノードに繋がっていないノードのリスト
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 ;; 対象ノードとそれに繋がっているノードを島リストに追加する
                 (push connected islands)
                 ;; 対象ノードに繋がっていないノードに対して探索する
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  "2つ以上島がある場合、それらの島に橋を架ける
   islands: 島のリスト
   return: 橋となるエッジのリスト"
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  "街に存在する島々に橋を架ける
   nodes: 全てのノードのリスト
   edge-list: 全てのエッジのリスト
   return: 橋を架けた後のエッジのリスト"
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))


;; congestion city のためのエッジリストを完成させる
(defun make-city-edges ()
  "congestion city のエッジリストを作成する"
  ;; nodes: 1から*node-num*までのノード番号のリスト
  ;; edge-list: 街の全ノードに接続されたエッジのリスト
  ;; cops: 警察の検問を設置したエッジのリスト
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         ;; 1 / *cop-odds* の確率でedge-listから要素を抽出しcopsに追加する
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  "エッジをalistに変換する
   edge-list: ((1.2)(2.1)...(a.b)(b.a)) 形式のエッジのリスト
   return: ((1 (2)) (2 (1) (3)) (3 (2))) 形式のエッジのalist"
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

