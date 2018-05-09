;; congestion city の情報
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)    ; congestion city の場所の数
(defparameter *edge-num* 45)    ; congestion city の道路の本数
(defparameter *worm-num* 3)     ; ギャング Gluesome Glowworm のチーム数
(defparameter *cop-odds* 15)    ; 検問のある確率(1/*cop-odds*の確率)
(defvar *player-pos*)           ; プレイヤーがいるノード

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
  "nodeと直接つながっているエッジをedge-listから抽出する
   node: 起点となるノード B
   edge-list: 全エッジのリスト ((A.B)(B.A)(B.C)(C.B))
   return: nodeと直接つながっているエッジのリスト ((B.A)(B.C))"
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  "繋がっているノードを探す
   node: 出発点となるノード A
   edge-list: 全エッジのリスト ((A.B)(B.A)(B.C)(C.B)(D.E)(E.D))
   return: 出発点と繋がっているノードのリスト (ABC)"
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
   nodes: 全てのノードのリスト  (ABCDE)
   edge-list: 全てのエッジのリスト ((A.B)(B.A)(B.C)(C.B)(D.E)(E.D))
   return islands: 街に存在する島のリスト ((DE)(ABC))"
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
   islands: 島のリスト  ((ABC)(DE))
   return: 橋となるエッジのリスト   ((A.D)(D.A))"
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  "街に存在する島々に橋を架ける
   nodes: 全てのノードのリスト  (ABCDE)
   edge-list: 全てのエッジのリスト  ((A.B)(B.A)(B.C)(C.B)(D.E)(E.D))
   return: 橋を架けた後のエッジのリスト ((A.D)(D.A)(A.B)(B.A)(B.C)(C.B)(D.E)(E.D))"
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))


;; congestion city のためのエッジリストを完成させる
(defun make-city-edges ()
  "congestion city のエッジリストを作成する"
  ;; nodes: 1から*node-num*までのノード番号のリスト
  ;; edge-list: 街の全ノードに接続されたエッジのリスト
  ;; cops: 警察の検問所を設置したエッジのリスト
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         ;; 1 / *cop-odds* の確率でedge-listから要素を抽出しcopsに追加する
         (cops (remove-if-not (lambda ()
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  "エッジをalistに変換する
   edge-list: ((1.2)(2.1)...(a.b)(b.a)) 形式のエッジのリスト
   return: ((1 (2)) (2 (1) (3)) (3 (2))) 形式のエッジのalist"
  (mapcar (lambda (node1)
            ;; node1 をキーとしてそれに直接繋がっているエッジのalist要素 (1 (2)))
            (cons node1
                  ;; 各ノードに直接つながっているエッジのリスト ((2))
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          ;; node1とそれに直接つながっているエッジのリスト((1.2))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          ;; エッジの起点となるノードのリスト(1 2 3 ...)
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  "alistに検問所の情報を追加する
   edge-alist: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   edges-with-cops: 検問所のあるエッジのリスト
   return: 検問所の情報が追加されたエッジのalist"
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

;; congestion cityのノードリストを作る
(defun neighbors (node edge-alist)
  "nodeに直接繋がっているノードのリストを返す
   node: 基準となるノード 2
   edge-alist: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   return: 直接繋がっているノードのリスト (1 3)"
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  "ノードbが他ノードaと隣同士で繋がっているかを返す
   a: 基準となるノード 1
   b: 隣同士で繋がっているか判定したいノード 3
   edge-alist: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   return: 隣同士で繋がっているかの真偽値 nil"
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  "ノードbが隣同士で、または、他ノードを挟んでノードaと繋がっているかを返す
   a: 基準となるノード 1
   b: 隣同士で、または、他ノードを挟んで繋がっているか判定したいノード 3
   edge-alist: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   return: 隣同士で、または、他ノードを挟んで繋がっているかの真偽値 t"
  (or (within-one a b edge-alist)
      ;; aの隣のノードの中で、bの隣のノードが1つ以上存在するか
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  "congestion city の最終的なマップを返す
   edge-alist: 街のエッジのリスト
   return: 情報つきの街の全ノードのリスト"
  ;; wumpus: wumpusがいるノードの番号(wumpusは一人なので一箇所)
  ;; glow-worms: glow-wormsがいるノードの番号
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    ;; 街の各ノードにゲーム情報を追加して、街のノードのリストを
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          ;; wumpusがいるノードには「wumpus」情報を追加
                          ;; wumpusが隣か二つ隣にいるノードには「blood!」情報を追加
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          ;; glow wormがいるノードには「glow-worms」情報を追加
                          ;; glow wormが隣にいるノードには「lights!」情報を追加
                          (cond ((member n glow-worms)
                                 '(glow-worms))
                                ((some (lambda (worm)
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                 '(lights!)))
                          ;; 警察が隣接する道路（エッジ）にいるノードには「sirens!」情報を追加
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))


;; gtwを初期化する
(defun new-game ()
  "ゲームを初期化する"
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  "プレーヤーが敵と同じ場所に配置されないように、空のノードを探し出す
   return: 情報が追加されていないノードの番号"
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

