;;; ---------------------------------------------------------------
;;; ゲーム用パラメータ・関数定義
;;; ---------------------------------------------------------------

;; ゲームで使用するディレクトリの名前
(defparameter *game-directory* "graph")


;; congestion city の情報{{{
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)    ; congestion city の場所の数
(defparameter *edge-num* 45)    ; congestion city の道路の本数
(defparameter *worm-num* 3)     ; ギャング Gluesome Glowworm のチーム数
(defparameter *cop-odds* 15)    ; 検問のある確率(1/*cop-odds*の確率)
(defvar *player-pos*)           ; プレイヤーがいるノード
;}}}

;; 連想リストをハッシュテーブルに変換する
(defun alist->hash-table (edge-list);{{{
  "リストの全てのキーとバリューをハッシュテーブルにプッシュする"
  (let ((tab (make-hash-table)))    ; tab: ハッシュテーブル
    (mapc (lambda (x)
            (let ((node (car x)))   ; node: キー
              ;; cdr x: バリュー
              ;; gethash node tab: ハッシュテーブルにおいてキーがnodeの場所
              (push (cdr x) (gethash node tab))))
          edge-list)
    tab));}}}

;; ハッシュテーブルを連想リストに変換する
(defun hash-table->alist (edge-hash-table);{{{
  "リストの全てのキーとバリューをハッシュテーブルにプッシュする"
  (let ((al nil))
    (maphash (lambda (k v)
             (princ (cons k (list v)))
             (fresh-line)
             (push (cons k (list v)) al))
           edge-hash-table)
    ;;
    ;;====================================
    ;; ハッシュテーブルにしたことで必要なデータが落ちているのでは……？要確認
    ;;====================================
    ;;
    al));}}}


;; ランダムなエッジの生成
(defun random-node ();{{{
  "ランダムなノード番号を返す
   ノード番号は1始まり"
  (1+ (random *node-num*)));}}}

(defun edge-pair (a b);{{{
  "ノードaとノードb(a!=b)を dotted pair として両方の向きでリストとする
   a: ノード番号
   b: ノード番号
   > ((a . b) (b . a))"
  (unless (eql a b)
    (list (cons a b) (cons b a))));}}}

(defun make-edge-list ();{{{
  "ランダムなエッジのリストを生成する
   始点と終点が同じノードだとエッジを作成しないため、
   エッジ数は*edge-num*と等しくならない場合がある
   さらに、完全一致するエッジが生成される可能性もある"
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))));}}}


;; 孤島を作らない
(defun direct-edges (node edge-list);{{{
  "nodeと直接つながっているエッジをedge-listから抽出する
   node: 起点となるノード B
   edge-list: 全エッジのリスト ((A.B)(B.A)(B.C)(C.B))
   return: nodeと直接つながっているエッジのリスト ((B.A)(B.C))"
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list));}}}

(defun get-connected (node edge-list);{{{
  "繋がっているノードを探す
   node: 出発点となるノード A
   edge-list: 全エッジのリスト ((A.B)(B.A)(B.C)(C.B)(D.E)(E.D))
   return: 出発点と繋がっているノードのリスト (ABC)"
  (let ((visited nil))  ; 探索済みのノードリスト
    (labels ((traverse (node)
               ;; 接続されたノードを横断しつつ訪問済みノードをvisitedに追加する
               ;; node: 訪れたノード
               ;; nodeが未訪問ならvisitedに追加して、訪問を続ける
               (unless (member node visited)
                 (push node visited)
                 ;; 接続先の全てのノードを訪問する
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited));}}}

(defun get-connected-hash (node edge-tab);{{{
  "繋がっているノードを探す（ハッシュテーブル版）"
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (mapc (lambda (edge)
                         (traverse edge))
                       (gethash node edge-tab)))))
      (traverse node))
    visited));}}}

(defun find-islands (nodes edge-list);{{{
  "街に存在する島を列挙する
   nodes: 全てのノードのリスト  (ABCDE)
   edge-list: 全てのエッジのリスト ((A.B)(B.A)(B.C)(C.B)(D.E)(E.D))
   return islands: 街に存在する島のリスト ((DE)(ABC))"
  (let ((islands nil))  ; 島ノードリスト
    (labels ((find-island (nodes)
               ;; ノードを島リスト(islands)に追加する
               ;; nodes: 出発点となるノードのリスト

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
    islands));}}}

(defun connect-with-bridges (islands);{{{
  "2つ以上島がある場合、それらの島に橋を架ける
   islands: 島のリスト  ((ABC)(DE))
   return: 橋となるエッジのリスト   ((A.D)(D.A))"
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))));}}}

(defun connect-all-islands (nodes edge-list);{{{
  "街に存在する島々に橋を架ける
   nodes: 全てのノードのリスト  (ABCDE)
   edge-list: 全てのエッジのリスト  ((A.B)(B.A)(B.C)(C.B)(D.E)(E.D))
   return: 橋を架けた後のエッジのリスト ((A.D)(D.A)(A.B)(B.A)(B.C)(C.B)(D.E)(E.D))"
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list));}}}


;; congestion city のためのエッジリストを完成させる
(defun make-city-edges ();{{{
  "congestion city のエッジリストを作成する"
  ;; nodes: 1から*node-num*までのノード番号のリスト
  ;; edge-list: 街の全ノードに接続されたエッジのリスト
  ;; cops: 警察の検問所を設置したエッジのリスト
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         ;; 1 / *cop-odds* の確率でedge-listから要素を抽出しcopsに追加する
         (cops (remove-if-not (lambda (x)
                                (declare (ignore x))
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)));}}}

(defun edges-to-alist (edge-list);{{{
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
          (remove-duplicates (mapcar #'car edge-list))));}}}

(defun add-cops (edge-alist edges-with-cops);{{{
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
          edge-alist));}}}


;; congestion cityのノードリストを作る
(defun neighbors (node edge-hash-table);{{{
  "nodeに直接繋がっているノードのリストを返す
   node: 基準となるノード 2
   edge-hash-table: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   return: 直接繋がっているノードのリスト (1 3)"
  (mapcar #'car (cdr (gethash node edge-hash-table))));}}}

(defun within-one (a b edge-hash-table);{{{
  "ノードbが他ノードaと隣同士で繋がっているかを返す
   a: 基準となるノード 1
   b: 隣同士で繋がっているか判定したいノード 3
   edge-hash-table: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   return: 隣同士で繋がっているかの真偽値 nil"
  (member b (neighbors a edge-hash-table)));}}}

(defun within-two (a b edge-hash-table);{{{
  "ノードbが隣同士で、または、他ノードを挟んでノードaと繋がっているかを返す
   a: 基準となるノード 1
   b: 隣同士で、または、他ノードを挟んで繋がっているか判定したいノード 3
   edge-hash-table: 街のエッジのリスト ((1 (2)) (2 (1) (3)) (3 (2)))
   return: 隣同士で、または、他ノードを挟んで繋がっているかの真偽値 t"
  (or (within-one a b edge-hash-table)
      ;; aの隣のノードの中で、bの隣のノードが1つ以上存在するか
      (some (lambda (x)
              (within-one x b edge-hash-table))
            (neighbors a edge-hash-table))));}}}

(defun make-city-nodes (edge-hash-table);{{{
  "congestion city の最終的なマップを返す
   edge-hash-table: 街のエッジのハッシュテーブル
   return: 情報つきの街の全ノードのリスト"
  ;; wumpus: wumpusがいるノードの番号(wumpusは一人なので一箇所)
  ;; glow-worms: glow-wormsがいるノードの番号
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    ;; 街の各ノードにゲーム情報を追加して、街のノードのリストを作成する
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          ;; wumpusがいるノードには「wumpus」情報を追加
                          ;; wumpusが隣か二つ隣にいるノードには「blood!」情報を追加
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-hash-table) '(blood!)))
                          ;; glow wormがいるノードには「glow-worms」情報を追加
                          ;; glow wormが隣にいるノードには「lights!」情報を追加
                          (cond ((member n glow-worms)
                                 '(glow-worms))
                                ((some (lambda (worm)
                                         (within-one n worm edge-hash-table))
                                       glow-worms)
                                 '(lights!)))
                          ;; 警察が隣接する道路（エッジ）にいるノードには「sirens!」情報を追加
                          (when (some #'cdr (cdr (gethash n edge-hash-table)))
                            '(sirens!))))));}}}


;;; ---------------------------------------------------------------
;;; ゲームエンジン本体
;;; ---------------------------------------------------------------

;; gtwを初期化する
(defun new-game ();{{{
  "ゲームを初期化する"
  (setf *congestion-city-edges* (alist->hash-table (make-city-edges)))
  (setf *congestion-city-nodes* (alist->hash-table (make-city-nodes *congestion-city-edges*)))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city));}}}

(defun find-empty-node ();{{{
  "プレーヤーが敵と同じ場所に配置されないように、空のノードを探し出す
   return: 情報が追加されていないノードの番号"
  (let ((x (random-node)))
    (if (cdr (gethash x *congestion-city-nodes*))
        (find-empty-node)
        x)));}}}


;; congestion cityのマップを描く
(defun draw-city ();{{{
  "graphvizを使ってcongestion cityのマップを描く"
  (ugraph->png (concatenate 'string *game-directory* "/" "city")
               (hash-table->alist *congestion-city-nodes*)
               (hash-table->alist *congestion-city-edges*)));}}}


;; 部分的な知識から congestion city を描く
(defun draw-known-city ();{{{
  "既知の部分だけの地図を描く"
  (ugraph->png (concatenate 'string *game-directory* "/" "known-city")
               (known-city-nodes)
               (known-city-edges)));}}}

(defun known-city-nodes ();{{{
  "既知のノードからなるalistを作る"
  ;; 既に訪れたノード&そこに直接繋がったノード(?と*付き)のリスト
  (mapcar (lambda (node)
            ;; nodeが既に訪れたノードで、
            ;; - 現在プレイヤーがいるノードなら(node node情報 *)を表示する
            ;; - 現在プレイヤーがいないノードなら(node node情報)を表示する
            ;; nodeがまだ訪れていないノードなら(node番号 ?)を表示する
            (if (member node *visited-nodes*)
                (let ((n (gethash node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          ;; 既に訪れたノード&そこに直接繋がったノードのリスト
          ;; *congestion-city-edges*: '((1 (2)) (2 (1) (3)) (3 (2)) (4 (3)))
          ;; *visited-nodes*: '(1 2)
          ;; return: '(1 2 3)
          (remove-duplicates
            (append *visited-nodes*
                    ;; 既に訪れたノードに直接繋がったノードのリスト
                    ;; *congestion-city-edges*: '((1 (2)) (2 (1) (3)) (3 (2) (4)) (4 (3)))
                    ;; *visited-nodes*: '(1 2)
                    ;; return '(2 1 3)
                    (mapcan (lambda (node)
                              (mapcar #'car
                                      (cdr (gethash node *congestion-city-edges*))))
                            *visited-nodes*)))));}}}

(defun known-city-edges ();{{{
  "まだ訪れていない道にいる警官のサイレンの情報を取り除いたエッジのalistを作る"
  (mapcar (lambda (node)
            ;; すべての既に訪れたノードに対して警察のサイレンの情報を消すか否か操作する
            ;; 操作したエッジ情報を使って新たにエッジのalistを作る
            (cons node (mapcar (lambda (x)
                                 ;; 両端が既に訪れたノード(=通った道)
                                 ;;   ...警察のサイレンの情報をそのまま残す
                                 ;; 少なくとも片方がまだ訪れていないノード(=通ってない道)
                                 ;;   ...警察のサイレンの情報を消す
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               ;; 既に訪れたノードに直接繋がったリスト
                               (cdr (gethash node *congestion-city-edges*)))))
          *visited-nodes*));}}}


;;; ---------------------------------------------------------------
;;; ゲームコマンド
;;; ---------------------------------------------------------------

;; 街を歩き回る
(defun walk (pos);{{{
  "ただ移動するだけ"
  (handle-direction pos nil));}}}

(defun charge (pos);{{{
  "移動しつつ残った最後の弾丸で攻撃する"
  (handle-direction pos t));}}}

(defun handle-direction (pos charging);{{{
  "posが移動できるノードなら移動する
   攻撃を選択していたら攻撃する"
  ;; 現在位置からposへ行く道があれば、移動と、場合によっては攻撃する
  ;; 現在位置からposへ行く道がなければ、移動できない旨のメッセージを表示する
  (let ((edge (gethash pos
                     (cdr (gethash *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))));}}}

(defun handle-new-place (edge pos charging);{{{
  "posへ移動する。cop, wumpus, glow wormの有無によってイベントメッセージを表示する"
  ;; node:     移動先のノード情報
  ;; has-worm: glow wormがいるか否かのフラグ
  ;;           ただしglow worm は1匹につき1回のみ攻撃するため、
  ;;           訪問済みノードの場合、glow wormはいないものとする
  (let* ((node (gethash pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)   ; posを訪問済みノードに追加する
    (setf *player-pos* pos)         ; 現在位置をposに更新する
    (draw-known-city)               ; 既知の部分だけの地図を描く
    ;; ノード、または、エッジの状態によってプレイヤー状態を更新する
    (cond
      ;; 通り道に警察がいたら捕まってゲームオーバー
      ((member 'cops edge) (princ "You ran into the cops. Game Over."))
      ;; 移動先のノードにwumpusがいた場合、
      ;;   攻撃していたら、wumpusを見つけた
      ;;   攻撃していなかったら、wumpusに追いついた
      ((member 'wumpus node) (if charging
                                 (princ "You found the Wumpus!")
                                 (princ "You ran into the Wumpus")))
      ;; 最後の弾丸を無駄にしたらゲームオーバー
      (charging (princ "You wasted your last bullet. Game Over!"))
      ;; Glow Wormに出会ったらランダムな場所に飛ばされる
      (has-worm (let ((new-pos (random-node)))
                  (princ "You ran into a Glow Worm Gang! You're now at ")
                  (princ new-pos)
                  (handle-new-place nil new-pos nil))))));}}}


;;; ---------------------------------------------------------------
;;; ゲーム開始
;;; ---------------------------------------------------------------

;; カレントディレクトリを変更する
(ext:cd "~/github/LandOfLisp/LandOfLisp-GrandTheftWumpus/")

;; graphviz用のユーティリティをロードする
(load "GraphMaker.lisp")

;; graphvizが生成するファイルを格納するディレクトリを作成する
(ensure-directories-exist
  (make-pathname :directory
                 (append
                   (pathname-directory (ext:default-directory))
                   (list *game-directory*))))

