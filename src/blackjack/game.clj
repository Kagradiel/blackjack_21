(ns blackjack.game
  (:require [card-ascii-art.core :as card]))

; A, 2, 3, 4, 5, 6, 7, 8, 9, 10, Q, J, K
;1...13
(defn new-card []
  "Generate a card number between 1 and 13"
  (inc (rand-int 13)))

; sum the points
; J, Q, K = 10 (Not 11, 12, 13)
; [A 10] = 11 or 21 = 21
; [A 5 7] = 1+5+7(13) or 11+5+7 (23)
; A = 11, if returns more than 21, value 1

(defn JQK->10 [card]
  (if (> card 10) 10 card))

(defn A->11 [card]
  (if (= card 1) 11 card))

(defn points-cards [cards]
  (let [cards-without-JQK (map JQK->10 cards)
        cards-with-a11 (map A->11 cards-without-JQK)
        points-with-A-1 (reduce + cards-without-JQK)
        points-with-A-11 (reduce + cards-with-a11)]
    (if (> points-with-A-11 21) points-with-A-1 points-with-A-11)))


(defn player [player-name]
  (let [card1 (new-card)
        card2 (new-card)
        cards [card1 card2]
        points (points-cards cards)]
    {:player-name player-name
     :cards       cards
     :points      points}))

;call new-card to generate a new card
;update array card from player with new card
;sum player points with new card
(defn more-card [player]
  (let [card (new-card)
        cards (conj (:cards player) card)
        new-player (update player :cards conj card)
        points (points-cards cards)]
    (assoc new-player :points points)))

(defn player-decision-continue? [player]
  (= (read-line) "sim"))

(defn dealer-decision-continue? [player-points dealer]
  (let [dealer-points (:points dealer)]
    (if (> player-points 21) false (<= dealer-points player-points))))

(defn game [player fn-decision-continue?]
  (println (:player-name player) " quer mais uma carta?")
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (more-card player)]
      (card/print-player player-with-more-cards)
      (recur player-with-more-cards fn-decision-continue?))
    player))

(defn endgame [player dealer]
  (let [player-points (:points player)
        dealer-points (:points dealer)
        player-name (:player-name player)
        dealer-name (:player-name dealer)
        message (cond
                  (and (> player-points 21) (> dealer-points 21)) "Ambos perderam"
                  (= player-points dealer-points) "Empatou"
                  (> player-points 21) (str dealer-name " ganhou")
                  (> dealer-points 21) (str player-name " ganhou")
                  (> player-points dealer-points) (str player-name " ganhou")
                  (> dealer-points player-points) (str dealer-name " ganhou"))]
    (card/print-player player)
    (card/print-player dealer)
    (print message)))

(def player-1 (player "Matheus"))
(card/print-player player-1)

(def dealer (player "Dealer"))
(card/print-masked-player dealer)

(def player-after-game (game player-1 player-decision-continue?))
(def dealer-after-game (game dealer (partial dealer-decision-continue? (:points player-after-game))))

(endgame player-after-game dealer-after-game)

