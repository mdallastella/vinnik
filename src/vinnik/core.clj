(ns vinnik.core
  (:require [vinnik.board :as board]))

(def initial-state
  {:board board/initial-board
   :player :white
   :turn 1})

(defn -main []
  (-> initial-state
      :board
      board/pprint))
