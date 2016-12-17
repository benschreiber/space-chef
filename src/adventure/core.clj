(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:dorm {:desc "You are in the cramped living quarters of the station. Your pillow is on your bed."
          :title "in the dormitory"
          :dir {:north :cargo
                :west :lab
                :south :showers}
          :contents #{:pillow}
          :examine "Bunk beds are the second coolest part of living in space."}
   :mess {:desc "There are a few tables and a locked kitchen. There is ketchup on a table."
          :title "in the mess hall"
          :dir {:north :showers
                :west :bridge
                :south :armory}
          :contents #{:ketchup}
          :examine "Even in the future, cafeteria food sucks."}
   :cargo {:desc "You see lots of boxes. Of what? Who knows!"
           :title "in the cargo bay"
           :dir {:north :armory
                 :south :dorm
                 :west :reactor}
           :contents #{:box}
           :examine "Seriously, what’s in those boxes?"}
   :brig  {:desc "There is an ominous metal door, but it’s locked."
           :title "in the brig"
           :dir {:north :bio
                 :south :gym
                 :east :reactor}
           :contents #{}
           :examine "They once threw a guy in there for complaining about potatoes."}
   :bridge {:desc "You see a few comfy chairs, some monitors, and a big window."
            :title "on the bridge"
            :dir {:north :deck
                  :south :airlock
                  :west :library
                  :east :mess}
            :contents #{}
            :examine "Disappointing lack of giant steering wheel."}
   :gym   {:desc "There are some weights are lying on the floor."
           :title "in the gym"
           :dir {:north :brig
                 :south :med
                 :east :lab}
           :contents #{:weight}
           :examine "The coriolis force makes volleyball really weird."}
   :armory  {:desc "Plasma rifles are neatly stacked against the wall."
             :title "in the armory"
             :dir {:north :mess
                   :south :cargo
                   :west :airlock}
             :contents #{:rifle}
             :examine "Not exactly child-safe."}
   :med   {:desc "There are standard medical supplies. You spot some scalpels on a table."
           :title "in the med bay"
           :dir {:north :gym
                 :south :library
                 :east :deck}
           :contents #{:scalpel}
           :examine "There are robots for medicine. The scalpels are for fun."}
   :lab   {:desc "You see some benches with strange goop growing in petri dishes."
           :title "in the lab"
           :dir {:north :reactor
                 :south :deck
                 :west :gym
                 :east :dorm}
           :contents #{:goop}
           :examine "Science!"}
   :airlock {:desc "You don’t have a space suit."
             :title "by the airlock"
             :dir {:north :bridge
                   :south :reactor
                   :west :bio
                   :east :armory}
             :contents #{}
             :examine "Sorry, but no, you can't go outside."}
   :bio   {:desc "The station’s food is grown here with hydroponics. Well, really just potatoes."
           :title "in the biosphere"
           :dir {:north :library
                 :south :brig
                 :east :bridge}
           :contents #{:potato}
           :examine "You didn’t always hate potatoes."}
   :reactor {:desc "A glowing blue core pulses. There’s a sack of fuel pellets in the corner."
             :title "by the reactor"
             :dir {:north :airlock
                   :south :lab
                   :west :brig
                   :east :cargo}
             :contents #{:pellet}
             :examine "Is that the best place for the fuel pellets? Probably not."}
   :library {:desc "There is a variety of technology, including a stack of universal translators."
             :title "in the library"
             :dir {:north :med
                   :south :bio
                   :east :bridge}
             :contents #{:translator}
             :examine "Yes, there are still paper books in the future."}
   :deck    {:desc "The little furry thing is sitting on the floor, staring at you."
             :title "on the observation deck. The furry thing is still there"
             :dir {:north :lab
                   :south :bridge
                   :west :med
                   :east :showers}
             :contents #{}
             :examine "The furry thing does NOT look happy."}
   :showers {:desc "You see a row of showers lined up. Bars of soap sit in trays."
             :title "in the showers"
             :dir {:north :dorm
                   :south :mess
                   :west :bridge}
             :contents #{:soap}
             :examine "Try not to think about the recycled water."}
  })

(def adventurer
  {:location :deck
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

; (defn to-keywords [commands]
;   (filterv (fn [x] (not (contains? #(:go :the :a :an :on) x))) (mapv keyword (str/split commands #"[.,?! ]+"))))
(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn tock [player]
  (case (player :tick)
    5 (println "You can hear the furry... thing... moaning")
    10 (println "The moaning is getting loader.")
    20 (println "The moaning is so loud that the walls are starting to vibrate")
    30 (println "The moaning is now painful. The creature is clearly in great pain.")
    45 (println "The walls are violently shaking from the moans. Alarms are starting to go off.")
    60 (do (println "The furry thing explodes, rupturing the hull. You are sucked into the cold")
           (println "vacuum of space. Better luck next time!")
           (System/exit 0))
    ())
  (update-in player [:tick] inc))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in (tock player) [:location] dest))))

;(defn takeitem [command player local-map]
;  (if (empty? command)
;    (do (println "what do you want to take?") player)
;    (if (((local-map (player :location)) :contents) (command 0))
;        (update-in player [:inventory] #(clojure.set/union % ((local-map (player :location)) :contents)))
;        (do (println "you can't take that.") player))))

(defn takeitem [item player local-map]
    (if (((local-map (player :location)) :contents) item)
        (do (println (str "You take the " (name item) ".")) 
            (update-in player [:inventory] conj item))
        (do (println "You can't take that.") player)))

(defn useitem [item1 item2 player]
  (if (not (and (-> player :inventory item1) (-> player :inventory item2)))
      (do (println "You don't have the right things to do that.") player)
      (cond (and (= item1 :scalpel) (= item2 :hot_potato)) 
                 (do (println "You cut up the hot potato into chunks.")
                     (update-in (update-in player [:inventory] #(disj % :hot_potato)) [:inventory] conj :chunks))
            (and (= item1 :hot_potato) (= item2 :scalpel))
                 (do (println "You cut up the hot potato into chunks.")
                     (update-in (update-in player [:inventory] #(disj % :hot_potato)) [:inventory] conj :chunks))
            (and (= item1 :ketchup) (= item2 :chunks))
                 (do (println "You put some ketchup on the chunks to make tangy chunks.")
                     (update-in (update-in player [:inventory] #(disj % :chunks)) [:inventory] conj :tangy_chunks))
            (and (= item1 :chunks) (= item2 :ketchup))
                 (do (println "You put some ketchup on the chunks to make tangy chunks.")
                     (update-in (update-in player [:inventory] #(disj % :chunks)) [:inventory] conj :tangy_chunks))
            :else (do (println "That's not a good idea. Trust me.") player))))
            

(defn giveitem [item player]
  (cond (not (-> player :inventory item))
            (do (println "You don't have that.") player)
        (= item :tangy_chunks) 
            (do (println "The furry thing looks up with delight. It munches on the tangy potato chunks, burps, then vanishes into a shimmery mist.")
                (println "On the ground, you see a strange, black puddle. You lean in for a closer look...")
                (println "\"My God! It's full of stars!\"")
                (println "You win! Thanks for playing!")
                (System/exit 0))
        (not (-> player :inventory :translator))
            (do (println "The furry thing chucks it back at you. It seems upset, but you have no idea what it is saying.") player)
        (= item :potato)
            (do (println "\"I can't eat this! It's too cold and hard! Heat it up for me!\"") player)
        (= item :hot_potato)
            (do (println "\"This is too big! It won't fit in my mouth! Divide it into smaller pieces!\"") player)
        (= item :chunks)
            (do (println "\"These are bland and flavorless! Make them more exciting!\"") player)
        :otherwise
            (do (println "\"This isn't what I want to eat at all! Bring me something else!\"") player)))
            

(defn talk [player]
  (if (= (player :location) :deck)
    (if ((player :inventory) :translator)
      (do (println "\"Human! I hunger! I must feed, or many will perish!\"") player)
      (do (println "\"QrrtUUlllf mEBSha! ZZzzzygio d'walma karapusht!\"") player))
    (do (println "There's nobody to talk to.") player)))

(defn shoot [item player]
  (if (nil? item)
    (println "What do you want to shoot?")
    (if ((player :inventory) :rifle)
        (cond 
          (= item :thing) 
            (if (= (player :location) :deck)
                (do (println "The furry thing explodes, taking out a good portion of the hull with it.")
                              (println "You are sucked into the cold vacuum of space. Maybe shooting it was a bad idea.")
                              (System/exit 0))
                (do (println "You can't see the furry thing from here.") player))
          (= item :potato) (if ((player :inventory) :potato)
                               (do (println "You heat up a potato.")
                                   (update-in (update-in player [:inventory] #(disj % :potato)) [:inventory] conj :hot_potato)) ; hot potato
                               (do (println "You need a potato before you can shoot one.") player))
          :otherwise  (do (println "It's probably a bad idea to shoot that.") player))
        (do (println "With what? Your fingers? Pew-pew!") player))))

(defn help [player]
  (do (println "Commands:")
      (println "north/south/east/west/n/s/e/w = Go in that direction")
      (println "look = Re-examine your surroundings.")
      (println "examine = Gain more information about a room.")
      (println "take [item] = Add an item from a room to your inventory")
      (println "search = Identify usable items near you.")
      (println "sack = View your inventory.")
      (println "use [item1] [item2] = Use item1 on item2")
      (println "give [item] = Give an item to whomever is in the room with you.")
      (println "shoot [target] = If you have a rifle, shoot something.")
      (println "punch [target] = Punch something. Tip: the furry thing is called \"thing\"")
      (println "hint1 = Only use this if you're really stuck!")
      (println "hint2 = Only use this if you're really *really* stuck!")
      (println "quit = Quit the game.")
      player))

(defn respond [player command local-map]
  (match (command 0)
         :look (update-in player [:seen] #(disj % (-> player :location)))
         (:or :n :north) (go :north player)
         (:or :s :south) (go :south player)
         (:or :e :east)  (go :east player)
         (:or :w :west)  (go :west player)
         :take (if (> (count command) 1)
                   (takeitem (command 1) player local-map)
                   (do (println "What do you want to take?") player))
         :help (help player)
         :examine (do (println ((local-map (player :location)) :examine)) player)
         :use (if (< (count command) 3)
                  (do (println "I'm not sure what you want to do.") player)
                  (useitem (command 1) (command 2) player))
         :search (if (seq ((local-map (player :location)) :contents))
                   (do (println "You see these items in the room:") 
                       (println ((local-map (player :location)) :contents)) 
                       ; (map #(print (name %)) ((local-map (player :location)) :contents))
                       player)
                   (do (println "You don't see anything useful") player))
         :give (if (not (= (player :location) :deck))
                   (do (println "There's nobody to give that to.") player)
                   (if (< (count command) 2)
                       (do (println "What do you want to give?") player)
                       (giveitem (command 1) player))) 
         :talk (talk player)
         :sack (do (println (player :inventory)) player)
         :shoot (if (> (count command) 1)
                    (shoot (command 1) player)
                    (do (println "What do you want to shoot?") player))
         :quit (System/exit 0)
         :punch (cond (< (count command) 2) (do (println "What do you want to punch?") player)
                      (= (command 1) :thing) (do (println "It explodes. You die. Boo-hoo.") (System/exit 0))
                      :otherwise (do (println "That won't be helpful.") player))
         :hint1 (do (println "If you're having trouble talking, check the library. Maybe stop for a potato to munch on the way?") player)
         :hint2 (do (println "I'm not sure what's more painful, the searing heat of a plasma rifle or the slice of a scalpel.") player)

         :else (do (println "I don't understand you.")
               player)

         )) 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "SPACE CHEF")
  (println "Ben Schreiber 2016 - Created for CS 296-25 @ UIUC")
  (println "")
  (println "It is 2178, and you are part of the elite team of scientists and soldiers at ARMSTRONG STATION,")
  (println "a massive space station positioned at the edge of the solar system. Your mission is to monitor")
  (println "the HUB, the wormhole that connects humanity to the rest of the universe. The rest of the crew")
  (println "has gone to meet a visiting delegation of Betelguesians, leaving you to hold down the fort.")
  (println "Except you're not alone. There's a little furry thing sitting on the observation deck,")
  (println "and it seems angry...")
  (println "")
  (println "Tip: try \"help\"")
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println " What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command) local-map)))))

