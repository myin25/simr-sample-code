/*
* Created: October 4, 2022
* Written by: Melody Yin
* Expert: Sophia Zhu
* Implementation of 'think of an animal' game, where the user thinks of an animal
*    and the expert system will ask yes/no questions, which the user can answer with
*    yes, no, unknown or y/n/u in uppercase or lowercase. The expert system will accept
*    any kind of input that begins with y, n, or u, independent of capitalization. 'f' is also
*    accepted and will return the list of facts.
* Implementation approach: The system is primarily forward chained for general characteristics, such as
*    where the animal lives, if it's a mammal, etc. From there, a series of more specific characteristics
*    that distinguish it from relatives are backwards chained. The reason that they aren't forwards
*    chained is because some characteristics that would be important for distinguishing two animals (such as
*    discriminating between a flamingo and other birds by asking if it's pink) would be completely
*    irrelevant for other animals (ex: you wouldn't distinguish between a raven and a swan by asking if
*    it's pink or not).
* Limitations: This system is generally limited in scope, considering that approximately 30 animals have been
*    implemented. Two groups of animals that it does not include are amphibians and insects. The majority of
*    the animals within the system's capacity to guess are land mammals or birds.
*/ 

; reset conditions to default, import utilities
(clear)
(reset)
(batch "utilities_v4.clp")

; reset number of questions left to ask to 20
(defglobal ?*NUM_QASKED* = 20)

; do-backward-chaining
(do-backward-chaining red)
(do-backward-chaining pink)
(do-backward-chaining intelligent)
(do-backward-chaining quills)
(do-backward-chaining move)
(do-backward-chaining edible)
(do-backward-chaining eel)
(do-backward-chaining fox)
(do-backward-chaining tail)
(do-backward-chaining social)
(do-backward-chaining fur)
(do-backward-chaining domesticated)
(do-backward-chaining near_water)

/**************************************
* functions
*/

/*
* function: takes in user response to question and returns it as Y, N, or U. If the
*    user inputs something other than Y, N, or U, the function will repeatedly ask for
*    a valid input.
*/

(deffunction userInterface (?question)
   ; decrement the number of questions left and check to see if there are questions left
   ;    if there are no questions left, halt the game.
   (-- ?*NUM_QASKED*)
   (end_or_not)

   ; get user input in response to ?question. as long as user input isn't Y/N/U,
   ;    keep asking user for answer to the question.
   ; exception: if user input is F, return facts but ask for answer to question still
   (bind ?input (userInterface-helper ?question))
   (while (= (isValidInput ?input) FALSE) do
      (if (= ?input "F") then (facts)
       else (printline "Invalid input."))
      (bind ?input (userInterface-helper ?question)))

   ; convert input (which was previously a string) into a token
   (if (= ?input "Y") then (bind ?input Y))
   (if (= ?input "N") then (bind ?input N))
   (if (= ?input "U") then (bind ?input U))

   (return ?input)
); userInterface function

/*
* helper function: takes in user response to question and returns the first letter capitalized.
*/
(deffunction userInterface-helper (?question)
   (bind ?input (askline ?question))
   (return (upcase (sub-string 1 1 ?input)))
); userInterface-helper function

/*
* helper function: returns TRUE if the user input is valid (is Y, N, or U) and FALSE otherwise
*/
(deffunction isValidInput (?input)
   (bind ?output (= ?input "Y"))
   (if (= ?input "N") then (bind ?output TRUE))
   (if (= ?input "U") then (bind ?output TRUE))
   (return ?output)
); isValidInput function

/*
* function: Resets number of questions asked to 0
*/
(deffunction start ()
   ; remains as Y until all 20 questions have been asked
   (assert (has-questions-left Y))
); start function

/*
* function: takes in user response to final animal guess and celebrates/mourns accordingly
*/
(deffunction final_question (?question)
   ; get user input
   (bind ?input (userInterface-helper ?question))

   ; same invalid input loop as userInterface
   (while (= (isValidInput ?input) FALSE) do
      (if (= ?input "F") then (facts)
       else (printline "Invalid input."))
      (bind ?input (userInterface-helper ?question)))

   ; responding depending on if the guess was correct.
   (if (= ?input "Y") then (printline "Yay, I win! "))
   (if (= ?input "N") then (printline "I lose :("))
   (if (= ?input "U") then (printline "How do you not know your own animal..."))

   (printline (str-cat "Questions left: " ?*NUM_QASKED*))

   ; end the game
   (bind ?*NUM_QASKED* 0)
   (end_or_not)
); userInterface function

/*
* function: ends the game. only fires if there are no questions left or if an animal was guessed.
*/
(deffunction end_or_not ()
   (if (= ?*NUM_QASKED* 0) then (halt))
)

/**************************************
* rules
*/

; NEED-ATTRIBUTE rules: these are attributes that define a specific animal
(defrule red 
   (need-red ?)
=>
   (assert (red (userInterface "Is it red?")))
)

(defrule pink 
   (need-pink ?)
=>
   (assert (pink (userInterface "Is it pink?")))
)

(defrule intelligent "Able to imitate human speech"
   (need-intelligent ?)
=>
   (assert (intelligent (userInterface "Is it intelligent (able to imitate human speech)?")))
)

(defrule domesticated
   (need-domesticated ?)
   (water ?x &~Y)
=>
   (bind ?ans (userInterface "Is it domesticated?"))
   (assert (domesticated ?ans))
   (if (= ?ans Y) then
      (assert (ursidae N))
      (assert (snake   N))) ; snakes/bears are not domesticated
)

(defrule quills 
   (need-quills ?) 
   (water  N)
   (rodent Y)
=>
   (assert (quills (userInterface "Does it have quills?")))
)

(defrule move "Can it move on its own?"
   (need-move ?)
=>
   (assert (move (userInterface "Can it move on its own?")))
)

(defrule edible "Do we COMMONLY eat it? (different from if it's just edible)"
   (need-edible ?)
=>
   (assert (edible (userInterface "Do we commonly eat it?")))
)
(defrule tail 
   (need-tail ?)
=>
   (assert (tail (userInterface "Does it have a tail?")))
)

(defrule social 
   (need-social ?)
=>
   (assert (social (userInterface "Is it a social creature (doesn't live alone/in pairs)?")))
)

(defrule fox 
   (need-fox ?)
   (canine ?x &~N)
=>
   (assert (fox (userInterface "Is it a kind of fox?")))
)

(defrule eel 
   (need-eel ?)
   (fish ?x &~N)
=>
   (assert (eel (userInterface "Is it an eel?")))
)

(defrule fur
   (need-fur ?)
=>
   (assert (fur (userInterface "Does it have a coat of fur?")))
)

(defrule near_water
   (need-near_water ?)
   (water           N)
=>
   (assert (water (userInterface "Does it live near water?")))
)

; STARTING AND ENDING RULES: these statements should introduce and end the game
/*
* Prints out introductory statement with instructions for user (use Y/N/U to respond to prompts)
*/

(defrule start "Starts the game by giving instructions"
   (declare (salience 1))
=>
   (printline "Animal Game: Think of an animal and we'll play 20 questions! Use Y for yes, U for unknown, and N for no.")
   (start)
)

(defrule end "When we've run out of rules"
   (declare (salience -1))
=>
   (printline "You've stumped me :(")
)

; ATTRIBUTES: all of the rules listed below ask about general characteristics of the animal

; general characteristics
(defrule water
=>
   (assert (water (userInterface "Is it aquatic?")))
)

(defrule mammal
   (water ?x)
=>
   (bind ?ans (userInterface "Is it a mammal?"))
   (assert (mammal ?ans))
   (if (= ?ans Y) then 
      (assert (fish N))
      (assert (crustacean N)) 
      (assert (cnidaria N)) 
      (assert (bird N)) 
      (assert (snake N)))
   (if (= ?ans N) then 
      (assert (water_rodent N)) 
      (assert (ursidae N)) 
      (assert (canine N)) 
      (assert (feline N))
      (assert (land_rodent N)) 
      (assert (rabbit_relative N)))
)

; WATER
(defrule fish
   (not (fish ?x))
   (water Y)
=>
   (bind ?ans (userInterface "Is it a fish?"))
   (assert (fish ?ans))
   (if (= ?ans Y) then 
      (assert (crustacean N)) 
      (assert (cnidaria N)))
)

(defrule crustacean
   (not (crustacean ?x))
   (water ?x &~N) 
=>
   (bind ?ans (userInterface "Is it a crustacean?"))
   (assert (crustacean ?ans))
   (if (= ?ans Y) then 
      (assert (fish N)) 
      (assert (cnidaria N)))
)

(defrule cnidaria
   (not (cnidaria ?x))
   (water Y)
=>
   (bind ?ans (userInterface "Is it cnidaria?"))
   (assert (cnidaria ?ans))
   (if (= ?ans Y) then 
      (assert (crustacean N)) 
      (assert (fish N)))
)

(defrule water_rodent
   (not (water_rodent ?x))
   (water Y)
=>
   (assert (rodent (userInterface "Is it a rodent?")))
)

; GEOGRAPHY
(defrule equator "Asks the user if it lives near the equator"
   (not (equator ?x))
   (water ?x)
=>
   (bind ?ans (userInterface "Is it primarily found near the equator?"))
   (assert (equator ?ans))
   (if (= ?ans Y) then 
      (assert (asia N)) 
      (assert (north_america N)))
)

(defrule amazon "Asks the user if it lives in the Amazon"
   (not (amazon ?x))
   (equator Y)
   (water   Y)
=>
   (assert (amazon (userInterface "Does it live in the Amazon river?")))
)

(defrule north_america "Asks the user if it primarily found in North America"
   (water ?x)
   (not (north_america ?x))
=>
   (bind ?ans (userInterface "Is it primarily found in North America?"))
   (assert (north_america ?ans))
   (if (= ?ans Y) then 
      (assert (asia N)) 
      (assert (africa N)) 
      (assert (equator N)))
)

(defrule asia "Asks the user if it is from Asia"
   (water ?x)
   (not (asia ?x)) ; asia hasn't been asserted yet
=>
   (bind ?ans (userInterface "Is it from Asia?"))
   (assert (asia ?ans))
   (if (= ?ans Y) then 
      (assert (north_america N)) 
      (assert (africa N)) 
      (assert (equator N)))
)

(defrule africa "Asks the user if it primarily found in Africa"
   (water ?x)
   (not (africa ?x)) ; africa hasn't been asserted yet
   (equator ?x &~N)
=>
   (bind ?ans (userInterface "Is it primarily found in Africa?"))
   (assert (africa ?ans))
   (if (= ?ans Y) then 
      (assert (north_america N)) 
      (assert (asia N)))
)

; LAND
(defrule bird
   (water ?x &~Y)
   (mammal N)
   (not (bird ?x))
=>
   (bind ?ans (userInterface "Is it a bird?"))
   (if (= ?ans Y) then 
      (assert (ursidae N)) 
      (assert (canine N)) 
      (assert (feline N))
      (assert (rodent N)) 
      (assert (rabbit_relative N)) 
      (assert (snake N)))
   (assert (bird ?ans))
)

(defrule ursidae
   (water N)
   (mammal ?x &~N)
   (not (ursidae ?N))
=>
   (bind ?ans (userInterface "Is it a member of ursidae? (bear family)"))
   (assert (ursidae ?ans))
   (if (= ?ans Y) then 
      (assert (canine N)) 
      (assert (feline N)) 
      (assert (rodent N)))
   (assert (bird N))
)

(defrule canine
   (water N)
   (mammal ?x &~N)
   (not (canine ?N))
=>
   (bind ?ans (userInterface "Is it a canine?"))
   (assert (canine ?ans))
   (if (= ?ans Y) then 
      (assert (ursidae N)) 
      (assert (feline N)) 
      (assert (rodent N)))
   (assert (bird N))
)

(defrule feline
   (water N)
   (mammal ?x &~N)
   (not (feline ?N))
=>
   (bind ?ans (userInterface "Is it a feline?"))
   (assert (feline ?ans))
   (if (= ?ans Y) then 
      (assert (ursidae N))
      (assert (canine N)) 
      (assert (rodent N)))
   (assert (bird N))
)

(defrule land_rodent
   (water N)
   (mammal ?x &~N)
   (not (rodent ?N))
=>
   (bind ?ans (userInterface "Is it a rodent/rodent-like?"))
   (assert (rodent ?ans))
   (if (= ?ans Y) then 
      (assert (ursidae N)) 
      (assert (feline N)) 
      (assert (canine N)))
   (assert (bird N))
)

(defrule rabbit_relative
   (mammal ?x &~N)
   (rodent ?y &~N)
=>
   (assert (rabbit_relative (userInterface "Is it related to a rabbit?")))
)

; can live on water or land
(defrule snake
   (mammal N)
   (water ?x)
=>
   (bind ?ans (userInterface "Is it a snake?"))
   (assert (snake ?ans))
   (if (= ?ans Y) then 
      (assert (rabbit_relative N)) 
      (assert (land_rodent N)) 
      (assert (feline N)) 
      (assert (canine N))
      (assert (ursidae N)) 
      (assert (bird N)) 
      (assert (water_rodent N)) 
      (assert (cnidaria N)) 
      (assert (crustacean N))
      (assert (fish N)))
)

; FINAL ANIMAL RULES: guesses the animal
; WATER
(defrule piranha
   (fish   Y) 
   (amazon Y)
=>
   (assert (piranha (final_question "Is it a piranha?")))
)

(defrule guppy
   (fish    Y)               
   (equator Y)
=>
   (assert (guppy (final_question "Is it a guppy?")))
)

(defrule tuna
   (fish   Y) 
   (amazon N)        ; lives in the tropics but not in the amazon
   (edible Y)
=>
   (assert (tuna (final_question "Is it a tuna?")))
)

(defrule moray_eel
   (eel     Y)
   (equator Y)
=>
   (assert (moray_eel (final_question "Is it a moray eel?")))
)

(defrule pufferfish 
   (fish   Y)
   (quills N)
=>
   (assert (pufferfish (final_question "Is it a pufferfish?")))
)

(defrule jellyfish 
   (cnidaria Y)
=>
   (assert (jellyfish (final_question "Is it a jellyfish?")))
)

(defrule barnacle
   (crustacean Y)
   (move N)
=>
   (assert (barnacle (final_question "Is it a barnacle?")))
)

(defrule crab
   (crustacean ?x &~N)
   (move Y)
=>
   (assert (crab (final_question "Is it a crab?")))
)

; mammals
(defrule beaver 
   (water  Y)
   (rodent Y)
=>
   (assert (beaver (final_question "Is it a beaver?")))
)
 
(defrule whale 
   (water  Y)
   (mammal Y)
   (fur    N)
=>
   (assert (whale (final_question "Is it a whale?")))
)

; LAND
; felines
(defrule housecat
   (feline       Y)
   (domesticated Y)
=>
   (assert (housecat (final_question "Is it a housecat?")))
)

(defrule lynx
   (feline        Y)
   (north_america Y)
   (domesticated  N)
=>
   (assert (lynx (final_question "Is it a lynx?")))
)

(defrule puma
   (feline Y)
   (equator      ?x &~N)
   (domesticated N)
=>
   (assert (puma (final_question "Is it a puma?")))
)

; ursidae
(defrule grizzly_bear
   (ursidae ?x &~N)
   (north_america Y)
=>
   (assert (grizzly_bear (final_question "Is it a grizzly bear?")))
)

(defrule panda
   (ursidae ?x &~N)
   (asia Y)
=>
   (assert (panda (final_question "Is it a panda?")))
)

; canines
(defrule african_wild_dog 
   (canine       Y)
   (africa       Y)
   (domesticated N)
=>
   (assert (african_wild_dog (final_question "Is it an African wild dog?")))
)

(defrule red_fox 
   (fox Y) 
   (red ?x &~N) 
=>
   (assert (red_fox (final_question "Is it a red fox?")))
)

; small furry/hairy things
(defrule hedgehog 
   (rodent ?x &~Y)
   (quills ?y &~N)
=>
   (assert (hedgehog (final_question "Is it a hedgehog?")))
)

(defrule porcupine
   (rodent ?x &~N)
   (quills ?x &~N)
=>
   (assert (porcupine (final_question "Is it a porcupine?")))
)

(defrule pika 
   (rabbit_relative ?x &~N)
   (tail N)
=>
   (assert (pika (final_question "Is it a pika?")))
)

(defrule hare 
   (rabbit_relative ?x &~N)
   (social          ?x &~Y)
   (tail Y)
=>
   (assert (hare (final_question "Is it a hare?")))
)

(defrule rabbit 
   (rabbit_relative ?x &~N)
   (social Y)
   (tail   Y)
=>
   (assert (rabbit (final_question "Is it a rabbit?")))
)

(defrule hairless_dog 
   (canine  Y)
   (equator Y)
   (fur     N)
=>
   (assert (hairless_dog (final_question "Is it an hairless dog?")))
)

; misc. mammals
(defrule pig 
   (mammal Y)
   (fur    N)
   (pink   Y)
=>
   (assert (rabbit (final_question "Is it a rabbit?")))
)

; birds
(defrule raven 
   (bird          Y)
   (north_america Y)
=>
   (assert (raven (final_question "Is it a raven?")))
)

(defrule parrot 
   (bird         Y)
   (equator      Y)
   (domesticated N)
=>
   (assert (parrot (final_question "Is it a parrot?")))
)

(defrule parakeet 
   (bird         Y)
   (equator      Y)
   (domesticated Y)
=>
   (assert (parakeet (final_question "Is it a parakeet?")))
)

(defrule flamingo 
   (bird   Y)
   (africa Y)
   (pink   Y)
=>
   (assert (flamingo (final_question "Is it a flamingo?")))
)

(defrule swan 
   (bird          Y)
   (north_america Y)
   (water         Y)
=>
   (assert (swan (final_question "Is it a swan?")))
)

(defrule chicken 
   (bird         Y)
   (asia         Y)
   (domesticated Y)
=>
   (assert (chicken (final_question "Is it a chicken?")))
)

; snakes
(defrule anaconda 
   (water  Y)
   (snake  Y)
   (amazon Y)
=>
   (assert (flamingo (final_question "Is it an anaconda?")))
)

(defrule black_mamba 
   (water  N)
   (snake  Y)
   (africa Y)
=>
   (assert (black_mamba (final_question "Is it a black mamba?")))
)

(defrule cottonmouth 
   (water         Y)
   (snake         Y)
   (north_america Y)
=>
   (assert (cottonmouth (final_question "Is it a cottonmouth (water moccasin)?")))
)
