/*
* October 4, 2022
* Melody Yin
* Implementation of 'think of an animal' game, where the user thinks of an animal
*    and the expert system will ask yes/no questions, which the user can answer with
*    yes, no, unknown or y/n/u in uppercase or lowercase. The expert system will accept
*    any kind of input that begins with y, n, or u, independent of capitalization.
*/ 

; reset conditions to default, import utilities
(clear)
(reset)
(batch "utilities_v4.clp")
(defglobal ?*NUM_QASKED* = 0)

/**************************************
* functions
*/

/*
* function: takes in user response to question and returns the first letter capitalized.
*/
(deffunction userInterface-helper (?question)
   (bind ?input (askline ?question))
   (return (upcase (sub-string 1 1 ?input)))
); userInterface-helper function

/*
* function: returns TRUE if the user input is valid (is Y, N, or U) and FALSE otherwise
*/
(deffunction isValidInput (?input)
   (bind ?output (= ?input "Y"))
   (if (= ?input "N") then (bind ?output TRUE))
   (if (= ?input "U") then (bind ?output TRUE))
   (return ?output)
); isValidInput function

/*
* Resets number of questions asked to 0
*/
(deffunction start ()
   ; remains as Y until all 20 questions have been asked
   (assert (has-questions-left Y))
)

/*
* Checks if the number of questions asked is under 19. If not, assert has-questions-left
*    as N and immediately guess the animal.
*/
(deffunction update_questions-left ()
   (if (>= ?*NUM_QASKED* 19) then (assert (has-questions-left N)))
)

/*
* function: takes in user response to question and returns it as Y, N, or U. If the
*    user inputs something other than Y, N, or U, the function will repeatedly ask for
*    a valid input.
*/
(deffunction userInterface (?question)
   (++ ?*NUM_QASKED*)
   (update_questions-left)

   (bind ?input (userInterface-helper ?question))

   (while (= (isValidInput ?input) FALSE) do
      (printline "Invalid input.")
      (bind ?input (userInterface-helper ?question)))

   ; convert input (which was previously a string) into a token
   (if (= ?input "Y") then (bind ?input Y))
   (if (= ?input "N") then (bind ?input N))
   (if (= ?input "U") then (bind ?input U))

   (return ?input)
); userInterface function

/*
* function: takes in user response to final animal guess and celebrates accordingly
*/
(deffunction final_question (?question)
   (bind ?input (userInterface-helper ?question))

   (while (= (isValidInput ?input) FALSE) do
      (printline "Invalid input.")
      (bind ?input (userInterface-helper ?question)))

   (if (= ?input "Y") then (printline "Yay, I win!"))
   (if (= ?input "N") then (printline "I lose :("))
   (if (= ?input "U") then (printline "How do you not know your own animal..."))

   (halt)
); userInterface function

/**************************************
* rules
*

; ATTRIBUTES: all of the rules listed below ask about characteristics of the animal
/*
* Prints out introductory statement with instructions for user (use Y/N/U to respond to prompts)
*/

(defrule start "Starts the game by giving instructions"
=>
   (printline "Animal Game: Think of an animal and we'll play 20 questions! Use Y for yes, U for unknown, and N for no.")
   (start)
)

(defrule water "Asks if it lives in water"
   (has-questions-left Y)
=>
   (assert (water (userInterface "Does it live in water?")))
)

(defrule fish 
   (water Y | U)
   (has-questions-left Y)
=>
   (assert (fish (userInterface "Is it a kind of fish?")))
)

(defrule move "Asks the user if it can move on its own"
   (fish N | U)
   (has-questions-left Y)
=>
   (assert (move (userInterface "Can it move on its own?")))
)

(defrule bioluminescent 
   (fish N | U)
   (has-questions-left Y)
=>
   (assert (bioluminescent (userInterface "Is it bioluminescent?")))
)

(defrule equator "Asks the user if it lives near the equator"
   (has-questions-left Y)
=>
   (assert (equator (userInterface "Does it live near the equator?")))
)

(defrule ugly 
   (has-questions-left Y)
=>
   (assert (ugly (userInterface "Is it ugly?")))
)

(defrule north_america "Asks the user if it primarily found in North America"
   (water N | U)   ; execute if it was a terrestrial animal
   (equator N | U) ; execute if it isn't near the equator
   (has-questions-left Y)
=>
   (assert (north_america (userInterface "Is it primarily found in North America?")))
)

(defrule fur
   (water N | U)
   (has-questions-left Y)
=>
   (assert (fur (userInterface "Does it have fur?")))
)

(defrule bird 
   (water N | U)   ; birds don't live in the water
   (has-questions-left Y)
=>
   (assert (bird (userInterface "Is it a bird?")))
)

(defrule canine 
   (water N | U)  ; canines don't live in the water
   (bird N | U) ; if it isn't a feline, ask if it's a canine
   (has-questions-left Y)
=>
   (assert (canine (userInterface "Is it a canine?")))
)

(defrule fox 
   (canine Y | U) ; execute if it's a canine
   (has-questions-left Y)
=>
   (assert (fox (userInterface "Is it a kind of fox?")))
)

(defrule red 
   (water N | U)
   (has-questions-left Y)
=>
   (assert (red (userInterface "Is it red?")))
)

(defrule paws 
   (water N | U)
   (bird N | U)
   (has-questions-left Y)
=>
   (assert (paws (userInterface "Does it have paws?")))
)

(defrule rabbit_relative 
   (water N | U)
   (fur Y | U)
   (has-questions-left Y)
=>
   (assert (rabbit_relative (userInterface "Is it related to a rabbit?")))
)

(defrule rodent 
   (water N | U)
   (bird N | U) 
   (has-questions-left Y)
=>
   (assert (rodent (userInterface "Is it a rodent?")))
)

(defrule quills 
   (rodent Y | U) 
   (has-questions-left Y)
=>
   (assert (quills (userInterface "Does it have quills?")))
)

(defrule crustacean
   (water Y | U) 
   (fish N | U)
   (has-questions-left Y)
=>
   (assert (crustacean (userInterface "Is it a crustacean?")))
)

; FINAL ANIMAL RULES: guesses the animal
(defrule piranha
   (fish Y | U)                    ; fish (that lives in water)
   (equator Y | U)                 ; lives near the equator
   (ugly Y | U)                    ; ugly
=>
   (assert (piranha (final_question "Is it a piranha?")))
   (halt)
)

(defrule red_fox 
   (red Y | U) ; red
   (fox Y | U) ; fox
=>
   (assert (red_fox (final_question "Is it a red fox?")))
)

(defrule jellyfish 
   (bioluminescent Y | U)
   (move N | U)
=>
   (assert (jellyfish (final_question "Is it a jellyfish?")))
)

(defrule pika 
   (rabbit_relative Y | U)
=>
   (assert (pika (final_question "Is it a pika?")))
)

(defrule flamingo 
   (bird Y | U)
=>
   (assert (flamingo (final_question "Is it a flamingo?")))
)

(defrule hedgehog 
   (rodent Y | U)
   (quills Y | U)
   (ugly N | U)
=>
   (assert (hedgehog (final_question "Is it a hedgehog?")))
)

(defrule porcupine
   (rodent Y | U)
   (quills Y | U)
   (ugly Y | U)
=>
   (assert (porcupine (final_question "Is it a porcupine?")))
)

(defrule crab
   (crustacean Y | U)
=>
   (assert (crustacean (final_question "Is it a crab?")))
)
