/*
* Created: December 15, 2022
* Written by: Melody Yin
* Expert: Mr. Walsh, Mr. Smith, and Mr. Douglas
* Implementation of a college counselor assistant. Supplements a counselor's recommendations that a
*    student can use to independently generate a university that they may be interested in,
*    based on factors like their interests and financial situation. The student will be asked a series
*    of questions ranging from ones that require numeric responses to yes/no questions. Each question
*    will specify what kind of response it expects.
*       Numeric responses: Questions like the student's SAT score will request a numeric answer.
*          In the case that a college does not require an SAT score, any answer that begins with
*          "o", independent of capitalization, will be accepted as opting out of sending in a score.
*          However, this will result in the removal of all colleges that require an SAT score.
*       Categorical responses: For questions like a student's major, the possible/expected answers will be
*          listed out following the question prompt.
*       Yes/No questions: Similar to the animals game project, the program will accept any answer,
*          independent of capitalization, beginning with y as yes and any answer beginning with
*          n as no. Any answer beginning with u will be interpreted as unknown. Any other answer will
*          be rejected as invalid input.
* Implementation approach: The system will be forward chained for generic questions (such as questions
*    about standardized testing and majors that the student is interested in). The generic information
*    will then be supplemented with more college-specific questions, such as if they are interested
*    in research, which a handful of schools are known to specialize in. 
* Limitations: The scope is limited to a list of 56 colleges.
*    They are the following:
*    Amherst College, Babson College, Rice University, San Jose State University, Boston College, Santa 
*    Clara University, Boston University, Stanford University, Brown University, Swarthmore College,
*    California Institute of Technology, The University of Texas at Austin, Tufts University, University of
*    California, Berkeley, Carnegie Mellon University, University of California, Davis, Case Western Reserve
*    University, University of California, Irvine, Chapman University, University of California, Los Angeles,
*    Claremont McKenna College, University of California, San Diego, University of California, Santa Barbara, 
*    Colombia University, Cornell University, University of Chicago, Dartmouth College, Duke University, 
*    University of Illinois at Urbana-Champaign, Emerson College, University of Maryland, Emory University,
*    George Washington University, University of Michigan, Georgetown University, Georgia Institute of 
*    Technology, Harvard University, Harvey Mudd College, University of Oregon, University of Pennsylvania,
*    Johns Hopkins University, Massachusetts Institute of Technology, New York University, University of
*    Toronto, University of Washington, University of Wisconsin, Vanderbilt University, Princeton University,
*    Purdue University, Yale University
*/

; reset conditions, import utilities
(clear)
(reset)
(batch "utilities_v4.clp")

; all uni names
(bind ?*UNI_NAMES* (create$ "Amherst College"            "Babson College"          
   "Rice University"        "San Jose State University"  "Boston College"
   "Santa Clara University" "Boston University"          "Stanford University"
   "Brown University"       "Swarthmore College"         "California Institute of Technology" 
   "The University of Texas at Austin"                   "Tufts University"
   "UC Berkeley"            "Carnegie Mellon University" "UC Davis"               
   "Case Western Reserve University"                     "UC Irvine"                  
   "Chapman University"     "UC Los Angeles"             "Claremont Mckenna College" 
   "UC San Diego"           "UC Santa Barbara"           "Colombia University"     
   "Cornell University"     "University of Chicago"      "Dartmouth University"   
   "Duke University"        "University of Illinois"     "Emerson College"            
   "University of Maryland" "Emory College"              "George Washington University" 
   "University of Michigan" "Georgetown University"      "Georgia Institute of Technology" 
   "Harvard University"     "Harvey Mudd College"        "University of Oregon" 
   "University of Pennsylvania"                          "Johns Hopkins University" 
   "Massachusetts Institute of Technology"               "New York University" 
   "University of Toronto"  "University of Washington"   "University of Wisconsin" 
   "Vanderbilt University"  "Princeton University"       "Purdue University" 
   "Yale University")
); all uni names

(bind ?*UNI_SHORTHAND* (create$ amherst           babson               rice
   sjsu        bostonc          scu               bostonu              stanford 
   brown       swarthmore       caltech           texasaustin          tufts 
   ucberkeley  carnegiemellon   ucdavis           casewesternreserve   ucirvine 
   chapman     ucla             claremontmckenna  ucsandiego           ucsantabarbara 
   colombia    cornell          uchicago          dartmouth            duke 
   uillinois   emerson          umaryland         emory                gwashington
   umichigan   georgetown       gitech            harvard              harveymudd 
   uoregon     upenn            johnshopkins      mit                  nyu 
   utoronto    uwashington      uwisconsin        vanderbilt           princeton 
   purdue      yale)
); uni_shorthand

; do-backward-chaining
(do-backward-chaining conservative)
(do-backward-chaining research)
(do-backward-chaining sports)
(do-backward-chaining sat_m)
(do-backward-chaining sat_e)
(do-backward-chaining doublemajor)
(do-backward-chaining networking)
(do-backward-chaining residentialsystem)
(do-backward-chaining needblind)
(do-backward-chaining international)
(do-backward-chaining competitive)
(do-backward-chaining theology)
(do-backward-chaining honors)
(do-backward-chaining corecurriculum)
(do-backward-chaining interdisciplinary)
(do-backward-chaining consortium)
(do-backward-chaining groupwork)
(do-backward-chaining early)
(do-backward-chaining party)
(do-backward-chaining flexible)
(do-backward-chaining independent)

/**************************************
* functions
*/

/*
* function: given the shorthand of a university, return the full name of the university.
* Input: shorthand of university
*    Example: babson
* Output: full name of university (string)
*    Example: "Babson University"
*/
(deffunction fullname (?shorthand)
   (return (nth$ (member$ ?shorthand ?*UNI_SHORTHAND*) ?*UNI_NAMES*))
); fullname

/*
* function: takes in user response to question and returns it as Y, N, or U. If the
*    user inputs something other than Y, N, or U, the function will repeatedly ask for
*    a valid input.
* Input: question to be asked
* Output: user input, if valid (Y/N/U)
*/
(deffunction userInterfaceYN (?question)
   ; get user input in response to ?question. as long as user input isn't Y/N/U,
   ;    keep asking user for answer to the question.
   ; exception: if user input is F, return facts but ask for answer to question still

   (bind ?input (userInterfaceYN-helper ?question))

   (while (= (isValidInputYN ?input) FALSE) do
      (if (= ?input "F") then (facts)
       else (printline "Invalid input. Please enter an answer that starts with Y, N, or U."))
      (bind ?input (userInterfaceYN-helper ?question)))

   ; convert input (which was previously a string) into a token
   (if (= ?input "Y") then (bind ?input Y))
   (if (= ?input "N") then (bind ?input N))
   (if (= ?input "U") then (bind ?input U))

   (return ?input)
); userInterfaceYN

/*
* helper function: takes in user response to question and returns the first letter capitalized
* Input: question to be asked
* Output: user input
*/
(deffunction userInterfaceYN-helper (?question)
   (bind ?input (askline ?question))
   (return (upcase (sub-string 1 1 ?input)))
); userInterfaceYN-helper

/*
* helper function: returns TRUE if the user input is valid (is Y, N, or U) and FALSE otherwise
*/
(deffunction isValidInputYN (?input)
   (bind ?output (= ?input "Y"))
   (if (= ?input "N") then (bind ?output TRUE))
   (if (= ?input "U") then (bind ?output TRUE))
   (return ?output)
); isValidInputYN

/*
* function: takes in user response to question and returns it. If the
*    user inputs something other than a given category, the function will repeatedly ask for
*    a valid input.
* Input: question to be asked and possible categories for user input
*/
(deffunction userInterfaceCateg (?question ?categories)
   ; get user input in response to ?question. as long as user input isn't one of the categories,
   ;    keep asking user for answer to the question.
   ; exception: if user input is F, return facts but ask for answer to question still

   (bind ?input (userInterfaceCateg-helper (sym-cat ?question " (Please enter one of the following categories. 
Possible Categories: " (toString ?categories) ")")))

   (while (= (numberp (member$ ?input ?categories)) FALSE) do
      (if (= ?input "F") then (facts)
       else (printline "Invalid input. Please enter one of the categories specified in the question."))
      (bind ?input (userInterfaceCateg-helper ?question)))

   (return ?input)
); userInterfaceCateg

/*
* helper function: takes in user response to question and returns the answer all lowercase
* Input: question to be asked
* Output: user input, converted to all lowercase
*/
(deffunction userInterfaceCateg-helper (?question)
   (bind ?input (askline ?question))
   (return (lowcase (sub-string 1 (str-length ?input) ?input)))
); userInterface-helper

/*
* Given a list, convert to a string representation. Ex: if (a b) is given, convert to "a, b".
*/
(deffunction toString (?items)
   (for (bind ?i 1) (< ?i (length$ ?items)) (++ ?i)
      (bind ?items (replace$ ?items ?i ?i (sym-cat (nth$ ?i ?items) ",")))
   )

   (bind ?items (replace$ ?items (length$ ?items) (length$ ?items) (sym-cat (nth$ (length$ ?items) ?items) "")))

   (return (implode$ ?items))
); toString

/*
* Given a list, convert to a string representation. Ex: if (a b) is given, convert to "a, b".
*/
(deffunction recommend (?college)
   (printline (sym-cat "I recommend " (fullname ?college)))
   (halt)
); recommend

/**************************************
* rules
*/

; backward chaining: aspects characteristic to specific colleges.
(defrule conservative 
   (need-conservative ?)
=>
   (assert (conservative (userInterface "Would you be comfortable in an environment that is more conservative?")))
)

(defrule research 
   (need-research ?)
=>
   (assert (research (userInterface "Are you interested in conducting research?")))
)

(defrule sports 
   (need-sports ?)
=>
   (assert (sports (userInterfaceYN "Are you interested in sports/actively participating in athletics?")))
)

(defrule sat_m
   (need-sat_m ?)
   (sat ?x)
=>
   (assert (sat_m (userInterfaceCateg "In what range is your math SAT score?" (create$ "200-300"
      "310-400" "410-500" "510-550" "560-600" "610-650" "660-670" "680-690" "700-710" "710-720" 
      "730-740" "750-760" "770-780" "790-800"))))
)

(defrule sat_e
   (need-sat_e ?)
   (sat ?x)
=>
   (assert (sat_e (userInterfaceCateg "In what range is your english SAT score?" (create$ "200-300"
      "310-400" "410-500" "510-550" "560-600" "610-650" "660-670" "680-690" "700-710" "710-720" 
      "730-740" "750-760" "770-780" "790-800"))))
)

(defrule doublemajor
   (need-doublemajor ?)
=>
   (assert (doublemajor (userInterfaceYN "Do you want to pursue a double major?")))
)

(defrule networking
   (need-networking ?)
=>
   (assert (networking (userInterfaceYN "Are you interested in networking with alumni?")))
)

(defrule residentialsystem
   (need-residentialsystem ?)
=>
   (assert (residentialsystem (userInterfaceYN "Are you looking for a residential system/ok with having to live on campus?")))
)

(defrule needblind
   (need-needblind ?)
=>
   (assert (needblind (userInterfaceYN "Are you looking for a need blind college? (Chooses based on merits)")))
)

(defrule international
   (need-international ?)
=>
   (assert (international (userInterfaceYN "Are you looking for an international college?")))
)

(defrule competitive
   (need-competitive ?)
=>
   (assert (competitive (userInterfaceYN "Are you looking for a competitive/cutthroat college?")))
)

(defrule theology
   (need-theology ?)
=>
   (assert (theology (userInterfaceYN "Are you comfortable with a core curriculum based on theology?")))
)

(defrule honors
   (need-honors ?)
=>
   (assert (honors (userInterfaceYN "Are you looking for a college with an honors program?")))
)

(defrule corecurriculum
   (need-corecurriculum ?)
=>
   (assert (corecurriculum (userInterfaceYN "Are you looking for a college with a core curriculum?")))
)

(defrule interdisciplinary
   (need-interdisciplinary ?)
=>
   (assert (interdisciplinary (userInterfaceYN "Are you looking for a college with a focus on interdisciplinary work?")))
)

(defrule consortium
   (need-consortium ?)
=>
   (assert (consortium (userInterfaceYN "Are you looking for a college that's part of a consortium?")))
)

(defrule groupwork
   (need-groupwork ?)
=>
   (assert (groupwork (userInterfaceYN "Are you looking for a college that focuses on group work?")))
)

(defrule early
   (need-early ?)
=>
   (assert (early (userInterfaceYN "Are you applying for early decision?")))
)

(defrule party
   (need-party ?)
=>
   (assert (party (userInterfaceYN "Would you attend a party school?")))
)

(defrule independent
   (need-independent ?)
=>
   (assert (independent (userInterfaceYN "Are you an independent student?")))
)

(defrule flexible
   (need-flexible ?)
=>
   (assert (flexible (userInterfaceYN "Are you looking for a flexible curriculum?")))
)


/*
* Prints out introductory statement with instructions for user
*/

(defrule start "Starts the game by giving instructions"
   (declare (salience 1))
=>
   (printline "College recommending program! Enter Y/N/U for yes/no questions. For categorical questions where a list of possible answers are specified by the question, please choose one category. For numerical questions, please enter a number appropriate to the value asked for in the question. If you have no preference, choose Unknown.")
)

(defrule end "When there are no more rules"
   (declare (salience -1))
=>
   (printline "Unfortunately, there are no colleges that I know of which satisfy your requirements.")
)

; general questions
(defrule geography "Asks the user to pick one of the states or an international school."
=>
   (assert (geography (userInterfaceCateg "Where would you want to study? Choose Unknown if you have no preference" 
      (create$        "alabama"        "alaska"        "arizona"        "arkansas" 
      "california"    "colorado"       "connecticut"   "delaware"       "florida" 
      "georgia"       "idaho"          "illinois"      "indiana"        "iowa" 
      "kansas"        "kentucky"       "louisiana"     "maine"          "maryland" 
      "massachusetts" "michigan"       "minnesota"     "mississippi"    "missouri" 
      "montana"       "nebraska"       "nevada"        "new hampshire"  "new jersey" 
      "new york"      "north carolina" "north dakota"  "ohio"           "oklahoma" 
      "oregon"        "pennsylvania"   "rhode island"  "south carolina" "south dakota" 
      "tennessee"     "texas"          "utah"          "vermont"        "virginia" 
      "washington"    "west virginia"  "wisconsin"     "wyoming"        "canada" 
      "unknown"))))
); geography

(defrule size 
=>
   (assert (size (userInterfaceCateg "What size college would you prefer?" 
      (create$ "small" "medium" "large"))))
)

(defrule culturematters 
=>
   (assert (culturematters (userInterfaceYN "Does the culture of the college matter to you?")))
)

(defrule greeklife
   (culturematters ?x &~N)
=>
   (assert (greeklife (userInterfaceYN "Are you looking for a college with a sorority or fraternity? (Y is not interested, U is no preference, N is yes)")))
)

(defrule campusculture
   (culturematters ?x &~N) 
=>
   (assert (campusculture (userInterfaceYN "Are you looking for a social life that is mainly on campus? (Y is not interested, U is no preference, N is yes)")))
)

(defrule major
=>
   (assert (major (userInterfaceCateg "What major are you interested in pursuing?" 
      (create$                 "math"                   "economics"           "psychology"    
       "computer science"      "english"                "business"            "natural sciences"  
       "engineering"           "biosciences"            "finance"             "communications"
       "biology"               "neuroscience"           "visual arts"         "performing arts" 
       "physics"               "bioengineering"         "architecture"        "social work"
       "philosophy"            "international relations"                      "sociology" 
       "drama"                 "biotechnology"          "agriculture"         "animal sciences"
       "human sciences"        "biomed"                 "business management" "nursing" 
       "film production"       "television"             "government"          "chemistry" 
       "oceanography"          "environmental science"  "political science"   "hotel administration" 
       "foreign languages"     "ecology"                "journalism"          "international history" 
       "international affairs" "applied math"           "history"             "design"
       "education"             "creative writing"       "biochemistry"        "forensic science" 
       "music"                 "undecided"))))
)

(defrule standardizedtesting
=>
   (assert (standardizedtesting (userInterfaceYN "Do you want to include scores from standardized testing in your application?")))
)

(defrule sat
   (standardizedtesting ?x &~N)
=>
   (assert (sat (userInterfaceCateg "In what range is your SAT score?" (create$ "400-500" "500-600" "600-700"
      "700-800" "800-900" "900-1000" "1000-1100" "1100-1200" "1200-1300" "1300-1400" "1400-1500" "1500-1550"
      "1550-1600"))))
)

(defrule tuition
=>
   (assert (tuition (userInterfaceCateg "What is the sticker price (the price without institutional grant) you're willing to pay for your college?"
      (create$ "<30k" "<35k" "<40k" "<45k" "<55k" "<60k" "<65k" "<70k" "<75k" "<80k" "<85k"))))
)

(defrule institutionalgrant
=>
   (assert (institutionalgrant (userInterfaceCateg "How much of an institutional grant are you looking for?"
      (create$ "<10k" "10k-15k" "15k-25k" "25k-40k" "40k-45k" "45k-50k" "50k-60k" "unknown"))))
)

; COLLEGE RECOMMENDATIONS

/*
* Tuition: 80k a year for room and board
* Institutional grant: 55k
* State: Massachusetts
* SAT: middle 50% is 1490, generally 1500+
* Most popular majors: math, econ, psych, comp sci, english
* Common to do double majors (45% or 46% do double majors)
* Pretty campusy college, social life focused on campus
* Type A school
* Motivated by grades
* Notes: institutional grant not included in restrictions because it's the highest possible value.
*/
(defrule amherst
   (tuition      "<85k")
   (size        "small")
   (independent       Y)
   (geography ?w &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland"
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?x &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?y &~"business" &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (doublemajor ?z &~N)
=>
   (recommend amherst)
); amherst

/*
* Tuition: 75k
* Institutional grant: 39k
* State: Massachusetts
* SAT: math 690-770, english 640-710
* Most popular majors: business with a liberal arts foundation
* Pretty big on campus family, big alumni network
* Motivated by money
*/
(defrule babson
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?v &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (sat_m ?x &~"700-710" &~"710-720" &~"730-740" &~"750-760" &~"770-780" &~"790-800")
   (sat_e ?y "610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720" &~"730-740" &~"750-760"
      &~"770-780" &~"790-800")
   (geography ?w &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (campusculture ?a &~N)
   (networking    ?b &~N)
   (size         "small")
=>
   (recommend babson)
); babson


/*
* Tuition: 70k
* Institutional grant: 45k
* State: Texas
* SAT: 1560+
* Most popular majors: natural sciences, engineering, comp sci, biosciences, econ
* Need-blind school (not interested in ability to pay for tuition)
* Liberal, friendly, international school
* Lots of weird traditions
* Sports oriented
*/
(defrule rice
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k")
   (institutionalgrant ?x &~"45k-50k" &~"50k-60k")
   (sat ?y &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500" &~"1500-1550")
   (geography ?z &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?a &~"math" &~"psychology" &~"english" &~"business" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size         "small")
   (needblind          Y)
   (campusculture ?b &~N)
   (sports        ?c &~N)
   (international ?d &~N)
=>
   (recommend rice)
); rice

/*
* Tuition: 29k
* Institutional grant: 3k
* State: California
* SAT: math 520-680, english 500-640
* Most popular majors: psychology, economics
* Not international
* Big on campus thing
* Big school
* Note: No restriction for tuition because it falls in the lowest category
*/
(defrule sjsu
   (institutionalgrant ?v &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k"
      &~"50k-60k")
   (sat_m ?w &~"200-300" &~"310-400" &~"410-500")
   (sat_e ?x &~"200-300" &~"310-400" &~"410-500")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size         "large")
   (international ?a &~Y)
   (campusculture      Y)
=>
   (recommend sjsu)
); sjsu

/*
* Tuition: 80k
* Institutional grant: 40k
* State: Massachusetts 
* SAT: math 720-780, english 600-750
* Most popular majors: econ, finance, communications, bio
* Super into sports (D1 athletics)
* Religion/theology
* Low international population
* Large school
*/
(defrule bostonc
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"700-710" &~"710-720" &~"730-740" &~"750-760" &~"770-780" &~"790-800")
   (sat_e ?a &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720" &~"730-740" &~"750-760"
      &~"770-780" &~"790-800")
   (major ?b &~"math" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences"  
      &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (sports             Y)
   (theology           Y)
   (international ?c &~Y)
   (size         "large")
=>
   (recommend bostonc)
); bostonc

/*
* Tuition: 77k
* Institutional grant: 24k
* State: California
* SAT: math 680-770, english 650-720
* Most popular majors: psychology, business
* Core curriculum, honors program
* International students
* Sporty
* Medium size
*/
(defrule scu
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650")
   (major ?b &~"math" &~"economics" &~"computer science" &~"english" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (corecurriculum ?c &~N)
   (honors         ?d &~N)
   (sports         ?e &~N)
   (size         "medium")
=>
   (recommend scu)
); scu

/*
* Tuition: 79k
* Institutional grant: 43k
* State: Massachusetts
* SAT: math 700-770, english 660-740
* Most popular majors: business
* Size: large
* Very campusy
* Core curriculum, honors program
* Not super sporty
*/
(defrule bostonu
   (tuition ?w &~"<80k" &~"<85k")
   (institutionalgrant ?x &~"<10k" &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size          "large")
   (campusculture       Y)
   (honors         ?c &~N)
   (corecurriculum ?d &~N)
   (sports         ?e &~Y)
=>
   (recommend bostonu)
); bostonu

/*
* Tuition: 80k
* Institutional grant: 51k
* State: California
* SAT: math 720-770, english 750-800
* Most popular majors: CS, human bio, econ, engineering
* Size: large
* Highly competitive
* Have to live on campus
* Huge research fund
* Not as sporty
* Note: no condition about institutional grant, as it's in the highest category.
*/
(defrule stanford
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?y &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720" &~"730-740")
   (major ?a &~"math" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size        "large")
   (competitive       Y)
   (campusculture     Y)
   (residentialsystem Y)
   (competitive       Y)
   (research          Y)
   (sports ?b &~N)
=>
   (recommend stanford)
); stanford

/*
* Tuition: 83k
* Institutional grant: 47k
* State: Rhode Island
* SAT: 1540+
* Most popular majors: CS, classics (engineering, etc), neuro, english
* Size: medium
* No core curriculum
* Interdisciplinary
* Not very sporty
*/
(defrule brown
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"<10k" &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size           "medium")
   (corecurriculum        N)
   (interdisciplinary     Y)
   (flexible              Y)
   (independent           Y)
   (sports ?b &~Y)
=>
   (recommend brown)
); brown

/*
* Tuition: 72k
* Institutional grant: 51k
* State: Pennsylvania
* SAT: 1500+
* Most popular majors: econ/math/comp sci/bio/visual and performing arts
* Size: small
* Part of a consortium (Quaker Consortium)
* A lot of work required
* Emphasis on group projects
* Note: no restriction on institutional grant, since it's in the highest category
*/
(defrule swarthmore
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?y &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?z &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"neuroscience" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "small")
   (consortium ?a &~N)
   (groupwork Y)
=>
   (recommend swarthmore)
); swarthmore

/*
* Tuition: 80k
* Institutional grant: 47k
* State: California
* SAT: 1545
* Most popular majors: physics, comp sci, engineering, math, bioengineering
* Size: small
* Required to live on campus all 4 years
*/
(defrule caltech
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"50k-60k")
   (sat ?y &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500" &~"1500-1550")
   (geography ?z &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?y &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" 
      &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "small")
   (residentialsystem Y)
=>
   (recommend caltech)
); 

/*
* Tuition: 57k
* Institutional grant: 5k
* State: Texas
* SAT: 610-770 (math), 620-730 (english)
* Most popular majors: business, architecture, communication, social work, engineering, english
* Size: large
* Honors program
*/
(defrule texasaustin
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (honors ?c &~N)
=>
   (recommend texasaustin)
); texasaustin

/*
* Tuition: 84k
* Institutional grant: 36k
* State: Massachusetts
* SAT: 730-790 (math), 710-760 (english)
* Most popular majors:  international relations, classics (philosophy, bio), engineering
* Size: medium
* Early decision, generally friendly environment
*/
(defrule tufts 
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670"
      &~"680-690" &~"700-710" &~"710-720")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670"
      &~"680-690" &~"700-710")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (competitive ?c &~Y)
   (size "medium")
   (early Y)
=>
   (recommend tufts)
); tufts

/*
* Tuition: 40k
* Institutional grant: 
* State: California
* SAT: N/A
* Most popular majors: engineering, architecture, business, econ, sociology
* Size: large
* Intense, not international
*/
(defrule ucberkeley
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?y &~"math" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"social work" &~"philosophy" &~"international relations"
      &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (competitive Y)
   (international ?z &~Y)
=>
   (recommend ucberkeley)
); ucberkeley

/*
* Tuition: 78k
* Institutional grant: 36k
* State: New York
* SAT: 1500+
* Most popular majors: CS, engineering, architecture, drama
* Size: medium
* Greek system
* Not big on campus living
*/
(defrule carnegiemellon
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (greeklife Y)
=>
   (recommend carnegiemellon)
); carnegiemellon

/*
* Tuition: 36k
* Institutional grant: 8k
* State: California
* SAT: N/A
* Most popular majors: biosciences/biotech/agriculture/animal sciences
* Size: large
* Generally friendly
*/
(defrule ucdavis
   (tuition ?w &~"<30k" &~"<35k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (competitive N)
=>
   (recommend ucdavis)
); ucdavis

/*
* Tuition: 74k
* Institutional grant: 32k
* State: Ohio
* SAT: 680-740 (english), 740-790 (math)
* Most popular majors: biomedical/mechanical/business management/nursing
* Size: medium
* International students, welcoming community
*/
(defrule casewesternreserve
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670")
   (sat_m ?a &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (international Y)
   (competitive   N)
=>
   (recommend casewesternreserve)
); casewesternreserve

/*
* Tuition: 34k
* Institutional grant: 10k
* State: California
* SAT: N/A
* Most popular majors: biological sciences, comp sci, engineering, nursing
* Size: large
* Big on research, premed
*/
(defrule ucirvine
   (tuition ?w &~"<30k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (research Y)
=>
   (recommend ucirvine)
); ucirvine

/*
* Tuition: 79k
* Institutional grant: 27k
* State: California
* SAT: 610-700(english) 600-710(math)
* Most popular majors: film production, television, performing arts
* Size: medium
* Focus on community environment, Greek life
* Personalized education
*/
(defrule chapman
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (campusculture Y)
   (greeklife ?c &~N)
=>
   (recommend chapman)
); chapman

/*
* Tuition: 35k
* Institutional grant: 10k
* State: California
* SAT: N/A
* Most popular majors: engineering/biosciences/math/theater
* Size: large
* Social scene off campus
* Sporty
* Mostly CA residents
*/
(defrule ucla
   (tuition ?w &~"<30k" &~"<35k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (campusculture N)
   (sports        Y)
   (international ?a &~Y)
=>
   (recommend ucla)
); ucla

/*
* Tuition: 77k
* Institutional grant: 50k
* State: California
* SAT: 720-780(math) 700-750(english)
* Most popular majors: econ/gov/international relations
* Size: small
* Claremont Consortium
* Good social life on campus
* Party school
* Leadership
* Guest speakers
*/
(defrule claremontmckenna
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690")
   (major ?b &~"math" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" 
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "small")
   (campusculture ?c &~N)
   (consortium ?d &~N)
   (party       Y)
   (networking  Y)
   (independent Y)
=>
   (recommend claremontmckenna)
); claremontmckenna

/*
* Tuition: 33k
* Institutional grant: 8k
* State: California
* SAT: N/A
* Most popular majors: engineering/bio/chem/cs/oceanography
* Size: large
* Most kids live off campus
*/
(defrule ucsandiego
   (tuition ?w &~"<30k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (campusculture ?a &~Y)
=>
   (recommend ucsandiego)
); ucsandiego

/*
* Tuition: 36k
* Institutional grant: 9k
* State: California
* SAT: N/A
* Most popular majors: oceanography/physics/chem/environmental science
* Size: large
* Pretty collaborative
* Party school, big on campus activities
* Most live on campus
* Not popular internationally
*/
(defrule ucsantabarbara
   (tuition ?w &~"<30k" &~"<35k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (major ?z &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (groupwork     ?a &~N)
   (campusculture ?b &~N)
   (international ?c &~Y)
   (party Y)
=>
   (recommend ucsantabarbara)
); ucsantabarbara

/*
* Tuition: 83k
* Institutional grant: 55k
* State: New York
* SAT: 1500+
* Most popular majors: engineering/english/history/polsci/econ/neuro
* Size: large
* Core curriculum, inflexible
* Competitive
* Taught by TAs
* Note: No restriction on institutional grant (it's in the highest category)
*/
(defrule colombia
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?y &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?z &~"math" &~"psychology" &~"computer science" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (corecurriculum Y)
   (competitive    Y)
   (flexible       N)
   (independent    Y)
=>
   (recommend colombia)
); colombia

/*
* Tuition: 80k
* Institutional grant: 46k
* State: New York
* SAT: 1490+
* Most popular majors: engineering/hotel administration/architecture
* Size: large
* Community is important (it's in the middle of nowhere)
* Good on-campus social life
* Lots of research
*/
(defrule cornell
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (collegeculture Y)
   (research       Y)
=>
   (recommend cornell)
); cornell

/*
* Tuition: 82k
* Institutional grant: 42k
* State: Illinois
* SAT: 1500+
* Most popular majors: social sciences/math/biomed
* Size: medium
* Super intensive
* Early decision important
*/
(defrule uchicago
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (competitive Y)
   (early       Y)
=>
   (recommend uchicago)
); uchicago

/*
* Tuition: 82k
* Institutional grant: 50k
* State: New Hampshire
* SAT: 1500+
* Most popular majors: engineering/biosciences/environmental studies/foreign languages
* Size: medium
* In the middle of nowhere (social life all on campus)
* Greek life
* Outdoorsy
* Flexible schedule
*/
(defrule dartmouth
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"political science" 
      &~"hotel administration" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (campusculture Y)
   (greeklife     Y)
   (flexible      Y)
=>
   (recommend dartmouth)
); dartmouth

/*
* Tuition: 80k
* Institutional grant: 50k
* State: North Carolina
* SAT: 1500+
* Most popular majors: engineering/biology/ecology/neuro
* Size: large
* Relatively collaborative
* Required to live on campus for 3 years
*/
(defrule duke
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (groupwork     Y)
   (campusculture Y)
=>
   (recommend duke)
); duke

/*
* Tuition: 50k
* Institutional grant: 9k
* State: Illinois
* SAT: 650-740 (english), 680-780 (math)
* Most popular majors: cs, engineering, communications
* Size: large
* Pretty diverse
* 50% live on campus
* Interdisciplinary (mixing computer science with liberal arts)
*/
(defrule uillinois
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650")
   (major ?b &~"math" &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size    "large")
   (campusculture N)
   (international     ?c &~N)
   (interdisciplinary ?d &~N)
=>
   (recommend uillinois)
); uillinois

/*
* Tuition: 71k
* Institutional grant: 23k
* State: Massachusetts
* SAT: 610-690 english, 590-700 math
* Most popular majors: performing arts, visual media, communications
* Size: small
* Very LGBTQ friendly
* Very creative kids
*/
(defrule emerson
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k")
   (institutionalgrant ?x &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"biology" &~"neuroscience" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (conservative N)
   (size "small")
=>
   (recommend emerson)
); emerson

/*
* Tuition: 27k
* Institutional grant: 8k
* State: Maryland
* SAT: 650-730 for english, 680-780 math
* Most popular majors: engineering, cs, journalism, business
* Size: large
* Used to be a party school
* Most kids are from the same state
* Large focus on LGBTQ community/racial awareness
* Note: Lowest category for tuition, so no restrictions
*/
(defrule umaryland
   (institutionalgrant ?w &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?y &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650")
   (major ?a &~"math" &~"economics" &~"psychology" &~"english" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (party        Y)
   (conservative N)
   (size   "large")
   (international ?b &~Y)
=>
   (recommend umaryland)
); umaryland

/*
* Tuition: 72k
* Institutional grant: 43k
* State: Georgia
* SAT: 1450+
* Most popular majors: business, admin, bio, chem, biological sciences, econ
* Size: medium
* Good social life based on campus
* Pretty sporty
*/
(defrule emory
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k")
   (institutionalgrant ?x &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400")
   (major ?a &~"math" &~"psychology" &~"computer science" &~"english"  
      &~"natural sciences" &~"engineering" &~"finance" 
      &~"communications" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"oceanography" &~"environmental science" &~"political science" 
      &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (campusculture ?b &~N)
   (sports Y)
=>
   (recommend emory)
); emory

/*
* Tuition: 80k
* Institutional grant: 32k
* State: Washington
* SAT: 1400s
* Most popular majors: international relations/psych/political science
* Size: large
* Not cutthroat
* Sports are important
* Decent social life
*/
(defrule gwashington
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400")
   (major ?a &~"math" &~"economics" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science"  
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size  "large")
   (competitive N)
   (sports      Y)
   (campusculture ?b &~N)
=>
   (recommend gwashington)
); gwashington

/*
* Tuition: 32k
* Institutional grant: 19k
* State: Michigan
* SAT: 1460+
* Most popular majors: engineering, business, architecture
* Size: large
* Challenging but not cutthroat
* Personalized attention
* Good balance of social life
*/
(defrule umichigan
   (tuition ?w &~"<30k")
   (institutionalgrant ?x &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english"  
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size  "large")
   (competitive N)
   (campusculture ?b &~N)
=>
   (recommend umichigan)
); umichigan

/*
* Tuition: 82k
* Institutional grant: 42k
* State: Washington
* SAT: 700-770 (english), 710-780 (math)
* Most popular majors: school of international history/affairs/business/economics
* Size: medium
* Lots of traditions
* 50% study abroad and not on campus
*/
(defrule georgetown
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670" 
      &~"680-690" &~"700-710")
   (sat_e ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670" 
      &~"680-690")
   (major ?b &~"math" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" 
      &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (campusculture N)
   (size   "medium")
   (international Y)
=>
   (recommend georgetown)
); georgetown

/*
* Tuition: 53k
* Institutional grant: 11k
* State: Georgia
* SAT: 750+ english, 690-790 math
* Most popular majors: engineering
* Size: large
* Very rigorous
* Mostly from Georgia
* Party area outside of campus
* Independent self directed kids
*/
(defrule gitech
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k")
   (institutionalgrant ?x &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670" 
      &~"680-690" &~"700-710" &~"710-720" &~"730-740")
   (sat_m ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size    "large")
   (competitive   Y)
   (international N)
   (party         Y)
   (independent   Y)
   (campusculture ?c &~N)
=>
   (recommend gitech)
); gitech

/*
* Tuition: 78k
* Institutional grant: 55k
* State: Massachusetts
* SAT: 730-780 (english), 750-800 (math)
* Most popular majors: econ/gov/comp sci/applied math/history
* Size: large
* Not big on undergrads
* Super competitive
* High achievers and old money
* Everyone lives on campus
* Highest category for institutional grant, no restrictions placed
*/
(defrule harvard
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland"  
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?y &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670" 
      &~"680-690" &~"700-710" &~"710-720" &~"730-740")
   (major ?a &~"math" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" 
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (competitive     Y)
   (independent     Y)
   (residencysystem Y)
   (size      "large")
=>
   (recommend harvard)
); harvard

/*
* Tuition: 80k
* Institutional grant: 40k
* State: California
* SAT: 710-760 (english), 770-800 (math)
* Most popular majors: engineering, cs, math
* Size: small
* Most live on campus
* Claremont Consortium
* Whole brain education
*/
(defrule harveymudd
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710")
   (sat_m ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670" 
      	&~"680-690" &~"700-710" &~"710-720" &~"730-740" &~"750-760")
   (major ?b &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size    "small")
   (campusculture Y)
   (consortium    Y)
   (corecurriculum ?c &~N)
=>
   (recommend harveymudd)
); harveymudd

/*
* Tuition: 57k
* Institutional grant: 7k
* State: Oregon
* SAT: 1350+
* Most popular majors: journalism design business education
* Size: large
* Honors program
* Half of the kids are from Oregon
* Independent, passionate kids
*/
(defrule uoregon
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"international history"
      &~"international affairs" &~"applied math" &~"history"  
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size    "large")
   (honors        Y)
   (international N)
   (independent   Y)
=>
   (recommend uoregon)
); uoregon

/*
* Tuition: 83k
* Institutional grant: 51k
* State: Pennsylvania
* SAT: 1500+
* Most popular majors: wharton school of business, engineering, communication
* Size: large
* Core curriculum
* Sporty, party school
* Highly specialized programs
* Highest category of institutional grant, no restrictions
*/
(defrule upenn
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?y &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?z &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (corecurriculum ?a &~N)
   (sports Y)
   (party  Y)
=>
   (recommend upenn)
); upenn

/*
* Tuition: 79k
* Institutional grant: 46k
* State: Maryland
* SAT: 1500s
* Most popular majors: engineering, creative writing, neuro, international studies, film, premed
* Size: large
* Research opportunities
* Not overly competitive
*/
(defrule johnshopkins
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"business management" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"applied math" &~"history" &~"design" &~"education" 
      &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (competitive ?b &~Y)
   (research Y)
=>
   (recommend johnshopkins)
); johnshopkins

/*
* Tuition: 78k
* Institutional grant: 50k
* State: Massachusetts
* SAT: 1500+
* Most popular majors: engineering, econ
* Size: medium
* Research
* 95% of students live on campus
* Interdisciplinary: mix of STEM and humanities
*/
(defrule mit
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k")
   (institutionalgrant ?x &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland"  
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (research Y)
   (campusculture     ?b &~N)
   (interdisciplinary ?c &~N)
=>
   (recommend mit)
); mit

/*
* Tuition: 82k
* Institutional grant: 27k
* State: New York
* SAT: 1500+
* Most popular majors: stern business school, film, tv, visual/performing arts,
*    economics
* Size: large
* A lot of kids study abroad in Florence
* Super competitive, failed 70% of their class
* Campus is pretty small
*/
(defrule nyu
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k" &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400" &~"1400-1500")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (international Y)
   (competitive   Y)
   (campusculture N)
=>
   (recommend nyu)
); nyu

/*
* Tuition: 34k
* Institutional grant: N/A
* Country: Canada
* SAT: 1330+
* Most popular majors: ir (international relations), biochem, forensic science, cs
* Size: large
* Pretty international
*/
(defrule utoronto
   (tuition ?w &~"<30k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming")
   (sat ?y &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300")
   (major ?z &~"math" &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"music")
   (size "large")
   (international Y)
=>
   (recommend utoronto)
); utoronto

/*
* Tuition: 60k
* Institutional grant: 8k
* State: Washington
* SAT: 480-580 (reading) 480-590 (math)
* Most popular majors: business, english, environmental studies
* Size: large
* Competitive in business
* Honors programs, top professors
* 70% of kids live in Washington
*/
(defrule uwashington
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" 
      &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500")
   (sat_m ?a &~"200-300" &~"310-400" &~"410-500")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (competitive Y)
   (honors        ?c &~N)
   (international ?d &~Y)
=>
   (recommend uwashington)
); uwashington

/*
* Tuition: 56k
* Institutional grant: 11k
* State: Wisconsin
* SAT: 650-730 (english) 690-780 (math)
* Most popular majors: education agriculture communication
* Size: large
* Independent school
* Main social life revolves around student union
* Well rounded school across subjects
*/
(defrule uwisconsin
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k")
   (institutionalgrant ?x &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wyoming" &~"canada")
   (sat_e ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650")
   (sat_m ?a &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670")
   (major ?b &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (independent       ?c &~N)
   (collegeculture    ?d &~N)
   (interdisciplinary ?e &~N)
=>
   (recommend uwisconsin)
); uwisconsin

/*
* Tuition: 80k
* Institutional grant: 45k
* State: Tennessee
* SAT: 1400+
* Most popular majors: engineering education music
* Size: medium
* Collaborative place
* Conservative
* Low international percentage
* Majority of kids live on campus
* Sporty
*/
(defrule vanderbilt
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k"
      &~"<75k" &~"<80k")
   (institutionalgrant ?x &~"45k-50k" &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100"
      &~"1100-1200" &~"1200-1300" &~"1300-1400")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" 
      &~"creative writing" &~"biochemistry" &~"forensic science")
   (size "medium")
   (groupwork Y)
   (conservative   ?b &~N)
   (international  ?c &~Y)
   (collegeculture ?d &~Y)
   (sports         ?e &~N)
=>
   (recommend vanderbilt)
); vanderbilt

/*
* Tuition: 77k
* Institutional grant: 52k
* State: New Jersey
* SAT: 720-770 (english) 740-800 (math)
* Most popular majors: math philosophy econ architecture history
* Size: medium
* Pretty diverse
* Low international percentage
* Everything is based on campus
* Note: no restriction on institutional grant, as it's in the highest category
*/
(defrule princeton
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" &~"<70k"
      &~"<75k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?y &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720")
   (major ?a &~"psychology" &~"computer science" &~"english" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"social work" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "medium")
   (international    ?b &~Y)
   (residencyprogram ?c &~N)
=>
   (recommend princeton)
); princeton

/*
* Tuition: 44k
* Institutional grant: 7k
* State: Indiana
* SAT: 1400+
* Most popular majors: cs, engineering, agriculture
* Size: large
* Competitive, independent school
* 15% international
*/
(defrule purdue
   (tuition ?w &~"<30k" &~"<35k" &~"<40k")
   (institutionalgrant ?x &~"10k-15k" &~"15k-25k" &~"25k-40k" &~"40k-45k" &~"45k-50k"
      &~"50k-60k")
   (geography ?y &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"connecticut" &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat ?z &~"400-500" &~"500-600" &~"600-700" &~"700-800" &~"800-900" &~"900-1000" &~"1000-1100" 
      &~"1100-1200" &~"1200-1300" &~"1300-1400")
   (major ?a &~"math" &~"economics" &~"psychology" &~"english" &~"business" 
      &~"natural sciences" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"visual arts" &~"performing arts" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"drama" &~"biotechnology" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science" &~"music")
   (size "large")
   (competitive ?b &~N)
   (independent ?c &~N)
=>
   (recommend purdue)
); purdue

/*
* Tuition: 82k
* Institutional grant: 56k
* State: Connecticut
* SAT: 730-780 english, 750-800 math
* Most popular majors: fine arts, drama, music, english
* Size: medium
* Residential colleges
* Small percentage of sororities/frats
* Everything is on campus
* Note: no restriction on institutional grant, since it's in the highest category
*/
(defrule yale
   (tuition ?w &~"<30k" &~"<35k" &~"<40k" &~"<45k" &~"<55k" &~"<60k" &~"<65k" 
      &~"<70k" &~"<75k" &~"<80k")
   (geography ?x &~"alabama" &~"alaska" &~"arizona" &~"arkansas" &~"california" &~"colorado" 
      &~"delaware" &~"florida" &~"georgia" &~"idaho" &~"illinois" &~"indiana" 
      &~"iowa" &~"kansas" &~"kentucky" &~"louisiana" &~"maine" &~"maryland" &~"massachusetts" 
      &~"michigan" &~"minnesota" &~"mississippi" &~"missouri" &~"montana" &~"nebraska" &~"nevada" 
      &~"new hampshire" &~"new jersey" &~"new york" &~"north carolina" &~"north dakota" &~"ohio" 
      &~"oklahoma" &~"oregon" &~"pennsylvania" &~"rhode island" &~"south carolina" &~"south dakota" 
      &~"tennessee" &~"texas" &~"utah" &~"vermont" &~"virginia" &~"washington" &~"west virginia" 
      &~"wisconsin" &~"wyoming" &~"canada")
   (sat_e ?y &~"200-300" &~"310-400" &~"410-500"
      &~"510-550" &~"560-600" &~"610-650" &~"660-670" &~"680-690" &~"700-710" &~"710-720")
   (sat_m ?z &~"200-300" &~"310-400" &~"410-500" &~"510-550" &~"560-600" &~"610-650" &~"660-670" 
      &~"680-690" &~"700-710" &~"710-720" &~"730-740")
   (major ?a &~"math" &~"economics" &~"psychology" &~"computer science" &~"business" 
      &~"natural sciences" &~"engineering" &~"biosciences" &~"finance" 
      &~"communications" &~"biology" &~"neuroscience" &~"physics" 
      &~"bioengineering" &~"architecture" &~"social work" &~"philosophy" &~"international relations"
      &~"sociology" &~"biotechnology" &~"agriculture" &~"animal sciences" &~"human sciences" 
      &~"biomed" &~"business management" &~"nursing" &~"film production" &~"television" &~"government"
      &~"chemistry" &~"oceanography" &~"environmental science" &~"political science" 
      &~"hotel administration" &~"foreign languages" &~"ecology" &~"journalism" &~"international history"
      &~"international affairs" &~"applied math" &~"history" &~"design" &~"education" 
      &~"creative writing" &~"biochemistry" &~"forensic science")
   (size "medium")
   (residentialprogram ?b &~N)
   (greeklife          ?c &~N)
=>
   (recommend yale)
); yale






