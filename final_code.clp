
;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;Start	   
(defrule want-to-start ""
   (not (start ?))
   (not (stream ?))
   =>
   (assert (start (yes-or-no-p "DO you want to start(yes/no)? "))))
   
;Physics
(defrule question1 ""
    (start yes)
   (not (count-physics1 ?))
   =>
   (assert (physics1 (ask-question "Which one is a scalar quantity? (a. acceleration/b. displacement/ c. speed )? "acceleration displacement speed))))

(defrule question2 ""
    (start yes)
	(or(or(physics1 speed ) (physics1 acceleration)) (physics1 displacement))
   (not (count-physics1 ?))
   =>
   (assert (physics2 (ask-question "What is the rate of acceleration of gravity? (a. 10 m/s/s b. 9.8 m/s/s c. 6 m/s/s )? "10 9.8 6))))
;Chemistry   
(defrule question3 ""
    (or(or(physics2 10 ) (physics2 9.8)) (physics2 6))
	(not (count-chemistry1 ?))
   =>
   (assert (chemistry1 (ask-question "What is the most commonly used bleaching agent? (a. alcohol b. chlorine c. sodium_chloride )? "alcohol chlorine sodium_chloride))))
    
(defrule question4 ""
    (or(or(chemistry1 alcohol ) (chemistry1 chlorine)) (chemistry1 sodium_chloride))
	(not (count-chemistry1 ?))
   =>
   (assert (chemistry2 (ask-question "What is the most abundant rare gas in the atmosphere? (a. ar b. ne c. he )? "ar ne he))))

;Maths   
(defrule question5 ""
    (or(or(chemistry2 ar ) (chemistry2 ne)) (chemistry2 he))
	(not (count-maths1 ?))
   =>
   (assert (maths1 (ask-question "How many sides of Decagon have? (a. 20 b. 10  c. 15 )? "20 10 15))))
    
(defrule question6 ""
    (or(or(maths1 20 ) (maths1 10)) (maths1 15))
	(not (count-maths1 ?))
   =>
   (assert (maths2 (ask-question "What is the largest prime number less than 50? (a. 49 b. 41 a c. 47 )? "49 41 47))))

   
;Biology   
(defrule question7 ""
    (or(or(maths2 49 ) (maths2 41)) (maths2 47))
	(not (count-biology1 ?))
   =>
   (assert (biology1 (ask-question "Which is the longest bone in human body? (a. back_bone b. large_intestine  c. fermur )? "back_bone large_intestine fermur))))
    
(defrule question8 ""
    (or(or(biology1 back_bone ) (biology1 large_intestine)) (biology1 fermur))
	(not (count-biology1 ?))
   =>
   (assert (biology2 (ask-question "Which blood group is a universal donor? (a. o b. a c. b )? "o a b))))

;COMMERCE
(defrule question9 ""
    (start yes)
	(or(or(biology2 o ) (biology2 b)) (biology2 a))
   (not (count-commerce1 ?))
   =>
   (assert (commerce1 (ask-question "A letter of credit (L/C) is produced by–
 (a. an_exporter b. an_importer c. custom_authorities )? "an_exporter an_importer custom_authorities))))
 
 
 (defrule question10 ""
    (start yes)
	(or(or(commerce1 an_exporter ) (commerce1 an_importer)) (commerce1 custom_authorities))
   (not (count-commerce1 ?))
   =>
   (assert (commerce2 (ask-question "First Auditor of a Company is appointed by the–
 (a. shareholders b. board_of_directors c. central_government )? "shareholders board_of_directors central_government ))))
   
(defrule question11 ""
    (start yes)
	(or(or(commerce2 shareholders ) (commerce2 board_of_directors)) (commerce2 central_government))
   (not (count-commerce1 ?))
   =>
   (assert (commerce3 (ask-question "Which of the following documents defines the scope of company’s activities?
 (a. memorandum_of_association b. prospectus c. statutory_declaration )? "memorandum_of_association prospectus statutory_declaration ))))

;ARTS

(defrule question12 ""
    (start yes)
    (or(or(commerce3 memorandum_of_association ) (commerce3 articles_of_association)) (commerce3 prospectus ))
   (not (count-arts1 ?))
   =>
   (assert (arts1 (ask-question "Who created blue jeans? (a. brooke_shields /b. calvin_klein /c. levi_strauss )? "brooke_shields calvin_klein levi_strauss))))

(defrule question13 ""
    (start yes)
	(or(or(arts1 brooke_shields ) (arts1 calvin_klein)) (arts1 levi_strauss))
   (not (count-arts1 ?))
   =>
   (assert (arts2 (ask-question "Full form of YLC? (a. yves_saint_laurent /b. youth_leadership_council /c. young_life_christian )? "yves_saint_laurent youth_leadership_council young_life_christian))))
    
;;;****************
;;;* COUNT RULES *
;;;****************

;Physics
(defrule conclusions-physics1 ""
   (physics1 speed )
   (physics2 9.8 )
   (not (count-physics1 ?))
   =>
   (assert ( count-physics1 2)))
   

(defrule conclusions-physics2 ""
   (or(physics1 acceleration) (physics1 displacement))
   (or(physics2 10 ) (physics2 6))
   (not (count-physics1 ?))
   =>
   (assert ( count-physics1 0)))
   
(defrule conclusions-physics3 ""
  (or(and (or(physics1 acceleration) (physics1 displacement))
   (physics2 9.8 ))(and (physics1 speed )
   (or(physics2 10 ) (physics2 6))))
   (not (count-physics1 ?))
   =>
   (assert ( count-physics1 1)))

;Chemistry
(defrule conclusions-chemistry1 ""
   (chemistry1 chlorine )
   (chemistry2 ar )
   (not (count-chemistry1 ?))
   =>
   (assert ( count-chemistry1 2)))
   

(defrule conclusions-chemistry2 ""
   (or(chemistry1 alcohol) (chemistry1 sodium_chloride))
   (or(chemistry2 ne) (chemistry2 he))
   (not (count-chemistry1 ?))
   =>
   (assert ( count-chemistry1 0)))
   
(defrule conclusions-chemistry3 ""
  (or(and (or(chemistry1 alcohol) (chemistry1 sodium_chloride))
   (chemistry2 ar ))(and (chemistry1 chlorine )
   (or(chemistry2 ne ) (chemistry2 he))))
   (not (count-chemistry1 ?))
   =>
   (assert ( count-chemistry1 1)))

;Maths
(defrule conclusions-maths1 ""
   (maths1 10 )
   (maths2 47 )
   (not (count-maths1 ?))
   =>
   (assert ( count-maths1 2)))
   

(defrule conclusions-maths2 ""
   (or(maths1 20) (maths1 15))
   (or(maths2 49 ) (maths2 41))
   (not (count-maths1 ?))
   =>
   (assert ( count-maths1 0)))
   
(defrule conclusions-maths3 ""
  (or(and (or(maths1 20) (maths1 15))
   (maths2 47 ))(and (maths1 10 )
   (or(maths2 41 ) (maths2 49))))
   (not (count-maths1 ?))
   =>
   (assert ( count-maths1 1)))


;Biology
(defrule conclusions-biology1 ""
   (biology1 fermur )
   (biology2 o )
   (not (count-biology1 ?))
   =>
   (assert ( count-biology1 2)))
   

(defrule conclusions-biology2 ""
   (or(biology1 back_bone) (biology1 large_intestine))
   (or(biology2 b ) (biology2 a))
   (not (count-biology1 ?))
   =>
   (assert ( count-biology1 0)))
   
(defrule conclusions-biology3 ""
  (or(and (or(biology1 back_bone) (biology1 large_intestine))
   (biology2 o ))(and (biology1 fermur )
   (or(biology2 b ) (biology2 a))))
   (not (count-biology1 ?))
   =>
   (assert ( count-biology1 1)))
   
;Commerce

(defrule conclusions-commerce1 ""
   (commerce1 an_importer )
   (commerce2 board_of_directors )
   (commerce3 memorandum_of_association )
   (not (count-commerce1 ?))
   =>
   (assert ( count-commerce1 3)))
   

(defrule conclusions-commerce2 ""
   (and(and(or(commerce1 an_exporter) (commerce1 custom_authorities))
   (or(commerce2 shareholders ) (commerce2 central_government)))
   (or(commerce3 prospectus ) (commerce3 statutory_declaration)))
   (not (count-commerce1 ?))
   =>
   (assert ( count-commerce1 0)))
 
 (defrule conclusions-commerce3 ""
 (or (or(and(and (or(commerce1 an_exporter) (commerce1 custom_authorities))
   (or (commerce2 shareholders )(commerce2 central_government)))(commerce3 memorandum_of_association))
   
   (and(and (or(commerce3 prospectus ) (commerce3 statutory_declaration))
   (or (commerce2 shareholders )(commerce2 central_government)))(commerce1 an_importer)))
   
   (and(and (or(commerce1 an_exporter) (commerce1 custom_authorities))
   (or (commerce3 prospectus )(commerce3 statutory_declaration)))(commerce2 board_of_directors)))
   (not (count-commerce1 ?))
   =>
   (assert ( count-commerce1 1)))
   
  (defrule conclusions-commerce4 ""
 (or (or(and(and (or(commerce1 an_exporter) (commerce1 custom_authorities))
   (commerce2 board_of_directors ))(commerce3 memorandum_of_association))
   
   (and(and (or(commerce3 prospectus) (commerce3 statutory_declaration))
   (commerce2 board_of_directors ))(commerce1 an_importer)))
   
   (and(and (or(commerce2 shareholders) (commerce2 central_government))
   (commerce1 an_importer ))(commerce3 memorandum_of_association)))
   (not (count-commerce1 ?))
   =>
   (assert ( count-commerce1 2)))
   
;ARTS
(defrule conclusions-arts1 ""
   (arts1 levi_strauss)
   (arts2 yves_saint_laurent)
   (not (count-arts1 ?))
   =>
   (assert ( count-arts1 2)))
   

(defrule conclusions-arts2 ""
   (or(arts1 brooke_shields) (arts1 calvin_klein))
   (or(arts2 youth_leadership_council) (arts2 young_life_christian))
   (not (count-arts1 ?))
   =>
   (assert ( count-arts1 0)))
   
   (defrule conclusions-arts3 ""
  (or(and (or(arts1 brooke_shields) (art1 calvin_klein))
   (arts2 yves_saint_laurent ))(and (arts1 levi_strauss )
   (or(arts2 youth_leadership_council) (arts2 young_life_christian))))
   (not (count-arts1 ?))
   =>
   (assert ( count-arts1 1)))
   
;count of science
(defrule conclusions-science1 ""
(count-chemistry1 ?c)
(count-biology1 ?b)
(count-physics1 ?p)
(count-maths1 ?m)
=>
(assert (count-science1 (+(+(+ ?c ?p)?m)?b))))

;percentage of science
(defrule percentage-science ""
(count-science1 ?s)
=>
(assert (percentage-science ( *(/ ?s 8)100)))
)

;percentage of commerce
(defrule percentage-commerce ""
(count-commerce1 ?co)
=>
(assert (percentage-commerce ( *(/ ?co 3)100)))
)

;percentage of arts
(defrule percentage-arts ""
(count-arts1 ?art)
=>
(assert (percentage-arts ( *(/ ?art 2)100)))
)

;greater check

(defrule check-greater ""
  (percentage-science ?ps)
  (percentage-arts ?pa)
  (percentage-commerce ?pc)
  =>
  (if (and(> ?ps ?pa) (> ?ps ?pc))
  then
  (assert (greater-ps yes))
  else
  (if (> ?pa ?pc)
  then
  (assert (greater-pa yes))
  else
  (assert (greater-pc yes))
  )
))

(defrule check-equals ""
  (percentage-science ?ps)
  (percentage-arts ?pa)
  (percentage-commerce ?pc)
  
  =>
  (if (and(= ?ps ?pa)(= ?ps ?pc))
	then
		(assert (equals-ps-pa-pc yes))
	else
		(if (= ?ps ?pc)
			then
				(assert (equals-ps-pc yes))
				
			else
				(if (= ?ps ?pa)
					then 
						(assert (equals-ps-pa yes))
					else
						(if (= ?pa ?pc)
							then
								(assert (equals-pa-pc yes))
							else
								(assert (no-equals yes))
						)
				)
		)
	)
	
	(if(and (= ?ps 0)(= ?pa 0))
		then
		
		(assert (equals-ps-pa no))
	)
	(if(and (= ?ps 0)(= ?pc 0))
		then
		
		(assert (equals-ps-pc no))
	)
	(if(and (= ?pa 0)(= ?pc 0))
		then
		
		(assert (equals-pa-pc no))
	)
)

(defrule check-zero_rule1 ""
 ?rule2 <-(equals-ps-pa yes)
 =>
(retract ?rule2)
)

(defrule check-zero_rule2 ""
 ?rule2 <-(equals-ps-pc yes)
 =>
(retract ?rule2)
)

(defrule check-zero_rule3 ""
 ?rule2 <-(equals-pa-pc yes)
 =>
(retract ?rule2)
)









(defrule question14 ""
  (or (or(or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes)) (equals-ps-pa-pc yes))
  (not (count-medical1 ?))
   =>
   (assert (medical1 (ask-question "A sudden interruption of the blood supply to the brain can cause?.  (a. stroke  b. depression c. memory_loss )? "stroke depression memory_loss))))

(defrule question15 ""
   (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
   (not (count-medical1 ?))
    (or(or(medical1 stroke ) (medical1 depression)) (medical1 memory_loss))
   =>
   (assert (medical2 (ask-question "Insulin deficiency causes? (a. diabetes b.jaundice c. dementia )? "diabetes jaundice dementia))))

(defrule question16 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(medical2 diabetes ) (medical2 jaundice)) (medical2 dementia))
  (not (count-comps1 ?))
   =>
   (assert (comps1 (ask-question "The brain of any computer system is?  (a. memory  b.cpu c. alu )? "memory cpu alu))))

(defrule question17 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(comps1 memory ) (comps1 cpu)) (comps1 alu))
  (not (count-comps1 ?))
   =>
   (assert (comps2 (ask-question "computer program that converts the assembly language to machine language?  (a. compiler  b.assembler c.interpretor )? "compiler assembler interpretor))))

(defrule question18 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(comps2 compiler ) (comps2 interpretor)) (comps2 assembler))
  (not (count-civil1 ?))
   =>
   (assert (civil1 (ask-question "in a mortar, binding material is?  (a. cement  b.sand c.surkhi )? "cement sand surkhi))))

(defrule question19 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(civil1 sand ) (civil1 cement)) (civil1 surkhi))
  (not (count-civil1 ?))
   =>
   (assert (civil2 (ask-question "irrigation canals are generally alined along?  (a.ridge_line  b.contour_line c.valley_line )? " ridge_line contour_line valley_line))))

(defrule question20 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(civil2 ridge_line ) (civil2 valley_line)) (civil2 contour_line))
  (not (count-entc1 ?))
   =>
   (assert (entc1 (ask-question ". if the resistance in a circuit of the constant voltage inc ,the current will?  (a. increase  b. decrease c.remain_same )? " increase decrease remain_same))))

(defrule question21 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(entc1  increase) (entc1 decrease)) (entc1 remain_same))
  (not (count-entc1 ?))
   =>
   (assert (entc2 (ask-question "a single transistor is used to build which of the following digital logic gates?  (a. and  b. or c. not )? " and or not))))

(defrule question22 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(entc2  or) (entc2 and)) (entc2 not))
  (not (count-mech1 ?))
   =>
   (assert (mech1 (ask-question "the wet bulp heating during sensible heating of air?  (a. increases  b. decreases c. remain_constant  )? " increases decreases remain_constant))))

(defrule question23 ""
  (or (or(greater-ps yes)(equals-ps-pa yes))(equals-ps-pc yes))  
    (or(or(mech1  increases) (mech1 decreases)) (mech1 remain_constant))
  (not (count-mech1 ?))
   =>
   (assert (mech2 (ask-question "Euler’s formula holds good only for?  (a. long_column  b. short_column c. weak_column   )? " long_column short_column weak_column))))
   
;For Commerce Driling
(defrule question24 ""
  (or (or(or(greater-pc yes)(equals-pa-pc yes))(equals-ps-pc yes)) (equals-ps-pa-pc yes))
  (not (count-ca1 ?))
   =>
   (assert (ca1 (ask-question "Which of the following is a current liability?  (a. prepaid_expense  b. trademark c. outstanding_salaries   )? " prepaid_expense trademark outstanding_salaries))))
   
(defrule question25 ""
  (or (or(greater-pc yes)(equals-pa-pc yes))(equals-ps-pc yes))  
    (or(or(ca1 prepaid_expense) (ca1 trademark)) (ca1 outstanding_salaries))
  (not (count-ca1 ?))
   =>
   (assert (ca2 (ask-question "purchase of fixed assets on credit is originally recorded in? (a. ledger b. journal_proper c. purchase_book    )? " ledger journal_proper purchase_book))))
 
   
(defrule question26 ""
  (or (or(greater-pc yes)(equals-pa-pc yes))(equals-ps-pc yes))  
    (or(or(ca2 ledger) (ca2 journal_proper)) (ca2 purchase))
  (not (count-cs1 ?))
   =>
   (assert (cs1 (ask-question "The first Secretary of the company is appointed by– (a. promoters b. directors c. shareholders    )? " promoters shareholders directors))))
  
(defrule question27 ""
  (or (or(greater-pc yes)(equals-pa-pc yes))(equals-ps-pc yes))  
    (or(or(cs1 promoters) (cs1	directors)) (cs1 shareholders))
  (not (count-cs1 ?))
   =>
   (assert (cs2 (ask-question "Where title in shares of a company is in dispute, the matter has to be resolved by ?– (a. court b. central_government c. arbitrator    )? " court arbitrator central_government))))
  
(defrule question28 ""
  (or(or (or(greater-pa yes)(equals-pa-pc yes))(equals-ps-pa yes)) (equals-ps-pa-pc yes)) 
  (not (count-fashion1 ?))
   =>
   (assert (fashion1 (ask-question "Which Indian State is the leading Cotton producer ?  (a. gujarat b. maharashtra c. kerela    )? " gujarat maharashtra kerela))))
  
(defrule question29 ""
  (or (or(greater-pa yes)(equals-pa-pc yes))(equals-ps-pa yes))
   (or(or(fashion1 gujarat) (fashion1 maharashtra)) (fashion1 kerela))
  (not (count-fashion1 ?))
   =>
   (assert (fashion2 (ask-question "The direction in which the yarn is passing in the fabric?  (a. fibre b. cross_wise c. grain    )? " grain fibre cross_wise))))
 
(defrule question30 ""
  (or (or(greater-pa yes)(equals-pa-pc yes))(equals-ps-pa yes))
   (or(or(fashion2 grain) (fashion2 cross_wise)) (fashion2 fibre))
  (not (count-journalism1 ?))
   =>
   (assert (journalism1 (ask-question "Life’s Good is the catchline of which of the following brands?  (a. lg b. hp c. samsung   )? " lg samsung hp))))

 (defrule question31 ""
  (or (or(greater-pa yes)(equals-pa-pc yes))(equals-ps-pa yes))
   (or(or(journalism1 lg) (journalism1 hp)) (journalism1 samsung))
  (not (count-journalism1 ?))
   =>
   (assert (journalism2 (ask-question "A licence in copyright matters creates in the license?  (a. proprietory_right  b. public_right c. personal_right   )? " proprietory_right public_right personal_right))))


;;;****************
;;;* COUNT RULES *
;;;****************

;medical
(defrule conclusions-medical1 ""
   (medical1 stroke )
   (medical2 diabetes )
   (not (count-medical1 ?))
   =>
   (assert ( count-medical1 2)))
   

(defrule conclusions-medical2 ""
   (or(medical1 depression) (medical1 memory_loss))
   (or(medical2 jaundice ) (medical2 dementia))
   (not (count-medical1 ?))
   =>
   (assert ( count-medical1 0)))
   
(defrule conclusions-medical3 ""
  (or(and (or(medical1 depression) (medical1 memory_loss))
   (medical2 diabetes ))(and (medical1 stroke )
   (or(medical2 jaundice ) (medical2 dementia))))
   (not (count-medical1 ?))
   =>
   (assert ( count-medical1 1)))

;comps
(defrule conclusions-comps1 ""
   (comps1 cpu )
   (comps2 assembler )
   (not (count-comps1 ?))
   =>
   (assert ( count-comps1 2)))
   

(defrule conclusions-comps2 ""
   (or(comps1 alu) (comps1 memory))
   (or(comps2 compiler ) (comps2 interpretor))
   (not (count-comps1 ?))
   =>
   (assert ( count-comps1 0)))
   
(defrule conclusions-comps3 ""
  (or(and (or(comps1 alu) (comps1 memory))
   (comps2 assembler ))(and (comps1 cpu )
   (or(comps2 compiler ) (comps2 interpretor))))
   (not (count-comps1 ?))
   =>
   (assert ( count-comps1 1)))

;civil
(defrule conclusions-civil1 ""
   (civil1 cement )
   (civil2 ridge_line )
   (not (count-civil1 ?))
   =>
   (assert ( count-civil1 2)))
   

(defrule conclusions-civil2 ""
   (or(civil1 surkhi) (civil1 sand))
   (or(civil2 valley_line ) (civil2 contour_line))
   (not (count-civil1 ?))
   =>
   (assert ( count-civil1 0)))
   
(defrule conclusions-civil3 ""
  (or(and (or(civil1 sand) (civil1 surkhi))
   (civil2 ridge_line ))(and (civil1 cement )
   (or(civil2 valley_line ) (civil2 contour_line))))
   (not (count-civil1 ?))
   =>
   (assert ( count-civil1 1)))

;ENTC
(defrule conclusions-entc1 ""
   (entc1 decrease )
   (entc2 not )
   (not (count-entc1 ?))
   =>
   (assert ( count-entc1 2)))
   

(defrule conclusions-entc2 ""
   (or(entc1 increase) (entc1 remain_same))
   (or(entc2 and ) (entc2 or))
   (not (count-entc1 ?))
   =>
   (assert ( count-entc1 0)))
   
(defrule conclusions-entc3 ""
  (or(and (or(entc1 increase) (entc1 remain_same))
   (entc2 not ))(and (entc1 decrease )
   (or(entc2 or ) (entc2 and))))
   (not (count-entc1 ?))
   =>
   (assert ( count-entc1 1)))
   
  ;mechanical
(defrule conclusions-mech1 ""
   (mech1 increases )
   (mech2 long_column )
   (not (count-mech1 ?))
   =>
   (assert ( count-mech1 2)))
   

(defrule conclusions-mech2 ""
   (or(mech1 decreases) (mech1 remain_constant))
   (or(mech2 weak_column ) (mech2 short_column))
   (not (count-mech1 ?))
   =>
   (assert ( count-mech1 0)))
   
(defrule conclusions-mech3 ""
  (or(and (or(mech1 decreases) (mech1 remain_constant))
   (mech2 long_column ))(and (mech1 increases )
   (or(mech2 weak_column) (mech2 short_column))))
   (not (count-mech1 ?))
   =>
   (assert ( count-mech1 1)))

;ca
(defrule conclusions-ca1 ""
   (ca1 outstanding_salaries )
   (ca2 journal_proper )
   (not (count-ca1 ?))
   =>
   (assert ( count-ca1 2)))
   

(defrule conclusions-ca2 ""
   (or(ca1 trademark) (ca1 prepaid_expense))
   (or(ca2 ledger ) (ca2 purchase_book ))
   (not (count-ca1 ?))
   =>
   (assert ( count-ca1 0)))
   
(defrule conclusions-ca3 ""
  (or(and (or(ca1 trademark) (ca1 prepaid_expense))
   (ca2 journal_proper ))(and (ca1 outstanding_salaries )
   (or(ca2 ledger) (ca2 purchase_book))))
   (not (count-ca1 ?))
   =>
   (assert ( count-ca1 1)))

;cs
(defrule conclusions-cs1 ""
   (cs1  promoters)
   (cs2  court)
   (not (count-cs1 ?))
   =>
   (assert ( count-cs1 2)))
   

(defrule conclusions-cs2 ""
   (or(cs1 directors) (cs1 shareholders))
   (or(cs2 arbitrator ) (cs2 central_government ))
   (not (count-cs1 ?))
   =>
   (assert ( count-cs1 0)))
   
(defrule conclusions-cs3 ""
  (or(and (or(cs1 directors) (cs1 shareholders))
   (cs2 court ))(and (cs1 promoters )
   (or(cs2 arbitrator) (cs2 central_government))))
   (not (count-cs1 ?))
   =>
	  (assert ( count-cs1 1)))

; fashion
 (defrule conclusions-fashion1 ""
   (fashion1  gujarat)
   (fashion2  grain)
   (not (count-fashion1 ?))
   =>
   (assert ( count-fashion1 2)))
   

(defrule conclusions-fashion2 ""
   (or(fashion1 maharashtra) (fashion1 kerela))
   (or(fashion2 cross_wise) (fashion2 fibre))
   (not (count-fashion1 ?))
   =>
   (assert ( count-fashion1 0)))
   
(defrule conclusions-fashion3 ""
  (or(and (or(fashion1 maharashtra) (fashion1 kerela))
   (fashion2 grain ))(and (fashion1 gujarat )
   (or(fashion2 cross_wise) (fashion2 fibre))))
   (not (count-fashion1 ?))
   =>
	  (assert ( count-fashion1 1)))

;journalism
(defrule conclusions-journalism1 ""
   (journalism1 lg)
   (journalism2 personal_right)
   (not (count-journalism1 ?))
   =>
   (assert ( count-journalism1 2)))
   

(defrule conclusions-journalism2 ""
   (or(journalism1 hp) (journalism1 samsung))
   (or(journalism2 proprietory_right)  (journalism2 public_right ))
   (not (count-journalism1 ?))
   =>
   (assert ( count-journalism1 0)))
   
(defrule conclusions-journalism3 ""
  (or(and (or(journalism1 hp) (journalism1 samsung))
   (journalism2 personal_right ))(and (journalism1 lg )
   (or(journalism2 proprietory_right) (journalism2 public_right))))
   (not (count-journalism1 ?))
   =>
	  (assert ( count-journalism1 1))
	  )

	  
;percentage of medical
(defrule percentage-medical ""
(count-medical1 ?me )
=>
(assert (percentage-medical ( *(/ ?me 2)100)))
)

;percentage of comps
(defrule percentage-comps ""
(count-comps1 ?co )
=>
(assert (percentage-comps ( *(/ ?co 2)100)))
)

;percentage of civil
(defrule percentage-civil ""
(count-civil1 ?ci )
=>
(assert (percentage-civil ( *(/ ?ci 2)100)))
)

;percentage of entc
(defrule percentage-entc ""
(count-entc1 ?en )
=>
(assert (percentage-entc ( *(/ ?en 2)100)))
)

;percentage of mech
(defrule percentage-mech ""
(count-mech1 ?me )
=>
(assert (percentage-mech ( *(/ ?me 2)100)))
)

;percentage of ca
(defrule percentage-ca ""
(count-ca1 ?ca )
=>
(assert (percentage-ca ( *(/ ?ca 2)100)))
)

;percentage of cs
(defrule percentage-cs ""
(count-cs1 ?cs )
=>
(assert (percentage-cs ( *(/ ?cs 2)100)))
)

;percentage of fashion
(defrule percentage-fashion ""
(count-fashion1 ?fas )
=>
(assert (percentage-fashion ( *(/ ?fas 2)100)))
)

;percentage of journalism
(defrule percentage-journalism ""
(count-journalism1 ?jou )
=>
(assert (percentage-journalism ( *(/ ?jou 2)100)))
)


;;*****************************
;;* PRINT ALL PERCENTAGES *
;;*****************************



(defrule print-Science ""

(percentage-science ?science)
(percentage-mech ?mech)
(percentage-entc ?entc)
(percentage-civil ?civil)
(percentage-comps ?comp)
(percentage-medical ?medical)
(percentage-commerce ?commerce)
(percentage-arts ?arts)
=>
   (printout t " -------------------------------" crlf)   
   (printout t " FIELDS                 PERCENTAGES" crlf)
   (printout t "Science -----" ?science "%" crlf)
   (printout t "Mechanical -----" ?mech "%" crlf) 
   (printout t "Electronics -----" ?entc "%" crlf)
   (printout t "Civil -----" ?civil "%" crlf)
   (printout t  "Computer -----" ?comp "%" crlf)
   (printout t  "Medical -----" ?medical "%" crlf)
    (printout t "Commerce -----" ?commerce "%" crlf)
	 (printout t "Arts -----" ?arts "%" crlf)
   (printout t " -------------------------------" crlf))
   
(defrule print-commerce ""

(percentage-ca ?ca)
(percentage-cs ?cs)



   =>
   
   (printout t " -------------------------------" crlf)   
   (printout t " FIELDS                 PERCENTAGES" crlf)
  
   (printout t "CA -----" ?ca "%" crlf)
   (printout t "CS -----" ?cs "%" crlf)
   (printout t " -------------------------------" crlf))
   
   (defrule print-arts ""


(percentage-journalism ?journalism)
(percentage-fashion ?fashion)

   =>
   (printout t " -------------------------------" crlf)   
   (printout t " FIELDS                 PERCENTAGES" crlf)
  
   (printout t "Fashion -----" ?fashion "%" crlf)
   (printout t "Journalism -----" ?journalism "%" crlf)
   (printout t " -------------------------------" crlf))