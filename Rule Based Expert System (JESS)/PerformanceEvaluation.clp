; Project 1 
; Rule Based Expert System (JESS)
; Employee performance evaluation system
; Username : Snow White

; templates

(deftemplate employee
(slot designation)
(slot experience (default 0))
(slot competency-score (default 0))
(slot goal-accomplishments-score (default 0))
(slot additional-achievements-score (default 0))
(slot leadership-score (default 0))
(slot teamwork-score (default 0)))

(deftemplate question (slot text) (slot type) (slot ident))

(deftemplate answer (slot ident) (slot text))

(deftemplate performance-rating (slot rating))

(deftemplate recommendation (slot performance-rating) (slot description))


; function to check the answer types 

(deffunction is-of-type (?answer ?type)
"Check that the answer has the correct type as specified"
(if (eq ?type text) then
(return (or (eq ?answer SE) (eq ?answer SSE) (eq ?answer QA) (eq ?answer TL) (eq ?answer DM)))
else (if (eq ?type number) then
(return (numberp ?answer))
else (return (> (str-length ?answer) 0)))))


; function to ask questions

(deffunction ask-user (?question ?type)
"Ask a question and return the answer"
(bind ?answer "")
(bind ?count 0)
(while (not (is-of-type ?answer ?type)) do
(if (eq ?count 1) then
(printout t "The answer to the question is invalid. Please enter the answer in the correct expected format." crlf))
(printout t ?question " ")
(bind ?answer (read))
(bind ?count 1))
(return ?answer))


; ask module to map the answers to the corresponding questions and assert them

(defmodule ask)
(defrule ask::ask-question-by-id
"Ask a question and assert the answer"
(declare (auto-focus TRUE))
(MAIN::question (ident ?id) (text ?text) (type ?type))
(not (MAIN::answer (ident ?id)))
?ask <- (MAIN::ask ?id)
=>
(bind ?answer (ask-user ?text ?type))
(assert (MAIN::answer (ident ?id) (text ?answer)))
(retract ?ask)
(return))

; startup module

(defmodule startup)
(defrule print-welcome
=>
(printout t crlf "Welcome to the Employee Performance Evaluation System!" crlf)
(printout t "******************************************************" crlf)
(printout t crlf "Type the name of the employee and press Enter > ")
(bind ?name (read))
(printout t crlf "Let us begin the performance evaluation for " ?name "." crlf)
(printout t "Please provide the requested details to get the performance rating of the given employee. " crlf crlf))


; link all questions to the corresponding ids

(deffacts questions
"The questions that are asked to the user by the system."
(question 
(ident designation) (type text)
(text "Please enter the designation of the employee from below options: 
1.'SE' for Software Engineer
2.'SSE' for Senior Software Engineer
3.'QA' for Quality Analyst
4.'TL' for Technical Lead
5.'DM' for Dev Manager
"))
(question 
(ident experience) (type number)
(text "What is the relative experience of the employee (in years)? "))
(question 
(ident competency-score) (type number)
(text "What is the competency score of the employee (0-5)? "))
(question 
(ident goal-accomplishments-score) (type number)
(text "What is the goal accomplishments score of the employee (0-5)? "))
(question 
(ident additional-achievements-score) (type number)
(text "What is the additional achievements score of the employee (0-5)? "))
(question 
(ident leadership-score) (type number)
(text "What is the leadership score of the employee (0-5)? "))
(question 
(ident teamwork-score) (type number)
(text "What is the teamwork score of the employee (0-5)? ")))


; interview module to request the details of the employee and assert the answers as facts to knowledge base

(defmodule request-employee-details)
(defrule request-designation (declare (salience 100))
=>
(assert (ask designation)))
(defrule request-experience (declare (salience 99))
=>
(assert (ask experience)))
(defrule request-competency-score (declare (salience 98))
=>
(assert (ask competency-score)))
(defrule request-goal-accomplishments-score (declare (salience 97))
=>
(assert (ask goal-accomplishments-score)))
(defrule request-additional-achievements-score (declare (salience 96))
=>
(assert (ask additional-achievements-score)))
(defrule request-leadership-score (declare (salience 95))
=>
(assert (ask leadership-score)))
(defrule request-teamwork-score (declare (salience 94))
=>
(assert (ask teamwork-score)))

(defrule assert-employee-fact
(answer (ident designation) (text ?d))
(answer (ident experience) (text ?e))
(answer (ident competency-score) (text ?c))
(answer (ident goal-accomplishments-score) (text ?g))
(answer (ident additional-achievements-score) (text ?a))
(answer (ident leadership-score) (text ?l))
(answer (ident teamwork-score) (text ?t))
=>
(assert (employee (designation ?d) (experience ?e) (competency-score ?c) (goal-accomplishments-score ?g) (additional-achievements-score ?a) (leadership-score ?l) (teamwork-score ?t))))


; recommend module to compute the overall performance rating and recommendation based on the given information and scores

(defmodule recommend)
(defrule rating-scenario1
(employee (designation ?d&:(eq ?d SE))
(experience ?e&:(> ?e 0)&:(< ?e 3))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 1)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.30 ?g) (* 0.15 ?a) (* 0.05 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario2
(employee (designation ?d&:(eq ?d SE))
(experience ?e&:(> ?e 2)&:(< ?e 7))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 2)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.30 ?g) (* 0.15 ?a) (* 0.05 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario3
(employee (designation ?d&:(eq ?d SSE))
(experience ?e&:(> ?e 0)&:(< ?e 3))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 1)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.25 ?g) (* 0.20 ?a) (* 0.05 ?l) (* 0.20 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario4
(employee (designation ?d&:(eq ?d SSE))
(experience ?e&:(> ?e 2)&:(< ?e 7))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 2)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.25 ?g) (* 0.20 ?a) (* 0.05 ?l) (* 0.20 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario5
(employee (designation ?d&:(eq ?d SSE))
(experience ?e&:(> ?e 6)&:(< ?e 13))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 3)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.25 ?g) (* 0.20 ?a) (* 0.05 ?l) (* 0.20 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario6
(employee (designation ?d&:(eq ?d QA))
(experience ?e&:(> ?e 0)&:(< ?e 3))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 1)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.20 ?g) (* 0.20 ?a) (* 0.10 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario7
(employee (designation ?d&:(eq ?d QA))
(experience ?e&:(> ?e 2)&:(< ?e 7))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 2)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.20 ?g) (* 0.20 ?a) (* 0.10 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario8
(employee (designation ?d&:(eq ?d QA))
(experience ?e&:(> ?e 6)&:(< ?e 13))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 3)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.20 ?g) (* 0.20 ?a) (* 0.10 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario9
(employee (designation ?d&:(eq ?d QA))
(experience ?e&:(> ?e 12)&:(< ?e 21))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 4)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.20 ?g) (* 0.20 ?a) (* 0.10 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario10
(employee (designation ?d&:(eq ?d QA))
(experience ?e&:(> ?e 20)&:(< ?e 91))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 5)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.30 ?c) (* 0.20 ?g) (* 0.20 ?a) (* 0.10 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario11
(employee (designation ?d&:(eq ?d TL))
(experience ?e&:(> ?e 0)&:(< ?e 3))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 1)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.20 ?g) (* 0.10 ?a) (* 0.25 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario12
(employee (designation ?d&:(eq ?d TL))
(experience ?e&:(> ?e 2)&:(< ?e 7))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 2)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.20 ?g) (* 0.10 ?a) (* 0.25 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario13
(employee (designation ?d&:(eq ?d TL))
(experience ?e&:(> ?e 6)&:(< ?e 13))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 3)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.20 ?g) (* 0.10 ?a) (* 0.25 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario14
(employee (designation ?d&:(eq ?d TL))
(experience ?e&:(> ?e 12)&:(< ?e 21))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 4)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.20 ?g) (* 0.10 ?a) (* 0.25 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario15
(employee (designation ?d&:(eq ?d TL))
(experience ?e&:(> ?e 20)&:(< ?e 91))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 5)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.20 ?g) (* 0.10 ?a) (* 0.25 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario16
(employee (designation ?d&:(eq ?d DM))
(experience ?e&:(> ?e 6)&:(< ?e 13))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 3)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.15 ?g) (* 0.10 ?a) (* 0.30 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario17
(employee (designation ?d&:(eq ?d DM))
(experience ?e&:(> ?e 12)&:(< ?e 21))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 4)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.15 ?g) (* 0.10 ?a) (* 0.30 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))

(defrule rating-scenario18
(employee (designation ?d&:(eq ?d DM))
(experience ?e&:(> ?e 20)&:(< ?e 91))
(competency-score ?c&:(>= ?c 0)&:(<= ?c 5))
(goal-accomplishments-score ?g&:(>= ?g 0)&:(<= ?g 5))
(additional-achievements-score ?a&:(>= ?a 0)&:(<= ?a 5))
(leadership-score ?l&:(>= ?l 0)&:(<= ?l 5))
(teamwork-score ?t&:(>= ?t 0)&:(<= ?t 5)))
=>
(bind ?normalized-exp 5)
(bind ?overall-rating (float (+ (* 0.05 ?normalized-exp) (* 0.25 ?c) (* 0.15 ?g) (* 0.10 ?a) (* 0.30 ?l) (* 0.15 ?t))))
(if(and (> ?overall-rating 0) (< ?overall-rating 1)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is unsatisfactory.")))
elif(and (>= ?overall-rating 1) (< ?overall-rating 2)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance needs improvement.")))
elif(and (>= ?overall-rating 2) (< ?overall-rating 3)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance meets the expectations.")))
elif(and (>= ?overall-rating 3) (< ?overall-rating 4)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance exceeds expectations.")))
elif(and (>= ?overall-rating 4) (<= ?overall-rating 5)) then
(assert (recommendation
(performance-rating ?overall-rating)
(description "The employee's performance is exceptional!")))))


; check if the inputs are valid

(defrule check-inputs
(employee (designation ?d)
(experience ?e)
(competency-score ?c)
(goal-accomplishments-score ?g)
(additional-achievements-score ?a)
(leadership-score ?l)
(teamwork-score ?t))
=>
(if (or (<= ?e 0) (> ?e 90)) then
(printout t "The experience value entered is not logically correct with respect to this system. Please enter an experience value (> 0 and < 91). " crlf)
else (if (and (= ?d SE) (> ?e 6)) then
(printout t "The experience value entered for SE is not logically correct with respect to this system. Please re-enter an experience value (< 7). " crlf)
else (if (and (= ?d SSE) (> ?e 12)) then
(printout t "The experience value entered for SSE is not logically correct with respect to this system. Please re-enter an experience value (< 13). " crlf)
else (if (and (= ?d DM) (< ?e 7)) then
(printout t "The experience value entered for DM is not logically correct with respect to this system. Please re-enter an experience value (> 6). " crlf)
else (if (or (< ?c 0) (> ?c 5)) then
(printout t "The competency-score value entered is not correct with respect to this system. Please enter a score between (0-5)." crlf)
else (if (or (< ?g 0) (> ?g 5)) then
(printout t "The goal-accomplishments-score value entered is not correct with respect to this system. Please enter a score between (0-5)." crlf)
else (if (or (< ?a 0) (> ?a 5)) then
(printout t "The additional-achievements-score value entered is not correct with respect to this system. Please enter a score between (0-5)." crlf)
else (if (or (< ?l 0) (> ?l 5)) then
(printout t "The leadership-score value entered is not correct with respect to this system. Please enter a score between (0-5)." crlf)
else (if (or (< ?t 0) (> ?t 5)) then
(printout t "The teamwork-score value entered is not correct with respect to this system. Please enter a score between (0-5)." crlf)))))))))))


;Module that contains the rules to print out the final result of the evaluation

(defmodule report)
(defrule print-result
?p1 <- (recommendation (performance-rating ?pr) (description ?d))
=>
(printout t crlf "*********************************************" crlf)
(printout t "The performance rating for this employee is :" ?pr crlf)
(printout t "Description: " ?d crlf)
(printout t "*********************************************" crlf))


; run the modules in the program in the specified order

(deffunction run-program()
(reset)
(focus startup request-employee-details recommend report)
(run))


; run the loop to re run the program

(while TRUE
(run-program))