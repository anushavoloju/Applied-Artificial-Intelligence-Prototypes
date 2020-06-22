; Project 2 
; Fuzzy Rule Based System (fuzzy JESS)
; Health Monitor System
; Username : Snow White

(import nrc.fuzzy.*)

(import nrc.fuzz.jess.*)

(load-package nrc.fuzzy.jess.FuzzyFunctions)


; templates

(deftemplate User
    (slot name)
    (slot age)
    (slot gender)
    (slot BloodPressureSys)
    (slot BloodPressureDys)
    (slot HeartRate)
    (slot BodyTemperature))

(deftemplate question (slot text) (slot type) (slot ident))

(deftemplate answer (slot ident) (slot text))


(defglobal ?*BloodPressureSysVar* = (new FuzzyVariable "BloodPressure" 80 250 "mmHg"))

(defglobal ?*BloodPressureDysVar* = (new FuzzyVariable "BloodPressure" 40 150 "mmHg"))

(defglobal ?*HeartRateVar* = (new FuzzyVariable "HeartRate" 20 200 "BPM"))

(defglobal ?*BodyTemperatureVar* = (new FuzzyVariable "BodyTemperature" 90 120 "Fahrenheit"))

(defglobal ?*recommendVar* = (create$ ))


(call nrc.fuzzy.FuzzyValue setMatchThreshold 0.1)


; function to check the answer types 

(deffunction is-of-type (?answer ?type)
"Check that the answer has the correct type as specified"
(if (eq ?type text) then
(return (or (eq ?answer MALE) (eq ?answer FEMALE) (eq ?answer NOTSPECIFIED)))
else (if (eq ?type integer) then
(return (integerp ?answer))
else (if (eq ?type number) then
(return (numberp ?answer))
else (return (> (str-length ?answer) 0))))))


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


; Module 0
; Initializing global variables
(defmodule initial)
(defrule initial-terms
    (declare (salience 100))
    =>
    (import nrc.fuzzy.*)
    (import nrc.fuzz.jess.*)
    (load-package nrc.fuzzy.jess.FuzzyFunctions)
    (?*BloodPressureSysVar* addTerm "low" (new ZFuzzySet 80 92))
    (?*BloodPressureSysVar* addTerm "moderate" (new TrapezoidFuzzySet 88 100 130 142))
    (?*BloodPressureSysVar* addTerm "high" (new SFuzzySet 124 200))
    (?*BloodPressureDysVar* addTerm "low" (new ZFuzzySet 40 65))
    (?*BloodPressureDysVar* addTerm "moderate" (new TrapezoidFuzzySet 59 60 80 92))
    (?*BloodPressureDysVar* addTerm "high" (new SFuzzySet 74 150))
    (?*HeartRateVar* addTerm "low" (new ZFuzzySet 20 70))
    (?*HeartRateVar* addTerm "moderate" (new TrapezoidFuzzySet 59 65 95 101))
    (?*HeartRateVar* addTerm "high" (new SFuzzySet 73 200))
    (?*BodyTemperatureVar* addTerm "low" (new ZFuzzySet 90 98.9))
    (?*BodyTemperatureVar* addTerm "moderate" (new TrapezoidFuzzySet 96.9 97.5 98.9 99.1))
    (?*BodyTemperatureVar* addTerm "high" (new SFuzzySet 93.3 120))
    )


; Module 2
; Start module

(defmodule startup)
(defrule welcome-user
(declare (salience 99))
=>

(printout t crlf "********************Welcome to Health Monitor System********************" crlf)
(printout t "Type the name of the user/patient and press Enter> ")
(bind ?name (read))
(printout t "Let us begin the health monitoring for " ?name "." crlf)
(printout t "Please provide the required details that would be recorded by the system and" crlf)
(printout t "the system will tell the health status of the user/patient." crlf crlf))

; link all questions to the corresponding ids

(deffacts questions
"The questions that are asked to the user by the system."
(question 
(ident age) (type number)
(text "What is the user's/patient's age in years (0-120) ? "))
(question 
(ident gender) (type text)
(text "What is the gender of the user/patient (MALE/FEMALE/NOTSPECIFIED) ? "))
(question 
(ident BloodPressureSys) (type integer)
(text "What is recorded Systolic Blood Pressure of the user/patient in mmHg (80-250) ? "))
(question 
(ident BloodPressureDys) (type integer)
(text "What is recorded Diastolic Blood Pressure of the user/patient in mmHg (40-150) ? "))
(question 
(ident HeartRate) (type integer)
(text "What is recorded Heart Rate of the user/patient in BPM (20-200) ? "))
(question 
(ident BodyTemperature) (type number)
(text "What is recorded Body Temperature of the user/patient in Fahrenheit (90-120) ? ")))

; Module 3
; interview module to request the details of the employee and assert the answers as facts to knowledge base

(defmodule request-details)
(defrule request-age (declare (salience 98))
=>
(assert (ask age)))
(defrule request-gender (declare (salience 97))
=>
(assert (ask gender)))
(defrule request-BloodPressureSys (declare (salience 96))
=>
(assert (ask BloodPressureSys)))
(defrule request-BloodPressureDys (declare (salience 95))
=>
(assert (ask BloodPressureDys)))
(defrule request-HeartRate (declare (salience 94))
=>
(assert (ask HeartRate)))
(defrule request-BodyTemperature (declare (salience 93))
=>
(assert (ask BodyTemperature)))

(defrule assert-fact
(declare (salience 92))
(answer (ident age) (text ?a))
(answer (ident gender) (text ?g))
(answer (ident BloodPressureSys) (text ?bps))
(answer (ident BloodPressureDys) (text ?bpd))
(answer (ident HeartRate) (text ?hr))
(answer (ident BodyTemperature) (text ?bt))
=>
(assert (User (age ?a) (gender ?g) (BloodPressureSys ?bps) (BloodPressureDys ?bpd) (HeartRate ?hr) (BodyTemperature ?bt))))
    
(defrule check-inputs
(declare(salience 91))
(User (name ?n)
(age ?a)
(gender ?g)
(BloodPressureSys ?bps)
(BloodPressureDys ?bpd)
(HeartRate ?hr)
(BodyTemperature ?bt))
=>
(if (or (<= ?a 0) (> ?a 120)) then
(printout t crlf "The age value entered is not logically correct with respect to this system. Please enter an age value (> 0 and < 120). " crlf)
(reset)
(run)
else (if (not (or (= ?g MALE) (= ?g FEMALE) (= ?g NOTSPECIFIED))) then
(printout t crlf "The gender value entered is not logically correct with respect to this system. Please re-enter a value for gender as (MALE, FEMALE OR NOTSPECIFIED). " crlf)
(reset)
(run)
else (if (or (< ?bps 80) (> ?bps 250)) then
(printout t crlf "The blood pressure systolic value entered is not logically correct with respect to this system. Please re-enter a value between (80-250). " crlf)
(reset)
(run)
else (if (or (< ?bpd 40) (> ?bpd 150)) then
(printout t crlf "The blood pressure diastolic value entered is not logically correct with respect to this system. Please re-enter a value between (40-150). " crlf)
(reset)
(run)
else (if (or (< ?hr 20) (> ?hr 200)) then
(printout t crlf "The heart rate value entered is not logically correct with respect to this system. Please enter a value between (20-200)." crlf)
(reset)
(run)
else (if (or (< ?bt 90) (> ?bt 120)) then
(printout t crlf "The body temperature value entered is not logically correct with respect to this system. Please enter a value between (90-120)." crlf))))))))


(defrule generate-fuzzyset "FuzzySets"
(declare(salience 90))
?User <- (User (name ?name))
=>
    (assert(age_data ?User.age))
    (assert(gender_data ?User.gender))
    (assert(BloodPressureSys_data
        (new FuzzyValue ?*BloodPressureSysVar*
        (new SingletonFuzzySet
        ?User.BloodPressureSys))))
    (assert(BloodPressureDys_data
        (new FuzzyValue ?*BloodPressureDysVar*
        (new SingletonFuzzySet
        ?User.BloodPressureDys))))
    (assert(HeartRate_data
        (new FuzzyValue ?*HeartRateVar*
        (new SingletonFuzzySet
        ?User.HeartRate))))
     (assert(BodyTemperature_data
        (new FuzzyValue ?*BodyTemperatureVar*
        (new SingletonFuzzySet
        ?User.BodyTemperature))))
    )
    
    
; Rule 1
; To check whether the user's HR and BPSys are low and BPDys is high
(defrule HRLow_BPSyslow_BPDysHigh
    (declare (salience 88))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "low"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "low"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Bradycardia))
    )

; Rule 2
; To check whether the user's HR, BP and BT are low 
(defrule HRLow_BPLow_BTLow
    (declare (salience 87))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "low"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "low"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "low"))
    (BodyTemperature_data ?bt&:(fuzzy-match ?bt "low"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypothermia))
    )

; Rule 3
; To check whether the user's HR and BP are low 
(defrule HRLow_BPlow
    (declare (salience 86))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "low"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "low"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "low"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypotension))
    )

; Rule 4
; To check whether the user's HR is high, BPSys is low and BPDys is high
(defrule HRHigh_BPSyslow_BPDysHigh
    (declare (salience 85))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "high"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "low"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Tachycardia))
    )

; Rule 5
; To check whether the user's HR is high, BPSys is low and BPDys is high and BT is high
(defrule HRHigh_BPSyslow_BPDysHigh_BTHigh
    (declare (salience 84))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "high"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "low"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "high"))
    (BodyTemperature_data ?bt&:(fuzzy-match ?bt "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Fever))
    )

; Rule 6
; To check whether the user's HR is high, BPSys is high and BPDys is high
(defrule HRHigh_BPSysHigh_BPDysHigh
    (declare (salience 83))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "high"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "high"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypertension))
    )
    
; Rule 7
; To check whether the user's BT is Low
(defrule BTLow
    (declare (salience 82))
    (BodyTemperature_data ?bt&:(fuzzy-match ?bt "low"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypothermia))
    )

; Rule 8
; To check whether the user's BT is High
(defrule BTHigh
    (declare (salience 81))
    (BodyTemperature_data ?bt&:(fuzzy-match ?bt "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Fever))
    )

; Rule 9
; To check whether the user's HR is low
(defrule HRLow
    (declare (salience 80))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "low"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Bradycardia))
    )

; Rule 10
; To check whether the user's HR is high
(defrule HRHigh
    (declare (salience 79))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Tachycardia))
    )
    
; Rule 11
; To check whether the user's BPSys is high
(defrule BPSysHigh
    (declare (salience 78))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "high"))
    (BloodPressureDys_data ?bpd&:(or (fuzzy-match ?bpd "low") (fuzzy-match ?bpd "moderate") (fuzzy-match ?bpd "high")))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypertension))
    )

; Rule 12
; To check whether the user's BPDys is high
(defrule BPDysHigh
    (declare (salience 77))
    (BloodPressureSys_data ?bps&:(or (fuzzy-match ?bps "low") (fuzzy-match ?bps "moderate") (fuzzy-match ?bps "high")))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "high"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypertension))
    )

; Rule 13
; To check whether the user's BPSys is Low
(defrule BPSysLow
    (declare (salience 76))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "low"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypotension))
    )

; Rule 14
; To check whether the user's BPDys is Low
(defrule BPDysLow
    (declare (salience 75))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "low"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Hypotension))
    )
    

; Rule 15
; To check whether the user's All are moderate
(defrule AllModerate
    (declare (salience 74))
    (HeartRate_data ?hr&:(fuzzy-match ?hr "moderate"))
    (BloodPressureSys_data ?bps&:(fuzzy-match ?bps "moderate"))
    (BloodPressureDys_data ?bpd&:(fuzzy-match ?bpd "moderate"))
    (BodyTemperature_data ?bt&:(fuzzy-match ?bt "moderate"))
     =>
    (bind ?*recommendVar* (create$ ?*recommendVar* Normal))
    )

; Rule 16
; Print the recommended actions
(defrule report
    (declare (salience 73))
     =>
    (if (member$ Normal ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The Heart Rate, Blood Pressure and the Body Temperature are Normal." crlf)
    (printout t "*************************************************************************" crlf)
    )
    (if (member$ Bradycardia ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The readings indicate a Warning for Bradycardia." crlf)
    (printout t "Please consult the doctor to get the treatment for Bradycardia (low heart rate)" crlf)
    (printout t "*************************************************************************" crlf)
    )
    (if (member$ Tachycardia ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The readings indicate a Warning for Tachycardia." crlf)
    (printout t "Please consult the doctor to get the treatment for Tachycardia (high heart rate)" crlf)
    (printout t "*************************************************************************" crlf)
    )
    (if (member$ Hypertension ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The readings indicate a Warning for Hypertension." crlf)
    (printout t "Please consult the doctor to get the treatment for Hypertension (high Blood Pressure)" crlf)
    (printout t "*************************************************************************" crlf)
    )
    (if (member$ Hypotension ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The readings indicate a Warning for Hypotension." crlf)
    (printout t "Please consult the doctor to get the treatment for Hypotension (low Blood Pressure)" crlf)
    (printout t "*************************************************************************" crlf)
    )
    (if (member$ Fever ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The readings indicate a Warning for Fever." crlf)
    (printout t "Please consult the doctor to get the treatment for Fever (high body temperature)" crlf)
    (printout t "*************************************************************************" crlf)
    )
    (if (member$ Hypothermia ?*recommendVar*) then
    (printout t "*************************************************************************" crlf)
    (printout t "The readings indicate a Warning for Hypothermia." crlf)
    (printout t "Please consult the doctor to get the treatment for Hypothermia (low body temperature)" crlf)
    (printout t "*************************************************************************" crlf)
    )
    )


; function to run the program

(deffunction run-application ()
(reset)
(focus initial startup request-details)
(run))


; loop to re-run the program to get the required details of the next user/patient

(while TRUE
(run-application))
   

