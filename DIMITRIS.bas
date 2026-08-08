REM DIMITRIS.BAS
'Πρόγραμμα αριθμητικών πράξεων

DEFINT A-Z  'Επιλογή ακεραίων σαν πρότυπο τύπο δεδομένων

'Δηλώσεις διαδικασιών
DECLARE SUB DisplayResults ()
DECLARE SUB Good ()
DECLARE SUB Oops ()
DECLARE SUB Addition2 ()
DECLARE SUB Subtraction ()
DECLARE SUB Addition3 ()
DECLARE SUB Multiplication ()
DECLARE SUB Division ()
DECLARE SUB Practice ()
DECLARE FUNCTION RandNum% (Low%, High%)

'Δήλωση και ορισμός καθολικών μεταβλητών
COMMON SHARED Correct%, InCorrect%, TimeSpent#, InCorrectFlag%
COMMON SHARED ESC$, SpaceBar$, Name$, limit%, limitdiv%

'Απόδοση αρχικής τιμής σε καθολικές μεταβλητές και καθάρισμα οθόνης
Correct% = 0: InCorrect% = 0: TimeSpent# = 0: InCorrectFlag% = 1
ESC$ = CHR$(27): SpaceBar$ = CHR$(32)
Name$ = "ΔΗΜΗΤΡΗ": limit% = 1000: limitdiv% = 1000
COLOR 15, 4
CLS

'Δημιουργία φόρμας εκκίνησης
LOCATE 2, 2: PRINT CHR$(201); STRING$(75, 205); CHR$(187)
FOR i = 3 TO 20
   LOCATE i, 2: PRINT CHR$(186); TAB(78); CHR$(186)
NEXT i
LOCATE i, 2: PRINT CHR$(200); STRING$(75, 205); CHR$(188)
LOCATE 8, 32: PRINT "Π P O Γ P A M M A"
LOCATE 10, 30: PRINT "A P I Θ M H T I K H Σ"
LOCATE 12, 30: PRINT "Δ'  Δ H M O T I K O Y "
LOCATE 18, 50: PRINT "ΓΡAMMENO AΠO TO ΣΠ. ΠΛATH"
LOCATE 19, 61: PRINT "ΓIA TO ΔHMHTPH"

'Περιμένει για οδηγίες
DO
   VIEW PRINT 23 TO 25
   LOCATE 23, 25: PRINT "ΠIEΣE";
   COLOR 10, 4: PRINT " MΠAPA";
   COLOR 15, 4: PRINT " ΓIA NA ΣΥΝEXIΣEIΣ"
   LOCATE 24, 25: PRINT "ΠIEΣE";
   COLOR 10, 4: PRINT " ESC";
   COLOR 15, 4: PRINT " ΓIA NA ΣTAMATHΣEIΣ"
   choice$ = INPUT$(1)
   IF choice$ = ESC$ THEN
      COLOR 15, 0
      VIEW PRINT: CLS
      EXIT DO
   END IF
   IF choice$ = SpaceBar$ THEN Practice
LOOP

'------------------------- Addition2 ---------------------------
'Εξάσκηση στην πρόσθεση με 2 προσθεταίους
'---------------------------------------------------------------
SUB Addition2
   'Καθάρισμα οθόνης και απόδοση αρχικής τιμής σε γεννήτρια
   CLS
   RANDOMIZE TIMER

   'Εύρεση τυχαίου προσήμου
   sign1 = RandNum(1, 2)

   'Εύρεση τυχαίων αριθμών που το άθροισμα ή η διαφορά τους δεν είναι
   'μεγαλύτερη του limit%
   DO
      a = RandNum(1, limit% - 1)
      b = RandNum(1, limit% - 1)
      c = a + b * (-1) ^ sign1
   LOOP WHILE c > limit%
   IF c < 0 THEN
      aTEMP = a
      a = b
      b = aTEMP
      c = -c
   END IF

  'Προσδιορισμός προσήμων
   sign1$ = "+"
   IF sign1 = 1 THEN sign1$ = "-"

   'Εισαγωγή απάντησης από χρήστη
   TimeStart# = TIMER
   LOCATE 12, 25: PRINT "(Δώσε τον αριθμό και πίεσε Enter)"
   LOCATE 10, 25: PRINT Name$; " ΠΟΣΟ ΚΑΝΟΥΝ "; a; sign1$; b; "= ";
   INPUT "", answer

   'Ελεγχος ορθότητας της απάντησης
   TimeEnd# = TIMER
   TimeSpent# = TimeSpent# + (TimeEnd# - TimeStart#)
   IF answer = c THEN
      CALL Good
   ELSE
      CALL Oops
      LOCATE 12, 22: PRINT "Λ Α Θ Ο Σ. ΤΟ ΣΩΣΤΟ ΕΙΝΑΙ:"; a; sign1$; b; "="; c
   END IF
 
   'Παρουσίαση αποτελεσμάτων
   CALL DisplayResults
END SUB

'------------------------- Addition3 ---------------------------
'Εξάσκηση στην πρόσθεση με 3 προσθεταίους
'---------------------------------------------------------------
SUB Addition3
   'Καθάρισμα οθόνης και απόδοση αρχικής τιμής σε γεννήτρια
   CLS
   RANDOMIZE TIMER

   'Εύρεση πρώτου τυχαίου προσήμου
   sign1 = RandNum(1, 2)

   'Εύρεση τυχαίων αριθμών που το άθροισμα ή η διαφορά τους δεν είναι
   'μεγαλύτερη του limit%
   DO
      a = RandNum(1, limit% - 1)
      b = RandNum(1, limit% - 1)
      c = a + b * (-1) ^ sign1
   LOOP WHILE c > limit%
   IF c < 0 THEN
      aTEMP = a
      a = b
      b = aTEMP
      c = -c
   END IF

   'Εύρεση δευτέρου τυχαίου προσήμου
   sign2 = RandNum(1, 2)

   'Εύρεση τρίτου τυχαίου αριθμού που το άθροισμα ή η διαφορά του με τους δυο
   'προηγούμενους δεν είναι μεγαλύτερη του limit%
   iter = 1
   DO
      IF iter <= 99 THEN d = RandNum(1, limit% - 1) ELSE d = 0
      e = c + d * (-1) ^ sign2
      iter = iter + 1
   LOOP WHILE (e > limit% OR e < 0) AND iter <= 100
 
  'Προσδιορισμός προσήμων
   sign1$ = "+": sign2$ = "+"
   IF sign1 = 1 THEN sign1$ = "-"
   IF sign2 = 1 THEN sign2$ = "-"

   'Εισαγωγή απάντησης από χρήστη
   TimeStart# = TIMER
   LOCATE 12, 25: PRINT "(Δώσε τον αριθμό και πίεσε Enter)"
   LOCATE 10, 25: PRINT Name$; " ΠΟΣΟ ΚΑΝΟΥΝ "; a; sign1$; b; sign2$; d; "= ";
   INPUT "", answer

   'Ελεγχος ορθότητας της απάντησης
   TimeEnd# = TIMER
   TimeSpent# = TimeSpent# + (TimeEnd# - TimeStart#)
   IF answer = e THEN
      CALL Good
   ELSE
      CALL Oops
      LOCATE 12, 22: PRINT "Λ Α Θ Ο Σ. ΤΟ ΣΩΣΤΟ ΕΙΝΑΙ:"; a; sign1$; b; sign2$; d; "="; e
   END IF
  
   'Παρουσίαση αποτελεσμάτων
   CALL DisplayResults
END SUB

'------------------------ DisplayResults -----------------------
'Παρουσιάζει το σκορ και την ταχύτητα
'---------------------------------------------------------------
SUB DisplayResults
   LOCATE 16, 29: PRINT "ΤΟ ΣΚΟΡ ΕΙΝΑΙ:"; Correct%; "-"; InCorrect%
   LOCATE 17, 28: PRINT "EXEIΣ KEPΔIΣEI:"; 10 * Correct%; "ΔPAXMEΣ"

'  IF Correct% >= 10 THEN
'     LOCATE 18, 20: PRINT USING "H TAXYTHTA ΣOY EINAI:#####,"; CINT(1000 * TimeSpent# / Correct%);
'     PRINT " ΔΕΥΤΕΡΟΛΕΠΤA/ΠPAΞH"
'  END IF
END SUB

'----------------------------- Division --------------------------
'Εξάσκηση στη διαίρεση
'-----------------------------------------------------------------
SUB Division
   'Καθάρισμα οθόνης και απόδοση αρχικής τιμής σε γεννήτρια
   CLS
   RANDOMIZE TIMER

   'Εύρεση τυχαίων αριθμών μικρότερων του limitdiv% που το πηλίκον τους είναι
   'ακέραιος αριθμός
   DO
      a = RandNum(3, limitdiv%)
      IF a > 10 THEN
         b = RandNum(2, 10)
      ELSE
         b = RandNum(2, a)
      END IF
      c = a / b
   LOOP WHILE a MOD b <> 0

   'Εισαγωγή απάντησης από χρήστη
   TimeStart# = TIMER
   LOCATE 12, 25: PRINT "(Δώσε τον αριθμό και πίεσε Enter)"
   LOCATE 10, 25: PRINT Name$; " ΠΟΣΟ ΚΑΝΟΥΝ "; a; ":"; b; "= ";
   INPUT "", d

   'Ελεγχος ορθότητας της απάντησης
   TimeEnd# = TIMER
   TimeSpent# = TimeSpent# + (TimeEnd# - TimeStart#)
   IF d = c THEN
      CALL Good
   ELSE
      CALL Oops
      LOCATE 12, 22: PRINT "Λ Α Θ Ο Σ. ΤΟ ΣΩΣΤΟ ΕΙΝΑΙ:"; a; ":"; b; "="; c
   END IF

   'Παρουσίαση αποτελεσμάτων
   CALL DisplayResults
  
END SUB

'--------------------- Good ---------------------
'Απόκριση σε σωστές απαντήσεις με αύξοντες τόνους
'και αρίθμηση των σωστών απαντήσεων
'------------------------------------------------
SUB Good STATIC
   IF InCorrectFlag% = 1 THEN Pitch = 200
   FOR Freq = Pitch TO Pitch + 200 STEP 20
      SOUND Freq, 1
   NEXT Freq
   Pitch = Pitch + 30
   Correct% = Correct% + 1
   InCorrectFlag% = 0
   LOCATE 12, 25: PRINT STRING$(33, " ")
   LOCATE 12, 29: PRINT "M Π P A B O  "; Name$
END SUB

'--------------------------- Multiplication ----------------------------
'Εξάσκηση στον πολλαπλασιασμό
'-----------------------------------------------------------------------
SUB Multiplication
   'Καθάρισμα οθόνης και απόδοση αρχικής τιμής σε γεννήτρια
   CLS
   RANDOMIZE TIMER

   'Εύρεση τυχαίων αριθμών που το γινόμενό τους δεν είναι μεγαλύτερο του
   'limit%
   DO
      a = RandNum(2, 10)
      b = RandNum(2, 10)
      c = a * b
   LOOP WHILE c > limit%

   'Εισαγωγή απάντησης από χρήστη
   TimeStart# = TIMER
   LOCATE 12, 25: PRINT "(Δώσε τον αριθμό και πίεσε Enter)"
   LOCATE 10, 25: PRINT Name$; " ΠΟΣΟ ΚΑΝΟΥΝ "; a; "."; b; "= ";
   INPUT "", d
 
   'Ελεγχος ορθότητας της απάντησης
   TimeEnd# = TIMER
   TimeSpent# = TimeSpent# + (TimeEnd# - TimeStart#)
   IF d = c THEN
      CALL Good
   ELSE
      CALL Oops
      LOCATE 12, 22: PRINT "Λ Α Θ Ο Σ. ΤΟ ΣΩΣΤΟ ΕΙΝΑΙ:"; a; "."; b; "="; c
   END IF

   'Παρουσίαση αποτελεσμάτων
   CALL DisplayResults
END SUB

'-------------------- Oops ----------------------
'Απόκριση σε λάθος απαντήσεις με φθήνοντες τόνους
'και αρίθμηση των λανθασμένων απαντήσεων
'------------------------------------------------
SUB Oops STATIC
   FOR Freq = 200 TO 50 STEP -10
      SOUND Freq, 1
   NEXT Freq
   SOUND 100, 3
   InCorrect% = InCorrect% + 1
   InCorrectFlag% = 1
   LOCATE 12, 25: PRINT STRING$(33, " ")
END SUB

'----------------------- Practice ------------------------
'Επιλογή πράξης
'---------------------------------------------------------
SUB Practice
   VIEW PRINT
   CLS
   
   RANDOMIZE TIMER
   choice = RandNum(1, 4)
   SELECT CASE choice
      CASE 1
         CALL Addition2
      CASE 2
         CALL Addition3
      CASE 3
         CALL Multiplication
      CASE 4
         CALL Division
   END SELECT
END SUB

'-------------------------- RandNum -----------------------------
'Παραγωγή τυχαίων αριθμών στην περιοχή Low ως High
'----------------------------------------------------------------
FUNCTION RandNum (Low, High)
   RandNum = INT(RND * (High - Low + 1)) + Low
END FUNCTION

