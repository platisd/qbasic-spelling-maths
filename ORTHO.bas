'==========================================================
'                         ORTHO.BAS
'           Πρόγραμμα εξάσκησης στην ορθογραφία
'==========================================================

DEFINT A-Z   'Επιλογή ακεραίων σαν πρότυπο τύπο δεδομένων

'Τύπος δομής κάθε εγγραφής
TYPE RecordType
   WordName AS STRING * 20
END TYPE

'Ορισμός μεταβλητής εγγραφής που χρησιμοποιεί τον τύπο
DIM SHARED WordRecord AS RecordType
DIM SHARED Word AS RecordType

'Δήλωση και ορισμός καθολικών μεταβλητών
COMMON SHARED Correct, Incorrect, TimeSpent
COMMON SHARED ESC$, SpaceBar$, Name$, limit%
COMMON SHARED NumberOfRecords

'Δηλώσεις διαδικασιών
DECLARE SUB AddRecord (Word AS RecordType)
DECLARE SUB DisplayRecord (Word AS RecordType)
DECLARE SUB Update (Word AS RecordType)
DECLARE SUB Practice ()
DECLARE SUB Lexiko ()
DECLARE SUB Good ()
DECLARE SUB Oops ()
DECLARE SUB DisplayResults ()
DECLARE SUB PressKey (PositionX%, PositionY%, String1$, String2$)
DECLARE SUB Siren (Hi%, Range%)
DECLARE SUB AddLetter (Word AS RecordType)
DECLARE FUNCTION RandNum% (Low%, High%)
DECLARE FUNCTION GreekLetter$ (Latin$)

'Απόδοση αρχικής τιμής σε καθολικές μεταβλητές και καθάρισμα οθόνης
Correct = 0: Incorrect = 0: TimeSpent = 0
ESC$ = CHR$(27): SpaceBar$ = CHR$(32): Zero$ = CHR$(48)
'==================================================================
Name$ = "ΔΗΜΗΤΡΗ": limit% = 20
'==================================================================
COLOR 15, 4
CLS

'Δημιουργία φόρμας εκκίνησης
LOCATE 2, 2: PRINT CHR$(201); STRING$(75, 205); CHR$(187)
FOR i = 3 TO 20
   LOCATE i, 2: PRINT CHR$(186); TAB(78); CHR$(186)
NEXT i
LOCATE i, 2: PRINT CHR$(200); STRING$(75, 205); CHR$(188)
LOCATE 8, 32: PRINT "Π P O Γ P A M M A"
LOCATE 10, 30: PRINT "O P Θ O Γ P A Φ I A Σ"
LOCATE 12, 30: PRINT "B'  Δ H M O T I K O Y "
LOCATE 18, 50: PRINT "ΓΡAMMENO AΠO TO ΣΠ. ΠΛATH"
LOCATE 19, 61: PRINT "ΓIA TO ΔHMHTPH"

'Ανοιγμα αρχείου και δήλωση μήκους εγγραφής
OPEN "WordsKrt.dat" FOR RANDOM AS #1 LEN = LEN(WordRecord)

'Υπολογισμός αριθμού εγγραφών
NumberOfRecords = LOF(1) \ LEN(WordRecord)

'Περιμένει για οδηγίες
DO
   VIEW PRINT 22 TO 25
   CALL PressKey(23, 25, "MΠAPA", "ΓIA NA ΣΥΝEXIΣEIΣ")
   CALL PressKey(24, 5, "ESC", "ΓIA NA ΣTAMATHΣEIΣ")
   CALL PressKey(24, 50, "0", "ΓIA ΛEΞEIΣ KAPTEΛAΣ")

   choice$ = INPUT$(1)
   SELECT CASE choice$
      CASE ESC$
         COLOR 15, 0
         VIEW PRINT: CLS
         EXIT DO
      CASE SpaceBar$
         CALL Practice
      CASE Zero$
         CALL Lexiko
         VIEW PRINT 22 TO 25: CLS
   END SELECT
LOOP
 
'Κλείσιμο αρχείου
CLOSE #1

'Φωνήεντα, διπλά φωνήεντα και σύμφωνα και δίφθογγοι
DATA ε, η, ι, ο, υ, ω, έ, ή, ί, ό, ύ, ώ, Ε, Η, Ι, Ο, Υ, Ω, Έ, Ή, Ί, Ό, Ύ, Ώ
DATA ει, οι, αι, εί, οί, αί, εϊ, οϊ, αϊ, Ει, Οι, Aι, Εί, Οί, Aί
DATA αυ, ευ, ου, αύ, εύ, ού, Aυ, Ευ, Ου, Aύ, Εύ, Ού
DATA σσ, μμ, κκ, ββ, νν, λλ, ρρ, ππ
DATA γκ, γγ, Γκ

SUB AddLetter (Word AS RecordType) STATIC
    SHARED NumberOfRecords
      AnswerPart$ = ""
      i = 0
      LOCATE 12, 27: PRINT STRING$(40, " ")
      LOCATE 10, 18: PRINT "ΠΟΙA ΕΙΝAΙ H ΛEΞH KAPTEΛAΣ: ";
      Ypos = POS(0)
   
      'Μετατροπή λατινικών χαρακτήρων εισόδου σε Ελληνικούς
      DO
         DO
            Answer$ = INPUT$(1)
            GreekAnswer$ = GreekLetter$(Answer$)
            IF GreekAnswer$ = "WrongKey" THEN
               LOCATE 12, 18: COLOR 29, 4
               PRINT "Το κουμπί που πάτησες δεν αντιστοιχεί σε γράμμα"
               LOCATE 14, 25: COLOR 15, 4
               PRINT "Πάτησε ένα κουμπί για συνέχεια."
               Siren 780, 650
               AnyKey$ = INPUT$(1)
               LOCATE 12, 18: PRINT STRING$(50, " ")
               LOCATE 14, 25: PRINT STRING$(50, " ")
            END IF
         LOOP WHILE GreekAnswer$ = "WrongKey"
         LOCATE 10, Ypos + i: PRINT GreekAnswer$
         i = i + 1
         AnswerPart$ = AnswerPart$ + GreekAnswer$
      LOOP WHILE GreekAnswer$ <> ""
      Word.WordName = AnswerPart$

END SUB

'---------------------- AddRecord -----------------------
'Εισαγωγή εγγραφών στο αρχείο Words.dat
'--------------------------------------------------------
SUB AddRecord (Word AS RecordType) STATIC
   SHARED NumberOfRecords
   DO
      'Εμφάνιση επικεφαλίδας
      LOCATE 10, 20: PRINT "EIΣAΓΩΓH NEΩN ΛEΞEΩN KAPTEΛAΣ"
      LOCATE 11, 20: PRINT STRING$(29, 205)

      'Εισαγωγή δεδομένων
      CLS
      CALL AddLetter(Word)

      'Υπολογισμός αριθμού εγγραφής
      NumberOfRecords = NumberOfRecords + 1

      'Εγγραφή δεδομένων στο αρχείο
      PUT #1, NumberOfRecords, Word

      PRINT : PRINT TAB(20); "Θα εισάγετε άλλη; (Ν/Ο) [";
      COLOR 26, 4: PRINT "N";
      COLOR 15, 4: PRINT "]"
      Continue$ = UCASE$(INPUT$(1))
   LOOP WHILE Continue$ <> "O"
END SUB

'------------------------ DisplayRecord --------------------------
'Εμφάνιση λέξεων καρτέλας με τους Α/Α αριθμούς στην οθόνη
'-----------------------------------------------------------------
SUB DisplayRecord (Word AS RecordType)
   SHARED NumberOfRecords

   'Εμφάνιση επικεφαλίδας
   CLS
   LOCATE 1, 25: PRINT "A/A KAI ΛEΞEIΣ KAPTEΛAΣ"
   LOCATE 2, 24: PRINT STRING$(25, 205)
  
   'Ανάγνωση και εμφάνιση λέξεων καρτέλας
   LineCounter = 0: RecordCounter = 0
   FOR RecordNumber = 1 TO NumberOfRecords
      GET #1, RecordNumber, Word
      RecordCounter = RecordCounter + 1
      LineCounter = LineCounter + 1
      IF RecordCounter <= 20 THEN
            TabStop = 0
         ELSEIF RecordCounter <= 40 THEN
            TabStop = 25
         ELSE
            TabStop = 50
      END IF
      LOCATE 2 + LineCounter, 1 + TabStop: PRINT RecordNumber;
      LOCATE 2 + LineCounter, 6 + TabStop: PRINT Word.WordName
      IF RecordCounter = 60 THEN
         LOCATE 24, 1: PRINT TAB(25); "ΠATHΣE ENA KOYMΠI ΓIA ΣYNEXEIA..."
         AnyKey$ = INPUT$(1)
         CLS
         LOCATE 1, 25: PRINT "A/A KAI ΛEΞEIΣ KAPTEΛAΣ"
         LOCATE 2, 24: PRINT STRING$(25, 205)
      END IF
      IF LineCounter = 20 THEN LineCounter = 0
      IF RecordCounter = 60 THEN RecordCounter = 0
   NEXT RecordNumber
   PRINT
END SUB

'------------------------ DisplayResults -----------------------
'Παρουσιάζει το σκορ και ερωτά για τη συνέχεια
'---------------------------------------------------------------
SUB DisplayResults
   LOCATE 16, 29: PRINT "ΤΟ ΣΚΟΡ ΕΙΝΑΙ:"; Correct; "-"; Incorrect
END SUB

'--------------------- Good ---------------------
'Απόκριση σε σωστές απαντήσεις με αύξοντες τόνους
'και αρίθμηση των σωστών απαντήσεων
'------------------------------------------------
SUB Good STATIC
   IF Correct = 0 THEN Pitch = 200
   FOR Freq = Pitch TO Pitch + 200 STEP 20
      SOUND Freq, 1
   NEXT Freq
   Pitch = Pitch + 30
   Correct = Correct + 1
   LOCATE 12, 22: PRINT STRING$(40, " ")
   LOCATE 12, 29: PRINT "M Π P A B O  "; Name$
END SUB

'------------------------------- GreekLetter -------------------------------
'Μετατροπή λατινικών χαρακτήρων του πληκτρολογίου σε αντίστοιχους Ελληνικούς
'---------------------------------------------------------------------------
FUNCTION GreekLetter$ (l$)
SELECT CASE l$
   CASE "a"
      GreekLetter$ = CHR$(152)
   CASE "b"
      GreekLetter$ = CHR$(153)
   CASE "c"
      GreekLetter$ = CHR$(175)
   CASE "d"
      GreekLetter$ = CHR$(155)
   CASE "e"
      GreekLetter$ = CHR$(156)
   CASE "f"
      GreekLetter$ = CHR$(173)
   CASE "g"
      GreekLetter$ = CHR$(154)
   CASE "h"
      GreekLetter$ = CHR$(158)
   CASE "i"
      GreekLetter$ = CHR$(160)
   CASE "j"
      GreekLetter$ = CHR$(165)
   CASE "k"
      GreekLetter$ = CHR$(161)
   CASE "l"
      GreekLetter$ = CHR$(162)
   CASE "m"
      GreekLetter$ = CHR$(163)
   CASE "n"
      GreekLetter$ = CHR$(164)
   CASE "o"
      GreekLetter$ = CHR$(166)
   CASE "p"
      GreekLetter$ = CHR$(167)
   CASE "r"
      GreekLetter$ = CHR$(168)
   CASE "s"
      GreekLetter$ = CHR$(169)
   CASE "t"
      GreekLetter$ = CHR$(171)
   CASE "u"
      GreekLetter$ = CHR$(159)
   CASE "v"
      GreekLetter$ = CHR$(224)
   CASE "w"
      GreekLetter$ = CHR$(170)
   CASE "x"
      GreekLetter$ = CHR$(174)
   CASE "y"
      GreekLetter$ = CHR$(172)
   CASE "z"
      GreekLetter$ = CHR$(157)
   CASE "A"
      GreekLetter$ = CHR$(65)
   CASE "B"
      GreekLetter$ = CHR$(129)
   CASE "C"
      GreekLetter$ = CHR$(150)
   CASE "D"
      GreekLetter$ = CHR$(131)
   CASE "E"
      GreekLetter$ = CHR$(132)
   CASE "F"
      GreekLetter$ = CHR$(148)
   CASE "G"
      GreekLetter$ = CHR$(130)
   CASE "H"
      GreekLetter$ = CHR$(134)
   CASE "I"
      GreekLetter$ = CHR$(136)
   CASE "J"
      GreekLetter$ = CHR$(141)
   CASE "K"
      GreekLetter$ = CHR$(137)
   CASE "L"
      GreekLetter$ = CHR$(138)
   CASE "M"
      GreekLetter$ = CHR$(139)
   CASE "N"
      GreekLetter$ = CHR$(140)
   CASE "O"
      GreekLetter$ = CHR$(142)
   CASE "P"
      GreekLetter$ = CHR$(143)
   CASE "R"
      GreekLetter$ = CHR$(144)
   CASE "S"
      GreekLetter$ = CHR$(145)
   CASE "T"
      GreekLetter$ = CHR$(146)
   CASE "U"
      GreekLetter$ = CHR$(135)
   CASE "V"
      GreekLetter$ = CHR$(151)
   CASE "X"
      GreekLetter$ = CHR$(149)
   CASE "Y"
      GreekLetter$ = CHR$(147)
   CASE "Z"
      GreekLetter$ = CHR$(133)
   CASE CHR$(13)
      GreekLetter$ = ""
   CASE ";"
      l2$ = INPUT$(1)
      SELECT CASE l2$
         CASE "a"
            GreekLetter$ = CHR$(225)
         CASE "e"
            GreekLetter$ = CHR$(226)
         CASE "h"
            GreekLetter$ = CHR$(227)
         CASE "i"
            GreekLetter$ = CHR$(229)
         CASE "o"
            GreekLetter$ = CHR$(230)
         CASE "y"
            GreekLetter$ = CHR$(231)
         CASE "v"
            GreekLetter$ = CHR$(233)
         CASE "A"
            GreekLetter$ = CHR$(234)
         CASE "E"
            GreekLetter$ = CHR$(235)
         CASE "H"
            GreekLetter$ = CHR$(236)
         CASE "I"
            GreekLetter$ = CHR$(237)
         CASE "O"
            GreekLetter$ = CHR$(238)
         CASE "Y"
            GreekLetter$ = CHR$(239)
         CASE "V"
            GreekLetter$ = CHR$(240)
         CASE ELSE
            GreekLetter$ = "WrongKey"
      END SELECT
   CASE ":"
      l3$ = INPUT$(1)
      SELECT CASE l3$
         CASE "i"
            GreekLetter$ = CHR$(228)
         CASE "y"
            GreekLetter$ = CHR$(232)
         CASE ELSE
            GreekLetter$ = "WrongKey"
       END SELECT
   CASE ELSE
      GreekLetter$ = "WrongKey"
END SELECT

END FUNCTION

'----------------------------- Lexiko ---------------------------
'Εισαγωγή/Επεξεργασία λέξεων καρτέλας
'----------------------------------------------------------------
SUB Lexiko
   VIEW PRINT
   CLS

   'Ελεγχος κωδικού πρόσβασης
  ' LOCATE 8, 20: PRINT "EΛEΓXOΣ ΠΡOΣBAΣHΣ XΡHΣTH ΣTIΣ ΛEΞEIΣ KAΡTEΛAΣ"
  ' LOCATE 10, 15: PRINT "Δώσε το κλειδί πρόσβασης: ";
  ' FOR n = 1 TO 5
  '    PassWordLetter$ = INPUT$(1)
  '    PassWord$ = PassWord$ + PassWordLetter$
  '    PRINT "*";
  ' NEXT n
  ' IF PassWord$ <> "18457" THEN
  '    LOCATE 12, 15: PRINT "Λυπάμαι αλλά δεν είσαι εξουσιωδοτημένος χρήστης"
  '    LOCATE 18, 25: PRINT "ΠATHΣE ENA ΠΛHKTPO ΓIA ΣYNEXEIA"
  '    AnyKey$ = INPUT$(1)
  '    EXIT SUB
  ' END IF

   'Εισαγωγή νέας εγγραφής
   CLS
   LOCATE 9, 15: PRINT "Θέλετε να προσθέσετε νέα λέξη καρτέλας στο αρχείο (Ν/Ο); "
   Response$ = UCASE$(INPUT$(1))
   IF Response$ = "N" THEN CALL AddRecord(WordRecord)

   'Εμφάνιση εγγραφών του αρχείου
   CALL DisplayRecord(WordRecord)

   'Διόρθωση υπάρχουσας εγγραφής
   LOCATE 24, 1: PRINT TAB(20); "Θέλετε να διορθώσετε μιά εγγραφή; (Ν/Ο) "
   Response$ = UCASE$(INPUT$(1))
   IF Response$ = "N" THEN CALL Update(WordRecord)

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
   Incorrect = Incorrect + 1
   LOCATE 12, 19: PRINT STRING$(40, " ")
END SUB

'----------------------------- Practice ----------------------------
'Εισαγωγή/Επεξεργασία εγγραφών
'-------------------------------------------------------------------
SUB Practice
   VIEW PRINT
   CLS

   DIM PossibleComb(1 TO 10, 1 TO 2) AS INTEGER

   RANDOMIZE TIMER
   RecordNumber = RandNum(1, NumberOfRecords)

   'Εισαγωγή νέας εγγραφής
   GET #1, RecordNumber, Word
  
   'Δημιουργία της άγνωστης λέξης
   i = 0
   RESTORE
   FOR m = 1 TO 62
      READ MissingLetter$
      PositionLetter = INSTR(Word.WordName, MissingLetter$)
      IF PositionLetter <> 0 THEN
         i = i + 1
         PossibleComb(i, 1) = LEN(MissingLetter$)
         PossibleComb(i, 2) = PositionLetter
      END IF
   NEXT m

   RANDOMIZE TIMER
   NumberComb = RandNum(1, i)
   LeftPartLen = PossibleComb(NumberComb, 2) - 1
   CorrectAnswer$ = RTRIM$(Word.WordName)
   RightPartLen = LEN(CorrectAnswer$) - LeftPartLen - PossibleComb(NumberComb, 1)
   LeftPart$ = LEFT$(CorrectAnswer$, LeftPartLen)
   RightPart$ = RIGHT$(CorrectAnswer$, RightPartLen)
   MidPart$ = STRING$(PossibleComb(NumberComb, 1), "_")
   Question$ = LeftPart$ + MidPart$ + RightPart$

   PositionOf1stDush = INSTR(Question$, "_")
   PositionOf2ndDush = INSTR(PositionOf1stDush + 1, Question$, "_")
   IF PositionOf2ndDush = 0 THEN iter = 1 ELSE iter = 2

   'Εισαγωγή απάντησης από χρήστη
   DO
      AnswerPart$ = ""
      LOCATE 12, 27: PRINT STRING$(40, " ")
      LOCATE 10, 18: PRINT Name$; " ΠΟΙΑ ΕΙΝΑΙ ΤΑ ΓΡΑΜΜΑΤΑ ΠΟΥ ΛΕΙΠΟΥΝ: ";
      Ypos = POS(0) + PositionOf1stDush - 1
      PRINT Question$;
     
      'Μετατροπή λατινικών χαρακτήρων εισόδου σε Ελληνικούς
      FOR i = 1 TO iter
         DO
            Answer$ = INPUT$(1)
            GreekAnswer$ = GreekLetter$(Answer$)
            IF GreekAnswer$ = "WrongKey" OR GreekAnswer$ = "" THEN
               LOCATE 12, 18: COLOR 29, 4
               PRINT "Το κουμπί που πάτησες δεν αντιστοιχεί σε γράμμα"
               LOCATE 14, 25: COLOR 15, 4
               PRINT "Πάτησε ένα κουμπί για συνέχεια."
               Siren 780, 650
               AnyKey$ = INPUT$(1)
               LOCATE 12, 18: PRINT STRING$(50, " ")
               LOCATE 14, 25: PRINT STRING$(50, " ")
            END IF
         LOOP WHILE GreekAnswer$ = "WrongKey" OR GreekAnswer$ = ""
         LOCATE 10, Ypos + i - 1: PRINT GreekAnswer$
         AnswerPart$ = AnswerPart$ + GreekAnswer$
      NEXT i
      LOCATE 12, 27: PRINT "Είναι όλα σωστά; (Ν/Ο) [";
      COLOR 26, 4: PRINT "N";
      COLOR 15, 4: PRINT "]"
      YesNo$ = UCASE$(INPUT$(1))
   LOOP WHILE YesNo$ = "O"

   LeftPart$ = LEFT$(Question$, PositionOf1stDush - 1)
   IF PositionOf2ndDush = 0 THEN
      RightPart$ = RIGHT$(Question$, LEN(Question$) - PositionOf1stDush)
   ELSE
      RightPart$ = RIGHT$(Question$, LEN(Question$) - PositionOf2ndDush)
   END IF
   CompleteAnswer$ = LeftPart$ + AnswerPart$ + RightPart$

   'Ελεγχος ορθότητας της απάντησης
   IF CompleteAnswer$ = CorrectAnswer$ THEN
      CALL Good
   ELSE
      CALL Oops
      LOCATE 12, 19: PRINT "Λ Α Θ Ο Σ. ΤΟ ΣΩΣΤΟ ΕΙΝΑΙ: "; CorrectAnswer$;
      PRINT " ΚΑΙ ΟΧΙ "; CompleteAnswer$
   END IF
 
   'Παρουσίαση αποτελεσμάτων
   CALL DisplayResults

END SUB

SUB PressKey (PositionX%, PositionY%, String1$, String2$)
   LOCATE PositionX%, PositionY%: PRINT "ΠIEΣE ";
   COLOR 10, 4: PRINT String1$; " ";
   COLOR 15, 4: PRINT String2$
END SUB

'-------------------------- RandNum -----------------------------
'Παραγωγή τυχαίων αριθμών στην περιοχή Low ως High
'----------------------------------------------------------------
FUNCTION RandNum (Low, High)
   RandNum = INT(RND * (High - Low + 1)) + Low
END FUNCTION

'------------------------- Siren ---------------------------
' Loop a sound from low to high to low
'-----------------------------------------------------------
SUB Siren (Hi%, Range%)
   DO WHILE INKEY$ = ""
      FOR Count = Range TO -Range STEP -4
         SOUND Hi - ABS(Count), .3
         Count = Count - 2 / Range
      NEXT Count
   LOOP
'
END SUB

'-------------------------- Update ---------------------------
'Χρήση αριθμών εγγραφών για διόρθωση συγκεκριμένης εγγραφής
'-------------------------------------------------------------
SUB Update (Word AS RecordType)
   SHARED NumberOfRecords
   DO
      'Εύρεση εγγραφής προς ενημέρωση
      INPUT "ΔΩΣTE TON A/A THΣ ΛEΞEΩΣ KAPTEΛAΣ: ", RecordNumber
      CLS

      'Δήλωσε αν ο Α/Α της εγγραφής δεν υπάρχει
      IF RecordNumber > NumberOfRecords THEN
         LOCATE 10, 25: PRINT "Η εγγραφή δεν υπάρχει."

      'ή ενημέρωσε το αρχείο
      ELSE
         'Λήψη νέας λέξης και γραμμάτων
         CALL AddLetter(Word)

         'Γράψε τη διορθωμένη εγγραφή στο αρχείο
         PUT #1, RecordNumber, Word
         PRINT

         'Εμφάνιση της ενημερωμένης λίστας
         CALL DisplayRecord(Word)
      END IF
      LOCATE 24, 1: PRINT TAB(20); "Θα αλλάξετε άλλες εγγραφές (Ν/Ο); "
      Continue$ = UCASE$(INPUT$(1))
   LOOP WHILE Continue$ = "N"
END SUB

