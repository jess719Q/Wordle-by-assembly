include Irvine32.inc

mWriteSpace MACRO count:=<1>
LOCAL spaces

.data
spaces BYTE count DUP(' '),0
.code
	push edx
	mov edx, OFFSET spaces
	call WriteString
	pop edx
ENDM
mWriteString MACRO buffer:REQ
	push edx
	mov edx, OFFSET buffer
	call WriteString
	pop edx
ENDM
check_Valid_Word PROTO,
	source: PTR BYTE,
	target: PTR BYTE
allCorrect PROTO,
	source: PTR BYTE,
	target: PTR BYTE
AddNChr PROTO,
	nChr: BYTE,
	list: PTR BYTE,
	lLen: BYTE
ChangeWordColor PROTO,
	pHandle: DWORD,
	xyPos: COORD
ChangeChrsColor PROTO,
	pHandle: DWORD
Reset PROTO, 
	list: PTR BYTE, 
	lLen: BYTE

.data

;Word List
word_lst BYTE "apple"
    BYTE "apply"
    BYTE "arena"
    BYTE "argue"
    BYTE "arise"
    BYTE "array"
    BYTE "aside"
    BYTE "asset"
    BYTE "audio"
    BYTE "audit"
    BYTE "avoid"
    BYTE "award"
    BYTE "aware"
    BYTE "badly"
    BYTE "baker"
    BYTE "bases"
    BYTE "basic"
    BYTE "basis"
    BYTE "beach"
    BYTE "began"
    BYTE "begin"
    BYTE "begun"
    BYTE "being"
    BYTE "below"
    BYTE "bench"
    BYTE "billy"
    BYTE "birth"
    BYTE "black"
    BYTE "blame"
    BYTE "blind"
    BYTE "block"
    BYTE "blood"
    BYTE "board"
    BYTE "boost"
    BYTE "booth"
    BYTE "bound"
    BYTE "brain"
    BYTE "brand"
    BYTE "bread"
    BYTE "break"
    BYTE "breed"
    BYTE "brief"
    BYTE "bring"
    BYTE "broad"
    BYTE "broke"
    BYTE "brown"
    BYTE "build"
    BYTE "built"
    BYTE "buyer"
    BYTE "cable"
    BYTE "calif"
    BYTE "carry"
    BYTE "catch"
    BYTE "cause"
    BYTE "chain"
    BYTE "chair"
    BYTE "chart"
    BYTE "chase"
    BYTE "cheap"
    BYTE "check"
    BYTE "chest"
    BYTE "chief"
    BYTE "child"
    BYTE "china"
    BYTE "chose"
    BYTE "civil"
    BYTE "claim"
    BYTE "class"
    BYTE "clean"
    BYTE "clear"
    BYTE "click"
    BYTE "clock"
    BYTE "close"
    BYTE "coach"
    BYTE "coast"
    BYTE "could"
    BYTE "count"
    BYTE "court"
    BYTE "cover"
    BYTE "craft"
    BYTE "crash"
    BYTE "cream"
    BYTE "crime"
    BYTE "cross"
    BYTE "crowd"
    BYTE "crown"
    BYTE "curve"
    BYTE "cycle"
    BYTE "daily"
    BYTE "dance"
    BYTE "dated"
    BYTE "dealt"
    BYTE "death"
    BYTE "debut"
    BYTE "delay"
    BYTE "depth"
    BYTE "doing"
    BYTE "doubt"
    BYTE "dozen"
    BYTE "draft"
    BYTE "drama"
    BYTE "drawn"
    BYTE "dream"
    BYTE "dress"
    BYTE "drill"
    BYTE "drink"
    BYTE "drive"
    BYTE "drove"
    BYTE "dying"
    BYTE "eager"
    BYTE "early"
    BYTE "earth"
    BYTE "eight"
    BYTE "elite"
    BYTE "empty"
    BYTE "enemy"
    BYTE "enjoy"
    BYTE "enter"
    BYTE "entry"
    BYTE "equal"
    BYTE "error"
    BYTE "event"
    BYTE "every"
    BYTE "exact"
    BYTE "exist"
    BYTE "extra"
    BYTE "faith"
    BYTE "false"
    BYTE "fault"
    BYTE "fiber"
    BYTE "field"
    BYTE "fifth"
    BYTE "fifty"
    BYTE "fight"
    BYTE "final"
    BYTE "first"
    BYTE "fixed"
    BYTE "flash"
    BYTE "fleet"
    BYTE "floor"
    BYTE "fluid"
    BYTE "focus"
    BYTE "force"
    BYTE "forth"
    BYTE "forty"
    BYTE "forum"
    BYTE "found"
    BYTE "frame"
    BYTE "frank"
    BYTE "fraud"
    BYTE "fresh"
    BYTE "front"
    BYTE "fruit"
    BYTE "fully"
    BYTE "funny"
    BYTE "giant"
    BYTE "given"
    BYTE "glass"
    BYTE "globe"
    BYTE "going"
    BYTE "grace"
    BYTE "grade"
    BYTE "grand"
    BYTE "grant"
    BYTE "grass"
    BYTE "great"
    BYTE "green"
    BYTE "gross"
    BYTE "group"
    BYTE "grown"
    BYTE "guard"
    BYTE "guess"
    BYTE "guest"
    BYTE "guide"
    BYTE "happy"
    BYTE "harry"
    BYTE "heart"
    BYTE "heavy"
    BYTE "hence"
    BYTE "henry"
    BYTE "horse"
    BYTE "hotel"
    BYTE "house"
    BYTE "human"
    BYTE "ideal"
    BYTE "image"
    BYTE "index"
    BYTE "inner"
    BYTE "input"
    BYTE "issue"
    BYTE "japan"
    BYTE "jimmy"
    BYTE "joint"
    BYTE "jones"
    BYTE "judge"
    BYTE "known"
    BYTE "label"
    BYTE "large"
    BYTE "laser"
    BYTE "later"
    BYTE "laugh"
    BYTE "layer"
    BYTE "learn"
    BYTE "lease"
    BYTE "least"
    BYTE "leave"
    BYTE "legal"
    BYTE "level"
    BYTE "lewis"
    BYTE "light"
    BYTE "limit"
    BYTE "links"
    BYTE "lives"
    BYTE "local"
    BYTE "logic"
    BYTE "loose"
    BYTE "lower"
    BYTE "lucky"
    BYTE "lunch"
    BYTE "lying"
    BYTE "magic"
    BYTE "major"
    BYTE "maker"
    BYTE "march"
    BYTE "maria"
    BYTE "match"
    BYTE "maybe"
    BYTE "mayor"
    BYTE "meant"
    BYTE "medal"
    BYTE "media"
    BYTE "metal"
    BYTE "might"
    BYTE "minor"
    BYTE "minus"
    BYTE "mixed"
    BYTE "model"
    BYTE "money"
    BYTE "month"
    BYTE "moral"
    BYTE "motor"
    BYTE "mount"
    BYTE "mouse"
    BYTE "mouth"
    BYTE "movie"
    BYTE "music"
    BYTE "needs"
    BYTE "never"
    BYTE "newly"
    BYTE "night"
    BYTE "noise"
    BYTE "north"
    BYTE "noted"
    BYTE "novel"
    BYTE "nurse"
    BYTE "occur"
    BYTE "ocean"
    BYTE "offer"
    BYTE "often"
    BYTE "onion"
    BYTE "order"
    BYTE "other"
    BYTE "ought"
    BYTE "paint"
    BYTE "panel"
    BYTE "paper"
    BYTE "party"
    BYTE "peace"
    BYTE "peter"
    BYTE "phase"
    BYTE "phone"
    BYTE "photo"
    BYTE "piece"
    BYTE "pilot"
    BYTE "pitch"
    BYTE "place"
    BYTE "plain"
    BYTE "plane"
    BYTE "plant"
    BYTE "plate"
    BYTE "point"
    BYTE "pound"
    BYTE "power"
    BYTE "press"
    BYTE "price"
    BYTE "pride"
    BYTE "prime"
    BYTE "print"
    BYTE "prior"
    BYTE "prize"
    BYTE "proof"
    BYTE "proud"
    BYTE "prove"
    BYTE "queen"
    BYTE "quick"
    BYTE "quiet"
    BYTE "quite"
    BYTE "radio"
    BYTE "raise"
    BYTE "range"
    BYTE "rapid"
    BYTE "ratio"
    BYTE "reach"
    BYTE "ready"
    BYTE "refer"
    BYTE "right"
    BYTE "rival"
    BYTE "river"
    BYTE "robin"
    BYTE "roger"
    BYTE "roman"
    BYTE "rough"
    BYTE "round"
    BYTE "route"
    BYTE "royal"
    BYTE "rural"
    BYTE "scale"
    BYTE "scene"
    BYTE "scope"
    BYTE "score"
    BYTE "sense"
    BYTE "serve"
    BYTE "seven"
    BYTE "shall"
    BYTE "shape"
    BYTE "share"
    BYTE "sharp"
    BYTE "sheet"
    BYTE "shelf"
    BYTE "shell"
    BYTE "shift"
    BYTE "shirt"
    BYTE "shock"
    BYTE "shoot"
    BYTE "short"
    BYTE "shown"
    BYTE "sight"
    BYTE "since"
    BYTE "sixth"
    BYTE "sixty"
    BYTE "sized"
    BYTE "skill"
    BYTE "sleep"
    BYTE "slide"
    BYTE "small"
    BYTE "smart"
    BYTE "smile"
    BYTE "smith"
    BYTE "smoke"
    BYTE "solid"
    BYTE "solve"
    BYTE "sorry"
    BYTE "sound"
    BYTE "south"
    BYTE "space"
    BYTE "spare"
    BYTE "speak"
    BYTE "speed"
    BYTE "spend"
    BYTE "spent"
    BYTE "split"
    BYTE "spoke"
    BYTE "sport"
    BYTE "staff"
    BYTE "stage"
    BYTE "stake"
    BYTE "stand"
    BYTE "start"
    BYTE "state"
    BYTE "steam"
    BYTE "steel"
    BYTE "stick"
    BYTE "still"
    BYTE "stock"
    BYTE "stone"
    BYTE "stood"
    BYTE "store"
    BYTE "storm"
    BYTE "story"
    BYTE "strip"
    BYTE "stuck"
    BYTE "study"
    BYTE "stuff"
    BYTE "style"
    BYTE "sugar"
    BYTE "suite"
    BYTE "super"
    BYTE "sweet"
    BYTE "table"
    BYTE "taken"
    BYTE "taste"
    BYTE "taxes"
    BYTE "teach"
    BYTE "teeth"
    BYTE "terry"
    BYTE "texas"
    BYTE "thank"
    BYTE "theft"
    BYTE "their"
    BYTE "theme"
    BYTE "there"
    BYTE "these"
    BYTE "thick"
    BYTE "thing"
    BYTE "think"
    BYTE "third"
    BYTE "those"
    BYTE "three"
    BYTE "threw"
    BYTE "throw"
    BYTE "tight"
    BYTE "times"
    BYTE "tired"
    BYTE "title"
    BYTE "today"
    BYTE "topic"
    BYTE "total"
    BYTE "touch"
    BYTE "tough"
    BYTE "tower"
    BYTE "track"
    BYTE "trade"
    BYTE "train"
    BYTE "treat"
    BYTE "trend"
    BYTE "trial"
    BYTE "tried"
    BYTE "tries"
    BYTE "truck"
    BYTE "truly"
    BYTE "trust"
    BYTE "truth"
    BYTE "twice"
    BYTE "under"
    BYTE "undue"
    BYTE "union"
    BYTE "unity"
    BYTE "until"
    BYTE "upper"
    BYTE "upset"
    BYTE "urban"
    BYTE "usage"
    BYTE "usual"
    BYTE "valid"
    BYTE "value"
    BYTE "video"
    BYTE "virus"
    BYTE "visit"
    BYTE "vital"
    BYTE "voice"
    BYTE "waste"
    BYTE "watch"
    BYTE "water"
    BYTE "wheel"
    BYTE "where"
    BYTE "which"
    BYTE "while"
    BYTE "white"
    BYTE "whole"
    BYTE "whose"
    BYTE "woman"
    BYTE "women"
    BYTE "world"
    BYTE "worry"
    BYTE "worse"
    BYTE "worst"
    BYTE "worth"
    BYTE "would"
    BYTE "wound"
    BYTE "write"
    BYTE "wrong"
    BYTE "wrote"
    BYTE "yield"
    BYTE "young"
    BYTE "youth"
    BYTE 0

;Positions on screen
outHandle DWORD ?
cursor_state CONSOLE_CURSOR_INFO <100, TRUE>
bufSize COORD <81,26>
pRect SMALL_RECT <0,0,80,25>
show_wordle COORD <18,3>
show_score COORD <52,3>
show_highest_score COORD <52,2>
show_highest_score0 COORD <18,7>
show_reveal COORD  <28,2>
show_message COORD <6,5>
wordPos COORD <35,11>		
chrsPos COORD <0,0>			

cellsWritten DWORD 0
color WORD 0
allColor WORD 27 DUP(0)		; The color of the letters below

console_title BYTE "Wordle", 0

;String Constants

;at Start Screen
start_print_line1 BYTE "*     *   ***   *****  *****   *      *****", 0 ;<18,1>
start_print_line2 BYTE "*     *  *   *  *   *  *    *  *      *    ", 0 ;<18,2>
start_print_line3 BYTE "*  *  *  *   *  *****  *    *  *      *****", 0 ;<18,3>
start_print_line4 BYTE " ** **   *   *  * *    *    *  *      *    ", 0 ;<18,4>
start_print_line5 BYTE " ** **    ***   *   *  *****   *****  *****", 0 ;<18,5>
start_print_line6 BYTE "Press ENTER to Start",0 ;<30,12>
start_print_line7 BYTE "Tips: You have 6 chance to guess a 5 letter word!", 0 ;<15,20>
start_print_line8 BYTE "Use lowercase letters and press ENTER to input words.",0 ;<13,21>

;Winning Message
winning_message1 BYTE "WHAT A SHOT! 10000PTS!", 0
winning_message2 BYTE " IMPRESSIVE!  5000PTS!", 0
winning_message3 BYTE " INCREDIBLE!  1000PTS!", 0
winning_message4 BYTE "PRETTY GOOD!   500PTS!", 0
winning_message5 BYTE "GOOD ENOUGH!   200PTS!", 0
winning_message6 BYTE "       NICE!   100PTS!", 0

;Score, Print at right upper corner
score_print BYTE " CURRENT SCORE: ", 0
highest_score_print BYTE " HIGHEST SCORE: ", 0

;Invalid Word Warning
message_invalidWord BYTE "WORD NOT IN DICTIONARY", 0

;Reveal the correct answer
revealAns BYTE "Correct Answer is ",0

;MsgBoxAsk questions
question1 BYTE "Would you like to continue? (y/n)", 0
question2 BYTE "Would you like to restart? (y/n)", 0

;a line below
lineColor WORD 15 DUP(08h)
drawaLine BYTE 15 DUP('-')
linePos COORD <32,19>

; KeyBoard
allChrs_for_output BYTE 'QWERT YUIOP',
						'ASDFGHJKL', 
						'ZXCVBNM', 0
allChrs_for_compare BYTE 'qwert yuiop',
						 'asdfghjkl',
						 'zxcvbnm', 0

;Game Constants
guessTimes BYTE 0				; Total guess times in each round
tot_score DWORD 0				; Total scores before lose
highest_score DWORD 0           ; Hightest score
								; Example
answer          BYTE 5 DUP(?),0	; 'THEME'
currentWord     BYTE 5 DUP(?),0	; 'HENCE', Current guess
answerCopy      BYTE 5 DUP(?),0
currentWordCopy BYTE 5 DUP(0),0	
corrPos BYTE 5 DUP(0)			; '00001' The position of correct characters to answer in input
samePos BYTE 5 DUP(0)			; '11000' The position of same characters to answer in input
corrChr BYTE 5 DUP(0)			; 'E'	  The correct characters
sameChr BYTE 5 DUP(0)			; 'HE'	  The same characters
wronChr BYTE 21 DUP(0)			; 'NC'	  The wrong characters
test_valid_count DWORD 0		; Used to examine each word in word list by sequence	
word_valid_invalid BYTE 0		; Values: 0, valid word ; 1, invalid word
correctOrNot BYTE 0				; Values: 0, NOT all correct ; 1, all correct
question_respond_cont DWORD ?	; Respond from show_msg_cont	
question_respond_restart DWORD ?; Respond from show_msg_restart	

.code

;===============================================================================================
word_generate PROC USES eax ebx ecx esi edi
; To generate answer
	mov eax, 475		; total number of words
	;Re-seeds the random number generator
	call Randomize 
	call RandomRange	; range = 0 ~ eax
	mov bl, 5
	mul bl			; eax = eax * 5
	mov ecx, 5		; move times
	mov esi, OFFSET word_lst
	add esi, eax
	mov edi, OFFSET answer
	rep movsb
	ret
word_generate ENDP
;===============================================================================================
check_Valid_Word PROC USES esi edi eax,
	source: PTR BYTE,
	target: PTR BYTE
; To check if the guessing word in word list
test_invalid:
	mov edi, target
	add edi, test_valid_count	; offset
	mov al, [edi]
	cmp al, 0					; The end of word_lst is 0
	je invalid					; equals if guessing word is invalid
	mov esi, source				; source
	mov ecx, 5					; loop counter
	repe cmpsb					; repeat compare while equal
	jne next_word				; find next word
	je return					; valid word
next_word:
	add test_valid_count, 5		; offset to next word in word_lst
	jmp test_invalid			; retest a new word
invalid:						; invalid
	; Set text color
	push eax
	mov eax, white+(black*16)
	call SetTextColor

	mov word_valid_invalid, 1
	; clear the line and set cursor to original pos
	INVOKE SetConsoleCursorPosition, outHandle, wordPos	;game_pos
	mWriteSpace 10
	INVOKE SetConsoleCursorPosition, outHandle, show_message
	mWriteString OFFSET message_invalidWord
	INVOKE sleep, 1000			; wait 1000 mseconds or 1sec											
	INVOKE SetConsoleCursorPosition, outHandle, show_message
	mWriteSpace 22				; clear warning message
	INVOKE SetConsoleCursorPosition, outHandle, wordPos	;game_pos
	pop eax
return:
	ret
check_Valid_Word ENDP
;===============================================================================================
allCorrect PROC	USES esi edi ecx,
	source: PTR BYTE,
	target: PTR BYTE
; To test if the guessing word is answer
	mov esi, source
	mov edi, target
	mov ecx, 5	; compare times
	repe cmpsb
	jne return
allCorr:	; all correct
	mov correctOrNot, 1
return:
	ret
allCorrect ENDP
;===============================================================================================
input PROC USES edi ecx
	push eax
	INVOKE SetConsoleCursorPosition, outHandle, wordPos
input_initial:
	mov ecx, 5					; loop counter
	mov edi, OFFSET currentWord ; edi points to the addr of currentWord
	mov word_valid_invalid, 0	; reset
	mov test_valid_count, 0		; reset offset
inp:
	; Set text color
	mov eax, lightgray+(gray*16)
	call SetTextColor
	; Read a character
	call ReadChar
	; Judge if the char is a backspace
	cmp ax, 0E08h
	je backspace
	; Judge if the char is between a to z
	cmp al, 97
	jb inp
	cmp al, 122
	ja inp
	; move valid char to [edi]
	mov [edi], al
	inc edi
	; Output with upper case char
	sub al, 32
	call WriteChar
	; Set text color
	push eax
	mov eax, white+(black*16)
	call SetTextColor
	pop eax

	mWriteSpace
	add wordPos.x, 2
	jmp inp_end
backspace:
	; already at the leftmost position of a word
	.IF cl == 5h	
		jmp input_initial
	.ENDIF
	; Set text color
	mov eax, white+(black*16)
	call SetTextColor
	sub wordPos.x, 2
	; push&pop ecx because SetConsoleCursorPosition
	; would affect ecx
	push ecx
	INVOKE SetConsoleCursorPosition, outHandle, wordPos
	pop ecx
	; cover the letters on the screen by spaces
	mWriteSpace 2
	push ecx
	INVOKE SetConsoleCursorPosition, outHandle, wordPos
	pop ecx
	dec edi ; go back to previous addr
	inc ecx	; this time counter stay unchanged
	jmp inp
inp_end:
	; dec ecx and jne inp can prevent error "Jump to far"
	dec ecx
	jne inp
ReadChr:
	call ReadChar
	.IF ax == 1C0Dh	; enter
		sub wordPos.x, 10
		; examine if is in word list
		INVOKE check_Valid_Word, ADDR currentWord, ADDR word_lst
		cmp word_valid_invalid, 0
		jne input_initial			; not in word list
	.ELSEIF ax == 0E08h	; backspace
		jmp backspace
	; repeatly read a char until backspace or enter is pressed
	.ELSE	
		jmp ReadChr
	.ENDIF
	pop eax
input_return:
	inc guessTimes	;guessTimes + 1
	ret
input ENDP
;===============================================================================================
Judge PROC USES eax ebx ecx edx esi edi,
; Copy the currentWord to currentWordCopy in order to do the following operation
	mov ecx, 5
	mov esi, OFFSET currentWord
	mov edi, OFFSET currentWordCopy
	rep movsb

; Find the correct characters
	mov ecx, 5							; The length of word
	mov esi, OFFSET answer				; The answer
	mov edi, OFFSET currentWordCopy		; The current word
	mov ebx, OFFSET corrPos				; The correct position in input
FindCorr:
	mov al, [esi]
	cmp al,[edi]						; Compare current word to answer
	jne NotEq
Equal:									; If equal
	mov BYTE PTR [ebx], 1				; Record the position
	INVOKE AddNChr,						; Record the character
		[edi], OFFSET corrChr, 5
	mov BYTE PTR [edi], 0				; Delete the character in current word, prevent judge repeatedly
NotEq:
	inc esi
	inc edi
	inc ebx
	loop FindCorr

; Find the same characters
	mov ecx, 5							; The length of word
	mov esi, OFFSET answer				; The answer
	mov ebx, OFFSET corrPos				; The correct position in current word
FindSame:
	push ecx					
	mov ecx, 5
	mov al, [ebx]
	cmp al, 1							; Check if the correct character have been found or not
	push ebx
	je Next2							; If so, jump to the next character
	mov ebx, OFFSET samePos				; The same position in current word
	mov edi, OFFSET currentWordCopy		; The current word after the opeartion above
	mov al, [esi]
Next1:
	cmp al, [edi]						; Compare current word to answer
	jne NotSame
Same:									; If same
	mov BYTE PTR [ebx], 1				; Record the position
	INVOKE AddNChr,						; Record the character
		[edi], OFFSET sameChr, 5
	mov BYTE PTR [edi], 0				; Delete the character in current word, prevent judge repeatedly
	jmp Next2
NotSame:
	inc edi
	inc ebx
	loop Next1
Next2:
	inc esi
	pop ebx
	pop ecx
	inc ebx
	loop FindSame

; Find the wrong characters
	mov ecx, 5							; The length of word			
	mov edi, OFFSET currentWordCopy		; The current word after the opeartion above
FindWrong:
	mov al, [edi]
	cmp al, 0							; The left characters are wrong (!=0)
	je NotWrong							; The deleted characters are either correct or same
Wrong:									; If wrong
	INVOKE AddNChr,						; Record the character
		[edi], OFFSET wronChr, 21
NotWrong:
	inc edi
	loop FindWrong
	ret
Judge ENDP
;===============================================================================================
AddNChr PROC USES esi ecx,
	nChr: BYTE,
	list: PTR BYTE,
	lLen: BYTE
; Add a new character to list
	mov esi, list						; The list to be added the new character
	movzx ecx, lLen						; The length of the list
CheckExist:								; Check if the new character has been exist or not
	mov al, [esi]
	cmp al, 0							; If the character in list is null, others behind are null, too;
	je NotExist							;	then no valid chacter in front is the same as the new chacter
	cmp al, nChr						; Compare the new character to the character in list
	je Exist							; If equal, there the new character has been exist
	inc esi
	loop CheckExist
NotExist:								; If not exist
	mov al, nChr						; Change the last null character in list to the new character
	mov BYTE PTR [esi], al
Exist:
	ret
AddNChr ENDP
;===============================================================================================
ChangeWordColor PROC USES eax ebx ecx edx esi edi,
	pHandle: DWORD,
	xyPos: COORD
; Print the changed input word color on screen
	mov edx, 0
	mov ecx, 5							; The lenght of word
	INVOKE Str_ucase, OFFSET currentWord
	mov esi, OFFSET currentWord			; The current word
	mov edi, OFFSET corrPos				; The correct position in current word
	mov ebx, OFFSET samePos				; The same position in current word
PrintInp:
	push ecx
	mov al, [edi]
	cmp al, 1							; Check if correct or not
	je CorrWord
	mov al, [ebx]
	cmp al, 1							; Check if same or not
	je SameWord
WrongWord:								; Neither correct nor same
	mov dx, 08h							; Set gray character with black background
	jmp Print
CorrWord:								; If correct
	mov dx, 27h							; Set white character with green background
	jmp Print
SameWord:								; If same
	mov dx, 67h							; Set white character with yellow background
Print:
	mov color, dx
	INVOKE WriteConsoleOutputAttribute,							; Set the color to write
		pHandle, ADDR color, 1, xyPos, OFFSET cellsWritten
	INVOKE WriteConsoleOutputCharacter,							; Write the character
		pHandle, esi, 1, xyPos, OFFSET cellsWritten
	add xyPos.x, 2												; Set the position of next character
	inc esi
	inc edi
	inc ebx
	pop ecx
	loop PrintInp
	ret
ChangeWordColor ENDP
;===============================================================================================
ChangeChrsColor PROC USES eax ebx ecx esi edi,
	pHandle: DWORD,
; Print the changed input word color on screen
	mov ebx, OFFSET allColor						; The color of the character in all letters
	mov esi, OFFSET allChrs_for_compare				; The character in all letters
	mov ecx, LENGTHOF allCHrs_for_compare			; The length of all letters
Chrs:
	push ecx
	mov al, [esi]
	mov ecx, LENGTHOF corrChr
	mov edi, OFFSET corrChr
IFCorrChr:
	cmp al, [edi]						; Check if correct or not
	je PrintCorr
	inc edi
	loop IFCorrChr
	mov ecx, LENGTHOF sameChr
	mov edi, OFFSET sameChr
IFSameChr:
	cmp al, [edi]						; Check if same or not
	je PrintSame
	inc edi
	loop IFSameChr
	mov ecx, LENGTHOF wronChr
	mov edi, OFFSET wronChr
IFWronChr:
	cmp al, [edi]						; Check if wrong or not
	je PrintWron
	inc edi
	loop IFWronChr
PrintnDef:								; Not defined (not correct, same, nor wrong)
	mov dx, 07h							; Set write character with black background
	jmp NextChr
PrintCorr:								; If correct
	mov dx, 02h							; Set green character with black background
	jmp NextChr
PrintSame:								; If same
	mov dx, 06h							; Set yellow character with black background
	jmp NextChr
PrintWron:								; If wrong
	mov dx, 08h							; Set gray character with black background
NextChr:
	mov WORD PTR [ebx], dx				; Record the color
	inc esi
	add ebx, 2
	pop ecx
	loop Chrs
	mov chrsPos.x, 34													; Set the position of first line
	mov chrsPos.y, 20
	INVOKE WriteConsoleOutputAttribute,									; Set the color to write
		pHandle, OFFSET allColor, 11, chrsPos, OFFSET cellsWritten
	INVOKE WriteConsoleOutputCharacter,									; Write the characters
		pHandle, OFFSET allChrs_for_output, 11, chrsPos, OFFSET cellsWritten
	inc chrsPos.x														; Set the position of second line
	inc chrsPos.y
	INVOKE WriteConsoleOutputAttribute,									; Set the color to write
		pHandle, OFFSET allColor+22, 9, chrsPos, OFFSET cellsWritten
	INVOKE WriteConsoleOutputCharacter,									; Write the characters
		pHandle, OFFSET allChrs_for_output+11, 9, chrsPos, OFFSET cellsWritten
	inc chrsPos.x														; Set the position of third line
	inc chrsPos.y
	INVOKE WriteConsoleOutputAttribute,									; Set the color to write
		pHandle, OFFSET allColor+40, 7, chrsPos, OFFSET cellsWritten
	INVOKE WriteConsoleOutputCharacter,									; Write the characters
		pHandle, OFFSET allChrs_for_output+20, 7, chrsPos, OFFSET cellsWritten

	INVOKE WriteConsoleOutputAttribute,							; Set the color to write
		pHandle, ADDR lineColor, 15, linePos, OFFSET cellsWritten
	INVOKE WriteConsoleOutputCharacter,							; Write the character
		pHandle, ADDR drawaLine, 15, linePos, OFFSET cellsWritten

		ret
ChangeChrsColor ENDP
;===============================================================================================
scoreCount PROC USES edx
	INVOKE SetConsoleCursorPosition, outHandle, show_message
;guesstimes should be added in another procedure
	; Set text color
	push eax
	mov eax, green+(black*16)
	call SetTextColor
	pop eax
	; calculate current game points
	.IF guessTimes == 1
		add tot_score, 10000
		mov edx, OFFSET winning_message1
	.ELSEIF guessTimes == 2
		add tot_score, 5000
		mov edx, OFFSET winning_message2
	.ELSEIF guessTimes == 3
		add tot_score, 1000
		mov edx, OFFSET winning_message3
	.ELSEIF guessTimes == 4
		add tot_score, 500
		mov edx, OFFSET winning_message4
	.ELSEIF guessTimes == 5
		add tot_score, 200
		mov edx, OFFSET winning_message5
	.ELSEIF guessTimes == 6
		add tot_score, 100
		mov edx, OFFSET winning_message6
	.ENDIF
		call writeString
	ret
scoreCount ENDP
;===============================================================================================
show_msg_cont PROC USES eax edx
; if wins the game, ask the player whether to play again
	INVOKE SetConsoleCursorPosition, outHandle, show_message
	push eax
	mov eax, white+(black*16)
	call SetTextColor
	pop eax
	mWriteSpace 33
	mWriteString OFFSET question1
ReadChr:
	call ReadChar
	.IF al == 79h
		mov question_respond_cont, 'y'
	.ELSEIF al == 6Eh
		mov question_respond_cont, 'n'
	; repeatly read a char until 'y' or 'n' is pressed
	.ELSE	
		jmp ReadChr
	.ENDIF
	ret
show_msg_cont ENDP
;===============================================================================================
show_msg_restart PROC USES eax edx
; if loses the game, ask the player whether to restart
	INVOKE SetConsoleCursorPosition, outHandle, show_message
	push eax
	mov eax, white+(black*16)
	call SetTextColor
	pop eax
	mWriteSpace 32
	mWriteString OFFSET question2
ReadChr:
	call ReadChar
	.IF al == 79h
		mov question_respond_restart, 'y'
	.ELSEIF al == 6Eh
		mov question_respond_restart, 'n'
	; repeatly read a char until 'y' or 'n' is pressed
	.ELSE
		jmp ReadChr
	.ENDIF
	ret
show_msg_restart ENDP
;===============================================================================================
Reset PROC USES ecx esi,
	list: PTR BYTE, lLen: BYTE

	mov esi, list
	movzx ecx, lLen
; set all places to 0 (null)
ResetTo0:
	mov BYTE PTR [esi], 0
	inc esi
	loop ResetTo0
	ret
Reset ENDP
;===============================================================================================
PrintScores PROC USES eax
; print the scores on right upper

    mov eax, black+(cyan*16)
    call setTextColor

	INVOKE SetConsoleCursorPosition, outHandle, show_score
	mWriteString OFFSET score_print
	mov eax, tot_score
	call WriteDec

    INVOKE SetConsoleCursorPosition, outHandle, show_highest_score
	mWriteString OFFSET highest_score_print
	mov eax, highest_score
	call WriteDec
    ret
PrintScores ENDP
;===============================================================================================
RevealAnswer PROC USES eax esi edi
;reveal the correct answer on screen

    ; change to uppercase letter
    mov ecx, 5
    mov esi, OFFSET answer
    mov edi, OFFSET answerCopy
CopyA:
    mov al, [esi]
    mov [edi], al
    sub BYTE PTR [edi], 20h
    inc esi
    inc edi
    loop CopyA

    ;print
    mov eax, black+(lightgray*16)
    call setTextColor
    INVOKE SetConsoleCursorPosition, outHandle, show_reveal
	mWriteString OFFSET revealAns
    mov eax, magenta+(lightgray*16)
    call setTextColor
	mWriteString OFFSET answerCopy
    ret
RevealAnswer ENDP
;===============================================================================================

;===================================The main procedure==========================================
main PROC
    ; Set the game window
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outHandle, eax
	INVOKE SetConsoleCursorInfo, outHandle, ADDR cursor_state
	INVOKE SetConsoleScreenBufferSize, outHandle, bufSize
	INVOKE SetConsoleWindowInfo, outHandle, TRUE, ADDR pRECT
	INVOKE SetConsoleTitle, ADDR console_title
game_loop:
start_wordle:
    call Clrscr
	mov question_respond_restart, 'U';unknown
	mov tot_score, 0                 ;reset the score

    ;print the big title
	mov eax, white+(green*16)
	call setTextColor
    mov show_wordle.x, 18
    mov show_wordle.y, 1
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line1
	inc show_wordle.y
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line2
	inc show_wordle.y
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line3
	inc show_wordle.y
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line4
	inc show_wordle.y
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line5
    
    ;print the tips
    mov eax, yellow+(black*16)
	call setTextColor
    mov show_wordle.x, 15
    mov show_wordle.y, 19
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line7
    sub show_wordle.x, 2
    add show_wordle.y, 1
    INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteString OFFSET start_print_line8
    
    ;print the highest score
    mov eax, white+(black*16)
    call setTextColor
	INVOKE SetConsoleCursorPosition, outHandle,  show_highest_score0
	mWriteString OFFSET highest_score_print
	mov eax, highest_score
	call WriteDec
   
	mov eax, white+(black*16)
	call setTextColor
	mov show_wordle.x, 30
	mov show_wordle.y, 12
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
start_repeat:	
	call ReadKey
	cmp ax, 1C0Dh                                  ; start if 'ENTER' is pressed
    je Initial
	INVOKE sleep, 1000

    ;print the instruction to start game (repeatedly until start)
	mWriteString OFFSET start_print_line6
	mov show_wordle.x, 30
	INVOKE sleep, 1000
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	mWriteSpace 20
	mov show_wordle.x, 30
	INVOKE SetConsoleCursorPosition, outHandle,  show_wordle
	jmp start_repeat

Initial:
	call Clrscr
    ; reset the states of characters recorded
	INVOKE Reset, OFFSET corrChr, LENGTHOF corrChr
	INVOKE Reset, OFFSET sameChr, LENGTHOF sameChr
	INVOKE Reset, OFFSET wronChr, LENGTHOF wronChr
    INVOKE ChangeChrsColor, outHandle

    ;reset 
    mov correctOrNot, 0
	mov guessTimes, 0
	mov question_respond_cont, 'U';unknown

	call PrintScores
	call word_generate        ; get random answer
	mov wordPos.y, 7
	INVOKE SetConsoleCursorPosition, outHandle, wordPos

continue_playing:
	call input                ; get the current word input
	INVOKE allCorrect, ADDR currentWord, ADDR answer

    ; reset the positions in word
	INVOKE Reset, OFFSET corrPos, LENGTHOF corrPos
	INVOKE Reset, OFFSET samePos, LENGTHOF samePos

	call Judge
	INVOKE ChangeWordColor, outHandle, wordPos
	INVOKE ChangeChrsColor, outHandle

	add wordPos.y, 2            ; next input position
	cmp correctOrNot, 1         ; if all correct
	jne examine_guessTimes
	
    ;all correct
    call scoreCount
	INVOKE sleep, 2000

    ; renew the highest score
    mov eax, tot_score
    cmp eax, highest_score
    jl nothigher
    mov eax, tot_score
    mov highest_score, eax
    call PrintScores
nothigher:
	call show_msg_cont
	cmp question_respond_cont, 'y'      ;next term
	je Initial
	cmp question_respond_cont, 'n'      ;to start screen
	je start_wordle

examine_guessTimes:    
	cmp guessTimes, 6
	jae Not_Found                       ;lose if times bigger than 6
	jb continue_playing

Not_Found:
    call RevealAnswer
    call show_msg_restart

	cmp question_respond_restart, 'y'
	je start_wordle
Close_Screen:
	exit    ;leaving
main ENDP
END main