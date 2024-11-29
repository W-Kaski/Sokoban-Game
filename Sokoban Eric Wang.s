# Sokoban
# @author Eric Wang

# Enhancements :
# (1) cheat code: We offer an unlimited number of undo and reset for player.
# The detail of this part is in the MoveBack method and Reset method.

# (2) multiple targets and boxes (at most 3 targets and 3 boxes): There are 3 difficulty of the game, 1,2 or 3. Each difficulty can create corresponding number of targets and boxes.
# The detail of this part is in the PlaceInnerBoxTargetAndCharacter method

# (3) Different inner wall for different difficulty: The walls of Difficulty 2 are cross. The walls of Difficulty 3 are striped. The walls will be different size when you have different size of grid.
# The detail of this part is in the PlaceInnerWallDifficulty2 method and PlaceInnerWallDifficulty3 method.

# Citation :
# The algorithm used to implement randomness (In the end of the code (LCG and InitLCG method)):
# W. E. Thomson and A. Rotenberg. 1958. LCG. linear congruential generator: it is an algorithm that yields a sequence of pseudo-randomized numbers calculated with a discontinuous piecewise linear equation. Retrieved from https://en.wikipedia.org/wiki/Linear_congruential_generator.

# The numbers corresponding to items.
# target+box 5
# character: 4
# box:       3
# target:    2
# wall:	     1
# empty:     0


.data
# The rows and cols of grid
gridsize:   .byte 0,0
# The position of current character
currPos:    .byte 0,0
# The numbers of steps we current used
steps:      .byte 0
# The boxes that beside the outer wall (At most 3)
invalidBoxes:.byte 0,0,0,0,0,0
# How many boxes that are invalid
invalidBoxesIndex: .byte 0
.align     4
# The seed of LCG Algorithm
seed:       .word 0
# The length of the grid (cols times rows)
grid:	    .word 0
.align     4

# display of different items
characterDis:  .string "! "
boxDis:        .string "@ "
targetDis:     .string "O "
wallDis:		.string "X "
emptyDis:		.string "  "
# The followings are questions that the game need to ask player.
askMove:    .string "\nPlease press valid button\nw: move up\na: move left\ns: move down\nd: move right\nb: roll back to previous step\nr: reset the game\n"
askRow:     .string "Enter How many rows do you want (Enter integer between 6 and 127, inclusively)\n"
askCol:     .string "Enter How many cols do you want (Enter integer between 6 and 127, inclusively)\n"
askDifficulty: .string "Enter difficulty of the game! (Please press 1, 2 or 3)\n"
invalidSize: .string "Please enter valid rows or cols (Enter integer between 6 and 127, inclusively)\n"
gameStart:   .string "Valid size, Game Start!!!\n"
curSpotPrint: .string "\nCurrent spot is "
theFirstStep: .string "\nThis is the first step!!!!\n"
gameOver: .string "\nGame Over, You use "
gameSteps: .string "\nNow, you've used "
usedSteps: .string " steps\n"
askPlayAgain: .string "\nDo you want to play again? \nPress 1 starts a new game\nPress any other will end the game\n"
# space and new line
space:  .string " "
newLine:	.string "\n"

.text
.global _start

Sokoban:
# The main method of the game
_start:
    # initial sp and gp
    li sp, 0x80000000
    li gp, 0x10000000
    # initial LCG Algorithm (since we need the first seed)
    jal InitLCG
    
    # go to play method
	jal Play

    # After game, ask whether play again
    li a7,4
    la a0,askPlayAgain
    ecall 

    li a7,5
    ecall

    li t0,1
    beq t0, a0,_start
    
    # if not, exit.
    li a7,10 	# system call code for exit
	ecall 	# terminate program	
# This is the play method, we play game use this method.
Play:
    addi sp, sp, -4
    sw ra,0(sp)
	jal AskSize
    jal GenerateGrid

    PlayLoop:
        jal PrintGrid
        jal CharacterMove
        # if game is not over, go to play loop again 
        jal IsGameOver
        beq zero,a0,PlayLoop
		
	# print the final board
	jal PrintGrid
    
    la a7,4
    la a0,gameOver
    ecall

    la a7,1
    lb a0,steps
    ecall

    la a7,4
    la a0,usedSteps
    ecall

    lw ra,0(sp)
    addi sp, sp, 4
    ret
    
# The methods BELOW deal with MOVE character-------------------------------------------------------------
# This method will ask players for the move of character.
CharacterMove:
    addi sp, sp, -4
    sw ra,0(sp)

    AskMove:
        la a7,4
        la a0,askMove
        ecall 
		
		la a7, 12
		ecall
		
        li t0,97 # char a
        beq a0,t0,IfPressA
        li t0,115 # char s
        beq a0,t0,IfPressS
        li t0,100 # char d
        beq a0,t0,IfPressD
        li t0,119 # char w
        beq a0,t0,IfPressW
        li t0,98 # char b
        beq a0,t0,IfPressB
        li t0,114 # char r
        beq a0,t0,IfPressR

        j AskMove

    IfPressA:
        li a0,0
        li a1,-1
        jal ValidMove

        j CharacterMoveEnd
    IfPressS:
        li a0,1
        li a1,0
        jal ValidMove

        j CharacterMoveEnd
    IfPressD:
        li a0,0
        li a1,1
        jal ValidMove

        j CharacterMoveEnd
    IfPressW:
        li a0,-1
        li a1,0
        jal ValidMove

        j CharacterMoveEnd
    IfPressB:
        jal MoveBack

        j CharacterMoveEnd
    IfPressR:
        jal Reset

        j CharacterMoveEnd
    
    CharacterMoveEnd:
        # load return address and reset sp.
        lw ra,0(sp)
        addi sp, sp, 4
        ret
# This method determine if this move is valid. 
ValidMove:
    addi sp, sp, -20
    sw ra,0(sp)
    sw a0,4(sp) # dir row
    sw a1,8(sp) # dir col

    la t0,currPos
    lb t1,0(t0) # row
    lb t2,1(t0) # col

    add a0,t1,a0 # next row
    add a1,t2,a1 # next col

    sw a0,12(sp) # next row
    sw a1,16(sp) # next col

    jal GetItem

    li t0,1 # wall
    beq a0,t0,NextIfWall
    li t0,3 # box
    beq a0,t0,NextIfBox
    li t0,5 # box and target
    beq a0,t0,NextIfBox

    Default:
        la t0,currPos
        lb a0, 0(t0) # rows
        lb a1, 1(t0) # cols
        lw a2, 12(sp) # next row
        lw a3,16(sp) # next col

        jal MoveNext
            
        lw ra,0(sp)
        addi sp, sp, 20
        ret
    # if next is a wall
    NextIfWall:
        lw ra,0(sp)
        addi sp, sp, 20
        ret
    # if next is a box
    NextIfBox: 
        lw t0,4(sp) # dirR
        lw t1,8(sp) # dirC
        lw t2,12(sp) # nextRow
        lw t3,16(sp) # nextCol

        add a0,t0,t2 # nextRow + dirR
        add a1,t1,t3 # nextCol + dirC
        sw a0, 4(sp) # nextRow + dirR 
        sw a1,8(sp) # nextCol + dirC

        jal GetItem

        li t0,1 # wall
        beq t0,a0,NextIfWall
        li t0,3 # box
        beq t0,a0,NextIfWall

        la t0,currPos
        lb a0, 0(t0) # rows
        lb a1, 1(t0) # cols
        lw a2, 12(sp) # next row
        lw a3,16(sp) # next col

        jal MoveNext

        lw a0,12(sp) # nextRow
        lw a1,16(sp) # nextCol
        jal GetItem

        li t0, 5
        bne a0,t0,NotTargetAndBox

        lw a0,12(sp) # nextRow
        lw a1,16(sp) # nextCol
        li a2, 2 # target
        jal SetItems
        # if next is not target and box
        NotTargetAndBox:
        lw a0,4(sp) # nextRow + dirR
        lw a1,8(sp) # nextCol + dirC
        jal GetItem

        li t5, 2 # target
        bne a0, t5, BoxElse
        lw a0,4(sp)
        lw a1,8(sp)
        li a2,5 # box + target   
        jal SetItems 

        lw ra,0(sp)
        addi sp, sp, 20
        ret
        
        BoxElse:
        lw a0,4(sp) # nextRow + dirR
        lw a1,8(sp) # nextCol + dirC
        li a2,3 # box
        jal SetItems
            
        lw ra,0(sp)
        addi sp, sp, 20
        ret
# This method will make character move to the next spot.
MoveNext:
    addi sp, sp, -20
    sw ra,0(sp)
    sw a0,4(sp) # row
    sw a1,8(sp) # col
    sw a2,12(sp) # next Row
    sw a3,16(sp) # next col
	
	jal CopyGrid
    
    la t1,steps
    lb t0, 0(t1)
    addi t0,t0,1
    sb t0, 0(t1) # store steps

    lw a0,12(sp) # next Row
    lw a1,16(sp) # next col
    jal GetItem

    li t0, 2 # Target
    beq t0,a0, IfCurrentTarget # if current spot is target.
    li t0, 5 # TargetAndBox
    beq t0,a0, IfCurrentTarget

    lw a0,12(sp) # next Row
    lw a1,16(sp) # next col
    li a2,4
    jal SetItems

    IfCurrentTarget:
        lw a0,4(sp) # Row
        lw a1,8(sp) # col
        jal GetItem

        li t0, 2 # Target
        beq a0,t0, MoveNextEnd
        
        lw a0,4(sp) # Row
        lw a1,8(sp) # col
        li a2, 0 # empty
        jal SetItems

    MoveNextEnd:
        # change current spot
        la t0, currPos
        lw t1,12(sp) # next Row
        lw t2,16(sp) # next col
        sb t1,0(t0) # store current row
        sb t2,1(t0) # store current col

    lw ra, 0(sp)
    addi sp,sp,20
    ret 
# The game board will go back one step by using this method. 
MoveBack:
    addi sp, sp, -4
    sw ra,0(sp)
    
    lb t2,steps
    beq t2, zero,MoveBackElse # if steps is 0
	
    addi t0,t2,-1
	la t2,steps
    sb t0,0(t2) # store steps back

    lw t1,grid
    sub gp,gp,t1
    # you need to reset currspot since you change the board.
    jal FindCur

    lw ra,0(sp)
    addi sp, sp, 4
    ret

    # if it is the first step
    MoveBackElse:
        la a7,4
        la a0,theFirstStep # print this is the first step.
        ecall

        lw ra,0(sp)
        addi sp, sp, 4
        ret
# The game board will reset by using this method. 
Reset:
    addi sp, sp, -4
    sw ra,0(sp)

    lb t0,steps # get current steps
    lw t1,grid # the length of the grid
    mul t0,t0,t1
    sub gp,gp,t0 # set gp to initial

    la t1, steps
    sb zero,0(t1) # set current steps to 0

    jal FindCur

    lw ra,0(sp)
    addi sp, sp, 4
    ret
# This method will help us to find current character's spot.
FindCur:
    addi sp, sp, -12
    sw ra,0(sp)

    li t1,0
    sw t1,4(sp) # Outter counter
    sw t1,8(sp) # Inner counter
    # iterate all items in the current board. 
    FindCurLoop:
        # get item in the grid
        lw a0,4(sp)
        lw a1,8(sp)
        jal GetItem

        li t0,4 # character
        bne a0,t0,FindCurLoopEnd # if not character

        lw t0,4(sp)
        lw t1,8(sp)

        # store current spot
        la t2,currPos
        sb t0,0(t2) 
        sb t1,1(t2) 

        lw ra,0(sp)
        addi sp, sp, 12
        ret
       
    FindCurLoopEnd:
    la t0,gridsize
    lb t1,0(t0) # rows

    lw t4,8(sp) # Inner counter

    addi t4,t4,1 # Inner counter add 1
	sw t4,8(sp)
    blt t4,t1,FindCurLoop

    sw zero,8(sp) # reset Inner counter
	
    lw t3,4(sp) # Outter counter
	lb t2,1(t0) # cols
	
    addi t3,t3,1 # Outter counter add 1
	sw t3,4(sp)
    blt t3,t2,FindCurLoop

    lw ra,0(sp)
    addi sp, sp, 12
    ret
# This method determine whether the game is over.
IsGameOver:
    addi sp,sp,-4
    sw ra,0(sp)
    li t0,0
    lw t1, grid

    IsGameOverFor:
        add t2,t0,gp
        li t3,2 # target
        lb t2,0(t2) # get the value of current spot
        beq t2, t3,IfTarget
		
        addi t0,t0,1
        blt t0,t1,IsGameOverFor

    # don't find target
    lw ra,0(sp) 
	addi sp,sp,4
    la a0,1 # game is over.
    ret

    IfTarget:
        
        lw ra,0(sp)
		addi sp,sp,4
        la a0, 0 # if there is a target, the game is not over.
        ret 
# This method will copy current board to next board.
CopyGrid:
    addi sp,sp,-4
    li t0,0 # counter
    lw t1, grid # length of grid
    sw ra,0(sp)
    
    CopyGridFor:
        add t2, t0, gp
        lb t2,0(t2) # original value

        add t3,t1,gp # add the length of the grid
        add t3,t3,t0 
        sb t2,0(t3) # new address

        addi t0,t0,1
        blt t0,t1,CopyGridFor
    
    add gp,gp,t1 # move gp to the new address
	
	lw ra,0(sp)
	addi sp,sp,4
    ret
    
# The methods BELOW deal with INITIAL grid-------------------------------------------------------------
# This method will ask the rows and cols of the game board.
AskSize:
    li a7, 4 
	la a0, askRow 	
	ecall 	# print the string

    li a7, 5
    ecall

    mv t0, a0
    li t2, 6
    blt t0, t2, InvalidSize
	
    li a7, 4 
	la a0, askCol 	
	ecall 	# print the string

    li a7, 5
    ecall

    mv t1, a0
    li t2, 6
    blt t1, t2, InvalidSize
    li t2, 128
    bge t1, t2, InvalidSize
    

    la t2, gridsize
    sb t0,0(t2)
    sb t1,1(t2)

    mul t1,t0,t1
    la t2, grid
    sw t1, 0(t2) # save the grid string size

    li a7, 4 
	la a0, gameStart 	
	ecall 
    ret
	
    InvalidSize:
        li a7, 4 
        la a0, invalidSize 	
        ecall 
        j AskSize
	
# This method will generate the board of the game
GenerateGrid:
	addi sp,sp,-4
	sw ra, 0(sp)
    
	la t0,steps
	sb zero,0(t0)
    jal ClearUp # clear the memory for grid.    
    # add outer wall of the board.
    AddOuterWall:
        li t0, 0 # t0 is counter

        la t2, gridsize
        lb t3, 0(t2) # t3 is rows
        lb t4, 1(t2) # t4 is cols
        AddRows:
            addi sp,sp, -12
            sw t0,0(sp)
            sw t3,4(sp)
            sw t4,8(sp)

            mv a0, t0
            li a1, 0
            li a2, 1
            jal SetItems

            lw t0,0(sp)
            lw t3,4(sp)
            lw t4,8(sp)

            addi t1,t4, -1
            mv a1, t1
            jal SetItems

            
            lw t0,0(sp)
            lw t3,4(sp)
            lw t4,8(sp)
            addi sp,sp, 12
			
            addi t0,t0,1
            blt t0,t3,AddRows

        li t0,0
        AddCols:
            addi sp,sp, -12
            sw t0,0(sp)
            sw t3,4(sp)
            sw t4,8(sp)

            li a0, 0
            mv a1, t0
            li a2, 1
            jal SetItems

            lw t0,0(sp)
            lw t3,4(sp)
            lw t4,8(sp)

            addi t1,t3, -1
            mv a0, t1
            jal SetItems
            
            lw t0,0(sp)
            lw t3,4(sp)
            lw t4,8(sp)
            addi sp,sp, 12
			
            addi t0,t0,1
            blt t0,t4,AddCols
    # ask difficulty of the game
    AskDiff:
        la a7,4
        la a0, askDifficulty
        ecall

        la a7,5
        ecall

        li t0, 1
        beq t0, a0, CorrectDiff1

        li t0,2
        beq t0, a0, CorrectDiff2

        li t0,3
        beq t0, a0, CorrectDiff3
        j AskDiff
    # The following are three different difficulties.
    CorrectDiff1:
        jal PlaceInnerBoxTargetAndCharacter
        j GenerateGridEnd
    
    CorrectDiff2:
       	jal PlaceInnerWallDifficulty2
	   	li a0,2
        jal PlaceInnerBoxTargetAndCharacter
        j GenerateGridEnd

    CorrectDiff3:
        jal PlaceInnerWallDifficulty3
		li a0,3
        jal PlaceInnerBoxTargetAndCharacter
        j GenerateGridEnd

    GenerateGridEnd:
    lw ra,0(sp)
	addi sp,sp,4
    ret


# This method will place box, target and character randomly.
PlaceInnerBoxTargetAndCharacter:
    addi sp, sp, -12
    li t0,0 # counter

    sw ra,0(sp) # return address
    sw a0,4(sp) # difficulty
    sw t0,8(sp) # counter

    PlaceBoxLoopStart:
    li a0, 3 # box
    jal PlaceRandomItems

    mv t0, a0 # row
    mv t1, a1 # col

    li t2,1
    beq t2, t0, InvalidBox # if the row of box is 1

    beq t2, t1, InvalidBox # if the col of box is 1

    la t2, gridsize
    lb t2, 0(t2)
    addi t2,t2,-2
    beq t2,t0, InvalidBox # if the row of box is size[0] - 2

    la t2, gridsize
    lb t2, 1(t2)
    addi t2,t2,-2
    beq t2,t1, InvalidBox # if the col of box is size[1] - 2

    j PlaceBoxLoopEnd
    # If current box is next to the outer wall
    InvalidBox:
        lb t3, invalidBoxesIndex # index
        la t2, invalidBoxes # address
        add t4, t3, t2 
        sb t0,0(t4) # save row

        addi t3,t3,1

        add t4,t3,t2
        sb t1,0(t4) # save col

        addi t3, t3,1

		la t5, invalidBoxesIndex
        sb t3,0(t5)

    PlaceBoxLoopEnd:
        lw t0, 4(sp) # difficulty
        lw t1, 8(sp) # counter
        addi t1,t1,1
        sw t1, 8(sp) # save counter
        bne t0,t1,PlaceBoxLoopStart
    
    li t0, 0
    sw t0, 8(sp) # reset counter

    PlaceTargetLoopStart:
        li a0, 2 # Target
        jal PlaceRandomItems

        lw t0, 4(sp) # difficulty
        lw t1, 8(sp) # counter
        addi t1,t1,1
        sw t1, 8(sp)
        bne t0,t1,PlaceTargetLoopStart
    
    li a0, 4 # Character
    jal PlaceRandomItems

    la t1, currPos
    sb a0,0(t1)
    sb a1,1(t1)

    lw ra, 0(sp)
    addi sp,sp,12
    ret
# This method determine if the initial of items is valid.
ValidInit:
    addi sp, sp, -28
    sw ra,0(sp)
    sw a0,4(sp) # row
    sw a1,8(sp) # col
    sw a2,12(sp) # item 
    li t1,-1
    sw t1,16(sp) # outter counter
    sw t1,20(sp) # inner counter
    li t1,0
    sw t1,24(sp) # the counter of number of wall 

    jal GetItem

    mv t3, a0 
    beq t3, zero,ValidBox # if item is empty

    lw ra,0(sp)
    addi sp, sp, 28
    li a0, 0 # return false
    ret 

    ValidBox: # determine if a valid box
        li t3,3 # box
        lw t2,12(sp) # item
        bne t2,t3,ValidTarget # item != 3 (box)
        
        InnerLoop:
            lw t0,16(sp) # outter counter
            lw t1,20(sp) # inner counter
            beq t0,zero,OuterZero  
            beq t1,zero,InnerZero

            lw t2,4(sp) # row
            lw t3,8(sp) # col

            add a0,t2,t0
            add a1,t3,t1
            jal GetItem

            mv t1, a0
            li t2, 3 # box
            bne t1, t2, InnerLoopEnd # item != 3 (box)

            lw ra,0(sp) 
            addi sp, sp, 28
            li a0,0  # return false
            ret 

            # if outer counter is zero
            OuterZero:
                beq t1,zero,InnerLoopEnd # both 0, skip
                lw t1,4(sp) # row
                lw t2,8(sp) # col

                lw t3,16(sp) # outter counter
                lw t4,20(sp) # inner counter

                add a0,t1,t3 
                add a1,t2,t4
                jal GetItem 

                mv t1, a0
                li t2, 3 #box
                bne t1, t2, IfWall # if current item is not a box 
                
                lw ra,0(sp) 
                addi sp, sp, 28
                li a0,0 # return false if its a box
                ret
                
            # if inner counter is zero
            InnerZero:
                lw t1,4(sp) # row
                lw t2,8(sp) # col

                lw t3,16(sp) # outter counter
                lw t4,20(sp) # inner counter

                add a0,t1,t3
                add a1,t2,t4
                jal GetItem

                mv t1, a0
                li t2, 3 # box
                bne t1, t2,IfWall # if current item is not a box 

                lw ra,0(sp)
                addi sp, sp, 28
                li a0,0  # return false
                ret
            # if it is a wall
            IfWall:
                li t2, 1 # wall
                bne t1, t2, InnerLoopEnd # if current item is not a wall 
                lw t0, 24(sp)
                addi t0,t0,1
                sw t0, 24(sp)

        InnerLoopEnd:
            lw t0,20(sp) # inner counter
            addi t0,t0,1
            sw t0,20(sp)
            li t3,1
            ble t0,t3,InnerLoop

            li t0,-1
            sw t0,20(sp)

            lw t1,16(sp) # outter counter
            addi t1,t1,1
            sw t1,16(sp)
            li t3,1
            ble t1,t3,InnerLoop

        lw t0, 24(sp)
        li t1, 2
        blt t0,t1, LT2

        li a0,0
        lw ra,0(sp)
        addi sp, sp, 28
        ret

        LT2:
        li a0, 1    
        lw ra,0(sp)
        addi sp, sp, 28
        ret

    ValidTarget:
        li t3,2 # target
        lw t2,12(sp) 
        bne t2, t3,ValidEnd # if item != target

        lw t0,4(sp) # row
        lw t1,8(sp) # col
        la t2,invalidBoxesIndex
        lb t2, 0(t2) # the invalid boxes index

        beq t2, zero,ValidEnd # if index is 0

        addi t3, t2, -1
        addi t4, t2 ,-2

        la t5, invalidBoxes
        add t3,t3,t5 # the col of invalid box
        add t4,t4,t5 # the row of invalid box

        lb t3, 0(t3)# the col of invalid box
        lb t4, 0(t4)# the row of invalid box

        li t5,1
        beq t4, t5, InvalidTargetRow # if BoxRow == 1

        la t5, gridsize
        lb t5, 0(t5)
        addi t5,t5,-2
        beq t5,t4, InvalidTargetRow # if BoxRow == size[0] - 2

        la t5, gridsize
        lb t5, 1(t5)
        addi t5,t5,-2
        beq t5,t3, InvalidTargetCol # if BoxCol == size[1] - 2

        la t5, 1
        beq t5,t3, InvalidTargetCol #if BoxCol == 1 

        j ValidEnd

        InvalidTargetRow:
            beq t4,t0,HasValidTarget # if boxRow == row

            lw ra,0(sp)
            addi sp,sp,28
			li a0,0
            ret 

        InvalidTargetCol:
            beq t3,t1,HasValidTarget # if boxCol == col

            lw ra,0(sp)
            addi sp,sp,28
			li a0,0
            ret 
        # if target is next to current wall that the box next to, it is a valid target.
        HasValidTarget:
            lb t0, invalidBoxesIndex
            addi t0,t0,-2
			la t1, invalidBoxesIndex
            sb t0, 0(t1)

            lw ra,0(sp)
            addi sp, sp, 28
			li a0,1
            ret 

    ValidEnd:
        lw ra,0(sp)
        addi sp, sp, 28
        li a0,1
        ret 
# This method will help us place random items in the board
PlaceRandomItems: 
    li t0, 0
    addi sp, sp, -16
    sw ra,0(sp) # return address
    sw a0,4(sp) # item
    sw t0,8(sp) # col
    sw t0,12(sp) # row

    RandomWhlie:
        la t0,gridsize
        lb a0,0(t0) # size of grid row
        jal LCG
        sw a0,8(sp)

        la t0,gridsize
        lb a0,1(t0) # size of grid col
        jal LCG
        sw a0,12(sp)
        
        lw a0,8(sp)
        lw a1,12(sp)
        lw a2,4(sp)

        jal ValidInit # determine if it is a valid initial

        beq a0, zero, RandomWhlie # not valid, then go to while again.

        lw a0,8(sp) # row
        lw a1,12(sp) # col
        lw a2,4(sp)
        jal SetItems

        lw a0,8(sp) # row
        lw a1,12(sp) # col
        lw ra,0(sp)
        addi sp, sp, 16
        ret
# If the difficulty is 2, this method will place the inner wall.
PlaceInnerWallDifficulty2:
    addi sp,sp,-16
    sw ra,0(sp)
    la t0,gridsize
    lb t1,0(t0) # rows of grid
    lb t2,1(t0) # cols of grid

    srli t1,t1,1 # rows/2
    srli t2,t2,1 # col/2
    sw t1,4(sp)
    sw t2,8(sp)

    li t3, 3 # index
    sw t3,12(sp)

    IWD2ColFor:
        lw t2,12(sp) # index

        la t0,gridsize
        lb t1,0(t0) # rows of grid
        addi t1,t1,-3
        bge t2,t1,IWD2ColEnd

        lw a0,12(sp) # index
        lw a1,8(sp) # cols/2
        li a2,1
        jal SetItems
		
		lw t2,12(sp) # index
        addi t2,t2,1
        sw t2,12(sp)

        j IWD2ColFor
    IWD2ColEnd:
    
    li t0,3
    sw t0,12(sp) #reset pointer

    IWD2RowFor: 
       	lw t2,12(sp) # index

        la t0,gridsize
        lb t1,1(t0) # clos of grid
        addi t1,t1,-3
        bge t2,t1,IWD2RowEnd

        lw a0,4(sp) # rows/2
        lw a1,12(sp) # index
        li a2,1
		jal SetItems
		
		lw t2,12(sp) # index
        addi t2,t2,1
        sw t2,12(sp)

        j IWD2RowFor
    IWD2RowEnd:

    lw ra,0(sp)
    addi sp,sp,16
    ret

# If the difficulty is 3, this method will place the inner wall.
PlaceInnerWallDifficulty3:
    addi sp,sp,-12
    sw ra,0(sp)
    la t0,gridsize
    lb t1,0(t0) # rows of grid
    lb t2,1(t0) # cols of grid

    li t3, 3 # index
    sw t3,4(sp) # outer index
    sw t3,8(sp) # inner indx

    IWD3:
        lw t2,8(sp) # index

        la t0,gridsize
        lb t1,0(t0) # rows of grid
        addi t1,t1,-3
        bge t2,t1,IWD3OutterLoop
    
        lw a0,8(sp) # outer index
        lw a1,4(sp) # inner indx
        li a2,1
        jal SetItems
		
		lw t2,8(sp) # index
        addi t2,t2,1
        sw t2,8(sp)
		j IWD3
	IWD3OutterLoop:
		li t0,3
        sw t0,8(sp) #reset pointer

        lw t2,4(sp) # index
        addi t2,t2,2
        sw t2,4(sp)

        la t0,gridsize
        lb t1,1(t0) # cols of grid
        addi t1,t1,-3
        blt t2,t1,IWD3

    IWD3End:
    lw ra,0(sp)
    addi sp,sp,12
    ret
# this method will clear up all the items in the board.
ClearUp:
	addi sp,sp,-4
	sw ra, 0(sp)
    la t0, grid
    lw t0, 0(t0)
    li t1, 0 # counter

    clear:
        add t2, gp, t1
        li t3, 0
        sb t3,0(t2)

        addi t1,t1,1
        blt t1,t0,clear
		
	lw ra,0(sp)
	addi sp,sp,4
    ret
# This method will ask row and col of item and return the item in that spot
GetItem:
    mv t0, a0 # row
    mv t1, a1 # col

    la t3, gridsize
    lb t4, 1(t3) # t4 is cols
    mul t0,t0,t4
    add t0,t0,t1 # row * size[1] + col

    add t1, gp, t0
    lb t2,0(t1)
    mv a0,t2
    ret
# This method will ask row, col and item, then place item in that spot.
SetItems:
    mv t0, a0 # row
    mv t1, a1 # col
    mv t2, a2 # item

    la t3, gridsize
    lb t4, 1(t3) # t4 is cols
    mul t0,t0,t4
    add t0,t0,t1 # row * size[1] + col
    
    add t1, gp, t0 # get the address of current point.
    sb t2, 0(t1)
    ret
# This method will print the item corresponding to the numbers.
PrintGrid:
	addi sp,sp,-4
	sw ra, 0(sp)
	
    PrintCurPos:
        li a7, 4 
        la a0, curSpotPrint 	
        ecall 	# print the string

        la t0, currPos
        lb t1, 0(t0)
        lb t2, 1(t0)

        li a7, 1 
        mv a0, t1
        ecall

        li a7, 4 
        la a0, space 	
        ecall 	# print the string

        li a7, 1
        mv a0, t2
        ecall

        li a7, 4 
        la a0, newLine 	
        ecall 

    la t0,grid
    lw t1, 0(t0) # t1 is the length of grid
    li t2, 0 #t2 is counter
    li t4, 0 # t4 is counter
    PrintDis:
        If:
			la t3, gridsize
			lb t3,1(t3)
            bne t4, t3, Else

            li a7, 4 
            la a0, newLine
            ecall
            li t4, 0
        Else:
        add t3,t2,gp
        lb t0,0(t3)

        li t3, 1    # wall
        beq t0, t3,PrintWall
        li t3, 2    # target
        beq t0, t3,PrintTarget
        li t3, 3    # box
        beq t0, t3,PrintBox
        li t3, 4    # character
        beq t0, t3,PrintCharacter
        li t3, 5    # box and target
        beq t0, t3,PrintBox
        PrintEmpty:
            li a7, 4 
            la a0,emptyDis
            ecall
            j End
        PrintWall:
            li a7, 4 
            la a0,wallDis
            ecall
            j End
        PrintBox:
            li a7, 4 
            la a0,boxDis
            ecall
            j End
        PrintTarget:
            li a7, 4 
            la a0,targetDis
            ecall
            j End
        PrintCharacter:
            li a7, 4 
            la a0,characterDis
            ecall
            j End

        # this is the end of print grid.
        End:        
        addi t2, t2,1    
        addi t4,t4,1    
        blt t2, t1,PrintDis
    # print the steps that player used.
    la a7,4
    la a0,gameSteps
    ecall

    la a7,1
    lb a0,steps
    ecall

    la a7,4
    la a0,usedSteps
    ecall
	
    li a7, 4 
    la a0, newLine 	
    ecall 
	
	lw ra,0(sp)
	addi sp,sp,4
    ret
# This method generate the first seed of LCG.
InitLCG:
    li a7, 30 # time syscall (returns milliseconds)
    ecall  
	la t1,seed
    sw a0,0(t1)
    ret	
# This method will return: A number from 0 (inclusive) to the number in parameter. (exclusive)
# The reference is in the top of the code.
LCG:	
    mv t0, a0
    li t1, 262139 # modulus m
    li t2,92717 # multiplier a
    li t3,166972 # increment c
	lw t4, seed
	
    mul t2,t2,t4
    add t2,t2,t3
    remu t2,t2,t1 # (a * seed + c) % m
	
	la t1,seed
    sw t2,0(t1)
    remu a0,t2,t0 # mod the number in parameter.
    ret