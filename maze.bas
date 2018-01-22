CONST North = "N"
CONST East = "E"
CONST West = "W"
CONST South = "S"
CONST LEFT_TURN = "L"
CONST RIGHT_TURN = "R"
CONST QUIT = "Q"
CONST MAN = "M"
CONST AUTO = "A"

'Yes/No
CONST YES = "yes"
CONST NO = "no"
'Location type
TYPE Location
    row AS INTEGER
    column AS INTEGER
    visited AS INTEGER
END TYPE
'Maze_Block type
TYPE Maze_Block
    maze_element AS STRING * 1
    loc AS Location
END TYPE
'Maze_Runner type
TYPE Maze_Runner
    face AS STRING * 1
    direction AS STRING * 1
    loc AS Location
END TYPE

'Shared
'This will be expanded (REDIM)
'$DYNAMIC

DIM SHARED Maze_Blocks(0) AS Maze_Block
DIM SHARED sz%
DIM SHARED maze_end AS Location
DIM SHARED maze_start AS Location
DIM SHARED r AS Maze_Runner
DIM SHARED e_no%

e_no% = 0

'Nonshared
DIM m_a$
DIM turn$
DIM i%
DIM clr%
DIM spd%


ON ERROR GOTO handler
'set the maze blocks
Set_Maze_Blocks
'smiley face for runner
r.face = CHR$(1)
'start location for runner
r.loc = maze_start

'auto or manual mode
PRINT ("THIS PROGRAM WILL MAKE A MAZE RUNNER RUN THROUGH A MAZE.")
PRINT
DO
    INPUT "WOULD YOU LIKE TO EXECUTE IN M(ANUAL) OR A(UTO)"; m_a$
    m_a$ = UCASE$(MID$(m_a$, 1, 1))
LOOP UNTIL m_a$ = MAN OR m_a$ = AUTO

'if auto then choose algorithm and speed
IF m_a$ = AUTO THEN
    PRINT
    PRINT ("YOU COULD CHOOSE TO FORCE THE MAZE RUNNER TO USE EITHER LEFT OR RIGHT TURNS")
    PRINT
    DO
        INPUT "L(EFT) OR R(IGHT)"; turn$
        turn$ = UCASE$(MID$(turn$, 1, 1))
    LOOP UNTIL turn$ = LEFT_TURN OR turn$ = RIGHT_TURN
    PRINT
    PRINT "CHOOSE ONE OF THE FOLLOWING SPEEDS FOR THE MAZE RUNNER"
    PRINT
    PRINT "FAST = 1"
    PRINT "NORMAL = 2"
    PRINT "SLOW = 3"
    PRINT
    DO
        INPUT "CHOOSE SPEED ", spd%
    LOOP WHILE spd% < 1 OR spd% > 3
END IF

'choose color
PRINT "SELECT A COLOR FOR THE MAZE AND THE MAZE RUNNER FROM THE OPTIONS BELOW"
PRINT
COLOR 4: PRINT "RED = 1 "
COLOR 6: PRINT "ORANGE = 2"
COLOR 14: PRINT "YELLOW = 3"
COLOR 2: PRINT "GREEN = 4"
COLOR 1: PRINT "BLUE = 5"
COLOR 5: PRINT "PURPLE = 6"
COLOR 7: PRINT "WHITE = 7"
PRINT
DO
    INPUT "SELECT A COLOR ", clr%
LOOP WHILE clr% < 1 OR clr% > 7
PRINT

SCREEN 13
SELECT CASE clr%
    CASE 1
        COLOR 4:
    CASE 2
        COLOR 6:
    CASE 3
        COLOR 14:
    CASE 4
        COLOR 2:
    CASE 5
        COLOR 1:
    CASE 6
        COLOR 5:
    CASE 7
        COLOR 7:
END SELECT

Draw_Grid
LOCATE r.loc.row, r.loc.column: PRINT r.face
IF m_a$ = AUTO THEN
    found$ = Traverse_Maze$(r, turn$, spd%)
ELSE
    found$ = Manual$(r)
END IF
END

handler:
IF ERR > 0 THEN
    e_no% = ERR
    RESUME NEXT
END IF
SUB Draw_Grid
    FOR i% = 0 TO sz% - 1
        LOCATE Maze_Blocks(i%).loc.row, Maze_Blocks(i%).loc.column
        IF Maze_Blocks(i%).maze_element = "1" THEN
            PRINT CHR$(176);
        ELSEIF Maze_Blocks(i%).maze_element = "X" THEN
            PRINT CHR$(164)
        ELSE
            PRINT " ";
        END IF
    NEXT
END SUB
SUB Set_Maze_Blocks
    DIM counter%
    DIM rows%
    rows% = 0
    DIM l$ 'a string
    OPEN "maze.txt" FOR INPUT AS #1 ' Open the file for INPUT
    IF e_no% = 0 THEN
        DO WHILE NOT EOF(1)
            LINE INPUT #1, l$
            rows% = rows% + 1
        LOOP
        'PRINT rows%
        'PRINT LEN(l$)
        sz% = rows% * LEN(l$)
        REDIM Maze_Blocks(sz%) AS Maze_Block

    ELSE
        PRINT "ERROR OCCURRED"
    END IF
    CLOSE #1

    OPEN "maze.txt" FOR INPUT AS #1 ' Open the file for INPUT
    IF e_no% = 0 THEN
        counter% = 0
        rows% = 1
        DO WHILE NOT EOF(1)
            LINE INPUT #1, l$
            FOR column% = 0 TO LEN(l$) - 1
                Maze_Blocks(counter%).maze_element = (MID$(l$, column% + 1, 1))
                Maze_Blocks(counter%).loc.row = rows%
                Maze_Blocks(counter%).loc.column = column% + 1
                IF Maze_Blocks(counter%).maze_element = "X" THEN
                    maze_end.row = Maze_Blocks(counter%).loc.row
                    maze_end.column = Maze_Blocks(counter%).loc.column
                END IF
                IF Maze_Blocks(counter%).maze_element = "N" OR Maze_Blocks(counter%).maze_element = "S" OR Maze_Blocks(counter%).maze_element = "W" OR Maze_Blocks(counter%).maze_element = "E" THEN
                    maze_start.row = Maze_Blocks(counter%).loc.row
                    maze_start.column = Maze_Blocks(counter%).loc.column
                    r.direction = Maze_Blocks(counter%).maze_element
                END IF
                counter% = counter% + 1
            NEXT column%
            rows% = rows% + 1
        LOOP
        'PRINT counter%

    ELSE
        PRINT "ERROR OCCURRED"
    END IF
    CLOSE #1
END SUB

FUNCTION Dead_End_Ahead$ (l AS Location, direction$)
    STATIC loc_ahead AS Location
    STATIC maze_block_ahead AS Maze_Block
    'PRINT "direction"; direction%
    'PRINT "current: ("; l.row; ","; l.column; ")"
    SELECT CASE direction$
        CASE North
            loc_ahead.row = l.row - 1
            loc_ahead.column = l.column
        CASE East
            loc_ahead.row = l.row
            loc_ahead.column = l.column + 1
        CASE West
            loc_ahead.row = l.row
            loc_ahead.column = l.column - 1
        CASE South
            loc_ahead.row = l.row + 1
            loc_ahead.column = l.column

    END SELECT
    'PRINT "ahead: ("; loc_ahead.row; ","; loc_ahead.column; ")"
    FOR x% = 0 TO sz% - 1 STEP 1
        maze_block_ahead = Maze_Blocks(x%)
        IF maze_block_ahead.loc.row = loc_ahead.row AND maze_block_ahead.loc.column = loc_ahead.column THEN
            IF maze_block_ahead.maze_element = "1" OR maze_block_ahead.loc.visited = 1 THEN
                Dead_End_Ahead$ = YES
            ELSE
                Dead_End_Ahead$ = NO
            END IF
        END IF
    NEXT x%
END FUNCTION


FUNCTION Exit_Reached$ (l AS Location)
    IF l.row = maze_end.row AND l.column = maze_end.column THEN
        Exit_Reached$ = YES
    ELSE
        Exit_Reached$ = NO
    END IF
END FUNCTION

SUB Do_Turn (r AS Maze_Runner, turn$)
    SELECT CASE turn$
        CASE LEFT_TURN
            SELECT CASE r.direction
                CASE North
                    r.direction = West
                CASE East
                    r.direction = North
                CASE West
                    r.direction = South
                CASE South
                    r.direction = East
            END SELECT
        CASE RIGHT_TURN
            SELECT CASE r.direction
                CASE North
                    r.direction = East
                CASE East
                    r.direction = South
                CASE West
                    r.direction = North
                CASE South
                    r.direction = West
            END SELECT
    END SELECT
END SUB

SUB Undo_Turn (r AS Maze_Runner, turn$)
    IF turn$ = LEFT_TURN THEN
        Do_Turn r, RIGHT_TURN
    ELSE
        Do_Turn r, LEFT_TURN
    END IF
END SUB

SUB Move (r AS Maze_Runner)
    SELECT CASE r.direction
        CASE North
            r.loc.row = r.loc.row - 1
        CASE East
            r.loc.column = r.loc.column + 1
        CASE West
            r.loc.column = r.loc.column - 1
        CASE South
            r.loc.row = r.loc.row + 1
    END SELECT
    'PRINT r.loc.row; ","; r.loc.column
END SUB

SUB Move_Back (r AS Maze_Runner)
    SELECT CASE r.direction
        CASE North
            r.loc.row = r.loc.row + 1
        CASE East
            r.loc.column = r.loc.column - 1
        CASE West
            r.loc.column = r.loc.column + 1
        CASE South
            r.loc.row = r.loc.row - 1
    END SELECT
END SUB


SUB Set_Visted (Maze_Blocks() AS Maze_Block, l AS Location)
    FOR x% = 0 TO sz% - 1 STEP 1
        IF l.row = Maze_Blocks(x%).loc.row AND l.column = Maze_Blocks(x%).loc.column THEN
            Maze_Blocks(x%).loc.visited = l.visited
            'PRINT Maze_Blocks(x%).loc.row; ","; Maze_Blocks(x%).loc.column
            GOTO return_from_here
        END IF
    NEXT
    return_from_here:
END SUB

FUNCTION Traverse_Maze$ (r AS Maze_Runner, turn$, spd%)
    DIM x%
    DIM found$
    SELECT CASE spd%
        CASE 1
            _DELAY .03
        CASE 2
            _DELAY .08
        CASE 3
            _DELAY .3
    END SELECT

    ' if not dead end
    IF Dead_End_Ahead$(r.loc, r.direction) = NO THEN
        'moves runner and marks location visited
        Move r
        r.loc.visited = 1
        'maze_block also contains location
        'set location visited
        Set_Visted Maze_Blocks(), r.loc
    END IF

    found$ = Exit_Reached$(r.loc)
    'print the character with locate
    LOCATE r.loc.row, r.loc.column: PRINT r.face
    IF found$ = YES THEN
        Move_Back r
    ELSE
        Do_Turn r, turn$
        FOR x% = 1 TO 3 STEP 1
            IF found$ = NO AND Dead_End_Ahead$(r.loc, r.direction) = NO THEN
                found$ = Traverse_Maze$(r, turn$, spd%)
            END IF
            Undo_Turn r, turn$
        NEXT x%
        Move r
        Do_Turn r, turn$
        Do_Turn r, turn$

    END IF
    Traverse_Maze$ = found$
END FUNCTION

FUNCTION Manual$ (r AS Maze_Runner)
    DIM move_key$

    'DO
    '    DO
    '        LOCATE 24, 30: INPUT move_key$
    '        move_key$ = UCASE$(MID$(move_key$, 1, 1))
    '    LOOP UNTIL move_key$ = "W" OR move_key$ = "S" OR move_key$ = "A" OR move_key$ = "D"
    '    CLS
    '    Draw_Grid
    '    SELECT CASE move_key$
    '        CASE "W"
    '            r.direction = North
    '        CASE "S"
    '            r.direction = South
    '        CASE "A"
    '            r.direction = West
    '        CASE "D"
    '            r.direction = East
    '    END SELECT
    '    IF Dead_End_Ahead$(r.loc, r.direction) = NO THEN
    '        'moves runner and marks location visited
    '        Move r
    '    END IF
    '    LOCATE r.loc.row, r.loc.column: PRINT r.face

    'LOOP UNTIL Exit_Reached$(r.loc) = YES


    DO
        'INKEY$ function returns user input as ASCII STRING character(s) from the keyboard buffer.
        move_key$ = (INKEY$)
        IF move_key$ = CHR$(0) + CHR$(72) THEN 'up arrow key
            r.direction = North
        ELSEIF move_key$ = CHR$(0) + CHR$(80) THEN 'down arrow key
            r.direction = South
        ELSEIF move_key$ = CHR$(0) + CHR$(75) THEN 'left arrow key
            r.direction = West
        ELSEIF move_key$ = CHR$(0) + CHR$(77) THEN 'right arrow key
            r.direction = East
        ELSE
            GOTO skip
        END IF
        IF Dead_End_Ahead$(r.loc, r.direction) = NO THEN
            'moves runner and marks location visited
            Move r
        END IF
        LOCATE r.loc.row, r.loc.column: PRINT r.face
        skip:
    LOOP UNTIL move_key$ = QUIT OR Exit_Reached$(r.loc) = YES

    Manual$ = Exit_Reached$(r.loc)
END FUNCTION
