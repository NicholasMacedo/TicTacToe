! ======================================= !
! A PROGRAM TO PLAY TIC-TAC-TOE           !
! Written By: Michael Worth               !
! Edited By: Nicholas Macedo - Van Horne  !
! ======================================= !
PROGRAM TICTACTOE
      
    character (len=1), dimension(3,3) :: gameArray
    character (len=1) :: winner
    logical :: isFinished = .FALSE.
    logical :: isSpotTaken
    integer :: playerChoice, gameTurn

    write(*,*) " Welcome to TIC-TAC-TOE written in Fortran!"
    write(*,*) " Enter a value (1-9) to select a box."
    write(*,*) " "
    write(*,*) "             +---+---+---+"
    write(*,*) "             | 1 | 2 | 3 |"
    write(*,*) "             +---+---+---+"
    write(*,*) "             | 4 | 5 | 6 |"
    write(*,*) "             +---+---+---+"
    write(*,*) "             | 7 | 8 | 9 |"
    write(*,*) "             +---+---+---+"
    write(*,*) " "
    write(*,*) " To win, get 3 in a row, column or diagonally!"
    write(*,*) " Good Luck! "
    write(*,*) " "
           
    call gameBoardSetUp(gameArray) ! Initializing the board.
    write(*,*) "The Game Board:"
    call drawBoard(gameArray)      ! Drawing the board to start off.
    call playerMove(gameArray)     ! Process first player turn.
    gameTurn = 0
      
    do while (.NOT. isFinished)
        if (gameTurn .EQ. 0) then
            write(*,*) "Game Board After Player's Move: "
            call drawBoard(gameArray)
            call isGameFinished(gameArray,isFinished,winner)
            if (isFinished) then
                exit
            end if
            gameTurn = 1
            call computerTurn(gameArray)
        else
            write(*,*) "Game Board After Computer's Move:"
            call drawBoard(gameArray)
            call isGameFinished(gameArray,isFinished,winner)
            if (isFinished) then
                exit
            end if
            gameTurn = 0
            call playerMove(gameArray)
        end if
    end do
      
    write(*,*) "The game is over!"
    if (winner .EQ. "D") then
        write(*,*) "The game ended in a draw. "
    else
        write(*,*) "The winner is: ", winner
    end if
    stop
end

! ============================================== !
! Subroutine: isGameFinished                     !
! In: Array to check, answer, winner if one.     !
! Out: Answer of true if done false if not.      !
! Used to check if the game is over or not.      !
! ============================================== !
    subroutine isGameFinished(gameArray,isFinished,winner)
        character (len=1), intent(in), dimension(3,3) :: gameArray
        character (len=1), intent(out) :: winner
        logical, intent(out) :: isFinished
        character (len=1), parameter :: draw = 'D'
        character (len=1), parameter :: blank = ' '

        logical :: areSame 
        logical :: diagSame
        integer :: row, column

        ! ASSUME GAME IS OVER AT START
        isFinished = .TRUE.

        ! CHECK FOR A WINNER
        ! CHECK ROWS FOR A WINNER
        do row = 1, 3
            if (areSame(gameArray(row,1),gameArray(row,2),gameArray(row,3))) then
                winner = gameArray(row,1)
                return
            end if
        end do
        ! NO WINNER BY ROWS, CHECK COLUMNS FOR A WINNER
        do column = 1, 3
            if (areSame(gameArray(1,column),gameArray(2,column),gameArray(3,column))) then
               winner = gameArray(1,column)
               return
            end if
        end do 
    ! NO WINNER BY ROWS OR COLUMNS, CHECK DIAGONALS
        diagSame = areSame(gameArray(1,1),gameArray(2,2),gameArray(3,3)) .OR. areSame(gameArray(1,3),gameArray(2,2),gameArray(3,1)) 
            if (diagSame) then
                winner = gameArray(2,2)
                return
            end if
    ! NO WINNER AT ALL. SEE if GAME IS A DRAW
    ! CHECK EACH ROW FOR AN EMPTY SPACE
        do row = 1,3
            do column = 1,3
                if (gameArray(row,column) .EQ. blank) then
                    isFinished = .FALSE.
                    return
                end if
            end do
        end do

    ! NO BLANK FOUND, GAME IS A DRAW
        winner = draw
        return    
    end subroutine isGameFinished

! ============================================== !
! Subroutine: computerTurn                       !
! In: Game array to be changed.                  !
! Out: Game array with added computer move.      !
! Used to process the player move. Gets input,   !
!  processes it and adds it to array.            !
! ============================================== !
    subroutine computerTurn(gameArray)
        character (len=1), dimension(3,3), intent(inout) :: gameArray
        integer paths(3,8), pathSum(8)
        data paths/1,2,3,4,5,6,7,8,9, + 1,4,7,2,5,8,3,6,9, + 1,5,9,3,5,7/
        integer board(9,2), K, X, Y, randPos
        data board/1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3/

        ! CALCULATE THE PATHSUMS
        do I = 1,8
            pathSum(I) = 0
            do J = 1,3
                X = board(paths(J,I),1)
                Y = board(paths(J,I),2)
                if (gameArray(X,Y) .EQ. " ") then
                    K = 0
                else if (gameArray(X,Y) .EQ. "X") then
                    K = 1
                else if (gameArray(X,Y) .EQ. "O") then
                    K = 4
                end if
                pathSum(I) = pathSum(I) + K     
            end do
        end do 

        ! OFFENSIVE CODE TO DEAL WITH SCENARIOS WHERE THE
        ! COMPUTER HAS TWO IN A PATH
        do I = 1,8
            if (pathSum(I) .EQ. 8) then
                do J = 1,3
                    X = board(paths(J,I),1)
                    Y = board(paths(J,I),2)
                    if (gameArray(X,Y) .EQ. " ") then
                        gameArray(X,Y) = "O"
                        return
                    end if
                end do
            end if
        end do
  
        ! DEFENSIVE CODE TO DEAL WITH SCENARIOS WHERE THE
        ! OPPONENT HAS TWO IN A PATH
        do I = 1,8
            if (pathSum(I) .EQ. 2) then
                do J = 1,3
                    X = board(paths(J,I),1)
                    Y = board(paths(J,I),2)
                    if (gameArray(X,Y) .EQ. " ") then
                        gameArray(X,Y) = "O"
                        return
                    end if
                end do
            end if
        end do
  
        do I = 1,10
            randPos = int(rand(0)*9)+1
            X = board(randPos,1)
            Y = board(randPos,2)
            if (gameArray(X,Y) .EQ. " ") then
                gameArray(X,Y) = "O"
                return
            end if
        end do
  
        return    
    end subroutine computerTurn

! ============================================== !
! Function: areSame                              !
! In: The array locations in question.           !
! Out: True if same, false if not.               !
! Used to check if the array locations passed in !
!  have the same value. X, O ,or empty.          !
! ============================================== !
    logical function areSame(T1,T2,T3)
        character (len=1), intent(in) :: T1,T2,T3

        if (T1 .EQ. "X" .AND. T2 .EQ. "X" .AND. T3 .EQ. "X") then
            areSame = .TRUE.      
        else if (T1 .EQ. "O" .AND. T2 .EQ. "O" .AND. T3 .EQ. "O") then
            areSame = .TRUE.
        else
            areSame = .FALSE.
        end if    
        return  
    end function areSame
  
! ============================================== !
! Subroutine: gameBoardSetUp                     !
! In: Array to initialize as blank for the game. !
! Out: Array full of spaces for the board.       !
! Used to initialize the array before game..     !
! ============================================== ! 
    subroutine gameBoardSetUp(gameArray)
        character (len=1), dimension(3,3), intent(inout) :: gameArray
        do I = 1,3
            do J = 1,3
                gameArray(I,J) = " "
            end do
        end do
        return
    end subroutine gameBoardSetUp

! ============================================== !
! Subroutine: playerMove                         !
! In: game array to be changed.                  !
! Out: game array with added player move.        !
! Used to process the player move. Gets input,   !
!  processes it and adds choice to array.        !
! ============================================== !
    subroutine playerMove(gameArray)
        character (len=1), dimension(3,3), intent(inout) :: gameArray
        integer :: playerChoice
        logical :: isSpotTaken

        playerChoice = 100
        do while (playerChoice < 1 .OR. playerChoice > 9)  ! Loops while player input is invalid.
            write(*,*) "Please enter your move. (Empty spot from 1 - 9) "
            read(*,*) playerChoice
            if (playerChoice < 1 .OR. playerChoice > 9) then
                write(*,*) "Invalid input. Please enter a value from 1-9."
            else if (isSpotTaken(gameArray,playerChoice)) then   ! If he value is valid, checks if spot is empty.
                write(*,*) "Invalid move, box already occupied."
                playerChoice = 100
            end if
        end do

        if (playerChoice == 1) then  !Places an X in the spot chosen by player.
            gameArray(1,1) = "X"
        end if

        if (playerChoice == 2) then
            gameArray(1,2) = "X"
        end if

        if (playerChoice == 3) then
            gameArray(1,3) = "X"
        end if

        if (playerChoice == 4) then
            gameArray(2,1) = "X"
        end if

        if (playerChoice == 5) then
            gameArray(2,2) = "X"
        end if

        if (playerChoice == 6) then
            gameArray(2,3) = "X"
        end if

        if (playerChoice == 7) then
            gameArray(3,1) = "X"
        end if

        if (playerChoice == 8) then
            gameArray(3,2) = "X"
        end if

        if (playerChoice == 9) then
            gameArray(3,3) = "X"
        end if
        return
    end subroutine playerMove

! ============================================== !
! Function: isSpotTaken                          !
! In: Array for spot checking & choice for spot. !
! Out: True if taken, False if empty.            !
! Used to check is the spot chosen is empty.     !
! ============================================== !
    logical function isSpotTaken(gameArray,playerChoice) 
        character (len=1), dimension(3,3), intent(in) :: gameArray
        integer, intent(in) :: playerChoice
                
        select case (playerChoice)   ! Case that switches based on the player chosen value
        case (1)                     ! To see if empty.
            if (gameArray(1,1) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (2)
            if (gameArray(1,2) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (3)
            if (gameArray(1,3) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (4)
            if (gameArray(2,1) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (5)
            if (gameArray(2,2) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (6)
            if (gameArray(2,3) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (7)
            if (gameArray(3,1) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (8)
            if (gameArray(3,2) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case (9)
            if (gameArray(3,3) .EQ. " ") then
                isSpotTaken = .FALSE.
            else
                isSpotTaken = .TRUE.
            end if

        case default
            write(*,*) 'Terminal Error.' ! This case should NEVER happen.
        end select

        return
    end function isSpotTaken

! ============================================= !
! Subroutine: drawBoard (gameArray to print)    !
! In: Array to draw to the screen.              !
! Out: Nothing, prints to the screen.           !
! Used to draw the board in its current state.  !
! ============================================= !
    subroutine drawBoard(gameArray)
        character (len=1), dimension(3,3), intent(in) :: gameArray
        write(*,*) " "
        write(*,*) "         +---+---+---+"
        do I=1,3
            write(*,400) (gameArray(I,J), J=1,3)
            400 format("          |",1X,A1,1X,"|",1X,A1,1X,"|",1X,A1,1X,"|")
            write(*,*) "         +---+---+---+"
        end do
        write(*,*) " "
        return
    end subroutine drawBoard
