-- Alex Somerey c7r7 72597099
-- Ancelle Tache r3d8 35967116
--Sample Board Representation for n = 4

--[00][10][20][30]
--  [01][11][21]
--    [02][12]
--  [03][13][23]
--[04][14][24][34]
--

wtest = ["wwww","---","--","---","bbbb"]
btest1 =["-www","w--","--","---","bbbb"]
wnomove = ["----","--b","--","---","w---"]
bnomove = ["---b","---","--","-w-","w---"]

-- Assumes that the player is the one who is moving
oska_c7r7 :: [String]->Char->Int->[String]
oska_c7r7 board player moves = get_1st_possible (move_generator_c7r7 (parse_c7r7 board) player) board

-- TEMPORARY DUMB MOVE GENERATOR
-- takes the first possible move and returns it
get_1st_possible :: [[(Int,Int,Char)]] -> [String] -> [String]
get_1st_possible [] board = board
get_1st_possible possibleboards board = reparse (head possibleboards) 

--Static Board Evaluator
board_eval_c7r7 :: [(Int,Int,Char)]->Char->Int
board_eval_c7r7 board char
	| ((victory_w_c7r7 board) && (victory_b_c7r7 board)) = tie_breaker_c7r7 board
	| (victory_w_c7r7 board) = 1000
	| (victory_b_c7r7 board) = (-1000) 
	| char == 'w' = board_eval_wh_c7r7 board
	| char == 'b' = board_eval_bh_c7r7 board

--Helper Function to Evaluate for White
board_eval_wh_c7r7 :: [(Int,Int,Char)]->Int
board_eval_wh_c7r7 [] = 0
board_eval_wh_c7r7 (x:xs)  = (board_eval_wh2_c7r7 x) + (board_eval_wh_c7r7 xs)
	
board_eval_wh2_c7r7 :: (Int,Int,Char)->Int
board_eval_wh2_c7r7 (_,y,c)
	| c == 'w' = (y+1)
	| otherwise = 0
	
--Helper Function to Evaluate for Black
board_eval_bh_c7r7 :: [(Int,Int,Char)]->Int
board_eval_bh_c7r7 [] = 0
board_eval_bh_c7r7 (x:xs)
	| (not (null xs)) = (board_eval_bh2_c7r7 x (last xs)) + (board_eval_bh_c7r7 xs)
	| otherwise = 0

board_eval_bh2_c7r7 :: (Int,Int,Char)->(Int,Int,Char)->Int
board_eval_bh2_c7r7 (_,y,c) (_,numRows,_)
	| c == 'b' = (-1)*(numRows-y)
	| otherwise = 0

--Implemented Tie-Breaker, Returning +/- 1000 if there is a winner or 0 if it is a tie
tie_breaker_c7r7 :: [(Int,Int,Char)]->Int
tie_breaker_c7r7 board
	|(tie_breaker_h_c7r7 board 0) > 0 = 1000
	|(tie_breaker_h_c7r7 board 0) < 0 = (-1000)
	|otherwise = 0

tie_breaker_h_c7r7 :: [(Int,Int,Char)]->Int->Int
tie_breaker_h_c7r7 [] boardScore = boardScore
tie_breaker_h_c7r7 (x:xs) boardScore = tie_breaker_h_c7r7 xs (boardScore + (tie_breaker_h2_c7r7 x)) 

tie_breaker_h2_c7r7 :: (Int,Int,Char)->Int
tie_breaker_h2_c7r7 (_,_,char)
	| char == 'b' = (-1)
	| char == 'w' = 1
	| otherwise = 0

--Tests if Black has won
victory_b_c7r7 :: [(Int,Int,Char)]->Bool
victory_b_c7r7 board = (victory_bh1_c7r7 board) || (victory_bh2_c7r7 board)

--Tests if all Black pieces are in far end (Black Won)
victory_bh2_c7r7 :: [(Int,Int,Char)]->Bool
victory_bh2_c7r7 [] = True
victory_bh2_c7r7 (x:xs) = ((victory_bh2h_c7r7 x) && (victory_bh2_c7r7 xs))

victory_bh2h_c7r7 :: (Int,Int,Char)->Bool
victory_bh2h_c7r7 (_,y,c)
	| (y /= 0) && (c == 'b') = False
	| otherwise = True

--Tests if no White pieces Remain (Black Won)
victory_bh1_c7r7 :: [(Int,Int,Char)]->Bool
victory_bh1_c7r7 [] = True
victory_bh1_c7r7 (x:xs) = (victory_bh1h_c7r7 x) && (victory_bh1_c7r7 xs)

victory_bh1h_c7r7 :: (Int,Int,Char)->Bool
victory_bh1h_c7r7 (_,_,c)
	| c == 'b' = True
	| c == 'z' = True
	| otherwise = False

--Tests if White has won
victory_w_c7r7 :: [(Int,Int,Char)]->Bool
victory_w_c7r7 board = (victory_wh1_c7r7 board) || (victory_wh2_c7r7 board (last board))

--Tests if all White pieces are in far end (White Won)
victory_wh2_c7r7 :: [(Int,Int,Char)]->(Int,Int,Char)->Bool
victory_wh2_c7r7 [] boardInfo = True
victory_wh2_c7r7 (x:xs) boardInfo = ((victory_wh2h_c7r7 x boardInfo) && (victory_wh2_c7r7 xs boardInfo))

victory_wh2h_c7r7 :: (Int,Int,Char)->(Int,Int,Char)->Bool
victory_wh2h_c7r7 (_,y,c) (_,numRows,_)
	| (y /= (numRows-1)) && (c == 'w') = False
	| otherwise = True

--Tests if no Black pieces Remain (White Won)
victory_wh1_c7r7 :: [(Int,Int,Char)]->Bool
victory_wh1_c7r7 [] = True
victory_wh1_c7r7 (x:xs) = (victory_wh1h_c7r7 x) && (victory_wh1_c7r7 xs)

victory_wh1h_c7r7 :: (Int,Int,Char)->Bool
victory_wh1h_c7r7 (_,_,c)
	| c == 'w' = True
	| c == 'z' = True
	| otherwise = False

--Move Generator
move_generator_c7r7 :: [(Int,Int,Char)]->Char->[[(Int,Int,Char)]]
move_generator_c7r7 board 'w' = move_generator_wh_c7r7 [] board 
move_generator_c7r7 board 'b' = move_generator_bh_c7r7 board

--State shows the board, state is the root of the tree
--Need a function to change the lists into tree form
data MoveTree = MoveTree { state :: [(Int,Int,Char)], moveTree :: [MoveTree] } deriving (Show)

make_move_tree :: [(Int,Int,Char)] -> Char -> Int -> MoveTree
make_move_tree board player moves 
	= (MoveTree board (move_tree_generator (move_generator_c7r7 board player) player (moves-1) []))
	
move_tree_generator :: [[(Int,Int,Char)]] -> Char -> Int -> [MoveTree] -> [MoveTree]
move_tree_generator newstates player moves treearray
	| null newstates		= treearray
	| moves == 0			= move_tree_generator (tail newstates) player moves ((MoveTree (head newstates) []) : treearray)
	| otherwise				= move_tree_generator (tail newstates) player moves ((MoveTree (head newstates) deeperstates) : treearray)
		where deeperstates = (move_tree_generator (move_generator_c7r7 (head newstates) (getopp player)) (getopp player) (moves-1) [])

getopp :: Char -> Char
getopp player
		|player == 'w' 	='b'
		|otherwise 		= 'w'
	
--Helper Function to Generate Moves for White
move_generator_wh_c7r7 :: [(Int,Int,Char)]->[(Int,Int,Char)]->[[(Int,Int,Char)]]
move_generator_wh_c7r7 piecesBefore [] = []
move_generator_wh_c7r7 piecesBefore (x:xs) = filter (not . null) 
			(concat[
			((generate_new_states_wr_c7r7 x piecesBefore xs):
			(generate_new_states_wl_c7r7 x piecesBefore xs):[]),
			(move_generator_wh_c7r7 (piecesBefore++(x:[])) xs)
			])

--Generates New Jumps down and to the right for white pieces
generate_new_states_wr_c7r7 :: (Int,Int,Char)->
				[(Int,Int,Char)]->
				[(Int,Int,Char)]->
				[(Int,Int,Char)]
generate_new_states_wr_c7r7 (x,y,char) piecesBefore piecesAfter
	| char == 'w' && (open_space_c7r7 x (y+1) (piecesBefore++piecesAfter))
	= piecesBefore++(x,(y+1),'w'):piecesAfter
	| char == 'w' && (open_space_c7r7 x (y+1) (piecesBefore++piecesAfter)) 
	&& (jumpable_w_c7r7 x (y+1) (piecesBefore++piecesAfter))
	= piecesBefore++((x+1),(y+2),'w'):piecesAfter
	| otherwise = []

--Generates New Jumps down and to the left for white pieces
generate_new_states_wl_c7r7 :: (Int,Int,Char)->
				[(Int,Int,Char)]->
				[(Int,Int,Char)]->
				[(Int,Int,Char)]
generate_new_states_wl_c7r7 (x,y,char) piecesBefore piecesAfter
	| char == 'w' && (open_space_c7r7 (x-1) (y+1) (piecesBefore++piecesAfter))
	&& (not (below_middle_c7r7 y piecesAfter))
	= piecesBefore++((x-1),(y+1),'w'):piecesAfter
	| char == 'w' && (open_space_c7r7 (x+1) (y+1) (piecesBefore++piecesAfter))
	&& (below_middle_c7r7 y piecesAfter)
	= piecesBefore++((x+1),(y+1),'w'):piecesAfter
	| otherwise = []

--Returns True if the row is below the middle 
--meaning expanding instead of contracting
below_middle_c7r7 :: Int->[(Int,Int,Char)]->Bool
below_middle_c7r7 y x = below_middle_h_c7r7 y (last x)

below_middle_h_c7r7 :: Int->(Int,Int,Char)->Bool
below_middle_h_c7r7 y (_,numRows,_) 
	| y < (div numRows 2) = False
	| otherwise = True

--Tests if a space can be jumped by a white piece
jumpable_w_c7r7 :: Int->Int->[(Int,Int,Char)]->Bool
jumpable_w_c7r7 newx newy [] = True
jumpable_w_c7r7 newx newy (x:xs)
	| xs /= [] = (jumpable_wh_c7r7 newx newy x (last xs)) &&
			(jumpable_w_c7r7 newx newy xs)
	| otherwise = True

jumpable_wh_c7r7 :: Int->Int->(Int,Int,Char)->(Int,Int,Char)->Bool
jumpable_wh_c7r7 newx newy (x,y,char) (_,_,_)
	| (newx == x) && (newy == y) && (char == 'w') = False
	| otherwise = True 

--Tests if a space is open to move into
open_space_c7r7 :: Int->Int->[(Int,Int,Char)]->Bool
open_space_c7r7 newx newy [] = True
open_space_c7r7 newx newy (x:xs)
	| xs /= [] = (open_space_h_c7r7 newx newy x (last xs)) && 
			    (open_space_c7r7 newx newy xs)
	| otherwise = True
	

open_space_h_c7r7 :: Int->Int->(Int,Int,Char)->(Int,Int,Char)->Bool
open_space_h_c7r7 newx newy (x,y,_) (_,numRows,_)
	| newy == numRows = False
	| (newx == (row_size_c7r7 newy numRows)) = False
	| (newx == x) && (newy == y) = False
	| otherwise = True

--Returns the size of a specific row given the total number of rows
row_size_c7r7 :: Int->Int->Int
row_size_c7r7 row numRows
	| row < (div numRows 2) = numRows-(row+1)
	| otherwise = row


--Helper Function to Generate for Black
move_generator_bh_c7r7 :: [(Int,Int,Char)]->[[(Int,Int,Char)]]
move_generator_bh_c7r7 (x:xs) = filter (not . null) (generate_new_states_bh [] x xs  (x:xs) [[]])

generate_new_states_bh :: [(Int,Int,Char)] ->(Int,Int,Char) -> [(Int,Int,Char)] ->[(Int,Int,Char)] -> [[(Int,Int,Char)]] -> [[(Int,Int,Char)]]
generate_new_states_bh passed currentpiece rest board moves
        | null rest                = moves
        | otherwise                = generate_new_states_bh (passed ++ [currentpiece]) (head rest) (tail rest) board 
					((generate_new_states_b_l passed currentpiece rest):((generate_new_states_b_r passed currentpiece rest) : moves))

-- generates a move to the right for a piece if piece is black
-- supports jumps over white
generate_new_states_b_r ::  [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_new_states_b_r  passed (x,y,char) rest
        | y==0 || char /= 'b'      			  = []
		|below_middle_c7r7 (y+1) rest		= generate_b_r_below passed (x,y,char) rest
        | otherwise                         = generate_b_r_above passed (x,y,char) rest
		
 -- Generates move for black below bottleneck
generate_b_r_below :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_b_r_below passed (x,y,char) rest
	| (in_bound (x,(y-1)) (last rest)) && (open_space_c7r7 x (y-1) passed)
		= (passed ++ ((x,(y-1),char) : rest))
	| otherwise		= jumpable_r passed (x,y,char) rest

-- checks if black under bottleneck can jump and returns new board if it can		
jumpable_r :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)]-> [(Int,Int,Char)]
jumpable_r passed (x,y,char) rest
	| y == get_y_after_bottleneck(last rest)
		= jump_r_bottleneck passed (x,y,char) rest
	| (in_bound (x, (y-2)) (last rest))&& (elem (x,(y-1),'w') passed) &&(open_space_c7r7 x (y-2) passed)
		= ((remove1(x,(y-1),'w')[] passed) ++ ((x,(y-2),char) : rest))
	| otherwise	= [] 
	
jump_r_bottleneck :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)]-> [(Int,Int,Char)]
jump_r_bottleneck passed (x,y,char) rest
	|(in_bound ((x+1), (y-2)) (last rest)) && (elem (x,(y-1),'w') passed)&&(open_space_c7r7 (x+1) (y-2) passed)
		= ((remove1(x,(y-1),'w')[] passed) ++ ((x+1),(y-2),char) : rest)
	|otherwise = []
		
-- Generates move for black above bottleneck
generate_b_r_above :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_b_r_above passed (x,y,char) rest
	 | (in_bound ((x+1), (y-1)) (last rest)) && (open_space_c7r7 (x+1) (y-1) passed)
                = passed ++ ((x+1, y-1, char) : rest)
	  | (in_bound ((x+2),(y-2)) (last rest))  && (elem (x+1,(y-1),'w') passed) && (open_space_c7r7 (x+2) (y-2) passed)
                = ((remove1(x+1,(y-1),'w')[] passed) ++ ((x+2),(y-2),char) : rest)
  			
      | otherwise    = []
	  
	  
-- generates a move to the left for a piece if piece is black
-- supports jumps over white
generate_new_states_b_l ::  [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_new_states_b_l  passed (x,y,char) rest
        | y==0 || char /= 'b'      			  = []
		|below_middle_c7r7 (y+1) rest		= generate_b_l_below passed (x,y,char) rest
        | otherwise                         = generate_b_l_above passed (x,y,char) rest
		
        
generate_b_l_below :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_b_l_below passed (x,y,char) rest
	| (in_bound ((x-1),(y-1)) (last rest)) && (open_space_c7r7 (x-1) (y-1) passed)
		= (passed ++ (((x-1),(y-1),char) : rest))
	| otherwise		= jumpable_l passed (x,y,char) rest
	
jumpable_l :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)]-> [(Int,Int,Char)]
jumpable_l passed (x,y,char) rest
	| y == get_y_after_bottleneck(last rest)
		= jump_l_bottleneck passed (x,y,char) rest
	| (in_bound ((x-2), (y-2)) (last rest))&& (elem (x-1,(y-1),'w') passed) &&(open_space_c7r7 (x-2) (y-2) passed)
		= ((remove1(x-1,(y-1),'w')[] passed) ++ (((x-2),(y-2),char) : rest))
	| otherwise	= [] 
	
jump_l_bottleneck :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)]-> [(Int,Int,Char)]
jump_l_bottleneck passed (x,y,char) rest
	|(in_bound ((x-1), (y-2)) (last rest)) && (elem ((x-1),(y-1),'w') passed)&&(open_space_c7r7 (x-1) (y-2) passed)
		= ((remove1(x-1,(y-1),'w')[] passed) ++ ((x-1),(y-2),char) : rest)
	|otherwise = []
	
generate_b_l_above :: [(Int,Int,Char)] -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_b_l_above passed (x,y,char) rest
	 | (in_bound (x, (y-1)) (last rest)) && (open_space_c7r7 x (y-1) passed)
                = passed ++ ((x, y-1, char) : rest)
	  | (in_bound (x,(y-2)) (last rest))  && (elem (x,(y-1),'w') passed) && (open_space_c7r7 x (y-2) passed)
                = ((remove1(x,(y-1),'w')[] passed) ++ ((x,(y-2),char) : rest))
  			
      | otherwise    = []
	
-- Returns the row numb right after smallest row
get_y_after_bottleneck :: (Int,Int,Char) -> Int
get_y_after_bottleneck(x,y,char)
	= (x-1)
	
-- the input piece should always be in rest so rest should never reach [] option
-- removes a piece from board
remove1 :: (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)] ->[(Int,Int,Char)]
remove1 piece        before [] = []
remove1 piece before (x:xs)
        | piece == x        = (before ++ xs)
        | otherwise                = remove1 piece (x:before) xs

in_bound :: (Int,Int) -> (Int,Int,Char) -> Bool
in_bound point (x,y,'Z')         = in_bound_helper point ((x-1),(y-1))
in_bound point unavailable                = False

in_bound_helper :: (Int,Int) -> (Int,Int) -> Bool
in_bound_helper  point (x,y) 
        | x        == 1        = in_bound_helper2 (point) (x,y)
        | otherwise = in_bound_helper1 (point) (x,y)

in_bound_helper1 :: (Int,Int) -> (Int,Int) -> Bool
in_bound_helper1 (x1,y1) (x2,y2)
        |x1 < 0                = False
        |y1 < 0                = False
        |y1== y2         = x1 <= x2
        |otherwise        = in_bound_helper (x1, y1) ((x2-1),(y2-1))
        
in_bound_helper2 :: (Int,Int) -> (Int,Int) -> Bool
in_bound_helper2 (x1,y1) (x2,y2)
        |y2 < 0                = False
        |y1 == y2         = x1 <= x2
        |otherwise        = in_bound_helper2 (x1, y1) ((x2+1),(y2-1))

--Parse Board into Tuples (x-cord,y-cord,char)
--With the final Tuple being (Size of 1st Row, Number of Rows, Z)
parse_c7r7 :: [String]->[(Int,Int,Char)]
parse_c7r7 board = (parse_row_c7r7 board 0)++[((length (head board)),length board,'Z')]

parse_row_c7r7 :: [String]->Int->[(Int,Int,Char)]
parse_row_c7r7 [] y = []
parse_row_c7r7 (x:xs) y = (parse_column_c7r7 x y)++(parse_row_c7r7 xs (y+1))

parse_column_c7r7 :: String->Int->[(Int,Int,Char)]
parse_column_c7r7 row y = parse_column_h_c7r7 row 0 y

parse_column_h_c7r7 :: String->Int->Int->[(Int,Int,Char)]
parse_column_h_c7r7 [] x y = []
parse_column_h_c7r7 (l:ls) x y
	| l /= '-' = (x,y,l):(parse_column_h_c7r7 ls (x+1) y)
	| otherwise = parse_column_h_c7r7 ls (x+1) y

-- Turns Tuples of board back into [String] Format
reparse :: [(Int,Int,Char)] -> [String]
reparse newboard 
	= reparse_h0 newboard [] (get_bound (last newboard))


reparse_h0 :: [(Int,Int,Char)] -> [String] -> (Int, Int) -> [String]
reparse_h0 newboard list (x,y)	
	| y < 0		= list
	| otherwise	=  reparse_h1 newboard list (x,y)
	
reparse_h1 :: [(Int,Int,Char)] -> [String] -> (Int, Int) -> [String]
reparse_h1  newboard list (x,y) 
	| x == 1	= reparse_h2 newboard list (x,y) 
	| otherwise = reparse_h1 newboard ((reparse_row newboard x y []) : list) (x-1, y-1)

reparse_h2 :: [(Int,Int,Char)] -> [String] -> (Int, Int) -> [String]
reparse_h2 newboard list (x,y) 
	| y < 0		= list
	| otherwise	=  reparse_h2 newboard ((reparse_row newboard x y []) : list) (x+1, y-1)

	
reparse_row :: [(Int,Int,Char)] -> Int-> Int -> String ->String
reparse_row newboard x y row
	| x <0		=row
	| (elem (x, y, 'w') newboard) = reparse_row newboard (x-1) y ('w' : row)
	| (elem (x, y, 'b') newboard) = reparse_row newboard (x-1) y ('b' : row)
	| otherwise			= reparse_row newboard (x-1) y ('-' : row)


get_bound :: (Int,Int,Char) -> (Int,Int)
get_bound (x,y,'Z')	= (x-1, y-1)
get_bound invalid 	= (-1,-1)

