-- Alex Somerey c7r7
-- Ancelle Tache r3d8 35967116
--Sample Board Representation for n = 4

--[00][10][20][30]
--  [01][11][21]
--    [02][12]
--  [03][13][23]
--[04][14][24][34]
--

oska_c7r7 :: [String]->Char->Int->[String]
oska_c7r7 board player moves = []


--Static Board Evaluator
--TODO
board_eval_c7r7 :: [String]->Char->Int
board_eval_c7r7 board 'w' = board_eval_wh_c7r7 board
board_eval_c7r7 board 'b' = board_eval_bh_c7r7 board

--Helper Function to Evaluate for White
--TODO
board_eval_wh_c7r7 :: [String]->Int
board_eval_wh_c7r7 board = 0

--Helper Function to Evaluate for Black
--TODO
board_eval_bh_c7r7 :: [String]->Int
board_eval_bh_c7r7 board = 0

--Move Generator
--TODO
move_generator_c7r7 :: [(Int,Int,Char)]->Char->[[(Int,Int,Char)]]
move_generator_c7r7 board 'w' = move_generator_wh_c7r7 [] board 
move_generator_c7r7 board 'b' = move_generator_bh_c7r7 board

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
move_generator_bh_c7r7 (x:xs) = generate_new_states_bh x xs  (x:xs) [[]]
--move_generator_bh_c7r7 before  (x:xs) board = (generate_new_states_b_r_c7r7 x before (xs) board) :
--										(move_generator_bh_c7r7 (before++(x:[])) xs board)

generate_new_states_bh :: (Int,Int,Char) -> [(Int,Int,Char)] ->[(Int,Int,Char)] -> [[(Int,Int,Char)]] -> [[(Int,Int,Char)]]
generate_new_states_bh currentpiece	rest board moves
	| null rest		= moves
	| otherwise		= generate_new_states_bh (head rest) (tail rest) board ((generate_new_states_b_l currentpiece board):((generate_new_states_b_r currentpiece board) : moves))

-- generates a move to the right for a piece if piece is black
-- supports jumps over white
generate_new_states_b_r :: (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_new_states_b_r (x,y,char) board
	| y==0 || char == 'w'	= []
	| (in_bound (x, (y-1)) board) && open_space_c7r7 x (y-1) board
		= (x, y-1, char) : (remove1 (x,y,char) [] board)
	| (in_bound ((x+1),(y-2)) board)  && (elem (x,(y-1),'w') board) && (open_space_c7r7 (x+1) (y-2) board)
		= (x+1, (y-2), char) : (remove2 (x,y,char) (x,(y-1),'w') [] board)
	| otherwise 			= []
	
-- generates a move to the left for a piece if piece is black
-- supports jumps over white
generate_new_states_b_l :: (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)]
generate_new_states_b_l (x,y,char) board
	| y == 0 || char == 'w'		= []
	| (in_bound ((x-1), (y-1)) board) && open_space_c7r7 (x-1) (y-1) board
		= ((x-1), (y-1), char) : (remove1 (x,y,char) [] board)
	| (in_bound ((x-1),(y-2)) board)  && (elem ((x-1),(y-1),'w') board) && (open_space_c7r7 (x-1) (y-2) board)
		= (x-1, (y-2), char) : (remove2 (x,y,char) ((x-1),(y-1),'w') [] board)
		
	|otherwise = []

-- the input piece should always be in rest so rest should never reach [] option
-- removes a piece from board
remove1 :: (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)] ->[(Int,Int,Char)]
remove1 piece	before [] = []
remove1 piece before (x:xs)
	| piece == x	= (before ++ xs)
	| otherwise		= remove1 piece (x:before) xs

-- this should always reach the empty list for rest
-- by the time it reaches empty list, 2 pieces will have been removed 
remove2 :: (Int,Int,Char) -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int,Char)] ->[(Int,Int,Char)]
remove2 piece1 piece2 before [] = before
remove2 piece1 piece2 before (x:xs)	
	| piece1 == x 	|| piece2 ==x 	= (before ++ xs)
	| otherwise						= remove2 piece1 piece2 (x:before) xs
	

in_bound :: (Int,Int) -> [(Int,Int,Char)] -> Bool
in_bound point [(x,y,'Z')] 	= in_bound_helper point ((x-1),(y-1))
in_bound point (x:xs) 		= in_bound point xs

in_bound_helper :: (Int,Int) -> (Int,Int) -> Bool
in_bound_helper  point (x,y) 
	| x	== 1	= in_bound_helper2 (point) (x,y)
	| otherwise = in_bound_helper1 (point) (x,y)

in_bound_helper1 :: (Int,Int) -> (Int,Int) -> Bool
in_bound_helper1 (x1,y1) (x2,y2)
	|x1 < 0		= False
	|y1 < 0		= False
	|y1== y2 	= x1 <= x2
	|otherwise	= in_bound_helper (x1, y1) ((x2-1),(y2-1))
	
in_bound_helper2 :: (Int,Int) -> (Int,Int) -> Bool
in_bound_helper2 (x1,y1) (x2,y2)
	|y2 < 0		= False
	|y1 == y2 	= x1 <= x2
	|otherwise	= in_bound_helper2 (x1, y1) ((x2+1),(y2-1))

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



