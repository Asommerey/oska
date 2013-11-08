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
move_generator_c7r7 board 'b' = [[]]

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
move_generator_bh_c7r7 :: [(Int,Int,Char)]->[[(Int,Int,Char)]]->[[(Int,Int,Char)]]
move_generator_bh_c7r7 board [] = [[]]

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



