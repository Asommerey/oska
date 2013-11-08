oska_c7r7 :: [String]->Char->Int->[String]
oska_c7r7 board player moves = []


--Static Board Evaluator
board_eval_c7r7 :: [String]->Char->Int
board_eval_c7r7 board 'w' = board_eval_wh_c7r7 board
board_eval_c7r7 board 'b' = board_eval_bh_c7r7 board

--Helper Function to Evaluate for White
board_eval_wh_c7r7 :: [String]->Int
board_eval_wh_c7r7 board = 0

--Helper Function to Evaluate for Black
board_eval_bh_c7r7 :: [String]->Int
board_eval_bh_c7r7 board = 0

--Move Generator
move_generator_c7r7 :: [(Int,Int,Char)]->Char->[[(Int,Int,Char)]]
move_generator_c7r7 board 'w' = move_generator_wh_c7r7 [] board 
move_generator_c7r7 board 'b' = [[]]

--Helper Function to Generate for White
move_generator_wh_c7r7 :: [(Int,Int,Char)]->[(Int,Int,Char)]->[[(Int,Int,Char)]]
move_generator_wh_c7r7 piecesBefore [] = []
move_generator_wh_c7r7 piecesBefore (x:xs) = 
				(generate_new_states_w_r_c7r7 x piecesBefore xs):
				(move_generator_wh_c7r7 (piecesBefore++(x:[])) xs)

--Generates New Jumps to the right for white
generate_new_states_w_r_c7r7 :: (Int,Int,Char)->
				[(Int,Int,Char)]->
				[(Int,Int,Char)]->
				[(Int,Int,Char)]
generate_new_states_w_r_c7r7 (x,y,char) piecesBefore piecesAfter
	| char == 'w' && (open_space_c7r7 x y (piecesBefore++piecesAfter))
	= piecesBefore++((x+1),(y+1),'w'):piecesAfter
	| otherwise = []

open_space_c7r7 :: Int->Int->[(Int,Int,Char)]->Bool
open_space_c7r7 newx newy [] = True
open_space_c7r7 newx newy (x:xs) = (open_space_h_c7r7 newx newy x (last xs)) && 
			      (open_space_c7r7 newx newy xs)

open_space_h_c7r7 :: Int->Int->(Int,Int,Char)->(Int,Int,Char)->Bool
open_space_h_c7r7 newx newy (x,y,_) (n,numRows,_)
	| (newx == x) && (newy == y) = False
	| otherwise = True

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



