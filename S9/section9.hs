module Section9 where
import Parser
import Data.Char

type Code = [Instruction]

-- instructions for a simple machine
data Instruction
    = Copy Value Reg
    | Incr Reg
    | Decr Reg
    | JNZ Value Int
    deriving Show

-- a value is either a register or a number
data Value = Register Reg | Constant Int
    deriving Show

-- registers are named by lowercase letters
type Reg = Char

-- parse a string containing several instructions
-- (misparses are silently discarded)
-- In terminal type: parseCode testInput
parseCode :: String -> Code
parseCode = concat . map (parseAll instruction) . lines


-- In terminal type: parseAll instruction "cpy 1 a"
-- In terminal type: parseAll instruction testInput
-- In terminal type: parsePrefix instruction testInput
instruction :: Parser Instruction
instruction =
    Copy <$ string "cpy" <* space <*> value <* space <*> reg <|>
    Incr <$ string "inc" <* space <*> reg <|>
    Decr <$ string "dec" <* space <*> reg <|>
    JNZ <$ string "jnz" <* space <*> value <* space <*> int


-- In terminal type: parsePrefix value "b more stuff"
-- In terminal type: parsePrefix value testInput
value :: Parser Value
value = Register <$> reg <|> Constant <$> int


-- In terminal type: parsePrefix reg "a b"
-- In terminal type: parsePrefix reg testInput
reg :: Parser Reg
reg = satisfy isLower

-- In terminal type: testInput
testInput :: String
testInput = "\
    \cpy 1 a\n\
    \cpy 1 b\n\
    \cpy 26 d\n\
    \jnz c 2\n\
    \jnz 1 5\n\
    \cpy 7 c\n\
    \inc d\n\
    \dec c\n\
    \jnz c -2\n\
    \cpy a c\n\
    \inc a\n\
    \dec b\n\
    \jnz b -2\n\
    \cpy c b\n\
    \dec d\n\
    \jnz d -6\n\
    \cpy 19 c\n\
    \cpy 11 d\n\
    \inc a\n\
    \dec d\n\
    \jnz d -2\n\
    \dec c\n\
    \jnz c -5\n"