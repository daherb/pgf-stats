import PGF
import PGF.Internal
import System.Environment
import Data.List

-- | Type 'FunType' consists of a CId that is the the result category and [CId] are the parameter categories
data FunType = Fun CId [CId] | NoType deriving (Ord,Eq,Show)

isEmptyPGF pgf = absname pgf == wildCId

-- | The function 'getFunType' extracts the function type from a grammar given a function name
getFunType :: PGF -> CId -> FunType
getFunType grammar id
  | isEmptyPGF grammar = NoType -- Empty grammar
  | otherwise =
    let
      typ = functionType grammar id
    in
      case typ of {
        Nothing -> NoType ; -- No type found in grammar
        Just t ->
        let
          (hypos,typeid,exprs) = unType t
          cats = (map (\(_,_,DTyp _ cat _) -> cat) hypos)
        in
          (Fun typeid cats) ;
        }
isSyntactic :: (CId,FunType) -> Bool
isSyntactic (_,t) =
    case t of {
      Fun _ [] -> False;
      _ -> True
      }
    
main =
  do
    args <- getArgs
    putStrLn $ show args
    pgf <- readPGF $ args !! 0
    let funs = functions pgf
    let missing = missingLins pgf $ mkCId $ args !! 1
    let impl = funs \\ missing
    let types = map (getFunType pgf) impl
    let mtypes = map (getFunType pgf) missing
    let (syn,lex) = partition isSyntactic $ zip impl types
    let (msyn,mlex) = partition isSyntactic $ zip missing mtypes
    putStrLn $ (show $ length lex) ++ " lexical constructions"
    putStrLn $ show lex
    putStrLn $ (show $ length syn) ++ " syntactic constructions"
    putStrLn $ show syn
    putStrLn $ (show $ length mlex) ++ " missing lexical constructions"
    putStrLn $ show mlex
    putStrLn $ (show $ length msyn) ++ " missing syntactic constructions"
    putStrLn $ show msyn
    return ()
    
