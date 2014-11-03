(*

 _____            _           
|_   _|   _ _ __ | | ___  ___ 
  | || | | | '_ \| |/ _ \/ __|
  | || |_| | |_) | |  __/\__ \
  |_| \__,_| .__/|_|\___||___/
           |_|                
*)

fun fst [a:::Type] [b:::Type] ((x,_):(a*b)) : a = x

fun snd [a:::Type] [b:::Type] ((_,y):(a*b)) : b = y

(*
 _     _     _   
| |   (_)___| |_ 
| |   | / __| __|
| |___| \__ \ |_ 
|_____|_|___/\__|
                 
*)

fun head [a:::Type] (l:list a) : a = 
  case l of
    |[] => error <xml>head: empty list</xml>
    | x :: _ => x

fun tail [a:::Type] (l:list a) : list a = 
  case l of
    | [] => error <xml>head: empty list</xml>
    | _ :: l => l

fun for [a:::Type] [b:::Type] (l:list a) (f:a->b) : list b = List.mp f l

fun add1 [a ::: Type] (f : a -> a -> bool) (i : a) (ls : list a) : list a =
  let
    fun srch ls' =
      case ls' of
        | [] =>  (i :: ls)
        | x :: ls'' => if f x i then ls else srch ls''
  in
    srch ls
  end

fun cycledAt  [t ::: Type] (i:int) (ls : list t) : t =
  let
    val l = List.length ls
  in
    case List.nth ls (mod i l) of
      | Some x => x
      | None => error <xml>cycledAt: nth is None</xml>
  end

fun strlist (s:string) : list char =
  let
    val l = strlen s
    fun strlist' s i =
      case i >= l of
        |True => []
        |False => ((strsub s 0) :: (strlist' (strsuffix s 1) (i+1)))
  in
    strlist' s 0
  end


fun dropBy  [t ::: Type] (f:t -> bool) (l : list t) : list t =
  case l of
    |[] => []
    |(x :: ls) => if f x then dropBy f ls else x :: (dropBy f ls)

fun insert  [a ::: Type] [b ::: Type] (_:eq a) (x:a) (y:b) (l : list (a*b)) : list (a*b) =
  (x,y) :: dropBy (fn (x',_) => x' = x) l


fun delete  [a ::: Type] [b ::: Type] (_:eq a) (x:a) (l : list (a*b)) : list (a*b) =
  dropBy (fn (x',_) => x' = x) l

(*

 __  __                       _ 
|  \/  | ___  _ __   __ _  __| |
| |\/| |/ _ \| '_ \ / _` |/ _` |
| |  | | (_) | | | | (_| | (_| |
|_|  |_|\___/|_| |_|\__,_|\__,_|
                                
*)

fun ap [a:::Type] [b:::Type] [m:::Type->Type] (_:monad m) (f:a->b) (ma:m a) : m b =
  a <- ma;
  return (f a)

fun forM_ [m ::: (Type -> Type)] (_ : monad m) [a] (ls:list a) (f:a -> m {}) : m {} =
    let
        fun mapM' ls =
            case ls of
              | []      => return {}
              | x :: ls => f x; mapM' ls
    in
        mapM' ls
    end

fun forM [m ::: (Type -> Type)] (_ : monad m) [a] [b] (ls:list a) (f:a -> m b) : m (list b) = List.mapM f ls

fun foldlM_ [m ::: (Type -> Type)] (_ : monad m) [a ::: Type] [b ::: Type]
  (f:a -> b -> m b) (s:b) (l:list a) : m {} =
    _ <- List.foldlM f s l;
    return {}

(*

 __  __ _          
|  \/  (_)___  ___ 
| |\/| | / __|/ __|
| |  | | \__ \ (__ 
|_|  |_|_|___/\___|
*)
                   
fun id [t ::: Type] (x:t) : t = x

fun swap [a:::Type] [b:::Type] [c:::Type] (f:a->b->c) (y:b) (z:a) : c = f z y

(* val show_pair [a] [b] [show a] [show b] : show (a*b) = mkShow (fn (a,b) => "("^(show a) ^ "," ^ (show b) ^ ")") *)

val show_int_string  : show (int*string) = mkShow (fn (a,b) => "("^(show a) ^ "," ^ (show b) ^ ")")
val show_int_int  : show (int*int) = mkShow (fn (a,b) => "("^(show a) ^ "," ^ (show b) ^ ")")

val show_opt_int_int  : show (option (int*int)) = mkShow (fn o =>
  case o of
    |Some (a,b) => "Some ("^(show a) ^ "," ^ (show b) ^ ")"
    |None => "None")

