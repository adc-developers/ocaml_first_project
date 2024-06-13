type person = { name : string; age : int; hobbies : string list }
type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

type db = person list

let newDatabase : db = []

let insert (p : person) (db : db) : db = p :: db

let remove (name : string) (db : db) : db =
  List.filter (fun p -> p.name <> name) db

let sort (comparator : comparator) (db : db) : db =
  List.sort comparator db

let rec query (cond : condition) (db : db) : db =
  match cond with
  | True -> db
  | False -> []
  | Age f -> List.filter (fun p -> f p.age) db
  | Name f -> List.filter (fun p -> f p.name) db
  | Hobbies f -> List.filter (fun p -> f p.hobbies) db
  | And (c1, c2) -> query c1 (query c2 db)
  | Or (c1, c2) -> List.sort_uniq compare (query c1 db @ query c2 db)
  | Not c -> List.filter (fun p -> not (List.mem p (query c db))) db
  | If (c1, c2, c3) ->
      if query c1 db = [] then query c2 db else query c3 db




let queryBy (cond : condition) (db : db) (comparator : comparator) : db =
  sort comparator (query cond db)

let update (cond : condition) (db : db) (change : person -> person) : db =
  List.map (fun p -> if List.mem p (query cond db) then change p else p) db

let deleteAll (cond : condition) (db : db) : db =
  List.filter (fun p -> not (List.mem p (query cond db))) db
