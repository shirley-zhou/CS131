(*Problem 1: subset returns whether a is a subset of b*)
let rec contains e l = match l with
    [] -> false
  | h::t -> if e=h then true else contains e t;;

let rec subset a b = match a with
    [] -> true
  | h::t -> if (contains h b) then (subset t b) else false;;

(*Problem 2: equal_sets, check if two sets are equal by checking if each is a subset of the other*)
let rec equal_sets a b = if (subset a b && subset b a) then true else false;;

(*Problem 3: return a union b, repeats allowed so just concatenate*)
let set_union a b = a@b;;

(*Problem 4: set_intersection returns a intersect b, so for each element in a, check that it is also in b*)
let rec set_intersection a b = match a with
    [] -> []
  | h::t -> if (contains h b) then h::(set_intersection t b) else (set_intersection t b);;

(*Problem 5: set_diff returns a-b, all elements in a NOT in b*)
let rec set_diff a b = match a with
    h::t -> if (contains h b) then set_diff t b else h::(set_diff t b)
  | [] -> [];;

(*Problem 6: find fixed point for function f, where input to function eq output, where eq is some sort of equality check*)
let rec computed_fixed_point eq f x = if (eq (f x) x) then x else computed_fixed_point eq f (f x);;

(*Problem 7: find periodic point for function f given period p, where taking function f on input p times yields output==input*)
let rec loop_p_times f p x = if p=0 then x else loop_p_times f (p-1) (f x);;

(*for x: do computed_fixed_point for p times then check equal. if not then plug in f x and try again*)
let rec computed_periodic_point eq f p x = if (loop_p_times f p x)=x then x else computed_periodic_point eq f p (f x);;

(*Problem 8: return longest list such that p e is true for every element in e, progresses with [x; s x; s(s x); ...] as long as p e is true for each consecutive element*)
let rec while_away s p x = if p x then x::(while_away s p (s x)) else [];;

(*Problem 9: expnd pairs in run-length encoding form where each pair is (length, element) to repeat element by length times*)
let rec expand_pair pair = match pair with
    (x, y) -> if x=0 then [] else y::(expand_pair (x-1, y));;

let rec rle_decode lp = match lp with
    h::t -> (expand_pair h)@(rle_decode t)
  | _ -> [];;

(*Problem 10:*)
(***********Filter Blind Alleys************)

type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

(*Loop through rhs list. For each element, if itś a terminal can move on and check rest. If not terminal, return false*)
let rec all_terminals rhs = match rhs with
    [] -> true
  | h::t -> (match h with 
                T h -> all_terminals t
              | N h -> false);;

(*Loop through all rules. For each rule, if rhs contains either all terminals or already validated symbols, then lhs symbols is also valid. If not already in validated list, add to list. Continue looping through remaining rules. Return pair (validated, rules)*)
let rec loop_rules (validated, rules) = match rules with
    (lhs,rhs)::t -> let var = N lhs in (if all_terminals (set_diff rhs validated) && not (contains var validated) then loop_rules (var::validated, t) else loop_rules (validated, t))
  | [] -> validated;;

(*Call another function to loop through rules and add to validated list. Return new pair of (validated, rules) where validated may have had elements added*)
let validate_rules (validated, rules) = (loop_rules (validated, rules), rules);;

(*Loop through rules and filter by validated symbols. Check if lhs is a validated symbol. Rhs must also contain only validated symbols OR, for any elements that are not validated symbols, they must be all_terminals, Return list of ok rules*)
(*requirement for rule to be acceptable: lhs is validated, and every element of rhs is validated or already terminal*)
let rec filter_rules rules validated = match rules with
    (lhs, rhs)::t -> let var = N lhs in (if contains var validated && all_terminals (set_diff rhs validated) then (lhs, rhs)::(filter_rules t validated) else filter_rules t validated)
  | _ -> [];;

(*For the equality check that computed_fixed_point uses, only need to check the first list in pair (validated, rules). The validated rules list is the only thing changing. Check if new iteration has added to it or left it unchanged*)
let pair_equal (v1, rules) (v2, rules) = equal_sets v1 v2;;

(*Use computed_fixed_point to keep repeatedly attempt to validate more rules each iteration, until get repeat results ie done. Then call filter_rules to filter the rules list by only those which contain symbols returned in the validated list*)
let process_rules rules = 
  filter_rules rules (fst (computed_fixed_point pair_equal validate_rules ([], rules)));;

(*return pair (start, filtered_rules)*)
let filter_blind_alleys g = match g with
    (start, rules) -> (start, process_rules rules);;
