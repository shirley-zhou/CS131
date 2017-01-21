type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

(*Problem 1: convert grammar format from hw1 to hw2 format*)

(*For a given nt, search through rules list for lhs that matches. Return a list of all concatenated rhs possibilities for that nt*)
let rec convert_rules rules nt = match rules with
  | (lhs, rhs)::t -> if lhs = nt then rhs::(convert_rules t nt) else (convert_rules t nt)(*where rules is a list of pairs, grab first pair*)
  | [] -> [];;

let rec convert_grammar gram1 = match gram1 with
    (start, rules) -> (start, convert_rules rules);;

(*Problem 2: Return a matcher for the grammar gram*)
(*try to expand a specific rule until it equals*)
(*loop through all terms*)
let rec expand_rule all_rules a_rule accept derivation frag = match a_rule with
  | [] -> accept derivation frag
  | first::rest -> match frag with
    | [] -> None
    | prefix::suffix -> match first with
      | T symbol -> if prefix = symbol then (expand_rule all_rules rest accept derivation suffix) else None
      | N symbol -> try_derive symbol all_rules (all_rules symbol) (expand_rule all_rules rest accept) derivation frag

(*try to derive fragment from specific rule_list*)
(*loop through the rule list of a specific nonterminal, try to find a rule that derives to frag*)
and try_derive symbol all_rules sub_rules accept derivation frag = match sub_rules with
  | [] -> None
  | first::rest -> match (expand_rule all_rules first accept (derivation@[(symbol, first)]) frag) with
    | None -> try_derive symbol all_rules rest accept derivation frag
    | result -> result;;

let matcher start rules accept frag = try_derive start rules (rules start) accept [] frag;;

(*given a grammar gram, return a matcher FUNCTION for that grammar. The matcher must be a function that takes an acceptor accept and fragment frag*)
let parse_prefix gram = match gram with
    (start, rules) -> matcher start rules;;

(*try grammar rules in order*)
(*if accept succeeds on a derivation and suffix fragment -> return acceptor´s ret*)
(*else return None*)