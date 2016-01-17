let rec exist e l =
	match l with 
		| [] -> false
		| h::t -> if e = h then true else exist e t;;

let rec subset a b = 
	match a with
		| [] -> true
		| h::t -> if exist h b then subset t b else false;;

let equal_sets a b = 
	if subset a b && subset b a then true else false;;

let set_union a b = a @ b;;

let rec set_intersection a b = 
	match a with
		| [] -> []
		| h::t -> if exist h b
					then h::set_intersection t b 
					else set_intersection t b;;

let rec set_diff a b = 
	match a with
		| [] -> []
		| h::t -> if exist h b
					then set_diff t b
					else h::set_diff t b;;

let rec computed_fixed_point eq f x = 
	if eq (f x) x
		then x
		else computed_fixed_point eq f (f x);;

(* call f(x) p times *)
let rec call_f f p x = 
	if p = 0
		then x
		else call_f f (p-1) (f x);;

let rec computed_periodic_point eq f p x = 
	if eq (call_f f p x) x
		then x
		else computed_periodic_point eq f p (f x);; 

type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal;;

let is_symbol_terminal sym terminal_symbols = 
	match sym with
		| T s -> true
		| N s -> exist s terminal_symbols
		| _ -> false;;

let rec is_right_hand_side_terminal rhs terminal_symbols = 
	match rhs with
		| [] -> true
		| h::t -> if is_symbol_terminal h terminal_symbols
					then is_right_hand_side_terminal t terminal_symbols
					else false;;

let rec find_terminal_set rules terminal_symbols = 
	match rules with
		| [] -> terminal_symbols
		| (sym, rhs)::t -> if is_right_hand_side_terminal rhs terminal_symbols
							then find_terminal_set t (sym::terminal_symbols)
							else find_terminal_set t terminal_symbols;;

let fixed_terminal_set (rules, terminal_symbols) = 
	(rules, find_terminal_set rules terminal_symbols);;

let snd_equal (ft_start, a) (sd_start, b) = 
	equal_sets a b;;

let rec check_rules rules terminal_symbols = 
	match rules with
		| [] -> []
		| (sym, rhs)::t -> if is_right_hand_side_terminal rhs terminal_symbols
							then (sym, rhs)::(check_rules t terminal_symbols)
							else check_rules t terminal_symbols;;

let filter_blind_alleys (start_sym, rules) =
	 (start_sym, check_rules rules (snd(computed_fixed_point snd_equal fixed_terminal_set (rules, []))));;