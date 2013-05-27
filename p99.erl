%% ================================
%%      99 Questions in Erlang
%% -------------------------------- 
%% 
%% Reference for the Lisp original:
%%  
%% http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
%%
%% @author Paul J. Sholtz
%% @copyright 2013 sholtz9421.com
%% @doc Implementation of the 99 (Functional) Questions in Erlang.
%% @end
%% =================================
-module(p99).
-author('psholtz [at] gmail (dot) com').

%% ========================= 
%% List Manipulation Exports
%% ========================= 
-export([my_last/1,            %% Problem 01
	 my_but_last/1,        %% Problem 02
	 element_at/2,         %% Problem 03
	 length/1,             %% Problem 04
	 reverse/1,            %% Problem 05
	 reverse_no_tail/1,
	 palindrome/1,         %% Problem 06
 	 my_flatten/1,         %% Problem 07
	 compress/1,           %% Problem 08
	 pack/1,               %% Problem 09
	 encode/1,             %% Problem 10
	 encode_modified/1,    %% Problem 11
	 encode_modified1/1,
	 decode_modified/1,    %% Problem 12
	 encode_direct/1,      %% Problem 13
	 dupli/1,              %% Problem 14
	 repli/2,              %% Problem 15 
	 drop/2,               %% Problem 16
	 split/2,              %% Problem 17
	 slice/3,              %% Problem 18
	 rotate/2,             %% Problem 19
	 remove_at/2,          %% Problem 20
	 insert_at/3,          %% Problem 21
	 range/2,              %% Problem 22
	 md_select/2,          %% Problem 23
	 lotto_select/2,       %% Problem 24
	 md_permu/1,           %% Problem 25
	 combinations/2,       %% Problem 26
	 group3/1,             %% Problem 27 
	 group/2,  
	 lsort/1,              %% Problem 28
	 lfsort/1
]).

%% ============= 
%% Documentation
%% ============= 
-export([document/0]).

%%
%% It will mess up our "length" procedure if we import the BIF.
%%
-compile({no_auto_import,[length/1]}).

%%
%% =============
%%  CHEAT SHEET
%% ============= 
%% 
%% For "solutions" which can be accomplished using a BIF or standard Erlang library, a "cheat" attribute 
%% is attached to the answer as well since, in the "real" world, we'll want to know and use these more 
%% "standardized" tools.
%%

%%
%% =================== 
%%  TERMINATING CASES
%% =================== 
%%
%% Lispers are generally trained to put the "terminating" case "first" in the code, and in general this is 
%% my preference as well. However, Erlang lends itself so well to an almost "pictorial" style of coding 
%% -- in the sense of "drawing" how the data structures flow through the algorithm -- that for many of 
%% the solutions presented below I place the terminating case at the end, where it "fits pictorially" more
%% naturally (in terms of the program flow).
%%

%%
%% ======================== 
%%  REVERSING ACCUMULATORS
%% ======================== 
%% 
%% One of the most canonical ways to solve a functional problem is to make use of an accumulator variable.
%% For many of the problems below, it is more natural to build the accumulator in reverse order, which 
%% will save us the effort of traversing the (increasingly growing) accumulator list each time we evaluate
%% a new argument. When this pattern is used, we reverse the accumulator before terminating.
%%

%%
%% ==============================
%%  PART I -- LIST MANIPULATIONS 
%% ==============================
%%

%% =====================================
%% @doc
%% P01 (*) Find the last box of a list.
%% @end
%%
%% Example:
%% > (my-list '(a b c d))
%% > (d)
%%
%% CHEAT: lists:last/1
%% ===================================== 
-spec my_last(List) -> T when 
      List :: [T], 
      T :: term().

my_last(L) when is_list(L) -> my_last(L, []).
my_last([H|T],_) -> my_last(T,H);
my_last([],Acc) -> Acc.

%% =============================================
%% @doc
%% P02 (*) Find the last but one box of a list.
%% @end
%%
%% Example:
%% > (my-but-last '(a b c d)
%% > (c d)
%% =============================================
-spec my_but_last(List) -> T when 
      List :: [T], 
      T :: term().				   

my_but_last(L) when is_list(L) -> my_but_last(L,[],[]).
my_but_last([H|T],A,_) -> my_but_last(T,H,A);
my_but_last([],_,A) -> A.

%% ===========================================
%% @doc
%% P03 (*) Find the K'th element of a list.
%%
%% The first element in the list is number 1.
%% @end
%%
%% Example:
%% > (element-at '(a b c d e) 3)
%% > c
%%
%% CHEAT: lists:nth/2
%% ===========================================
-spec element_at(List, N) -> T when 
      List :: [T], N :: pos_integer(), 
      T :: term().

element_at(L,N) when is_list(L), is_integer(N), N > 0 -> element_at(L,N,1).
element_at([H|_],_N,_N) -> H;
element_at([_|T],N,K) -> element_at(T,N,K+1);
element_at([],_,_) -> {error, out_of_bounds}.

%% ===============================================
%% @doc
%% P04 (*) Find the number of elements in a list.
%% @end
%%
%% CHEAT: length/1 (BIF)
%% ===============================================
-spec length(List) -> non_neg_integer() when 
      List :: [T], T :: term().

length(L) when is_list(L) -> length(L,0).
length([_|T], Acc) -> length(T, Acc+1);
length([], Acc) -> Acc.

%% ===========================================
%% @doc
%% P05 (*) Reverse a list.
%% 
%% Non-tail-recursive version.
%% @end
%%
%% CHEAT: lists:reverse/1
%% ===========================================
-spec reverse_no_tail(List) -> List when 
      List :: [T], T :: term().

reverse_no_tail([H|T]) -> reverse_no_tail(T) ++ [H];
reverse_no_tail([]) -> [].

%% ===========================================
%% @doc
%% P05 (*) Reverse a list.
%%
%% Tail-recursive version.
%% @end
%%
%% CHEAT: lists:reverse/1
%% =========================================== 
-spec reverse(List) -> List when 
      List :: [T], T :: term().

reverse(L) when is_list(L) -> reverse(L,[]).
reverse([H|T], Acc) -> reverse(T, [H|Acc]);
reverse([], Acc) -> Acc.

%% =================================================
%% @doc
%% P06 (*) Find out whether a list is a palindrome.
%%
%% A palindrome can be read forward or backward;
%% e.g., (x a m a x).
%% @end
%% =================================================
-spec palindrome(List) -> boolean() when 
      List :: [T], T :: term(). 

palindrome(L) when is_list(L) ->
    M = reverse(L),
    case L of
        M -> true;
        _ -> false
    end.

%% ===============================================
%% @doc
%% P07 (**) Flatten a nested list structure.
%%
%% Transform a list, possibly holding lists as
%% elements into a 'flag' list by replacing each
%% list with its elements (recursively).
%% @end
%%
%% Example:
%% > (my-flatten '(a (b (c d) e)))
%% > (a b c d e)
%%
%% CHEAT: lists:flatten/2
%% ===============================================
-spec my_flatten(DeepList) -> List when 
      DeepList :: [term() | DeepList], 
      List :: [term()]. 

my_flatten(L) when is_list(L) -> my_flatten(L,[]).
my_flatten([H|T], Acc) when is_list(H) -> my_flatten(T, Acc ++ my_flatten(H,[]));
my_flatten([H|T], Acc) -> my_flatten(T, Acc ++ [H]);
my_flatten([], Acc) -> Acc.

%% ============================================================ 
%% @doc
%% P08 (**) Eliminate consecutive duplicates of list elements.
%%
%% If a list contains repeated elements they should be replaced 
%% with a single copy of the element. The order of the elements
%% should not be changed.
%% @end
%%
%% Example:
%% > (compress '(a a a a b c c a a d e e e e))
%% > (a b c a d e)
%% ============================================================ 
%%
%% For "purists" who don't want to fall back on the standard 
%% library function lists:reverse/1, note that we implemented
%% our own version of "reverse" in P05.
%%
-spec compress(List) -> List when 
      List :: [T], T :: term().

compress(L) when is_list(L) -> compress(L, []).
compress([H|T], []) -> compress(T, [H]);
compress([H|T], [H]) -> compress(T, [H]);
compress([H|T], [A]) -> compress(T, [H] ++ [A]);
compress([H|T], [H|B]) -> compress(T, [H|B]);
compress([H|T], [A|B]) -> compress(T, [H] ++ [A|B]);
compress([], Acc) -> lists:reverse(Acc).

%% ===================================================================== 
%% @doc
%% P09 (**) Pack consecutive duplicates of list elements into sublists.
%%
%% If a list contains repeated elements they should be placed in 
%% separate sublists. 
%% @end
%%
%% Example:
%% > (pack '(a a a a b c c a a d e e e e))
%% > ((a a a a) (b) (c c) (a a) (d) (e e e e))
%% =====================================================================
-spec pack(List) -> DeepList when 
      DeepList :: [List], 
      List :: [term()].

pack(L) when is_list(L) -> pack(L, [], []).
pack([H|T], [], Acc) -> pack(T, [H], Acc);
pack([H|T], [H|A], Acc) -> pack(T, [H] ++ [H|A], Acc);
pack([H|T], [A|B], Acc) -> pack(T, [H], [[A|B]] ++ Acc);
pack([], A, Acc) -> lists:reverse([A] ++ Acc).

%% ====================================================================== 
%% @doc
%% P10 (**) Run-length encoding of a list.
%%
%% Use the result of P09 to implement the so-called run-length encoding
%% data compression method. Consecutive duplicates of elements are 
%% encoded as lists (N E) where N is the number of duplicates of the 
%% element E.
%% @end
%%
%% Example:
%% > (encode '(a a a a b c c a a d e e e e))
%% > ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
%% ====================================================================== 
-spec encode(List) -> DeepList when 
      DeepList :: [List], 
      List :: [term()].

encode(L) when is_list(L) -> encode(L, [], []).
encode([H|T], [], Acc) -> encode(T, [1,H], Acc);
encode([H|T], [N,H], Acc) -> encode(T, [N+1,H], Acc);
encode([H|T], A, Acc) -> encode(T, [1,H], [A] ++ Acc);
encode([], A, Acc) -> lists:reverse([A] ++ Acc).

%% ====================================================================== 
%% @doc
%% P11 (*) Modified run-length encoding.
%%
%% Modify the result of problem P10 in such a way that if an element
%% has no duplicates it is simply copied into the result list. Only
%% elements with duplicates are transferred as (N E) lists.
%%
%% Implemented using HOPs and lists:map.
%% @end
%%
%% Example:
%% > (encode-modified '(a a a a b c c a a d e e e e))
%% > ((4 a) b (2 c) (2 a) d (4 e))
%% ====================================================================== 
%%
%% One simple way to implement this is to map a HOF over the solution to 
%% the previous exercise.
%%
%% For "purists" who don't want to fall back on the standard library
%% function lists:map/2, note that a simple implementation of "map"
%% can be given as:
%%
%% map(F, L) -> map(F, L, []).
%% map(_, [], Acc) -> lists:reverse(Acc);
%% map(F, [H|T], Acc) -> map(F, T, [F(H)|Acc]).
%%
-spec encode_modified1(List) -> DeepList when 
      DeepList :: [term() | List], 
      List :: [term()].

encode_modified1(L) when is_list(L) -> 
    Mapper = fun([H|T]) ->
  	         case H of
	             1 -> hd(T);
	             _ -> [H|T]
                 end
             end,
    lists:map(Mapper, encode(L)).

%% ======================================================================
%% @doc
%% P11 (*) Modified run-length encoding.
%%
%% Modify the result of problem P10 in such a way that if an element
%% has no duplicates it is simply copied into the result list. Only
%% elements with duplicates are transferred as (N E) lists.
%%
%% Implemented "manually".
%% @end
%%
%% Example:
%% > (encode-modified '(a a a a b c c a a d e e e e))
%% > ((4 a) b (2 c) (2 a) d (4 e))
%% ======================================================================
%%
%% We can also code this "by hand" by adding just one more pattern to match:
%%
-spec encode_modified(List) -> DeepList when 
      DeepList :: [term() | List], 
      List :: [term()].

encode_modified(L) when is_list(L) -> encode_modified(L, [], []).
encode_modified([H|T], [], Acc) -> encode_modified(T, H, Acc);
encode_modified([H|T], H, Acc) -> encode_modified(T, [2,H], Acc);
encode_modified([H|T], [N,H], Acc) -> encode_modified(T, [N+1,H], Acc);
encode_modified([H|T], A, Acc) -> encode_modified(T, H, [A] ++ Acc);
encode_modified([], A, Acc) -> lists:reverse([A] ++ Acc).

%% ===============================================================
%% @doc
%% P12 (**) Decode a run-length encoded lists.
%% @end
%%
%% Given a run-length encoded list generated as specified in P11. 
%% Construct its uncompressed version.
%% ===============================================================
-spec decode_modified(DeepList) -> List when 
      DeepList :: [term() | List], 
      List :: [term()].

decode_modified(L) when is_list(L) -> decode_modified(L, []).
decode_modified([H|T], Acc) when is_list(H) -> 
    Num = hd(H), Elem = hd(tl(H)),
    decode_modified(T, Acc ++ duplicate(Num, Elem));
decode_modified([H|T], Acc) -> decode_modified(T, Acc ++ [H]);
decode_modified([], Acc) -> Acc.

%%
%% To make this procedure work, we first need to definea helper 
%% "duplicate" procedure. We'll need this procedure to solve other 
%% problems as well, so let's define and abstract it out here.
%%
%% Note that this is (essentially) identical to lists:duplicate/2:
%%
-spec duplicate(N, T) -> List when 
      List :: [T], T :: term(), 
      N :: non_neg_integer().

duplicate(Num, Elem) when is_integer(Num), Num >= 0 -> duplicate(Num, Elem, []).
duplicate(Num, Elem, Acc) when Num > 0 -> duplicate(Num-1, Elem, [Elem|Acc]);
duplicate(0, _, Acc) -> Acc.

%% ================================================================ 
%% @doc
%% P13 (**) Run-length encoding of a list (direct solution)
%%
%% Implement the so-called run-length encoding data compression
%% method directly, i.e., don't explicitly create the sublists
%% containing the duplicates, as in P09, but only count them.
%% As in P11, simplify the result list by replacing the singleton
%% lists (1 X) by X.
%% @end
%%
%% Example:
%% > (encode-direct '(a a a a b c c a a d e e e e)
%% > ((4 a) b (2 c) (2 a) d (4 e))
%% ================================================================ 
-spec encode_direct(List) -> DeepList when 
      DeepList :: [term() | List], 
      List :: [term()].

encode_direct([H|T]) -> encode_direct(T, H, 1, []);
encode_direct([]) -> [].
encode_direct([H|T], H, Num, Acc) -> encode_direct(T, H, Num+1, Acc);
encode_direct([H|T], Elem, 1, Acc) -> encode_direct(T, H, 1, Acc ++ [Elem]);
encode_direct([H|T], Elem, Num, Acc) -> encode_direct(T, H, 1, Acc ++ [[Num, Elem]]);
encode_direct([], Elem, 1, Acc) -> Acc ++ [Elem];
encode_direct([], Elem, Num, Acc) -> Acc ++ [[Num, Elem]].

%% ==================================================================== 
%% @doc
%% P14 (*) Duplicate the elements of a list.
%% @end
%%
%% Example:
%% > (dupli '(a b c c d))
%% > (a a b b c c c c d d)
%%
%% CHEAT: This is similar to (but different from): lists:duplicate/2.
%% ===================================================================
-spec dupli(List) -> List when 
      List :: [term()].

dupli(L) when is_list(L) -> dupli(L,[]).
dupli([H|T], Acc) -> dupli(T, Acc ++ [H,H]);
dupli([], Acc) -> Acc.

%% =================================================================== 
%% @doc
%% P15 (**) Replicate the elements of a list given a number of times.
%% @end
%%
%% Example:
%% > (repli '(a b c) 3)
%% > (a a a b b b c c c)
%%
%% CHEAT: This is similar to (but different from): lists:duplicate/2.
%% =================================================================== 
%%
%% We can utilize the "duplicate/2" helper function we defined above:
%%
-spec repli(List, N) -> List when 
      List :: [term()], 
      N :: non_neg_integer().

repli(L, N) when is_list(L), is_integer(N), N >= 0 -> repli(L, N, []).
repli([H|T], N, Acc) -> repli(T, N, Acc ++ duplicate(N,H));
repli([], _, Acc) -> Acc.

%% ============================================================ 
%% @doc
%% P16 (**) Drop every N'th element from a list.
%% @end
%%
%% Example:
%% > (drop '(a b c d e f g h i j k) 3)
%% > (a b d e g h j k)
%%
%% CHEAT: Similar, albeit different, standard library
%% functions include lists:delete/2 and lists:dropwhile/2.
%% This could even be solved (probably) using lists:filter/2.
%% ============================================================
-spec drop(List, N) -> List when 
      List :: [term()], 
      N :: pos_integer().

drop(L, N) when is_list(L), is_integer(N), N > 0 -> drop(L, N, 1, []).
drop([_|T], N, N, Acc) -> drop(T, N, 1, Acc);
drop([H|T], N, Count, Acc) -> drop(T, N, Count+1, Acc ++ [H]);
drop([], _, _, Acc) -> Acc.

%% ============================================================================
%% @doc
%% P17 (*) Split a list into two parts; the length of the first part is given.
%% 
%% Do not use any predefined predicates.
%% @end
%%
%% Example:
%% > (split '(a b c d e f g h i j k) 3)
%% > ((a b c) (d e f g h i j k))
%%
%% CHEAT: This is essentially identical to lists:split/2.
%% ============================================================================ 
-spec split(List, N) -> {List,List} when 
      List :: [term()], 
      N :: non_neg_integer().

split(L, N) when is_list(L), is_integer(N), N >= 0 -> split(L, N, 0, []).
split(L, N, N, Acc) -> {Acc, L}; 
split([H|T], N, Count, Acc) -> split(T, N, Count+1, Acc ++ [H]);
split([], _, _, Acc) -> {Acc,[]}.

%% =============================================================================== 
%% @doc
%% P18 (**) Extract a list from a list.
%%
%% Given two indices, I and K, the slice is the list containing the elements
%% between the Ith and Kth elements of the origianl list (both limits included).
%% Start counting the elements with 1.
%% @end
%%
%% Example:
%% > (slice '(a b c d e f g h i j k) 3 7)
%% > (c d e f g)
%% ==============================================================================
-spec slice(List, N, N) -> List when 
      List :: [term()], 
      N :: pos_integer().

slice([], _, _) -> [];
slice(L, I, K) when is_list(L), is_integer(I), is_integer(K), I > 0, K > 0, K >= I -> 
    case A = element_at(L,I) of
        {error,out_of_bounds} -> [];
        _ -> slice(L, I, K, I+1, [A])
    end.
slice(L, I, K, Count, Acc) when K >= Count ->
    case A = element_at(L,Count) of
        {error,out_of_bounds} -> Acc;
        _ -> slice(L, I, K, Count+1, Acc ++ [A])
    end;
slice(_, _, _, _, Acc) -> Acc.

%% ======================================================= 
%% @doc
%% P19 (**) Rotate a list N places to the left.
%% @end
%%
%% Examples:
%% > (rotate '(a b c d e f g h) 3)
%% > (d e f g h a b c)
%%
%% > (rotate '(a b c d e f g h) -2)
%% > (g h a b c d e f)
%%
%% Hint: Use the predefined functions length and append, 
%%       as well as the result of problem P17.
%% =======================================================
-spec rotate(List, N) -> List when 
      List :: [term()], 
      N :: integer().

rotate(L, N) when is_list(L), is_integer(N) -> 
    A = split(L, mod(N, length(L))),
    element(2,A) ++ element(1,A).

%% 
%% We need a "mod" function to support the "rotate" procedure above:
%%
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0. 

%% ===============================================
%% @doc
%% P20 (*) Remove the K'th element from a list.
%% @end
%%
%% Example:
%% > (remove-at '(a b c d) 2)
%% > (a c d)
%% 
%% CHEAT: Simiar to lists:delete/2.
%% =============================================== 
-spec remove_at(List, N) -> List when 
      List :: [term()], 
      N :: pos_integer().

remove_at(L, N) when is_list(L), is_integer(N), N > 0 -> remove_at(L, N, length(L)).
remove_at(L, N, Len) when N > Len -> L;
remove_at(L, N, _) ->
    SplitList = split(L, N),
    FirstList = element(1, SplitList),
    DeletedList = drop(FirstList, length(FirstList)),
    DeletedList ++ element(2, SplitList).
    

%% =============================================================
%% @doc
%% P21 (*) Insert an element at a given position into a list.
%% @end
%%  
%% Example:
%% > (insert-at 'alfa '(a b c d) 2)
%% > (a alfa b c d)
%% =============================================================
-spec insert_at(Token, List, N) -> List when 
      Token:: term(), 
      List ::[term()], 
      N :: pos_integer().

insert_at(T, L, N) when is_list(L), is_integer(N), N > 0 ->
    {Front, Back} = split(L, N-1),
    Front ++ [T] ++ Back.  

%% ====================================================================
%% @doc
%% P22 (*) Create a list containing all integers within a given range.
%%
%% If the first argument is larger than the second, produce a list 
%% in decreasing order.
%% @end
%%
%% Example:
%% > (range 4 9)
%% > (4 5 6 7 8 9)
%% ====================================================================
-spec range(N, M) -> List when 
      N :: integer(), 
      M ::integer(), 
      List :: [term()].

range(N, M) when N < M, is_integer(N), is_integer(M) -> range(N, M, up, []);
range(N, M) when is_integer(N), is_integer(M)  -> range(N, M, down, []).

range(N, N, _, Acc) -> Acc ++ [N];
range(N, M, up, Acc) -> range(N+1, M, up, Acc ++ [N]);
range(N, M, down, Acc) -> range(N-1, M, down, Acc ++ [N]).

%% ===========================================================================
%% @doc
%% P23 (**) Extract a given number of randomly selected elements from a list.
%%
%% The selected items shall be returned in a list.
%% @end
%%
%% Example:
%% > (md-select '(a b c d e f g h) 3)
%% > (e d a)
%%
%% Hint: Use the built-in random number generator and the result of P20.
%% =========================================================================== 
-spec md_select(List, N) -> List when 
      List :: [term()], 
      N :: pos_integer().

md_select(List, N) when is_list(List), is_integer(N), N > 0, N =< erlang:length(List) -> 
    random:seed(now()),
    md_select(List, N, 0, []).

md_select(_, N, N, Acc) -> Acc;
md_select(List, N, Count, Acc) ->
    K = random:uniform(length(List)),
    NewElement = lists:nth(K, List),
    NewList = remove_at(List, K),
    md_select(NewList, N, Count+1, [NewElement|Acc]).

%% ===================================================================== 
%% @doc
%% P24 (*) Lotto: Draw N different random numbers from the set 1 .. M. 
%% 
%% The selected numbers shall be returned in a list.
%% @end
%%
%% Example:
%% > (lotto-select 6 49)
%% > (23 1 17 33 21 37)
%% 
%% Hint: Combine the solutions of problems P22 and P23.
%% ===================================================================== 
-spec lotto_select(N, Size) -> List when 
      N :: pos_integer(), 
      Size :: pos_integer(), 
      List :: [term()].

lotto_select(N, Size) when is_integer(N), is_integer(Size), N > 0, Size > 0, N =< Size ->
    Sample = range(1, Size),
    md_select(Sample, N).

%% =================================================================
%% @doc 
%% P25 (*) Generate a random permutation of the elements of a list. 
%% @end
%%
%% Example:
%% > (md-permu '(a b c d e f))
%% > (b a d c e f)
%%
%% Hint: Use the solutions of problem P23.
%% =================================================================
-spec md_permu(List) -> List when 
      List :: [term()].

md_permu(List) when is_list(List) ->
    md_select(List, length(List)).

%% =============================================================================================== 
%% @doc
%% P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
%%          
%% In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that 
%% there are C(12,3) = 220 possibilities. For pure mathematicians, this result may be great. But
%% we want to really generate all the possibilities in a list.
%% @end
%%
%% Example:
%% > (combinations 3 '(a b c d e f))
%% > ((a b c) (a b d) (a b e) ...)
%% =============================================================================================== 
-spec combinations(N, List) -> DeepList when 
      N :: non_neg_integer(),
      List :: [term()],
      DeepList :: [term() | DeepList].
      
combinations(0, _) -> [[]];
combinations(_, []) -> [[]];
combinations(N, List) when is_integer(N), is_list(List), N =:= erlang:length(List) -> [List];
combinations(N, List) when is_integer(N), is_list(List), N  < erlang:length(List) ->
    %% Find the sub-combinations, beginning at the given index
    SubCombos = fun(Index) ->
		    Elem = element_at(List, Index),
		    SubList = lists:sublist(List, Index+1, length(List)-Index+1),
		    lists:map(fun(X) -> [Elem] ++ X end, combinations(N-1, SubList))
		end,
    unpack([SubCombos(Index) || Index <- range(1, length(List) - N + 1)]).
     
%%
%% Helper function for the "combinations" procedure above, to "unpack" a collection 
%% of lists into a single master list (similar to the "flatten" or "my-flatten" 
%% procedures above, but we want to start the flattening process at just one
%% level of recursion.
%%
-spec unpack(DeepList) -> DeepList when
      DeepList :: [term() | DeepList].

unpack(L) -> unpack(L, []).
unpack([], Acc) -> Acc;
unpack([H|T], Acc) -> unpack(T, lists:append(Acc, H)).

%% ================================================================================
%% @doc
%% P27A (**) Group the elements of a set into disjoint subsets.
%%
%% (a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 
%% 2, 3 and 4 persons? Write a function that generates all the possibilities and 
%% returns them in a list.
%% @end
%%
%% Example A:                                                                                                  
%% > (group3 '(aldo beat carla david evi flip gary hugo ida))                                
%% > ((aldo beat) (carla david evi) (flip gary hugo ida) ...)  
%% ================================================================================
-spec group3(List) -> DeepList when
      List :: [term()],
      DeepList :: [term() | DeepList].

group3(L) when is_list(L) -> 
    %%
    %% Recursive walk to calculate the combinations,
    %% generates a golded collection of tuples, Invoke
    %% helper "unfold" procedure to unfold the tuples.
    %%
    Comb1 = fun(X) ->
	      Comb2 = fun(Y) ->
		        { Y, (L -- X) -- Y }
		      end,
	      { X, lists:map(Comb2, combinations(3, L -- X)) }
	    end,
    unfold(lists:map(Comb1, combinations(2, L)), []).

%%
%% Helper functions to "unfold" the tuples generated in the "group3" procedure above.
%%
-spec unfold(List, DeepList) -> DeepList when
      List :: [term()],
      DeepList :: [term() | DeepList].
		   
unfold([], Acc) -> Acc;
unfold([H|T], Acc) -> 
    A = element(1, H),
    B = element(2, H),
    unfold(T, Acc ++ unfold_iter(A, B, [])).

-spec unfold_iter(List, List, DeepList) -> DeepList when
      List :: [term()],
      DeepList :: [term() | DeepList].

unfold_iter(_, [], Acc) -> Acc;
unfold_iter(Prefix, [H|T], Acc) -> 
    unfold_iter(Prefix, T, Acc ++ [[Prefix, element(1,H), element(2,H)]]).
     
%% ========================================================================================
%% @doc
%% P27B (**) Group the elements of a set into disjoint subsets.
%%
%% (b) Generalize the above predicate in a way that we can specify a list of group sizes 
%% and the predicate will return a list of groups. 
%% @end
%%
%% Example B:
%% > (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
%% > (((aldo beat) (carla david) (evi flip gary huge ida)) ...)
%%
%% Note that we do not want permutations of the group members, i.e., ((aldo beat) ...) 
%% is the same solution as ((beta aldo) ...), however we make a difference between 
%% ((aldo beat) (carla david) ...) and((carla david) (aldo beat) ...).
%%
%% You may find more about this combinatorial problem in a good book on discrete 
%% mathematics under the term "multinomial coefficients".
%% ========================================================================================
%%
%% I'm going to go forward with the presumption that we'll be dividing the list
%% into precisely three groups, and all that changes from part (a) is that the 
%% number of elements in each group is now variable. 
%%
%% This means that we can essentially re-use (with only slight modifications) 
%% the solution that we derived above.
%%
%% If we were to require in addition that a variable number of subgroups, each 
%% of variable size, could be specified, we would have to go back and re-work 
%% this solution considerably.
%%
-spec group(List1, List2) -> DeepList when 
      List1 :: [term()],
      List2 :: [pos_integer()],
      DeepList :: [term() | DeepList].

group(L, [A, B, C]) when is_list(L), is_integer(A), is_integer(B), is_integer(C), 
			 A > 0, B > 0, C > 0, A + B + C =:= 9 ->
    Comb1 = fun(X) ->
                Comb2 = fun(Y) ->
                          { Y, (L--X) -- Y }
                        end,
	        { X, lists:map(Comb2, combinations(B, (L--X))) }
            end,
    unfold(lists:map(Comb1, combinations(A, L)), []).
		  
%% =============================================================================================
%% @doc
%% P28A (**) Sorting a list of lists according to length of sublists.
%%
%% (a) We suppose that a list contains elements that are lists themselves. The objective is to
%% sort the elements of this list according to their length. e.g., short lists first, longer 
%% lists later, or vice versa.
%% @end
%%
%% Example A:                                                                                                              
%% > (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))                       
%% > ((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))    
%% ============================================================================================  
%%
%% See discussion below for explanation of how this is implemented: 
%%
-spec lsort(List) -> List when
      List :: [term() | List].

lsort(L) when is_list(L) -> sort(L, fun sort_predicate/2).

%%
%% We need a "quick and dirty" sorting procedure to implement P28. We give a standard, 
%% textbook implementation of QuickSort below. Code of this style is frequently cited
%% as an example of the expressive power (or perhaps, of the expressive terseness) of 
%% Erlang. Note that it's NOT tail recursive, and hence should probably NOT be used in
%% a production setting:
%%
-spec sort(List, Predicate) -> List when
      List :: [term() | List],
      Predicate :: fun().

sort([], _Predicate) -> [];
sort([H|T], Predicate) -> 
    sort([X || X <- T, Predicate(X,H)], Predicate)
    ++ [H] ++
    sort([X || X <- T, not Predicate(X,H)], Predicate).
		    
%%
%% The "predicate" term above lets us define custom sort predicates/metrics for how
%% to arrange the elements of the target set.
%%
%% A standard, example predicate might look something like:
%%
%%  sort_predicate(A, B) -> A =< B.
%%
%% This will arrange the elements of the set in increasing order.
%%
%% We define the following predicate for sorting for P28:
%%
-spec sort_predicate(T, T) -> boolean() when
      T :: [term() | [term()]].

sort_predicate(A,B) when is_list(A), is_list(B) -> length(A) < length(B).

%%
%% We keep the ordering metric above "strictly less than" to preserve "in place" sorting here.
%%
      
%% ================================================================================================================
%% @doc
%% P28B (**) Sorting a list of lists according to length of sublists.
%%
%% (b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective 
%% is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting 
%% is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come length.
%% @end
%%
%% Example B:
%% > (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
%% > ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
%%
%% Note that in the above example, the first two lists in the result have length 4 and 1, both lengths appear just 
%% once. The third and fourth lists have length 3 which appears twice (there are two lists of this length). And 
%%finally, the last three lengths have length 2. This is  the most frequent length.
%% ================================================================================================================
%%
%% See discussion below for explanation of how this is implemented:
%%
-spec lfsort(List) -> List when
      List :: [term() | List].

lfsort(L) when is_list(L) ->
    Sort = make_sort_predicate(L),
    sort(L, Sort).

%%
%% Let's use an auxilliary data structure (i.e., an orddict) to 
%% keep track of the frequency of the length of each list item:
%%
-spec counter(List) -> [{Key,Val}] when 
      List :: [term() | List],
      Key :: term(),
      Val :: term().

counter(L) when is_list(L) -> counter(L, orddict:new()).
counter([], Acc) -> Acc;
counter([H|T], Acc) -> 
    Len = length(H),
    case orddict:is_key(Len, Acc) of
        true -> Value = orddict:fetch(Len, Acc);
        false -> Value = 0
    end,
    counter(T, orddict:store(Len, Value+1, Acc)).

%%
%% We need a sort predicate that utilizes the data structure we constructed above;
%% Let's do it using a closure:
%%    
-spec make_sort_predicate(List) -> fun() when
      List :: [term() | List].

make_sort_predicate(L) when is_list(L) ->
    Counter = counter(L),
    fun(A,B) when is_list(A), is_list(B) ->
        ValA = orddict:fetch(length(A), Counter),
	ValB = orddict:fetch(length(B), Counter),   
        ValA < ValB
    end.
    
%%
%% =======================
%%  PART II -- ARITHMETIC
%% =======================
%%

%%
%% =============== 
%%  DOCUMENTATION 
%% =============== 
%%

%% ==========================================
%% @doc
%% Used just to generate EDoc documentation.
%% @end
%% ========================================== 
document() ->
  edoc:run([],["q99.erl"],[]).
