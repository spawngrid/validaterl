-module(validaterl).
-export([validate/1, validate/2]).
-include_lib("validaterl/include/validaterl.hrl").
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(FAILED(X), X).

-type spec() :: #numericality{} | #range{} | #format{}.
-type name() :: any().
-type report() :: any().
-type plan() :: {name(), any(), spec()}.
-type error() :: {name(), any(), spec(), report()}.

-spec validate(plan()) -> true | list(error()).
validate(Plan) ->
    lists:filter(fun({_, _, _, true}) ->
                         false;
                    (_) ->
                         true
                 end,
                 [ {Name, Value, Spec, validate(Value, Spec)} ||
                     {Name, Value, Spec} <- Plan ]).
    

%% numericality
validate(undefined, #numericality{
           allow_undefined = false
          }) ->
    ?FAILED(undefined_not_allowed);
validate(undefined, #numericality{
           allow_undefined = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(null, #numericality{
           allow_null = false
          }) ->
    ?FAILED(null_not_allowed);
validate(null, #numericality{
           allow_null = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(Empty, #numericality{
           allow_string = true,
           allow_empty = false
           }) when Empty == [] orelse Empty == <<>> ->
    ?FAILED(empty_not_allowed);
validate(Empty, #numericality{
           allow_string = true,
           allow_empty = true,
           default = Default
           } = V) when Empty == [] orelse Empty == <<>> ->
    validate(Default, V);
validate(S, #numericality{
           allow_string = false
          }) when is_list(S) orelse is_binary(S) ->
    ?FAILED(string_not_allowed);
validate(S, #numericality{
           allow_string = true,
           allow_rest = Rest
          } = V) when is_list(S) orelse is_binary(S) ->
    Str = to_string(S),
    case string:to_integer(Str) of
        {error, no_integer} ->
            ?FAILED(number_expected);
        {Value, []} ->
           validate(Value, V);
        {Value, _} ->
            case string:to_float(Str) of
                {error, _Error} ->
                    if Rest ->
                            validate(Value, V);
                       true ->
                            ?FAILED(rest_not_allowed)
                    end;
                {ValueF, []} ->
                    validate(ValueF, V);
                {ValueF, _} ->
                    if Rest ->
                            validate(ValueF, V);
                       true ->
                            ?FAILED(rest_not_allowed)
                    end
            end
    end;
    validate(V, #numericality{ allow_float = false }) when is_float(V) ->
    ?FAILED(float_not_allowed);
validate(V, #numericality{}) when is_integer(V) orelse is_float(V) ->
    true;
validate(_, #numericality{}) ->
    ?FAILED(number_expected);

validate(_, #range{ from = undefined,
                    to = undefined }) ->
    true;
validate(V, #range{ from = From,
                    to = undefined,
                    exclusive = false}) when From /= undefined andalso
                                             V < From ->
    ?FAILED(lesser);
validate(V, #range{ from = From,
                    to = undefined,
                    exclusive = true}) when From /= undefined andalso 
                                            V =< From ->
    ?FAILED(lesser);
validate(V, #range{ from = undefined,
                    to = To,
                    exclusive = false}) when To /= undefined andalso
                                             V > To ->
    ?FAILED(greater);
validate(V, #range{ from = undefined,
                    to = To,
                    exclusive = true}) when To /= undefined andalso
                                            V >= To ->
    ?FAILED(greater);
validate(V, #range{ from = _From,
                    to = To,
                    exclusive = false}) when To /= undefined andalso
                                             V > To ->
    ?FAILED(greater);
validate(V, #range{ from = From,
                    to = _To,
                    exclusive = false}) when From /= undefined andalso
                                             V < From ->
    ?FAILED(lesser);
validate(V, #range{ from = From,
                    to = _To,
                    exclusive = true}) when From /= undefined andalso
                                             V =< From ->
    ?FAILED(lesser);
validate(V, #range{ from = _From,
                    to = To,
                    exclusive = false}) when To /= undefined andalso 
                                             V > To ->
    ?FAILED(greater);
validate(V, #range{ from = _From,
                    to = To,
                    exclusive = true}) when To /= undefined andalso
                                            V >= To ->
    ?FAILED(greater);
validate(_, #range{}) ->
    true;

validate(undefined, #format{
           allow_undefined = false
          }) ->
    ?FAILED(undefined_not_allowed);
validate(undefined, #format{
           allow_undefined = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(null, #format{
           allow_null = false
          }) ->
    ?FAILED(null_not_allowed);
validate(null, #format{
           allow_null = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(Empty, #format{
           allow_empty = false
           }) when Empty == [] orelse Empty == <<>> ->
    ?FAILED(empty_not_allowed);
validate(Str, #format{
           re = Re
          }) when is_list(Str) orelse is_binary(Str) ->
    case re:run(Str, Re) of
        nomatch ->
            ?FAILED(no_match);
        {match, _} ->
            true;
        {error, Error} ->
            ?FAILED(Error)
    end;
validate(_, #format{}) ->
    ?FAILED(string_expected);

validate(L, #length{ is = Validator }) when is_list(L) ->
    validate(length(L), Validator);

validate(L, #length{ is = Validator }) when is_binary(L) orelse is_tuple(L) ->
    validate(size(L), Validator);

validate(L, #length{ is = Validator }) ->
    validate(0, Validator);

validate(A, #type{ is = number }) when is_number(A) ->
    true;
validate(A, #type{ is = boolean }) when A == true orelse A == false ->
    true;
validate(A, #type{ is = atom }) when is_atom(A) ->
    true;
validate(A, #type{ is = binary }) when is_binary(A) ->
    true;
validate(A, #type{ is = reference }) when is_reference(A) ->
    true;
validate(A, #type{ is = function }) when is_function(A) ->
    true;
validate(A, #type{ is = port }) when is_port(A) ->
    true;
validate(A, #type{ is = pid }) when is_pid(A) ->
    true;
validate(A, #type{ is = tuple }) when is_tuple(A) ->
    true;
validate(A, #type{ is = list }) when is_list(A) ->
    true;
validate(A, #type{}) ->
    false;

validate(A, A) -> %% equality validator
    true;

%% custom validators, the convention is that the first
%% record element (2nd tuple element) has to be a reference
%% to a validation fun
validate(V, T) when is_tuple(T) andalso size(T) > 1 andalso is_function(element(2, T)) ->
    (element(2, T))(V, T);
                   

validate(A, B) when A < B ->
    ?FAILED(lesser);
validate(A,B) when A > B ->
    ?FAILED(greater).



to_string(S) when is_list(S) ->
    S;
to_string(S) when is_binary(S) ->
    binary_to_list(S).


-ifdef(TEST).

numericality_test_() ->
    [fun numericality_test_undefined/0,
     fun numericality_test_null/0,
     fun numericality_test_empty/0,
     fun numericality_test_string/0,
     fun numericality_test_allow_float/0,
     fun numericality_test_default/0
    ].

numericality_test_undefined() ->
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #numericality{})),
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #numericality{ allow_undefined = false })),
    ?assert(validate(undefined, #numericality{ allow_undefined = true })).

numericality_test_null() ->
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #numericality{})),
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #numericality{ allow_null = false })),
    ?assert(validate(null, #numericality{ allow_null = true })).

numericality_test_empty() ->
    ?assertEqual(?FAILED(string_not_allowed), validate("", #numericality{ allow_empty = true })),
    ?assertEqual(?FAILED(string_not_allowed), validate(<<>>, #numericality{ allow_empty = true })),
    ?assertEqual(true, validate("", #numericality{ allow_empty = true, allow_string = true })),
    ?assertEqual(true, validate(<<>>, #numericality{ allow_empty = true, allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #numericality{ allow_empty = false, allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #numericality{ allow_empty = false, allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #numericality{ allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #numericality{  allow_string = true })).
    
numericality_test_string() ->
    ?assertEqual(true, validate("1", #numericality{ allow_string = true })),
    ?assertEqual(true, validate(<<"1">>, #numericality{ allow_string = true })),
    ?assertEqual(true, validate("1a", #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(true, validate(<<"1a">>, #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(true, validate("1.1", #numericality{ allow_string = true })),
    ?assertEqual(true, validate(<<"1.1">>, #numericality{ allow_string = true })),
    ?assertEqual(true, validate("1.1a", #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(true, validate(<<"1.1a">>, #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(?FAILED(number_expected), validate("garbage", #numericality{ allow_string = true })),
    ?assertEqual(?FAILED(number_expected), validate(<<"garbage">>, #numericality{ allow_string = true })).

numericality_test_allow_float() ->
    ?assertEqual(true, validate(1.1, #numericality{})),
    ?assertEqual(?FAILED(float_not_allowed), validate(1.1, #numericality{ allow_float = false})).

numericality_test_default() ->
    ?assertEqual(true, validate(undefined, #numericality{ allow_undefined = true })),
    ?assertEqual(true, validate(null, #numericality{ allow_null = true })),
    ?assertEqual(true, validate("", #numericality{ allow_string = true, allow_empty = true })),
    ?assertEqual(true, validate(<<>>, #numericality{ allow_string = true, allow_empty = true })),

    ?assertEqual(true, validate(undefined, #numericality{ allow_undefined = true, default = 1 })),
    ?assertEqual(true, validate(null, #numericality{ allow_null = true, default = 1 })),
    ?assertEqual(true, validate("", #numericality{ allow_string = true, allow_empty = true, default = 1 })),
    ?assertEqual(true, validate(<<>>, #numericality{ allow_string = true, allow_empty = true, default = 1 })),

    ?assertEqual(?FAILED(number_expected), validate(undefined, #numericality{ allow_undefined = true, default = x })),
    ?assertEqual(?FAILED(number_expected), validate(null, #numericality{ allow_null = true, default = x })),
    ?assertEqual(?FAILED(number_expected), validate("", #numericality{ allow_string = true, allow_empty = true, default = x })),
    ?assertEqual(?FAILED(number_expected), validate(<<>>, #numericality{ allow_string = true, allow_empty = true, default = x })).

qc(T) ->
    ?assertEqual(true, proper:quickcheck(T,
                                         [
                                          {numtests, 1000},
                                          {on_output, fun(String, Format) ->
                                                              io:format(user, String, Format)
                                                      end}])).
                                                                  
                                                                  
range_test_() ->
    [{"include",
      fun() -> qc(range_test_inclusive_prop()) end},
     {"exclude",
      fun() -> qc(range_test_exclusive_prop()) end}].


range() ->
    ?SUCHTHAT({from, F, to, T},
              {from, oneof([undefined, integer()]), 
               to, oneof([undefined, integer()])},
              if F == undefined ->
                      true;
                 T == undefined ->
                      true;
                 true ->
                      F =< T
              end).

range_test_inclusive_prop() ->
    ?FORALL({{from, From, to, To}, Random}, {range(),
                                             integer()},
            begin
                Validate = validate(Random, #range{ from = From,
                                                    to = To }),
                if  From /= undefined andalso Random < From ->
                        Validate == ?FAILED(lesser);
                    To /= undefined andalso Random > To ->
                        Validate == ?FAILED(greater);
                    From /= undefined andalso To /= undefined 
                    andalso Random >= From andalso Random =< To ->
                        Validate == true;
                    From == undefined andalso To /= undefined 
                    andalso Random =< To ->
                        Validate == true;
                    To == undefined andalso From /= undefined 
                    andalso Random >= From ->
                        Validate == true;
                    From == undefined andalso To == undefined ->
                        Validate == true;
                    Random < From ->
                        Validate == ?FAILED(lesser);
                    Random > To ->
                        Validate == ?FAILED(greater);
                    true ->
                        false
                end
            end).

range_test_exclusive_prop() ->
    ?FORALL({{from, From, to, To}, Random}, {range(),
                                             integer()},
            begin
                Validate = validate(Random, #range{ from = From,
                                                    to = To,
                                                    exclusive = true}),
                if 
                    From /= undefined andalso Random =< From
                    andalso From == To ->
                        (Validate == ?FAILED(lesser)) orelse
                        (Validate == ?FAILED(greater));
                    From /= undefined andalso Random =< From ->
                        Validate == ?FAILED(lesser);
                    To /= undefined andalso Random >= To
                    andalso From == To ->
                        (Validate == ?FAILED(lesser)) orelse
                        (Validate == ?FAILED(greater));
                    To /= undefined andalso Random >= To ->
                        Validate == ?FAILED(greater);
                    From /= undefined andalso To /= undefined 
                    andalso Random > From andalso Random < To ->
                        Validate == true;
                    From == undefined andalso To /= undefined 
                    andalso Random < To ->
                        Validate == true;
                    To == undefined andalso From /= undefined 
                    andalso Random > From ->
                        Validate == true;
                    From == undefined andalso To == undefined ->
                        Validate == true;
                    Random =< From ->
                        Validate == ?FAILED(lesser);
                    Random >= To ->
                        Validate == ?FAILED(greater);
                    true ->
                        false
                end
            end).                                 

format_test_() ->
    [fun format_test_undefined/0,
     fun format_test_null/0,
     fun format_test_empty/0,
     fun format_test_string/0,
     fun format_test_default/0
    ].

format_test_undefined() ->
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #format{})),
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #format{ allow_undefined = false })),
    ?assert(validate(undefined, #format{ allow_undefined = true, allow_empty = true })).

format_test_null() ->
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #format{})),
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #format{ allow_null = false })),
    ?assert(validate(null, #format{ allow_null = true, allow_empty = true })).

format_test_empty() ->
    ?assertEqual(true, validate("", #format{ allow_empty = true })),
    ?assertEqual(true, validate(<<>>, #format{ allow_empty = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #format{ allow_empty = false })),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #format{ allow_empty = false })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #format{})),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #format{})).
    
format_test_string() ->
    ?assertEqual(true, validate("1", #format{ re = "^1$" })),
    ?assertEqual(?FAILED(no_match), validate("2", #format{ re = "^1$" })).

format_test_default() ->
    ?assertEqual(true, validate(undefined, #format{ allow_undefined = true, allow_empty = true })),
    ?assertEqual(true, validate(null, #format{ allow_null = true, allow_empty = true })),
    ?assertEqual(true, validate("", #format{ allow_empty = true })),
    ?assertEqual(true, validate(<<>>, #format{allow_empty = true })),

    ?assertEqual(?FAILED(string_expected), validate(undefined, #format{ allow_undefined = true, default = x })),
    ?assertEqual(?FAILED(string_expected), validate(null, #format{ allow_null = true, default = x })).


custom_validator_test() ->
    Validator = fun(_, _) ->
                        true
                end,
    ?assert(validate(x,{my_validator, Validator})).

equality_test() ->
    ?assert(validate(1,1)),
    ?assertEqual(?FAILED(lesser), validate(1,2)).

comparison_test() ->
    ?assertEqual(?FAILED(lesser), validate(1, 2)),
    ?assertEqual(?FAILED(greater), validate(2, 1)).

length_test() ->
    ?assert(validate(atom, #length{ is = 0})),
    Tests = [{"a", [{1, true},{2, lesser}, {0, greater}, {#range{ from = 0, to = 2 }, true}]},
             {<<"a">>, [{1, true},{2, lesser}, {0, greater}, {#range{ from = 0, to = 2}, true}]},
             {{a}, [{1, true},{2, lesser}, {0, greater}], {#range{ from = 0, to = 2}, true}}],
    
    [ 
      begin
          ?assertEqual(?FAILED(Outcome), validate(Data, #length{ is = Is }))
      end || {Data, Cases} <- Tests,
             {Is, Outcome} <- Cases ].
       
type_test() ->       
    ?assert(validate(1, #type{ is = number })),
    ?assert(validate(1.1, #type{ is = number })),
    ?assert(validate(atom, #type{ is = atom })),
    ?assert(validate(<<>>, #type{ is = binary })),
    ?assert(validate(make_ref(), #type{ is = reference })),
    ?assert(validate(fun type_test/0, #type{ is = function })),
    ?assert(validate(hd(erlang:ports()), #type{ is = port })),
    ?assert(validate(self(), #type{ is = pid })),
    ?assert(validate({}, #type{ is = tuple })),
    ?assert(validate([], #type{ is = list })),
    ?assert(validate(true, #type{ is = boolean })),
    ?assert(validate(false, #type{ is = boolean })).
    
    
    

-endif.
