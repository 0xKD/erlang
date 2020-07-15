-module(ex3).
-export([print_bill/1]).

% A supermarket billing system will take a sequence of barcodes such as
% [1234,4719,3814,1112,1113,1234]
% into a printed bill of the form
%
%          Erlang Stores
%
% Dry Sherry, 1lt...........5.40
% Fish Fingers..............1.21
% Orange Jelly..............0.56
% Hula Hoops (Giant)........1.33
% Unknown Item..............0.00
% Dry Sherry, 1lt...........5.40
%
% Total....................13.90
%
% using the data in a simple database such as
%
%  [  (4719, "Fish Fingers" , 121),
%     (5643, "Nappies" , 1010),
%     (3814, "Orange Jelly", 56),
%     (1111, "Hula Hoops", 21),
%     (1112, "Hula Hoops (Giant)", 133),


db() ->
    [{4719, "Fish Fingers" , 121},
     {5643, "Nappies" , 1010},
     {3814, "Orange Jelly", 56},
     {1111, "Hula Hoops", 21},
     {1112, "Hula Hoops (Giant)", 133},
     {1234, "Dry Sherry, 1lt", 540}].

maxlength() -> 30.

print_bill(Codes) ->
    MAXLEN=maxlength(),
    print_header("Erlang Stores", MAXLEN),
    print_empty(),
    Total=print_items(Codes, MAXLEN),
    print_empty(),
    print_item("Total", Total, MAXLEN).


print_header(Text, LineLength) ->
    io:fwrite(get_centered_text(Text, LineLength)++"\n", []).

get_centered_text(Text, LineLength) ->
    Extra=max(LineLength - length(Text), 0),
    Half=Extra div 2,Rem=Extra rem 2,
    get_padding($\s,Half)++Text++get_padding($\s,Half+Rem).

print_empty() ->
    io:fwrite("\n").

% This is probably the hackiest part, where a "print_items" func
% is also returning the price. It is convenient though!
print_items([], _) ->
    0;
print_items([Code|Codes], LineLength) ->
    {Name,Price}=find_item(Code, db()),
    print_item(Name, Price, LineLength),
    Price+print_items(Codes, LineLength).

% This is highly inefficient but given we're dealing with a small
% dataset (embedded in code), it is fine.
find_item(_, []) ->
    {"Unknown", 0};
find_item(Code, [{Code, Name, Price}|_]) ->
    {Name,Price};
find_item(Code, [{_Code, _, _}|Items]) ->
    find_item(Code, Items).

print_item(Name, Price, LineLength) ->
    FormattedPrice=get_formatted_price(Price),
    PaddingLength=LineLength-length(Name)-length(FormattedPrice),
    Line=Name++get_padding($., PaddingLength)++FormattedPrice,
    io:fwrite(Line++"\n", []).

get_formatted_price(Price) ->
    erlang:float_to_list(Price / 100, [{decimals, 2}]).

get_padding(_, Repeat)
  when Repeat =< 0 ->
    [];
get_padding(Char, Repeat) ->
    [Char|get_padding(Char, Repeat-1)].
