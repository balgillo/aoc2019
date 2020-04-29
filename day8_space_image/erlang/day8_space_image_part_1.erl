-module(day8_space_image_part_1).

-export([image_checksum/3]).

-record(histogram, {
    zeroes,
    ones,
    twos}).


image_checksum(PixelsWide, PixelsTall, FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    ImageLayers = try load_image_layers(Device, PixelsWide, PixelsTall)
      after file:close(Device)
    end,
    calc_image_checksum(ImageLayers).


calc_image_checksum(Layers) ->
    #histogram{ones = Ones, twos = Twos} = calc_histogram_for_layer_with_least_zeroes(Layers),
    Ones * Twos.


calc_histogram_for_layer_with_least_zeroes([L]) ->
    calc_layer_histogram(L, histogram_zero());
calc_histogram_for_layer_with_least_zeroes([L1|OtherLayers]) ->
    L1Histogram = calc_layer_histogram(L1, histogram_zero()),
    L2Histogram = calc_histogram_for_layer_with_least_zeroes(OtherLayers),
    if 
        L1Histogram#histogram.zeroes < L2Histogram#histogram.zeroes ->
            L1Histogram;
        true ->
            L2Histogram
    end.


calc_layer_histogram([Row|Rest], TotalsSoFar) ->
    calc_layer_histogram(Rest, calc_row_histogram(Row, TotalsSoFar));
calc_layer_histogram([], TotalsSoFar) ->
    TotalsSoFar.


calc_row_histogram([$0|Rest], TotalsSoFar) ->
    NewTotals = TotalsSoFar#histogram{zeroes = 1 + TotalsSoFar#histogram.zeroes},
    calc_row_histogram(Rest, NewTotals);
calc_row_histogram([$1|Rest], TotalsSoFar) ->
    NewTotals = TotalsSoFar#histogram{ones = 1 + TotalsSoFar#histogram.ones},
    calc_row_histogram(Rest, NewTotals);
calc_row_histogram([$2|Rest], TotalsSoFar) ->
    NewTotals = TotalsSoFar#histogram{twos = 1 + TotalsSoFar#histogram.twos},
    calc_row_histogram(Rest, NewTotals);
calc_row_histogram([_|Rest], TotalsSoFar) ->
    calc_row_histogram(Rest, TotalsSoFar);
calc_row_histogram([], TotalsSoFar) ->
    TotalsSoFar.


histogram_zero() ->
    #histogram{zeroes = 0, ones = 0, twos = 0}.


load_image_layers(Device, PixelsWide, PixelsTall) ->
    case io:get_line(Device, "") of
        eof  -> 
            [];
        Line -> 
            parse_line(Line, PixelsWide, PixelsTall)
    end.


parse_line("\r", _, _) ->
    [];
parse_line("\n", _, _) ->
    [];
parse_line([], _, _) ->
    [];
parse_line(L, PixelsWide, PixelsTall) ->
    PixelsPerLayer = PixelsWide * PixelsTall,
    [parse_layer(lists:sublist(L, PixelsPerLayer), PixelsWide, PixelsTall)] ++
        parse_line(lists:nthtail(PixelsPerLayer, L), PixelsWide, PixelsTall).


parse_layer(_, _, 0) ->
    [];
parse_layer(L, PixelsWide, RowsRemaining) ->
    [parse_row(lists:sublist(L, PixelsWide))] ++ 
    parse_layer(lists:nthtail(PixelsWide, L), PixelsWide, RowsRemaining - 1).


parse_row(L) ->
    L.
