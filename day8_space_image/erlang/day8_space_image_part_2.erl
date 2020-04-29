-module(day8_space_image_part_2).

-export([render_image/3]).


render_image(PixelsWide, PixelsTall, FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    [TopLayer|OtherLayers] = try load_image_layers(Device, PixelsWide, PixelsTall)
      after file:close(Device)
    end,
    Image = render_image_layers(OtherLayers, TopLayer),
    print_image(Image).


print_image([TopRow|OtherRows]) ->
    io:fwrite("~s~n", [format_row(TopRow)]),
    print_image(OtherRows);
print_image([]) ->
    ok.


format_row([$1|Rest]) ->
    "*" ++ format_row(Rest);
format_row([_|Rest]) ->
    " " ++ format_row(Rest);
format_row([]) ->
    [].


render_image_layers([TopLayer|Rest], Image) ->
    render_image_layers(Rest, render_layer_under(TopLayer, Image));
render_image_layers([], Image) ->
    Image.


render_layer_under([LayerTopRow|LayerOtherRows], [ImageTopRow|ImageOtherRows]) ->
    [render_row_under(LayerTopRow, ImageTopRow)] ++ render_layer_under(LayerOtherRows, ImageOtherRows);
render_layer_under([], []) ->
    [].


render_row_under([LayerLeft|LayerRest], [$2|ImageRest]) ->
    [LayerLeft|render_row_under(LayerRest, ImageRest)];
render_row_under([_|LayerRest], [ImageLeft|ImageRest]) ->
    [ImageLeft|render_row_under(LayerRest, ImageRest)];
render_row_under([], []) ->
    [].


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
