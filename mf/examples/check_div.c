begin
    x := 100;
    y := 2;
    
    z := x / y;
    
    counter := 0;
    xres := 0;
    while xres < x do
    {
        xres := xres + 2;
        counter := xres + 1;
    }
    
    result := 0;
    if counter == z then
        result := 1000;
    else
        result := 0;

end