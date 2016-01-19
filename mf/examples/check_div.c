begin
    x := 100;
    y := 2;
    
    z := x / y;
    
    counter := 0;
    xres := 0;
    while (xres < x)
    {
        xres := xres + 2;
        counter := xres + 1;
    }
    
    result := 0;
    if (counter == z)
        result := 1000;
    else
        result := 0;
    
    result := 20;
end