begin
    x := 0;
    y := 1;
    z := 20;
    r := 0;
    
    while x < z do
    {
        x := x + 1;
        r := x;     
    }
    
    if (r > z) then
        r := y;
    else 
    {
        r := 0; 
    }        
end