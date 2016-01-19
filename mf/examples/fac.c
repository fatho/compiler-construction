begin
    proc factorial(val x, res y) is
        if (x == 0)
            y := 1;
        else if (x == 1)
            y := 1;
        else
        {
            call factorial(x - 1, y);
            y := x * y;
        }
    end
        
    call factorial(2, r);
    call factorial(r, s);
    call factorial(s, t);
    call factorial(t, u);
    call factorial(u, v);
    v := 20;
end