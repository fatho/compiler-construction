begin
    proc fib(val z, u, res v) is
        if z < 3 then
            v := 1;
        else {
            call fib(z-2, 0, u);
            call fib(z-1, 0, v);
            v := v + u;
        }
    end
    call fib(x, 0, y);
end