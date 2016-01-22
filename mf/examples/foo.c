begin
    proc zero(val unused, res v) is
        v := 0;
    end
    x := 2;
    y := 2;
    bla := x == y;
    if bool(bla) then {
        call zero(x, y);
    } else {
        call zero(y, y);
    }
    z := y;
end