begin
    proc func1(val z, res v) is
        z := z - 20;
        while z < 10 do
            v := v + 20;
        
        if v > 100 then
            v := 100;
        else
            v := 100;
    end
    
    proc func2(val a, res b) is 
        b := a * 20;
        if b > 100 then
            b := 20;
        else
            b := 30;
    end
    
    call func1(10, y);
    call func2(y, r);
    r := 10;
    
end