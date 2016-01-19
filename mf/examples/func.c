begin
    proc func1(val z, res v) is
        z := z - 20;
        while (z < 10) 
            v := v + 20;
        
        if (v > 100)
            v := 100;
        else
            v := 100;
    end
    
    proc func2(val a, res b) is 
        b := a * 20;
        if (b > 100)
            b := 20;
        else
            b := 30;
    end
    
    call func1(10, y);
    call func2(y, r);
    r := 10;
    
end