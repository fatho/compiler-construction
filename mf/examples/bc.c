begin
  y := 2; 
  z := 1;
  while x > 0 do {
    z := z * y;
    if z >= 16 then {
      break;
    } else {
      if z >= 8 then {
        continue;
      }
    }
    x := x - 1;
  }
end