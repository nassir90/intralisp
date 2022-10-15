entity Adder3Bit_TB is
end 

...

% dotimes (i 8)
(dotimes (j 8) @
wait until falling_edge(CLK);
A_TB <= %=~ "~3,'0b" i%;
B_TB <= %=~ "~3,'0b" j%;
@)%