solid cap = sphere (0, 0, -0.8; 3.1);
solid body = cone (0, 0, -5; 5; 0, 0, 0; 3);
solid bottom = sphere (0, 0, 0; 5); 

solid upper = cap or body;
solid whole = upper and bottom;

tlo whole;
