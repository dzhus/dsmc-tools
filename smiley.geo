solid head = sphere (0, 0, 0; 10);
solid cut = plane (1, 0, 0; 1, 0, 0) and plane (-1, 0, 0; -1, 0, 0);


solid eyeballs = sphere (0, -4.2, 3; 2) or sphere (0, 4.2, 3; 2);
solid eyecuts = not eyeballs;


solid smile = cylinder (-2, 0, 0; 2, 0, 0; 5) and not cylinder (-3, 0, 2; 3, 0, 2; 5);

solid smilecut = head and cut and eyecuts;
solid whole = smilecut and not smile;

tlo whole;
