encrypt n e m = m ^ e `mod` n
decrypt n d c = c ^ d `mod` n

encrypt1 m = encrypt 138689 13 m
decrypt1 c = decrypt 138689 95497 c
