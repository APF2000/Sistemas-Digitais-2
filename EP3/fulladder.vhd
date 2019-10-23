library ieee;
use ieee.numeric_bit.all;

entity fulladder is
  port(
    a, b, cin : in  bit;
    s, cout   : out bit
  );
end entity;

architecture arc of fulladder is
  begin
    s <= a xor b xor cin;
    cout <= (cin and (a or b)) or (a and b);
end arc;
