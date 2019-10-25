library ieee;
use ieee.numeric_bit.all;

entity alu is
  generic (
    size: natural := 10 --bit size
  );
  port(
    A, B : in bit_vector(size-1 downto 0); --inputs
    F    : out bit_vector(size-1 downto 0); --output
    S    : in bit_vector(3 downto 0); --op selection
    Z    : out bit; --zero flag
    Ov   : out bit; --overflow flag
    Co   : out bit --carry out
  );
end entity alu;

architecture arcalu of alu is
  component alu1bit
    port (
      a, b, less, cin             : in  bit;
      result, cout, set, overflow : out bit;
      ainvert, binvert            : in  bit;
      operation                   : in  bit_vector(1 downto 0)
    );
  end component;

  signal ignore1, ignore2 : bit_vector(size-1 downto 0);
  signal ainvert, binvert : bit;
  signal op : bit_vector(1 downto 0);
  signal less : bit_vector(size downto 0);
  signal cin, cout : bit_vector(size-1 downto 0);
  signal aux, auxless, auxF, auxSLT : bit_vector(size-1 downto 0);
  signal firstBit : bit_vector(size-1 downto 0);

  begin
    ainvert <= S(3);
    binvert <= S(2);
    op(1) <= S(1);
    op(0) <= S(0);
    less(size) <= '0';

    GEN: for i in size-1 downto 0 generate
        aux(i) <= (not A(i)) and B(i);
        auxless(i) <= aux(i) or less(i+1);

        LOWER_BIT: if i=0 generate
          U0: alu1bit port map
             (A(i), B(i), auxless(i), '0',
              auxF(i), cin(i+1), ignore1(i), ignore2(i),
              ainvert, binvert, op);
          firstBit(i) <= '1';
        end generate LOWER_BIT;

        UPPER_BITS: if i>0 and i<size-1 generate
          U1: alu1bit port map
             (A(i), B(i), auxless(i), cout(i-1),
              auxF(i), cin(i+1), ignore1(i), ignore2(i),
              ainvert, binvert, op);
          firstBit(i) <= '0';
        end generate UPPER_BITS;

        HIGHER_BIT: if i=size-1 generate
          U2: alu1bit port map
             (A(i), B(i), auxless(i), cout(i-1),
              auxF(i), Co, ignore1(i), Ov,
              ainvert, binvert, op);
          firstBit(i) <= '0';
        end generate HIGHER_BIT;

        auxSLT(i) <= auxless(0) and firstBit(i);
    end generate GEN;

    with op select
      F <=
           auxSLT when "11",
           auxF when others;

    Z <= '0';
end architecture;
