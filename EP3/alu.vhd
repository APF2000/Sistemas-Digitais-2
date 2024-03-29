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

  signal sum, ignore : bit_vector(size-1 downto 0);
  signal ainvert, binvert : bit;
  signal op : bit_vector(1 downto 0);
  signal less : bit_vector(size downto 0);
  signal cin, cout : bit_vector(size-1 downto 0);
  signal auxF, auxSLT : bit_vector(size-1 downto 0);
  signal Alast, Blast : bit;
  signal zero, res : bit_vector(size-1 downto 0);

  begin
    ainvert <= S(3);
    binvert <= S(2);
    op(1) <= S(1);
    op(0) <= S(0);
    less(size) <= '0';

    GEN: for i in size-1 downto 0 generate

        LOWER_BIT: if i=0 generate
          U0: alu1bit port map
             (A(i), B(i), less(i), binvert,
              auxF(i), cin(i+1), sum(i), ignore(i),
              ainvert, binvert, op);
          auxSLT(i) <= sum(size-1);
        end generate LOWER_BIT;

        UPPER_BITS: if i>0 and i<size-1 generate
          U1: alu1bit port map
             (A(i), B(i), less(i), cin(i),
              auxF(i), cin(i+1), sum(i), ignore(i),
              ainvert, binvert, op);
          auxSLT(i) <= '0';
        end generate UPPER_BITS;

        HIGHER_BIT: if i=size-1 generate
          U2: alu1bit port map
             (A(i), B(i), less(i), cin(i),
              auxF(i), Co, sum(i), ignore(i),
              ainvert, binvert, op);
          auxSLT(i) <= '0';
        end generate HIGHER_BIT;
    end generate GEN;

    with op select
      res <= auxSLT when "11",
           auxF when others;
    F <= res;

    zero <= (others => '0');
    Z <= '1' when res = zero else
         '0';

    with Ainvert select
      Alast <= A(size-1) when '0',
              not A(size-1) when '1',
              '0' when others;
    with Binvert select
      Blast <= B(size-1) when '0',
              not B(size-1) when '1',
              '0' when others;

    Ov <= (sum(size-1) and (not Alast) and (not Blast))
      or ((not sum(size-1)) and Alast and Blast);

end architecture;
