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

  begin
    with operation select
      Acorrect <= A     when '0',
                  (not A) when '1',
                  '0'   when others;

    --ALU1BIT: allu1bit port map();
    GEN: for i in size-1 downto 0 generate
        --generic map(size)
        LOWER_BIT: if i=0 generate
            U0: alu1bit port map
               (Acorrect(i), Bcorrect(i), less, '0',
                F(i), cin(i+1), ignore1(i), ignore2(i)
                ainvert, binvert, op);
          end generate LOWER_BIT;
        port map(A, B, less, );
    end generate GEN;
end architecture;
