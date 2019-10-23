library ieee;
use ieee.numeric_bit.all;

entity alu1bit is
  port (
    a, b, less, cin             : in  bit;
    result, cout, set, overflow : out bit;
    ainvert, binvert            : in  bit;
    operation                   : in  bit_vector(1 downto 0)
  );
end entity;

architecture arcalu of alu1bit is
  component fulladder
    port(
      a, b, cin : in  bit;
      s, cout   : out bit
    );
  end component;

  signal co : bit;
  signal Acorrect, Bcorrect : bit;

  begin
    ADDER : fulladder port map(a, b, cin, set, co);

    Acorrect <= a when ainvert = '0'
                (not a) when ainvert = '1'
                '0';
    Bcorrect <= b when binvert = '0'
                not b when binvert = '1'
                '0';

    result <= (Acorrect and Bcorrect) when (operation = "00")
              (Acorrect or Bcorrect)  when (operation = "01")
              s                       when (operation = "10")
              less                    when (operation = "11")
              '0';
    cout <= co;
    set <= s;
    overflow <= cin xor co;
end arcalu;
