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
  signal s : bit;

  begin
    ADDER : fulladder port map(Acorrect, Bcorrect, cin, s, co);

    with ainvert select
      Acorrect <= a     when '0',
                  (not a) when '1',
                  '0'   when others;

    with binvert select
      Bcorrect <= b     when '0',
                  (not b) when '1',
                  '0'   when others;

    with operation select
      result <= (Acorrect and Bcorrect) when ("00"),
                (Acorrect or Bcorrect)  when ("01"),
                s                       when ("10"),
                less                    when ("11"),
                '0'                     when others;

    cout <= co;
    set <= s;
    overflow <= cin xor co;
end arcalu;
