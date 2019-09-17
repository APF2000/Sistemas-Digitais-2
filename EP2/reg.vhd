library ieee;
use ieee.numeric_bit.all;

entity reg is
  generic(wordSize: natural := 4);
  port(
    clock: in  bit;
    reset:  in  bit;
    load:  in  bit;
    d:     in  bit_vector(wordSize-1 downto 0);
    q:     out bit_vector(wordSize-1 downto 0)
  );
end reg;

architecture arc of reg is

  signal q_n: bit_vector(wordSize-1 downto 0);

  begin

  flipFlop: process(clock, reset) is
    begin

      if reset = '1' then
        q <= (others => '0');
        q_n <= (others => '1');
      elsif clock = '1' and clock'event then
        if load = '1' then
          q <= d;
          q_n <= not d;
        end if;
      end if;
  end process;

end arc;
