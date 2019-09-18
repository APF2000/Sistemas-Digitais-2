library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;

entity regfile is
  generic(
    regn: natural := 32;
    wordSize: natural := 64
  );
  port(
    clock       : in bit;
    reset       : in bit;
    regWrite    : in bit;
    rr1, rr2, wr: in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
    d           : in  bit_vector(wordSize-1 downto 0);
    q1, q2      : out bit_vector(wordSize-1 downto 0)
  );
end regfile;

architecture arc of regfile is

  type index is array(regn-1 downto 0)
    of bit_vector(wordSize-1 downto 0);
  signal bancoReg: index;
  signal dontCare: index;

  component reg
    generic(wordSize: natural := 64);
    port(
      clock: in  bit;
      reset: in  bit;
      load:  in  bit;
      d:     in  bit_vector(wordSize-1 downto 0);
      q:     out bit_vector(wordSize-1 downto 0)
    );
  end component;
  begin
    GEN:
    for i in regn-1 downto 0 generate
        REGX : reg
        generic map(wordSize)
        port map(clock, reset, regWrite, bancoReg(i), dontCare(i));
    end generate;

    --bancoReg(regn-1) <= (others => '0');
    q1 <= bancoReg(to_integer(unsigned(rr1)));
    q2 <= bancoReg(to_integer(unsigned(rr2)));

    PRO: process(clock) is begin

      if reset = '1' then
        for i in 0 to regn-1 loop
          bancoReg(i) <= (others => '0');
        end loop;
      end if;

      if regWrite = '1' and clock'event and rising_edge(clock)
        and to_integer(unsigned(wr)) /= regn-1 then
          -- um registrador recebe a entrada
          bancoReg(to_integer(unsigned(wr))) <= d;
      end if;
    end process;

end arc;
