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
  --type banco is array(regn downto 0)
  --  of bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
  --signal bancoReg : banco;
  type index is array(regn-1 downto 0)
    of bit_vector(regn-1 downto 0);
  signal indexVec: index;

  component reg
    generic(wordSize: natural := 64);
    port(
      clk:      in  bit;
      load:     in  bit;
      entrada:  in  bit_vector(wordSize-1 downto 0);
      index:    out bit_vector(regn-1 downto 0)
    );
  end component;
  begin
    GEN:
    for i in regn downto 0 generate
        REGX : reg port map
          (clock, regWrite, d, indexVec(i));
        --indexVec(i) <= ;
    end generate;

    q1 <= indexVec(to_integer(unsigned(rr1)));
    q2 <= indexVec(to_integer(unsigned(rr2)));

    PRO: process(reset) is begin
      if reset = '1' then
        for i in 0 to regn loop
          indexVec(i) <= (others => '0');
        end loop;
      end if;

      if regWrite = '1' and clock'event then
        indexVec(to_integer(unsigned(wr))) <= d;
      end if;
    end process;

end arc;
