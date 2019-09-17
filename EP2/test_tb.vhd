library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.all;

entity test_tb is end;

architecture dut of test_tb is
  constant wordSize: integer := 4;
  component reg is
    generic(wordSize: natural := 4);
    port(
      clock: in  bit;
      reset: in  bit;
      load:  in  bit;
      d:     in  bit_vector(wordSize-1 downto 0);
      q:     out bit_vector(wordSize-1 downto 0)
    );
  end component;
  signal clk, res, en: bit :='0';
  signal entrada, saida: bit_vector(wordSize-1 downto 0);
  signal saidai: integer range 0 to wordSize-1;
  constant periodoClock : time := 1 ns;

begin
  clk <= not clk after periodoClock/2;
  saidai <= to_integer(unsigned(saida));

  dut: reg
    generic map(wordSize)
    port map(clk,res,en,entrada,saida);

  st: process is
  begin
    --! Imprime mensagem de inicio de teste
    assert false report "BOT" severity note;

    --! Testa se o clear está OK
    en<='1';
    wait until rising_edge(clk);
    entrada <= "1010";
    saida <= "1010";
    wait until falling_edge(clk);
    assert saidai=10 report "Teste de clear falhou." &
      " Obtido: " & integer'image(saidai)
      severity failure;

    --! Testa se a contagem crescente está OK
    en<='1';

    assert false report "EOT" severity note;
    wait;
  end process;
end dut;
