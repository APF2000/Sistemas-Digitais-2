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
  signal clk, res, lod: bit;
  signal entradaD, saidaQ: bit_vector(wordSize-1 downto 0);
  signal saidai: integer range 0 to wordSize-1;
  constant periodoClock : time := 1 ns;

begin
  clk <= not clk after periodoClock/2;
  saidai <= to_integer(unsigned(saidaQ));

  dut: reg
    generic map(wordSize)
    port map(clk,res,lod,entradaD,saidaQ);

  st: process is
  begin
    --! Imprime mensagem de inicio de teste
    assert false report "BOT" severity note;

    --! Testa se o clear está OK
    res<='1';
    wait until rising_edge(clk);
    wait until falling_edge(clk);
    assert saidai = 0 report "Teste de reset falhou." &
      " Obtido: " & integer'image(saidai) & " em vez de 0"
      severity note;
    res<='0';

    lod<='1';

    entradaD <= "0000";

    assert false report
    "Test1 falhou. Esperado: " & integer'image(0) &
    " Obtido: " & integer'image(saidai)
      severity note;
    wait until falling_edge(clk);

    lod<='1';

    entradaD <= "0001";

    assert false report
      "Test2 falhou. Esperado: " & integer'image(1) &
      " Obtido: " & integer'image(saidai)
      severity note;
    wait until falling_edge(clk);

    lod<='1';

    entradaD <= "0010";

    assert false report
      "Test3 falhou. Esperado: " & integer'image(2) &
      " Obtido: " & integer'image(saidai)
      severity note;
    wait until falling_edge(clk);

    lod<='1';

    entradaD <= "0011";

    assert false report
      "Test4 falhou. Esperado: " & integer'image(3) &
      " Obtido: " & integer'image(saidai)
      severity note;
    wait until falling_edge(clk);


    assert false report "EOT" severity note;
    wait;

  end process;
end dut;
