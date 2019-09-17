for i in 0 to wordSize-1 loop
  --! Verifica a contagem
  assert i = to_integer(unsigned(saidai)) report
    "Contagem falhou. Esperado:aaaa " & integer'image(i) &
    " Obtido: " & integer'image(saidai)
    severity failure;
  wait until falling_edge(clk);
end loop;

assert saidai=0 report "Teste de overflow falhou." severity failure;
en<='1';
wait until falling_edge(clk);
assert saidai=(wordSize-1) report "Teste de underflow falhou." severity failure;

--! Testa se a contagem decrescente está OK
for i in wordSize-1 downto 0 loop
  --! Verifica a contagem
  assert saidai = i report
    "Contagem falhou. Esperado: " & integer'image(i) &
    " Obtido: " & integer'image(saidai)
    severity failure;
  wait until falling_edge(clk);
end loop;

en<='1';
entrada <= (others=>'1');
wait until falling_edge(clk);
assert saidai=(wordSize-1) report "Teste de load max falhou." severity failure;
entrada <= (others=>'0');
wait until falling_edge(clk);
assert saidai=0 report "Teste de load min falhou." severity failure;

en<='1';
for i in 1 to 3 loop
  wait until falling_edge(clk);
  --! Verifica a contagem
  assert saidai=0 report
    "Teste de enable falhou no " & integer'image(i) &
    " ciclo." severity failure;
end loop;

assert false report "EOT" severity note;
wait;
