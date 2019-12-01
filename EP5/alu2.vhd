library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
entity alu1bit is
  port(
    a, b, less, cin: in bit;
    result, cout, set, overflow: out bit;
    ainvert, binvert: in bit;
    operation: in bit_vector(1 downto 0)
  );
end entity;
architecture arch_alu1bit of alu1bit is
  signal entradaa: bit ;
  signal entradab: bit;
  signal soma: bit;
  signal ou: bit;
  signal e: bit;
  signal cou: bit;
  begin
    entradaa <= (not a) when ainvert = '1' else a;
    entradab <= (not b) when binvert = '1' else b;
    cou <= (entradaa and entradab) or (cin and (entradaa xor entradab));
    overflow <= cou xor cin;
    cout<= cou;
    soma <= entradaa xor entradab xor cin;
    set <= soma;
    ou <= entradaa or entradab;
    e <= entradaa and entradab;
    with operation select
    	result <= e when "00",
        		    ou when "01",
                soma when "10",
                b when others;
end arch_alu1bit;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
entity alu is
  generic(
    size : natural := 10
  );
  port(
    A, B : in bit_vector(size - 1 downto 0);
    F : out bit_vector(size - 1 downto 0);
    S : in bit_vector(3 downto 0);
    Z : out bit;
    Ov : out bit;
    Co : out bit
  );
end entity;
architecture arch_alu of alu is
  component alu1bit is
    port(
      a,b,less,cin: in bit;
      result,cout,set,overflow: out bit;
      ainvert,binvert: in bit;
      operation: in bit_vector(1 downto 0)
    );
  end component;
  signal entradaA: bit_vector(size - 1 downto 0);
  signal entradaB: bit_vector(size - 1 downto 0);
  signal Cin: bit;
  signal Set: bit_vector(size - 1 downto 0);
  signal Overflow: bit_vector(size - 1 downto 0);
  signal Result: bit_vector(size - 1 downto 0);
  signal Cou: bit_vector(size - 1 downto 0);
  signal Operation: bit_vector(1 downto 0);
  begin
    a0:for i in size - 1 downto 1 generate
      a1:alu1bit port map (A(i),B(i),'0',Cou(i-1),Result(i),Cou(i),Set(i),Overflow(i),S(3),S(2),Operation);
    end generate;
    a2:alu1bit port map(A(0),B(0),Set(size - 1),Cin,Result(0),Cou(0),Set(0),Overflow(0),S(3),S(2),Operation);
    Operation <= S(1 downto 0);
    Cin <= S(3) or S(2);
    entradaA <= (not A) when S(3) = '1' else A;
    entradaB <= (not B) when S(2) = '1' else B;
    Co <= Cou(size - 1);
    Ov <= Cou(size - 1) xor Cou(size - 2);
    F <= Result;
    Z <= '1' when unsigned(Result) = 0 else '0';
end arch_alu;
