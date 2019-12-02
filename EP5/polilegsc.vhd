library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;

entity reg is
  generic(
    wordSize : natural:= 64
  );
  port(
    clock, reset, load : in bit;
    d : in bit_vector(wordSize-1 downto 0);
    q : out bit_vector(wordSize-1 downto 0)
  );
end reg;

architecture arch_reg of reg is

  begin
    process(clock,reset,d)
    begin
      if(reset = '1') then
        q <= (others => '0');
      elsif (load = '1' and rising_edge(clock)) then
        q <= d;
      end if;
    end process;
  end arch_reg;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
use IEEE.math_real.ceil;
use IEEE.math_real.log2;
entity regfile is
  generic(
    regn : natural := 32;
    wordSize : natural := 64
  );

  port(
    clock: in bit;
    reset: in bit;
    regWrite: in bit;
    rr1, rr2, wr: in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
    d: in bit_vector(wordSize-1 downto 0);
    q1,q2: out bit_vector(wordSize-1 downto 0)
  );
end regfile;

architecture arch_regfile of regfile is
  type banco_reg is array(0 to regn-1) of bit_vector(wordSize-1 downto 0);
  signal registradores: banco_reg ;
  begin
    process(clock,reset,d)
    begin
      if(reset = '1') then
        for j in 0 to regn-1 loop
          registradores(j) <= (others => '0');
        end loop;
      end if;
      if (regWrite = '1' and rising_edge(clock) and to_integer(unsigned(wr)) /= regn-1) then
        registradores(to_integer(unsigned(wr))) <= d;
      end if;
    end process;
    q1 <= registradores(to_integer(unsigned(rr1)));
    q2 <= registradores(to_integer(unsigned(rr2)));
end arch_regfile;

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
    size : natural := 64
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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
entity signExtend is
  port(
    i: in bit_vector(31 downto 0);
    o: out bit_vector(63 downto 0)
  );
end entity;
architecture arch_signExtend of signExtend is
  signal tipoB: bit;
  signal tipoCB: bit;
  signal tipoD: bit;
  signal opD: bit_vector(10 downto 0);
  signal opB: bit_vector(5 downto 0);
  signal opCB: bit_vector(7 downto 0);
  signal adB: bit_vector(25 downto 0);
  signal adCB: bit_vector(18 downto 0);
  signal adD: bit_vector(8 downto 0);
  signal extendB: bit_vector(63 downto 0);
  signal extendCB: bit_vector(63 downto 0);
  signal extendD: bit_vector(63 downto 0);
  signal tipo: bit_vector(2 downto 0);
  begin
    opD <= i(31 downto 21);
    opB <= i(31 downto 26);
    opCB <= i(31 downto 24);
    adB <= i(25 downto 0);
    adCB <= i(23 downto 5);
    adD <= i(20 downto 12);
    with opD select
      tipoD <= '1' when "11111000010",
               '1' when "11111000000",
               '0' when others;
    with opB select
      tipoB <= '1' when "000101",
               '0' when others;
    with opCB select
      tipoCB <= '1' when "10110100",
                '0' when others;
    extendB <= bit_vector(resize(signed(adB),64));
    extendCB <= bit_vector(resize(signed(adCB),64));
    extendD <= bit_vector(resize(signed(adD),64));
    tipo <= tipoB & tipoCB & tipoD;
    with tipo select
      o <= extendB when "100",
           extendCB when  "010",
           extendD when others;
end arch_signExtend;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
entity alucontrol is
  port(
    aluop: in bit_vector(1 downto 0);
    opcode: in bit_vector(10 downto 0);
    aluCtrl : out bit_vector (3 downto 0)
  );
end entity;
architecture arch_alucontrol of alucontrol is
  signal aluopr: bit_vector(3 downto 0);
  signal aluopd: bit_vector(3 downto 0);
  begin
    with opcode select
      aluopr <= "0010" when "10001011000",
                "0110" when "11001011000",
                "0000" when "10001010000",
                "0001" when "10101010000",
                "1000" when others;
    with aluop select
    	aluCtrl <= "0010" when "00",
                 "0111" when "01",
                 aluopr when "10",
                 "0000" when others;
end arch_alucontrol;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
entity controlunit is
  port(
    reg2loc : out bit;
    uncondBranch : out bit;
    branch: out bit;
    memRead: out bit;
    memToReg: out bit;
    aluOp: out bit_vector(1 downto 0);
    memWrite: out bit;
    aluSrc: out bit;
    regWrite: out bit;
    opcode:in bit_vector(10 downto 0)
  );
end entity;
architecture arch_controlunit of controlunit is
  signal tipoB: bit;
  signal tipoCB: bit;
  signal opB: bit_vector(5 downto 0);
  signal opCB: bit_vector(7 downto 0);
  signal opcodeB: bit_vector(10 downto 0);
  signal opcodeCB: bit_vector(10 downto 0);
  signal tipo: bit_vector(1 downto 0);
  signal opcode1: bit_vector(10 downto 0);
  begin
    opB <= opcode(10 downto 5);
    opCB <= opcode(10 downto 3);
    with opB select
      tipoB <= '1' when "000101",
               '0' when others;
    with opCB select
      tipoCB <= '1' when "10110100",
                '0' when others;
    with opB select
      opcodeB <= "00010100000" when "000101",
              opcode when others;
    with opCB select
      opcodeCB <= "10110100000" when "10110100",
                opcode when others;
    tipo <= tipoB & tipoCB;
    with tipo select
      opcode1 <= opcodeB when "10",
                 opcodeCB when "01",
                 opcode when others;
    with opcode1 select
      reg2loc <='0' when "11111000010",
                '1' when "11111000000",
                '1' when "10110100000",
                '0' when "00010100000",
                '0' when "10001011000",
                '0' when "11001011000",
                '0' when "10001010000",
                '0' when "10101010000",
                '0' when others;

      with opcode1 select
        uncondBranch <='0' when "11111000010",
                  '0' when "11111000000",
                  '0' when "10110100000",
                  '1' when "00010100000",
                  '0' when "10001011000",
                  '0' when "11001011000",
                  '0' when "10001010000",
                  '0' when "10101010000",
                  '0' when others;

      with opcode1 select
          branch <='0' when "11111000010",
                    '0' when "11111000000",
                    '1' when "10110100000",
                    '0' when "00010100000",
                    '0' when "10001011000",
                    '0' when "11001011000",
                    '0' when "10001010000",
                    '0' when "10101010000",
                        '0' when others;

        with opcode1 select
            memRead <='1' when "11111000010",
                      '0' when "11111000000",
                      '0' when "10110100000",
                      '0' when "00010100000",
                      '0' when "10001011000",
                      '0' when "11001011000",
                      '0' when "10001010000",
                      '0' when "10101010000",
                      '0' when others;

          with opcode1 select
              memToReg <='1' when "11111000010",
                        '0' when "11111000000",
                        '0' when "10110100000",
                        '0' when "00010100000",
                        '0' when "10001011000",
                        '0' when "11001011000",
                        '0' when "10001010000",
                        '0' when "10101010000",
                        '0' when others;

          with opcode1 select
              aluOp <="00" when "11111000010",
                        "00" when "11111000000",
                        "01" when "10110100000",
                        "01" when "00010100000",
                        "10" when "10001011000",
                        "10" when "11001011000",
                        "10" when "10001010000",
                        "10" when "10101010000",
                        "00" when others;

            with opcode1 select
              memWrite <='0' when "11111000010",
                        '1' when "11111000000",
                        '0' when "10110100000",
                        '0' when "00010100000",
                        '0' when "10001011000",
                        '0' when "11001011000",
                        '0' when "10001010000",
                        '0' when "10101010000",
                        '0' when others;

            with opcode1 select
              aluSrc <='1' when "11111000010",
                        '1' when "11111000000",
                        '0' when "10110100000",
                        '0' when "00010100000",
                        '0' when "10001011000",
                        '0' when "11001011000",
                        '0' when "10001010000",
                        '0' when "10101010000",
                        '0' when others;

            with opcode1 select
              regWrite <='1' when "11111000010",
                        '0' when "11111000000",
                        '0' when "10110100000",
                        '0' when "00010100000",
                        '1' when "10001011000",
                        '1' when "11001011000",
                        '1' when "10001010000",
                        '1' when "10101010000",
                        '0' when others;

end arch_controlunit;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
use IEEE.math_real.ceil;
use IEEE.math_real.log2;
entity datapath is
  generic(
    wordSize : natural:= 64;
    size : natural:= 64;
    regn : natural:= 32
  );
  port(
  --Common
    clock : in bit ;
    reset : in bit ;
  --From Control Unit
    reg2loc: in bit;
    pcsrc: in bit;
    memToReg: in bit;
    aluCtrl: in bit_vector(3 downto 0);
    aluSrc: in bit;
    regWrite: in bit;
  --To Control Unit
    opcode: out bit_vector(10 downto 0);
    zero: out bit;
  --IM interface
    imAddr: out bit_vector(63 downto 0);
    imOut: in bit_vector(31 downto 0);
  --DM interface
    dmAddr: out bit_vector(63 downto 0);
    dmIn: out bit_vector(63 downto 0);
    dmOut: in bit_vector(63 downto 0)
  );
end entity;
architecture arch_datapath of datapath is
  signal read1: bit_vector(63 downto 0);
  signal read2: bit_vector(63 downto 0);
  signal extendido: bit_vector(63 downto 0);
  signal shiftado2: bit_vector(63 downto 0);
  signal entrada1ula1: bit_vector(63 downto 0);
  signal resultalu1: bit_vector(63 downto 0);
  signal rr1: bit_vector(4 downto 0);
  signal rr2: bit_vector(4 downto 0);
  signal wr: bit_vector(4 downto 0);
  signal data: bit_vector(63 downto 0);
  signal instr40: bit_vector(4 downto 0);
  signal instr2016: bit_vector(4 downto 0);
  signal instr95: bit_vector(4 downto 0);
  signal instr3121: bit_vector(10 downto 0);
  signal q: bit_vector (63 downto 0);
  signal d: bit_vector(63 downto 0);
  signal resultalu4: bit_vector(63 downto 0);
  signal mais4: bit_vector(63 downto 0);
  signal resultaluInstr: bit_vector(63 downto 0);
  component alu is
    port(
    A, B : in bit_vector(size - 1 downto 0);
    F : out bit_vector(size - 1 downto 0);
    S : in bit_vector(3 downto 0);
    Z : out bit;
    Ov : out bit;
    Co : out bit);
  end component;
  component regfile is
    port(
      clock: in bit;
      reset: in bit;
      regWrite: in bit;
      rr1, rr2, wr: in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
      d: in bit_vector(wordSize-1 downto 0);
      q1,q2: out bit_vector(wordSize-1 downto 0)
    );
  end component;
  component signExtend is
    port(
      i: in bit_vector(31 downto 0);
      o: out bit_vector(63 downto 0)
    );
  end component;
  component reg is
    port(
      clock, reset, load : in bit;
      d : in bit_vector(wordSize-1 downto 0);
      q : out bit_vector(wordSize-1 downto 0)
    );
  end component;
  begin
    ULA1: alu port map(read1,entrada1ula1,resultalu1,aluCtrl,zero,open,open);
    sE: signExtend port map(imOut,extendido);
    BR: regfile port map(clock,reset,regWrite,rr1,rr2,wr,data,read1,read2);
    instr40 <= imOut(4 downto 0);
    instr2016 <= imOut(20 downto 16);
    instr95 <= imOut(9 downto 5);
    instr3121 <= imOut(31 downto 21);
    opcode <= instr3121;
    rr1 <= instr95;
    with reg2loc select
      rr2 <= instr2016 when '0',
             instr40 when others;
      wr <= instr40;
    with memToReg select
      data <= resultalu1 when '0',
              dmOut when others;
    dmAddr <= resultalu1;
    dmIn <= read2;
    with aluSrc select
      entrada1ula1 <= read2 when '0',
                      extendido when others;
    ULA4: alu port map(q,mais4,resultalu4,"0010",open,open,open);
    ULAINSTR: alu port map(q,shiftado2,resultaluInstr,"0010",open,open,open);
    mais4 <= "0000000000000000000000000000000000000000000000000000000000000100";
    shiftado2 <= extendido(61 downto 0) & "00";
    with pcsrc select
      d <= resultaluInstr when '1',
           resultalu4  when others;
    PC: reg port map(clock,reset,'1',d,q);
    imAddr <= q;
end arch_datapath;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_bit.all;
entity polilegsc is
  port(
    clock, reset: in bit;
    dmem_addr: out bit_vector(63 downto 0);
    dmem_dati: out bit_vector(63 downto 0);
    dmem_dato: in bit_vector(63 downto 0);
    imem_addr: out bit_vector(63 downto 0);
    imem_data: in bit_vector(31 downto 0);
    dmem_we: out bit
  );
end entity;
architecture arch_polilegsc of polilegsc is
  signal reg2locsignal: bit;
  signal uncondBranchsignal: bit;
  signal branchsignal: bit;
  signal memReadsignal: bit;
  signal memToRegsignal: bit;
  signal memWritesignal: bit;
  signal regWritesignal: bit;
  signal aluSrcsignal: bit;
  signal opcodesignal: bit_vector(10 downto 0);
  signal aluOpsignal: bit_vector(1 downto 0);
  signal aluCtrlsignal: bit_vector(3 downto 0);
  signal pcsrcsignal: bit;
  signal zerosignal: bit;
    component datapath is
      port(
      --Common
        clock : in bit ;
        reset : in bit ;
      --From Control Unit
        reg2loc: in bit;
        pcsrc: in bit;
        memToReg: in bit;
        aluCtrl: in bit_vector(3 downto 0);
        aluSrc: in bit;
        regWrite: in bit;
      --To Control Unit
        opcode: out bit_vector(10 downto 0);
        zero: out bit;
      --IM interface
        imAddr: out bit_vector(63 downto 0);
        imOut: in bit_vector(31 downto 0);
      --DM interface
        dmAddr: out bit_vector(63 downto 0);
        dmIn: out bit_vector(63 downto 0);
        dmOut: in bit_vector(63 downto 0)
      );
    end component;
    component controlunit is
      port(
        reg2loc : out bit;
        uncondBranch : out bit;
        branch: out bit;
        memRead: out bit;
        memToReg: out bit;
        aluOp: out bit_vector(1 downto 0);
        memWrite: out bit;
        aluSrc: out bit;
        regWrite: out bit;
        opcode:in bit_vector(10 downto 0)
      );
    end component;
    component alucontrol is
      port(
        aluop: in bit_vector(1 downto 0);
        opcode: in bit_vector(10 downto 0);
        aluCtrl : out bit_vector (3 downto 0)
      );
    end component;
    begin
      CU: controlunit port map(reg2locsignal,uncondBranchsignal,branchsignal,memReadsignal,memToRegsignal,aluOpsignal,memWritesignal,aluSrcsignal,regWritesignal,opcodesignal);
      DP: datapath port map(clock,reset,reg2locsignal,pcsrcsignal,memToRegsignal,aluCtrlsignal,aluSrcsignal,regWritesignal,opcodesignal,zerosignal,imem_addr,imem_data,dmem_addr,dmem_dati,dmem_dato);
      ALUControle: alucontrol port map(aluOpsignal,opcodesignal,aluCtrlsignal);
      pcsrcsignal <= uncondBranchsignal or (zerosignal and branchsignal);
      dmem_we <= memWritesignal;
end arch_polilegsc;
