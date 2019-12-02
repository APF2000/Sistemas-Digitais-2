library ieee;
use std.textio.all;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;

entity reg is
  generic(wordSize: natural := 64);
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

  flipFlops: process(clock, reset) is
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

    q1 <= bancoReg(to_integer(unsigned(rr1)));
    q2 <= bancoReg(to_integer(unsigned(rr2)));

    PRO: process(clock, reset) is begin

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

library ieee;
use ieee.numeric_bit.all;
entity fulladder is
  port(
    a, b, cin : in  bit;
    s, cout   : out bit
  );
end entity;

architecture arc of fulladder is
  begin
    s <= a xor b xor cin;
    cout <= (cin and (a or b)) or (a and b);
end arc;

library ieee;
use ieee.numeric_bit.all;
entity alu1bit is
  port(
    a, b, less, cin: in bit;
    result, cout, set, overflow: out bit;
    ainvert, binvert: in bit;
    operation: in bit_vector(1 downto 0)
  );
end entity;

architecture arcalu of alu1bit is
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
end arcalu;


library ieee;
use ieee.numeric_bit.all;
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

library ieee;
use ieee.numeric_bit.all;
entity alucontrol is
  port (
    aluop: in bit_vector(1 downto 0);
    opcode : in bit_vector(10 downto 0);
    aluCtrl : out bit_vector(3 downto 0)
  );
end entity;

architecture arc of alucontrol is
  signal auxop : bit_vector(3 downto 0);
  begin
    with aluop select
      aluCtrl <= "0010" when "00",
                 "0111" when "01",
                 auxop  when "10",
                 "1111" when others;

    with opcode select
      auxop <= "0010" when B"1000_1011_000", -- ADD
               "0110" when B"1100_1011_000", -- SUB
               "0000" when B"1000_1010_000", -- AND
               "0001" when B"1010_1010_000", -- ORR
               "1111" when others;
end architecture;

library ieee;
use ieee.numeric_bit.all;
entity signExtend is
  port(
    i: in bit_vector(31 downto 0);
    o: out bit_vector(63 downto 0)
  );
end entity signExtend;

architecture arc of signExtend is
  signal size : bit_vector(63 downto 0);
  signal aux, aux2 : bit_vector(2 downto 0);
  signal Dformat   : bit_vector(8 downto 0);
  signal CBZformat : bit_vector(18 downto 0);
  signal Bformat   : bit_vector(25 downto 0);
  begin
    Dformat <= i(20 downto 12);
    CBZformat <= i(23 downto 5);
    Bformat <= i(25 downto 0);

    o <= size;
    with aux select
      size <= bit_vector( resize(signed(Dformat), size'length) ) when "100",
              bit_vector( resize(signed(CBZformat), size'length) ) when "010",
              bit_vector( resize(signed(Bformat), size'length) ) when "001",
              (others => '1')  when others;

    with i(31 downto 26) select
      aux <= "001" when "000101",
             aux2 when others;

    with i(31 downto 24) select
      aux2 <= "010" when "10110100",
              "100" when others;
end architecture;

entity controlunit is
  port (
    -- To datapath
    reg2loc      : out bit;
    uncondBranch : out bit;
    branch       : out bit;
    memRead      : out bit;
    memToReg     : out bit;
    aluOp        : out bit_vector(1 downto 0);
    memWrite     : out bit;
    aluSrc       : out bit;
    regWrite     : out bit;
    -- From datapath
    opcode       : in bit_vector(10 downto 0)
  );
end entity;

architecture arc of controlunit is
  signal Rformat, Dformat : bit_vector(10 downto 0);
  signal CBZformat, cbz : bit_vector(7 downto 0);
  signal Bformat, b : bit_vector(5 downto 0);
  signal isB, isCBZ, isR, isD, ldur, stur : bit;
  signal whichFormat : bit_vector(4 downto 0);
  signal selection : bit_vector(9 downto 0);
  begin
    Rformat <= opcode;
    with Rformat select
      isR <= '1' when B"1000_1011_000", -- ADD
             '1' when B"1100_1011_000", -- SUB
             '1' when B"1000_1010_000", -- AND
             '1' when B"1010_1010_000", -- ORR
             '0' when others;

    Dformat <= opcode;
    with Dformat select
      isD <= '1' when B"1111_1000_010", -- LDUR
             '1' when B"1111_1000_000", -- STUR
             '0' when others;

    CBZformat <= opcode(10 downto 3);
    isCBZ <= '1' when (CBZformat = cbz)
             else '0';
    cbz <= B"1011_0100";

    Bformat <= opcode(10 downto 5);
    isB <= '1' when (Bformat = b)
           else '0';
    b <= B"0001_01";

    ldur <= isD and Dformat(1);
    stur <= isD and not Dformat(1);
    whichFormat(0) <= isR;
    whichFormat(1) <= ldur;
    whichFormat(2) <= stur;
    whichFormat(3) <= isCBZ;
    whichFormat(4) <= isB;

    with whichFormat select
      selection <= B"0_0000_10_001" when "00001", -- R
                   B"0_0011_00_011" when "00010", -- LDUR
                   B"1_0000_00_110" when "00100", -- STUR
                   B"1_0100_01_000" when "01000", -- CBZ
                   B"0_1000_01_000" when "10000", -- B
                   B"0_0000_00_000" when others;

    reg2loc <= selection(9);
    uncondBranch <= selection(8);
    branch <= selection(7);
    memRead <= selection(6);
    memToReg <= selection(5);

    aluOp <= selection(4 downto 3);
    memWrite <= selection(2);
    aluSrc <= selection(1);
    regWrite <= selection(0);
end architecture;

library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
entity datapath is
  generic (
    size : natural := 64
  );
  port(
    -- Common
    clock, reset : in bit;
    -- From control unit
    reg2loc, pcsrc, memToReg : in bit;
    aluCtrl : in bit_vector(3 downto 0);
    aluSrc, regWrite : in bit;
    -- To control unit
    opcode : out bit_vector(10 downto 0);
    zero : out bit;
    -- IM interface (instruction memory)
    imAddr : out bit_vector(63 downto 0);
    imOut : in bit_vector(31 downto 0);
    -- DM interface (data memory)
    dmAddr : out bit_vector(63 downto 0);
    dmIn   : out bit_vector(63 downto 0);
    dmOut  : in  bit_vector(63 downto 0)
  );
end entity datapath;
library ieee;
architecture arcdata of datapath is
  component regfile is
    generic(
      regn: natural := 32;
      wordSize: natural := 64
    );
    port(
      clock, reset, regWrite : in bit;
      rr1, rr2, wr: in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
      d           : in  bit_vector(wordSize-1 downto 0);
      q1, q2      : out bit_vector(wordSize-1 downto 0)
    );
  end component;
  component alu is
    generic (
      size: natural := 64 --bit size
    );
    port(
      A, B : in bit_vector(size-1 downto 0); --inputs
      F    : out bit_vector(size-1 downto 0); --output
      S    : in bit_vector(3 downto 0); --op selection
      Z    : out bit; --zero flag
      Ov   : out bit; --overflow flag
      Co   : out bit --carry out
    );
  end component;
  component signExtend is
    port(
      i: in bit_vector(31 downto 0);
      o: out bit_vector(63 downto 0)
    );
  end component;

  signal readRegister2 : bit_vector(4 downto 0);
  signal writeDataReg, writeDataMem, readData1, readData2 : bit_vector(63 downto 0);
  signal ALUresult, nextInstrAddr, shiftleft2, shiftleft2Aux : bit_vector(63 downto 0);
  signal instrPlus4, instrPlusShift, extendedSign : bit_vector(63 downto 0);
  signal imAddrAux : bit_vector(63 downto 0);
  signal src : bit_vector(63 downto 0);
  signal imOutAux : bit_vector(31 downto 0);
  constant sumOp : bit_vector(3 downto 0) := "0010";
  begin
    bancoReg: regfile port map
      (
        clock, reset, regWrite,
        imOutAux(9 downto 5), readRegister2, imOutAux(4 downto 0),
        writeDataReg, readData1, readData2
      );
    add4: alu port map
      (
        imAddrAux, X"0000_0000_0000_0004",  -- A, B
        instrPlus4, "0010", open, -- F,S,Z
        open, open -- Ov, Co
      );
    nextInstrAddrAlu: alu port map
      (
        imAddrAux, shiftleft2, -- A, B
        instrPlusShift, "0010", open, --F, S, Z
        open, open -- Ov, Co
      );
    mainAdder: alu port map
      (
        readData1, src,
        ALUresult, aluCtrl, zero,
        open, open
      );
    signExtendUnit: signExtend port map
      (
        imOutAux, shiftleft2Aux
      );
    imOutAux <= imOut;
    opcode <= imOutAux(31 downto 21);

    writeDataMem <= readData2;
    dmIn <= writeDataMem;

    dmAddr <= ALUresult;

    shiftleft2 <= shiftleft2Aux sll 2;

    with reg2loc select
      readRegister2 <= dmOut(4 downto 0) when '1',
                       dmOut(20 downto 16) when '0';
    with pcsrc select
      nextInstrAddr <= instrPlusShift when '1',
                       instrPlus4 when '0';
    imAddr <= nextInstrAddr;

    with aluSrc select
      src <= extendedSign when '1',
             readData2 when '0';

end arcdata;

entity polilegsc is
  port (
    clock, reset : in bit;
    -- Data memory
    dmem_addr : out bit_vector(63 downto 0);
    dmem_dati : out bit_vector(63 downto 0);
    dmem_dato : in bit_vector(63 downto 0);
    dmem_we : out bit;
    -- Instruction memory
    imem_addr : out bit_vector(63 downto 0);
    imem_data : in bit_vector(31 downto 0)
  );
end entity;

architecture arcpolileg of polilegsc is
  component datapath is
    generic (
      size : natural := 64
    );
    port(
      -- Common
      clock, reset : in bit;
      -- From control unit
      reg2loc, pcsrc, memToReg : in bit;
      aluCtrl : in bit_vector(3 downto 0);
      aluSrc, regWrite : in bit;
      -- To control unit
      opcode : out bit_vector(10 downto 0);
      zero : out bit;
      -- IM interface (instruction memory)
      imAddr : out bit_vector(63 downto 0);
      imOut : in bit_vector(31 downto 0);
      -- DM interface (data memory)
      dmAddr : out bit_vector(63 downto 0);
      dmIn   : out bit_vector(63 downto 0);
      dmOut  : in  bit_vector(63 downto 0)
    );
  end component;
  component controlunit is
    port (
      -- To datapath
      reg2loc      : out bit;
      uncondBranch : out bit;
      branch       : out bit;
      memRead      : out bit;
      memToReg     : out bit;
      aluOp        : out bit_vector(1 downto 0);
      memWrite     : out bit;
      aluSrc       : out bit;
      regWrite     : out bit;
      -- From datapath
      opcode       : in bit_vector(10 downto 0)
    );
  end component;
  component alucontrol is
    port (
      aluop: in bit_vector(1 downto 0);
      opcode : in bit_vector(10 downto 0);
      aluCtrl : out bit_vector(3 downto 0)
    );
  end component;
  component reg is
    generic(wordSize: natural := 64);
    port(
      clock: in  bit;
      reset:  in  bit;
      load:  in  bit;
      d:     in  bit_vector(wordSize-1 downto 0);
      q:     out bit_vector(wordSize-1 downto 0)
    );
  end component;

  signal dmInAux, dmOutAux : bit_vector(63 downto 0);
  signal nextInstrAddr : bit_vector(63 downto 0);
  signal instruction : bit_vector(31 downto 0);
  signal reg2loc, uncondBranch, branch : bit;
  signal memRead, memToReg : bit;
  signal aluOp : bit_vector(1 downto 0);
  signal aluCtrl : bit_vector(3 downto 0);
  signal memWrite, aluSrc, regWrite : bit;
  signal opcode : bit_vector(10 downto 0);
  signal zero, pcsrc : bit;

  begin
    data: datapath port map
     (
      clock, reset,
      -- Control Unit
      reg2loc, pcsrc, memToReg,
      aluCtrl, aluSrc, regWrite,
      opcode, zero,
      -- Instruction Memory
      imem_addr, instruction,
      -- Data memory
      dmem_addr, dmem_dati, dmem_dato
     );

    instruction <= imem_data;

    control: controlunit port map
     (
      reg2loc, uncondBranch, branch,
      memRead, memToReg,
      aluOp, memWrite,
      aluSrc, regWrite,
      opcode
     );
    pcsrc <= uncondBranch or (zero and branch);

    ctrlalu: alucontrol port map
      (
        aluOp, opcode, aluCtrl
      );
    dmem_we <= memWrite;

end arcpolileg;
