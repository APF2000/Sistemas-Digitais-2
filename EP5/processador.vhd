library ieee;
use std.textio.all;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;

entity ram is
  generic (
    addressSize : natural := 64;
    wordSize    : natural := 32
  );
  port(
    -- wr -> write = '1', read = '0'
    ck, wr : in  bit;
    addr   : in  bit_vector(addressSize-1 downto 0);
    data_i : in  bit_vector(wordSize-1 downto 0);
    data_o : out bit_vector(wordSize-1 downto 0)
  );
end ram;

architecture arc of ram is

  type memory is array (2**addressSize-1 downto 0)
      of bit_vector(wordSize-1 downto 0);
  signal chip : memory;

    begin
    process(ck, wr, addr, data_i)
      begin
        if rising_edge(ck) then
          -- write
          if wr = '1' then
            chip(to_integer(unsigned(addr))) <= data_i;
          else
          end if;
        else
        end if;
    end process;

    -- read
    data_o <= chip(to_integer(unsigned(addr)));
end arc;

library ieee;
use std.textio.all;
use ieee.numeric_bit.all;
entity rom is
  generic(
    addressSize : natural := 64;
    wordSize    : natural := 32;
    mifFileName : string  := "rom.dat"
  );
  port(
    addr : in  bit_vector(addressSize-1 downto 0);
    data : out bit_vector(wordSize-1 downto 0)
  );
end rom;

architecture arc of rom is

  constant address : natural := 2**addressSize;
  type memory is array (0 to address-1)
      of bit_vector(wordSize-1 downto 0);

  impure function init_mem(nameOfFile : in string) return memory is
      file mif_file : text open read_mode is nameOfFile;
      variable mif_line : line;
      variable temp_bv : bit_vector(wordSize-1 downto 0);
      variable temp_mem : memory;
  begin
      for i in memory'range loop
          readline(mif_file, mif_line);
          read(mif_line, temp_bv);
          temp_mem(i) := temp_bv;
      end loop;
      return temp_mem;
  end;

  constant mem : memory := init_mem(mifFileName);

  begin
    data <= mem(to_integer(unsigned(addr)));
end arc;

library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
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

library ieee;
use ieee.numeric_bit.all;
entity alu is
  generic (
    size: natural := 10 --bit size
  );
  port(
    A, B : in bit_vector(size-1 downto 0); --inputs
    F    : out bit_vector(size-1 downto 0); --output
    S    : in bit_vector(3 downto 0); --op selection
    Z    : out bit; --zero flag
    Ov   : out bit; --overflow flag
    Co   : out bit --carry out
  );
end entity alu;

architecture arcalu of alu is
  component alu1bit
    port (
      a, b, less, cin             : in  bit;
      result, cout, set, overflow : out bit;
      ainvert, binvert            : in  bit;
      operation                   : in  bit_vector(1 downto 0)
    );
  end component;

  signal sum, ignore : bit_vector(size-1 downto 0);
  signal ainvert, binvert : bit;
  signal op : bit_vector(1 downto 0);
  signal less : bit_vector(size downto 0);
  signal cin, cout : bit_vector(size-1 downto 0);
  signal auxF, auxSLT : bit_vector(size-1 downto 0);
  signal Alast, Blast : bit;
  signal zero, res : bit_vector(size-1 downto 0);

  begin
    ainvert <= S(3);
    binvert <= S(2);
    op(1) <= S(1);
    op(0) <= S(0);
    less(size) <= '0';

    GEN: for i in size-1 downto 0 generate

        LOWER_BIT: if i=0 generate
          U0: alu1bit port map
             (A(i), B(i), less(i), binvert,
              auxF(i), cin(i+1), sum(i), ignore(i),
              ainvert, binvert, op);
          auxSLT(i) <= sum(size-1);
        end generate LOWER_BIT;

        UPPER_BITS: if i>0 and i<size-1 generate
          U1: alu1bit port map
             (A(i), B(i), less(i), cin(i),
              auxF(i), cin(i+1), sum(i), ignore(i),
              ainvert, binvert, op);
          auxSLT(i) <= '0';
        end generate UPPER_BITS;

        HIGHER_BIT: if i=size-1 generate
          U2: alu1bit port map
             (A(i), B(i), less(i), cin(i),
              auxF(i), Co, sum(i), ignore(i),
              ainvert, binvert, op);
          auxSLT(i) <= '0';
        end generate HIGHER_BIT;
    end generate GEN;

    with op select
      res <= auxSLT when "11",
           auxF when others;
    F <= res;

    zero <= (others => '0');
    Z <= '1' when res = zero else
         '0';

    with Ainvert select
      Alast <= A(size-1) when '0',
              not A(size-1) when '1',
              '0' when others;
    with Binvert select
      Blast <= B(size-1) when '0',
              not B(size-1) when '1',
              '0' when others;

    Ov <= (sum(size-1) and (not Alast) and (not Blast))
      or ((not sum(size-1)) and Alast and Blast);
end architecture;

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
      size64: natural := 64 --bit size
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
  signal writeData, readData1, readData2 : bit_vector(63 downto 0);
  signal ALUresult, muxPC, shiftleft2 : bit_vector(63 downto 0);
  signal ignore : bit_vector(10 downto 0);
  constant sumOp : bit_vector(3 downto 0) : "0010";
  begin
    bancoReg: regfile port map
      (
        clock, reset, regWrite,
        imOut(9 downto 5), readRegister2, imOut(4 downto 0),
        writeData, readData1, readData2
      );
    add4: alu port map
      (
        imAddr, to_bitvector(size),  -- A, B
        muxPC, sumOp, ignore(0), -- F,S,Z
        ignore(1), ignore(2) -- Ov, Co
      );
    nextInstr: alu port map
      (
        imAddr, shiftleft2
      );
    mainAdder: alu port map
      (

      );
    signExtend: signExtend port map
      (
        imOut, shiftleft2
      );

    process(clock, reset)
      begin
        if rising_edge(clock) then
          -- write
          if reg2loc = '1' then
            readRegister2 <= dmOut(4 downto 0);
          else
            readRegister2 <= dmOut(20 downto 16);
          end if;

          if memRead = '1' then
            writeData <= dmOut;
          else
            writeData <= ALUresult;
          end if;
        else
        end if;
    end process;
end arcdata;
