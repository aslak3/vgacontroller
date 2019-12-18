-- regaddrdecode testbench

library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use work.P_VGACONTROLLER.ALL;

entity regaddrdecode_testbench is
end entity;

architecture testbench of regaddrdecode_testbench is
	signal MPU_CLOCK : STD_LOGIC := '0';
	signal CS : STD_LOGIC := '0';
	signal A : STD_LOGIC_VECTOR (2 downto 0) := "000";
	signal REGSELECTS : T_REG_LOGIC := (others => '0');
begin
	dut: entity work.regaddrdecode port map(MPU_CLOCK, CS, A, REGSELECTS);

	process is
	begin
		CS <= '1';
		A <= "000";

		for I in 0 to 7 loop
			A <= A + 1;
			wait for 1 us;
			MPU_CLOCK <= '0';
			wait for 1 us;
			MPU_CLOCK <= '1';
		end loop;

		CS <= '0';
		A <= "000";

		for I in 0 to 7 loop
			A <= A + 1;
			wait for 1 us;
			MPU_CLOCK <= '0';
			wait for 1 us;
			MPU_CLOCK <= '1';
		end loop;
	end process;

end architecture;

-- vramcontroller testbench

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.P_VGACONTROLLER.ALL;

entity vramcontroller_testbench is
end entity;

architecture testbench of vramcontroller_testbench is
	constant VGA_CLOCK_PERIOD : time := 39.7219463754 ns;  -- 25.175 MHz
	constant MPU_CLOCK_PERIOD : time := 62.5 ns; -- 16 MHz

	signal CLOCK : STD_LOGIC;
	signal WRITE_ENABLE : STD_LOGIC;
	signal READWRITE_ADDR : STD_LOGIC_VECTOR (15 downto 0);
	signal WRITE_DATA : STD_LOGIC_VECTOR (7 downto 0);
	signal READ_ENABLE : STD_LOGIC;
	signal READ_DATA : STD_LOGIC_VECTOR (7 downto 0);
	signal DISPLAY_ENABLE : STD_LOGIC;
	signal DISPLAY_ADDR : STD_LOGIC_VECTOR (15 downto 0) := x"0000";
	signal DISPLAY_DATA : STD_LOGIC_VECTOR (7 downto 0) := x"00";
	signal DATA : STD_LOGIC_VECTOR (7 downto 0);
	signal ADDR : STD_LOGIC_VECTOR (15 downto 0);
	signal READ : STD_LOGIC;
	signal WRITE : STD_LOGIC;

	signal MPU_CLOCK : STD_LOGIC;

	procedure delay (
		signal	CLOCK : out STD_LOGIC;
		constant PERIOD : time;
		constant C : integer) is
	begin
		delay: for x in 0 to C loop
			CLOCK <= '1';
			wait for PERIOD / 2;
			CLOCK <= '0';
			wait for PERIOD / 2;
		end loop;
	end;

begin
	dut: entity work.vramcontroller port map(
		CLOCK => CLOCK,
		WRITE_ENABLE => WRITE_ENABLE,
		READWRITE_ADDR => READWRITE_ADDR,
		WRITE_DATA => WRITE_DATA,
		READ_ENABLE => READ_ENABLE,
		READ_DATA => READ_DATA,
		DISPLAY_ENABLE => DISPLAY_ENABLE,
		DISPLAY_ADDR => DISPLAY_ADDR,
		DISPLAY_DATA => DISPLAY_DATA,
		DATA => DATA,
		ADDR => ADDR,
		READ => READ,
		WRITE => WRITE
	);

	mpu: process is
	begin
		WRITE_ENABLE <= '0';
		READ_ENABLE <= '0';
		READWRITE_ADDR <= x"1111";
		WRITE_DATA <= x"aa";
		delay(MPU_CLOCK, MPU_CLOCK_PERIOD, 3);

		WRITE_ENABLE <= '1';
		delay(MPU_CLOCK, MPU_CLOCK_PERIOD, 0);

		WRITE_ENABLE <= '0';
		READWRITE_ADDR <= x"2222";
		delay(MPU_CLOCK, MPU_CLOCK_PERIOD, 3);

		READ_ENABLE <= '1';
		delay(MPU_CLOCK, MPU_CLOCK_PERIOD, 0);
	end process;

	vga: process
	begin
		DISPLAY_ENABLE <= '0';
		delay(CLOCK, VGA_CLOCK_PERIOD, 2);

		DISPLAY_ENABLE <= '1';
		delay(CLOCK, VGA_CLOCK_PERIOD, 0);
		DISPLAY_ADDR <= DISPLAY_ADDR + 1;
	end process;

	DATA <= x"33" when (READ = '1') else "ZZZZZZZZ";

end architecture;

-- vgacontroller testbench

library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use work.P_VGACONTROLLER.ALL;

entity vgacontroller_testbench is
end entity;

architecture testbench of vgacontroller_testbench is
	constant VGA_CLOCK_PERIOD : time := 39.7219463754 ns;  -- 25.175 MHz
	constant MPU_CLOCK_PERIOD : time := 62.5 ns; -- 16 MHz

	signal VGA_CLOCK : STD_LOGIC;
	signal MPU_CLOCK : STD_LOGIC;

	-- VGA signals
	signal	H_SYNC : STD_LOGIC;
	signal	V_SYNC : STD_LOGIC;
	-- "000" (dark) to "111" (light)
	signal RED : STD_LOGIC_VECTOR (2 downto 0);
	signal GREEN : STD_LOGIC_VECTOR (2 downto 0);
	signal BLUE : STD_LOGIC_VECTOR (2 downto 0);

	-- VRAM control and busses
	signal nVRAM_CS : STD_LOGIC_VECTOR (2 downto 0);
	signal nVRAM_WRITE : STD_LOGIC;
	signal nVRAM_READ : STD_LOGIC;
	signal VRAM_D : STD_LOGIC_VECTOR (7 downto 0);
	signal VRAM_A : STD_LOGIC_VECTOR (14 downto 0);

	-- MPU interface
	signal nCS : STD_LOGIC;
	signal nREAD : STD_LOGIC;
	signal nWRITE : STD_LOGIC;
	signal D : STD_LOGIC_VECTOR (7 downto 0);
	signal A : STD_LOGIC_VECTOR (2 downto 0);
	signal nDTACK : STD_LOGIC;
	signal nINT : STD_LOGIC;

	-- Other
	signal TESTING : STD_LOGIC;
begin
	dut: entity work.vgacontroller port map(
		VGA_CLOCK => VGA_CLOCK,
		MPU_CLOCK => MPU_CLOCK,

		-- VGA signals
		H_SYNC => H_SYNC,
		V_SYNC => V_SYNC,
		-- "000" (dark) to "111" (light)
		RED => RED,
		GREEN => GREEN,
		BLUE => BLUE,

		-- VRAM control and busses
		nVRAM_CS => nVRAM_CS,
		nVRAM_WRITE => nVRAM_WRITE,
		nVRAM_READ => nVRAM_READ,
		VRAM_D => VRAM_D,
		VRAM_A => VRAM_A,

		-- MPU interface
		nCS => nCS,
		nREAD => nREAD,
		nWRITE => nWRITE,
		D => D,
		A => A,
		nDTACK => nDTACK,
		nINT => nINT,

		-- other
		TESTING => TESTING
	);

	nCS <= '1';
	nREAD <= '1'; nWRITE <= '1';
	A <= "000";

	VRAM_D <= x"2a" when (nVRAM_READ = '0') else "ZZZZZZZZ";

	vgaclock: process is
	begin
		VGA_CLOCK <= '0';
		wait for VGA_CLOCK_PERIOD / 2;
		VGA_CLOCK <= '1';
		wait for VGA_CLOCK_PERIOD / 2;
	end process;

	mpuclock: process is
	begin
		MPU_CLOCK <= '0';
		wait for MPU_CLOCK_PERIOD / 2;
		MPU_CLOCK <= '1';
		wait for MPU_CLOCK_PERIOD / 2;
	end process;

end architecture;
