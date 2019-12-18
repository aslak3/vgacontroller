library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Types for the registers presented

package P_VGACONTROLLER is
	type T_REGNAMES is (
		REG_DATA,
		REG_DEFAULT_ATTRIBUTE,
		REG_MODE1,
		REG_MODE2,
		REG_READWRITE_ADDR_HI,
		REG_READWRITE_ADDR_LO,
		REG_OFFSET_ADDR_HI,
		REG_OFFSET_ADDR_LO
	);

	type T_REG_LOGIC is array (T_REGNAMES) of STD_LOGIC;
end package;

-- MPU-clock related entities

-- Register decoding

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.P_VGACONTROLLER.ALL;

entity regaddrdecode is
	port (	CLOCK : in STD_LOGIC;
			CS : in STD_LOGIC;
			A : in STD_LOGIC_VECTOR (2 downto 0);
			REG_SELECTS : out T_REG_LOGIC);
end entity;

architecture behavioral of regaddrdecode is
begin
	process (CLOCK)
	begin
		if (CLOCK'Event and CLOCK = '0') then
			REG_SELECTS <= (others => '0');
			-- This marks the end of S2 in the 8 state MPU bus cycle
			if (CS = '1') then
				if    (A = "000") then REG_SELECTS(REG_DATA) <= '1';
				elsif (A = "001") then REG_SELECTS(REG_DEFAULT_ATTRIBUTE) <= '1';
				elsif (A = "010") then REG_SELECTS(REG_MODE1) <= '1';
				elsif (A = "011") then REG_SELECTS(REG_MODE2) <= '1';
				elsif (A = "100") then REG_SELECTS(REG_READWRITE_ADDR_HI) <= '1';
				elsif (A = "101") then REG_SELECTS(REG_READWRITE_ADDR_LO) <= '1';
				elsif (A = "110") then REG_SELECTS(REG_OFFSET_ADDR_HI) <= '1';
				elsif (A = "111") then REG_SELECTS(REG_OFFSET_ADDR_LO) <= '1';
				else                   REG_SELECTS <= (others => '0');
				end if;
			end if;
		end if;
	end process;
end architecture;

-- Video-clock related entities

-- Video RAM controller: arbiter of write, read and display memory action

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.P_VGACONTROLLER.ALL;

entity vramcontroller is
	port (	CLOCK : in STD_LOGIC;
			WRITE_ENABLE : in STD_LOGIC;
			READWRITE_ADDR : in STD_LOGIC_VECTOR (15 downto 0);
			WRITE_DATA : in STD_LOGIC_VECTOR (7 downto 0);
			READ_ENABLE : in STD_LOGIC;
			READ_DATA : out STD_LOGIC_VECTOR (7 downto 0);
			DISPLAY_ENABLE : in STD_LOGIC;
			DISPLAY_ADDR : in STD_LOGIC_VECTOR (15 downto 0);
			DISPLAY_DATA : out STD_LOGIC_VECTOR (7 downto 0);
			DATA : inout STD_LOGIC_VECTOR (7 downto 0);
			ADDR : out STD_LOGIC_VECTOR (15 downto 0);
			READ : out STD_LOGIC;
			WRITE : out STD_LOGIC
	);
end entity;

architecture behavioral of vramcontroller is
	type READWRITE_STATE is (IDLE, W1, W2, R1, D1);  -- Define the states
	signal STATE : READWRITE_STATE := IDLE;	
	signal WRITE_ENABLE2 : STD_LOGIC := '0';
	signal WRITE_ENABLE1 : STD_LOGIC := '0';
	signal WRITE_ENABLE0 : STD_LOGIC := '0';
	signal READ_ENABLE2 : STD_LOGIC := '0';
	signal READ_ENABLE1 : STD_LOGIC := '0';
	signal READ_ENABLE0 : STD_LOGIC := '0';
	signal WRITE_PENDING : STD_LOGIC := '0';
	signal READ_PENDING : STD_LOGIC := '0';
begin
	process (CLOCK)
	begin
		if (CLOCK'Event and CLOCK = '1') then
			WRITE_ENABLE2 <= WRITE_ENABLE; -- METASTABLE!
			WRITE_ENABLE1 <= WRITE_ENABLE2; -- STABLE!
			WRITE_ENABLE0 <= WRITE_ENABLE1; -- STABLE!
			READ_ENABLE2 <= READ_ENABLE;
			READ_ENABLE1 <= READ_ENABLE2;
			READ_ENABLE0 <= READ_ENABLE1;

			-- Latch rising edges so they are not missed if STATE /= IDLE
			if (WRITE_ENABLE0 = '1' and WRITE_ENABLE1 = '0') then
				WRITE_PENDING <= '1';
			elsif (READ_ENABLE0 = '1' and READ_ENABLE1 = '0') then
				READ_PENDING <= '1';
			end if;

			case STATE is
				when IDLE =>
					if (WRITE_PENDING = '1') then
						DATA <= WRITE_DATA;
						ADDR <= READWRITE_ADDR;
						READ <= '0';
						WRITE <= '0';
						DISPLAY_DATA <= x"00";
						STATE <= W1;
					elsif (READ_PENDING = '1') then
						DATA <= "ZZZZZZZZ";
						ADDR <= READWRITE_ADDR;
						READ <= '1';
						WRITE <= '0';
						DISPLAY_DATA <= x"00";
						STATE <= R1;
					elsif (DISPLAY_ENABLE = '1') then
						DATA <= "ZZZZZZZZ";
						ADDR <= DISPLAY_ADDR;
						READ <= '1';
						WRITE <= '0';
						STATE <= D1;
					else
						READ <= '0';
						WRITE <= '0';
					end if;

				when W1 =>
					WRITE <= '1';
					STATE <= W2;
				when W2 =>
					WRITE <= '0';
					WRITE_PENDING <= '0';
					STATE <= IDLE;

				when R1 =>
					READ_DATA <= DATA;
					READ_PENDING <= '0';
					STATE <= IDLE;

				when D1 =>
					DISPLAY_DATA <= DATA;
					STATE <= IDLE;

				when others =>
					STATE <= IDLE;
			end case;
		end if;
	end process;
end architecture;

-- Core VGA signals: syncs and counters

library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.P_VGACONTROLLER.ALL;

entity vgagenerator is
	port (	CLOCK : in STD_LOGIC;
			H_SYNC : out STD_LOGIC;
			V_SYNC : out STD_LOGIC;
			H_DISPLAYING_RANGE : out STD_LOGIC;
			H_VISIBLE : out STD_LOGIC;
			V_VISIBLE : out STD_LOGIC;
			H_COUNT : out STD_LOGIC_VECTOR (9 downto 0);
			V_COUNT : out STD_LOGIC_VECTOR (9 downto 0);
			FRAME_COUNT : out STD_LOGIC_VECTOR (5 downto 0)
	);
end entity;

architecture behavioral of vgagenerator is
begin
	process (CLOCK)
		variable H : INTEGER RANGE 0 TO 800 := 0;
		variable V : INTEGER RANGE 0 to 525 := 0;
		variable FRAME : INTEGER RANGE 0 to 255 := 0;
	begin
		if (CLOCK'Event and CLOCK = '1') then
			-- Scanning beam path
			if (H < 800) then
				H := H + 1;
			else
				H := 0;
				if (V < 525) then
					V := V + 1;
				else
					V := 0;
					FRAME := FRAME + 1;
				end if;
			end if;

			-- AKA H sync
			if (H >= 640 + 16 + 16 and H < 640 + 16 + 96 - 16) then
				H_SYNC <= '0';
			else
				H_SYNC <= '1';
			end if;

			-- AKA V sync
			if (V >= 480 + 10 and V < 480 + 10 + 2) then
				V_SYNC <= '0';
			else
				V_SYNC <= '1';
			end if;

			-- Flags to say that this pixel is in the visible range
			if (H >= 8 and H < 640 + 8) then
				H_DISPLAYING_RANGE <= '1';
			else
				H_DISPLAYING_RANGE <= '0';
			end if;
			if (H >= 16 and H < 640 + 16) then
				H_VISIBLE <= '1';
			else
				H_VISIBLE <= '0';
			end if;
			if (V < 480) then
				V_VISIBLE <= '1';
			else
				V_VISIBLE <= '0';
			end if;

			-- Convert to vectors from the integers
			H_COUNT <= std_logic_vector(to_unsigned(H, H_COUNT'length));
			V_COUNT <= std_logic_vector(to_unsigned(V, V_COUNT'length));
			FRAME_COUNT <= std_logic_vector(to_unsigned(FRAME, FRAME_COUNT'length));
		end if;
	end process;
end architecture;

-- Display address generator: fills out the address to read bytes from for the display

library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.P_VGACONTROLLER.ALL;

entity charaddrgenerator is
	port (	CLOCK : in STD_LOGIC;
			BITMAP_MODE : in STD_LOGIC;
			H_DISPLAYING_RANGE : in STD_LOGIC;
			V_VISIBLE : in STD_LOGIC;
			H_INTERCHAR : in STD_LOGIC_VECTOR (2 downto 0);
			V_INTERCHAR : in STD_LOGIC_VECTOR (2 downto 0);
			OFFSET_ADDR : in STD_LOGIC_VECTOR (15 downto 0);
			ADDR : inout STD_LOGIC_VECTOR (15 downto 0)
	);
end entity;

architecture behavioral of charaddrgenerator is
begin
	process (CLOCK)
		variable LINE_START : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');
	begin
		if (CLOCK'Event and CLOCK = '1') then
			if (H_DISPLAYING_RANGE = '0' and V_VISIBLE = '1' and H_INTERCHAR = "000" and BITMAP_MODE = '0') then
				-- Move to the next line.
				if (V_INTERCHAR = "111") then
					-- If on the last of 8 rows, then save the curren address
					LINE_START := ADDR;
				else
					-- Otherwise role it back to the start of this line
					ADDR <= LINE_START;
				end if;
			elsif (H_INTERCHAR = "100" and H_DISPLAYING_RANGE = '1' and V_VISIBLE = '1' and BITMAP_MODE = '0') then
				-- For the attribute byte (which follows the character byte)
				ADDR <= ADDR + '1';
			elsif (H_INTERCHAR = "111" and H_DISPLAYING_RANGE = '1' and V_VISIBLE = '1') then
				ADDR <= ADDR + '1';
			elsif (V_VISIBLE = '0' and H_DISPLAYING_RANGE = '0') then
				-- Bottom of the screen, reset the display ADDR to the current OFFSET_ADDR for scrolling etc
				ADDR <= OFFSET_ADDR;
				LINE_START := ADDR;
			end if;
		end if;
	end process;

end architecture;

-- Font ROM Multiplexor: obtains data for any of the 256 chars/3 rows per char

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity fontrom is
	port (	CLOCK : in STD_LOGIC;
			ADDRESS : in STD_LOGIC_VECTOR (10 downto 0);
			DATA : out STD_LOGIC_VECTOR (7 downto 0)
	);
end entity;

architecture behavioral of fontrom is
	signal FONT0 : STD_LOGIC_VECTOR (7 downto 0);
	signal FONT1 : STD_LOGIC_VECTOR (7 downto 0);
	signal FONT2 : STD_LOGIC_VECTOR (7 downto 0);
begin
	fontrom0: entity work.fontrom0 port map (ADDRESS (7 downto 0), CLOCK, FONT0); -- 0x20 to 0x3f
	fontrom1: entity work.fontrom1 port map (ADDRESS (7 downto 0), CLOCK, FONT1); -- 0x40 to 0x5f
	fontrom2: entity work.fontrom2 port map (ADDRESS (7 downto 0), CLOCK, FONT2); -- 0x60 to 0x7f

	-- A simple muxer for the 3 x 32 char pseudo ROMs, with an inverter for when the high bit is set
	DATA <= FONT0 when (ADDRESS (10 downto 8) = "001") else
			FONT1 when (ADDRESS (10 downto 8) = "010") else
			FONT2 when (ADDRESS (10 downto 8) = "011") else
			not FONT0 when (ADDRESS (10 downto 8) = "101") else
			not FONT1 when (ADDRESS (10 downto 8) = "110") else
			not FONT2 when (ADDRESS (10 downto 8) = "111") else
			x"00";
end architecture;

-- External entity

library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.P_VGACONTROLLER.ALL;

entity vgacontroller is
	port (	VGA_CLOCK : in STD_LOGIC;
			MPU_CLOCK : in STD_LOGIC;

			-- VGA signals
			H_SYNC : out STD_LOGIC;
			V_SYNC : out STD_LOGIC;
			-- "000" (dark) to "111" (light)
			RED : out STD_LOGIC_VECTOR (2 downto 0);
			GREEN : out STD_LOGIC_VECTOR (2 downto 0);
			BLUE : out STD_LOGIC_VECTOR (2 downto 0);

			-- VRAM control and busses
			nVRAM_WRITE : out STD_LOGIC;
			nVRAM_READ : out STD_LOGIC;
			VRAM_D : inout STD_LOGIC_VECTOR (7 downto 0);
			nVRAM_CS : out STD_LOGIC_VECTOR (2 downto 0);
			VRAM_A : out STD_LOGIC_VECTOR (14 downto 0);

			-- MPU interface
			nCS : in STD_LOGIC;
			nREAD : in STD_LOGIC;
			nWRITE : in STD_LOGIC;
			D : inout STD_LOGIC_VECTOR (7 downto 0);
			A : in STD_LOGIC_VECTOR (2 downto 0);
			nDTACK : out STD_LOGIC;
			nINT : out STD_LOGIC;

			-- Other
			TESTING : out STD_LOGIC
	);
end entity;

architecture behavioral of vgacontroller is
	-- MPU bus side managed
	signal READ : STD_LOGIC := '0';
	signal WRITE : STD_LOGIC := '0';
	signal CS : STD_LOGIC := '0';
	signal REG_SELECTS : T_REG_LOGIC := (others => '0');

	-- VRAM intermediates
	signal VRAM_READ : STD_LOGIC := '0';
	signal VRAM_WRITE : STD_LOGIC := '0';
	signal VRAM_A_SIG : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');

	-- Other config
	signal BITMAP_MODE : STD_LOGIC := '0';

	-- Display data
	signal DISPLAY_ADDR : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');
	signal DISPLAY_DATA : STD_LOGIC_VECTOR (7 downto 0) := x"00";
	signal DISPLAY_ENABLE : STD_LOGIC := '0';

	-- Read/Write
	signal READWRITE_ADDR : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');
	signal NEXT_READWRITE_ADDR : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');
	signal WRITE_DATA : STD_LOGIC_VECTOR (7 downto 0) := x"00";
	signal WRITE_ENABLE : STD_LOGIC := '0';
	signal READ_DATA : STD_LOGIC_VECTOR (7 downto 0) := x"00";
	signal READ_ENABLE : STD_LOGIC := '0';

	-- Edge detection on MPU pins
	signal LAST_WRITE : STD_LOGIC := '1';
	signal LAST_READ : STD_LOGIC := '1';

	-- Scrolling
	signal OFFSET_ADDR : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');

	-- VGA generator state
	signal H_COUNT : STD_LOGIC_VECTOR (9 downto 0) := (others => '0');
	signal V_COUNT : STD_LOGIC_VECTOR (9 downto 0) := (others => '0');
	signal FRAME_COUNT : STD_LOGIC_VECTOR (5 downto 0) := (others => '0');
	signal H_DISPLAYING_RANGE : STD_LOGIC := '0';
	signal H_VISIBLE : STD_LOGIC := '0';
	signal V_VISIBLE : STD_LOGIC := '0';
	signal VISIBLE : STD_LOGIC := '0';
	alias H_INTERCHAR : STD_LOGIC_VECTOR (2 downto 0) is H_COUNT (2 downto 0);
	alias V_INTERCHAR : STD_LOGIC_VECTOR (2 downto 0) is V_COUNT (2 downto 0);
	signal LINE_ADDR : STD_LOGIC_VECTOR (15 downto 0) := (others => '0');

	-- Char ROM
	signal DISPLAY_DATA_FOR_ROM : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal FONT_ROM_CLOCK : STD_LOGIC := '0';
	signal FONT_ROM_ADDR : STD_LOGIC_VECTOR (10 downto 0) := (others => '0');
	signal FONT_ROM_DATA : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');

	-- Text mode specifics
	signal ATTRIBUTE_BYTE : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');

	-- Bitmap mode specifics
	signal DEFAULT_ATTRIBUTE : STD_LOGIC_VECTOR (7 downto 0) := x"0f";

	-- Colous, from ATTRIBUTE_BYTE
	alias F_RED : STD_LOGIC is ATTRIBUTE_BYTE (0);
	alias F_GREEN : STD_LOGIC is ATTRIBUTE_BYTE (1);
	alias F_BLUE : STD_LOGIC is ATTRIBUTE_BYTE (2);
	alias F_BRIGHT : STD_LOGIC is ATTRIBUTE_BYTE (3);
	alias B_RED : STD_LOGIC is ATTRIBUTE_BYTE (4);
	alias B_GREEN : STD_LOGIC is ATTRIBUTE_BYTE (5);
	alias B_BLUE : STD_LOGIC is ATTRIBUTE_BYTE (6);
	alias B_BRIGHT : STD_LOGIC is ATTRIBUTE_BYTE (7);

	-- Byte to display and the shifted bit
	signal BYTE_TO_DISPLAY : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal GLYTH_PIXEL : STD_LOGIC := '0';

begin

	--
	-- MPU CLOCK DOMAIN
	--

	-- Internal positive logic
	CS <= not nCS;
	READ <= not nREAD;
	WRITE <= not nWRITE;

	regaddrdecode: entity work.regaddrdecode port map(MPU_CLOCK, CS, A, REG_SELECTS);

	process (MPU_CLOCK)
	begin
		if (MPU_CLOCK'Event and MPU_CLOCK = '1') then
			-- This should only be needed if the MPU is dealyed due to /DTACK
			LAST_WRITE <= WRITE;
			LAST_READ <= READ;

			if (WRITE_ENABLE = '1' or READ_ENABLE = '1') then
				-- This falling edge will propogate to the VRAM handler
				WRITE_ENABLE <= '0';
				READ_ENABLE <= '0';
				NEXT_READWRITE_ADDR <= READWRITE_ADDR + '1';
			end if;

			if (WRITE = '1' and LAST_WRITE = '0') then
				if (REG_SELECTS(REG_DATA) = '1') then
					WRITE_DATA <= D;
					-- Trigger a write in the VGA clock domain
					WRITE_ENABLE <= '1';
					-- Use the last write's next write
					READWRITE_ADDR <= NEXT_READWRITE_ADDR;
				elsif (REG_SELECTS(REG_DEFAULT_ATTRIBUTE) = '1') then
					DEFAULT_ATTRIBUTE <= D;
				elsif (REG_SELECTS(REG_MODE1) = '1') then
					BITMAP_MODE <= D (0);
				elsif (REG_SELECTS(REG_MODE2) = '1') then
					-- Unused
				elsif (REG_SELECTS(REG_READWRITE_ADDR_HI) = '1') then
					NEXT_READWRITE_ADDR <= D (7 downto 0) & NEXT_READWRITE_ADDR (7 downto 0);
				elsif (REG_SELECTS(REG_READWRITE_ADDR_LO) = '1') then
					NEXT_READWRITE_ADDR <= NEXT_READWRITE_ADDR (15 downto 8) & D;
				elsif (REG_SELECTS(REG_OFFSET_ADDR_HI) = '1') then
					OFFSET_ADDR <= D (7 downto 0) & OFFSET_ADDR (7 downto 0);
				elsif (REG_SELECTS(REG_OFFSET_ADDR_LO) = '1') then
					OFFSET_ADDR <= OFFSET_ADDR (15 downto 8) & D;
				end if;
			end if;

			if (READ = '1' and LAST_READ = '0') then
				if (REG_SELECTS(REG_DATA) = '1') then
					-- A read in the VGA clock domain fills out READ_DATA
					READ_ENABLE <= '1';
					READWRITE_ADDR <= NEXT_READWRITE_ADDR;
				end if;
			end if;
		end if;
	end process;

	--
	-- VGA CLOCK DOMAIN
	--
	vramcontroller: entity work.vramcontroller port map(VGA_CLOCK,
		WRITE_ENABLE, READWRITE_ADDR, WRITE_DATA,
		READ_ENABLE, READ_DATA,
		DISPLAY_ENABLE, DISPLAY_ADDR, DISPLAY_DATA,
		VRAM_D, VRAM_A_SIG, VRAM_READ, VRAM_WRITE);
	vgagenerator: entity work.vgagenerator port map(VGA_CLOCK,
		H_SYNC, V_SYNC, H_DISPLAYING_RANGE, H_VISIBLE, V_VISIBLE,
		H_COUNT, V_COUNT, FRAME_COUNT);
	VISIBLE <= H_VISIBLE and V_VISIBLE;
	charaddrgenerator: entity work.charaddrgenerator port map(VGA_CLOCK, BITMAP_MODE,
		H_DISPLAYING_RANGE, V_VISIBLE,
		H_INTERCHAR, V_INTERCHAR, OFFSET_ADDR, DISPLAY_ADDR);

	process (VGA_CLOCK)
	begin
		if (VGA_CLOCK'Event and VGA_CLOCK = '1') then
			-- Text mode: ignore flyback and leave memory bus idle
			if (BITMAP_MODE = '0' and H_DISPLAYING_RANGE = '1' and V_VISIBLE = '1') then
				if (H_INTERCHAR = "000") then
					-- Clear the font ROM clock for the previous read
					FONT_ROM_CLOCK <= '0';
					-- Request a memory read, answer in DISPLAY_DATA, in 3 ticks for the character
					DISPLAY_ENABLE <= '1';
				elsif (H_INTERCHAR = "001") then
					-- Clear the memory read request
					DISPLAY_ENABLE <= '0';
				elsif (H_INTERCHAR = "011") then
					-- The data obtained forms the font ROM read address
					DISPLAY_DATA_FOR_ROM <= DISPLAY_DATA;
				elsif (H_INTERCHAR = "100") then
					-- Request a memory read, answer in DISPLAY_DATA, in 3 ticks for the attribute
					DISPLAY_ENABLE <= '1';
				elsif (H_INTERCHAR = "101") then
					-- Clear the memory read request
					DISPLAY_ENABLE <= '0';
					-- Request a memory read on the font ROM, answer in FONT_ROM_DATA in next clock
					FONT_ROM_CLOCK <= '1';
				elsif (H_INTERCHAR = "111") then
					if (NEXT_READWRITE_ADDR (15 downto 1) = DISPLAY_ADDR (15 downto 1) and FRAME_COUNT (5) = '1') then
						-- Swap attributes for the cursor
						if (DISPLAY_DATA /= x"00") then
							ATTRIBUTE_BYTE <= DISPLAY_DATA (3 downto 0) & DISPLAY_DATA (7 downto 4);
						else
							ATTRIBUTE_BYTE <= DEFAULT_ATTRIBUTE (3 downto 0) & DISPLAY_DATA (7 downto 4);
						end if;
					else
						-- Use the DEFAULT_ATTRIBUTE if memory attribute is 0
						if (DISPLAY_DATA /= x"00") then
							ATTRIBUTE_BYTE <= DISPLAY_DATA;
						else
							ATTRIBUTE_BYTE <= DEFAULT_ATTRIBUTE;
						end if;
					end if;
					-- BYTE_TO_DISPLAY is set by text and bitmap modes
					BYTE_TO_DISPLAY <= FONT_ROM_DATA;
				end if;
			end if;

			-- Bitmap mode: ignore flyback and leave memory bus idle
			if (BITMAP_MODE = '1' and H_DISPLAYING_RANGE = '1' and V_VISIBLE = '1') then
				if (H_INTERCHAR = "000") then
					-- Request a memory read, answer in DISPLAY_DATA, in 3 ticks for the pixel byte
					DISPLAY_ENABLE <= '1';
				elsif (H_INTERCHAR = "001") then
					-- Clear the memory read request
					DISPLAY_ENABLE <= '0';
				elsif (H_INTERCHAR = "111") then
					-- BYTE_TO_DISPLAY is on the common path for text and bitmap modes
					BYTE_TO_DISPLAY <= DISPLAY_DATA;
					-- Use the register for the attribute in bitmap mode
					ATTRIBUTE_BYTE <= DEFAULT_ATTRIBUTE;
				end if;
			end if;
		end if;
	end process;

	-- Concenate the vertical interchar position (0-7) with the character to display
	FONT_ROM_ADDR <= DISPLAY_DATA_FOR_ROM (7 downto 0) & V_INTERCHAR;
	fontrom: entity work.fontrom port map (FONT_ROM_CLOCK, FONT_ROM_ADDR, FONT_ROM_DATA);

	process (VGA_CLOCK)
	begin
		if (VGA_CLOCK'Event and VGA_CLOCK = '1') then
			if (VISIBLE = '1') then
				-- Read the pixel state out of BYTE_TO_DISPLAY, reading one bit at a time, left to right 
				GLYTH_PIXEL <= BYTE_TO_DISPLAY (to_integer(unsigned(not H_INTERCHAR)));
			else
				-- Not a visible pixel; make pixel always off
				GLYTH_PIXEL <= '0';
			end if;
		end if;
	end process;

	-- Cater for visible or not, on or off, and bright or dark pixels
	RED <=		F_RED & F_RED & F_RED 		when (GLYTH_PIXEL = '1' and F_BRIGHT = '1' and VISIBLE = '1') else
				'0' & F_RED & F_RED			when (GLYTH_PIXEL = '1' and F_BRIGHT = '0' and VISIBLE = '1') else
				B_RED & B_RED & B_RED		when (GLYTH_PIXEL = '0' and B_BRIGHT = '1' and VISIBLE = '1') else
				'0' & B_RED & B_RED			when (GLYTH_PIXEL = '0' and B_BRIGHT = '0' and VISIBLE = '1') else
				"000";
	GREEN <=	F_GREEN & F_GREEN & F_GREEN when (GLYTH_PIXEL = '1' and F_BRIGHT = '1' and VISIBLE = '1') else
				'0' & F_GREEN & F_GREEN		when (GLYTH_PIXEL = '1' and F_BRIGHT = '0' and VISIBLE = '1') else
				B_GREEN & B_GREEN & B_GREEN	when (GLYTH_PIXEL = '0' and B_BRIGHT = '1' and VISIBLE = '1') else
				'0' & B_GREEN & B_GREEN		when (GLYTH_PIXEL = '0' and B_BRIGHT = '0' and VISIBLE = '1') else
				"000";
	BLUE <=		F_BLUE & F_BLUE & F_BLUE 	when (GLYTH_PIXEL = '1' and F_BRIGHT = '1' and VISIBLE = '1') else
				'0' & F_BLUE & F_BLUE		when (GLYTH_PIXEL = '1' and F_BRIGHT = '0' and VISIBLE = '1') else
				B_BLUE & B_BLUE & B_BLUE	when (GLYTH_PIXEL = '0' and B_BRIGHT = '1' and VISIBLE = '1') else
				'0' & B_BLUE & B_BLUE		when (GLYTH_PIXEL = '0' and B_BRIGHT = '0' and VISIBLE = '1') else
				"000";

	nVRAM_READ <= not VRAM_READ;
	nVRAM_WRITE <= not VRAM_WRITE;
	-- Simple decoder for the chip selects
	nVRAM_CS <=	"110" when VRAM_A_SIG (15) = '0' else
				"101" when VRAM_A_SIG (15) = '1' else
				-- TODO: do something with the 3rd 32KB SRAM
				"111";
	-- The low bits of the VRAM address
	VRAM_A <= VRAM_A_SIG (14 downto 0);

	-- TODO: DTACK?
	nDTACK <= '0';
	-- TODO: Add configurable interrupts for end of line, end of frame etc?
	nINT <= '1';

	-- Pass the last byte read, if the data register is selected
	D <=	READ_DATA when (READ = '1' and REG_SELECTS(REG_DATA) = '1') else
			-- We could allow reads on the other registers but it doesn't really have any utility
			"ZZZZZZZZ";

	TESTING <= '0';

end architecture;
