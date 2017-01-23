-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2016 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Jakub Svoboda
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
 
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
	--instructions
	type instructions is (ptrInc, ptrDec, valueInc, valueDec, whileBegin, whileEnd, valuePrint, valueRead, load, store, stop, noop);
	signal instructionsPtr: instructions;
	signal instructionsPtr2: instructions;
	signal instructionsReg: std_logic_vector (7 downto 0);
	signal instructionLoad: std_logic;
	
	--cpu states
	type states is (s_store2, s_store3, s_fetch, s_fetch2, s_ptrInc, s_ptrDec, s_valueInc, s_valueInc1, s_valueInc2, s_valueDec, s_valueDec1, s_valueDec2, s_whileBegin, s_whileBegin2, s_whileFetch, s_whileFetch2, s_whileEnd2,s_whileEnd3,s_whileEnd4, s_whileEndFetch, s_whileEndFetch2, s_whileContinue, s_whileEnd, s_valuePrint, s_valuePrint2, s_valuePrint3, s_valueRead, s_valueRead2, s_load, s_load2, s_store, s_stop, s_idle, s_decode);
	signal statesPtr: states;
	signal  statesNextPtr: states;

	--pointer	
	signal ptrIncrement: std_logic;
	signal ptrDecrement: std_logic;
	signal ptrSet: std_logic;
	signal ptrRegister: std_logic_vector (9 downto 0);
	
	--counter
	signal counterIncrement: std_logic;
	signal counterDecrement: std_logic;
	signal counterSet: std_logic;
	signal counterRegister: std_logic_vector (7 downto 0);
	
	--pc
	signal pcIncrement: std_logic;
	signal pcDecrement: std_logic;
	signal pcSet: std_logic;
	signal pcRegister: std_logic_vector (11 downto 0);
	
	signal writetmp: std_logic;
	signal TMP: std_logic_vector (7 downto 0);
	signal selector: std_logic_vector(1 downto 0); --MX selector
	signal rdataNormal: std_logic_vector(7 downto 0);
	signal rdataInc: std_logic_vector(7 downto 0);
	signal rdataDec: std_logic_vector(7 downto 0);
	signal rdataTmp: std_logic_vector(7 downto 0);
	signal rdataTmp2: std_logic_vector(7 downto 0);
	signal rdataBin: std_logic;

begin
 -- zde dopiste vlastni VHDL kod
	loadInstruction: process(CLK,RESET)
		begin
			if (RESET='1') then
				instructionsReg <= (others=>'0');								--if reset has been pressed, reset the register
			elsif ((CLK'event) and (CLK = '1')) then		
				if(instructionLoad = '1') then
					instructionsReg <= CODE_DATA;										--else load instruction	to instruction register
				end if;
			end if;
		end process;	
			
		
--Program Counter Register		
	PC: process (CLK, RESET)
		begin
			if (RESET = '1')then
				pcRegister <= "000000000000";											--nulify when reset
			elsif ((CLK'event) and (CLK = '1')) then
				if (pcIncrement = '1') and (pcDecrement = '0') then
					pcRegister <= pcRegister+1;										--increment the value of pcRegister
				elsif (pcIncrement = '0') and (pcDecrement = '1')	then
					pcRegister <= pcRegister-1;										--decrement the value of pcRegister
				end if;			
			end if;
		end process;
		
	CODE_ADDR <= pcRegister when pcSet = '1' else (others => 'Z');				
		
--Pointer Register
	PTR: process (CLK, RESET)
		begin
			if (RESET = '1')then
				ptrRegister <= "0000000000";											--nulify when reset
			elsif ((CLK'event) and (CLK = '1')) then
				if (ptrIncrement = '1') and (ptrDecrement = '0') then
					ptrRegister <= ptrRegister+1;										--increment the value of ptrRegister
				elsif (ptrIncrement = '0') and (ptrDecrement = '1')	then
					ptrRegister <= ptrRegister-1;										--decrement the value of ptrRegister
				end if;			
			end if;
		end process;
		
	DATA_ADDR <= ptrRegister when ptrSet = '1' else (others => 'Z');		-- DATA to register
		
--Counter Register
	COUNTER: process (CLK, RESET)
		begin
			if (RESET = '1')then
				counterRegister <= "00000000";											--nulify when reset
			elsif ((CLK'event) and (CLK = '1')) then
				if(counterSet = '0')then
					if (counterIncrement = '1') and (counterDecrement = '0') then
						counterRegister <= counterRegister+1;										--increment the value of counterRegister
					elsif (counterIncrement = '0') and (counterDecrement = '1')	then
						counterRegister <= counterRegister-1;										--decrement the value of counterRegister
					end if;
				else
					counterRegister <= "00000001";
				end if;
			end if;
		end process;	

--decode instruction:
	DECODE: process(CODE_DATA)
		begin
			case(CODE_DATA)is
				when X"3E" => instructionsPtr <= ptrInc;
            when X"3C" => instructionsPtr <= ptrDec;
            when X"2B" => instructionsPtr <= valueInc;
            when X"2D" => instructionsPtr <= valueDec;
            when X"5B" => instructionsPtr <= whileBegin;
            when X"5D" => instructionsPtr <= whileEnd;
            when X"2E" => instructionsPtr <= valuePrint;
            when X"2C" => instructionsPtr <= valueRead;
            when X"24" => instructionsPtr <= store;
            when X"21" => instructionsPtr <= load;
            when X"00" => instructionsPtr <= stop;
            when others => instructionsPtr <= noop;
			end case;
		end process;



	MINIPLEX: process (DATA_RDATA, CLK)
		begin
			rdataTmp <= DATA_RDATA;
			rdataTmp2 <= DATA_RDATA;
			if( DATA_RDATA = "00000000")then
				rdataBin <= '1';
			else
				rdataBin <= '0';
			end if;	
		end process;
	
	
	MULTIPLEXOR_PREV: process (DATA_RDATA,ptrRegister)
		begin
			rdataNormal <= DATA_RDATA;
			rdataInc <= DATA_RDATA+1;
			rdataDec <= DATA_RDATA-1;	
		end process;
		
--MX
	MULTIPLEXOR: process(IN_DATA, selector, rdataInc,DATA_RDATA, writeTmp)  
		begin
				--DATA_RDWR <= '0';
			if (writeTmp = '0') then	
				if(selector = "00")then
					DATA_WDATA <= rdataInc;
				elsif(selector = "01")then
					DATA_WDATA <= rdataDec;
				elsif(selector = "10")then
					DATA_WDATA <= IN_DATA;	
				else
					DATA_WDATA <= DATA_RDATA;
				end if;
			else
				DATA_WDATA <= TMP;
			end if;	
		end process;		

--Current State
	STATES_PROCESS: process (CLK, EN, RESET)
		begin
			if (RESET='1') then
				statesPtr <= s_idle;
			elsif	((CLK'event) and (CLK='1')) then
				if (EN='1') then
					statesPtr <= statesNextPtr;
				end if;
			end if;
		end process;

--Next State
	NEST_STATE: process (statesPtr, IN_VLD, OUT_BUSY, instructionsPtr)
		begin
			pcIncrement	 		<= '0';
			pcDecrement 		<= '0';
			ptrIncrement		<= '0';
			ptrDecrement		<= '0';
			counterIncrement 	<= '0';
			counterDecrement 	<= '0';
			ptrSet <= '0';
			pcSet <= '0';
			counterSet <= '0';
			selector <= "11"; --default value where no action is performed 
			instructionLoad <= '0';
			writetmp <= '0';
	
			CODE_EN <= '0';
			DATA_EN <= '0';
			DATA_RDWR <= '0';
			IN_REQ <= '0';
			OUT_WE <= '0';
		
			case statesPtr is
				when s_fetch => 
					statesNextPtr <= s_fetch2;
					CODE_EN <= '1';
					pcSet <= '1';
					DATA_RDWR <= '1';
					DATA_EN <= '1';
				when s_fetch2 =>
					statesNextPtr <= s_decode;
					instructionLoad <= '1';
					DATA_RDWR <= '1';
					DATA_EN <= '1';	
				when s_decode =>
					case instructionsPtr is
						when ptrInc	=> statesNextPtr <= s_ptrInc;			-- >
						when ptrDec	=> statesNextPtr <= s_ptrDec;			-- <
						when valueInc	=> statesNextPtr <= s_valueInc;		-- +
						when valueDec	=> statesNextPtr <= s_valueDec;		-- -	
						when whileBegin	=> statesNextPtr <= s_whileBegin;	-- [
						when whileEnd	=> statesNextPtr <= s_whileEnd;		-- ]
						when valuePrint	=> statesNextPtr <= s_valuePrint;	-- .
						when valueRead	=> statesNextPtr <= s_valueRead;		-- ,	
						when load	=> statesNextPtr <= s_load;			-- $
						when store	=> statesNextPtr <= s_store;			-- !
						when stop	=> statesNextPtr <= s_stop;			-- null	
						when others => statesNextPtr <= s_idle;			-- no instruction to execute
					end case;
				
					
				when s_idle => 				
					statesNextPtr <= s_fetch;
					--pcIncrement <= '1';
				when s_stop =>
					statesNextPtr <= s_stop;
				when s_ptrInc =>							-- < increment pointer
					ptrIncrement <= '1';		
					ptrDecrement <= '0';
					pcIncrement <= '1';	
					ptrSet <= '1';
					DATA_EN <= '1';
					DATA_RDWR <= '1';
					statesNextPtr <= s_fetch;
				when s_ptrDec=>							-- > decrement pointer
					ptrIncrement <= '0';	
					ptrDecrement <= '1';
					pcIncrement <= '1';	
					ptrSet <= '1';
					DATA_EN <= '1';
					DATA_RDWR <= '1';
					statesNextPtr <= s_fetch;
				when s_valueInc =>						-- + increment value
					ptrSet <= '1';
					DATA_RDWR <= '1';
					DATA_EN <= '1';
					statesNextPtr <= s_valueInc2;
				when s_valueInc2 =>
					ptrSet <= '1';
					DATA_RDWR <= '0';
					DATA_EN <= '1';	
					pcIncrement <= '1';
					statesNextPtr <= s_fetch;
					selector <= "00";
				when s_valueDec=>						-- - decrement value
					DATA_RDWR <= '1';
					DATA_EN <= '1';
					ptrSet <= '1';				
					statesNextPtr <= s_valueDec2;	
				when s_valueDec2=>	
					ptrSet <= '1';	
					DATA_RDWR <= '0';
					DATA_EN <= '1';
					pcIncrement <= '1';
					statesNextPtr <= s_fetch;
					selector <= "01";
				when s_valuePrint =>								-- . print
					DATA_EN <= '1';
					DATA_RDWR <= '1';
					if (OUT_BUSY = '1') then
						statesNextPtr <= s_valuePrint;		-- cycle here until not busy
					else
						statesNextPtr <= s_valuePrint2;		-- then jump to next state
						ptrSet <= '1';
					end if;					
				when s_valuePrint2 =>			
					DATA_EN <= '1';
					DATA_RDWR <= '1';
					ptrSet <= '1';	
					statesNextPtr <= s_valuePrint3;			
				when s_valuePrint3 =>		
					OUT_WE <= '1';
					OUT_DATA <= DATA_RDATA;
					DATA_EN <= '1';
					DATA_RDWR <= '1';
					pcIncrement <= '1';
					statesNextPtr <= s_fetch;
					ptrSet <= '1';
				when s_valueRead =>								-- , read
					IN_REQ <= '1';
					if (IN_VLD = '0')then
						statesNextPtr <= s_valueRead;	
					else
						statesNextPtr <= s_valueRead2;	
					end if;
				when s_valueRead2 =>
					DATA_EN <= '1';
					DATA_RDWR <= '0'; 			--TODO make sure its correct
					selector <= "10";
					ptrSet <= '1';
					pcIncrement <= '1';
					statesNextPtr <= s_fetch;
					IN_REQ <= '0';
				
				
				when s_whileBegin =>
					statesNextPtr <= s_whileBegin2;
					pcIncrement <= '1';	
					ptrSet <= '1';
					DATA_RDWR <= '1';		 			--TODO make sure its correct	
					DATA_EN <= '1';
				when s_whileBegin2 =>
					if(DATA_RDATA /= "00000000")then
						statesNextPtr <= s_fetch;
					else
						counterSet <= '1';		--reset counter
						statesNextPtr <= s_whileFetch;
					end if;	
				when s_whileFetch =>
					statesNextPtr <= s_whileFetch2;
					CODE_EN <= '1';
					pcSet <= '1';
					DATA_RDWR <= '1';
					DATA_EN <= '1';
				when s_whileFetch2 =>
					statesNextPtr <= s_whileContinue;
					instructionLoad <= '1';
					DATA_RDWR <= '1';
					DATA_EN <= '1';
				when s_whileContinue =>
					pcIncrement <= '1';				--TODO check
					if(counterRegister = "00000000") then
						statesNextPtr <= s_fetch;
					else
						statesNextPtr <= s_whileFetch;
						if(instructionsReg = "000001011101") then		--right bracket
							counterDecrement <= '1';
						elsif (instructionsReg = "000001011011") then		--left bracket
							counterIncrement <= '1';
						end if;	
					end if;
				
				when s_whileEnd =>
					ptrSet <= '1';
					DATA_EN <= '1';
					DATA_RDWR <= '1';		--TODO check
					statesNextPtr <= s_whileEnd2;
				when s_whileEnd2 =>
					if(DATA_RDATA /= "00000000")then
						pcDecrement <='1';
						counterSet <= '1';
						statesNextPtr <= s_whileEndFetch;
					else
						pcIncrement <= '1';
						statesNextPtr <= s_fetch;
					end if;	
				when s_whileEndFetch =>
					CODE_EN <= '1';
					DATA_RDWR <= '1';	
					pcSet <= '1';
					statesNextptr <= s_whileEndFetch2;
				when s_whileEndFetch2 =>
					statesNextPtr <= s_whileEnd3;
					instructionLoad <= '1';
				when s_whileEnd3 =>
					if(counterRegister = "000000000000") then
						statesNextPtr <= s_fetch;
					else
						statesNextPtr <= s_whileEnd4;
						if(instructionsReg = "000001011101") then		--right bracket
							counterIncrement <= '1';
						elsif (instructionsReg = "000001011011") then		--left bracket
							counterDecrement <= '1';
						end if;	
					end if;
				when s_whileEnd4 =>
					statesNextPtr <= s_whileEndFetch;
					if (counterRegister /= "000000000000") then
						pcDecrement <= '1';
					else 
						pcIncrement <= '1';
					end if;
				
				when s_store =>				--$
					ptrSet <= '1';
					statesNextPtr <= s_store2;
					DATA_EN <= '1';
					DATA_RDWR <='1';
				when s_store2 =>
					ptrSet <= '1';
					statesNextPtr <= s_store3;
					DATA_EN <= '1';
					DATA_RDWR <='1';
				when s_store3 =>
					ptrSet <= '1';
					statesNextPtr <= s_fetch;
					DATA_EN <= '1';
					DATA_RDWR <='1';
					TMP <= DATA_RDATA;
					pcIncrement <= '1';
				
				when s_load =>			--!
					ptrSet<='1';
					statesNextPtr <= s_load2;
					DATA_EN <= '1';
					DATA_RDWR <='0';	
					writeTmp <= '1';
				when s_load2 =>
					ptrSet <= '1';
					statesNextPtr <= s_fetch;
					DATA_EN <= '1';
					DATA_RDWR <='0';
					writeTmp <= '1';
					pcIncrement <= '1';
				
				
				when others => 
					pcIncrement <= '1';
					
				end case;	
		end process;		

end behavioral;
 
