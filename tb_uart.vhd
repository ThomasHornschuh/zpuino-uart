--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   18:56:00 11/01/2017
-- Design Name:   
-- Module Name:   /home/thomas/fusesoc_projects/zpuino-uart/tb_uart.vhd
-- Project Name:  zpuino-uart_0
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: zpuino_uart
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY tb_uart IS
END tb_uart;
 
ARCHITECTURE behavior OF tb_uart IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT zpuino_uart
    PORT(
         wb_clk_i : IN  std_logic;
         wb_rst_i : IN  std_logic;
         wb_dat_o : OUT  std_logic_vector(31 downto 0);
         wb_dat_i : IN  std_logic_vector(31 downto 0);
         wb_adr_i : IN  std_logic_vector(2 downto 2);
         wb_we_i : IN  std_logic;
         wb_cyc_i : IN  std_logic;
         wb_stb_i : IN  std_logic;
         wb_ack_o : OUT  std_logic;
         wb_inta_o : OUT  std_logic;
         id : OUT  std_logic_vector(15 downto 0);
         enabled : OUT  std_logic;
         tx : OUT  std_logic;
         rx : IN  std_logic
        );
    END COMPONENT;
    

   --Inputs
   signal wb_clk_i : std_logic := '0';
   signal wb_rst_i : std_logic := '0';
   signal wb_dat_i : std_logic_vector(31 downto 0) := (others => '0');
   signal wb_adr_i : std_logic_vector(2 downto 2) := (others => '0');
   signal wb_we_i : std_logic := '0';
   signal wb_cyc_i : std_logic := '0';
   signal wb_stb_i : std_logic := '0';
   signal rx : std_logic := '0';

 	--Outputs
   signal wb_dat_o : std_logic_vector(31 downto 0);
   signal wb_ack_o : std_logic;
   signal wb_inta_o : std_logic;
   signal id : std_logic_vector(15 downto 0);
   signal enabled : std_logic;
   signal tx : std_logic;
   -- No clocks detected in port list. Replace clk below with 
   -- appropriate port name 
 
   constant clk_period : time := 10 ns;
   
   signal clk : std_logic;
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: zpuino_uart PORT MAP (
          wb_clk_i => wb_clk_i,
          wb_rst_i => wb_rst_i,
          wb_dat_o => wb_dat_o,
          wb_dat_i => wb_dat_i,
          wb_adr_i => wb_adr_i,
          wb_we_i => wb_we_i,
          wb_cyc_i => wb_cyc_i,
          wb_stb_i => wb_stb_i,
          wb_ack_o => wb_ack_o,
          wb_inta_o => wb_inta_o,
          id => id,
          enabled => enabled,
          tx => tx,
          rx => rx
        );

   -- Clock process definitions
   clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;	

      wait for clk_period*10;

      -- insert stimulus here 

      wait;
   end process;

END;
