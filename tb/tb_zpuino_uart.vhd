-- TH: Enhanced UART Testbench to test UART receiver and sender more deeply
--  Copyright 2017 Thomas Hornschuh
--
--  Version: 1.0
--
--  The FreeBSD license
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above
--     copyright notice, this list of conditions and the following
--     disclaimer in the documentation and/or other materials
--     provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
--  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
--  ZPU PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
--  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
--  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
--  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
--  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;
use IEEE.MATH_REAL.ALL;

library STD;
use STD.textio.all;

use work.txt_util.all;
use work.log2.all;

entity tb_zpuino_uart is
generic (
  clk_frequency_mhz : real := 32.0;
  ext_mode : boolean := true;
  baudrate : natural := 38400
);
end entity tb_zpuino_uart;

architecture testbench of tb_zpuino_uart is


    constant clk_period : time := (1.0 / clk_frequency_mhz * real(10**6)) * 1 ps;
    constant clk_frequency : natural := natural( clk_frequency_mhz * real(10**6));

    signal TbClock : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

     -- Clock signal:
    signal clk : std_logic := '0';

    -- Reset signal:
    signal reset : std_logic := '1';

    -- UART ports:
    signal txd : std_logic;
    signal rxd : std_logic := '1';

    -- interrupt signals:
    signal irq : std_logic;

    -- Wishbone ports:
    signal wb_adr_in  : std_logic_vector(3 downto 2) := (others => '0');
    signal wb_dat_in  : std_logic_vector(31 downto 0) := (others => '0');
    signal wb_dat_out : std_logic_vector(31 downto 0);
    signal wb_we_in   : std_logic := '0';
    signal wb_cyc_in  : std_logic := '0';
    signal wb_stb_in  : std_logic := '0';
    signal wb_ack_out : std_logic;

   constant Teststr : string :="The quick brown fox jumps over the lazy dog";

   subtype t_byte is std_logic_vector(7 downto 0);

   --constant baudrate : natural := 115200;
   --constant baudrate : natural := 500000;
   --constant bit_time : time := 8.68 us;

   constant bit_time : time :=   (1.0 / real(baudrate) *  real(10**6)) *  1 us;
   signal cbyte : t_byte;
   signal bitref : integer :=0;

   signal framing_errors :  natural;
   signal total_count :  natural;

   signal rx_sent_bytes : natural := 0; -- Counter of bytes sent to to rx pin for receive test 

   signal receive_test_finish : boolean := false;
   signal send_test_finish : boolean := false;

   constant log_file : string := "receive.log";
   constant send_logfile : string := "send.log";

   type t_comp is  array( 0 to 1000 ) of t_byte; 
   signal recv_compare : t_comp;


   procedure write_byte(file f: TEXT; c:t_byte) is
   variable s : string(1 to 1);
   begin
     if unsigned(c)>=32 and unsigned(c)<=255 then
       s(1):=character'val(to_integer(unsigned(c)));
       write(f,s);
     else
       --print("non printable char" & hstr(c));
       write(f,"x" & hstr(c)); -- non printable char
     end if;
   end;


begin

    Inst_tb_uart_capture_tx: entity work.tb_uart_capture_tx
    GENERIC MAP (
      SEND_LOG_NAME => send_logfile,
      baudrate => baudrate,
      bit_time => bit_time,
      stop_mark => X"1A"
    )
    PORT MAP(
        txd => txd,
        stop => send_test_finish,
        framing_errors => framing_errors,
        total_count => total_count
     );


    wait_send : process(send_test_finish)
    begin
      if send_test_finish then
        print("");
         print("Send Test finished Total Bytes : " & str(total_count) & " Framing errors: " & str(framing_errors));
         assert framing_errors=0
            report "Send Test failed with framing errors"
            severity error;

         TbSimEnded <= '1';
      end if;

    end process;

    send_progress: process(total_count) -- will be triggered on every increement of total_count
    begin
      write(OUTPUT,".");
    end process;  

    
    uut: entity work.zpuino_uart
    generic map (
       bits => log2(32),
       wordSize =>wb_dat_out'length,
       extended => true
    )

     PORT MAP (
          wb_clk_i => clk,
          wb_rst_i => reset,
          wb_dat_o => wb_dat_out,
          wb_dat_i => wb_dat_in,
          wb_adr_i => wb_adr_in,
          wb_we_i => wb_we_in,
          wb_cyc_i => wb_cyc_in,
          wb_stb_i => wb_stb_in,
          wb_ack_o => wb_ack_out,
          wb_inta_o => irq,
          id => open,
          enabled => open,
          tx => txd,
          rx => rxd
        );


 -- Clock generation
    TbClock <= not TbClock after clk_period/2 when TbSimEnded /= '1' else '0';
    clk <= TbClock;
--


-- Simulates a serial bit stream to the rxd pin
   rxd_sim: process
      procedure send_byte(v: t_byte) is
      variable bi : integer;
      variable t : std_logic_vector(7 downto 0);
      begin

        recv_compare(rx_sent_bytes) <= v; 
        rx_sent_bytes <= rx_sent_bytes + 1; 

        bi:=7;
        for i in 0 to 7 loop
         t(bi) := v(i); -- for debugging purposes
         bi:=bi-1;
        end loop;
        cbyte <= t;

        bitref<= 0;

        rxd <= '0'; -- start bit
        for i in 0 to 7 loop
          wait for bit_time;
          rxd<=v(i);
          bitref<=bitref+1;
        end loop;
        wait for bit_time;
        rxd <= '1'; -- stop bit
        bitref<=bitref+1;
        wait for bit_time;

        
      end;

      procedure sendstring(s:string) is
      begin
        for i in 1 to s'length loop
          send_byte(std_logic_vector(to_unsigned(character'pos(s(i)),8)));
        end loop;
      end;

   begin
       wait for bit_time*10.5;

       --Optional "Clock drift" test
--       for i in 1 to 1000 loop
--          send_byte(X"55");
--          bit_time <= bit_time + 0.004us; -- drift bit time
--       end loop;

        send_byte("01010101");

        for i in 1 to 126 loop
           send_byte(std_logic_vector(to_unsigned(i,8)));
        end loop;


       -- Send a string to the UART receiver pin
       sendstring(Teststr);
       wait for bit_time*10; -- give some time for the receive process
       receive_test_finish<=true; -- signal end of send simulation
       wait;

   end process;


    stimulus: process
      variable divisor : natural;
      variable count : natural;
      variable error_count : natural;

      procedure uart_write(address : in std_logic_vector(wb_adr_in'range); data : in std_logic_vector(wb_dat_in'range)) is
        begin

            wait until rising_edge(clk);
            wb_adr_in <= address;
            wb_dat_in <= data;
            wb_we_in <= '1';
            wb_cyc_in <= '1';
            wb_stb_in <= '1';

            --wait until wb_ack_out = '1';
            wait  until rising_edge(clk) and  wb_ack_out = '1';
            wb_stb_in <= '0';
            wb_cyc_in <= '0';
            wb_dat_in <= (others=>'X');
            wb_we_in <= 'X';

        end procedure;

      procedure uart_read(address : in std_logic_vector(wb_adr_in'range);
                          data: out std_logic_vector(wb_dat_out'range) )  is
        begin

            wait until rising_edge(clk);
            wb_adr_in <= address;
            wb_we_in <= '1';
            wb_cyc_in <= '1';
            wb_stb_in <= '1';
            wb_we_in <= '0';
            wait until rising_edge(clk) and wb_ack_out = '1';
            data:= wb_dat_out;
            --wait until rising_edge(clk);
            wb_stb_in <= '0';
            wb_cyc_in <= '0';
           --wait for clk_period;
        end procedure;

        procedure uart_tx(byte:t_byte) is
        variable status : std_logic_vector(31 downto 0);
        begin
           status:=(others=>'U');
           while status(1)/='1'  loop
             uart_read("01",status);
           end loop;
           uart_write("00",X"000000" & byte);
        end;

      variable status,rx_byte,ctl : std_logic_vector(31 downto 0);
      variable s: string(1 to 1);
      file l_file: TEXT;
      variable L : line;
   


    begin
        file_open(l_file,log_file,WRITE_MODE);
        wait for clk_period * 2;
        reset <= '0';
        ctl:=(others=>'0');
        print("Clock Frequency: " & str(clk_frequency));
        print("Baudrate:" & str(Baudrate) );
        -- Bit Period print
        write(OUTPUT,"Bit period: ");  
        write(L, bit_time, unit => 1 us);
        writeline(OUTPUT,L);
        
        if ext_mode then
          print("Using ext_mode");
          divisor:= natural( real(clk_frequency) / real(baudrate)) - 1;
          ctl(17):='1';
        else
          divisor:= natural( real(clk_frequency) / real(baudrate*16)) - 1;
        end if;
        print("UART Divisor: " & str(divisor));
        ctl(15 downto 0):=std_logic_vector(to_unsigned(divisor,16));
        ctl(16):='1';

        uart_write("10",ctl);  -- Initalize UART
        count:=0;
        error_count:=0;
        --receive loop
        while not receive_test_finish loop
          -- Check Status Register
           status := (others=>'0');
           while status(0)='0'  and not receive_test_finish loop -- check ready bit (bit 0)
             uart_read("01",status);
           end loop;
           -- Get byte
           if status(0)='1' then
             uart_read("00",rx_byte);
             write_byte(l_file,rx_byte(7 downto 0));
             write_byte(output,rx_byte(7 downto 0));
             if rx_byte(31)='1' then
               print("Framing Error at position: " & str(count));
               error_count:=error_count+1; 
             end if;
             if rx_byte(t_byte'range) /= recv_compare(count) then
               print("Receive error at position: " & str(count) );
               error_count:=error_count+1; 
             end if;   
             count:=count+1;
           end if;
       end loop;

       file_close(l_file);
       if rx_sent_bytes = count or error_count > 0 then 
         print("Receive Simulation finished");
       else 
         print("Receive simulation error");
       end if;   
       print("Expected bytes: " & str(rx_sent_bytes) & " received bytes: " & str(count) &
             " error bytes: " & str(error_count)  );    


       -- UART Send Simulation
       for i in 1 to 5 loop
          uart_tx(X"55");
       end loop;
       for i in 1 to TestStr'length loop
          uart_tx(char_to_ascii_byte(TestStr(i)));
       end loop;
       uart_tx(X"1A"); -- eof
       wait;
    end process stimulus;

end architecture testbench;
