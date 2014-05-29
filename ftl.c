
#include "jasmine.h"

//----------------------------------
// macro
//----------------------------------
#define BYTES_PER_BLK		(BYTES_PER_PAGE * PAGES_PER_BLK)
#define VC_MAX              0xCDCD
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata

#define LPMAP_PER_BANK		(LPAGE_MAP_BYTES / BYTES_PER_VBLK)
#define LBMETA_PER_BANK		(LBLK_META_BYTES / BYTES_PER_VBLK)
#define VPMAP_PER_BANK		(VPAGE_MAP_BYTES / BYTES_PER_VBLK)
#define VBMETA_PER_BANK		(VBLK_META_BYTES / BYTES_PER_VBLK)
#define EMPTYBLK_PER_BANK	(EMPTY_BLK_BYTES / BYTES_PER_VBLK)

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)

//----------------------------------
// metadata structure
//----------------------------------
typedef struct _ftl_statistics
{
    UINT32 gc_cnt;
    UINT32 page_wcount; // page write count
}ftl_statistics;

typedef struct _misc_metadata
{
    UINT32 cur_miscblk_vbn; // current write vpn for logging the misc. metadata
    UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC

    //=================================================================
    UINT32 timer;
    UINT32 cur_write_page;

    UINT32 cur_gc_victim_vcnt;
    UINT32 cur_gc_victim;

    UINT32 free_blk_cnt; // total number of free block count
    UINT32 gc_block;
    UINT32 df_block;

	UINT32 page_hit;
	UINT32 total_hit;

	UINT32 avg_cost;

    UINT32 cur_lpm_blk_vbn[LPMAP_PER_BANK]; // current write vpn for logging the age mapping info.
    UINT32 cur_lbm_blk_vbn[LBMETA_PER_BANK];
    UINT32 cur_vpm_blk_vbn[VPMAP_PER_BANK];
    UINT32 cur_vbm_blk_vbn[VBMETA_PER_BANK];
    UINT32 cur_emptyblk_vbn [EMPTYBLK_PER_BANK];
    //=================================================================

}misc_metadata; // per bank

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata  g_misc_meta[NUM_BANKS];
static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32		  g_bad_blk_count[NUM_BANKS];

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;


//
// New Implement
//

//----------------------------------
// NAND layout
//----------------------------------
// block #0: scan list, firmware binary image, etc.
// block #1: FTL misc. metadata
// block #2 : data block mapping table
// block #3 : log block mapping table
// block #4 : empty block mapping table
// block #5 : a free block for gc
// block #6~ : data blocks

//----------------------------------
// macro functions
//----------------------------------
#define inc_mapblk_vpn(bank, mapblk_lbn)    (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn]++)
#define inc_miscblk_vpn(bank)               (g_misc_meta[bank].cur_miscblk_vpn++)

// page-level striping technique (I/O parallelism)
#define get_num_bank(lpn)             ((lpn) % NUM_BANKS)
#define get_bad_blk_cnt(bank)         (g_bad_blk_count[bank])
#define get_miscblk_vbn(bank)         (g_misc_meta[bank].cur_miscblk_vbn)
#define set_miscblk_vbn(bank, vbn)    (g_misc_meta[bank].cur_miscblk_vbn = vbn)

//=================================================================
#define page_to_blk(pn)					(pn >> BLK_TO_PAGE)
#define blk_to_page(bn)					(bn << BLK_TO_PAGE)
#define page_offset(pn)					(pn & 127)

#define get_timer(bank)					(g_misc_meta[bank].timer)
#define inc_timer(bank)					(g_misc_meta[bank].timer = (g_misc_meta[bank].timer + 1) & TIMESTAMP_MAX)
#define set_timer(bank, time)			(g_misc_meta[bank].timer = time)

#define get_write_page(bank)			(g_misc_meta[bank].cur_write_page)
#define set_write_page(bank, vpn)		(g_misc_meta[bank].cur_write_page = vpn)

#define get_page_offset(bank, pg_per_bank, page)		((bank * pg_per_bank) + page)
#define get_blk_offset(bank, blk_per_bank, blk)		((bank * blk_per_bank) + blk)

#define get_gc_victim_cost(bank)		(g_misc_meta[bank].cur_gc_victim_vcnt)
#define get_gc_victim_blk(bank)			(g_misc_meta[bank].cur_gc_victim)
#define set_gc_victim_cost(bank, cost)	(g_misc_meta[bank].cur_gc_victim_vcnt = cost)
#define set_gc_victim_blk(bank, blk)	(g_misc_meta[bank].cur_gc_victim = blk)

#define get_free_blk_cnt(bank)			(g_misc_meta[bank].free_blk_cnt)
#define set_free_blk_cnt(bank, cnt)		(g_misc_meta[bank].free_blk_cnt = cnt)
#define inc_free_blk_cnt(bank)			(g_misc_meta[bank].free_blk_cnt++)
#define dec_free_blk_cnt(bank)			(g_misc_meta[bank].free_blk_cnt--)

#define get_gc_block(bank)				(g_misc_meta[bank].gc_block)
#define get_df_block(bank)				(g_misc_meta[bank].df_block)
#define set_gc_block(bank, vbn)			(g_misc_meta[bank].gc_block = vbn)	
#define set_df_block(bank, vbn)			(g_misc_meta[bank].df_block = vbn)

#define get_page_hit(bank)				(g_misc_meta[bank].page_hit)
#define get_total_hit(bank)				(g_misc_meta[bank].total_hit)
#define set_page_hit(bank, hit)			(g_misc_meta[bank].page_hit = hit)
#define set_total_hit(bank, hit)		(g_misc_meta[bank].total_hit = hit)
#define inc_page_hit(bank)				(g_misc_meta[bank].page_hit++)
#define inc_total_hit(bank)				(g_misc_meta[bank].total_hit++)

#define get_avg_cost(bank)				(g_misc_meta[bank].avg_cost)
#define set_avg_cost(bank, cost)		(g_misc_meta[bank].avg_cost = cost)

#define get_lpmblk_vbn(bank, mapblk)      (g_misc_meta[bank].cur_lpm_blk_vbn[mapblk])
#define set_lpmblk_vbn(bank, mapblk, vbn) (g_misc_meta[bank].cur_lpm_blk_vbn[mapblk] = vbn)

#define get_lbmblk_vbn(bank, mapblk)      (g_misc_meta[bank].cur_lbm_blk_vbn[mapblk])
#define set_lbmblk_vbn(bank, mapblk, vbn) (g_misc_meta[bank].cur_lbm_blk_vbn[mapblk] = vbn)

#define get_vpmblk_vbn(bank, mapblk)      (g_misc_meta[bank].cur_vpm_blk_vbn[mapblk])
#define set_vpmblk_vbn(bank, mapblk, vbn) (g_misc_meta[bank].cur_vpm_blk_vbn[mapblk] = vbn)

#define get_vbmblk_vbn(bank, mapblk)      (g_misc_meta[bank].cur_vbm_blk_vbn[mapblk])
#define set_vbmblk_vbn(bank, mapblk, vbn) (g_misc_meta[bank].cur_vbm_blk_vbn[mapblk] = vbn)

//=================================================================

#define get_emptyblk_vbn(bank, mapblk_lbn)		(g_misc_meta[bank].cur_emptyblk_vbn[mapblk_lbn])
#define set_emptyblk_vbn(bank, mapblk_lbn, vbn)		(g_misc_meta[bank].cur_emptyblk_vbn[mapblk_lbn] = vbn)

#define CHECK_LPAGE(lpn)              ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn)              ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))

//----------------------------------
// FTL internal function prototype
//----------------------------------
static void   format(void);
static void   write_format_mark(void);
static BOOL32 check_format_mark(void);
static void   sanity_check(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
static void   write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
//static UINT32 get_vpn(UINT32 const lpn);
//static UINT32 get_vt_vblock(UINT32 const bank);
static UINT32 assign_new_write_vpn(UINT32 const bank, UINT32 const lpn);

//=================================================================

//static UINT32 get_vpn (UINT32 const bank, UINT32 const lpn);
//static UINT32 set_vpn (UINT32 const bank, UINT32 const lpn, UINT32 const vpn);
#define get_vpn(bank, lpn)		(get_lpm_vpn(bank, lpn))
#define set_vpn(bank, lpn, vpn)	(set_lpm_vpn(bank, lpn, vpn))

static UINT32 get_new_page (UINT32 const bank);

// logical page mapping table 관리
static void set_lpm_vpn (UINT32 const bank, UINT32 const lpn, UINT32 const vpn);

static UINT32 get_lpm_vpn (UINT32 const bank, UINT32 const lpn);

// logical block metadata 관리
static void inc_lbm_vcnt (UINT32 const bank, UINT32 const lbn);
static void set_lbm_timestamp (UINT32 const bank, UINT32 const lbn, UINT16 const timestamp);
static void set_lbm_mapstat (UINT32 const bank, UINT32 const lbn, UINT32 const map_stat);

static UINT32 get_lbm_vcnt (UINT32 const bank, UINT32 const lbn);
static UINT32 get_lbm_timestamp (UINT32 const bank, UINT32 const lbn);
static UINT32 get_lbm_mapstat (UINT32 const bank, UINT32 const lbn);

// virtual page mapping table 관리
static void set_vpm_lpn (UINT32 const bank, UINT32 const vpn, UINT32 const lpn);
static UINT32 set_vpm_valid (UINT32 const bank, UINT32 const vpn);
static UINT32 set_vpm_invalid (UINT32 const bank, UINT32 const vpn);

static UINT32 get_vpm_lpn (UINT32 const bank, UINT32 const vpn);
static BOOL32 get_vpm_valid (UINT32 const bank, UINT32 const vpn);

// virtual block metadata 관리
static void set_vbm_option (UINT32 const bank, UINT32 const vbn, UINT32 const used, UINT32 const blk_stat);
static void set_vbm_used (UINT32 const bank, UINT32 const vbn);
static void set_vbm_notused (UINT32 const bank, UINT32 const vbn);
static void set_vbm_dblk (UINT32 const bank, UINT32 const vbn);
static void set_vbm_lblk (UINT32 const bank, UINT32 const vbn);
static void set_vbm_vcnt (UINT32 const bank, UINT32 const vbn, UINT32 const val);
static UINT32 inc_vbm_vcnt (UINT32 const bank, UINT32 const vbn);
static UINT32 dec_vbm_vcnt (UINT32 const bank, UINT32 const vbn);
static void set_vbm_ecnt (UINT32 const bank, UINT32 const vbn, UINT32 const val);
static UINT32 inc_vbm_ecnt (UINT32 const bank, UINT32 const vbn);

static UINT32 get_vbm_option (UINT32 const bank, UINT32 const vbn);
static BOOL32 is_vbm_used (UINT32 const bank, UINT32 const vbn);
static BOOL32 get_vbm_stat (UINT32 const bank, UINT32 const vbn);
static UINT32 get_vbm_vcnt (UINT32 const bank, UINT32 const vbn);
static UINT32 get_vbm_ecnt (UINT32 const bank, UINT32 const vbn);


// empty block bitmap 관리
static void set_using_blk (UINT32 const bank, UINT32 const vbn);
static void set_check_blk (UINT32 const bank, UINT32 const vbn);
static void set_empty_blk (UINT32 const bank, UINT32 const vbn);

static UINT32 get_empty_blk (UINT32 const bank);

// janus ftl fuction
static void fusion (UINT32 const bank, UINT32 const lbn);
static void defusion (UINT32 const bank);
static UINT32 garbage_collection (UINT32 const bank);

static UINT32 get_df_victim_blk (UINT32 const bank);

static UINT32 calc_gc_victim_blk (UINT32 const bank);
static UINT32 calc_avg_cost (UINT32 const bank);
static UINT32 calc_hit_rate (UINT32 const bank);


//=================================================================

static void blk_recovery (UINT32 const bank, UINT32 const vpn);

static void logging_misc_block (void);
static void logging_lpm_block (void);
static void logging_lbm_block (void);
static void logging_vpm_block (void);
static void logging_vbm_block (void);
static void logging_empty_block (void);

static void load_misc_block (void);
static void load_lpm_block (void);
static void load_lbm_block (void);
static void load_vpm_block (void);
static void load_vbm_block (void);
static void load_empty_block (void);

static void mem_copy_mod (void * const dst, const void * const src, UINT32 const num_bytes);
//=================================================================

static void sanity_check(void)
{
    UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
        + HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + EMPTY_BLK_BYTES
        + LPAGE_MAP_BYTES + LBLK_META_BYTES + VPAGE_MAP_BYTES + VBLK_META_BYTES + FTL_TEST_BYTES;

    if ((dram_requirement > DRAM_SIZE) || // DRAM metadata size check
        (sizeof(misc_metadata) > BYTES_PER_PAGE)) // misc metadata size check
    {
        led_blink();
        while (1);
    }
}

static void build_bad_blk_list(void)
{
    UINT32 bank, num_entries, result, vblk_offset;
    scan_list_t* scan_list = (scan_list_t*) TEMP_BUF_ADDR;

    mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

    disable_irq();

    flash_clear_irq();

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
        SETREG(FCP_BANK, REAL_BANK(bank));
        SETREG(FCP_OPTION, FO_E);
        SETREG(FCP_DMA_ADDR, (UINT32) scan_list);
        SETREG(FCP_DMA_CNT, SCAN_LIST_SIZE);
        SETREG(FCP_COL, 0);
        SETREG(FCP_ROW_L(bank), SCAN_LIST_PAGE_OFFSET);
        SETREG(FCP_ROW_H(bank), SCAN_LIST_PAGE_OFFSET);

        SETREG(FCP_ISSUE, NULL);
        while ((GETREG(WR_STAT) & 0x00000001) != 0);
        while (BSP_FSM(bank) != BANK_IDLE);

        num_entries = NULL;
        result = OK;

        if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT)
        {
            result = FAIL;
        }
        else
        {
            UINT32 i;

            num_entries = read_dram_16(&(scan_list->num_entries));

            if (num_entries > SCAN_LIST_ITEMS)
            {
                result = FAIL;
            }
            else
            {
                for (i = 0; i < num_entries; i++)
                {
                    UINT16 entry = read_dram_16(scan_list->list + i);
                    UINT16 pblk_offset = entry & 0x7FFF;

                    if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK)
                    {
                        #if OPTION_REDUCED_CAPACITY == FALSE
                        result = FAIL;
                        #endif
                    }
                    else
                    {
                        write_dram_16(scan_list->list + i, pblk_offset);
                    }
                }
            }
        }

        if (result == FAIL)
        {
            num_entries = 0;  // We cannot trust this scan list. Perhaps a software bug.
        }
        else
        {
            write_dram_16(&(scan_list->num_entries), 0);
        }

        g_bad_blk_count[bank] = 0;

        for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++)
        {
            BOOL32 bad = FALSE;

            #if OPTION_2_PLANE
            {
                UINT32 pblk_offset;

                pblk_offset = vblk_offset * NUM_PLANES;

                // fix bug@jasmine v.1.1.0
                if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
                {
                    bad = TRUE;
                }

                pblk_offset = vblk_offset * NUM_PLANES + 1;

                // fix bug@jasmine v.1.1.0
                if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
                {
                    bad = TRUE;
                }
            }
            #else
            {
                // fix bug@jasmine v.1.1.0
                if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)
                {
                    bad = TRUE;
                }
            }
            #endif

            if (bad)
            {
                g_bad_blk_count[bank]++;
                set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
            }
        }
    }
}

void ftl_open(void)
{
    // debugging example 1 - use breakpoint statement!
    /* *(UINT32*)0xFFFFFFFE = 10; */

    /* UINT32 volatile g_break = 0; */
    /* while (g_break == 0); */
    
    led(0);
    sanity_check();
    //----------------------------------------
    // read scan lists from NAND flash
    // and build bitmap of bad blocks
    //----------------------------------------
    build_bad_blk_list();
    //----------------------------------------
    // If necessary, do low-level format
    // format() should be called after loading scan lists, because format() calls is_bad_block().
    //----------------------------------------
    if (check_format_mark() == FALSE) 
    //if (TRUE)
    {
        uart_print("do format");
        format();
        uart_print("end format");
        gCheckRecovery = 0;
    }
    // load FTL metadata
    else
    {
        load_metadata();
        gCheckRecovery = 1;
    }
    g_ftl_read_buf_id = 0;
    g_ftl_write_buf_id = 0;

    // This example FTL can handle runtime bad block interrupts and read fail (uncorrectable bit errors) interrupts
    flash_clear_irq();

    SETREG(INTR_MASK, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);
    SETREG(FCONF_PAUSE, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);

    enable_irq();
}

// power off recovery
void ftl_flush(void)
{
#ifdef __TEST_LOGGING
    uart_printf ("ftl_flush :: logging metadata");
#endif
    logging_misc_block();
    logging_lpm_block ();
    logging_lbm_block ();
    logging_vpm_block ();
    logging_vbm_block ();
    logging_empty_block ();
#ifdef __TEST_LOGGING
    uart_printf ("ftl_flush :: logging complete");
#endif
    led(0);
}

// Testing FTL protocol APIs
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors)
{
    ASSERT(lba + num_sectors <= NUM_LSECTORS);
    ASSERT(num_sectors > 0);

    ftl_write(lba, num_sectors);
}

void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
    UINT32 remain_sects, num_sectors_to_read;
    UINT32 lpn, sect_offset;
    UINT32 bank, vpn;

    lpn          = lba / SECTORS_PER_PAGE;	// byte address에서 logical page number를 계산
    sect_offset  = lba % SECTORS_PER_PAGE;	// logical sector offset을 계산
    remain_sects = num_sectors;	

    while (remain_sects != 0)	// nand flash에서 page별로 data를 read
    {
        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)	// 한 page 초과 읽을 data가 남아있을 경우, offset에서부터 읽을 data가 한 page를 초과할 경우
        {
            num_sectors_to_read = remain_sects;
        }
        else // 한 page 이하 남아있을 경우
        {
            num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
        }

        // nand flash에서 bank와 page address를 받아온다
        bank = get_num_bank(lpn); // page striping
        vpn  = get_vpn(bank, lpn);
        CHECK_VPAGE(vpn);

		/*
		// PMA hit rate 계산
		inc_total_hit (bank);
		if (get_lbm_mapstat(bank, page_to_blk(lpn >> 3)) == PAGE_MAPPING)
		{
			inc_page_hit (bank);
		}
		*/

#ifdef __TEST_RD
        uart_printf ("ftl_read :: [lpn %d] [bank %d] [vpn %d]\n", lpn, bank, vpn);
#endif

        // 실제 page가 존재할 경우 nand flash에서 읽어온다
        if (vpn != NULL)
        {
            nand_page_ptread_to_host(bank,
                                     vpn / PAGES_PER_BLK,
                                     vpn % PAGES_PER_BLK,
                                     sect_offset,
                                     num_sectors_to_read);
        }

        // nand flash에 page가 존재하지 않을 경우 (쓰지 않은 page에서 read를 할 경우)
        // The host is requesting to read a logical page that has never been written to.
        else
        {
            UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

            #if OPTION_FTL_TEST == 0
            while (next_read_buf_id == GETREG(SATA_RBUF_PTR));	// wait if the read buffer is full (slow host)
            #endif

            // fix bug @ v.1.0.6
            // Send 0xFF...FF to host when the host request to read the sector that has never been written.
            // In old version, for example, if the host request to read unwritten sector 0 after programming in sector 1, Jasmine would send 0x00...00 to host.
            // However, if the host already wrote to sector 1, Jasmine would send 0xFF...FF to host when host request to read sector 0. (ftl_read() in ftl_xxx/ftl.c)
            // 쓰여진 적 없는 page의 경우 dram에 0000으로 임의의 data를 채운다
            mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + sect_offset*BYTES_PER_SECTOR,
                         0x00000000, num_sectors_to_read*BYTES_PER_SECTOR);

            flash_finish();

            SETREG(BM_STACK_RDSET, next_read_buf_id);	// change bm_read_limit
            SETREG(BM_STACK_RESET, 0x02);				// change bm_read_limit

            g_ftl_read_buf_id = next_read_buf_id;
        }
        sect_offset   = 0;
        remain_sects -= num_sectors_to_read;
        lpn++;
    }
}

void ftl_write(UINT32 const lba, UINT32 const num_sectors)
{
    UINT32 remain_sects, num_sectors_to_write;
    UINT32 lpn, sect_offset;

    //uart_printf ("write start : %d %d\n", lba, num_sectors);

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    while (remain_sects != 0)
    {
        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_write = remain_sects;
        }
        else
        {
            num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
        }
        // single page write individually
        write_page(lpn, sect_offset, num_sectors_to_write);

        sect_offset   = 0;
        remain_sects -= num_sectors_to_write;
        lpn++;
    }

    //uart_printf ("write end : %d %d\n", lba, num_sectors);
}

static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
// 한 page 안에서 nand flash memory에 data를 write 한다
{	
    CHECK_LPAGE(lpn);
    ASSERT(sect_offset < SECTORS_PER_PAGE);
    ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

    UINT32 bank, old_vpn, new_vpn;
    UINT32 vblock, page_num, page_offset, column_cnt;
	UINT32 new_cost;

    bank        = get_num_bank(lpn); // page striping
    page_offset = sect_offset;	
    column_cnt  = num_sectors;
    
    old_vpn  = get_vpn(bank, lpn);	// 이전에  write가 된 page 번호를 받는다

	// PMA hit rate 계산
	inc_total_hit (bank);
	if ((get_lbm_mapstat(bank, page_to_blk(lpn >> 3)) == PAGE_MAPPING) && (old_vpn != NULL))
	{
		inc_page_hit (bank);
	}

    CHECK_VPAGE (old_vpn);
    //CHECK_VPAGE (new_vpn);
    //ASSERT(old_vpn != new_vpn);

    g_ftl_statistics[bank].page_wcount++;

    // if old data already exist,
    if (old_vpn != NULL)
    {
        vblock   = old_vpn / PAGES_PER_BLK;	// block 번호 계산
        page_num = old_vpn % PAGES_PER_BLK;	// page 번호 계산

        //--------------------------------------------------------------------------------------
        // `Partial programming'
        // we could not determine whether the new data is loaded in the SATA write buffer.
        // Thus, read the left/right hole sectors of a valid page and copy into the write buffer.
        // And then, program whole valid data
        //--------------------------------------------------------------------------------------
        if (num_sectors != SECTORS_PER_PAGE)	// 한 page 전체를 쓰지 않을 경우
        {
            // Performance optimization (but, not proved)
            // To reduce flash memory access, valid hole copy into SATA write buffer after reading whole page
            // Thus, in this case, we need just one full page read + one or two mem_copy
            if ((num_sectors <= 8) && (page_offset != 0))	// page의 맨 앞에서부터 쓰지 않을 경우 (page의 양 끝에 hole이 생김)
            {
                // nand에서 page를 읽어온 후 memory에 left hole과 right hole을 채워넣는다

                // one page async read
                nand_page_read(bank,
                               vblock,
                               page_num,
                               FTL_BUF(bank));
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                             FTL_BUF(bank),
                             page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                             FTL_BUF(bank) + rhole_base,
                             BYTES_PER_PAGE - rhole_base);
                }
            }

            // left/right hole async read operation (two partial page read)
            else
            {
                // left hole이나 right hole만 생겼을 경우 nand에서 직접 memory로 data를 읽어온다
                // read `left hole sectors'
                if (page_offset != 0)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     0,
                                     page_offset,
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
                // read `right hole sectors'
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     page_offset + column_cnt,
                                     SECTORS_PER_PAGE - (page_offset + column_cnt),
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
            }
        }
        // page offset과 column count를 한 page에 맞게 align 시킨다
        // full page write
        page_offset = 0;
        column_cnt  = SECTORS_PER_PAGE;
    }
    // 새로 write하는 page인 경우
    else
    {
        // 쓰지 않는 sector는 0x00으로 초기화
        if (num_sectors != SECTORS_PER_PAGE)
        {
            // 한 page에서 partial programming을 할 경우
            if ((num_sectors <= 8) && (page_offset != 0))	
            {
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_set_dram (WR_BUF_PTR(g_ftl_write_buf_id),
                                NULL, page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

                    mem_set_dram (WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                                NULL, BYTES_PER_PAGE - rhole_base);
                }
            }

            // 두 페이지 이상에서 partial programming을 할 경우
            else
            {
                // left hole이나 right hole만 생겼을 경우 nand에서 직접 memory로 data를 읽어온다
                // read `left hole sectors'
                if (page_offset != 0)
                {
                    mem_set_dram(WR_BUF_PTR (g_ftl_write_buf_id),
                                NULL, page_offset * BYTES_PER_SECTOR);
                }
                // read `right hole sectors'
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR % BYTES_PER_PAGE;

                    mem_set_dram (WR_BUF_PTR (g_ftl_write_buf_id) + rhole_base,
                                NULL, BYTES_PER_PAGE - rhole_base);
                }
            }
        }
        // page offset과 column count를 한 page에 맞게 align 시킨다
        // full page write
        page_offset = 0;
        column_cnt  = SECTORS_PER_PAGE;
    }
    

    // 새로 사용할 page의 block 번호와 page offset을 구한다
    new_vpn  = assign_new_write_vpn(bank, lpn);	// 새로 write를 할 page 번호를 받는다
    vblock   = new_vpn / PAGES_PER_BLK;
    page_num = new_vpn % PAGES_PER_BLK;

#ifdef __TEST_WRT
    uart_printf ("write_page lpn : %d", lpn);
    uart_printf ("bank : %d, new vpn : %d, old vpn : %d", bank, new_vpn, old_vpn);
#endif

    // write new data (make sure that the new data is ready in the write buffer frame)
    // (c.f FO_B_SATA_W flag in flash.h)
    // nand flash memory에 데이터를 program 한다
    nand_page_ptprogram_from_host(bank,
                                  vblock,
                                  page_num,
                                  page_offset,
                                  column_cnt);

#ifdef __TEST_WRT
    uart_printf ("write_end :: bank : %d, vpn : %d, get_vpn : %d\n", bank, new_vpn, get_vpn(bank, lpn));
#endif	

	// timer 설정
	inc_timer(bank);

    // defusion 검사
	if ((new_cost = calc_avg_cost(bank)) > get_avg_cost(bank))
	// while ((new_cost = calc_avg_cost(bank)) > get_avg_cost(bank))
	{
		defusion (bank);
	}
	set_avg_cost (bank, new_cost);

	// PMA hit rate 정리
	calc_hit_rate (bank);
}

static UINT32 assign_new_write_vpn(UINT32 const bank, UINT32 const lpn)
{
    UINT32 lpn_meta, vcnt;
    UINT32 old_vpn, new_vpn;

	// logical block에 time stamp 입력
	set_lbm_timestamp (bank, page_to_blk(lpn >> 3), get_timer(bank));

    // 이전에 page를 기록했는지 검사
    old_vpn = get_lpm_vpn (bank, lpn);
    if (old_vpn != 0x00000000)
    {
#ifdef __TEST_WRT
		uart_printf ("assign_new_write_vpn :: set invalid old page [ %d ]", lpn);
#endif
        // data block에 있었으면
        if (get_lbm_mapstat (bank, page_to_blk (lpn >> 3)) != PAGE_MAPPING)
        {
            // fusion
            fusion(bank, page_to_blk(lpn >> 3));
        }

        // 이전에 기록한 page를 invalid 표시
        vcnt = set_vpm_invalid (bank, old_vpn);

        // 현재 write하는 block이 아닌 경우 garbage collection cost 계산
        if ((old_vpn >> BLK_TO_PAGE) != (get_write_page(bank) >> BLK_TO_PAGE))
        {
			// block의 valid page count가 0이면 다 쓴 block이므로 erase하고 empty block으로 반환
			if (vcnt == 0)
			{
				nand_block_erase (bank, page_to_blk (old_vpn));
				set_empty_blk (bank, page_to_blk (old_vpn));
			}
            // invalid check한 block의 valid count가 현재 victim block의 valid count보다 작을 경우
            else if (vcnt < get_gc_victim_cost(bank))
            {
                // invalid check된 block의 cost가 더 적으므로 victim block 교체
                set_gc_victim_cost(bank, vcnt);
                set_gc_victim_blk(bank, old_vpn >> BLK_TO_PAGE);
            }
        }
    }
	// 처음 기록하는 page이면 logical block의 valid count 증가
	else
	{
		inc_lbm_vcnt (bank, (lpn >> BLK_TO_PAGE));
	}

    // log block에서 새로운 page 할당
    new_vpn = get_new_page (bank);

    // lpn <-> vpn metadata 갱신
    set_lpm_vpn (bank, lpn, new_vpn);
    set_vpm_lpn (bank, new_vpn, lpn);
    
    // 연속성 검사
	if (((((lpn >> 3) + 1) & 127) == 0) && (((new_vpn+1) & 127) == 0))
	{
		// logical page와 virtual page가 모두 한 block에서 serial하면 data block으로 표시
		UINT32 pgcnt, p32, v32, is_seq;

#ifdef __TEST_FUSION
		//uart_printf ("pm_to_bm :: find sequential block lpn %d vpn %d", lpn, new_vpn);
#endif

		// block내의 page를 역순으로 sequential인지 검사
		is_seq = 1;
		p32 = lpn;
		v32 = new_vpn;
		for (pgcnt = 1; pgcnt < PAGES_PER_BLK; pgcnt++)
		{
			p32 = p32 - NUM_BANKS;
			v32 = v32 - 1;
			
			//uart_printf ("pm_to_bm :: lpn %x vpn_to_lpn %x, vpn %x lpn_to_vpn %x", p32, get_lpm_vpn (bank, p32), v32, get_vpm_lpn(bank, v32));

			if (p32 != get_vpm_lpn(bank, v32))
			{
				is_seq = 0;
				break;
			}
		}

		// sequential이면 data block으로 설정
		if (is_seq == 1)
		{
#ifdef __TEST_FUSION
			//uart_printf ("pm_to_bm :: convert to data block lpn %d", lpn);
#endif
			set_lbm_mapstat (bank, page_to_blk(lpn >> 3), BLOCK_MAPPING);
			set_vbm_dblk (bank, page_to_blk(new_vpn));
		}
	}

    return new_vpn;
}

static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
    if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE)
    {
        return FALSE;
    }
    return TRUE;
}

static void format(void)
{
    UINT32 i32, j32;
    UINT32 bank, vblock;

    ASSERT(NUM_MISC_META_SECT > 0);

    uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

    uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
    uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);

    //----------------------------------------
    // initialize DRAM metadata
    //----------------------------------------

    // initialize dram	
    mem_set_dram (LPAGE_MAP_ADDR, NULL, LPAGE_MAP_BYTES);
    mem_set_dram (LBLK_META_ADDR, NULL, LBLK_META_BYTES);
    mem_set_dram (VPAGE_MAP_ADDR, NULL, VPAGE_MAP_BYTES);
    mem_set_dram (VBLK_META_ADDR, NULL, VBLK_META_BYTES);
        
    mem_set_dram(EMPTY_BLK_ADDR, 0xffffffff, EMPTY_BLK_BYTES);

    //----------------------------------------
    // erase all blocks except vblock #0
    //----------------------------------------
    for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
    {
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (is_bad_block(bank, vblock) == FALSE)
            {
                nand_block_erase(bank, vblock);
            }
        }
    }

    //----------------------------------------
    // initialize global variable
    //----------------------------------------
    for (i32 = 0; i32 < NUM_BANKS; i32++)
    {		
        set_timer(i32, 0);
        set_write_page(i32, 0);

		set_total_hit (i32, 0);
		set_page_hit (i32, 0);

		set_avg_cost (i32, 0xffffffff);

        set_gc_victim_cost(i32, 128);
        set_gc_victim_blk(i32, 0);

        set_free_blk_cnt(i32, VBLKS_PER_BANK);
    }

    //----------------------------------------
    // initialize SRAM metadata
    //----------------------------------------
    init_metadata_sram();

    // flush metadata to NAND
    //logging_pmap_table();
    //logging_misc_metadata();

    

    write_format_mark();
    led(1);
    uart_print("format complete");
}

static void init_metadata_sram(void)
{
    UINT32 bank;
    UINT32 vblock;
    UINT32 mapblk_lbn;

//	uart_printf("init_metadata_sram :: MAPBLK %d LOGBLK %d EMPTYBLK %d", 
//			MAPBLKS_PER_BANK, LOGBLKMAP_PER_BANK, EMPTYBLK_PER_BANK);

    //----------------------------------------
    // initialize misc. metadata
    //----------------------------------------
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        set_check_blk (bank, 0);

        //g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
        //g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
        // NOTE: vblock #0,1 don't use for user space

        //----------------------------------------
        // assign misc. block
        //----------------------------------------
        // assumption: vblock #1 = fixed location.
        // Thus if vblock #1 is a bad block, it should be allocate another block.
        vblock = MISCBLK_VBN;
        while (is_bad_block (bank, vblock) == TRUE)
        {
            set_using_blk (bank, vblock);
            vblock++;
        }
        set_miscblk_vbn(bank, vblock);
        set_check_blk (bank, vblock);
        
        //----------------------------------------
        // assign logical page mapping block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < LPMAP_PER_BANK)
        {
            vblock++;
            ASSERT(vblock < VBLKS_PER_BANK);
            if (is_bad_block(bank, vblock) == FALSE)
            {
                set_lpmblk_vbn(bank, mapblk_lbn, vblock);
                mapblk_lbn++;
            }
            set_check_blk (bank, vblock);			
        }

        //----------------------------------------
        // assign logical block metadadta block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < LBMETA_PER_BANK)
        {
            vblock++;
            ASSERT (vblock < VBLKS_PER_BANK);
            if (is_bad_block (bank, vblock) == FALSE)
            {
                set_lbmblk_vbn (bank, mapblk_lbn, vblock);
                mapblk_lbn++;
            }
            set_check_blk (bank, vblock);
        }
        
        //----------------------------------------
        // assign virtual page mapping block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < VPMAP_PER_BANK)
        {
            vblock++;
            ASSERT (vblock < VBLKS_PER_BANK);
            if (is_bad_block (bank, vblock) == FALSE)
            {
                set_vpmblk_vbn (bank, mapblk_lbn, vblock);
                mapblk_lbn++;
            }
            set_check_blk (bank, vblock);
        }

        //----------------------------------------
        // assign virtual block metadadta block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < VBMETA_PER_BANK)
        {
            vblock++;
            ASSERT (vblock < VBLKS_PER_BANK);
            if (is_bad_block (bank, vblock) == FALSE)
            {
                set_vbmblk_vbn (bank, mapblk_lbn, vblock);
                mapblk_lbn++;
            }
            set_check_blk (bank, vblock);
        }

        //----------------------------------------
        // assign empty block map block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < EMPTYBLK_PER_BANK)
        {
            vblock++;
            ASSERT (vblock < VBLKS_PER_BANK);
            if (is_bad_block (bank, vblock) == FALSE)
            {
                set_emptyblk_vbn (bank, mapblk_lbn, vblock);
                mapblk_lbn++;
            }
            set_check_blk (bank, vblock);
        }

        //----------------------------------------
        // assign free block for gc
        //----------------------------------------
        do
        {
            vblock++;
            // NOTE: free block should not be secleted as a victim @ first GC
            // set free block
            set_gc_block(bank, vblock);
            set_check_blk (bank, vblock);

            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);

        //----------------------------------------
        // assign free block for defusion
        //----------------------------------------
        do
        {
            vblock++;
            // NOTE: free block should not be secleted as a victim @ first GC
            // set free block
            set_df_block(bank, vblock);
            set_check_blk (bank, vblock);

            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);

        // bad block을 사용하지 못하도록 표시
        do
        {
            vblock++;
            if (is_bad_block (bank, vblock) == TRUE)
            {
                set_check_blk (bank, vblock);
            }
        }while (vblock < BLKS_PER_BANK);
    }

    uart_printf ("init_metadata_sram :: end");
}

// load flushed FTL metadta
static void load_metadata(void)
{
#ifdef __TEST_PWRECV
    uart_printf ("load_metadata :: power off recovery");
#endif 
    load_misc_block ();
    load_lpm_block ();
    load_lbm_block ();
    load_vpm_block ();
    load_vbm_block ();
    load_empty_block ();
#ifdef __TEST_PWRECV
    uart_printf ("load_metadata :: loading metadata end");
#endif 

    led(1);
}

static void write_format_mark(void)
{
    // This function writes a format mark to a page at (bank #0, block #0).

    #ifdef __GNUC__
    extern UINT32 size_of_firmware_image;
    UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
    #else
    extern UINT32 Image$$ER_CODE$$RO$$Length;
    extern UINT32 Image$$ER_RW$$RW$$Length;
    UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
    UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
    #endif

    UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;

    mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);

    SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
    SETREG(FCP_BANK, REAL_BANK(0));
    SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
    SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// DRAM -> flash
    SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
    SETREG(FCP_COL, 0);
    SETREG(FCP_ROW_L(0), format_mark_page_offset);
    SETREG(FCP_ROW_H(0), format_mark_page_offset);

    // At this point, we do not have to check Waiting Room status before issuing a command,
    // because we have waited for all the banks to become idle before returning from format().
    SETREG(FCP_ISSUE, NULL);

    // wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
    while ((GETREG(WR_STAT) & 0x00000001) != 0);

    // wait until bank #0 finishes the write operation
    while (BSP_FSM(0) != BANK_IDLE);
}

static BOOL32 check_format_mark(void)
{
    // This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.

    #ifdef __GNUC__
    extern UINT32 size_of_firmware_image;
    UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
    #else
    extern UINT32 Image$$ER_CODE$$RO$$Length;
    extern UINT32 Image$$ER_RW$$RW$$Length;
    UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
    UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
    #endif

    UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
    UINT32 temp;

    flash_clear_irq();	// clear any flash interrupt flags that might have been set

    SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
    SETREG(FCP_BANK, REAL_BANK(0));
    SETREG(FCP_OPTION, FO_E);
    SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// flash -> DRAM
    SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
    SETREG(FCP_COL, 0);
    SETREG(FCP_ROW_L(0), format_mark_page_offset);
    SETREG(FCP_ROW_H(0), format_mark_page_offset);

    // At this point, we do not have to check Waiting Room status before issuing a command,
    // because scan list loading has been completed just before this function is called.
    SETREG(FCP_ISSUE, NULL);

    // wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
    while ((GETREG(WR_STAT) & 0x00000001) != 0);

    // wait until bank #0 finishes the read operation
    while (BSP_FSM(0) != BANK_IDLE);

    // Now that the read operation is complete, we can check interrupt flags.
    temp = BSP_INTR(0) & FIRQ_ALL_FF;

    // clear interrupt flags
    CLR_BSP_INTR(0, 0xFF);

    if (temp != 0)
    {
        return FALSE;	// the page contains all-0xFF (the format mark does not exist.)
    }
    else
    {
        return TRUE;	// the page contains something other than 0xFF (it must be the format mark)
    }
}

// BSP interrupt service routine - incomplete
void ftl_isr(void)
{
    UINT32 bank;
    UINT32 bsp_intr_flag;
    UINT32 vpn, vbn;

    uart_print("BSP interrupt occured...");
    // interrupt pending clear (ICU)
    SETREG(APB_INT_STS, INTR_FLASH);

    for (bank = 0; bank < NUM_BANKS; bank++) {
        while (BSP_FSM(bank) != BANK_IDLE);
        // get interrupt flag from BSP
        bsp_intr_flag = BSP_INTR(bank);

        if (bsp_intr_flag == 0) {
            continue;
        }
        UINT32 fc = GETREG(BSP_CMD(bank));
        // BSP clear
        CLR_BSP_INTR(bank, bsp_intr_flag);

        // interrupt handling
        if (bsp_intr_flag & FIRQ_DATA_CORRUPT) {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
            uart_print("FIRQ_DATA_CORRUPT occured...");
        }
        if (bsp_intr_flag & (FIRQ_BADBLK_H | FIRQ_BADBLK_L)) {
            uart_printf("BSP interrupt at bank: 0x%x", bank);

            // bad block이 발생한 page와 bank 번호 load
            vpn = GETREG(BSP_ROW_H(bank));
            vbn = vpn / PAGES_PER_BANK;

            if (fc == FC_COL_ROW_IN_PROG || fc == FC_IN_PROG || fc == FC_PROG) {
                // program에서 생긴 bad block은 data를 다른 위치로 mapping 시켜준다
                uart_print("find runtime bad block when block program...");
                blk_recovery (bank, vpn);
            }
            // read 에서 발생한 bad block 처리

            else {
                // erase에서 생긴 bad block은 data를 보존할 필요가 없으므로 사용 못하도록 체크만 한다
                uart_printf("find runtime bad block when block erase...vblock #: %d", vbn);
                ASSERT(fc == FC_ERASE);
            }

            // bad block난 block은 bad block map에 표시
            set_bit_dram (BAD_BLK_BMP_ADDR + bank * (VBLKS_PER_BANK / 8 + 1), vbn);

            // empty block bitmap에 using으로 표시해서 다시 사용할수 없도록 한다
            set_using_blk (bank, vbn);
        }
    }
}


static void blk_recovery (UINT32 const bank, UINT32 const vpn)
{
    /*
    UINT32 vbn, badpage, logblk, new_blk;
    UINT32 d32, i32;

    vbn = vpn / PAGES_PER_BLK;
    badpage = vpn % PAGES_PER_BLK;

    // log block에서 vbn을 검색
    for (logblk = 0; logblk < NUM_LOG_BLKS; logblk++)
    {
        d32 = read_dram_32 (LOG_BLK_ADDR + ((bank * NUM_LOG_BLKS + logblk) * LOG_BLK_SIZE) + LOG_BLK_VADDR);
        if (d32 == vbn)
        {
            break;
        }
    }

    // 새 block으로 bad block이 난 page를 제외하고 옮긴다
    new_blk = get_empty_blk (bank);

    // 순서대로 write하기 때문에 bad block이 발생한 page 전까지만 data가 존재
    for (i32 = 0; i32 < badpage; i32++)
    {
        nand_page_copyback (bank, vbn, i32, new_blk, i32);
    }

    // bad block이 발생한 page는 write buffer에서 데이터를 읽어온다
    nand_page_ptprogram_from_host (bank, new_blk, badpage, 0, SECTORS_PER_PAGE);
    
    // log block mapping table에 새로 연결
    write_dram_32 (LOG_BLK_ADDR + ((bank * NUM_LOG_BLKS + logblk) * LOG_BLK_SIZE) + LOG_BLK_VADDR, new_blk);
    */
}


//=================================================================

static UINT32 get_new_page (UINT32 const bank)
{
    UINT32 vpn, vbn;
    UINT32 cost;

    // cur write page를 받아온다
    vpn = get_write_page(bank);
#ifdef __TEST_WRT
	uart_printf ("get_new_page :: current write vpn [ %d ]", vpn);
#endif

	// current write page가 없으면
    if (vpn == 0)
    {
        // 새 block 할당
        vbn = get_empty_blk(bank);
		
		// 정상적으로 block을 할당 받았을 경우 write page에 입력
		if (vbn != 0)
		{
			vpn = vbn << BLK_TO_PAGE;
		}
		// empty block이 없을 경우 garbage collection으로 새 page 할당
		else
		{
			vpn = garbage_collection(bank);
		}
    }
	    
    // block의 page를 다 쓴 경우 다음에 write할 page number를 null로 입력
    if (((vpn+1) & 127) == 0)
    {
		set_write_page(bank, 0);

        // 이번에 다 쓴 block의 valid count를 gc victim block의 valid count와 비교
        vbn = vpn >> BLK_TO_PAGE;
        cost = get_vbm_vcnt(bank, vbn);
        if (cost < get_gc_victim_cost(bank))
        {
            // gc victim block의 valid count보다 현재 block의 valid count가 작은 경우 교체
            set_gc_victim_cost(bank, cost);
            set_gc_victim_blk(bank, vbn);
        }
    }
	// 더 쓸 공간이 있을 경우 다음 write page로 입력
    else
    {
        set_write_page(bank, vpn+1);
    }    

#ifdef __TEST_WRT
	uart_printf ("get_new_page :: current selected vpn [ %d ]", vpn);
#endif

    return vpn;
}

// logical page mapping table 관리
static void set_lpm_vpn (UINT32 const bank, UINT32 const lpn, UINT32 const vpn)
{
    //write_dram_32 (LPAGE_MAP_ADDR + get_page_offset(bank, LPAGES_PER_BANK, lpn) * LPAGE_MAP_SIZE, vpn);
	write_dram_32 (LPAGE_MAP_ADDR + lpn * LPAGE_MAP_SIZE, vpn);
}

static UINT32 get_lpm_vpn (UINT32 const bank, UINT32 const lpn)
{
    //return read_dram_32 (LPAGE_MAP_ADDR + get_page_offset (bank, LPAGES_PER_BANK, lpn) * LPAGE_MAP_SIZE);
	return read_dram_32 (LPAGE_MAP_ADDR + lpn * LPAGE_MAP_SIZE);
}

// logical block metadata 관리
static void inc_lbm_vcnt (UINT32 const bank, UINT32 const lbn)
{
    UINT8 d8;
    UINT32 addr;

    addr = LBLK_META_ADDR + get_blk_offset (bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE + LBLK_META_OP;
	//addr = LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_OP;

    d8 = read_dram_8 (addr);
    d8++;

    write_dram_8 (addr, d8);
}

static void set_lbm_timestamp (UINT32 const bank, UINT32 const lbn, UINT16 const timestamp)
{
    write_dram_16 (LBLK_META_ADDR + get_blk_offset (bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE + LBLK_META_TIME, timestamp);
	//write_dram_16 (LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_TIME, timestamp);
}

static void set_lbm_mapstat (UINT32 const bank, UINT32 const lbn, UINT32 const map_stat)
{
    if (map_stat == PAGE_MAPPING)
    {
        clr_bit_dram (LBLK_META_ADDR + get_blk_offset (bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE +
                        LBLK_META_OP, LBLK_META_OP_MA);
		//clr_bit_dram (LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_OP, LBLK_META_OP_MA);
    }
    else
    {		
        set_bit_dram (LBLK_META_ADDR + get_blk_offset (bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE +
                        LBLK_META_OP, LBLK_META_OP_MA);
		//set_bit_dram (LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_OP, LBLK_META_OP_MA);
    }
}

static UINT32 get_lbm_vcnt (UINT32 const bank, UINT32 const lbn)
{
    UINT8 d8;

    d8 = read_dram_8 (LBLK_META_ADDR + get_blk_offset(bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE + LBLK_META_OP);
	//d8 = read_dram_8 (LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_OP);
    d8 = d8 & LBLK_META_OP_VC_MSK;

    return (UINT32)d8;
}

static UINT32 get_lbm_timestamp (UINT32 const bank, UINT32 const lbn)
{
    UINT16 d16;

    d16 = read_dram_16 (LBLK_META_ADDR + get_blk_offset(bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE + LBLK_META_TIME);
	//d16 = read_dram_16 (LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_TIME);
    
    return (UINT32) d16;
}

static BOOL32 get_lbm_mapstat (UINT32 const bank, UINT32 const lbn)
{
    /*
    if (tst_bit_dram (LBLK_META_ADDR + get_blk_offset (bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE 
        + LBLK_META_OP,	LBLK_META_OP_MA))
    {
        return PAGE_MAPPING
    }
    else
    {
        return BLOCK_MAPPING
    }
    */
    return tst_bit_dram (LBLK_META_ADDR + get_blk_offset (bank, LBLKS_PER_BANK, lbn) * LBLK_META_SIZE 
        + LBLK_META_OP,	LBLK_META_OP_MA);
	//return tst_bit_dram (LBLK_META_ADDR + lbn * LBLK_META_SIZE + LBLK_META_OP,	LBLK_META_OP_MA);
}

// virtual page mapping table 관리
static void set_vpm_lpn (UINT32 const bank, UINT32 const vpn, UINT32 const lpn)
{
    write_dram_32 (VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE, lpn);
    set_vpm_valid (bank, vpn);
}

//#define set_vpm_valid(bank, vpn)	(set_bit_dram(VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE + VPAGE_MAP_OP, VPAGE_MAP_OP_V_BIT))
//#define set_vpm_invalid(bank, vpn)	(clr_bit_dram(VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE + VPAGE_MAP_OP, VPAGE_MAP_OP_V_BIT))

static UINT32 set_vpm_valid (UINT32 const bank, UINT32 const vpn)
{
    // page mapping data에 valid로 표시
    set_bit_dram (VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE + VPAGE_MAP_OP,
        VPAGE_MAP_OP_V_BIT);

    // virtual block의 valid counter 증가
    return inc_vbm_vcnt (bank, vpn / PAGES_PER_BLK);
}

static UINT32 set_vpm_invalid (UINT32 const bank, UINT32 const vpn)
{
    // page mapping data에 invalid로 표시
	write_dram_32 (VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE, 0);
    //clr_bit_dram (VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE + VPAGE_MAP_OP,
    //    VPAGE_MAP_OP_V_BIT);
	
    // virtual block의 valid counter 감소
    return dec_vbm_vcnt (bank, vpn / PAGES_PER_BLK);
}

static UINT32 get_vpm_lpn (UINT32 const bank, UINT32 const vpn)
{
    return (read_dram_32 (VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE) & VPAGE_MAP_LPN_MASK);
}

static BOOL32 get_vpm_valid (UINT32 const bank, UINT32 const vpn)
{
    return tst_bit_dram (VPAGE_MAP_ADDR + get_page_offset (bank, VPAGES_PER_BANK, vpn) * VPAGE_MAP_SIZE + VPAGE_MAP_OP, VPAGE_MAP_OP_V_BIT);
}

// virtual block metadata 관리
static void set_vbm_option (UINT32 const bank, UINT32 const vbn, UINT32 const used, UINT32 const blk_stat)
{
    UINT8 d8 = 0;

    if (used == TRUE)
    {
        d8 = d8 | VBLK_META_OP_USED_MASK;
    }
    if (blk_stat == TRUE)
    {
        d8 = d8 | VBLK_META_OP_BLK_MASK;
    }

    write_dram_8 (VBLK_META_ADDR + get_blk_offset (bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_OP, d8);
}

static void set_vbm_used (UINT32 const bank, UINT32 const vbn)
{
    set_bit_dram (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_OP, VBLK_META_OP_USED);
}

static void set_vbm_notused (UINT32 const bank, UINT32 const vbn)
{
    clr_bit_dram (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_OP, VBLK_META_OP_USED);
	inc_vbm_ecnt (bank, vbn);
}

static void set_vbm_dblk (UINT32 const bank, UINT32 const vbn)
{
    set_bit_dram (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_OP, VBLK_META_OP_BLK);
}

static void set_vbm_lblk (UINT32 const bank, UINT32 const vbn)
{
    clr_bit_dram (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_OP, VBLK_META_OP_BLK);
}

static void set_vbm_vcnt (UINT32 const bank, UINT32 const vbn, UINT32 const val)
{
    UINT32 d8 = (UINT8)val;

    write_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_VCNT, d8);
}

static UINT32 inc_vbm_vcnt (UINT32 const bank, UINT32 const vbn)
{
    UINT8 d8;

    d8 = read_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_VCNT);

    write_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_VCNT, d8 + 1);

    return (UINT32)(d8+1);
}

static UINT32 dec_vbm_vcnt (UINT32 const bank, UINT32 const vbn)
{
    UINT8 d8;

    d8 = read_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_VCNT);

    write_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_VCNT, d8 - 1);

    return (UINT32)(d8-1);
}

static void set_vbm_ecnt (UINT32 const bank, UINT32 const vbn, UINT32 const val)
{
    UINT32 d8 = (UINT8)val;

    write_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_ECNT, d8);
}

static UINT32 inc_vbm_ecnt (UINT32 const bank, UINT32 const vbn)
{
    UINT8 d8;

    d8 = read_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_ECNT);

    write_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE + VBLK_META_ECNT, d8 + 1);

	return (d8 + 1);
}

static UINT32 get_vbm_option (UINT32 const bank, UINT32 const vbn)
{
    return (UINT32)read_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE 
        + VBLK_META_OP);
}

static BOOL32 is_vbm_used (UINT32 const bank, UINT32 const vbn)
{
    return tst_bit_dram (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE 
        + VBLK_META_OP, VBLK_META_OP_USED);
}

static BOOL32 get_vbm_stat (UINT32 const bank, UINT32 const vbn)
{
    return tst_bit_dram (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE 
        + VBLK_META_OP, VBLK_META_OP_BLK);
}

static UINT32 get_vbm_vcnt (UINT32 const bank, UINT32 const vbn)
{
    return (UINT32)read_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE 
        + VBLK_META_VCNT);
}

static UINT32 get_vbm_ecnt (UINT32 const bank, UINT32 const vbn)
{
    return (UINT32)read_dram_8 (VBLK_META_ADDR + get_blk_offset(bank, VBLKS_PER_BANK, vbn) * VBLK_META_SIZE 
        + VBLK_META_ECNT);
}


// empty block bitmap 관리
static void set_using_blk (UINT32 const bank, UINT32 const vbn)
{
    UINT32 offset, bit_offset;

    offset = EMPTY_BLK_PER_BANK * bank + (vbn / 8);
    bit_offset = 7-(vbn % 8);
    clr_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

    // free block count 감소
    dec_free_blk_cnt(bank);

	// using block으로 입력
	set_vbm_used (bank, vbn);

    //uart_printf ("set_using_blk :: bank %d vbn %d offset %d, bit_offset %d", bank, vbn, offset, bit_offset);
}

// using bit을 표시하지 않고 나중에 사용할 용도로 check 하는 block
static void set_check_blk (UINT32 const bank, UINT32 const vbn)
{
    UINT32 offset, bit_offset;

    offset = EMPTY_BLK_PER_BANK * bank + (vbn / 8);
    bit_offset = 7-(vbn % 8);
    clr_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

    // free block count 감소
    dec_free_blk_cnt(bank);
	
	// using block을 제거
	set_vbm_notused (bank, vbn);

    //uart_printf ("set_using_blk :: bank %d vbn %d offset %d, bit_offset %d", bank, vbn, offset, bit_offset);
}

static void set_empty_blk (UINT32 const bank, UINT32 const vbn)
{
    UINT32 offset, bit_offset;

    offset = EMPTY_BLK_PER_BANK * bank + (vbn / 8);
    bit_offset = 7-(vbn % 8);
    set_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

    // free block count 증가
    inc_free_blk_cnt(bank);

	// 사용하지 않는 block으로 설정
	set_vbm_notused (bank, vbn);
}

static UINT32 get_empty_blk (UINT32 const bank)
{
    UINT32 offset, d32, mask;
    UINT32 i, bytes, vbn;	

    //uart_printf("get_empty_blk :: bank %d", bank);
	
	// bank에 빈 block이 없으면 0 반환
	if (get_free_blk_cnt (bank) == 0)
	{
		return 0;
	}

	// empty blk bitmap에서 한 byte씩 검사
	offset = EMPTY_BLK_PER_BANK * bank;
	for (vbn = 0; vbn < BLKS_PER_BANK; vbn++)
	{
		bytes = vbn / 8;

		if (tst_bit_dram(EMPTY_BLK_ADDR + offset + bytes, 7-(vbn & 0x7)))
		{
			// empty block bit map에 using으로 표시
			set_using_blk (bank, vbn);

			//uart_printf ("get_empty_blk :: bank %d vbn %d offset %d bit_offset %d", bank, vbn, offset+bytes, 7-(vbn & 0x7));

			return vbn;
		}
	}

	/*
    // empty blk bitmap에서 4 byte씩 검사
    offset = bank * BLKS_PER_BANK >> 3;
    for (bytes = 0; bytes < (BLKS_PER_BANK >> 3); bytes += 4)
    {
        d32 = read_dram_32 (EMPTY_BLK_ADDR + offset + bytes);

        if (d32 == 0)
        {
            continue;
        }

        // bank에 해당하는 bit를 검사
        mask = 0x80000000;
        for (i=0; i < 32; i++)
        {
            if (d32 & mask)
            {
                // bit에 empty로 표시되었을 경우 vbn 반환
                vbn = bytes * 8 + i;
                // empty block bit map에 using으로 표시
                set_using_blk (bank, vbn);

#ifdef __TEST_WRT
                uart_printf ("get_empty_blk :: vbn %d offset %d bit_offset %d", vbn, offset+bytes, 32-i);
#endif

                return vbn;
            }
            mask = mask >> 1;
        }
    }
	*/
}

// janus ftl fuction
static void fusion (UINT32 const bank, UINT32 const lbn)
{
#ifdef __TEST_FUSION
	//uart_printf ("fusion :: bank %d, lbn %d", bank, lbn);
#endif
	set_lbm_mapstat (bank, lbn, PAGE_MAPPING);
	set_vbm_lblk (bank, get_lpm_vpn (bank, (lbn << (BLK_TO_PAGE + 3)) + bank) >> BLK_TO_PAGE);
}

static void defusion (UINT32 const bank)
{
	UINT32 vblk, vpn, src_pn, dst_bn, dst_pn, dpn, vcnt;

#ifdef __TEST_DEFUSION
	uart_printf ("defusion :: under construction :(");
#endif

	// defusion victim block 선정
	vblk = get_df_victim_blk (bank);
	vpn = page_to_blk (vblk);
	// defusion 할 block이 없을 경우 강제 종료
	if (vblk == 0)
	{
		return;
	}

	// defusion 결과가 들어갈 block
	dst_bn = get_df_block (bank);
	dpn = blk_to_page (dst_bn);

	// defusion
	for (dst_pn = 0; dst_pn < PAGES_PER_BLK; dst_pn++)
	{
		// logical page가 위치한 virtual page load
		src_pn = get_lpm_vpn (bank, vpn);

		// copy back
		nand_page_copyback (bank, page_to_blk(src_pn), page_offset(src_pn), dst_bn, dst_pn);

		// 전에 사용한 page는 invalid로 표시
		vcnt = set_vpm_invalid (bank, src_pn);

		// 해당 block이 비었으면 erase하고 empty block으로 환원
		if (vcnt == 0)
		{
			nand_block_erase (bank, page_to_blk (src_pn));
			set_empty_blk (bank, page_to_blk (src_pn));
		}
		// invalid 된 block의 valid count가 garbage collection victim cost보다 작을 경우 교체
		else if (vcnt < get_gc_victim_cost (bank))
		{
			set_gc_victim_cost (bank, vcnt);
			set_gc_victim_blk (bank, page_to_blk(src_pn));
		}

		// mapping table 갱신
		set_lpm_vpn (bank, vpn, dpn);
		set_vpm_lpn (bank, dpn, vpn);
		
		dpn++;
		vpn++;
	}

	// data block으로 설정
	set_lbm_mapstat (bank, vblk, BLOCK_MAPPING);
	set_vbm_dblk (bank, dst_bn);

	// empty block이 있는 경우 새로 defusion용 block으로 설정
	if (get_free_blk_cnt (bank) > 0)
	{
		UINT32 vbn;

		vbn = get_empty_blk (bank);
		set_df_block (bank, vbn);
		set_check_blk (bank, vbn);
	}
	// empty block이 없을 경우
	else
	{
		UINT32 vbn;

		// garbage collection으로 empty block을 생성
		do
		{
			garbage_collection (bank);
		}
		while (get_free_blk_cnt(bank) == 0);

		// empty block이 생기면 defusion용 block으로 설정
		vbn = get_empty_blk (bank);
		set_df_block (bank, vbn);
		set_check_blk (bank, vbn);	
	}
}

static UINT32 garbage_collection (UINT32 const bank)
{
	UINT32 cnt_vblk, cnt_nblk, i;
	UINT32 vpn, npn, vbn, nbn, lpn, new_vpn;

#ifdef __TEST_GC
	uart_printf ("garbage_collection :: bank %d", bank);
#endif

	vbn = get_gc_victim_blk (bank);
	vpn = blk_to_page (vbn);

	// defusion 과정에서 empty block을 만들기 위해 gc를 할수 있으므로 현재 write page가 있을 경우 그 block으로 gc를 한다
	npn = get_write_page (bank);
	if (npn == 0)
	{
		// block이 가득 차서 garbage collection을 할 경우 page가 가득 찼으므로 무조건 current write page는 0
		// write 과정에서 block을 다 쓰면 current write page를 0으로 입력하도록 설정함
		nbn = get_gc_block (bank);
		set_vbm_used (bank, nbn);
		npn = 0;
	}
	else
	{
		/*
		 defusion 과정에서는 아무때나 garbage collection을 부르므로 write page가 남아있는데 garbage collection이
		 발생할 수 있음. 이 때는 새 empty block을 만들기 위해서 garbage collection을 하는 것이므로 current write page에
		 이어서 write를 해서 empty block을 만든다
		*/
		nbn = page_to_blk(npn);
		npn = npn & 127;
	}
	
	// victim block에서 valid한 page를 copy
	for (i=0; i < PAGES_PER_BLK; i++)
	{
		// valid page인지 확인
		if (get_vpm_valid (bank, vpn))
		{
			// valid page이면 copy한다
			nand_page_copyback (bank, vbn, i, nbn, npn);

			// copy된 page는 invalid로 설정
			set_vpm_invalid (bank, vpn);

			// logical page mapping table과 virtual page mapping table 갱신
			lpn = get_vpm_lpn (bank, vpn);
			new_vpn = blk_to_page (nbn) + npn;
			set_lpm_vpn (bank, lpn, new_vpn);
			set_vpm_lpn (bank, new_vpn, lpn);

			// new block에서 다음에 쓸 page count를 증가시킨다
			npn++;
			if ((npn & 127) == 0)
			{
				// 쓰던 block을 다 썼을 경우 garbage collection 용도로 할당해 놓은 block을 사용한다
				nbn = get_gc_block (bank);
				set_vbm_used (bank, nbn);
				npn = 0;
			}
		}

		vpn++;
	}

	nand_block_erase (bank, vbn);
	// garbage collection용 block을 사용하지 않고 gc를 완료했을 경우 새로 생긴 block은 empty block으로 반환
	if (get_gc_block (bank) != nbn)
	{
		set_empty_blk (bank, vbn);
	}
	// gc block을 사용해서 gc를 완료했을 경우 새로 생긴 block은 gc block으로 설정
	else
	{
		set_gc_block (bank, vbn);
	}

	// 다음 victim block을 미리 선정
	calc_gc_victim_blk (bank);

	// current write page를 갱신한다
	set_write_page (bank, (blk_to_page (nbn) + npn));

	return (blk_to_page(nbn) + npn);
}


static UINT32 get_df_victim_blk (UINT32 const bank)
{
	UINT32 lbn, lbn_min, ts_now, ts_lbn, ts_min, ts_diff;

#ifdef __TEST_DEFUSION
	
#endif
	// logical block을 돌면서 page mapping 상태이고 모든 page가 기록된 block 중 time stamp가 가장 늦은 block을 찾는다
	ts_now = get_timer(bank);
	ts_min = ts_now;
	lbn_min = 0;
	for (lbn = 0; lbn < LBLKS_PER_BANK; lbn++)
	{
		// block mapping인 경우 검사하지 않는다
		if (get_lbm_mapstat (bank, lbn) != PAGE_MAPPING)
		{
			continue;
		}
		// 모든 page가 기록중이 아닌 경우 검사하지 않는다
		if (get_lbm_vcnt (bank, lbn) != 127)
		{
			continue;
		}

		ts_lbn = get_lbm_timestamp (bank, lbn);
		
		// 현재 최소값이 time stamp를 한바퀴 돌지 않았을때
		if (ts_min <= ts_now)
		{
			if ((ts_lbn < ts_min) || (ts_lbn > ts_now))
			{
				ts_min = ts_lbn;
				lbn_min = lbn;
			}
		}
		// 현재 최소값이 time stamp를 한바퀴 돌아서 현재 시간보다 클때
		else 
		{
			if ((ts_lbn < ts_min) && (ts_now < ts_lbn))
			{
				ts_min = ts_lbn;
				lbn_min = lbn;
			}
		}
	}

	return lbn_min;
}


static UINT32 calc_gc_victim_blk (UINT32 const bank)
{
	// 모든 block을 돌면서 valid page count가 가장 적은 page mapping block을 찾는다
	UINT32 cnt_min, cnt_vbn, vbn_min, vbn;

	cnt_min = 128;
	vbn_min = 0;
	for (vbn = 0; vbn < VBLKS_PER_BANK; vbn++)
	{
		// page mapping인 virtual block에서만 valid page count 검사
		if ((get_vbm_stat == PAGE_MAPPING) && (is_vbm_used (bank, vbn) != 0))
		{
			cnt_vbn = get_vbm_vcnt (bank, vbn);
			if (cnt_vbn < cnt_min)
			{
				cnt_min = cnt_vbn;
				vbn_min = vbn;
			}
		}
	}

	// garbage collection용 victim block에 입력
	set_gc_victim_cost(bank, cnt_min);
	set_gc_victim_blk (bank, vbn_min);

	return vbn_min;
}


static UINT32 calc_avg_cost (UINT32 const bank)
{
	// page write에 소모되는 평균 cost를 계산
	return 0;
}

static UINT32 calc_hit_rate (UINT32 const bank)
{
}

//=================================================================




/*
// logical page가 들어간 data block의 virtual block number를 반환 - complete
static UINT32 get_dblock (UINT32 const lpn)
{
    UINT32 blk, lbpn, bank;
    
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    blk = lbpn / PAGES_PER_BLK;

    // 해당 logical page의 data block 번호를 반환 ( 0x00000000 : Invalid)
    return read_dram_32 (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR);		
}

// logical page의 data block이 존재하는지 확인 - complete
static BOOL8 is_exist_dblock (UINT32 const lpn)
{
    UINT32 lbpn, bank;
    UINT32 blk;

    //uart_printf ("is exist dblock : %d", lpn);

    // DRAM에서 data block의 option 정보 read
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    blk = lbpn / PAGES_PER_BLK;

    //uart_printf ("data block option :: %x", read_dram_8 (DATA_BLK_ADDR + (DATA_BLK_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_OP));

    // block valid bit이 활성화 되있는지 검사
    //if (tst_bit_dram (DATA_BLK_ADDR + (DATA_BLK_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_OP, DATA_BLK_OP_EX)) {
    if (read_dram_32 (DATA_BLK_ADDR + (DATA_BLK_SIZE * (blk * NUM_BANKS + bank)) + DATA_BLK_VADDR) != 0) {
        //uart_printf ("dblock is exist");
        return TRUE;
    }
    return FALSE;
}

// logical page가 data block이나 log block에 기록된 적이 있는지 확인 - complete
static BOOL8 is_exist_dpage (UINT32 const lpn)
{
    UINT32 blk, lbpn, bank, offset, bit_offset;

    // DRAM에서 data block의 page mapping 정보 read
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    blk = lbpn / PAGES_PER_BLK;
    offset = lbpn % PAGES_PER_BLK;
    bit_offset = offset % 8;

    // data block에서 valid 상태인지 검사
    if (tst_bit_dram (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset))
    {
        return TRUE;
    }
    else
    {
        // log block이 있는지 검사
        blk = get_log_blk (lpn);
        if (blk != 0xff) 
        {
            //log block에 page가 기록되어 있는지 검사
            if (get_logblk_page (blk, lpn) == NULL)
            {
                return FALSE;
            }
            else
            {
                return TRUE;
            }
        } 
        else
        {
            return FALSE;
        }
    }
}

// logical page가 data block에서 valid 상태인지 확인 - complete
static BOOL8 is_valid_dblock (UINT32 const lpn)
{
    UINT32 blk, offset, bitoffset;
    UINT32 bank, lbpn;

    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    blk = lbpn / PAGES_PER_BLK;
    offset = lbpn % PAGES_PER_BLK;
    bitoffset = offset % 8;

    // data block mapping data에서 해당 page의 page bitmap을 검사한다
    if (tst_bit_dram (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8),
                    bitoffset))
    {
        return TRUE;
    }
    
    return FALSE;
}

// block을 할당 받지 않은 data block에 새로 block을 할당 - complete
static UINT32 assign_dblock (UINT32 const lpn)
{
    UINT32 vblk, blk, bank, lbpn, map_offset;
    
    //uart_printf("assign new data block : %d", lpn);
    // data block용 새로운 block 할당
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    blk = lbpn / PAGES_PER_BLK;
    vblk = get_empty_blk(bank);

    //uart_printf("new data block number :%d", vblk);
    // data block mapping data 입력
    map_offset = blk * NUM_BANKS + bank;
    write_dram_32 (DATA_BLK_ADDR + (map_offset * DATA_BLK_SIZE) + DATA_BLK_VADDR, vblk); // virtual block number 입력
    //write_dram_8 (DATA_BLK_ADDR + (map_offset * DATA_BLK_SIZE) + DATA_BLK_OP, 0x80);	// option 입력
    set_bit_dram (DATA_BLK_ADDR + (map_offset * DATA_BLK_SIZE) + DATA_BLK_OP, DATA_BLK_OP_EX);

    return vblk;
}

// logical page에 해당하는 virtual page의 주소를 읽어온다
static UINT32 get_dpage (UINT32 const lpn)
{
    UINT32 vbn;

    vbn = get_dblock (lpn);

    return ((vbn * PAGES_PER_BLK) + ((lpn / NUM_BANKS) % PAGES_PER_BLK));
}

// data block에서 logical page를 할당받는다 - complete
static UINT32 set_dpage (UINT32 const lpn)
{
    UINT32 blk, offset, vblk, bit_offset;
    UINT32 bank, lbpn;

    //uart_printf ("set_datablock_page : %d", lpn);

    // logical block number와 offset 계산
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    blk = lbpn / PAGES_PER_BLK;
    offset = lbpn % PAGES_PER_BLK;
    bit_offset = offset % 8;
    
    // virtual data block number를 읽어온다
    vblk = read_dram_32 (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR);

    // data block의 valid page bitmap에 valid로 체크
    set_bit_dram (DATA_BLK_ADDR + ((blk * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset);

    //uart_printf ("set_dpage :: vpn %d", (vblk*PAGES_PER_BLK) + offset);

    // virtual page number 반환
    return ((vblk * PAGES_PER_BLK) + offset);
}

// data block에서 logical page를 invalid 시킨다 - complete
static BOOL8 set_invalid_dpage (UINT32 const lpn)
{
    UINT32 lbn, offset, bit_offset;
    UINT32 bank, lbpn;

    // block number와 offset 계산
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    lbn = lbpn / PAGES_PER_BLK;
    offset = lbpn % PAGES_PER_BLK;
    bit_offset = offset % 8;

    // 현재 상태와 상관없이 valid bit를 clear
    clr_bit_dram (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (offset / 8), bit_offset);

    return TRUE;
}

// log block에서 logical page를 dirty invalid로 체크한다 - complete
static BOOL8 set_dirty_log_page (UINT32 const lpn)
{
    UINT32 lblk, i;
    UINT32 bank, lbpn, offset, map_offset;
    UINT8 d8;

    // page의 block 내 offset 계산
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    offset = lbpn % PAGES_PER_BLK;

    // log block 번호 검색
    lblk = get_log_blk (lpn);

    // log block의 page table에서 page가 존재하는지 검색
    map_offset = bank * NUM_LOG_BLKS + lblk;
    for (i=0; i < PAGES_PER_BLK; i++) 
    {
        // log block mapping table에서 page offset을 통해 logical page를 찾는다
        d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i);
        
        if (d8 == offset)
        {			
            // log block mapping table에서 dirty로 입력
            write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i, 0xFE);

            // valid page counter 감소
            d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
            write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG, d8-1);
            
            return TRUE;
        }
    }

    return FALSE;
}

// data block에 log block을 새로 연결한다 - complete
static UINT32 set_log_blk (UINT32 const lpn)
{
    UINT32 vblk, lbn, bank;
    UINT32 d, i, lbpn, offset;
    UINT8 d8;

    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    lbn = lbpn / PAGES_PER_BLK;

    // log block 갯수가 가득 찼으면 garbage collection을 한다
    //if (log_blk_cnt[bank] == NUM_LOG_BLKS) 
    if (get_logblk_cnt(bank) == NUM_LOG_BLKS)
    {
        garbage_collection (bank, NUM_LOG_BLKS);
    }
    vblk = get_empty_blk (bank);

    // log block mapping table에서 비어있는 자리 검색
    for (i=0; i < NUM_LOG_BLKS; i++) 
    {
        d = read_dram_32 (LOG_BLK_ADDR + (bank * NUM_LOG_BLKS + i) * LOG_BLK_SIZE + LOG_BLK_VADDR);

        // virtual block number가 0인 곳에 새 log block을 할당
        if (d == 0)
        {
            break;
        }
    }

    ASSERT (i != NUM_LOG_BLKS);

    // data block mapping table에 log block 정보 기록
    d8 = 0b11000000 + (UINT8)i;
    write_dram_8 (DATA_BLK_ADDR + (lbn * NUM_BANKS + bank) * DATA_BLK_SIZE + DATA_BLK_OP, d8);

#ifdef __TEST_LB
    uart_printf ("set_log_blk :: bank %d vbn %d logblk %d", bank, vblk, i);
#endif

    // log block mapping table에 mapping data 기록
    offset = bank * NUM_LOG_BLKS + i;
    write_dram_32 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_VADDR, vblk); // virtual block number 기록
    write_dram_32 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_LADDR, lbn); // logical block number 기록
    write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_PGCNT, 0); // page counter 0로 초기화
    write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_VLDPG, 0); // valid page counter 0으로 초기화
    write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_MERGE, 0); // merge status를 partial merge로 초기화
    for (d=0; d < PAGES_PER_BLK; d++)		// page number map을 invalid로 초기화
    {
        write_dram_8 (LOG_BLK_ADDR + offset * LOG_BLK_SIZE + LOG_BLK_PGMAP + d, 0xff);
    }

    // log block counter 증가
    inc_logblk_cnt (bank);
    //log_blk_cnt[bank]++;

    return vblk;	// 새로 할당 된 log block의 virtual block number를 반환
}

// data block에 연결된 log block의 번호를 받아온다 - complete
static UINT32 get_log_blk (UINT32 const lpn) 
{
    UINT32 lbn, bank, lbpn;
    UINT8 d8;

    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    lbn = lbpn / PAGES_PER_BLK;

    if (tst_bit_dram (DATA_BLK_ADDR + (DATA_BLK_SIZE * (lbn * NUM_BANKS + bank)) + DATA_BLK_OP, DATA_BLK_OP_LB))
    {
        // log block이 있을 경우 log block의 번호 반환
        d8 = read_dram_8 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_OP);
        d8 = d8 & DATA_BLK_OP_LBNUM;

        return (UINT32)d8;
    }
    else
    {
        // log block이 없을 경우 0xff 반환
        return 0xff;
    }
}

// logical page 번호를 받아 log block에서 해당하는 page를 찾아서 반환한다 - complete
static UINT32 get_log_page (UINT32 const lpn)
{
    UINT32 lblk;

    lblk = get_log_blk (lpn);
    if (lblk != 0xff)
    {
        return get_logblk_page (lblk, lpn);
    }
    else
    {
        //uart_printf("get_log_page :: ALERT return address is NULL");
        return NULL;
    }
}

// log block에 page를 할당한다 - complete
static UINT32 set_log_page (UINT32 const lpn)
{
    UINT32 offset, lbpn, bank, page_offset;
    UINT32 lblk, lpage, loffset, map_offset;
    UINT8 d8;
        
    // log block에서 다음에 쓸 page 위치를 받는다
    bank = get_num_bank(lpn);
    lblk = get_log_blk (lpn);	// data block에 연결된 log block의 번호를 받는다
    map_offset = bank * NUM_LOG_BLKS + lblk;
    d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGCNT);
    loffset = (UINT32)d8;	// log block에 다음에 page를 쓸 위치

    // log block이 가득 찼으면 garbage collection을 한다
    if (loffset == PAGES_PER_BLK)
    {
        garbage_collection (bank, lblk);
        
        // data block에 page가 있을 경우 새로 log block 할당
        if (is_valid_dblock (lpn) == TRUE)
        {
            set_invalid_dpage (lpn);
            set_log_blk (lpn);
            return set_log_page(lpn);
        }
        else
        {
            return set_dpage(lpn);
        }
    }

    //log block에서 page를 invalid 시킨다
    set_dirty_log_page (lpn);

    // log block에서 page의 주소를 계산
    lpage = read_dram_32 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);
    lpage = lpage * PAGES_PER_BLK + loffset;

    //uart_printf ("set_log_page :: bank %d lpn %d log page %d", bank, lpn, lpage);

    // log block mapping table에 logical page를 연결
    lbpn = lpn / NUM_BANKS;
    offset = lbpn % PAGES_PER_BLK;
    write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + loffset, (UINT8)offset);
    
    // log block에 page를 기록할 위치를 한칸 뒤로 이동한다
    write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGCNT, (UINT8)(loffset + 1));

    // valid page counter를 증가시킨다
    d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
    write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VLDPG, d8 + 1);

    // 이번에 쓴 page가 순서에 맞지 않을 경우 merge 상태를 full merge로 입력
    page_offset = (lpn / NUM_BANKS) % PAGES_PER_BLK;	// 이번에 쓰는 page의 block 내 offset 계산
    if (page_offset != loffset)	// 이번에 쓴 위치와 비교
    {
        //full merge로 표시
        write_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_MERGE, 1);
    }

    return lpage;
}

// log block에 있는 logical page의 virtual address를 반환 - complete
static UINT32 get_logblk_page (UINT32 const lblk, UINT32 const lpn)
{
    UINT32 i, offset, blk, bank, lbpn;
    UINT8 d8, map_offset;
    
    // page의 block 내 offset 계산
    bank = get_num_bank (lpn);
    lbpn = lpn / NUM_BANKS;
    offset = lbpn % PAGES_PER_BLK;

    // log block의 page table에서 page가 존재하는지 검색
    map_offset = bank * NUM_LOG_BLKS + lblk;
    for (i=0; i < PAGES_PER_BLK; i++) 
    {
        // log block mapping table에서 page offset을 통해 logical page를 찾는다
        d8 = read_dram_8 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i);
        
        if (d8 == offset)
        {
            // log block의 virtual block number read
            blk = read_dram_32 (LOG_BLK_ADDR + (map_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);

            // page의 virtual page number 반환
            return ((blk * PAGES_PER_BLK) + i);
        }
    }

    // 존재하지 않을 경우 null 반환
    return NULL;
}

// 같은 bank에서 empty block 한개를 반환한다 - complete
static UINT32 get_empty_blk (UINT32 const bank)
{
    UINT32 offset;
    UINT32 i, bytes, vbn;
        
    //uart_printf("get_empty_blk :: bank %d", bank);

    // empty blk bitmap에서 한 byte씩 검사
    offset = bank * BLKS_PER_BANK / 8;
    for (bytes = 0; bytes < (BLKS_PER_BANK / 8); bytes++)
    {
        // bank에 해당하는 bit를 검사
        for (i=0; i < 8; i++)
        {
            if (tst_bit_dram(EMPTY_BLK_ADDR + offset + bytes, 7-i))
            {
                // bit에 empty로 표시되었을 경우 vbn 반환
                vbn = bytes * 8 + i;
                // empty block bit map에 using으로 표시
                set_using_blk (bank, vbn);

                //uart_printf ("get_empty_blk :: vbn %d offset %d bit_offset %d", vbn, offset+bytes, 7-i);

                return vbn;
            }
        }
    }
    
    // 그래도 빈 block이 없으면 garbage collection 시도
    return garbage_collection(bank, NUM_LOG_BLKS);
}

// 해당 bank의 virtual block을 using 상태로 체크 - incomplete
static BOOL8 set_using_blk (UINT32 const bank, UINT32 const vbn)
{
    UINT32 offset, bit_offset;

    offset = (bank * BLKS_PER_BANK / 8) + (vbn / 8);
    bit_offset = 7-(vbn % 8);
    clr_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

    //uart_printf ("set_using_blk :: bank %d vbn %d offset %d, bit_offset %d", bank, vbn, offset, bit_offset);

    return TRUE;
}

static BOOL8 set_empty_blk (UINT32 const bank, UINT32 const vbn)
{
    UINT32 offset, bit_offset;

    offset = (bank * BLKS_PER_BANK / 8) + (vbn / 8);
    bit_offset = 7-(vbn % 8);
    set_bit_dram (EMPTY_BLK_ADDR + offset, bit_offset);

    return TRUE;
}

// garbage collection - complete
static UINT32 garbage_collection (UINT32 const bank, UINT32 const logblk)
{
    UINT32 vblk, lbn, vbn, new_dbn, offset;
    UINT32 i32, is_fmerge, j32;
    UINT8 d8;

    // garbage collection을 할 victim block 선정
    if (logblk == NUM_LOG_BLKS) 
    {
        vblk = get_victim_block (bank);
    } 
    else
    {
        vblk = logblk;
    }
    lbn = read_dram_32 (LOG_BLK_ADDR + ((bank * NUM_LOG_BLKS + vblk) * LOG_BLK_SIZE) + LOG_BLK_LADDR);
    vbn = read_dram_32 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR);

#ifdef __TEST_GC
    uart_printf ("garbage_collection :: bank %d victim blk %d", bank, vblk);
#endif

    // log block에 page가 순서대로 위치하는지 검사
    is_fmerge = 0;
    for (i32 = 0; i32 < PAGES_PER_VBLK; i32++)
    {
        d8 = read_dram_8 (LOG_BLK_ADDR + ((bank * NUM_LOG_BLKS + vblk) * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i32);

        // log block에 기록된 적 없는 영역인 경우
        if (d8 == 0xff)
        {
            // 순서대로 기록되어 있고 빈 공간이 있으므로 partial merge
            break;
        }
        // log block에 page가 순서대로 들어있지 않은 상태
        else if (d8 != (UINT8)i32)
        {
            //uart_printf("lpn %d, logpn %d", i32, d8);
            is_fmerge = 1;
            //break;
        }
    }

    // partial merge
    if (is_fmerge == 0)
    {
        new_dbn = partial_merge(bank, vblk, lbn);
    }
    // full merge
    else
    {
        new_dbn = full_merge(bank, vblk, lbn);
    }

    // data block mapping table 갱신
    write_dram_32 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_VADDR, new_dbn);
    write_dram_32 (DATA_BLK_ADDR + ((lbn * NUM_BANKS + bank) * DATA_BLK_SIZE) + DATA_BLK_OP, 0x80);

    // 이전에 사용한 data block을 empty block mapping table에 표시
    set_empty_blk (bank, vbn);

    // log block mapping table 초기화
    offset = bank * NUM_LOG_BLKS + vblk;
    write_dram_32 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_VADDR, 0x00000000);
    write_dram_32 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_LADDR, 0x00000000);
    write_dram_8 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_PGCNT, 0x00);
    for (j32 = 0; j32 < PAGES_PER_BLK; j32++) 
    {
        write_dram_8 (LOG_BLK_ADDR + (offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + j32, 0xff);
    }

    // log block counter 감소
    dec_logblk_cnt  (bank);
    //log_blk_cnt[bank]--;

    // merge이전에 사용하던 data block이 empty 상태이므로 data block을 반환
    return vbn;
}

// partial merge - complete
static UINT32 partial_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn)
{
    UINT32 vbn, offset, bit_offset, lvbn;
    UINT32 dmap_offset, lmap_offset;
    UINT32 i32;
    UINT8 d8;

#ifdef __TEST_GC
    uart_printf ("partial_merge :: bank %d, vblk %d, lbn %d", bank, vblk, lbn);
#endif

    // garbage collection을 할 block의 virtual block number read
    dmap_offset = lbn * NUM_BANKS + bank;
    lmap_offset = bank * NUM_LOG_BLKS + vblk;
    vbn = read_dram_32 (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VADDR);
    lvbn = read_dram_32 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);
    
    for (i32 = 0; i32 < PAGES_PER_VBLK; i32++)
    {
        // log block에 있는 page 번호 검사
        d8 = read_dram_8 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + i32);

        // log block에 정위치에 있는 경우 data block의 page bitmap에 표시
        if (d8 == (UINT8)i32)
        {
            set_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + (i32 / 8), (i32 % 8));
        }
        // log block에 없는 page인 경우 data block에서 이동
        else if (d8 == 0xff)
        {
            // offset 계산
            offset = i32 / 8;
            bit_offset = i32 % 8;

            // data block에 page가 있는지 확인
            if (tst_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + offset, bit_offset))
            {
                // data block에 있는 내용을 log block으로 copy back
                nand_page_copyback (bank, vbn, i32, lvbn, i32);
            }
        }
    }
    // 정리가 끝난 data block은 erase
    nand_block_erase (bank, vbn);

#ifdef __TEST_GC
    uart_printf ("partial_merge :: end", bank, vblk, lbn);
#endif

    return lvbn;
}

// full merge - complete
static UINT32 full_merge (UINT32 const bank, UINT32 const vblk, UINT32 const lbn)
{
    UINT32 i32, j32, offset, bit_offset;
    UINT32 nblk, vbn, lvbn, dmap_offset, lmap_offset;
    UINT8 d8;

    // 새로 만들 block 할당
    nblk = get_gc_vblock (bank);

    dmap_offset = lbn * NUM_BANKS + bank;
    lmap_offset = bank * NUM_LOG_BLKS + vblk;

#ifdef __TEST_GC
    uart_printf ("full_merge :: bank %d, log blk %d, lbn %d, newblk %d", bank, vblk, lbn, nblk);
#endif

    // 0번 page부터 순서대로 이동
    vbn = read_dram_32 (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VADDR);
    lvbn = read_dram_32 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_VADDR);
    for (i32 = 0; i32 < PAGES_PER_BLK; i32++)
    {
        //data block에 있는 경우 data block에서 복사
        offset = i32 / 8;
        bit_offset = i32 % 8;

        // data block에 있는 page인 경우
        if (tst_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + offset, bit_offset))
        {
            // data block에 있는 내용을 새 block으로 copy back
#ifdef __TEST_GC
            uart_printf ("full_merge :: %d from data blk to new blk", i32);
#endif
            nand_page_copyback (bank, vbn, i32, nblk, i32);
        }
        // log block에 있는지 검사
        else
        {
            // log block mapping table 전체 검사
            for (j32 = 0; j32 < PAGES_PER_BLK; j32++)
            {
                d8 = read_dram_8 (LOG_BLK_ADDR + (lmap_offset * LOG_BLK_SIZE) + LOG_BLK_PGMAP + j32);

                // log block에 찾는 번호가 있을 경우
                if (d8 == i32)
                {
#ifdef __TEST_GC
                    uart_printf ("full_merge :: page %3d is moved from log block %2d's %3d page to new block %2d", d8, lvbn, j32, nblk);
#endif
                    nand_page_copyback (bank, lvbn, j32, nblk, i32);
                    // data block bitmap에 표시
                    set_bit_dram (DATA_BLK_ADDR + (dmap_offset * DATA_BLK_SIZE) + DATA_BLK_VPBMP + offset, bit_offset);
                    break;
                }	
            }
        }
    }

    // 정리가 끝난 data block과 log block을 erase
    nand_block_erase (bank, vbn);
    nand_block_erase (bank, lvbn);
    
    // 이전에 사용한 log block은 다음 full merge에 사용할 new block으로 설정
    set_gc_vblock (bank, lvbn);

    // 새로 완성된 block의 주소 반환
    return nblk;
}

// victim block을 선정한다 - sample
static UINT32 get_victim_block (UINT32 const bank)
{
    UINT32 i32, map_offset, sel_blk, sel_max;
    UINT8 d8;

    map_offset = NUM_LOG_BLKS * bank;

    // partial merge가 되는 block 중 valid page가 가장 많은 block 선택	
    sel_blk = -1;
    sel_max = 0;
    for (i32 = 0; i32 < NUM_LOG_BLKS; i32++)
    {
        // partial merge가 가능한 block
        if (read_dram_8 (LOG_BLK_ADDR + ((map_offset + i32)* LOG_BLK_SIZE) + LOG_BLK_MERGE) == 0)
        {
            // 현재 최댓값과 비교
            d8 = read_dram_8 (LOG_BLK_ADDR + ((map_offset + i32) * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
            if (d8 >= sel_max)
            {
                sel_blk = i32;
                sel_max = (UINT32)d8;
            }
        }
    }
    // partial merge가 가능한 block이 있으면 우선적으로 처리
    if (sel_blk != -1)
    {
        return sel_blk;
    }

    // full merge가 되는 block 중에 valid page가 가장 적은 block을 선택
    sel_max = PAGES_PER_BLK;
    for (i32 = 0; i32 < NUM_LOG_BLKS; i32++)
    {
        d8 = read_dram_8 (LOG_BLK_ADDR + ((map_offset + i32) * LOG_BLK_SIZE) + LOG_BLK_VLDPG);
        if (d8 < PAGES_PER_BLK)
        {
            sel_blk = i32;
            sel_max = d8;
        }
    }
    return sel_blk;
}
*/



//sram 내용을 nand flash에 logging한다
static void logging_misc_block (void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 bank;

#ifdef __TEST_LOGGING
    uart_printf ("logging_misc_block :: start");
#endif
    flash_finish();

    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        nand_block_erase(bank, get_miscblk_vbn(bank));        
    }

    // srma에 있는 misc metadata를 nand로 저장
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        mem_copy_mod((void *)FTL_BUF(bank), (void *)&g_misc_meta[bank], misc_meta_bytes);

        nand_page_ptprogram(bank,
                            get_miscblk_vbn(bank),
                            0,
                            0,
                            NUM_MISC_META_SECT,
                            FTL_BUF(bank));
    }

    flash_finish();
}

/*
static void logging_dram_block (UINT32 const logging_bytes, UINT32 const start_addr, UINT32 const blks_per_bank)
{
    UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;
    UINT32 * pmap;

#ifdef __TEST_LOGGING
    uart_printf ("logging_block :: start");
#endif

    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++) 
    {
        for (i32 = 0; i32 < blks_per_bank; i32++)
        {
            nand_block_erase (bank, get_mapblk_vbn (bank, i32));
        }
    }

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    remain_bytes = logging_bytes;
    dblk_addr = start_addr;
    while (remain_bytes > 0) 
    {
        // 각 bank에 backup
        for (bank = 0; bank < NUM_BANKS; bank++) 
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }

            vbn = get_mapblk_vbn (bank, nblk);
            
            // data block mapping table을 ftl buffer로 이동
            mem_copy_mod ((void *)FTL_BUF(bank), (void *)(dblk_addr), write_bytes);

            // ftl buffer에서 nand flash로 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptprogram (bank, vbn, vpn, 0, sectors, FTL_BUF(bank));

            remain_bytes -= write_bytes;
            dblk_addr += write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

    flash_finish();
}
*/

static void logging_lpm_block (void)
{	
    UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;

#ifdef __TEST_LOGGING
    uart_printf ("logging_data_block :: start");
#endif

    remain_bytes = LPAGE_MAP_BYTES;

    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++) 
    {
        for (i32 = 0; i32 < LPMAP_PER_BANK; i32++)
        {
            nand_block_erase (bank, get_lpmblk_vbn (bank, i32));
        }
    }

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = LPAGE_MAP_ADDR;
    while (remain_bytes > 0) 
    {
        // 각 bank에 backup
        for (bank = 0; bank < NUM_BANKS; bank++) 
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }

            vbn = get_lpmblk_vbn (bank, nblk);
            
            // data block mapping table을 ftl buffer로 이동
            mem_copy_mod ((void *)FTL_BUF(bank), (void *)(dblk_addr), write_bytes);

            // ftl buffer에서 nand flash로 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptprogram (bank, vbn, vpn, 0, sectors, FTL_BUF(bank));

            remain_bytes -= write_bytes;
            dblk_addr += write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

    flash_finish();
}

static void logging_lbm_block (void)
{	
    UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;

#ifdef __TEST_LOGGING
    uart_printf ("logging_data_block :: start");
#endif

    remain_bytes = LBLK_META_BYTES;

    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++) 
    {
        for (i32 = 0; i32 < LBMETA_PER_BANK; i32++)
        {
            nand_block_erase (bank, get_lbmblk_vbn (bank, i32));
        }
    }

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = LBLK_META_ADDR;
    while (remain_bytes > 0) 
    {
        // 각 bank에 backup
        for (bank = 0; bank < NUM_BANKS; bank++) 
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }

            vbn = get_lbmblk_vbn (bank, nblk);
            
            // data block mapping table을 ftl buffer로 이동
            mem_copy_mod ((void *)FTL_BUF(bank), (void *)(dblk_addr), write_bytes);

            // ftl buffer에서 nand flash로 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptprogram (bank, vbn, vpn, 0, sectors, FTL_BUF(bank));

            remain_bytes -= write_bytes;
            dblk_addr += write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

    flash_finish();
}

static void logging_vpm_block (void)
{	
    UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;

#ifdef __TEST_LOGGING
    uart_printf ("logging_data_block :: start");
#endif

    remain_bytes = VPAGE_MAP_BYTES;

    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++) 
    {
        for (i32 = 0; i32 < VPMAP_PER_BANK; i32++)
        {
            nand_block_erase (bank, get_vpmblk_vbn (bank, i32));
        }
    }

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = VPAGE_MAP_ADDR;
    while (remain_bytes > 0) 
    {
        // 각 bank에 backup
        for (bank = 0; bank < NUM_BANKS; bank++) 
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }

            vbn = get_vpmblk_vbn (bank, nblk);
            
            // data block mapping table을 ftl buffer로 이동
            mem_copy_mod ((void *)FTL_BUF(bank), (void *)(dblk_addr), write_bytes);

            // ftl buffer에서 nand flash로 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptprogram (bank, vbn, vpn, 0, sectors, FTL_BUF(bank));

            remain_bytes -= write_bytes;
            dblk_addr += write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

    flash_finish();
}

static void logging_vbm_block (void)
{	
    UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;

#ifdef __TEST_LOGGING
    uart_printf ("logging_data_block :: start");
#endif

    remain_bytes = VBLK_META_BYTES;

    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++) 
    {
        for (i32 = 0; i32 < VBMETA_PER_BANK; i32++)
        {
            nand_block_erase (bank, get_vbmblk_vbn (bank, i32));
        }
    }

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = VBLK_META_ADDR;
    while (remain_bytes > 0) 
    {
        // 각 bank에 backup
        for (bank = 0; bank < NUM_BANKS; bank++) 
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }

            vbn = get_vbmblk_vbn (bank, nblk);
            
            // data block mapping table을 ftl buffer로 이동
            mem_copy_mod ((void *)FTL_BUF(bank), (void *)(dblk_addr), write_bytes);

            // ftl buffer에서 nand flash로 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptprogram (bank, vbn, vpn, 0, sectors, FTL_BUF(bank));

            remain_bytes -= write_bytes;
            dblk_addr += write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

    flash_finish();
}

static void logging_empty_block (void)
{
    UINT32 i32, bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;

#ifdef __TEST_LOGGING
    uart_printf ("logging_empty_block :: start");
#endif
    // block에 쓰인 이전 log를 삭제
    for (bank = 0; bank < NUM_BANKS; bank++) 
    {
        for (i32 = 0; i32 < EMPTYBLK_PER_BANK; i32++)
        {
            nand_block_erase (bank, get_emptyblk_vbn (bank, i32));
        }
    }

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    remain_bytes = EMPTY_BLK_BYTES;
    dblk_addr = EMPTY_BLK_ADDR;
    while (remain_bytes > 0) 
    {
        // 각 bank에 backup
        for (bank = 0; bank < NUM_BANKS; bank++) 
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }

            vbn = get_emptyblk_vbn (bank, nblk);
            
            // data block mapping table을 ftl buffer로 이동
            mem_copy_mod ((void *)FTL_BUF(bank), (void *)(dblk_addr), write_bytes);

            // ftl buffer에서 nand flash로 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptprogram (bank, vbn, vpn, 0, sectors, FTL_BUF(bank));

            remain_bytes -= write_bytes;
            dblk_addr += write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

    flash_finish();
}

//nand flash에서 sram의 전역변수 값들을 load한다
static void load_misc_block (void)
{
    UINT32 bank, blk;

#ifdef __TEST_PWRECV
    uart_printf ("load_misc_block :: start");
#endif 

    flash_finish();

    disable_irq ();
    flash_clear_irq ();

    // misc metadata를 nand에서 읽어온다
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        // 0번 block 이후에 가장 처음 나오는 valid block에 misc metadata가 저장
        blk = MISCBLK_VBN;
        while (is_bad_block (bank, blk) == TRUE)
        {
            blk++;
        }
        
#ifdef __TEST_PWRECV
        uart_printf ("load_misc_block :: bank %d load misc block from %d", bank, blk);
#endif 

        // misc. metadata read
        nand_page_ptread(bank,
                         MISCBLK_VBN,
                         0,
                         0,
                         NUM_MISC_META_SECT,
                         FTL_BUF(bank),
                         RETURN_ON_ISSUE);        
    }

    flash_finish();

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));
    }

    enable_irq();

#ifdef __TEST_PWRECV
    uart_printf ("load_misc_block :: end");
#endif 
}

static void load_lpm_block (void)
{
    UINT32 bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;
    UINT32 dram_remain_bytes;

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: start");
#endif 
    
    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = LPAGE_MAP_ADDR;
    remain_bytes = LPAGE_MAP_BYTES;
    dram_remain_bytes = remain_bytes;
    while (remain_bytes > 0) 
    {
        // flash에서 dram buffer로 이동
        for (bank = 0; bank <NUM_BANKS; bank++)
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }
        
            vbn = get_lpmblk_vbn (bank, nblk);
        
            // nand flash에서 ftl buffer로 logging data 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptread (bank, vbn, vpn, 0, sectors, FTL_BUF(bank), RETURN_ON_ISSUE);
            remain_bytes -= write_bytes;	
        }
        flash_finish();

        // dram buffer에서 mapping table로 이동
        for (bank = 0; bank < NUM_BANKS; bank++)
        {	
            if (dram_remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (dram_remain_bytes > 0)
            {
                write_bytes = dram_remain_bytes;
            }
            else
            {
                break;
            }

            // ftl buffer에서 data block mapping table로 logging data 이동
            mem_copy_mod ((void *)dblk_addr, (void *)FTL_BUF(bank), write_bytes);
            dblk_addr += write_bytes;
            dram_remain_bytes -= write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: end");
#endif 

}

static void load_lbm_block (void)
{
    UINT32 bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;
    UINT32 dram_remain_bytes;

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: start");
#endif 
    
    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = LBLK_META_ADDR;
    remain_bytes = LBLK_META_BYTES;
    dram_remain_bytes = remain_bytes;
    while (remain_bytes > 0) 
    {
        // flash에서 dram buffer로 이동
        for (bank = 0; bank <NUM_BANKS; bank++)
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }
        
            vbn = get_lbmblk_vbn (bank, nblk);
        
            // nand flash에서 ftl buffer로 logging data 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptread (bank, vbn, vpn, 0, sectors, FTL_BUF(bank), RETURN_ON_ISSUE);
            remain_bytes -= write_bytes;	
        }
        flash_finish();

        // dram buffer에서 mapping table로 이동
        for (bank = 0; bank < NUM_BANKS; bank++)
        {	
            if (dram_remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (dram_remain_bytes > 0)
            {
                write_bytes = dram_remain_bytes;
            }
            else
            {
                break;
            }

            // ftl buffer에서 data block mapping table로 logging data 이동
            mem_copy_mod ((void *)dblk_addr, (void *)FTL_BUF(bank), write_bytes);
            dblk_addr += write_bytes;
            dram_remain_bytes -= write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: end");
#endif 

}

static void load_vpm_block (void)
{
    UINT32 bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;
    UINT32 dram_remain_bytes;

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: start");
#endif 
    
    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = VPAGE_MAP_ADDR;
    remain_bytes = VPAGE_MAP_BYTES;
    dram_remain_bytes = remain_bytes;
    while (remain_bytes > 0) 
    {
        // flash에서 dram buffer로 이동
        for (bank = 0; bank <NUM_BANKS; bank++)
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }
        
            vbn = get_vpmblk_vbn (bank, nblk);
        
            // nand flash에서 ftl buffer로 logging data 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptread (bank, vbn, vpn, 0, sectors, FTL_BUF(bank), RETURN_ON_ISSUE);
            remain_bytes -= write_bytes;	
        }
        flash_finish();

        // dram buffer에서 mapping table로 이동
        for (bank = 0; bank < NUM_BANKS; bank++)
        {	
            if (dram_remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (dram_remain_bytes > 0)
            {
                write_bytes = dram_remain_bytes;
            }
            else
            {
                break;
            }

            // ftl buffer에서 data block mapping table로 logging data 이동
            mem_copy_mod ((void *)dblk_addr, (void *)FTL_BUF(bank), write_bytes);
            dblk_addr += write_bytes;
            dram_remain_bytes -= write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: end");
#endif 

}

static void load_vbm_block (void)
{
    UINT32 bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;
    UINT32 dram_remain_bytes;

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: start");
#endif 
    
    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = VBLK_META_ADDR;
    remain_bytes = VBLK_META_BYTES;
    dram_remain_bytes = remain_bytes;
    while (remain_bytes > 0) 
    {
        // flash에서 dram buffer로 이동
        for (bank = 0; bank <NUM_BANKS; bank++)
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }
        
            vbn = get_vbmblk_vbn (bank, nblk);
        
            // nand flash에서 ftl buffer로 logging data 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptread (bank, vbn, vpn, 0, sectors, FTL_BUF(bank), RETURN_ON_ISSUE);
            remain_bytes -= write_bytes;	
        }
        flash_finish();

        // dram buffer에서 mapping table로 이동
        for (bank = 0; bank < NUM_BANKS; bank++)
        {	
            if (dram_remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (dram_remain_bytes > 0)
            {
                write_bytes = dram_remain_bytes;
            }
            else
            {
                break;
            }

            // ftl buffer에서 data block mapping table로 logging data 이동
            mem_copy_mod ((void *)dblk_addr, (void *)FTL_BUF(bank), write_bytes);
            dblk_addr += write_bytes;
            dram_remain_bytes -= write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

#ifdef __TEST_PWRECV
    uart_printf ("load_data_block :: end");
#endif 

}

static void load_empty_block (void)
{
    UINT32 bank, nblk, vbn, vpn, dblk_addr;
    UINT32 remain_bytes, write_bytes, sectors;
    UINT32 dram_remain_bytes;

#ifdef __TEST_PWRECV
    uart_printf ("load_empty_block :: start");
#endif 

    // 각 bank의 data block mapping data 저장용 block에 mapping data를 나누어 저장한다
    nblk = 0;
    vpn = 0;
    dblk_addr = EMPTY_BLK_ADDR;
    remain_bytes = EMPTY_BLK_BYTES;
    dram_remain_bytes = EMPTY_BLK_BYTES;
    while (remain_bytes > 0) 
    {
        for (bank = 0; bank <NUM_BANKS; bank++)
        {
            //한번에 한 page씩 nand로 back up
            if (remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (remain_bytes > 0)
            {
                write_bytes = remain_bytes;
            }
            else
            {
                break;
            }
        
            vbn = get_emptyblk_vbn (bank, nblk);
        
            // nand flash에서 ftl buffer로 logging data 이동
            sectors = (write_bytes + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR;
            nand_page_ptread (bank, vbn, vpn, 0, sectors, FTL_BUF(bank), RETURN_ON_ISSUE);
            remain_bytes -= write_bytes;	
        }
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {	
            if (dram_remain_bytes > BYTES_PER_PAGE)
            {
                write_bytes = BYTES_PER_PAGE;
            }
            else if (dram_remain_bytes > 0)
            {
                write_bytes = dram_remain_bytes;
            }
            else
            {
                break;
            }

            // ftl buffer에서 data block mapping table로 logging data 이동
            mem_copy_mod ((void *)dblk_addr, (void *)FTL_BUF(bank), write_bytes);
            dblk_addr += write_bytes;
            dram_remain_bytes -= write_bytes;
        }

        // backup block과 page counter 증가
        vpn++;
        if (vpn == PAGES_PER_BLK)
        {
            nblk++;
            vpn = 0;
        }
    }

#ifdef __TEST_PWRECV
    uart_printf ("load_empty_block :: end");
#endif 
}




static void mem_copy_mod (void * const dst, const void * const src, UINT32 const num_bytes)
{
    UINT32 i32, d, s;
    UINT8 d8;

    d = (UINT32) dst;
    s = (UINT32) src;

    //uart_printf("mem_copy_mod :: num_bytes %d", num_bytes);

    // dram to dram copy에서 ecc size에 align 되지 않은 경우
    if ((d > DRAM_BASE) && (s > DRAM_BASE) && ((num_bytes % DRAM_ECC_UNIT) != 0))
    {
        UINT32 aligned;

        aligned = (num_bytes / DRAM_ECC_UNIT) * DRAM_ECC_UNIT;
        mem_copy (dst, src, aligned);

        // 1 byte씩 이동
        for (i32 = 0; i32 < (num_bytes - aligned); i32++)
        {
            //uart_printf ("mem_copy_mod :: %d / %d", i32, num_bytes - aligned);
            d8 = read_dram_8 (s + aligned + i32);
            write_dram_8 (d + aligned + i32, d8);
        }
    }

    // sram <-> dram copy, ecc size에 align된 dram to dram copy는 mem_copy로 이동
    else
    {
        mem_copy (dst, src, num_bytes);
    }

    /*
    for (i32 = 0; i32 < num_bytes; i32++)
    {
        if ((d <= DRAM_BASE) || (s <= DRAM_BASE))
            break;
        d8 = read_dram_8 (d + i32);
        if (d8 != read_dram_8(s + i32)) {
            //uart_printf ("mem_copy_mod :: data miscompared");
            break;
        }
    }
    */
}