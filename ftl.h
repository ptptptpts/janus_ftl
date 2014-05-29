// Copyright 2011 INDILINX Co., Ltd.
//
// This file is part of Jasmine.
//
// Jasmine is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Jasmine is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Jasmine. See the file COPYING.
// If not, see <http://www.gnu.org/licenses/>.
//
// GreedyFTL header file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//

#ifndef FTL_H
#define FTL_H

//#define __TEST_GC
//#define __TEST_WRT
//#define __TEST_RD
//#define __TEST_LB
//#define __TEST_LOGGING
//#define __TEST_PWRECV
#define __TEST_FUSION
#define __TEST_DEFUSION
#define __TEST_GC

/////////////////
// DRAM buffers
/////////////////

#define NUM_RW_BUFFERS		((DRAM_SIZE - DRAM_BYTES_OTHER) / BYTES_PER_PAGE - 1)
#define NUM_RD_BUFFERS		(((NUM_RW_BUFFERS / 8) + NUM_BANKS - 1) / NUM_BANKS * NUM_BANKS)
#define NUM_WR_BUFFERS		(NUM_RW_BUFFERS - NUM_RD_BUFFERS)
#define NUM_COPY_BUFFERS	NUM_BANKS_MAX
#define NUM_FTL_BUFFERS		NUM_BANKS
#define NUM_HIL_BUFFERS		1
#define NUM_TEMP_BUFFERS	1

//================================================================================
#define NUM_LBLKS			(NUM_LPAGES / PAGES_PER_BLK)

#define TIMESTAMP_MAX		0xffff
#define HIT_MAX				0x65536
#define AGE_OLD				0

#define COST_COPY			10
#define COST_PROG			3
#define COST_ERASE			1

#define LPAGES_PER_BANK		(NUM_LPAGES / NUM_BANKS)
#define VPAGES_PER_BANK		(NUM_VPAGES / NUM_BANKS)

#define LBLKS_PER_BANK		(NUM_LBLKS / NUM_BANKS)

#define PAGE_MAPPING		0
#define BLOCK_MAPPING		1

#define BLK_TO_PAGE			7
#define PAGE_MASK			0x0003ffff
//================================================================================

// 수정필요
#define DRAM_BYTES_OTHER	((NUM_COPY_BUFFERS + NUM_FTL_BUFFERS + NUM_HIL_BUFFERS + NUM_TEMP_BUFFERS) * BYTES_PER_PAGE \
+ BAD_BLK_BMP_BYTES + LPAGE_MAP_BYTES + LBLK_META_BYTES + VPAGE_MAP_BYTES + VBLK_META_BYTES + EMPTY_BLK_BYTES + FTL_TEST_BYTES)

#define WR_BUF_PTR(BUF_ID)	(WR_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define WR_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - WR_BUF_ADDR) / BYTES_PER_PAGE)
#define RD_BUF_PTR(BUF_ID)	(RD_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define RD_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - RD_BUF_ADDR) / BYTES_PER_PAGE)

#define _COPY_BUF(RBANK)	(COPY_BUF_ADDR + (RBANK) * BYTES_PER_PAGE)
#define COPY_BUF(BANK)		_COPY_BUF(REAL_BANK(BANK))
#define FTL_BUF(BANK)       (FTL_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

///////////////////////////////
// DRAM segmentation
///////////////////////////////

#define RD_BUF_ADDR			DRAM_BASE										// base address of SATA read buffers
#define RD_BUF_BYTES		(NUM_RD_BUFFERS * BYTES_PER_PAGE)

#define WR_BUF_ADDR			(RD_BUF_ADDR + RD_BUF_BYTES)					// base address of SATA write buffers
#define WR_BUF_BYTES		(NUM_WR_BUFFERS * BYTES_PER_PAGE)

#define COPY_BUF_ADDR		(WR_BUF_ADDR + WR_BUF_BYTES)					// base address of flash copy buffers
#define COPY_BUF_BYTES		(NUM_COPY_BUFFERS * BYTES_PER_PAGE)				// bank당 한 page의 Copy용 buffer를 제공 (bank를 추가할 경우를 대비해 32개의 bank용 buffer를 모두 생성)

#define FTL_BUF_ADDR		(COPY_BUF_ADDR + COPY_BUF_BYTES)				// a buffer dedicated to FTL internal purpose
#define FTL_BUF_BYTES		(NUM_FTL_BUFFERS * BYTES_PER_PAGE)				// bank당 한 page의 FTL용 buffer를 제공 (현재 달려있는 bank만큼의 buffer만 사용)

#define HIL_BUF_ADDR		(FTL_BUF_ADDR + FTL_BUF_BYTES)					// a buffer dedicated to HIL internal purpose
#define HIL_BUF_BYTES		(NUM_HIL_BUFFERS * BYTES_PER_PAGE)

#define TEMP_BUF_ADDR		(HIL_BUF_ADDR + HIL_BUF_BYTES)					// general purpose buffer
#define TEMP_BUF_BYTES		(NUM_TEMP_BUFFERS * BYTES_PER_PAGE)

#define BAD_BLK_BMP_ADDR	(TEMP_BUF_ADDR + TEMP_BUF_BYTES)				// bitmap of initial bad blocks
#define BAD_BLK_BMP_BYTES	(((NUM_VBLKS / 8) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

// logical page number를 virtual page number로 연결하는 table
//
// | .... .... | .... .... | .... .... | .... .... |
// 0   option  8      12       page number         31
// option					
// virtual page number		12~31 : logical page에 연결된 physical page number
//							(0x00000000 - not used)
//
#define LPAGE_MAP_ADDR		(BAD_BLK_BMP_ADDR + BAD_BLK_BMP_BYTES)
#define LPAGE_MAP_SIZE		4
#define LPAGE_MAP_BYTES		(((NUM_LPAGES * LPAGE_MAP_SIZE) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)
#define LPAGE_MAP_VPN_MASK	0x000fffff


// logical block들의 metadata
//
// | .... .... | .... .... | .... .... | .... .... |
// 0       time stamp     15          24  option   31
// time stamp				0-15  : 최근에 write가 발생한 시간을 기록
// option					24    : 1 - BMA, 0 - PMA
//							25-31 : valid count
#define LBLK_META_ADDR		(LPAGE_MAP_ADDR + LPAGE_MAP_BYTES)
#define LBLK_META_SIZE		4
#define LBLK_META_BYTES		(((NUM_LBLKS * LBLK_META_SIZE) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)
#define LBLK_META_TIME		2
#define LBLK_META_OP		0
#define LBLK_META_OP_MA		7
#define LBLK_META_OP_MA_MSK	0x80
#define LBLK_META_OP_VC_MSK	0x7f


// virtual page와 logical page를 연결하는 mapping table
// | .... .... | .... .... | .... .... | .... .... |
// 0   option  8      12       page number         31
// option				0 - valid bit (1 : valid)
// logical page number  12-31 : virtual page에 연결된 logical page
//
#define VPAGE_MAP_ADDR		(LBLK_META_ADDR + LBLK_META_BYTES)
#define VPAGE_MAP_SIZE		4
#define VPAGE_MAP_BYTES		(VPAGE_MAP_SIZE * NUM_VPAGES)
#define VPAGE_MAP_LPN_MASK	0x0003ffff
#define VPAGE_MAP_OP		3
#define VPAGE_MAP_OP_V_BIT	7
#define VPAGE_MAP_OP_V_MSK	0x80000000



// virtual block들의 metadata
// | .... .... | .... .... | .... .... | .... .... |
// 0  option   8 valid cnt 16 erasecnt 23
// option				0 : used bit (1 - used)
//						1 : block status (0 - PMA, 1 - BMA)
// 
// valid cnt			8-15  : virtual block에 있는 valid page의 갯수
// erase cnt			16-23 : erase한 횟수
#define VBLK_META_ADDR		(VPAGE_MAP_ADDR + VPAGE_MAP_BYTES)
#define VBLK_META_SIZE		4
#define VBLK_META_BYTES		(VBLK_META_SIZE * NUM_VBLKS)
#define VBLK_META_OP		3
#define VBLK_META_OP_USED	7
#define VBLK_META_OP_BLK	6
#define VBLK_META_OP_USED_MASK	0x80
#define VBLK_META_OP_BLK_MASK	0x40
#define VBLK_META_VCNT		2
#define VBLK_META_ECNT		1	


// empty 상태의 virtual block들을 표시하는 bitmap
// | .... .... | ~ | .... .... |
// 0      block bit map       
// block bit map			: block이 empty상태인지 bit단위로 표시 ( '1' : empty )
#define EMPTY_BLK_ADDR		(VBLK_META_ADDR + VBLK_META_BYTES)
#define EMPTY_BLK_PER_BANK	((VBLKS_PER_BANK + 7) >> 3)
#define EMPTY_BLK_BYTES		(((NUM_BANKS * EMPTY_BLK_PER_BANK) / 128 + 1) * 128)
#define EMPTY_BLK_BND		0

#define FTL_TEST_ADDR		(EMPTY_BLK_ADDR + EMPTY_BLK_BYTES)
#define FTL_TEST_BYTES		(4 * 1024 * 1024)

#define BLKS_PER_BANK		VBLKS_PER_BANK


///////////////////////////////
// FTL public functions
///////////////////////////////

void ftl_open(void);
void ftl_read(UINT32 const lba, UINT32 const num_sectors);
void ftl_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_flush(void);
void ftl_isr(void);

UINT8 gCheckRecovery;

#endif //FTL_H
